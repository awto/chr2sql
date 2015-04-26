{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module CHR2.Parser where

import Control.Applicative
import Control.Monad (MonadPlus(..))
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import System.IO
import Text.Parser.Token.Highlight
import Text.Parser.Token.Style
import qualified Text.Parser.Expression as P
import Text.Trifecta
import Text.Trifecta.Delta
import qualified Text.Trifecta.Rendering as P
import qualified Data.HashSet as HashSet
import Database.Persist.Types
import Control.Monad.State.Strict
import Text.PrettyPrint.ANSI.Leijen((<+>))
import qualified Text.PrettyPrint.ANSI.Leijen as O
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS
import Control.Lens
import Data.Maybe
import CHR2.AST.Untyped

data PState = PState {
    _chr2Syntax :: !Bool,
    _solverName :: String,
    _chrConstrs :: M.Map String [(String,SqlType)]
    }

initPState :: PState
initPState = PState{..}
             where
               _chr2Syntax = True
               _solverName = "default"
               _chrConstrs = M.empty

{-
newtype M a = M { runM :: StateT PState Parser a } 
    deriving (Functor, Applicative, Monad, MonadState PState, CharParsing, 
              Parsing, Alternative, MonadPlus, TokenParsing, DeltaParsing)
-}

type M = StateT PState Parser

pos :: (DeltaParsing m, HasSpan a) => m a -> m a
pos p = do
  (r :~ s) <- spanned p
  return $ set P.span s r

opstyle :: (DeltaParsing m) => IdentifierStyle m
opstyle = emptyOps{ _styleReserved 
                        = HashSet.fromList ["+","-","=>", ".","*","?","@",
                                            "/","&&","||","/=",":-",
                                            ">=","<=",">","<"]}

idstyle :: DeltaParsing m => IdentifierStyle m
idstyle = emptyIdents{
           _styleName = "atom",
           _styleStart = lower <|> char '_'}

varstyle :: DeltaParsing m => IdentifierStyle m
varstyle = emptyIdents{
           _styleName = "variable name",
           _styleStart = upper <|> char '_'}


pRuleName :: (DeltaParsing m) => m (Maybe C)
pRuleName = optional $ try $ pos $ flip mkC [] <$> atom <* symbol "@"

rule :: (DeltaParsing m) => m Rule
rule = pos $ mkRule <$> pRuleName <*> commaSep headItem 
       <*> option [] ((:[]) <$ symbol "=>" <*> commaSep bodyItem) 
       <* dot
       <?> "rule"

headFtr :: (Monad m, DeltaParsing m) => m String
headFtr = symbol "+" <|> symbol "-" <|> symbol "?"
          <?> "head modifier"

headItem :: (DeltaParsing m) => m C
headItem = pos $ mkC <$> (symbol "+" <|> symbol "-" <|> symbol "?") 
           <*> (mkT <$> pFtr) 
           <|> cExpr
           <?> "head constraint"
           where mkT = (:[]) . mkFtr

bodyItem :: (DeltaParsing m) => m C
bodyItem = pFtr
           <?> "body constraint"

pFtr :: (DeltaParsing m) => m C
pFtr = pos $ mkC <$> atom <*> (parens(commaSep expr) <|> pure [])
       <?> "functor"

cExpr, cTerm, cFactor :: (DeltaParsing m) => m C 
cExpr = P.buildExpressionParser tableConstr cTerm
          <?> "constraint"

cTerm = parens cExpr 
       <|> cFactor
       <?> "simple constraint"

cBinOper :: DeltaParsing m => m String
cBinOper = reservedOp "<" 
           <|> reservedOp ">" 
           <|> reservedOp "=<" 
           <|> reservedOp ">=" 
           <|> reservedOp "\\="

mkBinC :: DeltaParsing m => m C
mkBinC = pos $ (\l n r -> mkC n [l,r]) <$> expr <*> cBinOper <*> expr

cFactor = mkBinC <|> pFtr

tableConstr :: (DeltaParsing m) => [[P.Operator m C]]
tableConstr = [[cBinary "&&" P.AssocLeft],
               [cBinary "||" P.AssocLeft]]

cBinary :: (DeltaParsing m) => String -> P.Assoc -> P.Operator m C
cBinary n a = P.Infix 
              ((\l r ->  mkC n [mkFtr l,mkFtr r]) 
                       <$ reservedOp n) a

expr,term :: (DeltaParsing m) => m Term
expr = P.buildExpressionParser tableExpr term
          <?> "expression"

term = pos $ parens expr 
       <|> mkFtr <$> 
               (pos $ mkC "list" <$> brackets (commaSep expr))  
       <|> factorExpr
       <?> "simple expression"

tableExpr :: (DeltaParsing m) => [[P.Operator m Term]]
tableExpr = [[prefix "-"],
             [binary "*" P.AssocLeft, binary "/" P.AssocLeft],
             [binary "+" P.AssocLeft, binary "-" P.AssocLeft]]

factorExpr :: (DeltaParsing m) => m Term
factorExpr = pos $ mkVal <$> value
             <|> mkFtr <$> pFtr
             <|> mkVar <$> ident varstyle

value :: DeltaParsing m => m PersistValue
value = 
    PersistText <$> stringLiteral'
    <|> either (PersistInt64 . fromInteger) PersistDouble <$> integerOrDouble

binary :: (DeltaParsing m) => String -> P.Assoc -> P.Operator m Term
binary n a = P.Infix (mkBin n <$ reservedOp n) a

prefix :: (DeltaParsing m) => String -> P.Operator m Term
prefix n  = P.Prefix (mkUn n <$ reservedOp n)

atom :: (DeltaParsing m) => m String
atom = ident idstyle

reservedOp :: (DeltaParsing m) => String -> m String
reservedOp n = n <$ reserve opstyle n

mkUn n = mkFtr . mkC n . (:[])
mkBin n = \l r -> mkFtr (mkC n [l,r])

solverOption :: (DeltaParsing m) => m C
solverOption = reservedOp ":-" *> pFtr <* dot 

solver :: (DeltaParsing m) => m [Either C Rule]
solver = many (Left <$> solverOption <|> Right <$> rule) <* eof

parse :: (MonadState Env m, MonadIO m) => String -> m ()
parse fn = do
  m <- parseFromFile solver fn
  case m of
    Nothing -> return () -- TODO: throw
    Just c -> mapM_ step c
        where 
          step (Right r) = modify $ \x -> x{_rules = r : _rules x}
          step (Left c) = interp c
          interp (C _ "name" [Ftr _ (C _ n [])]) = modify $ \x -> x{_name = n}
          interp (C _ "constraints" lst) = mapM_ parseC lst
          interp (C posit n _) = liftIO $ print
                        $ posMsg posit
                        $ O.text "no interpretation for" <+> O.text n 
          parseC (Ftr _ (C _ "-" 
                               [Ftr _ (C _ n []),
                                Ftr _ (C _ "list" f)])) = do
             flds <- return . catMaybes =<< mapM parseF f
             modify $ \x -> x{_types = M.insert n flds $ _types x}
             return ()
          parseC c = liftIO $ print
                           $ posMsg (c ^. P.span) 
                           $ O.text "invalid constraint definition"
          parseF (Ftr _ (C _ "-"
                             [Ftr _ (C _ n []),
                              Ftr _ (C posit t [])])) 
              = case tyFromString t of
                  Just t -> return $ Just (Just n, t)
                  _ -> do
                       liftIO $ print $ posMsg posit 
                                    $ O.text "unknown type" <+> O.text t
                       return Nothing
          parseF f = do
             liftIO $ print
                        $ posMsg (f ^. P.span) 
                        $ O.text "invalid constraint field definition"
             return Nothing

someTest :: IO (Result [Either C Rule])
someTest = parseFromFileEx solver "dijkstra.chr"

tyFromString :: String -> Maybe Ty
tyFromString "string" = Just $ SqlTy SqlString
tyFromString "int" = Just $ SqlTy SqlInt64
tyFromString "bool" = Just $ SqlTy SqlBool
tyFromString "double" = Just $ SqlTy SqlReal
tyFromString _ = Nothing
 

