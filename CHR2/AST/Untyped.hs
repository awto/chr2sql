{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
module CHR2.AST.Untyped where

import qualified Data.Map as M
import qualified Data.Set as S(empty)
import Database.Persist.Types
import Control.Lens
import Control.Lens.TH
import Control.Monad.State.Strict
import qualified Text.Trifecta as P
import Text.Trifecta(explain,renderingCaret,Err(..))
import Text.PrettyPrint.ANSI.Leijen((<+>))
import qualified Text.PrettyPrint.ANSI.Leijen as O
import Data.Monoid
import Data.ByteString(ByteString)
import qualified Data.ByteString as BS

data Ty = SqlTy SqlType
        -- | UserTy String [(String,[(Maybe String,Ty)])]
        deriving (Eq,Show,Ord)


-- data Posit = Posit { _posDelta :: !Delta, _posLine :: !ByteString }
--           deriving Show

type Posit = P.Span

posMsg :: Posit -> O.Doc -> O.Doc
posMsg s m = m O.<+> O.linebreak 
                       <> O.pretty (P.render s) O.<> O.linebreak

posEmpty = P.Span mempty mempty BS.empty

data Term = Var { _termPos :: Posit, _var :: String }
          | Val { _termPos :: Posit, _val :: PersistValue }
          | Ftr { _termPos :: Posit, _ftr :: C }
          deriving (Show)


mkVar = Var posEmpty
mkVal = Val posEmpty
mkFtr = Ftr posEmpty

type CTypes = M.Map String [Ty]

data C = C { _cPos :: Posit, _cName :: String, _cArgs :: [Term] }
          deriving (Show)

mkC = C posEmpty

data Rule = Rule {
      _rulePos :: Posit,
      _ruleName :: Maybe C,
      _ruleHead :: [C],
      _ruleBody :: [[C]]
    } deriving (Show)

mkRule = Rule posEmpty

data BuiltinDef = Infix String 
                | Prefix String
                | FunCall String
                deriving Show

builtinDefs = M.fromList [
               ("==", Infix "="),
               ("<", Infix "<"),
               ("<=", Infix "<="),
               ("=<", Infix "<="),
               (">", Infix ">"),
               (">=", Infix ">="),
               ("!=", Infix "<>"),
               ("!", Prefix "NOT")
              ]

data AggregateDef = AggregateDef
                  deriving Show

data Env = Env {
      _name :: String,
      _types :: M.Map String [(Maybe String, Ty)],
      _constrs :: M.Map String [Ty],
      _builtins :: M.Map String BuiltinDef,
      _aggregates :: M.Map String AggregateDef,
      _rules :: [Rule],
      _tables :: M.Map TableName TableInfo,
      _priority :: [((C,C),[Term])]
    }

emptyEnv :: Env
emptyEnv = Env{..}
    where
      _name = "noname"
      _types = M.empty
      _constrs = M.empty
      _builtins = builtinDefs
      _aggregates = M.empty
      _rules = []
      _tables = M.empty
      _priority = []

err e = fail e

tySomeStr :: Ty -> String
tySomeStr _ = "s"
data TableInfo = TableInfo {
      _tiCols :: [ColName],
      _tiTypes :: [Ty]
    } deriving (Show)

type TableName = String
type RuleName = String
type ColName = String
type VarName = String

data SqlVal = SqlConst { _sqlConst :: PersistValue }
            | SqlCol { _sqlColTable :: TableName, _sqlColName :: ColName }
            deriving (Show)

data BatchInfo = BatchInfo {
      _biConstrs :: [(Bool,C)]
    }

data RuleInfo = RuleInfo {
      _riId :: Int,
      _riName :: String,
      _riHeadConstrs :: [C],
      _riPositive :: [(Int,C)],
      _riNegative :: [(Int,C)],
      _riBuiltins :: [(String,[Term],BuiltinDef)],
      _riAggregates :: [(String,[Term],AggregateDef)],
      _riBatches :: [BatchInfo],
      _riHConstrTables :: M.Map Int (TableInfo,C),
      _riVarDef :: M.Map String [SqlVal],
      _riCond :: [String],
      _riEnv :: Env
    }

data Ctx = Ctx {
      _compiledRules :: M.Map String RuleInfo
    }

makeLenses ''Term
makeLenses  ''C
makeLenses ''Rule
makeLenses ''Env
makeLenses ''Ctx
makeLenses ''BatchInfo
makeLenses ''RuleInfo
makePrisms ''Term

instance P.HasSpan Term where span = termPos
instance P.HasSpan Rule where span = rulePos
instance P.HasSpan C where span = cPos

-- makeClassyPrisms ''Term

initRule, keep_shortest, lab :: Rule
initRule = mkRule (Just (mkC "init" [])) 
       [mkC "source" [mkVar "C"]]
       [[mkC "dist" [mkVar "C", mkVal (PersistInt64 0)]]]
keep_shortest = mkRule (Just (mkC "keep_shortest" []))
                [(mkC "dist" [mkVar "V",  mkVar "D1"]),
                 (mkC "-" [mkFtr (mkC "dist" [mkVar "V",  mkVar "D2"])]),
                 (mkC "<="  [mkVar "D1", mkVar "D2"])]
                []
lab = mkRule (Just (mkC "label" [mkVar "D"]))
      [(mkC "dist" [mkVar "V", mkVar "D"]),
       (mkC "edge" [mkVar "V", mkVar "C", mkVar "U"])]
      [[(mkC "dist" [mkVar "U", mkFtr (mkC "+" [mkVar "D", mkVar "C"])])]]

