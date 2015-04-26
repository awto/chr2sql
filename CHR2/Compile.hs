{-# LANGUAGE QuasiQuotes, RecordWildCards, FlexibleContexts #-}
module CHR2.Compile where

import CHR2.AST.Untyped
import Data.String.Interpolate
import Data.Function
import Control.Monad
import Control.Monad.Error
import Control.Monad.RWS.Strict
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Control.Monad.Reader
import qualified Data.Text as T
import Data.Text(Text)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe
import Data.List
import Data.List.Split 
import Database.Persist.Types
import Control.Lens
import Control.Lens.Prism
import qualified Text.Trifecta.Rendering as P
import qualified Text.PrettyPrint.ANSI.Leijen as O

makeColNames :: [Maybe String] -> [String]
makeColNames = flip evalState 0 . mapM hlp
    where hlp (Just n) = return n
          hlp Nothing = do
            n <- get
            put $ n+1
            return $ "fld$" ++ show n

makeNames :: [(Maybe String, Ty)] -> [String]
makeNames v = evalState (mapM hlp v) M.empty
    where  hlp (Just v, _) = return v
           hlp (Nothing, t) = do
             modify $ M.insertWith (+) ts 1
             c <- gets (fromMaybe (error "internal") . M.lookup ts)
             return [i|#{ts}#{c}|]
             where ts = tySomeStr t

mkEnv :: String -> 
         M.Map String [(Maybe String, Ty)] -> 
         M.Map String BuiltinDef -> 
         M.Map String AggregateDef ->
         [Rule] ->
         [((C,C),[Term])] -> Env
mkEnv _name _types _builtins _aggregates _rules _priority = Env{..}
    where
      _constrs = M.map (map snd) _types
      _tables = M.map (\n -> TableInfo (makeNames n) (map snd n) ) _types

initEnv :: MonadState Env m => m ()
initEnv = modify $ 
          \Env{..} -> mkEnv _name _types _builtins _aggregates _rules _priority

initialRuleInfo _riEnv _riId _riName = RuleInfo{..}
    where 
      _riHeadConstrs = []
      _riPositive = []
      _riNegative = []
      _riBuiltins = []
      _riAggregates = []
      _riBatches = []
      _riHConstrTables = M.empty
      _riVarDef = M.empty
      _riCond = []
      _riTableCnt = 0

sep :: String -> [String] -> String
sep s = concat . intersperse s

ind :: Int -> String -> String
ind n = (take n (repeat '\t') ++) 

initCtx = Ctx{..}
    where
      _compiledRules = M.empty



type M = ErrorT String (RWST Env String Ctx IO) 

runM :: Env -> M a -> IO (Either String (a, String))
runM e a = do
  r <- runRWST (runErrorT a) e initCtx
  return $ case r of
    (Right a,_,w) -> Right (a,w)
    (Left a, _, _) -> Left a

-- | rule analyzer monad 
type RAM = ErrorT String (ReaderT Env (StateT RuleInfo IO))

-- | rule writer monad
type RWM = ReaderT RuleInfo (Writer String)

runRAM :: (MonadError String m, MonadIO m) 
          => Int -> Rule -> Env -> RAM a -> m (RuleInfo, a)
runRAM x Rule{..} e a = do
  r <- liftIO $ runStateT (runReaderT (runErrorT a) e) (initialRuleInfo e x rn)
  case r of
    (Left v,_) -> err v
    (Right v, s) -> return (s, v)
  where rn = maybe [i|rule#{x}|] _cName _ruleName

runRWM :: (MonadWriter String m) => RWM a -> RuleInfo -> m a
runRWM m ri = case runWriter (runReaderT m ri) of
                (a,w) -> tell w >> return a

execRAM :: (MonadError String m, MonadIO m) 
           => Int -> Rule -> Env -> RAM a -> m RuleInfo
execRAM x r e a = runRAM x r e a >>= return . fst

notDefC n = do
  liftIO $ print
             $ posMsg (n ^. P.span) 
             $ O.text "not defined constraint"
  err $ "not defined constraint" 

errP p n = do
  liftIO $ print
             $ posMsg (p ^. P.span) 
             $ O.text n
  err $ "not defined constraint" 


getTableInfo :: (MonadReader RuleInfo m) 
                => TableName -> m TableInfo
getTableInfo tn = maybe (error "internal") return
                  =<< view (riEnv.tables.at tn)

createTableAlias :: Int -> C -> RAM TableInfo
createTableAlias ix c@(C _ n f) = maybe mk (return . fst)
                           =<< use (riHConstrTables.at ix)
    where
      mk = do
        ti <- maybe (notDefC c) return =<< view (tables . at n) 
        riHConstrTables . at ix .= Just (ti,c)
        return ti

getTableAlias :: Int -> RWM TableInfo
getTableAlias c = maybe (error "internal") (return . fst)
                =<< view (riHConstrTables . at c)

addSqlCond :: String -> RAM ()
addSqlCond v = riCond %= cons v

bindVar :: String -> SqlVal -> RAM ()
bindVar n v = riVarDef %= M.insertWith (++) n [v]

analyzeChrConstr :: Bool -> Int -> String -> [Term] -> [Ty] -> RAM ()
analyzeChrConstr neg ix nm args ty = do
    TableInfo{..} <- createTableAlias ix (mkC nm args)
    unless (length _tiCols == length args) $ err $ "wrong arity of "  ++ show nm
    let ta = [i|t$#{ix}|]
    mapM_ (\ (c,v) -> 
           case v of
             Val _ cv -> addSqlCond [i|#{ta}.#{c} = #{showSqlVal cv}|]
             Var _ n -> bindVar n (SqlCol ta c) 
             _ -> err $ "not implemented: argument for constraint: " ++ show v
        ) $ zip _tiCols args
    riHeadConstrs %= cons cs
    (if neg then riNegative else riPositive) %= cons (ix, cs)
    where cs = mkC nm args

-- pre stages allow binding more vars to be used in future stage
preAnalyzeBuiltin :: String -> [Term] -> BuiltinDef -> RAM ()
-- special case because it may define variables
preAnalyzeBuiltin _ [Var _ l, Val _ r] (Infix "=") = bindVar l (SqlConst r)  
preAnalyzeBuiltin _ [Val _ l, Var _ r] (Infix "=") = bindVar r (SqlConst l)
preAnalyzeBuiltin nm args def = riBuiltins %= cons (nm, args, def)

preAnalyzeAggregate :: String -> [Term] -> AggregateDef -> RAM ()
preAnalyzeAggregate nm args def = riAggregates %= cons (nm, args, def)

getVarRef :: String -> RAM SqlVal
getVarRef n = maybe er (return . head) =<< use (riVarDef.at n)
    where er = err $ "couldn't find definition for variable " ++ show n

getSqlVal :: Term -> RAM SqlVal
getSqlVal (Val _ c) = return $ SqlConst c
getSqlVal (Var _ n) = getVarRef n
getSqlVal t = err $ "not implemented converting to term: " ++ show t 

sqlValStr :: SqlVal -> String
sqlValStr (SqlConst c) = showSqlVal c
sqlValStr (SqlCol t c) = [i|#{t}.#{c}|]

showSqlVal :: PersistValue -> String
showSqlVal (PersistInt64 x) = show x
showSqlVal (PersistDouble d) = show d
-- TODO: 

analyzeBuiltin :: String -> [Term] -> BuiltinDef -> RAM ()
analyzeBuiltin nm args def  = 
    hlp def =<< mapM (return . sqlValStr <=< getSqlVal) args
    where
      hlp (Infix op) [l,r] = addSqlCond $ [i|#{l} #{op} #{r}|]
      hlp (Prefix op) [l] = addSqlCond $ [i|#{op} #{l}|]
      hlp (FunCall fn) args = addSqlCond $ [i|#{fn}(#{sep "," args})|]
      hlp _ _ = err $ "not supported builtin"

analyzeAggregate :: String -> [Term] -> AggregateDef -> RAM ()
analyzeAggregate _ _ _ = return ()

analyzeHeadItem :: Int -> C -> RAM ()
analyzeHeadItem x (C _ p [v]) | p == "+" || p == "-" = do
  case v of
    Ftr _ cc@(C _ n args) -> do
           c <- view (constrs.at n) 
           case c of
             Just ci -> analyzeChrConstr (p == "-") x n args ci
             Nothing -> notDefC cc
    _ -> err $ "expected CHR constraint, but found: " ++ show v
analyzeHeadItem x c@(C _ n a) = do
  errP c ("constraint " ++ show n ++ " isn't defined")
          `opt` (analyzeChrConstr False x, constrs)
          `opt` (preAnalyzeBuiltin, builtins)
          `opt` (preAnalyzeAggregate, aggregates)
          `opt` (analyzeBuiltin, builtins)
          `opt` (analyzeAggregate, aggregates)  
  where opt nxt (h,b) = maybe nxt (h n a) =<< view (b.at n)

analyzeBodyBatch :: [C] -> RAM ()
analyzeBodyBatch c = do
  _biConstrs <- return . flip zip c =<< mapM isChrC c
  riBatches %= cons BatchInfo{..}
  where
    isChrC (C _ n _) = do
      r <- view (constrs.at n.to isJust)
      unless r $ do
         b <- view (builtins.at n.to isJust)
         unless b $ err $ "unknown constraint in rule's body: " ++ show n
      return r

distinctConstr :: RAM ()
distinctConstr =
  mapM_ (\a -> 
         mapM_ (\(l,r) -> addSqlCond [i|t$#{l}.id <> t$#{r}.id|]) 
            [(i,j) | i <- a, j <- a, i /= j]
            ) =<< gets (map (map snd)
                    . filter((> 1) . length)
                    . groupBy ((==) `on` fst)
                    . sortBy (compare `on` fst)
                    . map (\(k,(_,(C _ n _))) -> (n,k)) 
                    . M.toList . _riHConstrTables)

-- TODO:
isOper :: String -> Bool
isOper n = 
    n `elem` ["AND","OR","NOT"]
    || all (`elem` "<>|=+-*&") n  

analyzeRule :: Rule -> RAM ()
analyzeRule r@Rule{..} = do
  mapM_ (uncurry analyzeHeadItem) $ zip [0..] _ruleHead
  distinctConstr
  mapM_ analyzeBodyBatch _ruleBody

termToSql :: Term -> String
termToSql (Var _ n) = n
termToSql (Val _ v) = showSqlVal v
termToSql (Ftr _ (C _ n [a,b])) | isOper n = [i|(#{termToSql a} #{n} #{termToSql b})|]
termToSql (Ftr _ (C _ n [a])) | isOper n = [i|(#{n} #{termToSql a})|]
termToSql (Ftr _ (C _ n args)) = [i|#{n}(#{sep ", " $ map termToSql args})|]

analyzeRules :: M (M.Map String RuleInfo)
analyzeRules = return . M.fromList =<< mapM go . zip [0..] =<< view rules
    where go (x,r@Rule{..}) = do
            e <- ask
            ri <- execRAM x r e (analyzeRule r)
            return (rn, ri)
            where 
              (C _ rn prms) = fromMaybe (mkC [i|rule@#{x}|] []) _ruleName
