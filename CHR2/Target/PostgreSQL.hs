{-# LANGUAGE QuasiQuotes, RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
module CHR2.Target.PostgreSQL where

import CHR2.AST.Untyped
import Data.String.Interpolate
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

-- * replace String
-- * replace ErrorT


-- CHR based thoughts
-- * Set constraint means id isn't required 

sqlType :: Ty -> String
sqlType IntTy = "INTEGER"
sqlType FloatTy = "FLOAT" 
sqlType StringTy = "TEXT"

err e = fail e

makeColNames :: [Maybe String] -> [String]
makeColNames = flip evalState 0 . mapM hlp
    where hlp (Just n) = return n
          hlp Nothing = do
            n <- get
            put $ n+1
            return $ "fld$" ++ show n

constrSchema :: String -> TableInfo -> String
constrSchema n TableInfo{..} = 
    [i|
CREATE TABLE c$#{n} (
   id SERIAL PRIMARY KEY,
   #{sep ",\n   " $ map hlp $ zip tiTypes tiCols}
);
|]
    where
      hlp (t,x) = [i|#{x} #{sqlType t} NOT NULL|]

data BuiltinDef = Infix String 
                | Prefix String
                | FunCall String
                deriving Show


builtinDefs = M.fromList [
               ("==", Infix "="),
               ("<", Infix "<"),
               ("<=", Infix "=<"),
               (">", Infix ">"),
               (">=", Infix ">="),
               ("!=", Infix "<>"),
               ("!", Prefix "NOT")
              ]

data AggregateDef = AggregateDef
                  deriving Show

data Env = Env {
      constrs :: M.Map String [Ty],
      builtins :: M.Map String BuiltinDef,
      aggregates :: M.Map String AggregateDef,
      rules :: [Rule],
      tables :: M.Map TableName TableInfo
    }

tySomeStr :: Ty -> String
tySomeStr IntTy = "i"
tySomeStr FloatTy = "f"
tySomeStr StringTy = "s"

makeNames :: [(Maybe String, Ty)] -> [String]
makeNames v = evalState (mapM hlp v) M.empty
    where  hlp (Just v, _) = return v
           hlp (Nothing, t) = do
             modify $ M.insertWith (+) ts 1
             c <- gets (fromMaybe (error "internal") . M.lookup ts)
             return [i|#{ts}#{c}|]
             where ts = tySomeStr t

mkEnv :: M.Map String [(Maybe String, Ty)] -> 
         M.Map String BuiltinDef -> 
         M.Map String AggregateDef ->
         [Rule] -> Env
mkEnv c builtins aggregates rules = Env{..}
    where
      constrs = M.map (map snd) c
      tables = M.map (\n -> TableInfo (makeNames n) (map snd n) ) c

data TableInfo = TableInfo {
      tiCols :: [ColName],
      tiTypes :: [Ty]
    } deriving (Show)

type TableName = String
type RuleName = String
type ColName = String
type VarName = String

data SqlVal = SqlConst Const
            | SqlCol TableName ColName
            deriving (Show)

constToSql :: Const -> String
constToSql c = case c of
                 I i -> show i
                 F f -> show f
                 S s -> "'" ++ s ++ "'" -- TODO: escapes

sqlValStr :: SqlVal -> String
sqlValStr (SqlConst c) = constToSql c
sqlValStr (SqlCol t c) = [i|#{t}.#{c}|]

data BatchInfo = BatchInfo {
      biConstrs :: [(Bool,C)]
    }

data RuleInfo = RuleInfo {
      riId :: Int,
      riName :: String,
      riHeadConstrs :: [C],
      riPositive :: [(Int,C)],
      riNegative :: [(Int,C)],
      riBuiltins :: [(String,[Term],BuiltinDef)],
      riAggregates :: [(String,[Term],AggregateDef)],
      riBatches :: [BatchInfo],
      riHConstrTables :: M.Map Int (TableInfo,C),
      riVarDef :: M.Map String [SqlVal],
      riCond :: [String],
      riEnv :: Env
    }

initialRuleInfo riEnv riId riName = RuleInfo{..}
    where 
      riHeadConstrs = []
      riPositive = []
      riNegative = []
      riBuiltins = []
      riAggregates = []
      riBatches = []
      riHConstrTables = M.empty
      riVarDef = M.empty
      riCond = []
      riTableCnt = 0

sep :: String -> [String] -> String
sep s = concat . intersperse s

ind :: Int -> String -> String
ind n = (take n (repeat '\t') ++) 

data Ctx = Ctx {
      compiledRules :: M.Map String RuleInfo
    }

initCtx = Ctx{..}
    where
      compiledRules = M.empty

type M = ErrorT String (RWS Env String Ctx) 

runM :: Env -> M a -> Either String (a, String)
runM e a = case runRWS (runErrorT a) e initCtx of
             (Right a,_,w) -> Right (a,w)
             (Left a, _, _) -> Left a

-- | rule analyzer monad 
type RAM = ErrorT String (ReaderT Env (State RuleInfo))

-- | rule writer monad
type RWM = ReaderT RuleInfo (Writer String)

runRAM :: (MonadError String m) => Int -> Rule -> Env -> RAM a -> m (RuleInfo, a)
runRAM x Rule{..} e a =
  case runState (runReaderT (runErrorT a) e) (initialRuleInfo e x rn) of
    (Left v,_) -> err v
    (Right v, s) -> return (s, v)
  where rn = maybe [i|rule#{x}|] fst ruleName

runRWM :: (MonadWriter String m) => RWM a -> RuleInfo -> m a
runRWM m ri = case runWriter (runReaderT m ri) of
                (a,w) -> tell w >> return a

execRAM :: (MonadError String m) => Int -> Rule -> Env -> RAM a -> m RuleInfo
execRAM x r e a = runRAM x r e a >>= return . fst

notDefC n = err $ "not defined constraint: " ++ show n

getTableInfo :: (MonadReader RuleInfo m) 
                => TableName -> m TableInfo
getTableInfo tn = maybe (error "internal") return
                  =<< asks (M.lookup tn . tables . riEnv)

createTableAlias :: Int -> C -> RAM TableInfo
createTableAlias ix c@(n,f) = maybe mk (return . fst)
                           =<< gets (M.lookup ix . riHConstrTables)
    where
      mk = do
        ti <- maybe (notDefC n) return =<< asks (M.lookup n . tables)
        modify $ \x -> x{riHConstrTables = 
                             M.insert ix (ti,c) $ riHConstrTables x}
        return ti

getTableAlias :: Int -> RWM TableInfo
getTableAlias c = maybe (error "internal") (return . fst)
                =<< asks (M.lookup c . riHConstrTables)

sqlConstVal :: Const -> String
sqlConstVal (I i) = show i
sqlConstVal (F f) = show f
sqlConstVal (S v) = show v

addSqlCond :: String -> RAM ()
addSqlCond v = modify $ \x -> x{riCond = v : riCond x}

bindVar :: String -> SqlVal -> RAM ()
bindVar n v = modify $ \x -> x{riVarDef = M.insertWith (++) n [v] $ riVarDef x}

analyzeChrConstr :: Bool -> Int -> String -> [Term] -> [Ty] -> RAM ()
analyzeChrConstr neg ix nm args ty = do
    TableInfo{..} <- createTableAlias ix (nm, args)
    unless (length tiCols == length args) $ err $ "wrong arity of "  ++ show nm
    let ta = [i|t$#{ix}|]
    mapM_ (\ (c,v) -> 
           case v of
             Const cv -> addSqlCond [i|#{ta}.#{c} = #{sqlConstVal cv}|]
             Var n -> bindVar n (SqlCol ta c) 
             _ -> err $ "not implemented: argument for constraint: " ++ show v
        ) $ zip tiCols args
    modify $ \x -> x{riHeadConstrs = cs : riHeadConstrs x}
    modify $ if neg then \x -> x{riNegative = (ix,cs) : riNegative x}
             else \x -> x{riPositive = (ix,cs) : riPositive x}
    where cs = (nm,args)

-- pre stages allow binding more vars to be used in future stage
preAnalyzeBuiltin :: String -> [Term] -> BuiltinDef -> RAM ()
-- special case because it may define variables
preAnalyzeBuiltin _ [Var l, Const r] (Infix "=") = bindVar l (SqlConst r)  
preAnalyzeBuiltin _ [Const l, Var r] (Infix "=") = bindVar r (SqlConst l)
preAnalyzeBuiltin nm args def 
    = modify $ \x -> x{riBuiltins = (nm, args, def) : riBuiltins x}

preAnalyzeAggregate :: String -> [Term] -> AggregateDef -> RAM ()
preAnalyzeAggregate nm args def
    = modify $ \x -> x{riAggregates = (nm, args, def) : riAggregates x}

getVarRef :: String -> RAM SqlVal
getVarRef n = maybe er (return . head) =<< gets (M.lookup n . riVarDef)
    where er = err $ "couldn't find definition for variable " ++ show n

getSqlVal :: Term -> RAM SqlVal
getSqlVal (Const c) = return $ SqlConst c
getSqlVal (Var n) = getVarRef n
getSqlVal t = err $ "not implemented converting to term: " ++ show t 

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
analyzeHeadItem x (p, [v]) | p == "+" || p == "-" = do
  case v of
    Ftr n args -> do
           c <- asks (M.lookup n . constrs) 
           case c of
             Just ci -> analyzeChrConstr (p == "-") x n args ci
             Nothing -> err $ "not defined CHR constraint " ++ show c
    _ -> err $ "expected CHR constraint, but found: " ++ show v
analyzeHeadItem x (n, a) = do
  err (n ++ " isn't defined")
          `opt` (analyzeChrConstr False x, constrs)
          `opt` (preAnalyzeBuiltin, builtins)
          `opt` (preAnalyzeAggregate, aggregates)
  
  where opt nxt (h,b) = maybe nxt (h n a) =<< asks (M.lookup n . b)

analyzeBodyBatch :: [C] -> RAM ()
analyzeBodyBatch c = do
  biConstrs <- return . flip zip c =<< mapM isChrC c
  modify $ \x -> x{riBatches = BatchInfo{..} : riBatches x}
  where
    isChrC (n,_) = do
      r <- asks (isJust . M.lookup n . constrs)
      unless r $ do
         b <- asks (isJust . M.lookup n . builtins)
         unless b $ err $ "unknown constraint in rule's body: " ++ show n
      return r

-- TODO:
isOper :: String -> Bool
isOper n = 
    n `elem` ["AND","OR","NOT"]
    || all (`elem` "<>|=+-*&") n  

analyzeRule :: Rule -> RAM ()
analyzeRule r@Rule{..} = do
  mapM_ (uncurry analyzeHeadItem) $ zip [0..] ruleHead
  mapM_ analyzeBodyBatch ruleBody

termToSql :: Term -> String
termToSql (Var n) = n
termToSql (Const v) = constToSql v
termToSql (Ftr n [a,b]) | isOper n = [i|(#{termToSql a} #{n} #{termToSql b})|]
termToSql (Ftr n [a]) | isOper n = [i|(#{n} #{termToSql a})|]
termToSql (Ftr n args) = [i|#{n}(#{sep ", " $ map termToSql args})|]

analyzeRules :: M (M.Map String RuleInfo)
analyzeRules = return . M.fromList =<< mapM go . zip [0..] =<< asks rules
    where go (x,r@Rule{..}) = do
            e <- ask
            ri <- execRAM x r e (analyzeRule r)
            return (rn, ri)
            where 
              (rn,prms) = fromMaybe ([i|rule@#{x}|],[]) ruleName

-- TODO: needs vars' resolution, since some may be bound to another vars
writeRulePreView :: RWM ()
writeRulePreView = do
  RuleInfo{..} <- ask
  let vars = M.toList riVarDef
  let tables = M.toList riHConstrTables
  let flds = map (\ (x,_) -> [i|t$#{x}.id AS t$#{x}$id|]) tables
          ++ map (\(n,(v:_)) -> [i|#{sqlValStr v} AS #{n}|]) vars
  let ph = null riNegative
      phc = 
          [i|chr$ph.ruleId = #{riId}|] :
          map (\(x,(_,_)) -> [i|t$#{x}.id = chr$ph.c#{x}|]) 
                (zip [0..] $ M.elems riHConstrTables)
      phcond = emptyUnless ph 
             [[i|
    EXISTS (SELECT 1 FROM chr$ph WHERE #{sep " AND " $ phc})|]] 
  tell [i|
CREATE VIEW pr$#{riName} AS SELECT\n|]
  tell $ ind 1 $ sep ", " flds
  tell "\n\tFROM "
  tell $ sep ", " $ map (\ (x,(_,(a,_))) -> [i|c$#{a} t$#{x}|]) tables
  let cond = (concat 
              $ map (\(n, e) -> 
                 map (\ (l,r) -> [i|#{l} = #{r}|]) $ pairs $ map sqlValStr e
                 ) vars) 
             ++ riCond ++ phcond
  unless (null cond) $ do
    tell $ "\n\tWHERE "
    tell $ sep " AND " cond 
  tell ";"
    where
      pairs = map (\ (l:r:_) -> (l,r)) . filter ((> 1) . length) . tails

emptyUnless :: Bool -> [a] -> [a]
emptyUnless True v = v
emptyUnless False _ = []

condStr [] = ""
condStr n = [i|WHERE #{sep " AND " n}|]

writeRuleView :: RWM () 
writeRuleView = do
  RuleInfo{..} <- ask
  -- TODO: use compiled before priorities
  tell [i|
|]
  tell [i|
CREATE VIEW r$#{riName} AS SELECT * FROM pr$#{riName};|]

tellLine n = tell "\n" >> tell n 

writeSolveScript :: RWM ()
writeSolveScript = do
  RuleInfo{..} <- ask
  tellLine "BEGIN;"
  tellLine [i|
CREATE TEMP TABLE t$#{riName}
       ON COMMIT PRESERVE ROWS
       AS SELECT * FROM r$#{riName};|]
  -- DELETES
  mapM_ (\c@(x,(n,_)) -> do 
        tell [i|
DELETE FROM c$#{n}
    USING t$#{riName} tmp
    WHERE id = tmp.t$#{x}$id;|]
               ) riNegative
  when (null riNegative) $ do
    let pcond = concat $ map (\(x,_) -> [i| AND c#{x} = t$#{x}$id|]) riPositive
    tell [i|
DELETE FROM chr$ph 
    USING t$#{riName} c
    WHERE ruleId = #{riId}#{pcond};|]
  tellLine "COMMIT;"
  -- BATCHES: TODO
  mapM_ (\ BatchInfo{..} -> do
         tellLine "BEGIN;"
         -- TODO: this probably should be interleaved in order they appear
         mapM_ (\(isChr,(n,args)) -> 
                    if isChr then do
                      TableInfo{..} <- getTableInfo n
                      tell [i|
INSERT INTO c$#{n}(#{sep ", " tiCols}) 
       SELECT #{sep ", " $ map termToSql args} FROM t$#{riName};|]
                    else do
                      return ()
               ) biConstrs
         tellLine "COMMIT;"
       ) riBatches
  return ()

translateEnv :: M ()
translateEnv = do
  tellLine "---------------- SOLVE STEPS -------------------"
  translateConstrs
  translateRules


translateConstrs :: M ()
translateConstrs = tell =<<
    asks (concat . map (uncurry constrSchema) . M.toList . tables) 

toVariable :: Term -> Maybe String
toVariable (Var n) = Just n
toVariable _ = Nothing

translateRules :: M ()
translateRules = do
    ri <- return . map snd . M.toList 
        =<< analyzeRules
    let mxp = maximum $ map (length . riPositive) 
              $ filter (null . riNegative) ri
    tell [i|
CREATE TABLE chr$ph (
    ruleId INTEGER,
    #{sep ",\n    " $ 
          map (\j -> "c" ++ show j ++ " INTEGER") [0..mxp-1]});|]
    let step s = mapM_ (runRWM s) ri
    step writeRulePreView 
    step writeRuleView
    tellLine "---------------- SOLVE STEPS -------------------"
    step writeSolveScript

test1Env = mkEnv constrs builtinDefs M.empty rules
      where
        constrs = M.fromList 
                  [("source",[(Nothing,IntTy)]),
                   ("dist",[(Just "n",IntTy),(Nothing,IntTy)]),
                   ("edge",[(Nothing,IntTy),(Nothing,IntTy),(Nothing,IntTy)])]
        rules = [init,keep_shortest,lab]
        init = Rule (Just ("init",[])) 
               [("source",[Var "C"])] 
               [[("dist",[Var "C", Const (I 0)])]]
        keep_shortest = Rule (Just ("keep_shortest", []))
                        [("dist",[Var "V",  Var "D1"]),
                         ("-", [Ftr "dist" [Var "V",  Var "D2"]]),
                         ("<=",  [Var "D1", Var "D2"])]
                        []
        lab = Rule (Just ("label", [Var "D"]))
               [("dist",[Var "V", Var "D"]),
                ("edge",[Var "V", Var "C", Var "U"])]
               [[("dist", [Var "U", Ftr "+" [Var "D", Var "C"]])]]

runTest1 :: IO ()
runTest1 = 
    case runM test1Env translateEnv of
      Right (v,w) -> putStrLn w
      Left e -> fail e


