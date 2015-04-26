{-# LANGUAGE QuasiQuotes, RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
module CHR2.Target.PostgreSQL where

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
import Database.Persist


import CHR2.AST.Untyped
import CHR2.Compile

-- * replace String
-- * replace ErrorT

-- CHR based thoughts
-- * Set constraint means id isn't required
-- * no needs for propogation history because no variables in stores

constrSchema :: String -> TableInfo -> String
constrSchema n TableInfo{..} = 
    [i|
DROP TABLE IF EXISTS "c$#{n}" CASCADE;
CREATE TABLE "c$#{n}" (
   id SERIAL PRIMARY KEY,
   #{sep ",\n   " $ map hlp $ zip _tiTypes _tiCols}
);
|]
    where
      hlp (t,x) = [i|#{show x} #{sqlType t} NOT NULL|]

-- adds condition for avoiding same constraint in single rule
--       riHConstrTables :: M.Map Int (TableInfo,C),

-- TODO: needs vars' resolution, since some may be bound to another vars
writeRulePreView :: RWM ()
writeRulePreView = do
  RuleInfo{..} <- ask
  let vars = M.toList _riVarDef
  let tables = M.toList _riHConstrTables
  let flds = map (\ (x,_) -> [i|t$#{x}.id AS t$#{x}$id|]) tables
          ++ map (\(n,(v:_)) -> [i|#{sqlValStr v} AS #{n}|]) vars
  
  let ph = null _riNegative
      phc = 
          [i|chr$ph.ruleId = #{_riId}|] :
          map (\(x,(_,_)) -> [i|t$#{x}.id = chr$ph.c#{x}|]) 
                (zip [0..] $ M.elems _riHConstrTables)
      phcond = emptyUnless ph 
             [[i|
    NOT EXISTS (SELECT 1 FROM chr$ph WHERE #{sep " AND " $ phc})|]] 
  tell [i|
CREATE VIEW pr$#{_riName} AS SELECT\n|]
  tell $ ind 1 $ sep ", " flds
  tell "\n\tFROM "
  tell $ sep ", " $ map (\ (x,(_,(C _ a _))) -> [i|c$#{a} t$#{x}|]) tables
  let cond = (concat 
              $ map (\(n, e) -> 
                 map (\ (l,r) -> [i|#{l} = #{r}|]) $ pairs $ map sqlValStr e
                 ) vars) 
             ++ _riCond ++ phcond
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

  tell [i|
CREATE VIEW r$#{_riName} AS SELECT * FROM pr$#{_riName};|]

tellLine n = tell "\n" >> tell n 

writeSolveScript :: RWM ()
writeSolveScript = do
  RuleInfo{..} <- ask
  tellLine [i|
CREATE OR REPLACE FUNCTION step$#{_riName}() RETURNS INTEGER AS $$|] 
  tellLine [i|
DECLARE
  result INTEGER;
BEGIN
CREATE TEMP TABLE t$#{_riName}
       -- ON COMMIT PRESERVE ROWS
       AS SELECT * FROM r$#{_riName};|]
  -- DELETES
  mapM_ (\c@(x,(C _ n _)) -> do 
        tell [i|
DELETE FROM c$#{n}
    USING t$#{_riName} tmp
    WHERE id = tmp.t$#{x}$id;|]
               ) _riNegative
  when (null _riNegative) $ do
    let (col,val) = unzip
                    $ map (\(x,_) -> ([i|,c#{x}|], [i|,t$#{x}$id|])) _riPositive
    tell [i|
INSERT INTO chr$ph(ruleId#{concat col}) 
       SELECT '#{_riId}'#{concat val}
       FROM t$#{_riName};
|]
  tellLine "-- COMMIT;"
  -- BATCHES: TODO
  mapM_ (\ BatchInfo{..} -> do
         tellLine "-- BEGIN;"
         -- TODO: this probably should be interleaved in order they appear
         mapM_ (\(isChr,(C _ n args)) -> 
                    if isChr then do
                      TableInfo{..} <- getTableInfo n
                      tell [i|
INSERT INTO c$#{n}(#{sep ", " $ map show _tiCols}) 
       SELECT #{sep ", " $ map termToSql args} FROM t$#{_riName};|]
                    else do
                      return ()
               ) _biConstrs
         tellLine "-- COMMIT;"
       ) _riBatches
  tellLine [i|
result := (SELECT count(*) FROM t$#{_riName});
DROP TABLE t$#{_riName};
RETURN result;
END;
$$  LANGUAGE plpgsql;|]
  return ()

translateEnv :: M ()
translateEnv = do
  tellLine "---------------- SOLVE STEPS -------------------"
  translateConstrs
  translateRules


translateConstrs :: M ()
translateConstrs = tell =<<
    asks (concat . map (uncurry constrSchema) . M.toList . _tables) 

toVariable :: Term -> Maybe String
toVariable (Var _ n) = Just n
toVariable _ = Nothing

translateRules :: M ()
translateRules = do
    ri <- return . map snd . M.toList 
        =<< analyzeRules
    let mxp = maximum $ map (length . _riPositive) 
              $ filter (null . _riNegative) ri
    let names = map _riName ri
    tell [i|
DROP TABLE IF EXISTS chr$ph;
CREATE TABLE chr$ph (
    ruleId INTEGER,
    #{sep ",\n    " $ 
          map (\j -> "c" ++ show j ++ " INTEGER") [0..mxp-1]});|]
    let step s = mapM_ (runRWM s) ri
    step writeRulePreView
    step writeRuleView
    tellLine "---------------- SOLVE STEPS -------------------"
    step writeSolveScript

    let decls = concat $ map (\x -> [i|
  #{x}$exit BOOLEAN;|]) names
    let calls = concat $ map (\x -> [i|
    #{x}$exit := step$#{x}() = 0;|]) names
    let exitCond = sep " AND " $ map (\x -> [i|#{x}$exit|]) names
    tell [i|
CREATE OR REPLACE FUNCTION simple$solver() RETURNS BOOLEAN AS $$
DECLARE#{decls}
BEGIN
  LOOP#{calls}
    IF #{exitCond} THEN
       EXIT;
    END IF;
  END LOOP;
  RETURN TRUE;
END;
$$ LANGUAGE plpgsql;
|]

publishScript :: Env -> IO ()
publishScript e@Env{..} = do
  r <- runM e translateEnv
  case r of
    Right (v,w) -> writeFile (_name ++ "_migrate.sql") w
    Left e -> fail e

-- FROM Database.Persist.Postgresql
showSqlType :: SqlType -> String
showSqlType SqlString = "VARCHAR"
showSqlType SqlInt32 = "INT4"
showSqlType SqlInt64 = "INT8"
showSqlType SqlReal = "DOUBLE PRECISION"
showSqlType (SqlNumeric s prec) = concat [ "NUMERIC(", (show s), ",", (show prec), ")" ]
showSqlType SqlDay = "DATE"
showSqlType SqlTime = "TIME"
showSqlType SqlDayTime = "TIMESTAMP WITH TIME ZONE"
showSqlType SqlBlob = "BYTEA"
showSqlType SqlBool = "BOOLEAN"


sqlType :: Ty -> String
sqlType (SqlTy t) = showSqlType t

