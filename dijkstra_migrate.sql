
---------------- SOLVE STEPS -------------------
DROP TABLE IF EXISTS "c$dist" CASCADE;
CREATE TABLE "c$dist" (
   id SERIAL PRIMARY KEY,
   "to" INT8 NOT NULL,
   "weight" DOUBLE PRECISION NOT NULL
);

DROP TABLE IF EXISTS "c$edge" CASCADE;
CREATE TABLE "c$edge" (
   id SERIAL PRIMARY KEY,
   "from" INT8 NOT NULL,
   "weight" DOUBLE PRECISION NOT NULL,
   "to" INT8 NOT NULL
);

DROP TABLE IF EXISTS "c$source" CASCADE;
CREATE TABLE "c$source" (
   id SERIAL PRIMARY KEY,
   "node" INT8 NOT NULL
);

DROP TABLE IF EXISTS chr$ph;
CREATE TABLE chr$ph (
    ruleId INTEGER,
    c0 INTEGER,
    c1 INTEGER);
CREATE VIEW pr$init AS SELECT
	t$0.id AS t$0$id, t$0.node AS C
	FROM c$source t$0
	WHERE 
    NOT EXISTS (SELECT 1 FROM chr$ph WHERE chr$ph.ruleId = 2 AND t$0.id = chr$ph.c0);
CREATE VIEW pr$keep_shortest AS SELECT
	t$0.id AS t$0$id, t$1.id AS t$1$id, t$0.weight AS D1, t$1.weight AS D2, t$1.to AS V
	FROM c$dist t$0, c$dist t$1
	WHERE t$1.to = t$0.to AND t$1.id <> t$0.id AND t$0.id <> t$1.id AND t$0.weight <= t$1.weight;
CREATE VIEW pr$label AS SELECT
	t$0.id AS t$0$id, t$1.id AS t$1$id, t$1.weight AS C, t$0.weight AS D, t$1.to AS U, t$1.from AS V
	FROM c$dist t$0, c$edge t$1
	WHERE t$1.from = t$0.to AND 
    NOT EXISTS (SELECT 1 FROM chr$ph WHERE chr$ph.ruleId = 0 AND t$0.id = chr$ph.c0 AND t$1.id = chr$ph.c1);
CREATE VIEW r$init AS SELECT * FROM pr$init;
CREATE VIEW r$keep_shortest AS SELECT * FROM pr$keep_shortest;
CREATE VIEW r$label AS SELECT * FROM pr$label;
---------------- SOLVE STEPS -------------------

CREATE OR REPLACE FUNCTION step$init() RETURNS INTEGER AS $$

DECLARE
  result INTEGER;
BEGIN
CREATE TEMP TABLE t$init
       -- ON COMMIT PRESERVE ROWS
       AS SELECT * FROM r$init;
INSERT INTO chr$ph(ruleId,c0) 
       SELECT '2',t$0$id
       FROM t$init;

-- COMMIT;
-- BEGIN;
INSERT INTO c$dist("to", "weight") 
       SELECT C, 0 FROM t$init;
-- COMMIT;

result := (SELECT count(*) FROM t$init);
DROP TABLE t$init;
RETURN result;
END;
$$  LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION step$keep_shortest() RETURNS INTEGER AS $$

DECLARE
  result INTEGER;
BEGIN
CREATE TEMP TABLE t$keep_shortest
       -- ON COMMIT PRESERVE ROWS
       AS SELECT * FROM r$keep_shortest;
DELETE FROM c$dist
    USING t$keep_shortest tmp
    WHERE id = tmp.t$1$id;
-- COMMIT;

result := (SELECT count(*) FROM t$keep_shortest);
DROP TABLE t$keep_shortest;
RETURN result;
END;
$$  LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION step$label() RETURNS INTEGER AS $$

DECLARE
  result INTEGER;
BEGIN
CREATE TEMP TABLE t$label
       -- ON COMMIT PRESERVE ROWS
       AS SELECT * FROM r$label;
INSERT INTO chr$ph(ruleId,c1,c0) 
       SELECT '0',t$1$id,t$0$id
       FROM t$label;

-- COMMIT;
-- BEGIN;
INSERT INTO c$dist("to", "weight") 
       SELECT U, (D + C) FROM t$label;
-- COMMIT;

result := (SELECT count(*) FROM t$label);
DROP TABLE t$label;
RETURN result;
END;
$$  LANGUAGE plpgsql;
CREATE OR REPLACE FUNCTION simple$solver() RETURNS BOOLEAN AS $$
DECLARE
  init$exit BOOLEAN;
  keep_shortest$exit BOOLEAN;
  label$exit BOOLEAN;
BEGIN
  LOOP
    init$exit := step$init() = 0;
    keep_shortest$exit := step$keep_shortest() = 0;
    label$exit := step$label() = 0;
    IF init$exit AND keep_shortest$exit AND label$exit THEN
       EXIT;
    END IF;
  END LOOP;
  RETURN TRUE;
END;
$$ LANGUAGE plpgsql;
