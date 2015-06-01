DELETE FROM source;
DELETE FROM dist;
DELETE FROM edge;
DELETE FROM chr$ph;
INSERT INTO source("node") VALUES(1);
INSERT INTO edge("from","weight","to") VALUES(1,1,2);
INSERT INTO edge("from","weight","to") VALUES(1,10,3);
INSERT INTO edge("from","weight","to") VALUES(2,1,4);
INSERT INTO edge("from","weight","to") VALUES(3,9,4);
INSERT INTO edge("from","weight","to") VALUES(4,2,1);

