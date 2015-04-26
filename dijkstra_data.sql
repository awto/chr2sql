DELETE FROM c$source;
DELETE FROM c$dist;
DELETE FROM c$edge;
DELETE FROM chr$ph;
INSERT INTO c$source("node") VALUES(1);
INSERT INTO c$edge("from","weight","to") VALUES(1,1,2);
INSERT INTO c$edge("from","weight","to") VALUES(1,10,3);
INSERT INTO c$edge("from","weight","to") VALUES(2,1,4);
INSERT INTO c$edge("from","weight","to") VALUES(3,9,4);
INSERT INTO c$edge("from","weight","to") VALUES(4,2,1);

