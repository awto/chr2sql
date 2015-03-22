DELETE FROM c$source;
DELETE FROM c$dist;
DELETE FROM c$edge;
DELETE FROM chr$ph;
INSERT INTO c$source(i1) VALUES(1);
INSERT INTO c$edge(i1,i2,i3) VALUES(1,1,2);
INSERT INTO c$edge(i1,i2,i3) VALUES(1,10,3);
INSERT INTO c$edge(i1,i2,i3) VALUES(2,1,4);
INSERT INTO c$edge(i1,i2,i3) VALUES(3,9,4);
INSERT INTO c$edge(i1,i2,i3) VALUES(4,2,1);

