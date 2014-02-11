CREATE SEQUENCE gameid START 1;
CREATE SEQUENCE categoryid START 1;

CREATE TABLE games (
     id  integer PRIMARY KEY DEFAULT nextval('gameid'),
     name   text NOT NULL CHECK (name <> ''),
     url    text
);

CREATE TABLE categories (
    id integer PRIMARY KEY DEFAULT nextval('categoryid'),
    name text NOT NULL,
    gameid integer NOT NULL
);


INSERT INTO games (name, url) VALUES ('test', 'http://test.com');
INSERT INTO games (name, url) VALUES ('flappybird', 'http://flappybird.com');
INSERT INTO games (name, url) VALUES ('biogems', 'http://biogems.com');


INSERT INTO categories (name, gameid) VALUES ('helicopter', 2);
INSERT INTO categories (name, gameid) VALUES ('match3', 3);
INSERT INTO categories (name, gameid) VALUES ('puzzle', 2);
INSERT INTO categories (name, gameid) VALUES ('puzzle', 1);
