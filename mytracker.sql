-- sqlite3 mytracker.db < mytracker.sql

CREATE TABLE tracker (
  site TEXT NOT NULL,
  page TEXT NOT NULL,
  hits INTEGER,
  CONSTRAINT pk_tracker PRIMARY KEY (site, page)
);

INSERT INTO Tracker VALUES('site1', 'page1', 0);
INSERT INTO Tracker VALUES('site1', 'page2', 0);
INSERT INTO Tracker VALUES('site2', 'page1', 0);
INSERT INTO Tracker VALUES('site2', 'page2', 0);

