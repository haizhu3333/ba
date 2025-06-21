PRAGMA foreign_keys = ON;

CREATE TABLE IF NOT EXISTS Events (
    id INTEGER NOT NULL,
    startDate TEXT NOT NULL,
    boardTop REAL NOT NULL,
    PRIMARY KEY (id)
) STRICT;

CREATE TABLE IF NOT EXISTS HandRecords (
    eventId INTEGER NOT NULL,
    sessionNo INTEGER NOT NULL,
    boardNo INTEGER NOT NULL,
    pointsN INTEGER NOT NULL,
    pointsS INTEGER NOT NULL,
    pointsE INTEGER NOT NULL,
    pointsW INTEGER NOT NULL,
    PRIMARY KEY (eventId, sessionNo, boardNo),
    FOREIGN KEY (eventId) REFERENCES Events(id)
) STRICT;

CREATE TABLE IF NOT EXISTS Players (
    eventId INTEGER NOT NULL,
    sessionNo INTEGER NOT NULL,
    sectionName TEXT NOT NULL,
    pairNo TEXT NOT NULL,
    direction TEXT NOT NULL,
    name TEXT NOT NULL,
    number TEXT NOT NULL,
    totalMPs REAL,
    PRIMARY KEY (eventId, sessionNo, sectionName, pairNo, direction),
    FOREIGN KEY (eventId) REFERENCES Events(id)
) STRICT;

CREATE INDEX IF NOT EXISTS PlayersByNumber ON Players (number);
CREATE INDEX IF NOT EXISTS PlayersByName ON Players (name);

CREATE TABLE IF NOT EXISTS Results (
    eventId INTEGER NOT NULL,
    sessionNo INTEGER NOT NULL,
    sectionName TEXT NOT NULL,
    boardNo INTEGER NOT NULL,
    tableNo INTEGER NOT NULL,
    pairNS TEXT NOT NULL,
    pairEW TEXT NOT NULL,
    scoreNS REAL NOT NULL,
    scoreEW REAL NOT NULL,
    PRIMARY KEY (eventId, sessionNo, sectionName, boardNo, tableNo),
    FOREIGN KEY (eventId, sessionNo, boardNo) REFERENCES HandRecords(eventId, sessionNo, boardNo)
) STRICT;

CREATE TRIGGER IF NOT EXISTS PairExistsCheck INSERT ON Results
WHEN (SELECT COUNT(*) FROM Players p
        WHERE p.eventId = NEW.eventId
        AND p.sessionNo = NEW.sessionNo
        AND p.sectionName = NEW.sectionName
        AND ((p.pairNo = NEW.pairNS AND p.direction IN ('N', 'S')) OR
             (p.pairNo = NEW.pairEW AND p.direction IN ('E', 'W')))) <> 4
BEGIN
    SELECT RAISE(FAIL, "Players not found");
END;
