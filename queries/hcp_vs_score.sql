SELECT
    e.startDate, p.sessionNo, r.boardNo,
    CASE p.direction
        WHEN 'N' THEN h.pointsN
        WHEN 'S' THEN h.pointsS
        WHEN 'E' THEN h.pointsE
        WHEN 'W' THEN h.pointsW
    END HCP,
    (CASE
        WHEN p.direction IN ('N', 'S') THEN r.scoreNS
        WHEN p.direction IN ('E', 'W') THEN r.scoreEW
    END) / e.boardTop * 100 Percent
FROM Events e
INNER JOIN Players p ON
    e.id = p.eventId
INNER JOIN Results r ON
    p.eventId = r.eventId AND
    p.sessionNo = r.sessionNo AND
    p.sectionName = r.sectionName AND
    ((p.direction IN ('N', 'S') AND p.pairNo = r.pairNS) OR
     (p.direction IN ('E', 'W') AND p.pairNo = r.pairEW))
INNER JOIN HandRecords h ON
    r.eventId = h.eventId AND
    r.sessionNo = h.sessionNo AND
    r.boardNo = h.boardNo
WHERE p.number = :playerNumber
