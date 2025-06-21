WITH MP AS (
    SELECT name, MIN(totalMPs) MinMPs, MAX(totalMPs) MaxMPs
    FROM Players
    GROUP BY name
)
SELECT
    RANK() OVER (ORDER BY MaxMPs DESC) rank,
    name, MinMPs, MaxMPs
FROM MP;
