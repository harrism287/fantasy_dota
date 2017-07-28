--This script grabs the necessary data table for calculating fantasy points from OpenDota.

SELECT
matches.match_id,
player_matches.account_id,
kills kills,
deaths deaths,
last_hits lasthits,
denies denies,
gold_per_min GPM,
towers_killed towerkills,
roshans_killed roshkills,
teamfight_participation teamfight,
obs_placed wards,
camps_stacked stacks,
rune_pickups runes,
firstblood_claimed firstblood,
stuns stuns
FROM matches
JOIN player_matches using(match_id)
JOIN heroes on heroes.id = player_matches.hero_id
LEFT JOIN notable_players ON notable_players.account_id = player_matches.account_id AND notable_players.locked_until = (SELECT MAX(locked_until) FROM notable_players)
LEFT JOIN teams using(team_id)
WHERE TRUE
AND kills IS NOT NULL
AND deaths IS NOT NULL
AND last_hits IS NOT NULL
AND denies IS NOT NULL
AND towers_killed IS NOT NULL
AND roshans_killed IS NOT NULL
AND teamfight_participation IS NOT NULL
AND obs_placed IS NOT NULL
AND camps_stacked IS NOT NULL
AND rune_pickups IS NOT NULL
AND firstblood_claimed IS NOT NULL
AND stuns IS NOT NULL
AND teams.team_id IN (5, 15, 39, 46, 2163, 350190, 1375614, 1838315, 1883502, 2108395, 2512249, 2581813, 2586976, 2640025, 2672298, 1333179, 3331948, 1846548)
ORDER BY match_id DESC
