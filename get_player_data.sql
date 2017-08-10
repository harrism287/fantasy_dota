--Grabs table of player data (ID, name, pos, team) from OpenDota.

SELECT
notable_players.account_id,
notable_players.name,
notable_players.fantasy_role,
notable_players.team_id,
notable_players.team_name,
notable_players.country_code
FROM matches
JOIN match_patch using(match_id)
JOIN leagues using(leagueid)
JOIN player_matches using(match_id)
JOIN heroes on heroes.id = player_matches.hero_id
LEFT JOIN notable_players ON notable_players.account_id = player_matches.account_id AND notable_players.locked_until = (SELECT MAX(locked_until) FROM notable_players)
LEFT JOIN teams using(team_id)
WHERE TRUE
AND teams.team_id IN (5, 15, 39, 46, 2163, 350190, 1375614, 1838315, 1883502, 2108395, 2512249, 2581813, 2586976, 2640025, 2672298, 1333179, 3331948, 1846548)
GROUP BY notable_players.account_id
HAVING count(distinct matches.match_id) >= 1
ORDER BY team_name ASC, fantasy_role ASC