setwd("~/GitHub/fantasy_dota")
source("functions.R")

url <- "https://api.opendota.com/"
path <- "api/explorer?sql=SELECT%0D%0Amatches.match_id%2C%0D%0Aplayer_matches.account_id%2C%0D%0Akills%20kills%2C%0D%0Adeaths%20deaths%2C%0D%0Alast_hits%20lasthits%2C%0D%0Adenies%20denies%2C%0D%0Agold_per_min%20GPM%2C%0D%0Atowers_killed%20towerkills%2C%0D%0Aroshans_killed%20roshkills%2C%0D%0Ateamfight_participation%20teamfight%2C%0D%0Aobs_placed%20wards%2C%0D%0Acamps_stacked%20stacks%2C%0D%0Arune_pickups%20runes%2C%0D%0Afirstblood_claimed%20firstblood%2C%0D%0Astuns%20stuns%0D%0AFROM%20matches%0D%0AJOIN%20player_matches%20using(match_id)%0D%0AJOIN%20heroes%20on%20heroes.id%20%3D%20player_matches.hero_id%0D%0ALEFT%20JOIN%20notable_players%20ON%20notable_players.account_id%20%3D%20player_matches.account_id%20AND%20notable_players.locked_until%20%3D%20(SELECT%20MAX(locked_until)%20FROM%20notable_players)%0D%0ALEFT%20JOIN%20teams%20using(team_id)%0D%0AWHERE%20TRUE%0D%0AAND%20kills%20IS%20NOT%20NULL%0D%0AAND%20deaths%20IS%20NOT%20NULL%0D%0AAND%20last_hits%20IS%20NOT%20NULL%0D%0AAND%20denies%20IS%20NOT%20NULL%0D%0AAND%20towers_killed%20IS%20NOT%20NULL%0D%0AAND%20roshans_killed%20IS%20NOT%20NULL%0D%0AAND%20teamfight_participation%20IS%20NOT%20NULL%0D%0AAND%20obs_placed%20IS%20NOT%20NULL%0D%0AAND%20camps_stacked%20IS%20NOT%20NULL%0D%0AAND%20rune_pickups%20IS%20NOT%20NULL%0D%0AAND%20firstblood_claimed%20IS%20NOT%20NULL%0D%0AAND%20stuns%20IS%20NOT%20NULL%0D%0AAND%20teams.team_id%20IN%20(5%2C%2015%2C%2039%2C%2046%2C%202163%2C%20350190%2C%201375614%2C%201838315%2C%201883502%2C%202108395%2C%202512249%2C%202581813%2C%202586976%2C%202640025%2C%202672298%2C%201333179%2C%203331948%2C%201846548)%0D%0AORDER%20BY%20match_id%20DESC%0D%0A"

rawdata <- getrawdata(url, path)
fantasydata <- fantasycalc(rawdata)

players <- read.csv("playerdata.csv", stringsAsFactors = FALSE)
teams <- unique(players[,c("team_id", "team_name")])

fantasydata <- merge(players, fantasydata)

eg <- subset(players, players$team_id == 39)

densityplot(fantasydata, eg[1,], xlim=c(0,35))

egtotal <- teamdensity(fantasydata, eg, xlim = c(0,35))
egGPM <- teamdensity(fantasydata, eg, stat = "GPM")


card_sumail <- list(player = "SumaiL", deaths = 5, GPM = 5, roshkills = 10, teamfight = 20, firstblood = 15)
card_matumba <- list(player = "Matumbaman", kills = 5, deaths = 5, CS = 10, GPM = 15, firstblood = 15)
card_burning  <- list(player = "BurNing", kills =10, teamfight = 20, runes = 5)
card_Moogy <- list(player = "Moogy", GPM = 20, towerkills = 5, teamfight = 10, wards = 20, firstblood = 20)
card_rtz <- list(player = "Arteezy", kills = 5, deaths = 20, towerkills = 20)
card_op <- list(player = "op", deaths = 20, towerkills = 10, runes = 20)
card_sccc <- list(player = "sccc", kills = 10, deaths = 10, towerkills = 15)
card_miracle <- list(player = "miracle", CS = 20, GPM = 15, roshkills = 15)

comparecards(card_sumail, card_matumba, fantasydata, players)
comparecards(card_sumail, card_burning, fantasydata, players)
comparecards(card_sumail, card_Moogy, fantasydata, players)
comparecards(card_sumail, card_rtz, fantasydata, players)
comparecards(card_sumail, card_op, fantasydata, players)
comparecards(card_sumail, card_sccc, fantasydata, players)
comparecards(card_sumail, card_miracle, fantasydata, players)

card_uni <- makecard("Universe", kills = 5, deaths = 15, CS = 5, GPM = 10, firstblood = 15)
card_mindcontrol <- makecard("Mind_Control", deaths = 15, CS = 15, towerkills = 5, teamfight = 10, stuns = 10)
card_xxs <- makecard("Xxs", deaths = 10, GPM = 5, stuns = 10)
card_kpii <- makecard("kpii", roshkills = 20, runes = 25, stuns = 15)

comparecards(card_uni, card_mindcontrol, fantasydata, players)
comparecards(card_uni, card_xxs, fantasydata, players)
comparecards(card_uni, card_kpii, fantasydata, players)

card_crit <- makecard("Cr1t-", deaths = 15, towerkills = 15, wards = 10, stacks = 15, stuns = 20)
card_zai <- makecard("zai", towerkills = 15, stacks = 25, runes = 5)
card_boboka <- makecard("BoBoKa", kills = 10, firstblood = 10, stuns = 20)
card_faith <- makecard("Faith", roshkills = 25, firstblood = 15, stuns = 10)
card_kaka <- makecard("kaka", kills = 15, roshkills = 15, firstblood = 10)
card_kuro <- makecard("KuroKy", towerkills = 10, teamfight = 20, firstblood = 5)
card_q <- makecard("Q")
card_gh <- makecard("Gh")

comparecards(card_crit, card_zai, fantasydata, players)
comparecards(card_crit, card_boboka, fantasydata, players)
comparecards(card_crit, card_faith, fantasydata, players)
comparecards(card_crit, card_kaka, fantasydata, players)
comparecards(card_crit, card_kuro, fantasydata, players)
comparecards(card_crit, card_q, fantasydata, players)
comparecards(card_crit, card_gh, fantasydata, players)



testcards <- importcards("../../../Desktop/testcards.csv", fantasydata, players)

rsconnect::deployApp(appFiles = c("app.R", "functions.R", "nobonus.csv", "playerdata.csv"))


