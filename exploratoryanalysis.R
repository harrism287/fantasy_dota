setwd("~/GitHub/fantasy_dota")
source("functions.R")

rawdata <- read.csv("dota_fantasy_raw.csv")
players <- read.csv("playerdata.csv", stringsAsFactors = FALSE)
teams <- unique(players[,c("team_id", "team_name")])

fantasydata <- data.frame("matchID" = rawdata$match_id,
                          "playerID" = rawdata$account_id,
                          "kills" = rawdata$kills * 0.3,
                          "deaths" = 3 - (rawdata$deaths * 0.3),
                          "CS" = (rawdata$lasthits + rawdata$denies) * 0.003,
                          "GPM" = rawdata$gpm * 0.002,
                          "towerkills" = rawdata$towerkills,
                          "roshkills" = rawdata$roshkills,
                          "teamfight" = rawdata$teamfight * 3,
                          "wards" = rawdata$wards * 0.5,
                          "stacks" = rawdata$stacks * 0.5,
                          "runes" = rawdata$runes * 0.25,
                          "firstblood" = rawdata$firstblood * 4,
                          "stuns" = rawdata$stuns * 0.05)
fantasydata$total <- rowSums(fantasydata[3:14])



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


