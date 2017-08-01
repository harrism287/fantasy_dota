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


testcardA <- list(player = "SumaiL", deaths = 5, GPM = 5, roshkills = 10, teamfight = 20, firstblood = 15)
testcardB <- list(player = "Matumbaman", kills = 5, deaths = 5, CS = 10, GPM = 15, firstblood = 15)

comparecards(testcardA, testcardB, fantasydata, players)

