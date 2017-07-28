setwd("~/GitHub/fantasy_dota")
library(ggplot2)
library(dplyr)
library(gridExtra)

rawdata <- read.csv("dota_fantasy_raw.csv")

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

players <- unique(fantasydata$playerID)


eg <- data.frame("playerName" = c("Arteezy", "SumaiL", "Universe", "zai", "Cr1t-"),
                 "playerID" = c(86745912, 111620041, 87276347, 73562326, 25907144),
                 "Position" = c(1,2,3,4,5), stringsAsFactors = FALSE)

densityplot <- function(x, playerinfo, stat = "total", xlim){
  
  playerName <- as.character(playerinfo[1])
  ID <- playerinfo[2]
  position <- playerinfo[3]
  
  playerdata <- filter(x, playerID == ID[[1]])
  
  p <- ggplot(playerdata, aes_string(stat)) + stat_density(geom="step")
  p <- p + labs(title = playerName, x = paste(stat, "points", sep = " ")) + geom_vline(aes(xintercept = mean(playerdata[,stat])))
  if(!missing(xlim)){return(p + xlim(xlim))}
  else{return(p)}
}

densityplot(fantasydata, eg[1,], xlim=c(0,35))

rtz <- subset(fantasydata, fantasydata$playerID == 86745912)
ggplot(rtz, aes(eval(as.symbol("total")))) + stat_density(geom="step") + xlim(0,35)

teamdensity <- function(x, team, stat = "total", xlim){
  #apply(team, 1, densityplot, x=x)
  
  plots <- list()
  if(missing(xlim)){
    for(i in seq(1:5)){
      plots[[i]] <- densityplot(x, team[i,], stat = stat)
    }
  }else{
    for(i in seq(1:5)){
      plots[[i]] <- densityplot(x, team[i,], stat = stat, xlim = xlim)
    }
    }
  arrangeGrob(plots[[1]], plots[[2]], plots[[3]], plots[[4]], plots[[5]], ncol=1)
}



egtotal <- teamdensity(fantasydata, eg, xlim = c(0,35))
egGPM <- teamdensity(fantasydata, eg, stat = "GPM")





