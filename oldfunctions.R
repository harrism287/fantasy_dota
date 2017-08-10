library(ggplot2)
library(plyr)
library(dplyr)
library(gridExtra)
library(scales)
library(httr)
library(jsonlite)
library(stats)
library(ggthemes)


getplayerinfo <- function(player, players){
  return(players[players$account_id == player,])
}

densityplot <- function(x, playerinfo, stat = "total", xlim, title){
  
  
  ID <- playerinfo[[1]]
  #print(ID)
  playerName <- playerinfo[[2]]
  if(missing(title)){title <- playerName}
  #print(playerName)
  position <- playerinfo[[3]]
  #print(position)
  
  playerdata <- filter(x, playerID == ID)
  
  p <- ggplot(playerdata, aes_string(stat)) + stat_density(geom="step")
  p <- p + labs(title = title, x = paste(stat, "points", sep = " ")) + geom_vline(aes(xintercept = mean(playerdata[,stat])))
  if(!missing(xlim)){return(p + xlim(xlim))}
  else{return(p)}
}

teamdensity <- function(x, team, stat = "total", xlim){
  
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

comparecards <- function(cardA, cardB, data, players){
  
  scoresA <- cardscores(cardA, data, players)
  scoresB <- cardscores(cardB, data, players)
  
  #print(summary(scoresA))
  #print(summary(scoresB))
  
  meanDiff <- mean(scoresA$total) - mean(scoresB$total)
  mannwhitney <- wilcox.test(scoresA$total, scoresB$total)
  
  if(meanDiff > 0){
    print(paste("Card A is better by", round(meanDiff, 2),
                "points, with a p-value of", scientific(mannwhitney$p.value, 3), sep=" "))
  }else if(meanDiff < 0){
    print(paste("Card B is better by", round((-1 * meanDiff), 2), "points.",
                "points, with a p-value of", scientific(mannwhitney$p.value, 3), sep=" "))
  }else{
    print("It's a tie!")
  }
  
  maxscore <- max(c(scoresA$total, scoresB$total))
  
  lim <- c(0, round_any(maxscore, 5, ceiling))
  
  plotA <- densityplot(scoresA, getplayerinfo(cardA$player, players), title = "CardA", xlim = lim)
  plotB <- densityplot(scoresB, getplayerinfo(cardB$player, players), title = "CardB", xlim = lim)
  
  grid.arrange(plotA, plotB, ncol = 2)
  
}

makecard <- function(player, kills, deaths, CS, GPM, towerkills, roshkills,
                     teamfight, wards, stacks, runes, firstblood, stuns){
  if(missing(player)){stop("No player")}
  
  argg <- as.list(environment())
  
  card <- list()
  
  for(stat in names(argg)){
    if(argg[[stat]] > 0){
      card[[stat]] <- argg[[stat]]
    }
  }
  
  return(card)
}

