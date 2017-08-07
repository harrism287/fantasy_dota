library(ggplot2)
library(plyr)
library(dplyr)
library(gridExtra)
library(scales)
library(httr)
library(jsonlite)
library(stats)
library(ggthemes)

getrawdata <- function(url, path){
  raw <- GET(url = url, path = path)
  return(as.data.frame(fromJSON(rawToChar(raw$content))$rows))
}

fantasycalc <- function(rawdata){
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
  return(fantasydata)
}

getplayerinfo <- function(player, players){
  if(is.numeric(player)){players[players$account_id == player,]}
  else{return(players[pmatch(tolower(player), tolower(players$name)),])}
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

cardscores <- function(card, data, players){
  ID <- getplayerinfo(card$player, players)$account_id
  
  playerdata <- filter(data, playerID == ID)
  #print(summary(playerdata))
  
  
  
  # if(length(card) == 1){
  #   return(playerdata)
  # }
  # 
   for(i in seq(3, length(card), 1)){
     #print(names(card)[[i]])
     playerdata[,names(card)[i]] <- playerdata[,names(card)[[i]]] * (1 + (card[[i]]/100))
   }
  
  playerdata$total <- rowSums(playerdata[3:14])
  
  return(playerdata)
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

statsummary <- function(statdata){
  
  statsum <- data.frame()
  
  for(stat in names(statdata[,3:15])){

    statsum[stat, "min"] = min(statdata[,stat])
    statsum[stat, "mean"] = mean(statdata[,stat])
    statsum[stat, "med"] = median(statdata[,stat])
    statsum[stat, "max"] = max(statdata[,stat])
    statsum[stat, "sd"] = sd(statdata[,stat])
    statsum[stat, "mad"] = mad(statdata[,stat])
  }
  return(statsum)
}

importcards <- function(filename, data, players){
  cards <- as.data.frame(read.csv(filename, header = TRUE, stringsAsFactors = FALSE))
  
  carddata <- list()
  
  for(i in 1:nrow(cards)){
    carddata[[cards[i,1]]]$cardstats <- cards[i,]
    carddata[[cards[i,1]]]$scores <- cardscores(cards[i,], data, players)
    carddata[[cards[i,1]]]$scoresums <- statsummary(carddata[[i]]$scores)
    carddata[[cards[i,1]]]$playerinfo <- getplayerinfo(cards[i,2], players)
  }
  return(carddata)
}

sumtable <- function(cards, stat, players){
  
  table <- NULL

  for(card in cards){
    table<- rbind(table, card$scoresums[stat,])
  }
  
  row.names(table) <- names(cards)
  
  return(table)
  
}

violinplot <- function(cards, stat = "total", players, xlim){
  
  fulltable <- NULL
  names <- names(cards)
  
  for(i in 1:length(cards)){
    
    currtable <- cards[[i]]$scores[,2:15]
    
    currtable$card <- rep(names[[i]], nrow(currtable))
    
    playerdata <- getplayerinfo(cards[[i]]$scores$playerID[[1]], players)
    
    currtable$pos <- rep(playerdata$fantasy_role, nrow(currtable))
    
    fulltable <- rbind(fulltable, currtable)
  }
  
  p <- ggplot(fulltable, aes_string(x = "card", y = stat)) + expand_limits(y=0)
  p <- p + geom_violin(fill = "paleturquoise", color = "turquoise4", size = 1.5)
  p <- p + stat_summary(fun.y = mean, geom="point", size = 3, color = "turquoise4")
  p <- p + theme_minimal() + theme(text = element_text(size = 20))
  p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  
  p
  
}

heatmap <- function(cards, stat = "total", players, xlim){
  
  fulltable <- NULL
  names <- names(cards)
  
  for(i in 1:length(cards)){
    
    currtable <- cards[[i]]$scores[,2:15]
    
    currtable$card <- rep(names[[i]], nrow(currtable))
    
    playerdata <- getplayerinfo(cards[[i]]$scores$playerID[[1]], players)
    
    currtable$pos <- rep(playerdata$fantasy_role, nrow(currtable))
    
    fulltable <- rbind(fulltable, currtable)
  }
  
  p <- ggplot(fulltable, aes_string(x = "card", y = stat)) + expand_limits(y=0)
  p <- p + stat_density2d(aes(alpha=..density.., fill=..density..))
  p
  
}

posfilter <- function(cards, pos){
  if(pos == "all"){
    return(cards)
  }
  
  cores <- list()
  offs <- list()
  sups <- list()
  
  cardnames <- names(cards)
  
  for(i in 1:length(cards)){
    
     switch(cards[[i]]$playerinfo$fantasy_role,
            cores[[cardnames[[i]]]] <- cards [[i]],
            sups[[cardnames[[i]]]] <- cards[[i]],
            offs[[cardnames[[i]]]] <- cards[[i]])
  }
  
  return(switch(pos, "core" = cores, "support" = sups, "offlane" = offs))
  
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


