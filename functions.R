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

cardscores <- function(card, data){

  cardinfo <- card$cardinfo
  playerdata <- filter(data, playerID == cardinfo$playerID)
  stats <- names(cardinfo)[8:length(cardinfo)]

   for(stat in stats){
     #print(names(card)[[i]])
     playerdata[,stat] <- playerdata[,stat] * (1 + (cardinfo[[stat]]/100))
   }
  
  playerdata$total <- rowSums(playerdata[3:14])
  
  return(playerdata)
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
  cardtable <- as.data.frame(read.csv(filename, header = TRUE, stringsAsFactors = FALSE))
  
  cards <- list()
  
  for(i in 1:nrow(cardtable)){
    cards[[cardtable[i,1]]]$cardinfo <- merge(players, cardtable[i,])
    cards[[cardtable[i,1]]]$scores <- cardscores(cards[[cardtable[i,1]]], data)
    cards[[cardtable[i,1]]]$scoresums <- statsummary(cards[[cardtable[i,1]]]$scores)
  }
  return(cards)
}

sumtable <- function(cards, stat){
  
  table <- data.frame()
  cardnames <- names(cards)
  
  i = 1
  for(card in cards){
    
    table <- rbind(table, data.frame(cardnames[i],card$cardinfo[,c("playerName", "team_name", "fantasy_role")]
                           ,card$scoresums[stat,]))
    i <- i + 1
  }
  

  colnames(table) <- c("Card", "Player", "Team", "Role", "Minimum", "Mean", "Median",
                       "Maximum", "Standard Deviation", "Median Absolute Deviation")
  
  return(table)
  
}

violinplot <- function(cards, stat = "total", players, xlim){
  
  fulltable <- NULL
  names <- names(cards)
  
  for(i in 1:length(cards)){
    
    currtable <- cards[[i]]$scores[,2:15]
    
    currtable$card <- rep(names[[i]], nrow(currtable))
    
    currtable$pos <- rep(cards[[i]]$cardinfo$fantasy_role, nrow(currtable))
    
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
    
    currtable$pos <- rep(cards[[i]]$cardinfo$fantasy_role, nrow(currtable))
    
    fulltable <- rbind(fulltable, currtable)
  }
  
  p <- ggplot(fulltable, aes_string(x = "card", y = stat)) + expand_limits(y=0)
  p <- p + stat_density2d(aes(alpha=..density.., fill=..density..))
  p
  
}

filtercards <- function(cards, key){
  
  if(is.null(key)){
    return(cards)
  }

  returnlist <- list()
  
  cardnames <- names(cards)
  
  for(i in 1:length(cards)){
    if(cards[[i]]$cardinfo$team_name %in% key |
       cards[[i]]$cardinfo$fantasy_role %in% key){
      returnlist[[cardnames[[i]]]] <- cards[[i]]
    }
  }
  return(returnlist)
}




