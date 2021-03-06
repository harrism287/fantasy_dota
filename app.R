source("./functions.R")
library(shiny)

url <- "https://api.opendota.com/"
path <- "api/explorer?sql=SELECT%0D%0Amatches.match_id%2C%0D%0Aplayer_matches.account_id%2C%0D%0Akills%20kills%2C%0D%0Adeaths%20deaths%2C%0D%0Alast_hits%20lasthits%2C%0D%0Adenies%20denies%2C%0D%0Agold_per_min%20GPM%2C%0D%0Atowers_killed%20towerkills%2C%0D%0Aroshans_killed%20roshkills%2C%0D%0Ateamfight_participation%20teamfight%2C%0D%0Aobs_placed%20wards%2C%0D%0Acamps_stacked%20stacks%2C%0D%0Arune_pickups%20runes%2C%0D%0Afirstblood_claimed%20firstblood%2C%0D%0Astuns%20stuns%0D%0AFROM%20matches%0D%0AJOIN%20player_matches%20using(match_id)%0D%0AJOIN%20heroes%20on%20heroes.id%20%3D%20player_matches.hero_id%0D%0ALEFT%20JOIN%20notable_players%20ON%20notable_players.account_id%20%3D%20player_matches.account_id%20AND%20notable_players.locked_until%20%3D%20(SELECT%20MAX(locked_until)%20FROM%20notable_players)%0D%0ALEFT%20JOIN%20teams%20using(team_id)%0D%0AWHERE%20TRUE%0D%0AAND%20kills%20IS%20NOT%20NULL%0D%0AAND%20deaths%20IS%20NOT%20NULL%0D%0AAND%20last_hits%20IS%20NOT%20NULL%0D%0AAND%20denies%20IS%20NOT%20NULL%0D%0AAND%20towers_killed%20IS%20NOT%20NULL%0D%0AAND%20roshans_killed%20IS%20NOT%20NULL%0D%0AAND%20teamfight_participation%20IS%20NOT%20NULL%0D%0AAND%20obs_placed%20IS%20NOT%20NULL%0D%0AAND%20camps_stacked%20IS%20NOT%20NULL%0D%0AAND%20rune_pickups%20IS%20NOT%20NULL%0D%0AAND%20firstblood_claimed%20IS%20NOT%20NULL%0D%0AAND%20stuns%20IS%20NOT%20NULL%0D%0AAND%20teams.team_id%20IN%20(5%2C%2015%2C%2039%2C%2046%2C%202163%2C%20350190%2C%201375614%2C%201838315%2C%201883502%2C%202108395%2C%202512249%2C%202581813%2C%202586976%2C%202640025%2C%202672298%2C%201333179%2C%203331948%2C%201846548)%0D%0AORDER%20BY%20match_id%20DESC%0D%0A"

rawdata <- getrawdata(url, path)
fantasydata <- fantasycalc(rawdata)

players <- read.csv("playerdata.csv", stringsAsFactors = FALSE)

for(i in 1:nrow(players)){
  players[i,"fantasy_role"] <- switch(as.character(players[i,"fantasy_role"]),
                                      "1" = "core", "3" = "offlane", "2" = "support")
}

teams <- unique(players[,c("team_id", "team_name")])


ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      fileInput("usercards", "Choose CSV File",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")
                
      ),
      helpText("The input file should be in CSV format with one card per line."),
      selectInput("displayscore", "Fantasy stat:", 
                  c("kills", "deaths", "CS", "GPM", "towerkills", "roshkills",
                    "teamfight", "wards", "stacks", "runes", "firstblood", "stuns", "total"),
                  selected = "total"),
      selectInput("displaystat", "Sort by:",
                  c("Minimum", "Mean", "Median", "Maximum", "Standard Deviation", "Median Absolute Deviation"),
                  selected = "Mean"),
      checkboxInput("dec", "sort descending", value = T),
      selectInput("displaypos", "Fantasy roles:",
                  c("core", "offlane", "support"), multiple = T),
      selectInput("displayteam", "Team:",
                  teams$team_name, multiple = T),
      checkboxInput("makeplot", "plot data", value = T)
    ),
    mainPanel(
      tableOutput("maintable"),
      plotOutput("plot")
    )
  )
)

server <- function(input, output) {
  
  tags$head(tags$style("maintable  {white-space: nowrap;  }"))
  
  output$maintable <- renderTable({
    
    inputcards <- input$usercards
    if (is.null(inputcards)){
      carddata <- importcards("nobonus.csv", fantasydata, players)
    }else{
      carddata <- importcards(inputcards$datapath, fantasydata, players)
    }
    
    summarytable <- sumtable(carddata, input$displayscore)
    
    if(!is.null(input$displayteam)){
      summarytable <- filter(summarytable, Team %in% input$displayteam)
    }
    if(!is.null(input$displaypos)){
      summarytable <- filter(summarytable, Role %in% input$displaypos)
    }
    
    
    if(input$dec){
      summarytable[order(summarytable[,input$displaystat], decreasing = T),]
    }else{
      summarytable[order(summarytable[,input$displaystat], decreasing = F),]
    }
    
  })

  output$plot <- renderPlot({

    if(!input$makeplot){
      return(NULL)
    }

    inputcards <- input$usercards
    if (is.null(inputcards)){
      carddata <- importcards("nobonus.csv", fantasydata, players)
    }else{
      carddata <- importcards(inputcards$datapath, fantasydata, players)
    }

    carddata <- filtercards(carddata, input$displaypos)
    carddata <- filtercards(carddata, input$displayteam)

    p <- violinplot(carddata, input$displayscore, players)

    return(p)

  })

}

shinyApp(ui, server)

