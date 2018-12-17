library(shiny)
library(markdown)
library(readr)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(knitr)
library(kableExtra)

Initial <- read.csv("2012-18_playerBoxScore.csv")
Initial$playAST <- as.numeric(Initial$playAST)
Initial$playTRB <- as.numeric(as.character(Initial$playTRB))
Initial$playWeight <- as.numeric(as.character(Initial$playWeight))
Initial$playMin <- as.numeric(as.character(Initial$playMin))
Initial <- Initial[,c(-1,-2,-3,-4,-5,-8,-11,-12,-13,-14,-15,-16,-17,-24,-48,-49,-50,-51)]
away_data <- filter(Initial,Initial$teamLoc=="Away")
home_data <- filter(Initial,Initial$teamLoc=="Home")
nba_total <- rbind(away_data,home_data)

ui <- navbarPage("NBA DATA!",
                 tabPanel("EDA for Free Throw Percentage",
                          sidebarLayout(
                            sidebarPanel(
                              radioButtons("shootingType", "Shooting Type:",
                                           c("Two Point Shooting",
                                             "Three Point Shooting",
                                             "Field Goal Shooting"))),
                            mainPanel(
                              plotOutput("plot")))),
                 tabPanel("EDA for Position",
                          sidebarLayout(
                            sidebarPanel(
                              radioButtons("graphType", "Graph Type:",
                                           c("Count", "Proportion")),
                              radioButtons("variables","Variables:",
                                          c("Player height(Count Only)",
                                            "Player weight(Count only)",
                                            "Player Minute",
                                            "Free throw shooting percentage(Proportion only)",
                                            "Field goal percentage(Proportion only)",
                                            "Two point shooting percentage(Proportion only)",
                                            "Three point shooting percentage(Proportion only)",
                                            "Assists made by player",
                                            "Points scored by player",
                                            "Rebounds made by player",
                                            "Turnovers made by player",
                                            "Personal fouls made by player",
                                            "Steals made by player",
                                            "Blocks made by player"))),
                            mainPanel(
                              plotOutput("plot2")))),
                 tabPanel("EDA for Home/Away",
                          sidebarLayout(
                            sidebarPanel(
                              radioButtons("location", "Location:",
                                           c("Home", "Away")),
                              radioButtons("stats","Stats:",
                                          c("Assist","Rebound"))),
                            mainPanel(
                              plotOutput("plot3")))),
                 navbarMenu("Chi-square test",
                            tabPanel("Chi-square test for assist",
                                     mainPanel(
                                       verbatimTextOutput("test1"))),
                            tabPanel("Chi-square test for rebound",
                                     mainPanel(
                                       verbatimTextOutput("test2"))),
                           tabPanel("Conclusion",
                                     mainPanel(
                                       verbatimTextOutput("conclusion")))
                                       )
                 )

server <- function(input, output, session) {
    output$plot <- renderPlot(
      if(input$shootingType == "Two Point Shooting"){
      ggplot(data = nba_total,aes(x=play2P.,y=playFT.))+geom_point()+
        ggtitle("NBA player free throw percentage and 2 point shooting percentgae")+
        xlab("Player 2 point shooting percentage")+ylab("Player free throw percentage")
    }
    else if (input$shootingType == "Three Point Shooting"){
      ggplot(data = nba_total,aes(x=play3P.,y=playFT.))+geom_point()+
        ggtitle("NBA player free throw percentage and 3 point shooting percentgae")+
        xlab("Player 3 point shooting percentage")+ylab("Player free throw percentage")
    }
    else if (input$shootingType == "Field Goal Shooting"){
      ggplot(data = nba_total,aes(x=playFG.,y=playFT.))+geom_point()+
        ggtitle("NBA player free throw percentage and Field goal percentgae")+
        xlab("Player Field goal percentage")+ylab("Player free throw percentage")
    }
    )
    
    output$plot2 <- renderPlot(
      if(input$graphType == "Count" & input$variables == "Player height(Count Only)"){
      ggplot(data = nba_total,aes(x=playHeight))+geom_bar(aes(fill=playPos))+
        ggtitle("NBA player average height")+xlab("Player height")+
        scale_fill_discrete(breaks=c("C","PF","F","SF","SG","G","PG"))
    }
    else if(input$graphType == "Count" & input$variables == "Player weight(Count only)"){
      ggplot(data = nba_total,aes(x=playWeight,fill=playPos))+geom_histogram(binwidth = 5)+
        scale_x_continuous(name='Player Weight',breaks=seq(140,360,20))+
        scale_fill_discrete(breaks=c("C","PF","F","SF","SG","G","PG"))
    }
    else if(input$graphType == "Count" & input$variables == "Player Minute"){
      ggplot(data = nba_total,aes(x=playMin,fill=playPos))+geom_histogram(binwidth = 1)+
        scale_x_continuous(name="Player Minute",breaks=seq(0,60,10))+
        scale_y_continuous(name="Count",breaks=seq(0,6000,1000))+
        scale_fill_discrete(breaks=c("C","PF","F","SF","SG","G","PG"))
    }
    else if(input$graphType == "Proportion" & input$variables == "Player Minute"){
      ggplot(data = nba_total,aes(x=playMin,fill=playPos))+geom_histogram(position = "fill",binwidth = 1)+
        scale_x_discrete(name='Player Minute',breaks=seq(0,60,10),limits=c(0,60))+
        ylab("Proportion")+scale_fill_discrete(breaks=c("C","PF","F","SF","SG","G","PG"))
    }
    else if(input$graphType == "Proportion" & input$variables == "Free throw shooting percentage(Proportion only)"){
      ggplot(data = nba_total,aes(x=playFT.,fill=playPos))+geom_histogram(position = "fill",binwidth = 0.1)+
        scale_x_discrete(name="Free throw shooting percentage",breaks=seq(0,1,0.1),limits=c(0,1))+
        ylab("Proportion")+scale_fill_discrete(breaks=c("C","PF","F","SF","SG","G","PG"))
    }
    else if(input$graphType == "Proportion" & input$variables == "Field goal percentage(Proportion only)"){
      ggplot(data = nba_total,aes(x=playFG.,fill=playPos))+geom_histogram(position = "fill",binwidth = 0.1)+
        scale_x_discrete(name="Field goal percentage",breaks=seq(0,1,0.1),limits=c(0,1))+
        ylab("Proportion")+scale_fill_discrete(breaks=c("C","PF","F","SF","SG","G","PG"))
    }
    else if(input$graphType == "Proportion" & input$variables == "Two point shooting percentage(Proportion only)"){
      ggplot(data = nba_total,aes(x=play2P.,fill=playPos))+geom_histogram(position = "fill",binwidth = 0.1)+
        scale_x_discrete(name="2 point shooting percentage",breaks=seq(0,1,0.1),limits=c(0,1))+
        ylab("Proportion")+scale_fill_discrete(breaks=c("C","PF","F","SF","SG","G","PG"))
    }
    else if(input$graphType == "Proportion" & input$variables == "Three point shooting percentage(Proportion only)"){
      ggplot(data = nba_total,aes(x=play3P.,fill=playPos))+geom_histogram(position = "fill",binwidth = 0.1)+
        scale_x_discrete(name="3 point shooting percentage",breaks=seq(0,1,0.1),limits=c(0,1))+
        ylab("Proportion")+scale_fill_discrete(breaks=c("C","PF","F","SF","SG","G","PG"))
    }
    else if(input$graphType == "Count" & input$variables == "Assists made by player"){
      ggplot(data = nba_total,aes(x=playAST,fill=playPos))+geom_histogram(binwidth = 1)+
        scale_x_continuous(name="Assists made by player",breaks=seq(0,20,5))+
        ylab("Count")+scale_fill_discrete(breaks=c("C","PF","F","SF","SG","G","PG"))
    }
    else if(input$graphType == "Proportion" & input$variables == "Assists made by player"){
      ggplot(data = nba_total,aes(x=playAST,fill=playPos))+geom_histogram(position="fill",binwidth = 1)+
        scale_x_continuous(name="Assists made by player",breaks=seq(0,25,5))+
        ylab("Proportion")+scale_fill_discrete(breaks=c("C","PF","F","SF","SG","G","PG"))
    }
    else if(input$graphType == "Count" & input$variables == "Points scored by player"){
      ggplot(data = nba_total,aes(x=playPTS,fill=playPos))+geom_histogram(binwidth = 1)+
        scale_x_continuous(name="Points scored by player",breaks=seq(0,70,5))+
        scale_y_continuous(name="Count",breaks=seq(0,13000,2000))+
        scale_fill_discrete(breaks=c("C","PF","F","SF","SG","G","PG"))
    }
    else if(input$graphType == "Proportion" & input$variables == "Points scored by player"){
      ggplot(data = nba_total,aes(x=playPTS,fill=playPos))+geom_histogram(position="fill",binwidth = 1)+
        scale_x_continuous(name="Points scored by player",breaks=seq(0,70,5))+
        ylab("Proportion")+scale_fill_discrete(breaks=c("C","PF","F","SF","SG","G","PG"))
    }
    else if(input$graphType == "Count" & input$variables == "Rebounds made by player"){
      ggplot(data = nba_total,aes(x=playTRB,fill=playPos))+geom_histogram(binwidth = 1)+
        scale_x_continuous(name="Rebounds made by player",breaks=seq(0,30,2))+
        scale_y_continuous(name="Count",breaks=seq(0,22000,2000))+
        scale_fill_discrete(breaks=c("C","PF","F","SF","SG","G","PG"))
    }
    else if(input$graphType == "Proportion" & input$variables == "Rebounds made by player"){
      ggplot(data = nba_total,aes(x=playTRB,fill=playPos))+geom_histogram(position="fill",binwidth = 1)+
        scale_x_continuous(name="Rebounds made by player",breaks=seq(0,30,2))+
        ylab("Proportion")+scale_fill_discrete(breaks=c("C","PF","F","SF","SG","G","PG"))
    }
    else if(input$graphType == "Count" & input$variables == "Turnovers made by player"){
      ggplot(data = nba_total,aes(x=playTO,fill=playPos))+geom_histogram(binwidth = 1)+
        scale_x_continuous(name="Turnovers made by player",breaks=seq(0,12,1))+
        scale_y_continuous(name="Count",breaks=seq(0,55000,5000))+
        scale_fill_discrete(breaks=c("C","PF","F","SF","SG","G","PG"))
    }
    else if(input$graphType == "Proportion" & input$variables == "Turnovers made by player"){
      ggplot(data = nba_total,aes(x=playTO,fill=playPos))+geom_histogram(position="fill",binwidth = 1)+
        scale_x_continuous(name="Turnovers made by player",breaks=seq(0,12,1))+
        ylab("Proportion")+scale_fill_discrete(breaks=c("C","PF","F","SF","SG","G","PG"))
    }
    else if(input$graphType == "Count" & input$variables == "Personal fouls made by player"){
      ggplot(data = nba_total,aes(x=playPF,fill=playPos))+geom_histogram(binwidth = 1)+
        scale_x_continuous(name="Personal fouls made by player",breaks=seq(0,6,1))+
        scale_y_continuous(name="Count",breaks=seq(0,40000,5000))+
        scale_fill_discrete(breaks=c("C","PF","F","SF","SG","G","PG"))
    }
    else if(input$graphType == "Proportion" & input$variables == "Personal fouls made by player"){
      ggplot(data = nba_total,aes(x=playPF,fill=playPos))+geom_histogram(position="fill",binwidth = 1)+
        scale_x_continuous(name="Personal fouls made by player",breaks=seq(0,6,1))+
        ylab("Proportion")+scale_fill_discrete(breaks=c("C","PF","F","SF","SG","G","PG"))
    }
    else if(input$graphType == "Count" & input$variables == "Steals made by player"){
      ggplot(data = nba_total,aes(x=playSTL,fill=playPos))+geom_histogram(binwidth = 1)+
        scale_x_continuous(name="Steals made by player",breaks=seq(0,10,1))+
        scale_y_continuous(name="Count",breaks=seq(0,90000,10000))+
        scale_fill_discrete(breaks=c("C","PF","F","SF","SG","G","PG"))
    }
    else if(input$graphType == "Proportion" & input$variables == "Steals made by player"){
      ggplot(data = nba_total,aes(x=playSTL,fill=playPos))+geom_histogram(position="fill",binwidth = 1)+
        scale_x_continuous(name="Steals made by player",breaks=seq(0,10,1))+
        ylab("Proportion")+scale_fill_discrete(breaks=c("C","PF","F","SF","SG","G","PG"))
    }
    else if(input$graphType == "Count" & input$variables == "Blocks made by player"){
      ggplot(data = nba_total,aes(x=playBLK,fill=playPos))+geom_histogram(binwidth = 1)+
        scale_x_continuous(name="Blocks made by player",breaks=seq(0,12,1))+
        scale_y_continuous(name="Count",breaks=seq(0,110000,5000))+
        scale_fill_discrete(breaks=c("C","PF","F","SF","SG","G","PG"))
    }
    else if(input$graphType == "Proportion" & input$variables == "Blocks made by player"){
      ggplot(data = nba_total,aes(x=playBLK,fill=playPos))+geom_histogram(position="fill",binwidth = 1)+
        scale_x_continuous(name="Blocks made by player",breaks=seq(0,12,1))+
        ylab("Proportion")+scale_fill_discrete(breaks=c("C","PF","F","SF","SG","G","PG"))
    }
    )
    
    output$plot3 <- renderPlot({
      if(input$location == "Home" & input$stats == "Assist"){
        hist(x=home_data$playAST,main = "Home Assist",xlab = "number of assist")
      }
      else if(input$location == "Home" & input$stats == "Rebound"){
        hist(x=home_data$playTRB,main = "Home Rebound",xlab = "number of rebound")
      }
      else if(input$location == "Away" & input$stats == "Assist"){
        hist(x=away_data$playAST,main = "Away Assist",xlab = "number of assist")
      }
      else if(input$location == "Away" & input$stats == "Rebound"){
        hist(x=away_data$playTRB,main = "Away Rebound",xlab = "number of rebound")
      }
    })
    output$test1 <- renderPrint({
      away_ast <- table(away_data$playAST)
      home_ast <- table(home_data$playAST)
      length(away_ast) <- length(home_ast)
      total_ast <- cbind(away_ast,home_ast)
      total_ast <- as.data.frame(total_ast)
      total_ast[is.na(total_ast)] <- 0
      chisq.test(total_ast)
    })
    output$test2 <- renderPrint({
      away_trb <- table(away_data$playTRB)
      home_trb <- table(home_data$playTRB)
      length(away_trb) <- length(home_trb)
      total_trb <- cbind(away_trb,home_trb)
      total_trb <- as.data.frame(total_trb)
      total_trb[is.na(total_trb)] <- 0
      chisq.test(total_trb)
    })
    output$conclusion <- renderText({
      "By doing the chi square test, we found that the both assist and rebound do not have significance difference between home game and away game."
    })
  }






shinyApp(ui, server)
