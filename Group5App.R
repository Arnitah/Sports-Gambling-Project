#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
# Buisness Analytics tools Open source - Group3
# 
#


#Install all required libraries
if(!require("dplyr")) install.packages("dplyr"); library("dplyr")
if(!require("tidyverse")) install.packages("tidyverse"); library("tidyverse")
if(!require("ggplot2")) install.packages("ggplot2"); library("ggplot2")
if(!require("shiny")) install.packages("shiny"); library("shiny")
if(!require("qdapTools")) install.packages("qdapTools"); library("qdapTools")
if(!require("lubridate")) install.packages("lubridate"); library("lubridate")
if(!require("DT")) install.packages("DT"); library("DT")
if(!require("plotly")) install.packages("plotly"); library("plotly")
if(!require("shinythemes")) install.packages("shinythemes"); library("shinythemes")
if(!require("gganimate")) install.packages("gganimate"); library("gganimate")
if(!require("shinyWidgets")) install.packages("shinyWidgets"); library("shinyWidgets")
if(!require("maptools")) install.packages("maptools"); library("maptools")
if(!require("fBasics")) install.packages("fBasics"); library("fBasics")
if(!require("foreign")) install.packages("foreign"); library("foreign")
if(!require("stringr")) install.packages("stringr"); library("stringr")
if(!require("rsconnect")) install.packages("rsconnect"); library("rsconnect")
if(!require("showtext")) install.packages("showtext"); library("showtext")

#Get Customised font from Google
#font_add_google("Special Elite", family = "special")
#showtext_auto()


#load source data
#DataGroupAssignment <- load("C:/Users/HP PC/Desktop/IESEG_Classes/Buisness Analytics Tools_OPenSource/GroupAssignment/DataGroupAssignment.Rdata")
GApp<-unique(all_products$ApplicationName)

ui <- fluidPage(
  #THEME OF THE PROJECT
  theme = shinytheme("cosmo"),
  #TITLE OF THE PROJECT
  titlePanel(titlePanel("Group Assignment Open Source Programming on Gambling Project Group 5")),
  sidebarLayout(
    sidebarPanel(
      
      pickerInput("Application","Select the applications:",GApp,
                  multiple = TRUE,
                  options = list(`actions-box` = TRUE), selected=GApp),
      
      
      #Filter Segments
    ),

    ###Main program    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(type = "tabs",
                  id= "tabsetpanel", 
                  
                  tabPanel(title = "User Information",
                           splitLayout(plotOutput("Gender_new"),plotOutput("Age_User"))),
                  
                  tabPanel(title = "Player Distribution by Language",
                           plotOutput("language")),
                  
                  tabPanel(title = "Player Distribution by Country",
                           plotOutput("country")),
                  
                  tabPanel(title="Poker Stats",
                           plotOutput("distrib")),
                  
                  tabPanel(title = "Casino Stats",
                           plotOutput("casino_D_plot"),plotOutput("casino_M_plot")),
                  
                  tabPanel(title="Games Stats",
                           plotOutput("games_D_plot"),plotOutput("games_M_plot")),
                  tabPanel(title="Sports Stats",
                           plotOutput("sports_D_plot"),plotOutput("sports_M_plot")),
                  tabPanel(title="supertoto Stats",
                           plotOutput("supertoto_D_plot"),plotOutput("supertoto_M_plot"))
                  
      )
    )
  )
)
server <- function(input, output){

##Plot Sports    
  output$sports_D_plot <- renderPlot({
    sports_Daily<-all_products %>% dplyr::filter (Highest_Played_Day_sports != 0) %>% select (UserID, Highest_Played_Day_sports)
    ggplot(sports_Daily,aes(x = Highest_Played_Day_sports, fill = Highest_Played_Day_sports)) +
      geom_bar() +
      theme_void()+
      labs(title = "Highest Played day in a week for sports", x="Weekdays", y="count")+
      theme(plot.title = element_text(hjust = 0.5))+
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(), 
            panel.background = element_blank(), 
            axis.line = element_line(colour = "black"))
    
  })
  
  output$sports_M_plot <- renderPlot({
    sports_Monthly<-all_products %>% dplyr::filter (Highest_Played_Month_sports != 0) %>% select (UserID, Highest_Played_Month_sports)
    ggplot(sports_Monthly,aes(x = Highest_Played_Month_sports, fill = Highest_Played_Month_sports)) +
      geom_bar() +
      theme_void()+
      labs(title = "Highest Played day in a week for sports", x="Weekdays", y="count")+
      theme(plot.title = element_text(hjust = 0.5))+
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(), 
            panel.background = element_blank(), 
            axis.line = element_line(colour = "black"))
    
  }) 
  
###Plot Games  
  output$games_D_plot <- renderPlot({
    Games_Daily<-all_products %>% dplyr::filter (Highest_Played_Day_games != 0) %>% select (UserID, Highest_Played_Day_games)
    ggplot(Games_Daily,aes(x = Highest_Played_Day_games, fill = Highest_Played_Day_games)) +
      geom_bar() +
      theme_void()+
      labs(title = "Highest Played day in a week for games", x="Weekdays", y="count")+
      theme(plot.title = element_text(hjust = 0.5))+
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(), 
            panel.background = element_blank(), 
            axis.line = element_line(colour = "black"))
    
  })
 
  output$games_M_plot <- renderPlot({
    Games_Monthly<-all_products %>% dplyr::filter (Highest_Played_Month_games != 0) %>% select (UserID, Highest_Played_Month_games)
    ggplot(Games_Monthly,aes(x = Highest_Played_Month_games, fill = Highest_Played_Month_games)) +
      geom_bar() +
      theme_void()+
      labs(title = "Highest Played day in a week for games", x="Weekdays", y="count")+
      theme(plot.title = element_text(hjust = 0.5))+
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(), 
            panel.background = element_blank(), 
            axis.line = element_line(colour = "black"))
    
  }) 
###Plot Casino
  output$casino_D_plot <- renderPlot({
    casino_Daily<-all_products %>% dplyr::filter (Highest_Played_Day_casino != 0) %>% select (UserID, Highest_Played_Day_casino)
    ggplot(casino_Daily,aes(x = Highest_Played_Day_casino, fill = Highest_Played_Day_casino)) +
      geom_bar() +
      theme_void()+
      labs(title = "Highest Played day in a week for casino", x="Weekdays", y="count")+
      theme(plot.title = element_text(hjust = 0.5))+
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(), 
            panel.background = element_blank(), 
            axis.line = element_line(colour = "black"))
    
  })
  
  output$casino_M_plot <- renderPlot({
    casino_Monthly<-all_products %>% dplyr::filter (Highest_Played_Month_casino != 0) %>% select (UserID, Highest_Played_Month_casino)
    ggplot(casino_Monthly,aes(x = Highest_Played_Month_casino, fill = Highest_Played_Month_casino)) +
      geom_bar() +
      theme_void()+
      labs(title = "Highest Played day in a week for casino", x="Weekdays", y="count")+
      theme(plot.title = element_text(hjust = 0.5))+
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(), 
            panel.background = element_blank(), 
            axis.line = element_line(colour = "black"))
    
  })
  
  ####Plot supertoto
  output$supertoto_D_plot <- renderPlot({
    supertoto_Daily<-all_products %>% dplyr::filter (Highest_Played_Day_supertoto != 0) %>% select (UserID, Highest_Played_Day_supertoto)
    ggplot(supertoto_Daily,aes(x = Highest_Played_Day_supertoto, fill = Highest_Played_Day_supertoto)) +
      geom_bar() +
      theme_void()+
      labs(title = "Highest Played day in a week for supertoto", x="Weekdays", y="count")+
      theme(plot.title = element_text(hjust = 0.5))+
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(), 
            panel.background = element_blank(), 
            axis.line = element_line(colour = "black"))
    
  })
  
  output$supertoto_M_plot <- renderPlot({
    supertoto_Monthly<-all_products %>% dplyr::filter (Highest_Played_Month_supertoto != 0) %>% select (UserID, Highest_Played_Month_supertoto)
    ggplot(supertoto_Monthly,aes(x = Highest_Played_Month_supertoto, fill = Highest_Played_Month_supertoto)) +
      geom_bar() +
      theme_void()+
      labs(title = "Highest Played day in a week for supertoto", x="Weekdays", y="count")+
      theme(plot.title = element_text(hjust = 0.5))+
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(), 
            panel.background = element_blank(), 
            axis.line = element_line(colour = "black"))
    
  }) 
  
}

# Run shiny app 
shinyApp(ui = ui, server = server)


