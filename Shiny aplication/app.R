# --------------------------------------------------------------
# Load libraries
# --------------------------------------------------------------
library(shiny)
library(ggplot2)
library(shinydashboard)
library(dplyr)
library(DT)
library(tidyverse)
library(lubridate)

# --------------------------------------------------------------
# Load the dataSet
# --------------------------------------------------------------

#Upload the DataMarketing
load("DataMartMarketing.RData")

# --------------------------------------------------------------
# Dashboard function
# --------------------------------------------------------------
ui <- dashboardPage(
  dashboardHeader(title = "OG-Group 9"),
  dashboardSidebar(
    
    sidebarMenu(id='sideBarMenu',
                menuItem("Customers", tabName = "customers", icon = icon("user")),
                menuItem("Analysis", tabName = "analysis_customers", icon = icon("chart-column",class = NULL,lib="font-awesome")),
                menuItem("Segmentation Details", tabName = "insights", icon = icon("table"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "customers",
              h2("Customer information"),
              fluidRow(
                valueBox("41,186", "Users",  color = "yellow", icon = icon("users")),
                valueBox(79, "Nationalities", color = "yellow", icon = icon("globe",class = NULL,lib="font-awesome")),
                valueBox(17, "Languages", color = "yellow",icon = icon("language",class = NULL,lib="font-awesome"))
              ),
              tabsetPanel(type = "tabs", selected = "Trend Activation",
                          tabPanel("Gender",
                                   fluidRow(
                                     plotOutput("graph_gender")
                                   )
                          ),
                          tabPanel("Country",
                                   fluidRow(
                                     plotOutput("graph_countries")
                                   )
                                   
                          ),
                          tabPanel("Language",
                                   fluidRow(
                                     plotOutput("graph_language")
                                   )),
                          tabPanel("Trend Activation",
                                   fluidRow(
                                     hr(),
                                     column(3,sliderInput('day', 'Select days', 1, 30, 26, step = 5)),
                                     hr(),
                                     plotOutput("trend_gender_activeDay")
                                   ))
              )
      ),
      
      #Second tab content
      tabItem(tabName = "analysis_customers",
              h2("Customer analysis"),
              fluidRow(
                valueBox("13,361.26 €", "Highest amout of individual purchase poker",  color = "yellow",
                         icon = icon("money-bill",lib="font-awesome")),
                valueBox("1,093,207 €", "Highest amount of individual win", color = "teal",
                         icon = icon("trophy")),
                valueBox("1,127,070 €", "Highest amount of individual stake", color = "teal",
                         icon = icon("handshake-simple",class = NULL,lib="font-awesome")),
                valueBox("193,137", "Highest individual number of times bet", color = "teal",
                         icon = icon("dice"))
              ),
              tabsetPanel(type = "tabs", selected = "Bettings RFM",
                          box(selectInput('Country_Name', 'Select country', choices = sort(unique(DataMartMarketing$Country_Name)), selected = 'Germany')),
                          tabPanel("Bettings RFM",
                                   fluidRow(
                                     plotOutput("rfm_bettings")
                                   )
                          ),
                          tabPanel("Poker RFM",
                                   fluidRow(
                                     plotOutput("rfm_poker")
                                   )
                          )
              )
      ),
      
      #Third tab content
      tabItem(tabName = "insights",
              h2(""),
              fluidRow(
                valueBox("23%", "Champions in Bettings",  color = "yellow",
                         icon = icon("medal",class = NULL,lib="font-awesome")),
                valueBox("25%", "Champions in Poker", color = "teal",
                         icon = icon("medal",class = NULL,lib="font-awesome")),
                valueBox("4%", "New customer in Bettings", color = "yellow",
                         icon = icon("thumbs-up",class = NULL,lib="font-awesome")),
                valueBox("1%", "New customer in Poker", color = "teal",
                         icon = icon("thumbs-up",class = NULL,lib="font-awesome"))
              ),
              tabsetPanel(type = "tabs", selected = "Betting Segmentation",
                          tabPanel("Betting Segmentation",
                                   hr(),
                                   selectInput('segmentation1', 'Select Segment', unique(na.omit(DataMartMarketing$Loyalty_segmentation_bettings))),
                                   hr(),
                                   DT::DTOutput('df_Betting'),
                                   #Add buttom of download
                                   downloadButton("downloadBettingCsv", "Download as CSV")
                          ),
                          tabPanel("Poker Segmentation",
                                   hr(),
                                   selectInput('segmentation2', 'Select Segment', unique(na.omit(DataMartMarketing$Loyalty_segmentation_pokerChip))),
                                   hr(),
                                   DT::DTOutput('df_Poker'),
                                   #Add buttom of download
                                   downloadButton("downloadPokerCsv", "Download as CSV")
                                   
                          )
              ),
              
      )
    )
  )
)

# --------------------------------------------------------------
# Dashboard server
# --------------------------------------------------------------

server <- function(input, output) {
  
  #Outplot Gender
  output$graph_gender <- renderPlot({
    DataMartMarketing%>%
      group_by(Gender)%>%
      summarise(Count = n())%>%
      ungroup()%>%
      mutate(Gender = factor(Gender, levels = c("Female", "Male")))%>%
      ggplot(aes(x=Gender, y=Count))+
      geom_segment(aes(xend=Gender, yend=0)) +
      geom_point( size=9, color="orange") +
      theme_bw() +
      xlab("")+
      ylab("Frecuency")+
      labs(title = "Gender distribution of users")+
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  #Output plot country
  output$graph_countries <- renderPlot({
    DataMartMarketing%>%
      group_by(Country_Name)%>%
      summarise(NCountry = n())%>%
      arrange(desc(NCountry))%>%
      slice(1:10)%>%
      ungroup()%>%
      mutate(Country_Name = fct_reorder(Country_Name, NCountry))%>%
      ggplot(aes(x=Country_Name, y=NCountry))+
      geom_segment( aes(xend=Country_Name, yend=0)) +
      geom_point( size=6, color="orange") +
      coord_flip() +
      theme_bw() +
      xlab("")+
      ylab("Frecuency")+
      labs(title = "Top 10 country distribution of users")+
      theme(plot.title = element_text(hjust = 0.5))
  })
  #output plot language
  output$graph_language <- renderPlot({
    DataMartMarketing%>%
      group_by(Language_Description)%>%
      summarise(Count = n())%>%
      arrange(desc(Count))%>%
      slice(1:17)%>%
      ungroup()%>%
      mutate(Language_Description = fct_reorder(Language_Description, Count))%>%
      ggplot(aes(x=Language_Description, y=Count))+
      geom_segment( aes(xend=Language_Description, yend=0)) +
      geom_point( size=6, color="orange") +
      coord_flip() +
      theme_bw() +
      xlab("")+
      ylab("Frecuency")+
      labs(title = "Language distribution of users")+
      theme(plot.title = element_text(hjust = 0.5))
  })
  #Trend gender first active day
  output$trend_gender_activeDay <- renderPlot({
    DataMartMarketing%>%
      mutate(day = day(First_Active_Date))%>%
      group_by(Gender, day)%>%
      summarise(count=n())%>%
      ggplot()+
      geom_line(aes(x=day,y=count, color = Gender))+
      xlim(c(1,input$day))+
      theme_bw()+
      xlab("Days") +
      ylab("Frecuency") +
      labs(title = "First active day distribution by gender")+
      theme(plot.title = element_text(hjust = 0.5))
  })
    
    
  #Output plot RFM Betting
  output$rfm_bettings <- renderPlot({
    DataMartMarketing%>%
      arrange(Country_Name)%>%
      filter(Country_Name == input$Country_Name)%>%
      filter(!is.na(Loyalty_segmentation_bettings))%>%
      ggplot() + geom_bar(aes(x = factor(Loyalty_segmentation_bettings, level = c('Can’t Lose Them',"At Risk Customers","New Customers","Potential Loyalists","Champions")),
                              fill = Loyalty_segmentation_bettings),
                          fill="lightblue",width=0.2)+
      theme_bw() +
      coord_flip() +
      theme(axis.text.x=element_text(angle=0,hjust=1), legend.position="none") +
      scale_fill_hue(c = 40) +
      xlab("")+
      ylab("Frecuency") +
      labs(title = "Loyalty Segmentation of Bettings")+
      theme(plot.title = element_text(hjust = 0.5))
  })

  #Output plot RFM Poker
  output$rfm_poker <- renderPlot({
    DataMartMarketing%>%
      arrange(Country_Name)%>%
      filter(Country_Name == input$Country_Name)%>%
      filter(!is.na(Loyalty_segmentation_pokerChip))%>%
      ggplot() + geom_bar(aes(x = factor(Loyalty_segmentation_pokerChip, level = c('Can’t Lose Them',"At Risk Customers","New Customers","Potential Loyalists","Champions"))
                              , fill = Loyalty_segmentation_pokerChip),fill="orange",width=0.2) +
      theme_bw() +
      coord_flip() +
      theme(axis.text.x=element_text(angle=0,hjust=1), legend.position="none") +
      scale_fill_hue(c = 40) +
      xlab("")+
      ylab("Frecuency") +
      labs(title = "Loyalty Segmentation of Poker")+
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  #Output interactive table Betting
  output$df_Betting <- DT::renderDT({  
    DataMartMarketing %>% 
      filter(Loyalty_segmentation_bettings == input$segmentation1) %>% 
      select(UserID, Gender, Country_Name, Language_Description, Application_Description, LOR, Total_Bets, final_balance)
  })
  
  #Output interactive table Poker
  output$df_Poker <- DT::renderDT({  
    DataMartMarketing %>% 
      filter(Loyalty_segmentation_pokerChip == input$segmentation2) %>% 
      select(UserID, Gender, Country_Name, Language_Description, Application_Description, LOR, Total_Bets, final_balance)
  })
  
  #Output button Betting
  output$downloadBettingCsv <- downloadHandler(
    filename = "ReportBettingSegmentation.csv",
    content = function(file) {
      write.csv(DataMartMarketing %>% 
                  filter(Loyalty_segmentation_bettings == input$segmentation1) %>% 
                  select(UserID, Gender, Country_Name, Language_Description, Application_Description, LOR, Total_Bets, final_balance), file)
    },
    contentType = "text/csv"
  )
  
  #Output button Poker
  output$downloadPokerCsv <- downloadHandler(
    filename = "ReportPokerSegmentation.csv",
    content = function(file) {
      write.csv(DataMartMarketing %>% 
                  filter(Loyalty_segmentation_pokerChip == input$segmentation2) %>% 
                  select(UserID, Gender, Country_Name, Language_Description, Application_Description, LOR, Total_Bets, final_balance), file)
    },
    contentType = "text/csv"
  )
  
}

shinyApp(ui = ui, server = server)

