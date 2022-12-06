library(shiny)
library(shinydashboard)
library(readr)
library(dplyr)
library(ggplot2)


ui <- fluidPage(
  dashboardPage(dashboardHeader(title = 'Indian Water Quality', titleWidth = 650),
                dashboardSidebar(
                  sidebarMenu(
                    id = "Sidebar",
                    menuItem("Introduction", tabName = "Intro", icon= icon("bars")),
                    menuItem("Parameters", tabName = "para", icon= icon("chart-pie")),
                    conditionalPanel("input.Sidebar == 'para' && input.t1 == 'part'", selectInput(inputId = "var1" , label ="Select Parameter" , choices = c1)),
                    menuItem("Analysis", tabName = "state", icon = icon("chart-line")),
                    
                    menuItem("Inferences", tabName = "Inf", icon = icon("bars"))
                    
                  )), 
                dashboardBody(
                  tabItems(
                    tabItem(tabName = "Intro",tabBox(id = "t", width = 12, tabPanel(title = "Objectives", icon = icon("align-center"),
                             align = "centre", tags$p("Water is existential for life on Earth. It plays an important role in sustaining communities, economies, and societies. 
                                                      The availability of clean drinking water is an essential need for all living beings. Quality of drinking water is among our prime concerns since the increasing changes in the environment and industrial sectors.
                                                      In this study, we try to analyse the water quality affected data from 2009 to 2012 and find out if there is any significant change in the water quality over the years."),
                             tags$p("1.	Observing the quality parameter most affecting the water quality in India."),
                             tags$p("2.	Observing the trend shown by each parameter over the years."),
                             tags$p("3.	Observing the proportion of parameters over the states."),
                             tags$p("4.	Observing the water quality of each state and the parameter that affects maximum."),
                             tags$p("5.	Observing the district wise and block wise water quality of each state.")),
                                   
                            tabPanel(title = "Dataset", icon = icon("table"),tags$p("The dataset used in this study is taken from Kaggle. The data contains areas with affected water quality for the years 2009, 2010, 2011 and 2012. These data
                                    sets identify the state, district and specific localities in which water quality degradation has been reported
                                    in that particular year."),
                                    tags$p("The key parameters observed in different regions over the years are:"),
                                    tags$strong("1. Salinity:"), tags$p("Salinity affects production in crops, pastures, and trees by  interfering with nitrogen uptake, reducing growth and stopping plant reproduction."),
                                    tags$strong("2. Fluoride"),tags$p("Excess amounts of fluoride ions in drinking water can cause health conditions like skeletal fluorosis, arthritis, bone damage, osteoporosis, etc."),
                                    tags$strong("3. Iron"),tags$p("High amounts of iron in water can cause diabetes, hemochromatosis, stomach problems and nausea."),
                                    tags$strong( "4. Arsenic"),tags$p("Long term exposure to arsenic from drinking water can cause cancer and skin lesions."), 
                                    tags$strong("5. Nitrate"), tags$p("High levels of nitrate in drinking water may increase the risk to colon cancer."),uiOutput("dataset"))
                            
                                    
                              
                            )),
                    tabItem(tabName = "para",tabBox(id = "t1", width =12,
                                                     tabPanel("General",value = "gen" ,icon = icon("bars"),
                                                              splitLayout(cellWidths = c("50%", "50%"), plotOutput("plotgraph1"), plotOutput("plotgraph2"))), 
                                                     tabPanel("Particular",value = "part", icon= icon("chart-simple"),plotOutput("Paraplot") ),
                             )),
                    tabItem(tabName = "state", tags$h3("Analysis of water quality within each state:"),tags$br(),selectInput(inputId = "statevar", label = "Select a State", choices = states),
                                                   splitLayout(  plotOutput("stateplot1"), plotOutput("stateplot2")),tags$br(),
                            tags$h3("Analysis within Districts:"),tags$br(),
                            uiOutput("Distr"), splitLayout(plotOutput("distplot1"), plotOutput("distplot2")), tags$br(), tags$br(),tags$h3("Analysis within blocks:"),
                            uiOutput("Block"), plotOutput("blockplot")),
                    tabItem(tabName = "Inf", tags$p("We analysed the Water Quality data set and arrived at the following conclusions:"),tags$br(), 
                            tags$p("1. The parameter that occurs the most is Iron, followed by Salinity, Fluoride and Arsenic respectively"), 
                            tags$p("2. An overall decrease is seen in each parameter over the years."),
                            tags$p("3. The parameter that affects water quality at most varies from state to state."),
                            tags$p("4. The most affected state is Rajasthan, followed by Bihar. Assam and Orrisa comes in the third and fourth positions respectively."),
                            tags$p("5. The proportion of parameters varies from state to state "),
                            tags$p("6. A general downward trend in quality paraeters over the years show that overall water quality is increasing over the years. "))
                    
                   
                ))))
  
  

    
        
    



server <- function(input, output) {
  
output$dataset <- renderUI({
  tagList("Link:", url)})  
output$plotgraph1 <- renderPlot( data %>% ggplot(aes(x = Quality.Parameter) ) +geom_bar(fill = "aquamarine3")+ 
                                   labs(title = "Occurrence of Parameters", x = "Quality parameter", y = "Count")+
                                   theme(plot.title=element_text(hjust = 0.5, face = "bold"),axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
                                   scale_y_continuous(label = scales::comma))
output$plotgraph2 <- renderPlot(freq %>% ggplot(aes( y = Freqs, x = Year, col = factor(Parameter))) + geom_point()+ geom_smooth(method = "loess", formula = y~x)+  facet_grid(~Parameter)+ 
                                  labs(title = "Trend seen in parameters over the years", x = "Years", y = "Count",
                                       col = "Parameter")+
                                  theme(plot.title = element_text(face= "bold", hjust = 0.5),
                                        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)))
output$Paraplot <-  renderPlot(subset(data, data$Quality.Parameter == input$var1) %>% ggplot(aes(x = State.Name, fill = Year)) + geom_bar(position = "dodge")+
                                 theme(plot.title = element_text(face = "bold",hjust=0.5),
                                       axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
                                 labs(title = "State-wise distribution over the years", x = "States" ,
                                      y = "Count", fill = " Year") )

output$stateplot1 <- renderPlot(subset(data, data$State.Name == input$statevar) %>% ggplot(aes(x = District.Name, fill = Year))+geom_bar(position = "dodge")+
                                  theme(plot.title = element_text(face = "bold", hjust = 0.5),
                                        axis.text.x = element_text(angle = 90,vjust = 0.5, hjust = 1))+
                                  labs(title = "Water Quality Over The Years", x = "Districts", y = "Count", fill = "Year"))
output$stateplot2 <- renderPlot(subset(data, data$State.Name == input$statevar) %>% ggplot(aes(x = Quality.Parameter, fill = Year)) +geom_bar(position ="dodge")+
                                  theme(plot.title = element_text(face = "bold", hjust = 0.5),
                                        axis.text.x =  element_text(angle = 90, vjust = 0.5, hjust = 1))+
                                  labs(title = "Presence Of Parameters Over the Years", x = "Parameters", y = "count", fill = "Year"))
output$Distr <- renderUI(selectInput(inputId = "distvar", label = "Select a District", choices = distinct(subset(data, data$State.Name== input$statevar), District.Name)))
output$distplot1 <- renderPlot(subset(freq2, District == input$distvar) %>% ggplot(aes(x = Year,y = Freq ,colour = Parameter)) +geom_point()+ geom_line(aes(group = Parameter))+
                                 theme(plot.title = element_text(face = "bold", hjust = 0.5))+
                                 labs(title = "Parameters Over The Years", x = "Years", y = "Count", colour = "Parameter"))
output$distplot2 <- renderPlot(subset(data, State.Name == input$statevar & District.Name == input$distvar) %>% ggplot(aes(x = Block.Name, fill = Quality.Parameter))+geom_bar(position = "dodge")+
                                 theme(plot.title = element_text(face = "bold", hjust = 0.5),
                                       axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
                                 labs(x = "Blocks", y = "Count", fill = "Parameters"))    
output$Block <- renderUI(selectInput(inputId = "blockvar", label = "Select a Block", choices = distinct(subset(data, data$State.Name== input$statevar & data$District.Name== input$distvar), Block.Name)))

output$blockplot <- renderPlot(subset(freq3, Block == input$blockvar) %>% 
                                 ggplot(aes(x = Year,y = Freq ,colour = Parameter )) +geom_point()+ geom_line(aes(group = Parameter))+
                                 theme(plot.title = element_text(face = "bold", hjust = 0.5))+
                                 labs(title = "Parameters Over The Years", x = "Years", y = "Count", colour = "Parameter"))
                                 
 }      

shinyApp(ui = ui, server = server)
