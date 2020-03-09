#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(rsconnect)
library(shiny)
library(plotly)
library(ggplot2)
library(shinythemes)
library(dplyr)
#library(ExPanDaR)


data <- read.csv("https://raw.githubusercontent.com/miasiracusa/CUNY_DATA_608/master/module3/data/cleaned-cdc-mortality-1999-2010-2.csv", header = TRUE)

q1data <- filter(data, data$Year == 2010)
shinyApp(
    ui = tagList(
        shinythemes::themeSelector(),
        navbarPage(
            # theme = "cerulean",  # <--- To use a theme, uncomment this
            "DATA608 Module 3: ",
            tabPanel("Question 1",
                     sidebarPanel(
                         # Select type of trend to plot
                         selectInput(inputId = "ICD.Chapter1", label = strong("Disease"),
                                     choices = unique(q1data$ICD.Chapter),
                                     selected = "Certain infectious and parasitic diseases")
                     ),
                     # Output: Description, lineplot, and reference
                     mainPanel(
                         plotOutput(outputId = "barplot", height = "800px", width="600px"),
                         textOutput(outputId = "desc")
                     )
            
            ),
            tabPanel("Question 2", 
                     sidebarPanel(
                         # Select state and diease to plot
                         selectInput(inputId = "ICD.Chapter2", label = strong("Disease"),
                                     choices = unique(data$ICD.Chapter),
                                     selected = "Certain infectious and parasitic diseases"),
                         
                         selectInput( inputId = "State2",label = strong("State"), choices = unique(data$State),
                                      selected = "AL")
                     ),
                     # Output: Description, lineplot, and reference
                     mainPanel(
                         plotOutput(outputId = "lineplot", height = "800px", width="600px"),
                         textOutput(outputId = "desc")
                     )
            )
        
        )
    ),
server = function(input, output) {
        selected_disease <- reactive({
            dfSlice <- q1data %>%
                filter(ICD.Chapter == input$ICD.Chapter1)%>%
                arrange(Crude.Rate)
        })
        
        
        # Create barplot object the plotOutput function is expecting
        output$barplot <- renderPlot({
                barplot(selected_disease()$Crude.Rate,
                    main="Instances of Disease",
                    names.arg = selected_disease()$State, 
                    xlab="Crude Rate", 
                    ylab = "State",
                    font.axis=2,
                    col = "#69b3a2",
                    horiz = TRUE,
                    cex.axis=1.5,
                    las=1)
        }
        
        )
        
        
        national_avg_state <- data %>%
            group_by(ICD.Chapter,Year) %>%
            summarise_at(vars(Crude.Rate), mean, na.rm = TRUE)
        
        # Subset data
        
        selected_disease_state <- reactive({
            dfSlice<- data %>%
                filter(ICD.Chapter == input$ICD.Chapter2, State == input$State2)
        })
        # Subset data
        national_disease_state <- reactive({ 
            dfSlice<- national_avg_state  %>%
                filter(ICD.Chapter == input$ICD.Chapter2)
        })
        

        # Create lineplot object the plotOutput function is expecting
        output$lineplot <- renderPlot({
            plot(selected_disease_state()$Crude.Rate~selected_disease_state()$Year,
                 main = "Instances of Disease",
                 xlab = "Rate",
                 type="o",
                 bty="o",
                 ylab = "Year",
                 col=rgb(0.2,0.4,0.1,0.7), 
                 lwd=1.5 , pch=20, ylim=c(1, 40)
            )
            lines(national_disease_state()$Crude.Rate~national_disease_state()$Year , col=rgb(0.8,0.4,0.1,0.7) , lwd=3 , pch=19 , type="b" )
        })
        
    }
)
