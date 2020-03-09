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
library(tidyverse)
library(plotly)
library(ggplot2)
library(shinythemes)
library(dplyr)
#library(ExPanDaR)


data <- read.csv("https://raw.githubusercontent.com/charleyferrari/CUNY_DATA_608/master/module3/data/cleaned-cdc-mortality-1999-2010-2.csv", header = TRUE)

q1data <- filter(data, data$Year == 2010)


# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("lumen"),
                titlePanel("Question 1: Disease Rates in America"),
                sidebarLayout(
                    sidebarPanel(
                        # Select type of trend to plot
                        selectInput(inputId = "ICD.Chapter", label = strong("Disease"),
                                    choices = unique(q1data$ICD.Chapter),
                                    selected = "Certain infectious and parasitic diseases")
                    ),
                    # Output: Description, lineplot, and reference
                    mainPanel(
                        plotOutput(outputId = "barplot", height = "800px", width="600px"),
                        textOutput(outputId = "desc")
                    )
                )
)
# Define server logic required to draw a histogram
server <- function(input, output, session) {
     
    # Subset data
    selected_disease <- reactive({
        dfSlice <- q1data %>%
            filter(ICD.Chapter == input$ICD.Chapter)%>%
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
}

# Run the application 
shinyApp(ui = ui, server = server)
