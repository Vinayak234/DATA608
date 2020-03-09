#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/


#
library(shiny)
library(tidyverse)
library(ggplot2)
library(shinythemes)
library(dplyr)

q2data <- read.csv("https://raw.githubusercontent.com/charleyferrari/CUNY_DATA_608/master/module3/data/cleaned-cdc-mortality-1999-2010-2.csv", header = TRUE)

# Define UI


ui <- fluidPage(theme = shinytheme("lumen"),
                titlePanel("Question 2:Diseases by State"),
                sidebarLayout(
                    sidebarPanel(
                        # Select state and diease to plot
                        selectInput(inputId = "ICD.Chapter", label = strong("Disease"),
                                    choices = unique(q2data$ICD.Chapter),
                                    selected = "Certain infectious and parasitic diseases"),
                        
                        selectInput( inputId = "State",label = strong("State"), choices = unique(q2data$State),
                                     selected = "AL")
                        ),
                    # Output: Description, lineplot, and reference
                    mainPanel(
                        plotOutput(outputId = "lineplot", height = "800px", width="600px"),
                        textOutput(outputId = "desc")
                        )
                    )
                )
# Define server function
server <- function(input, output, session) {
    
    national_avg_state <- q2data %>%
        group_by(ICD.Chapter,Year) %>%
        summarise_at(vars(Crude.Rate), mean, na.rm = TRUE)
    
    # Subset data
    
    selected_disease_state <- reactive({
        dfSlice<- q2data %>%
            filter(ICD.Chapter == input$ICD.Chapter, State == input$State)
    })
    # Subset data
    national_disease_state <- reactive({ 
        dfSlice<- national_avg_state  %>%
            filter(ICD.Chapter == input$ICD.Chapter)
    })
    
   
    
    # Create lineplot object the plotOutput function is expecting
    output$lineplot <- renderPlot({
        plot(selected_disease_state()$Crude.Rate~selected_disease_state()$Year,
             main = "Instances of Disease",
             xlab = "Year",
             type="o",
             bty="o",
             ylab = "Rate",
             col=rgb(0.2,0.4,0.1,0.7), 
             lwd=1.5 , pch=17, ylim=c(1, 40)
        )
        lines(national_disease_state()$Crude.Rate~national_disease_state()$Year , col=rgb(0.8,0.4,0.1,0.7) , lwd=1.5 , pch=19 , type="b" )
        legend("bottomleft", 
               legend = c("State Crude Rate", "National Avg Crude Rate"), 
               col = c(rgb(0.2,0.4,0.1,0.7), 
                       rgb(0.8,0.4,0.1,0.7)), 
               pch = c(17,19), 
               bty = "n", 
               pt.cex = 2, 
               cex = 1.2, 
               text.col = "black", 
               horiz = F , 
               inset = c(0.1, 0.1))
    })
}
# Create Shiny object
shinyApp(ui = ui, server = server)

