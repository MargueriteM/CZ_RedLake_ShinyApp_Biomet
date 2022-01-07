
### Welcome! This is a Shiny web application. 
#### This app shows biomet data collected by the Dryland Critical Zone Project at Red Lake Playa.

# Written by: Marguerite Mauritz
# Created: 11 August 2021
# Last update: 7 Jan 2022

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Red Lake Playa Biomet Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        # dateRangeInput("daterange", "Date range:",
        #                start = min(biomet.long$date),
        #                end   = max(biomet.long$date),
        #                min = min(biomet.long$date),
        #                max = max(biomet.long$date)),
        
        selectInput("variable1", "Biomet variable 1:",
                    c("Atmospheric Pressure" = "^PA",
                      "Soil Moisture" = "^SWC",
                      "Air Temperature" = "^TA",
                      "Soil Temperature" = "^TS",
                      "Canopy Temperature" ="^TC",
                      "Rainfall" =  "^P_RAIN",
                      "Leaf wetness" = "^LWS",
                      "Longwave Radiation" = "^LWIN|^LWOUT",
                      "Shortwave Radiation" = "^SWIN|^SWOUT",
                      "Albedo" = "^ALB",
                      "Global Radiation & PAR" = "^RG|^PPFD",
                      "Net Radiation" = "^RN")),
        
        selectInput("variable2", "Biomet variable 2:",
                    c("Atmospheric Pressure" = "^PA",
                      "Soil Moisture" = "^SWC",
                      "Air Temperature" = "^TA",
                      "Soil Temperature" = "^TS",
                      "Canopy Temperature" ="^TC",
                      "Rainfall" =  "^P_RAIN",
                      "Leaf wetness" = "^LWS",
                      "Longwave Radiation" = "^LWIN|^LWOUT",
                      "Shortwave Radiation" = "^SWIN|^SWOUT",
                      "Albedo" = "^ALB",
                      "Global Radiation & PAR" = "^RG|^PPFD",
                      "Net Radiation" = "^RN")),
        
        selectInput("variable3", "Radiation Components:",
                    c("Longwave Radiation" = "^LWIN|^LWOUT",
                      "Shortwave Radiation" = "^SWIN|^SWOUT",
                      "Albedo" = "^ALB",
                      "Global Radiation & PAR" = "^RG|^PPFD",
                      "Net Radiation" = "^RN")),
        
        radioButtons("ax.scales", "Axis scales:",
                     c("Fixed" = "fixed", 
                       "Free x" = "free_x",
                       "Free y" = "free_y",
                       "Free x & y" = "free"))
        
       ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("plot1"),
         plotOutput("plot2"),
         plotOutput("plot3")
      )
   )
)

# Define server logic required to draw a histogram
# load libraries and data
# load libraries
library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)
library(data.table)

# get windrose function, source directly from Github
#source("~/Desktop/R/R_programs/Functions/plot.windrose.R")
source(paste0("https://raw.githubusercontent.com/MargueriteM/R_functions/master/plot.windrose.R"))

# set working directory to One Drive folder with data (folder belongs to Marguerite Mauritz)
setwd("~/Desktop/OneDrive - University of Texas at El Paso/Tower Data/JER_Playa/Data/Biomet")

# read column names and import data
biomet.head <- colnames(read.table("CR3000 Red Lake Remote Connect_Biomet.dat", sep=",", dec=".", skip=1, header=TRUE))
biomet <- read.table("CR3000 Red Lake Remote Connect_Biomet.dat", sep=",", dec=".", skip=4, header=FALSE,
                     col.names=biomet.head, na.strings=c("NAN"))

# change data to long format
biomet.long <- biomet %>%
  pivot_longer(!c(TIMESTAMP,RECORD), names_to="variable",values_to="value") %>%
  mutate(TIMESTAMP = ymd_hms(TIMESTAMP),
         date=as.Date(TIMESTAMP))

# create nicer plot titles
plot.titles <- data.frame(selection1 = c("^PA",
                                         "^SWC",
                                         "^TS",
                                         "^TA",
                                         "^TC",
                                         "^P_RAIN",
                                         "^LWS",
                                         "^SWIN|^SWOUT",
                                         "^ALB",
                                         "^RG|^PPFD",
                                         "^RN"),
                          selection2 = c("^PA",
                                         "^SWC",
                                         "^TS",
                                         "^TA",
                                         "^TC",
                                         "^P_RAIN",
                                         "^LWS",
                                         "^SWIN|^SWOUT",
                                         "^ALB",
                                         "^RG|^PPFD",
                                         "^RN"),
                          selection3 = c("^PA",
                                         "^SWC",
                                         "^TS",
                                         "^TA",
                                         "^TC",
                                         "^P_RAIN",
                                         "^LWS",
                                         "^SWIN|^SWOUT",
                                         "^ALB",
                                         "^RG|^PPFD",
                                         "^RN"),
                          name = c("Atmospheric Pressure",
                                   "Soil Moisture Content (% VWC)",
                                   "Soil Temperature (C)",
                                   "Air Temperature (C)",
                                   "Canopy Temperature (C)",
                                   "Rainfall (mm)",
                                   "Leaf Wetness",
                                   "Shortwave Radiation",
                                   "Albedo",
                                   "Global Radiation & PAR",
                                   "Net Radiation"))


server <- function(input, output) {
   
   output$plot1 <- renderPlot({
     c <-  biomet.long %>%
       filter(str_detect(variable,input$variable1))# &
               # (date >= input$daterange[[1]] & date <= input$daterange[[2]])) 
     
     d <- plot.titles %>%
       filter(selection1 == input$variable1)
     # setnames(c,input$variable1,"selected")
     
     # plot
     ggplot(c, aes(TIMESTAMP, value))+
       geom_line()+
       labs(title = d$name, y=d$name)+
       facet_grid(variable~., scales = input$ax.scales)+
       theme_bw()
    
   })
   
   output$plot2 <- renderPlot({
     c <-  biomet.long %>%
       filter(str_detect(variable,input$variable2))# &
               # (date >= input$daterange[[1]] & date <= input$daterange[[2]])) 
     
     d <- plot.titles %>%
       filter(selection2 == input$variable2)
     # setnames(c,input$variable1,"selected")
     
     # plot
     ggplot(c, aes(TIMESTAMP, value))+
       geom_line()+
       labs(title = d$name, y=d$name)+
       facet_grid(variable~., scales = input$ax.scales)+
       theme_bw()})
   
   output$plot3 <- renderPlot({
     c <-  biomet.long %>%
       filter(str_detect(variable,input$variable3))# &
               # (date >= input$daterange[[1]] & date <= input$daterange[[2]])) 
     
     d <- plot.titles %>%
       filter(selection3 == input$variable3)
     # setnames(c,input$variable1,"selected")
     
     # plot
     ggplot(c, aes(TIMESTAMP, value, colour=variable))+
       geom_line()+
       labs(title = d$name, y=d$name)+
       facet_grid(input$variable3~.)+
       theme_bw()+
       theme(legend.position = "bottom")})
}

# Run the application 
shinyApp(ui = ui, server = server)

