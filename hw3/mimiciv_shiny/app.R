#---- Load packages ----
library(shiny)
library(tidyverse)

#---- Load data ----
icu_cohort <- readRDS("icu_cohort.rds")

#---- User interface ----
ui <- fluidPage(
  # APP title
  titlePanel("MIMIC-IV Data Descriptive Summary"),
  
  tabsetPanel(
    type = "tabs",
    
    tabPanel("Demographics", 
             sidebarLayout(sidebarPanel(),
                           mainPanel()
             ),
             plotOutput("")
    ),
    
    tabPanel("Labotory results", 
             sidebarLayout(
               sidebarPanel(
                 selectInput("varLabs", "Lab measurement:",
                             c("bicarbonate", "calcium", "chloride", 
                               "creatinine", "glucose", "magnesium", 
                               "potassium", "sodium", "hematocrit",
                               "wbc", "lactate"),
                             selected = "bicarbonate")
               ),
               mainPanel(
                 textOutput("missingLabs"),
                 textOutput("rangeLabs"),
                 helpText("Boxplot:"),
                 plotOutput("boxPlotlabs"),
                 helpText("Histogram:"),
                 sliderInput("binsLabs", 
                             "Number of bins:",
                             min = 1,
                             max = 50,
                             value = 30),
                 plotOutput("distPlotLabs")
               )
             )
    ),
    
    tabPanel("Vital status", 
             sidebarLayout(
               sidebarPanel(
                 selectInput("varVitals", "Vitals:",
                             c("heart_rate", 
                               "non_invasive_blood_pressure_systolic",
                               "non_invasive_blood_pressure_mean",
                               "respiratory_rate",
                               "temperature_fahrenheit",
                               "wrterial_blood_pressure_systolic",
                               "arterial_blood_pressure_mean",
                               "flag_die30"),
                             selected = "heart_rate")
               ),
               mainPanel(
                 textOutput("missingLabs"),
                 textOutput("rangeLabs"),
                 helpText("Boxplot:"),
                 plotOutput("boxPlotlabs"),
                 helpText("Histogram:"),
                 sliderInput("binsLabs", 
                             "Number of bins:",
                             min = 1,
                             max = 50,
                             value = 30),
                 plotOutput("distPlotLabs")
               )
             )
    )
    
    
    
  )
)


#---- Server logic ----
server <- function(input, output) {
  
}


#---- Run the app ----
shinyApp(ui = ui, server = server)