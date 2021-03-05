#---- Load packages ----
library(shiny)
library(tidyverse)

# Source helpers ----
source("helpers.R")

#---- Load data ----
icu_cohort <- readRDS("icu_cohort.rds")

#---- Set up variables ----
# colnames(icu_cohort)
Admissionsvar <- c("admission_type", "flag_die_30", "insurance")
Demovar <- c("age_at_admission", "gender", "ethnicity", "language", 
             "insurance", "marital_status")
Labvar <- c("bicarbonate", "calcium", "chloride", "creatinine", "glucose", 
            "magnesium", "potassium", "sodium", "hematocrit", "wbc", "lactate")
Vitalvar <- c("heart_rate", 
                 "non_invasive_blood_pressure_systolic",
                 "non_invasive_blood_pressure_mean",
                 "respiratory_rate",
                 "temperature_fahrenheit",
                 "wrterial_blood_pressure_systolic",
                 "arterial_blood_pressure_mean",
                 "flag_die30")

#---- User interface ----
ui <- fluidPage(
  # APP title
  titlePanel("MIMIC-IV Data Descriptive Summary"),
  
  tabsetPanel(
    type = "tabs",
    # Demographic Panel
    tabPanel("Demographics", 
             sidebarLayout(
               sidebarPanel(
                 # variables input
                 selectInput(inputId = "Demovar", 
                             label = "Demographic information:",
                             choices = Demovar,
                             selected = "age_at_admission"),
                 # stratified by admission types
                 checkboxInput(inputId = "stratified", 
                               label = "Stratified",
                               value = FALSE),
                 selectInput(inputId = "Admissionsvar",
                             label = "Stratified by:",
                             choices = Admissionsvar,
                             # selected = NULL,
                             selected = "flag_die_30")
               ),
               mainPanel(
                 # summary statistics table
                 tableOutput("tableDemo"),
                 helpText("Distribution:"),
                 # sliderInput("binsDemo",
                 #             "Number of bins:",
                 #             min = 1,
                 #             max = 50,
                 #             value = 30),
                 plotOutput("plotDemo")
                 # helpText("Boxplot:"),
                 # plotOutput("boxplotDemo"),
                 )
             ),
             
    ),
    
    # Laboratory Panel
    tabPanel("Laboratory results", 
             sidebarLayout(
               sidebarPanel(
                 # variables input
                 selectInput(inputId = "Labvar ", 
                             label = "Lab measurement:",
                             choices = Labvar,
                             selected = "bicarbonate"),
                 # stratified by admission types
                 checkboxInput(inputId = "stratified", 
                               label = "Stratified",
                               value = FALSE),
                 selectInput(inputId = "Admissionsvar",
                             label = "Stratified by:",
                             choices = Admissionsvar,
                             # selected = NULL,
                             selected = "flag_die_30")
               ),
              mainPanel(
                 # summary statistics table
                 tableOutput("tablelab"),
                 helpText("Distribution:"),
                 # sliderInput("binslab",
                 #             "Number of bins:",
                 #             min = 1,
                 #             max = 50,
                 #             value = 30),
                 plotOutput("plotlab")
                 # helpText("Boxplot:"),
                 # plotOutput("boxplotlab"),
               )
             )
    ),
    
    # Vital status Panel
    tabPanel("Vital status", 
             sidebarLayout(
               sidebarPanel(
                 selectInput("Vitalvar", "Vitals:",
                             Vitalvar,
                             selected = "heart_rate"),
                 # stratified by admission types
                 checkboxInput(inputId = "stratified", 
                               label = "Stratified",
                               value = FALSE),
                 selectInput(inputId = "Admissionsvar",
                             label = "Stratified by:",
                             choices = Admissionsvar,
                             # selected = NULL,
                             selected = "flag_die_30")
               ),
               mainPanel(
                 # summary statistics table
                 tableOutput("tablevital"),
                 helpText("Distribution:"),
                 # sliderInput("binsvital",
                 #             "Number of bins:",
                 #             min = 1,
                 #             max = 50,
                 #             value = 30),
                 plotOutput("plotvital")
                 # helpText("Boxplot:"),
                 # plotOutput("boxplotvital"),
               )
             )
    )
  


#---- Server logic ----
server <- function(input, output) {
  # dataInput_demo <- reactive(
  #   Demovar <- input$Demovar
  # )
  # 
  # output$distPlotdemo <- renderPlot({
  #   
  # })
  # output$distTable <- {
  #   results_bootsamples %>%
  #     kbl(caption = "Direct and indirect effects of X on Y through M")%>%
  #     kable_classic(full_width = F, html_font = "Arial")
  # }
 }


#---- Run the app ----
shinyApp(ui = ui, server = server)