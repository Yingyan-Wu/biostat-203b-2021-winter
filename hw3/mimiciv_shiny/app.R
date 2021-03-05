#---- Load packages ----
library(shiny)
library(tidyverse)

# Source helpers ----
# didn't use!
# source("helpers.R")

#---- Load data ----
icu_cohort <- readRDS("icu_cohort.rds")

#---- Set up variables ----
# colnames(icu_cohort)
Admissionsvar <- c("admission_type", "flag_die_30", "insurance")
demovar <- c("age_at_admission", "gender", "ethnicity", "language", 
             "insurance", "marital_status")
labvar <- c("bicarbonate", "calcium", "chloride", "creatinine", "glucose", 
            "magnesium", "potassium", "sodium", "hematocrit", "wbc", "lactate")
vitalvar <- c("heart_rate", 
                 "non_invasive_blood_pressure_systolic",
                 "non_invasive_blood_pressure_mean",
                 "respiratory_rate",
                 "temperature_fahrenheit",
                 "arterial_blood_pressure_mean",
                 "flag_die30")
catvar <- c("gender", "ethnicity", "language", "insurance", "marital_status")

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
                 selectInput(inputId = "demovar", 
                             label = "Demographic information:",
                             choices = demovar,
                             selected = "age_at_admission")
                 # # stratified by admission types
                 # checkboxInput(inputId = "stratified", 
                 #               label = "Stratified",
                 #               value = FALSE),
                 # selectInput(inputId = "Admissionsvar",
                 #             label = "Stratified by:",
                 #             choices = Admissionsvar,
                 #             # selected = NULL,
                 #             selected = "flag_die_30")
               ),
               mainPanel(
                 # summary statistics table
                 tableOutput("demotable"),
                 helpText("Distribution:"),
                 sliderInput("binsDemo",
                             "Number of bins:",
                             min = 1,
                             max = 50,
                             value = 30),
                 plotOutput("demoplot"),
                 helpText("Boxplot:"),
                 plotOutput("demoboxplot")
               )
             ),
             
    ),
    
    # Laboratory Panel
    tabPanel("Laboratory results", 
             sidebarLayout(
               sidebarPanel(
                 # variables input
                 selectInput(inputId = "labvar", 
                             label = "Lab measurement:",
                             choices = labvar,
                             selected = "bicarbonate")
                 # # stratified by admission types
                 # checkboxInput(inputId = "stratified", 
                 #               label = "Stratified",
                 #               value = FALSE),
                 # selectInput(inputId = "Admissionsvar",
                 #             label = "Stratified by:",
                 #             choices = Admissionsvar,
                 #             # selected = NULL,
                 #             selected = "flag_die_30")
               ),
               mainPanel(
                 # summary statistics table
                 tableOutput("labtable"),
                 helpText("Distribution:"),
                 sliderInput("binslab",
                             "Number of bins:",
                             min = 1,
                             max = 50,
                             value = 30),
                 plotOutput("labplot"),
                 helpText("Boxplot:"),
                 plotOutput("labboxplot")
               )
             )
    ),
    
    # Vital status Panel
    tabPanel("Vital status", 
             sidebarLayout(
               sidebarPanel(
                 selectInput("vitalvar", "Vitals:",
                             vitalvar,
                             selected = "heart_rate")
                 # # stratified by admission types
                 # checkboxInput(inputId = "stratified", 
                 #               label = "Stratified",
                 #               value = FALSE),
                 # selectInput(inputId = "Admissionsvar",
                 #             label = "Stratified by:",
                 #             choices = Admissionsvar,
                 #             # selected = NULL,
                 #             selected = "flag_die_30")
               ),
               mainPanel(
                 # summary statistics table
                 tableOutput("vitaltable"),
                 helpText("Distribution:"),
                 sliderInput("binsvital",
                             "Number of bins:",
                             min = 1,
                             max = 50,
                             value = 30),
                 plotOutput("vitalplot"),
                 helpText("Boxplot:"),
                 plotOutput("vitalboxplot")
               )
             )
    )
  )
)
  


#---- Server logic ----
server <- function(input, output) {
  # Demo panel
  demovarinput <- reactive(
    demovar <- input$demovar
  )
  
  
  # if(!is.numeric(demovarinput())){
  #   output$demotable <- renderTable(icu_cohort %>%
  #                                     count(!!sym(input$demovar)) %>%
  #                                     mutate(prop = prop.table(n))
  #                                   )
  # } else {
    output$demotable <- renderTable(
      icu_cohort %>%
        summarize(
          N = length(!!sym(input$demovar)),
          Mean = mean(!!sym(input$demovar), na.rm = T),
          Std = sd(!!sym(input$demovar), na.rm = T),
          Min = min(!!sym(input$demovar), na.rm = T),
          Median = median(!!sym(input$demovar), na.rm = T),
          Max = max(!!sym(input$demovar), na.rm = T),
          `N missing` = sum(is.na(!!sym(input$demovar))
          ))
    )
    
  # }
 
  
  output$labtable <- renderTable(
    icu_cohort %>%
      summarize(
        N = length(!!sym(input$labvar)), 
        Mean = mean(!!sym(input$labvar), na.rm = T),
        Std = sd(!!sym(input$labvar), na.rm = T),
        Min = min(!!sym(input$labvar), na.rm = T),
        Median = median(!!sym(input$labvar), na.rm = T),
        Max = max(!!sym(input$labvar), na.rm = T),
        `N missing` = sum(is.na(!!sym(input$labvar))
        ))
  )
  
  output$labplot <- renderPlot(
    icu_cohort %>%
      ggplot(., aes_string(x = input$labvar)) +
      geom_histogram(bins = input$binslab, 
                     colour = "grey", fill = "light blue") +
      #Pending label
      # xlab() +
      ylab("Density")
  )

  output$vitaltable <- renderTable(
    icu_cohort %>%
      summarize(
        N = length(!!sym(input$vitalvar)),
        Mean = mean(!!sym(input$vitalvar), na.rm = T),
        Std = sd(!!sym(input$vitalvar), na.rm = T),
        Min = min(!!sym(input$vitalvar), na.rm = T),
        Median = median(!!sym(input$vitalvar), na.rm = T),
        Max = max(!!sym(input$vitalvar), na.rm = T),
        `N missing` = sum(is.na(!!sym(input$vitalvar))
        ))
  )
  
  output$vitalplot <- renderPlot(
    icu_cohort %>%
      ggplot(., aes_string(x = log(input$vitalvar))) +
      geom_histogram(bins = input$binsvital, 
                     colour = "grey", fill = "light blue") +
      #Pending label
      # xlab() +
      ylab("Density")
  )
  
 #  output$distPlotdemo <- renderPlot({
 # 
 #  })
 #  output$distTable <- {
 #    icu_cohort %>%
 #      kbl(caption = "Direct and indirect effects of X on Y through M")%>%
 #      kable_classic(full_width = F, html_font = "Arial")
 #  }
 }


#---- Run the app ----
shinyApp(ui = ui, server = server)