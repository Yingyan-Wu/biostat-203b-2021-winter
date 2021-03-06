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
stratifiedvar <- c("admission_type", "flag_die30", "insurance")
demovar <- c("age_at_admission", "gender", "ethnicity", "language", 
             "insurance", "marital_status")
labvar <- c("bicarbonate", "calcium", "chloride", "creatinine", "glucose", 
            "magnesium", "potassium", "sodium", "hematocrit", "wbc", "lactate")
vitalvar <- c("heart_rate", 
              "non_invasive_blood_pressure_systolic",
              "non_invasive_blood_pressure_mean",
              "respiratory_rate",
              "temperature_fahrenheit",
              "arterial_blood_pressure_systolic",
              "arterial_blood_pressure_mean", "flag_die30")
catvar <- c("gender", "ethnicity", "language", "insurance", "marital_status", 
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
                 selectInput(inputId = "demovar", 
                             label = "Demographic information:",
                             choices = demovar,
                             selected = "age_at_admission")
                 # # stratified by admission types
                 # checkboxInput(inputId = "stratified",
                 #               label = "Stratified",
                 #               value = FALSE),
                 # selectInput(inputId = "stratifiedvar",
                 #             label = "Stratified by:",
                 #             choices = stratifiedvar,
                 #             # selected = NULL,
                 #             selected = "flag_die_30")
               ),
               mainPanel(
                 # summary statistics table
                 helpText("Summary stastistics:"),
                 tableOutput("demotable"),
                 helpText("Distribution plot:"),
                 sliderInput(
                   "binsdemo",
                   "# of bins for histogram (Not applicable for bar charts):",
                   min = 1,
                   max = 50,
                   value = 30),
                 plotOutput("demoplot"),
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
                             selected = "bicarbonate"),
                 # Log transform or not
                 checkboxInput(
                   "lablog", 
                   "log scale (only applied to continuous variables)",
                   value = F)
                 # # stratified by admission types
                 # checkboxInput(inputId = "stratified",
                 #               label = "Stratified",
                 #               value = FALSE),
                 # selectInput(inputId = "stratifiedvar",
                 #             label = "Stratified by:",
                 #             choices = stratifiedvar,
                 #             # selected = NULL,
                 #             selected = "flag_die_30")
                 
               ),
               mainPanel(
                 # summary statistics table
                 helpText("Summary stastistics:"),
                 tableOutput("labtable"),
                 helpText("Distribution:"),
                 sliderInput("binslab",
                             "# of bins for histogram:",
                             min = 1,
                             max = 50,
                             value = 30),
                 helpText("Histogram:"),
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
                 # variables input
                 selectInput("vitalvar", "Vitals:",
                             vitalvar,
                             selected = "heart_rate"),
                 # Log transform or not
                 checkboxInput(
                   "vitallog", 
                   "log scale (only applied to continuous variables)",
                   value = F)
                 # # stratified by admission types
                 # checkboxInput(inputId = "stratified", 
                 #               label = "Stratified",
                 #               value = FALSE),
                 # selectInput(inputId = "stratifiedvar",
                 #             label = "Stratified by:",
                 #             choices = stratifiedvar,
                 #             # selected = NULL,
                 #             selected = "flag_die_30")
               ),
               mainPanel(
                 # summary statistics table
                 helpText("Summary stastistics:"),
                 tableOutput("vitaltable"),
                 helpText("Distribution:"),
                 sliderInput(
                   "binsvital",
                   "# of bins for histogram(Not applicable for bar charts): ",
                   min = 1,
                   max = 50,
                   value = 30),
                 helpText("Histogram:"),
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
  #---- Demo panel ----
  #---- **Table ----
  output$demotable <- renderTable(
    if(input$demovar %in% catvar){
      icu_cohort %>%
        count(!!sym(input$demovar)) %>%
        mutate(prop = prop.table(n))
      
    } else {
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
    }
  )
  
  #---- **Histogram/barplot ----
  output$demoplot <- renderPlot({
    if(input$demovar %in% catvar){
      icu_cohort %>%
        ggplot(., aes_string(x = input$demovar)) + 
        geom_bar(aes_string(x = input$demovar)) +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) + 
        labs(title = paste0("Bar chart of ", input$demovar))
    } else{
      icu_cohort %>%
        ggplot(., aes_string(x = input$demovar)) +
        geom_histogram(bins = input$binsdemo, 
                       colour = "black", fill = "grey") +
        #Pending label
        # xlab() +
        ylab("Density") +
        labs(title = paste0("Histogram of ", input$demovar))
    }
  }, height = 400, width = 600
  ) 
  
  
  #---- Lab panel ----
  #---- **Table ----
  output$labtable <- renderTable(
    if (input$lablog){
      icu_cohort %>%
        filter(!!sym(input$labvar) > 0) %>%
        summarize(
          N = length(log(!!sym(input$labvar))), 
          Mean = mean(log(!!sym(input$labvar)), na.rm = T),
          Std = sd(log(!!sym(input$labvar)), na.rm = T),
          Min = min(log(!!sym(input$labvar)), na.rm = T),
          Median = median(log(!!sym(input$labvar)), na.rm = T),
          Max = max(log(!!sym(input$labvar)), na.rm = T),
          `N missing` = sum(is.na(!!sym(input$labvar))
          ))
    } else{
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
    }
  )
  
  #---- **Histogram ----
  output$labplot <- renderPlot(
    {
      if (input$lablog){
        icu_cohort %>%
          ggplot(., aes(x = log(!!sym(input$labvar)))) +
          geom_histogram(bins = input$binslab, 
                         colour = "black", fill = "grey") +
          #Pending label
          # xlab() +
          ylab("Density")
      } else{
        icu_cohort %>%
          ggplot(., aes_string(x = input$labvar)) +
          geom_histogram(bins = input$binslab, 
                         colour = "black", fill = "grey") +
          #Pending label
          # xlab() +
          ylab("Density")
      }
    }, 
    height = 400, width = 600
  ) 
  
  #---- **Boxplot ----
  output$labboxplot <- renderPlot(
    {
      if (input$lablog){
        icu_cohort %>%
          ggplot(., aes(x = log(!!sym(input$labvar)))) +
          geom_boxplot(color = "black", fill = "grey") +
          xlab(input$labvar)
      } else{
        icu_cohort %>%
          ggplot(., aes_string(x = input$labvar)) +
          geom_boxplot(color = "black", fill = "grey") +
          xlab(input$labvar)
      }
    }, 
    height = 400, width = 600
  )
  
  #---- Vital status panel ----
  #---- **Table ----
  output$vitaltable <- renderTable(
    if(input$vitalvar %in% catvar){
      icu_cohort %>%
        count(!!sym(input$vitalvar)) %>%
        mutate(prop = prop.table(n))
    } else {
      if (input$vitallog){
        icu_cohort %>%
          filter(!!sym(input$vitalvar) > 0) %>%
          summarize(
            N = length(log(!!sym(input$vitalvar))), 
            Mean = mean(log(!!sym(input$vitalvar)), na.rm = T),
            Std = sd(log(!!sym(input$vitalvar)), na.rm = T),
            Min = min(log(!!sym(input$vitalvar)), na.rm = T),
            Median = median(log(!!sym(input$vitalvar)), na.rm = T),
            Max = max(log(!!sym(input$vitalvar)), na.rm = T),
            `N missing` = sum(is.na(!!sym(input$vitalvar))
            ))
      } else {
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
      }
    }
  )
  
  #---- **Histogram/bar chart ----
  output$vitalplot <- renderPlot(
    {if(input$vitalvar %in% catvar){
      icu_cohort %>%
        ggplot(., aes(x = as.character(!!sym(input$vitalvar)))) + 
        geom_bar(aes(x = as.character(!!sym(input$vitalvar)))) +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) + 
        labs(title = paste0("Bar chart of ", input$vitalvar))
    } else{
      if (input$vitallog){
        icu_cohort %>%
          ggplot(., aes(x = log(!!sym(input$vitalvar)))) +
          geom_histogram(bins = input$binsvital, 
                         colour = "black", fill = "grey") +
          #Pending vitalel
          # xlab() +
          ylab("Density")
      } else{
        icu_cohort %>%
          ggplot(., aes_string(x = input$vitalvar)) +
          geom_histogram(bins = input$binsvital, 
                         colour = "black", fill = "grey") +
          #Pending label
          # xlab() +
          ylab("Density")
      }
    }
    }, 
    height = 400, width = 600
  ) 
  
  #---- **Boxplot ----
  output$vitalboxplot <- renderPlot(
    {if(input$vitalvar %in% catvar){
      
    } else {
      if (input$vitallog){
        icu_cohort %>%
          ggplot(., aes(x = log(!!sym(input$vitalvar)))) +
          geom_boxplot(color = "black", fill = "grey") +
          xlab(input$vitalvar)
      } else{
        icu_cohort %>%
          ggplot(., aes_string(x = input$vitalvar)) +
          geom_boxplot(color = "black", fill = "grey") +
          xlab(input$vitalvar)
      }
    }
    }, 
    height = 400, width = 600
  )
  
}


#---- Run the app ----
shinyApp(ui = ui, server = server)