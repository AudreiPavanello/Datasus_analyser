library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(readxl)
library(tidyr)
library(DT)
library(writexl)

source("R/univariate_analysis.R")
source("R/bivariate_analysis.R")
source("R/filter_data.R")

ui <- page_sidebar(
  title = "Análise de Dados",
  sidebar = sidebar(
    fileInput("file", "Carregar arquivo (Excel ou CSV)"),
    selectInput("variable", "Selecione a variável:", choices = NULL),
    selectInput("var_type", "Tipo da variável:",
                choices = c("Numérica" = "numeric", "Categórica" = "factor")),
    selectInput("plot_type", "Tipo de gráfico:",
                choices = c("Histograma" = "hist",
                            "Boxplot" = "box",
                            "Barras" = "bar"))
  ),
  navset_tab(
    nav_panel("Análise Univariada",
              univariateUI("univariate")
    ),
    nav_panel("Análise Bivariada",
              bivariateUI("bivariate")
    ),
    nav_panel("Filtros",
              filterUI("filter")
    )
  )
)

server <- function(input, output, session) {
  dataset <- reactive({
    req(input$file)
    
    ext <- tools::file_ext(input$file$name)
    
    if(ext == "xlsx" || ext == "xls") {
      read_excel(input$file$datapath)
    } else if(ext == "csv") {
      read.csv(input$file$datapath)
    }
  })
  
  # Initialize the filter module
  filtered_data <- filterServer("filter", dataset)
  
  observe({
    req(filtered_data())
    updateSelectInput(session, "variable", choices = names(filtered_data()))
  })
  
  # Store the current plot
  current_plot <- reactive({
    req(filtered_data(), input$variable)
    
    if(input$var_type == "numeric" && input$plot_type == "hist") {
      ggplot(data.frame(x = filtered_data()[[input$variable]]), aes(x = x)) +
        geom_histogram(fill = "steelblue", color = "white", bins = 30) +
        theme_minimal() +
        labs(x = input$variable, y = "Frequência",
             title = paste("Histograma de", input$variable))
      
    } else if(input$var_type == "numeric" && input$plot_type == "box") {
      ggplot(data.frame(x = filtered_data()[[input$variable]]), aes(y = x)) +
        geom_boxplot(fill = "steelblue", alpha = 0.7) +
        theme_minimal() +
        labs(y = input$variable,
             title = paste("Boxplot de", input$variable)) +
        theme(axis.text.x = element_blank(),
              axis.ticks.x = element_blank())
      
    } else {
      data.frame(x = filtered_data()[[input$variable]]) %>%
        count(x) %>%
        ggplot(aes(x = x, y = n)) +
        geom_bar(stat = "identity", fill = "steelblue", alpha = 0.7) +
        theme_minimal() +
        labs(x = input$variable, y = "Frequência",
             title = paste("Gráfico de Barras de", input$variable)) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  })
  
  # Get current statistics
  current_stats <- reactive({
    req(filtered_data(), input$variable)
    
    if(input$var_type == "numeric") {
      data <- filtered_data()[[input$variable]]
      list(
        summary = summary(data),
        sd = sd(data, na.rm = TRUE),
        var = var(data, na.rm = TRUE),
        cv = sd(data, na.rm = TRUE) / mean(data, na.rm = TRUE) * 100
      )
    } else {
      freq_table <- table(filtered_data()[[input$variable]])
      prop_table <- prop.table(freq_table) * 100
      list(
        frequencies = freq_table,
        percentages = prop_table
      )
    }
  })
  
  # Call the univariate module server
  univariateServer("univariate",
                   dataset = filtered_data,
                   input_variable = reactive(input$variable),
                   var_type = reactive(input$var_type),
                   plot_type = reactive(input$plot_type))
  
  # Call the bivariate module server
  bivariateServer("bivariate", dataset = filtered_data)
}

shinyApp(ui, server)