univariateUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Visualização"),
        plotOutput(ns("univariate_plot"), height = "400px")
      ),
      card(
        card_header("Estatísticas Descritivas"),
        verbatimTextOutput(ns("summary_stats"))
      )
    ),
    card(
      card_header("Tabela de Frequências"),
      DTOutput(ns("freq_table"))
    )
  )
}

univariateServer <- function(id, dataset, input_variable, var_type, plot_type) {
  moduleServer(id, function(input, output, session) {
    
    processed_var <- reactive({
      req(dataset(), input_variable())
      
      if(var_type() == "numeric") {
        as.numeric(as.character(dataset()[[input_variable()]]))
      } else if(var_type() == "factor") {
        as.factor(dataset()[[input_variable()]])
      } else {
        dataset()[[input_variable()]]
      }
    })
    
    output$univariate_plot <- renderPlot({
      req(processed_var())
      
      if(var_type() == "numeric" && plot_type() == "hist") {
        ggplot(data.frame(x = processed_var()), aes(x = x)) +
          geom_histogram(fill = "steelblue", color = "white", bins = 30) +
          theme_minimal() +
          labs(x = input_variable(), y = "Frequência")
        
      } else if(var_type() == "numeric" && plot_type() == "box") {
        ggplot(data.frame(x = processed_var()), aes(y = x)) +
          geom_boxplot(fill = "steelblue", alpha = 0.7) +
          theme_minimal() +
          labs(y = input_variable()) +
          theme(axis.text.x = element_blank(),
                axis.ticks.x = element_blank())
        
      } else if((var_type() == "factor" || plot_type() == "bar")) {
        data.frame(x = processed_var()) %>%
          count(x) %>%
          ggplot(aes(x = x, y = n)) +
          geom_bar(stat = "identity", fill = "steelblue", alpha = 0.7) +
          theme_minimal() +
          labs(x = input_variable(), y = "Frequência") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      }
    })
    
    output$summary_stats <- renderPrint({
      req(processed_var())
      
      if(var_type() == "numeric") {
        # Numerical summaries
        summary_stats <- summary(processed_var())
        cat("Sumário Estatístico:\n")
        print(summary_stats)
        
        cat("\nMedidas Adicionais:\n")
        cat("Desvio Padrão:", sd(processed_var(), na.rm = TRUE), "\n")
        cat("Variância:", var(processed_var(), na.rm = TRUE), "\n")
        cat("Coeficiente de Variação:", 
            sd(processed_var(), na.rm = TRUE) / mean(processed_var(), na.rm = TRUE) * 100,
            "%\n")
        
      } else {
        # Categorical summaries
        freq_table <- table(processed_var())
        prop_table <- prop.table(freq_table) * 100
        
        cat("Frequências:\n")
        print(freq_table)
        cat("\nPorcentagens:\n")
        print(round(prop_table, 2))
      }
    })
    
    output$freq_table <- renderDT({
      req(processed_var())
      
      if(var_type() == "numeric") {
        # Create bins for numeric variables
        breaks <- seq(min(processed_var(), na.rm = TRUE),
                      max(processed_var(), na.rm = TRUE),
                      length.out = 11)
        
        cut(processed_var(), breaks = breaks, include.lowest = TRUE) %>%
          table() %>%
          as.data.frame() %>%
          setNames(c("Intervalo", "Frequência"))
        
      } else {
        # Frequency table for categorical variables
        table(processed_var()) %>%
          as.data.frame() %>%
          setNames(c("Categoria", "Frequência")) %>%
          mutate(
            'Frequência Relativa (%)' = round(Frequência / sum(Frequência) * 100, 2),
            'Frequência Acumulada' = cumsum(Frequência),
            'Frequência Relativa Acumulada (%)' = round(cumsum(Frequência) / sum(Frequência) * 100, 2)
          )
      }
    })
  })
}