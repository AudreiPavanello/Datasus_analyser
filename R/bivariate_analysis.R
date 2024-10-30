bivariateUI <- function(id) {
  ns <- NS(id)
  
  card(
    card_header("Análise Bivariada"),
    layout_columns(
      col_widths = c(6, 6),  # Two equal columns
      card(
        card_header("Primeira Variável"),
        selectInput(ns("var1"), "Selecione a primeira variável:", choices = NULL),
        selectInput(ns("var1_type"), "Tipo da variável:",
                    choices = c("Numérica" = "numeric", "Categórica" = "factor"))
      ),
      card(
        card_header("Segunda Variável"),
        selectInput(ns("var2"), "Selecione a segunda variável:", choices = NULL),
        selectInput(ns("var2_type"), "Tipo da variável:",
                    choices = c("Numérica" = "numeric", "Categórica" = "factor"))
      )
    ),
    plotOutput(ns("bivariate_plot")),
    verbatimTextOutput(ns("correlation")),
    verbatimTextOutput(ns("statistical_test"))
  )
}

bivariateServer <- function(id, dataset) {
  moduleServer(id, function(input, output, session) {
    
    # Function to format numbers without scientific notation
    format_number <- function(x, digits = 3) {
      format(x, scientific = FALSE, digits = digits, big.mark = ",")
    }
    
    observe({
      req(dataset())
      updateSelectInput(session, "var1", choices = names(dataset()))
      updateSelectInput(session, "var2", choices = names(dataset()))
    })
    
    prepare_variables <- function(var, type) {
      if (type == "numeric") {
        as.numeric(as.character(var))
      } else {
        as.factor(var)
      }
    }
    
    perform_statistical_test <- function(var1, var2, type1, type2) {
      tryCatch({
        if(type1 == "numeric" && type2 == "numeric") {
          clean_data <- na.omit(data.frame(
            var1 = as.numeric(var1),
            var2 = as.numeric(var2)
          ))
          
          if(nrow(clean_data) < 3) {
            return(list(
              method = "Erro",
              message = "Dados insuficientes para realizar o teste"
            ))
          }
          
          test <- cor.test(clean_data$var1, clean_data$var2)
          return(list(
            method = "Correlação de Pearson",
            statistic = test$statistic,
            p.value = test$p.value,
            correlation = test$estimate
          ))
          
        } else if(type1 == "factor" && type2 == "factor") {
          tab <- table(as.factor(var1), as.factor(var2))
          
          if(any(dim(tab) < 2)) {
            return(list(
              method = "Erro",
              message = "Variáveis categóricas precisam ter pelo menos 2 níveis cada"
            ))
          }
          
          test <- chisq.test(tab, simulate.p.value = TRUE)
          return(list(
            method = "Teste Chi-quadrado",
            statistic = test$statistic,
            p.value = test$p.value
          ))
          
        } else {
          numeric_var <- if(type1 == "numeric") var1 else var2
          factor_var <- if(type1 == "factor") var1 else var2
          
          numeric_var <- as.numeric(as.character(numeric_var))
          factor_var <- as.factor(as.character(factor_var))
          
          clean_data <- na.omit(data.frame(
            numeric = numeric_var,
            factor = factor_var
          ))
          
          # Check number of levels
          if(length(unique(clean_data$factor)) == 2) {
            # Use t-test for two levels
            test <- t.test(numeric ~ factor, data = clean_data)
            return(list(
              method = "Teste t",
              statistic = test$statistic,
              p.value = test$p.value
            ))
          } else if(length(unique(clean_data$factor)) > 2) {
            # Use ANOVA for more than two levels
            test <- aov(numeric ~ factor, data = clean_data)
            anova_summary <- summary(test)[[1]]
            return(list(
              method = "ANOVA",
              statistic = anova_summary$`F value`[1],
              p.value = anova_summary$`Pr(>F)`[1]
            ))
          } else {
            return(list(
              method = "Erro",
              message = "A variável categórica deve ter pelo menos 2 níveis"
            ))
          }
        }
      }, error = function(e) {
        return(list(
          method = "Erro",
          message = paste("Erro ao realizar o teste:", e$message)
        ))
      })
    }
    
    output$bivariate_plot <- renderPlot({
      req(dataset(), input$var1, input$var2)
      
      var1 <- prepare_variables(dataset()[[input$var1]], input$var1_type)
      var2 <- prepare_variables(dataset()[[input$var2]], input$var2_type)
      
      if(input$var1_type == "numeric" && input$var2_type == "numeric") {
        ggplot(data.frame(x = var1, y = var2), aes(x = x, y = y)) +
          geom_point(alpha = 0.5) +
          geom_smooth(method = "lm", se = TRUE) +
          theme_minimal() +
          scale_x_continuous(labels = format_number) +
          scale_y_continuous(labels = format_number) +
          labs(x = input$var1, y = input$var2,
               title = paste("Relação entre", input$var1, "e", input$var2))
        
      } else if(input$var1_type == "factor" && input$var2_type == "factor") {
        data.frame(var1 = var1, var2 = var2) %>%
          count(var1, var2) %>%
          ggplot(aes(x = var1, y = n, fill = var2)) +
          geom_bar(stat = "identity", position = "dodge") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
          scale_y_continuous(labels = format_number) +
          labs(x = input$var1, y = "Frequência", fill = input$var2,
               title = paste("Relação entre", input$var1, "e", input$var2))
        
      } else {
        numeric_var <- if(input$var1_type == "numeric") var1 else var2
        factor_var <- if(input$var1_type == "factor") var1 else var2
        numeric_name <- if(input$var1_type == "numeric") input$var1 else input$var2
        factor_name <- if(input$var1_type == "factor") input$var1 else input$var2
        
        ggplot(data.frame(x = factor_var, y = numeric_var), aes(x = x, y = y)) +
          geom_boxplot(fill = "steelblue", alpha = 0.7) +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
          scale_y_continuous(labels = format_number) +
          labs(x = factor_name, y = numeric_name,
               title = paste("Distribuição de", numeric_name, "por", factor_name))
      }
    })
    
    output$correlation <- renderText({
      req(dataset(), input$var1, input$var2)
      
      if(input$var1_type == "numeric" && input$var2_type == "numeric") {
        var1 <- as.numeric(as.character(dataset()[[input$var1]]))
        var2 <- as.numeric(as.character(dataset()[[input$var2]]))
        
        clean_data <- na.omit(data.frame(var1 = var1, var2 = var2))
        
        if(nrow(clean_data) > 0) {
          cor_value <- cor(clean_data$var1, clean_data$var2)
          paste("Correlação:", format_number(cor_value))
        } else {
          "Não foi possível calcular a correlação (dados ausentes ou não numéricos)"
        }
      }
    })
    
    output$statistical_test <- renderPrint({
      req(dataset(), input$var1, input$var2)
      
      var1 <- dataset()[[input$var1]]
      var2 <- dataset()[[input$var2]]
      
      test_results <- perform_statistical_test(var1, var2, input$var1_type, input$var2_type)
      
      if(!is.null(test_results$message)) {
        cat(test_results$message)
      } else {
        cat(paste0("\nMétodo: ", test_results$method))
        cat(paste0("\nEstatística do teste: ", format_number(test_results$statistic)))
        cat(paste0("\nValor p: ", format_number(test_results$p.value)))
        if(!is.null(test_results$correlation)) {
          cat(paste0("\nCorrelação: ", format_number(test_results$correlation)))
        }
      }
    })
  })
}