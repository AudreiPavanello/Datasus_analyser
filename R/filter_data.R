filterUI <- function(id) {
  ns <- NS(id)
  
  card(
    card_header("Filtros"),
    selectInput(ns("var_to_filter"), "Selecione a variável para filtrar:", choices = NULL),
    uiOutput(ns("filter_controls")),
    actionButton(ns("add_filter"), "Adicionar Filtro", class = "btn-primary"),
    tags$hr(),
    actionButton(ns("apply_filters"), "Aplicar Filtros", class = "btn-success"),
    downloadButton(ns("download_filtered"), "Baixar Dados Filtrados"),
    tags$hr(),
    actionButton(ns("clear_filters"), "Limpar Filtros", class = "btn-warning"),
    tags$hr(),
    card(
      card_header("Preview dos Dados Filtrados"),
      tags$div(
        style = "display: flex; justify-content: space-between; align-items: center;",
        textOutput(ns("filtered_dims")),
        checkboxInput(ns("show_filtered"), "Mostrar apenas dados filtrados", value = TRUE)
      ),
      DTOutput(ns("data_preview"))
    )
  )
}

filterServer <- function(id, dataset) {
  moduleServer(id, function(input, output, session) {
    # Store active filters
    active_filters <- reactiveVal(list())
    
    # Update variable choices
    observe({
      req(dataset())
      updateSelectInput(session, "var_to_filter", 
                        choices = names(dataset()))
    })
    
    # Dynamic filter controls based on variable type
    output$filter_controls <- renderUI({
      req(dataset(), input$var_to_filter)
      
      var <- dataset()[[input$var_to_filter]]
      
      if(is.numeric(var)) {
        var_range <- range(var, na.rm = TRUE)
        sliderInput(session$ns("filter_value"), 
                    "Selecione o intervalo:",
                    min = var_range[1],
                    max = var_range[2],
                    value = var_range)
      } else {
        checkboxGroupInput(session$ns("filter_value"),
                           "Selecione as categorias:",
                           choices = unique(var),
                           selected = unique(var))
      }
    })
    
    # Add new filter
    observeEvent(input$add_filter, {
      req(dataset(), input$var_to_filter, input$filter_value)
      
      current_filters <- active_filters()
      var_name <- input$var_to_filter
      
      if(is.numeric(dataset()[[var_name]])) {
        filter_spec <- list(
          variable = var_name,
          type = "numeric",
          min = input$filter_value[1],
          max = input$filter_value[2]
        )
      } else {
        filter_spec <- list(
          variable = var_name,
          type = "categorical",
          values = input$filter_value
        )
      }
      
      current_filters[[var_name]] <- filter_spec
      active_filters(current_filters)
    })
    
    # Clear all filters
    observeEvent(input$clear_filters, {
      active_filters(list())
    })
    
    # Apply filters to dataset
    filtered_dataset <- reactive({
      req(dataset())
      data <- dataset()
      
      filters <- active_filters()
      if(length(filters) == 0) return(data)
      
      for(filter in filters) {
        if(filter$type == "numeric") {
          data <- data[data[[filter$variable]] >= filter$min & 
                         data[[filter$variable]] <= filter$max, ]
        } else {
          data <- data[data[[filter$variable]] %in% filter$values, ]
        }
      }
      
      data
    })
    
    # Data preview
    output$data_preview <- renderDT({
      if(input$show_filtered) {
        filtered_dataset()
      } else {
        dataset()
      }
    }, options = list(pageLength = 5))
    
    # Show dimensions of filtered data
    output$filtered_dims <- renderText({
      original_rows <- nrow(dataset())
      filtered_rows <- nrow(filtered_dataset())
      
      paste0("Linhas: ", filtered_rows, " / ", original_rows,
             " (", round(filtered_rows/original_rows * 100), "% dos dados originais)")
    })
    
    # Download handler
    output$download_filtered <- downloadHandler(
      filename = function() {
        paste0("dados_filtrados_", format(Sys.time(), "%Y%m%d_%H%M"), ".csv")
      },
      content = function(file) {
        write.csv(filtered_dataset(), file, row.names = FALSE)
      }
    )
    
    # Return the filtered dataset for use in other modules
    return(filtered_dataset)
  })
}