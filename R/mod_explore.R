#' explore UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import bs4Dash
mod_explore_ui <- function(id){
  # obtain names of available datasauRus data sets
  ds_names <- unique(datasauRus::datasaurus_dozen$dataset)
  
  ns <- NS(id)
  tagList(
    fluidRow(
      col_12(
        bs4Card(
          inputId = ns("doc_card"),
          title = "About this UI",
          status = "primary",
          gradientColor = "primary",
          solidHeader = TRUE,
          width = 12,
          collapsible = TRUE,
          collapsed = FALSE,
          closeable = TRUE,
          includeMarkdown(app_sys("app", "docs", "explore.md"))
        )
      )
    ),
    fluidRow(
      col_12(
        bs4Card(
          inputId = ns("explore_card"),
          title = "Explore!",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          collapsible = TRUE,
          collapsed = FALSE,
          closeable = TRUE,
          
          fluidRow(
            col_12(
              selectInput(
                ns("data_select"),
                "Select your dataset!",
                choices = ds_names,
                selected = ds_names[1]
              )
            )
          ),
          
          fluidRow(
            bs4InfoBoxOutput(
              ns("box_x"),
              width = 4
            ),
            bs4InfoBoxOutput(
              ns("box_y"),
              width = 4
            ),
            bs4InfoBoxOutput(
              ns("box_cor"),
              width = 4
            )
          ),
          
          fluidRow(
            col_6(
              plotly::plotlyOutput(ns("ds_plot"), height = "600px")
            ),
            col_6(
              DT::dataTableOutput(ns("ds_table"))
            )
          )
        )
      )
    )
  )
}
    
#' explore Server Function
#'
#' @noRd 
mod_explore_server <- function(input, output, session){
  ns <- session$ns
  
  # reactive for data frame selected
  data_df <- reactive({
    req(input$data_select)
    df <- datasauRus::datasaurus_dozen
    
    res <- df %>%
      dplyr::filter(dataset == input$data_select) %>%
      dplyr::select(., -dataset)
    
    return(res)
  })
  
  # render the info boxes
  output$box_x <- renderbs4InfoBox({
    req(data_df())
    bs4InfoBox(
      title = "Mean (SD) of X",
      gradientColor = "success",
      value = mean_sd_print(data_df(), "x"),
      icon = icon("bar-chart-o")
    )
  })
  
  output$box_y <- renderbs4InfoBox({
    req(data_df())
    bs4InfoBox(
      title = "Mean (SD) of Y",
      gradientColor = "success",
      value = mean_sd_print(data_df(), "y"),
      icon = icon("bar-chart-o")
    )
  })
  
  output$box_cor <- renderbs4InfoBox({
    req(data_df())
    bs4InfoBox(
      title = "Correlation",
      gradientColor = "primary",
      value = round(cor(x = data_df()$x, y = data_df()$y), 2),
      icon = icon("bar-chart-o")
    )
  })
  
  # output$mean_box <- renderbs4InfoBox({
  #   req(data_df())
  #   bs4InfoBox(
  #     title = "Mean of X",
  #     gradientColor = "success",
  #     value = mean_sd_print(data_df()),
  #     icon = "chart"
  #   )
  # })
  
  # render interactive graph via plotly
  output$ds_plot <- plotly::renderPlotly({
    req(data_df())
    render_data_graph(data_df())
  })
  
  output$ds_table <- DT::renderDataTable({
    req(data_df())
    data_df()
  },
  rownames = FALSE,
  options = list(dom = 'tp')
  )
 
}
    
## To be copied in the UI
# mod_explore_ui("explore_ui_1")
    
## To be copied in the server
# callModule(mod_explore_server, "explore_ui_1")
 
