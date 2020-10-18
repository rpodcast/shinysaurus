#' animate UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList
#' @import bs4Dash 
#' @import sortable
#' @import metamer
#' @import dplyr
mod_animate_ui <- function(id){
  ns <- NS(id)
  
  ds_choices <- unique(datasauRus::datasaurus_dozen$dataset)
  tagList(
    fluidRow(
      col_12(
        bs4Card(
          inputId = ns("doc_card"),
          title = "About this UI",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          collapsible = TRUE,
          collapsed = FALSE,
          closeable = TRUE,
          includeMarkdown(app_sys("app", "docs", "animate.md"))
        )
      )
    ),
    
    fluidRow(
      col_12(
        bs4Card(
          inputId = ns("settings_card"),
          title = "Parameter Settings",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          collapsible = TRUE,
          collapsed = FALSE,
          closeable = FALSE,
          fluidRow(
            col_4(
              # selectInput(
              #   ns("start_set"),
              #   label = "Select starting set",
              #   choices = ds_choices,
              #   selected = ds_choices[1]
              # ),
              # selectInput(
              #   ns("end_set"),
              #   label = "select ending set",
              #   choices = ds_choices,
              #   selected = ds_choices[2]
              # )
              # selectizeInput(
              #   ns("data_order"),
              #   label = "Customize the data order",
              #   choices = unique(datasauRus::datasaurus_dozen$dataset),
              #   selected = unique(datasauRus::datasaurus_dozen$dataset),
              #   multiple = TRUE,
              #   options = list(plugins = list('drag_drop', 'remove_button')),
              #   width = '100%'
              # )
              # shinyjqui::orderInput(
              #   ns("data_source"),
              #   label = "Drag from here",
              #   items = unique(datasauRus::datasaurus_dozen$dataset),
              #   as_source = FALSE,
              #   connect = ns("data_dest")
              # ),
              # shinyjqui::orderInput(
              #   ns("data_dest"),
              #   label = "Drag to here",
              #   items = NULL,
              #   as_source = FALSE,
              #   connect = ns("data_source")
              # )
              bucket_list(
                header = NULL,
                group_name = ns("bucket_list_group"),
                orientation = "horizontal",
                add_rank_list(
                  input_id = ns("rank_list_1"),
                  text = "Drag from here",
                  labels = as.list(ds_choices)
                ),
                add_rank_list(
                  input_id = ns("rank_list_2"),
                  text = "To here"
                )
              )
            ),
            col_4(
              numericInput(
                ns("n_metamer"),
                label = "Number of iterations",
                value = 30000,
                min = 100,
                max = 50000,
                step = 1
              ),
              tippy::tippy_this(
                ns("n_metamer"),
                tooltip = "Values above 30,000 may take a long time!",
                placement = "right"
              ),
              numericInput(
                ns("perturbation"),
                label = "Perturbation",
                value = 0.08,
                min = 0.01,
                max = 1,
                step = 0.01
              ),
              tippy::tippy_this(
                ns("perturbation"),
                tooltip = "Magnitude of the slight change made to data",
                placement = "right"
              ),
              numericInput(
                ns("trim"),
                label = "Maximum metamers",
                value = 150,
                min = 10,
                max = 300
              ),
              tippy::tippy_this(
                ns("trim"),
                tooltip = "Larger values may slow down the animation!",
                placement = "right"
              ),
              shinyWidgets::actionBttn(
                ns("save_settings"),
                label = "Lock it in!",
                style = "gradient",
                color = "royal",
                icon = icon("lock")
              )
            )
          ),
          fluidRow(
            col_12(
              verbatimTextOutput(ns("debug"))
            )
          )
        )
      )
    ),
    
    fluidRow(
      col_12(
        bs4Card(
          inputId = ns("plot_card"),
          title = "Play with animation!",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          height = "1000px",
          collapsible = TRUE,
          collapsed = FALSE,
          closeable = FALSE,
          fluidRow(
            col_8(
              plotly::plotlyOutput(ns("animate_plot"), height = "1000px")
            )
          )
        )
      )
    )
  )
}

# metamers <- metamerize(start_data, 
#                        preserve = delayed_with(mean(x), mean(y), cor(x, y)),
#                        minimize = mean_dist_to(star), 
#                        perturbation = 0.08,
#                        N = 30000,
#                        trim = 150) %>% 
#   metamerize(minimize = NULL, 
#              N = 3000, trim = 10) %>% 
#   metamerize(minimize = mean_dist_to(X), 
#              N = 30000, trim = 150) %>% 
#   metamerize(minimize = NULL, 
#              N = 3000, trim = 10) %>% 
#   metamerize(minimize = mean_dist_to(start_data),
#              N = 30000, trim = 150)

#' animate Server Function
#'
#' @noRd 
mod_animate_server <- function(input, output, session){
  ns <- session$ns
  
  # set up reactive values
  metamer_df <- reactiveVal(NULL)
  metamer_sum <- reactiveVal(NULL)
  
  # assemble settings and perform animation building
  observeEvent(input$save_settings, {
    
    # check if the two selected sets are the same
    # if (input$start_set == input$end_set) {
    #   shinyWidgets::show_alert(
    #     title = "Uh Oh!",
    #     text = "Please select 2 different sets!",
    #     type = "error",
    #     btn_labels = "Ok, got it",
    #     closeOnClickOutside = FALSE,
    #     showCloseButton = FALSE
    #   )
    #   
    #   return(NULL)
    # }
    # check if at least two data sets have been dragged over
    if (is.null(input$rank_list_2) || length(input$rank_list_2) < 2) {
      shinyWidgets::show_alert(
        title = "Uh Oh!",
        text = "Please drag over at least 2 data sets.",
        type = "error",
        btn_labels = "Ok, got it",
        closeOnClickOutside = FALSE,
        showCloseButton = FALSE
      )

      return(NULL)
    }
    
    # show modal to say something is happening
    shinyalert::shinyalert(
      title = "Magic is Happening!",
      text = NULL,
      size = "m",
      closeOnEsc = FALSE,
      closeOnClickOutside = FALSE,
      type = "info",
      showConfirmButton = FALSE,
      showCancelButton = FALSE,
      timer = 0,
      imageUrl = "https://media.giphy.com/media/xTiTnAUgTbDrsUiHja/giphy.gif",
      imageWidth = 400,
      imageHeight = 400,
      animation = TRUE
    )
    
    # first_dataset <- extract_dataset(input$start_set)
    # second_dataset <- extract_dataset(input$end_set)
    # perturbation = input$perturbation
    # N = input$n_metamer
    # trim = input$trim
    # 
    # metamers <- metamerize(
    #   data = first_dataset,
    #   preserve = delayed_with(mean(x), mean(y), cor(x, y)),
    #   minimize = mean_dist_to(second_dataset),
    #   perturbation = perturbation,
    #   N = N,
    #   trim = trim)
    
    metamers <- create_metamers(input$rank_list_2)

    
    meta2 <- as.data.frame(metamers)
    
    meta_sum <- tibble::as_tibble(meta2) %>%
      group_by(.metamer) %>%
      summarize(
        mean_x = mean(x),
        mean_x_print = glue::glue("Mean(X) = {round(mean_x, 2)}"),
        mean_y = mean(y),
        mean_y_print = glue::glue("Mean(Y) = {round(mean_y, 2)}"),
        cor_xy = cor(x, y),
        cor_xy_print = glue::glue("Cor(X,Y) = {round(cor_xy, 2)}")
      ) %>%
      ungroup
    
    metamer_df(meta2)
    metamer_sum(meta_sum)
    
    shinyalert::closeAlert()
    # browser()
    # #debugonce(create_metamers)
    # # res <- create_metamers(
    #   datasets = input$rank_list_2
    #   perturbation = input$perturbation
    #   N = input$n_metamer
    #   trim = input$trim
    # # )
    # 
    # # obtain the first and last elements of datasets
    # first_dataset <- datasets[1]
    # last_dataset <- datasets[length(datasets)]
    # 
    # # derive first set of metamers
    # df <- extract_dataset(first_dataset)
    # df2 <- extract_dataset(datasets[2])
    # 
    # debug(mean_dist_to)
    # metamers <- metamerize(
    #   #data = extract_dataset(first_dataset),
    #   data = df,
    #   preserve = delayed_with(mean(x), mean(y), cor(x, y)),
    #   minimize = mean_dist_to(df2),
    #   perturbation = perturbation,
    #   N = N,
    #   trim = trim)
    # 
    # # create additional metamers if we have more than 2 datasets
    # if (length(datasets) > 2) {
    #   # extract the 3rd or more dataset names
    #   remaining_datasets <- datasets[3:length(datasets)]
    #   
    #   for (ds in remaining_datasets) {
    #     metamers <- metamerize(metamers, minimize = NULL, N = N / 10, trim = 10)
    #     df2 <- extract_dataset(ds)
    #     metamers <- metamerize(metamers, minimize = mean_dist_to(!!!df), N = N, trim = trim)
    #   }
    # }
  })
  
  # animation plot
  output$animate_plot <- plotly::renderPlotly({
    req(metamer_df())
    req(metamer_sum())
    
    render_animation_graph(metamer_df(), metamer_sum())
    
    # base <- plot_ly(metamer_df(),  x = ~x, y = ~y) %>%
    #   add_markers(frame = ~.metamer) %>%
    #   add_text(x = 90, y = 95, frame = ~.metamer, text = ~mean_x_print, data = metamer_sum(), showlegend = FALSE) %>%
    #   add_text(x = 90, y = 93, frame = ~.metamer, text = ~mean_y_print, data = metamer_sum(), showlegend = FALSE) %>%
    #   add_text(x = 90, y = 91, frame = ~.metamer, text = ~cor_xy_print, data = metamer_sum(), showlegend = FALSE) %>%
    #   #add_text(x = 95, y = 95, text = "Hello") %>%
    #   layout(
    #     xaxis = list(range = c(0, 100)),
    #     yaxis = list(range = c(0, 100))
    #   )
    # 
    # base %>%
    #   animation_opts(frame = 100, easing = "linear", redraw = FALSE) %>%
    #   animation_button(
    #     x = 1, xanchor = "right", y = 0, yanchor = "bottom"
    #   )
    
  })
}
    
## To be copied in the UI
# mod_animate_ui("animate_ui_1")
    
## To be copied in the server
# callModule(mod_animate_server, "animate_ui_1")
 
