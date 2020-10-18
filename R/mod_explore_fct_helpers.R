#' Render a single datasauRus graph in plotly
#'
#' @param df data frame corresponding to a single dataset
#' @param register_select flag select event in plotly
#' @param ... additional params that might be used later
#' @return plotly object
#' @export
#' @import plotly
render_data_graph <- function(df, register_select = TRUE, ...) {
  base <- plot_ly(
    df, 
    x = ~x, 
    y = ~y,
    customdata = seq(1, nrow(df)),
    source = "A"
  ) %>%
  layout(
    xaxis = list(range = c(0, 100)),
    yaxis = list(range = c(0, 100)),
    dragmode = "select"
  )
  
  if (register_select) {
    base <- event_register(base, "plotly_selecting")
  }
  
  return(base)
}

mean_sd_print <- function(df, var) {
  # derive summary stats for supplied variable
  mean_print <- round(mean(df[[var]], 2))
  sd_print <- round(sd(df[[var]]), 2)
  
  glue::glue("{mean_print} ({sd_print})")
}
