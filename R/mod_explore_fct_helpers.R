#' Render a single datasauRus graph in plotly
#'
#' @param df data frame corresponding to a single dataset
#' @param ... additional params that might be used later
#' @return plotly object
#' @export
#' @import plotly
render_data_graph <- function(df) {
  base <- plot_ly(df, x = ~x, y = ~y) %>%
    layout(
      xaxis = list(range = c(0, 100)),
      yaxis = list(range = c(0, 100))
    )
  
  return(base)
}

mean_sd_print <- function(df, var) {
  # derive summary stats for supplied variable
  mean_print <- round(mean(df[[var]], 2))
  sd_print <- round(sd(df[[var]]), 2)
  
  glue::glue("{mean_print} ({sd_print})")
}
