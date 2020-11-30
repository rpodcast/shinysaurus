# simple function to extract a specific datasauRus data set
extract_dataset <- function(dataset) {
  df <- datasauRus::datasaurus_dozen
  
  df <- df %>%
    dplyr::filter(dataset == !!dataset) %>%
    select(x, y)
  
  return(df)
}

#' Create plotly version of animation
#'
#' @param metamer_df tidy data frame of metamer transition frames
#' @param metamer_sum tidy data frame of metamer summary statistics
#' @param frame amount of time between frames (in milliseconds)
#'
#' @return plotly object
#' @export
#' @import plotly
render_animation_graph <- function(metamer_df, metamer_sum, frame = 100) {
  base <- plot_ly(metamer_df,  x = ~x, y = ~y) %>%
    add_markers(frame = ~.metamer) %>%
    add_text(x = 90, y = 95, frame = ~.metamer, text = ~mean_x_print, data = metamer_sum, showlegend = FALSE) %>%
    add_text(x = 90, y = 93, frame = ~.metamer, text = ~mean_y_print, data = metamer_sum, showlegend = FALSE) %>%
    add_text(x = 90, y = 91, frame = ~.metamer, text = ~cor_xy_print, data = metamer_sum, showlegend = FALSE) %>%
    layout(
      xaxis = list(range = c(0, 100)),
      yaxis = list(range = c(0, 100))
    )

  base %>%
    animation_opts(frame = frame, easing = "linear", redraw = FALSE) %>%
    animation_button(
      x = 1, xanchor = "right", y = 0, yanchor = "bottom"
    )
}

#' Create a series of metamer data sets
#'
#' @param datasets vector of data sets
#' @param ... additional arguments directly to metamer metamerize function
#' @inheritParams metamer::metamerize
#'
#' @return a metamer list object (list of data frames)
#' @import metamer
#' @import dplyr
#' @export
create_metamers <- function(datasets, perturbation = 0.08, N = 30000, trim = 150, ...) {
  # obtain the first and last elements of datasets
  first_dataset <- datasets[1]
  second_dataset <- datasets[2]
  last_dataset <- datasets[length(datasets)]
  
  # derive first set of metamers
  df1 <- extract_dataset(first_dataset)
  df2 <- extract_dataset(second_dataset)
  
  metamers <- metamerize(
    data = df1,
    preserve = delayed_with(mean(x), mean(y), cor(x, y)),
    minimize = mean_dist_to(df2),
    perturbation = perturbation,
    N = N,
    trim = trim)
  
  # create additional metamers if we have more than 2 datasets
  # TODO: I got pretty close but following needs to happen
  # - store all of the looped versions of the metamers data frames in a list
  # - manually fix the .metamer column so that we properly "roll" the count
  #   as the data sets are generated. Need to keep track of the total
  if (length(datasets) > 2) {
    
    # extract the 3rd or more dataset names
    remaining_datasets <- datasets[3:length(datasets)]
    
    # establish list to store all data frame version of results
    meta_list <- vector("list", length(remaining_datasets))
    names(meta_list) <- remaining_datasets
    loop_n_trim <- 10 + trim
    
    for (ds in remaining_datasets) {
      message(ds)
      
      
      start_df <- metamers[[length(metamers)]]
      metamers_tmp <- metamerize(
        data = start_df, 
        preserve = delayed_with(mean(x), mean(y), cor(x, y)),
        minimize = NULL, 
        perturbation = perturbation,
        N = N / 10, 
        trim = 10
      )
      metamers_tmpdf <- as.data.frame(metamers_tmp)
      
      target_df <- extract_dataset(ds)
      metamers_tmp2 <- metamerize(
        data = metamers_tmp[[length(metamers_tmp)]],
        preserve = delayed_with(mean(x), mean(y), cor(x, y)),
        minimize = mean_dist_to(target_df),
        perturbation = perturbation,
        N = N,
        trim = trim)
      
      #metamers_tmp2df <- as.data.frame(metamers_tmp2)
      #metamers_tmpdf_all <- dplyr::bind_rows(metamers_tmpdf, metamers_tmp2df)
      #metamers <- c(metamers, metamers_tmp, metamers_tmp2)
    }
  }
  
  return(metamers)
}
