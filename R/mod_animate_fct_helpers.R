# simple function to extract a specific datasauRus data set
extract_dataset <- function(dataset) {
  df <- datasauRus::datasaurus_dozen
  
  df <- df %>%
    dplyr::filter(dataset == !!dataset) %>%
    select(x, y)
  
  return(df)
}

#' Create a series of metamer data sets
#'
#' @param datasets vector of data sets
#' @param ... additional arguments directly to metamer metamerize function
#' @inheritParams metamer::metamerize
#'
#' @return a metamer list object (list of data frames)
#' @import metamer
#' @export
create_metamers <- function(datasets, perturbation = 0.08, N = 30000, trim = 150, ...) {
  # obtain the first and last elements of datasets
  first_dataset <- datasets[1]
  last_dataset <- datasets[length(datasets)]
  
  
  
  # start_data <- subset(datasauRus::datasaurus_dozen, dataset == "dino")
  # start_data$dataset <- NULL
  
  # X <- subset(datasauRus::datasaurus_dozen, dataset == "x_shape")
  # X$dataset <- NULL
  # 
  # star <- subset(datasauRus::datasaurus_dozen, dataset == "star")
  # star$dataset <- NULL
  
  
  # metamers <- metamerize(start_data, 
  #                        preserve = delayed_with(mean(x), mean(y), cor(x, y)),
  #                        minimize = mean_dist_to(smiley), 
  #                        perturbation = 0.08,
  #                        N = 30000,
  #                        trim = 150) %>% 
  #   metamerize(minimize = NULL, 
  #              N = 3000, trim = 10) %>% 
  #   metamerize(minimize = mean_dist_to(X), 
  #              N = 30000, trim = 150) %>% 
  #   metamerize(minimize = NULL, 
  #              N = 3000, trim = 10) %>% 
  #   metamerize(minimize = mean_dist_to(star), 
  #              N = 30000, trim = 150) %>%
  #   metamerize(minimize = NULL, 
  #              N = 3000, trim = 10) %>% 
  #   metamerize(minimize = mean_dist_to(start_data),
  #              N = 30000, trim = 150)
  
  # X <- subset(datasauRus::datasaurus_dozen, dataset == "x_shape")
  # X$dataset <- NULL
  
  # derive first set of metamers
  df <- extract_dataset(first_dataset)
  
  metamers <- metamerize(
    #data = extract_dataset(first_dataset),
    data = df,
    preserve = delayed_with(mean(x), mean(y), cor(x, y)),
    minimize = mean_dist_to(extract_dataset(datasets[2])),
    perturbation = perturbation,
    N = N,
    trim = trim)

  # create additional metamers if we have more than 2 datasets
  if (length(datasets) > 2) {
    # extract the 3rd or more dataset names
    remaining_datasets <- datasets[3:length(datasets)]
    
    for (ds in remaining_datasets) {
      metamers <- metamerize(metamers, minimize = NULL, N = N / 10, trim = 10)
      df <- extract_dataset(ds)
      metamers <- metamerize(metamers, minimize = mean_dist_to(df), N = N, trim = trim)
    }
  }
  
  return(metamers)
}
