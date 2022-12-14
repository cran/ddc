#' @title Create the dataframe of the Dissimilarity matrix
#' @description Use the DTW to generate the matrix
#' @param standard_matrix the matrix genereated by function 'createStandardMatrix'
#' @param output_dir the file to save the dissimilarity matrix data
#' @param mc.cores the number of cores would be used in parallel
#' @param ... the same parameters which would be used in 'dtw' for calculating the distances of events
#' @return the matrix, which describes pairwise distinction between M objects. It is a square symmetrical 'MxM' matrix with the (ij)th element equal to the value of a chosen measure of distinction between the (i)th and the (j)th object.
#' @examples
#' \donttest{
#' original_data <- data.frame("1"=c(1, 2, 1), "2"=c(5,6,7), 
#'    "3"=c(4, 5, 8), "4"=c(3, 1, 9))
#' standard_matrix <- createStandardMatrix(data = original_data)
#' dist_matrix <- createDistMatrix(standard_matrix = standard_matrix)
#' }
#' @export
createDistMatrix <- function(standard_matrix, output_dir = NULL, mc.cores = 1, ...) {
  dtwDist.list <- parallel::mclapply(1:(ncol(standard_matrix) - 1), FUN = function(col) {
    colData <- standard_matrix[, col] %>% as.numeric()
    # Return the dtw values which would be used in the dist matrix column
    dtwDist.col <- lapply((col + 1):ncol(standard_matrix), FUN = function(pointer) {
      pointerData <- standard_matrix[, pointer] %>% as.numeric()
      dtwValue <- dtw::dtw(colData, pointerData, ...)$distance
      return(dtwValue)
    }) %>% unlist()
    # Add NA in the front of the array "dtwDist.col", to complete the array in the matrix
    dtwDist.array <- c(rep(NA, col), dtwDist.col)
    return(dtwDist.array)
  }, mc.cores = mc.cores)
  dtwDist.list[[length(dtwDist.list) + 1]] <- rep(NA, ncol(standard_matrix))
  dist_matrix <- do.call("cbind", dtwDist.list)
  dist_matrix %<>% as.dist() %>% as.matrix()
  colnames(dist_matrix) <- seq_len(ncol(standard_matrix)) %>% as.character()
  rownames(dist_matrix) <- seq_len(ncol(standard_matrix)) %>% as.character()
  if (!is.null(output_dir)) {
    connection <- file(output_dir)
    utils::write.csv(dist_matrix, connection)
  }
  dist_matrix %<>% as.data.frame()
  return(dist_matrix)
}


#' @title Create the dataframe with event names and the related labels
#' @param data data structure as the files in "UCR Time Series Classification Archive"
#' @param output_dir the file to save the label matrix data
#' @return the dataframe, including event names and labels
#' @examples
#' \donttest{
#' original_data <- data.frame("1"=c(1, 2, 1), "2"=c(5,6,7), 
#'    "3"=c(4, 5, 8), "4"=c(3, 1, 9))
#' label_matrix <- createLabelMatrix(data = original_data)
#' }
#' @export
"createLabelMatrix" <- function(data, output_dir = NULL) {
  label_matrix <- t(data[, 1])
  colnames(label_matrix) <- seq_len(ncol(label_matrix)) %>% as.character()
  if (!is.null(output_dir)) {
    connection <- file(output_dir)
    utils::write.csv(label_matrix, connection)
  }
  label_matrix %<>% as.data.frame()
  return(label_matrix)
}


#' @title Create the dataframe, only including the event data
#' @param data data structure as the files in "UCR Time Series Classification Archive"
#' @param output_dir the file to save the standard matrix data
#' @return the dataframe of event data
#' @examples
#' \donttest{
#' original_data <- data.frame("1"=c(1, 2, 1), "2"=c(5,6,7), 
#'    "3"=c(4, 5, 8), "4"=c(3, 1, 9))
#' standard_matrix <- createStandardMatrix(data = original_data)
#' }
#' @export
"createStandardMatrix" <- function(data, output_dir = NULL) {
  standard_matrix <- t(data) %>%
    .[-1, ] %>%
    as.data.frame()
  colnames(standard_matrix) <- seq_len(ncol(standard_matrix)) %>% as.character()
  rownames(standard_matrix) <- seq_len(nrow(standard_matrix)) %>% as.character()
  if (!is.null(output_dir)) {
    connection <- file(output_dir)
    utils::write.csv(standard_matrix, connection)
  }
  standard_matrix %<>% as.data.frame()
  return(standard_matrix)
}
