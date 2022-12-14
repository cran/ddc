#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @importFrom stats na.omit
#' @importFrom stats setNames
#' @importFrom stats as.dist
#' @importFrom dtw dtw
utils::globalVariables(".")


#' @title Execute DDC to cluster the dataset
#' @param standard_matrix the original data matrix
#' @param dist_matrix the created dist matrix
#' @param label_matrix the matrix including events  and labels
#' @param end_cluster_num the max number of cluster when the procedue ends
#' @param ... including: mc.cores(cores used in parallel), the dtw parameters like step.pattern, keep, mc.cores
#' @return the cluster array as a result, including 'Centroid', 'Elements' and 'DBAValue' for each cluster
#' @examples
#' \donttest{
#' original_data <- data.frame("1"=c(1, 2, 1), "2"=c(5,6,7), 
#'    "3"=c(4, 5, 8), "4"=c(3, 1, 9))
#' standard_matrix <- createStandardMatrix(data = original_data)
#' label_matrix <- createLabelMatrix(data = original_data)
#' dist_matrix <- createDistMatrix(standard_matrix = standard_matrix)
#' result <- ddc(dist_matrix=dist_matrix, standard_matrix=standard_matrix,
#'    label_matrix=label_matrix, end_cluster_num=2)
#' }
#' @export
"ddc" <- function(dist_matrix, standard_matrix, label_matrix, end_cluster_num = NULL, ...) {
  cluster_list <- list()

  if (is.null(label_matrix)) {
    stop("ddc: No inpput value for for param 'label_matrix'. ")
  }

  MIN_SEEDS <- as.numeric(label_matrix) %>%
    unique() %>%
    length()
  MAX_SEEDS <- ifelse(is.null(end_cluster_num), 5 * MIN_SEEDS, end_cluster_num)
  start_point <- 2

  for (value in start_point:MAX_SEEDS) {
    # cluster_list: include the cluster elements and the cluster DBAs, named after the seed events
    tryCatch(
      {
        cluster_list <- getClusters(cluster_list = cluster_list, standard_matrix = standard_matrix, dist_matrix = dist_matrix, ...)
      },
      error = function(error_message) {
        stop(paste("ddc: Error in DDC Cluster Process.", toString(error_message), ".", sep = " "))
      }
    )
    # When Function "getClusters" stop, the cluster stop and jump out of the loop
    if (is.null(cluster_list)) {
      stop(paste("ddc: Stop Cluster Process with the current cluster value: ", toString(value - 1), ".", sep = " "))
      break
    }
  }
  return(cluster_list)
}
