#' @description implement the function 'updateSeeds' to calculate the correct points
#' @details the first seed would be the farther event in the cluster: To pick random points, then record in the table. Verify the generated points whether in the list. When processing next points, 第二个点， 以第一个点位起始点， 到其他点距离， 按顺序排列后， 斜率最大的. input data frame instead file address
getClusters <- function(cluster_list, standard_matrix, dist_matrix, ...) {
  if (length(cluster_list) == 0) {
    seeds <- initialSeeds(dist_matrix = dist_matrix)
  } else {
    cluster_list.elements <- lapply(names(cluster_list), function(name) {
      cluster_list[[name]]$Elements
    }) %>% setNames(., names(cluster_list))
    seeds <- updateSeeds(cluster_list = cluster_list.elements, dist_matrix = dist_matrix)
  }

  # Process when the 'updateSeeds' fails
  if (is.null(seeds) & length(seeds) < 2) {
    # Stop DDC Process: Update Seeds process have no enough available elements
    return(NULL)
  } else {
    clusters <- balanceCluster(seeds = seeds, standard_matrix = standard_matrix, dist_matrix = dist_matrix, ...)
  }
  # Signal to stop clustering
  if (is.null(clusters)) {
    # Stop DDC Process: Stop DDC Process
    return(NULL)
  } else {
    return(clusters)
  }
}



#' @description
#' The main point is to validate the order of the seeds and the updated seeds and the difference of it may lead to wrong mistake in the process.
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @importFrom stats na.omit
balanceCluster <- function(seeds, standard_matrix, dist_matrix, ...) {
  seedsHash <- list()
  # Initial the seeds based on the input.
  seedsHash[as.numeric(seeds) %>%
    sort() %>%
    as.character() %>%
    convertSeedsToString()] <- TRUE

  while (TRUE) {
    # The seeds are the input seeds from the function outside or last loop, if these seeds are never repeated in the past. The following codes will calculate new clusters and DBA based on these seeds
    # Then find the event in the cluster, which has the smallest DTW distance with the cluster DBA
    # Cluster by seeds and the cluser list is based on the input /last loop seeds
    cluster_list <- clusterBySeeds(seeds = seeds, dist_matrix = dist_matrix)
    # Calculate DBA and Update Centroid
    seeds_info <- lapply(seeds, function(element) {
      events <- as.character(element) %>%
        cluster_list[[.]] %>%
        na.omit(.) %>%
        as.character()
      data.list <- lapply(events, function(event) {
        return(as.numeric(na.omit(standard_matrix[event][, 1])))
      }) %>% setNames(., events)
      data.DBA <- dtwclust::DBA(data.list, data.list[[as.character(element)]], trace = FALSE)
      # record the DBA, if the seeds are correct, then print them all.
      elementCentorid <- updateCentroid(
        DBAValue = data.DBA, cluster_events = cluster_list[[as.character(element)]] %>% na.omit() %>% as.character(),
        standard_matrix = standard_matrix, ...
      )
      elementCentorid <- ifelse(element %in% elementCentorid, element, elementCentorid[1])
      return(list(Centroid = elementCentorid, DBAValue = data.DBA))
    })
    # Update the seeds from the previous seeds and the updated seeds should follow the order of the previous one, since they should match the clusterList
    seeds <- lapply(seeds_info, `[[`, 1) %>%
      unlist() %>%
      as.character()
    # Update the name of "clusterList", which should be updated seeds
    # Since the Process needs the seeds from the names in clusterList, the step is very important and the elements in the cluster have to be exported by "temp = clusterList[[as.character(seed)]] %>% na.omit()"
    names(cluster_list) <- seeds
    names(seeds_info) <- seeds
    # Update the seeds to make them output by increasing order
    seeds %<>% as.numeric(.) %>%
      sort() %>%
      as.character()
    seedsString <- as.numeric(seeds) %>%
      sort() %>%
      as.character() %>%
      convertSeedsToString()

    # Validate whether the seeds have been repeated in the past loops
    if (is.null(seedsHash[[seedsString]])) {
      seedsHash[seedsString] <- TRUE
    } else {
      sapply(seeds, function(seed) {
        temp <- cluster_list[[as.character(seed)]] %>% na.omit()
      })
      # Add DBA ane Elements into clusterCurrent.list
      cluster_list <- clusterBySeeds(seeds = seeds, dist_matrix = dist_matrix)
      updated_cluster_list <- lapply(seeds, function(name) {
        list(Centroid = name, Elements = as.numeric(na.omit(cluster_list[[name]])), DBAValue = seeds_info[[name]]$DBAValue)
      }) %>% setNames(., as.character(seeds))
      return(updated_cluster_list)
    }
  }
  return(NULL)
}


#' @description
#' 选取 有最多 最小距离的点，作为centroid
#' Using semgented DTW in the new functions.
"selectCentroid" <- function(dist_matrix, selected_events) {
  if (!is.data.frame(dist_matrix)) {
    stop("The DistMtrx input in seed.farthest is not data frame format")
  }
  if (length(selected_events) < 3) {
    return(selected_events[1])
  }
  selected_dist_matrix <- dist_matrix[as.character(selected_events), as.character(selected_events)]
  # Statistics the nearest events to the current event
  event_stats <- lapply(selected_events, function(rowName) {
    selected_dist_matrix[rowName, !colnames(selected_dist_matrix) %in% colnames(selected_dist_matrix[rowName])] %>%
      unlist() %>%
      .[. == min(.)] %>%
      names()
  })
  centroid_event <- unlist(event_stats) %>%
    table() %>%
    .[. == max(.)] %>%
    names() %>%
    .[1]
  return(centroid_event)
}


#' @description select the event, which has the smallest dtw distance with the DBA,  seed also be compared with DBA
#' @param DBAValue a DBA value
#' @param cluster_events the labels from the label matrix
#' @param standard_matrix the same dataset as standard_matrix created
#' @return the array of char
"updateCentroid" <- function(DBAValue, cluster_events, standard_matrix, ...) {
  # Define the segment length used to as window to find peaks: WindowSize
  # Define the threshold value to filter peaks: peaksThreshold
  data_set <- lapply(cluster_events, function(element) {
    standard_matrix[[as.character(element)]] %>%
      na.omit() %>%
      as.numeric()
  }) %>% setNames(., as.character(cluster_events))
  centroid_event <- standardDTW(data_set = data_set, y = DBAValue, ...) %>%
    .[. == min(.)] %>%
    names()
  return(centroid_event)
}


#' @description
#' Using the standard dtw in mclapply speeding up
"standardDTW" <- function(data_set, y, mc.cores = 1, ...) {
  mc.cores <- ifelse(mc.cores > length(data_set), length(data_set), mc.cores)
  dtw_dist_list <- parallel::mclapply(data_set, FUN = function(x) {
    dtw::dtw(x, y, ...)$distance
  }, mc.cores = mc.cores) %>%
    unlist() %>%
    setNames(., names(data_set))
  return(dtw_dist_list)
}
