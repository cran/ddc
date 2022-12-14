#' @description
#' when the cluster only has two events, or the dtw between the initial event(farthest, random, nearest) and the others are equal, then stop clustering.
#' return character
#' find the farhter point
#' 根据最远点，将最远点和所有点的dtw值 排序， 计算这些值组成曲线斜率
#' 认为斜率最大的点将cluster分成两组
#' 利用centroid selected 去获取各自cluster的seeds
#' return character
#' find the farhter point
#' 根据最远点，将最远点和所有点的dtw值 排序， 计算这些值组成曲线斜率
#' 认为斜率最大的点将cluster分成两组
#' 利用centroid selected 去获取各自cluster的seeds
#' Update from SeedInitial4:
#' 1. Not jump the groups which have the same dist distance between members
#' 2. Add "next" when the event chosen failed.
#' 3. Next version may include  the parallel fucntion:
#' a. upper limit for the cores used in parallel
#' b. dynamic require for the cores when the amount is under the upper limit.
#'    Next version may need more require
#' a. when to give up splitting in the element group
#' b. when to give up all the initials, which means to stop clustering
#' @param dist_matrix: the created DistMtatrix
#' @returns the seed events used in the following cluster
"initialSeeds" <- function(dist_matrix) {
  events <- rownames(dist_matrix) %>% na.omit()
  event.chosen <- findFarthestSeed(cluster_events = na.omit(events), dist_matrix = dist_matrix)

  if (event.chosen == FALSE) {
    # The cluster can not be initialed
    stop("initialSeeds: The Data Set can not be initialed.")
  }

  cluster.target <- dist_matrix[event.chosen, !events %in% event.chosen] %>% divideCluster(dist.array = .)
  cluster.target$cluster1 <- c(event.chosen, cluster.target$cluster1)

  # Add event chosen to the cluster 1 for expanding the election range
  seed_1 <- selectCentroid(dist_matrix = dist_matrix, selected_events = na.omit(cluster.target$cluster1))
  seed_2 <- selectCentroid(dist_matrix = dist_matrix, selected_events = na.omit(cluster.target$cluster2))

  current_seeds <- c(seed_1, seed_2) %>%
    as.numeric() %>%
    sort() %>%
    as.character()
  return(current_seeds)
}


#' @description
#' when the cluster only has two events, or the dtw between the initial event(farthest, random, nearest) and the others are equal, then stop clustering.
#' return character
#' Using the seed to calculate the difference between the other events in the cluster and the seed event
#' return character
#' find the farhter point
#' Update from SeedInitial4:
#' 1. Not jump the groups which have the same dist distance between members
#' 2. Add "next" when the event chosen failed.
#' 3. Next version may include  the parallel fucntion:
#' a. upper limit for the cores used in parallel
#' b. dynamic require for the cores when the amount is under the upper limit.
#'    Next version may need more require
#' a. when to give up splitting in the element group
#' b. when to give up all the initials, which means to stop clustering
#' @param cluster_list: the latest culstered events
#' @param dist_matrix: the DistMatrix
#' @returns: the latest seeds for next cluster
"updateSeeds" <- function(cluster_list, dist_matrix) {
  clusterInfo <- lapply(names(cluster_list), FUN = function(name) {
    elements <- cluster_list[[name]] %>% na.omit()
    if (length(elements) < 3) {
      # Ignore the Cluster with Seed since too few elemnts in the cluster
      return(NA)
    }
    cluster.temp <- dist_matrix[name, cluster_list[[name]] %>% na.omit() %>% as.character() %>% .[!. %in% name]] %>% divideCluster(dist.array = .)
    cluster.temp$cluster1 <- c(name, cluster.temp$cluster1)
    return(cluster.temp)
  }) %>% setNames(., names(cluster_list))

  slope_arry <- lapply(clusterInfo, `[[`, 1) %>%
    unlist() %>%
    setNames(., names(clusterInfo))

  if (all(is.na(slope_arry))) {
    # Not enough elements in each cluster. Stop clustering.
    return(NULL)
  }

  if (na.omit(slope_arry) %>% max() == 0) {
    # All the events in each cluster are distributed equally.
    return(NULL)
  }

  selected_events <- which(slope_arry == (na.omit(slope_arry) %>% max())) %>% names()
  previous_seeds <- names(cluster_list) %>% .[!. %in% selected_events]

  seed_1 <- selectCentroid(dist_matrix = dist_matrix, selected_events = clusterInfo[[selected_events]]$cluster1 %>% na.omit())
  seed_2 <- selectCentroid(dist_matrix = dist_matrix, selected_events = clusterInfo[[selected_events]]$cluster2 %>% na.omit())

  current_seeds <- c(previous_seeds, seed_1, seed_2) %>%
    as.numeric() %>%
    sort() %>%
    as.character()
  return(current_seeds)
}


#' @description
#' Find the farthest point in the cluster
#' 其他点到该点距离之和最da， 则为最远点
"findFarthestSeed" <- function(cluster_events, dist_matrix) {
  dist_matrix <- dist_matrix[as.character(cluster_events), as.character(cluster_events)]
  colnames(dist_matrix) <- as.character(cluster_events)
  rownames(dist_matrix) <- as.character(cluster_events)

  event_stats <- lapply(as.character(cluster_events), function(row_name) {
    data_temp <- dist_matrix[row_name, !colnames(dist_matrix) %in% colnames(dist_matrix[row_name])] %>% unlist()
    eventsWithMaxDist <- which(data_temp == max(data_temp)) %>% names()
    return(eventsWithMaxDist)
  })

  farthest_event <- unlist(event_stats) %>%
    table() %>%
    .[. == max(.)] %>%
    names() %>%
    .[1]

  # get the dist array and analyse the the dist in the array, if all the values are same, select the second max as seed and continue the loop
  while (TRUE) {
    # IF returning the FALSE, THEN the value means that the cluster can not be seperate
    if (is.na(farthest_event) || is.null(farthest_event)) {
      # No Event is the required farthest seed.
      return(FALSE)
    }
    dist_array <- unlist(dist_matrix[which(!cluster_events == farthest_event), farthest_event])

    if (all(dist_array == dist_array[1])) {
      farthest_event <- unlist(event_stats) %>%
        table() %>%
        .[. == max(.)] %>%
        names() %>%
        .[1]
    } else {
      # RETURN the correct seed
      return(farthest_event)
    }
  }
}


"findNearestSeed" <- function(cluster_events, dist_matrix) {
  dist_matrix <- dist_matrix[as.character(cluster_events), as.character(cluster_events)]
  colnames(dist_matrix) <- as.character(cluster_events)
  rownames(dist_matrix) <- as.character(cluster_events)

  event_stats <- lapply(as.character(cluster_events), function(row_name) {
    data_temp <- dist_matrix[row_name, !colnames(dist_matrix) %in% colnames(dist_matrix[row_name])] %>% unlist()
    eventsWithMinDist <- which(data_temp == min(data_temp)) %>% names()
    return(eventsWithMinDist)
  })

  nearest_event <- unlist(event_stats) %>%
    table() %>%
    sort(., decreasing = TRUE) %>%
    names() %>%
    .[1]

  # get the dist array and analyse the the dist in the array, if all the values are same, select the second max as seed and continue the loop
  while (TRUE) {
    # IF returning the FALSE, THEN the value means that the cluster can not be seperated
    if (is.na(nearest_event) || is.null(nearest_event)) {
      # No Event is the required nearest seed
      return(FALSE)
    }
    dist_array <- unlist(dist_matrix[which(!rownames(dist_matrix[nearest_event]) == nearest_event), nearest_event])
    if (all(dist_array == dist_array[1])) {
      nearest_event <- unlist(event_stats) %>%
        table() %>%
        sort(., decreasing = TRUE) %>%
        names() %>%
        .[1]
    } else {
      # RETURN the correct seed
      return(nearest_event)
    }
  }
}


#' @description
#' input the seeds, int array
#' sort the seeds then changed into one string and returned
#' @param seeds: the input int array of seeds
#' @returns: the sorted seeds in string
"convertSeedsToString" <- function(seeds) {
  seeds_string <- as.numeric(seeds) %>%
    sort() %>%
    toString()
  return(seeds_string)
}
