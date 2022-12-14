#' @description
#' The package includes the options on the clusters
#' Funtion used to cluster all the events based on the seeds. The policy should be the event following the nearest seed.
"clusterBySeeds" <- function(seeds, dist_matrix) {
  events <- (!colnames(dist_matrix) %in% seeds) %>% colnames(dist_matrix)[.]
  event_labels <- lapply(events, FUN = function(event) {
    dist_matrix[event, seeds] %>%
      as.numeric() %>%
      setNames(., as.character(seeds)) %>%
      .[. == min(.)] %>%
      names()
  }) %>%
    unlist() %>%
    setNames(., as.character(events))
  clusters <- lapply(seeds, FUN = function(seed) {
    c(seed, event_labels[event_labels == seed] %>% names())
  }) %>% setNames(., as.character(seeds))
  return(clusters)
}


#' @description
#' devide the cluster given
#' get the two parts
#' get the max slope value
"divideCluster" <- function(dist.array) {
  dist.sortarray <- unlist(dist.array) %>% sort()
  diff.sortarray <- as.numeric(dist.sortarray) %>% diff()
  output <- list(
    value = max(diff.sortarray), cluster1 = names(dist.sortarray[1:which(diff.sortarray == max(diff.sortarray))]),
    cluster2 = names(dist.sortarray[(which(diff.sortarray == max(diff.sortarray)) + 1):(length(dist.sortarray))])
  )
  return(output)
}


#' @description
#' the Function is used to cluster all the events based on the label given
"getGroups" <- function(label_matrix = NULL) {
  if (is.null(label_matrix)) {
    stop("Function(getGroups): No label matrix input. ")
  }
  group_labels <- as.numeric(label_matrix) %>%
    unique() %>%
    sort()
  groups <- lapply(group_labels, function(label) {
    list(Elements = which(label_matrix == label) %>% (names(label_matrix))[.])
  }) %>% setNames(., group_labels)
  return(groups)
}

#' @description
#' Get the clusters and merge them into the standard labels and the function is the standard step in the accuracy calculation.
#' The standard label names are replaced the cluster seeds with.
#' @param cluster_list: the completed clusters
#' @param label_matrix: the created the matrix including events and labels
#' @returns after voting by all the events' label in the single cluster, the clustr would be labeled with the event label of the most events in the cluster. Then merge the clsuters with the same label together.
"merge_clusters" <- function(cluster_list, label_matrix) {
  # try to calculate the elements in every group and find the percent of these elements in author's group
  label_types <- as.numeric(label_matrix) %>%
    unique() %>%
    sort() %>%
    as.character()
  # Assign the standard lable to each current cluster
  cluster_labels <- lapply(names(cluster_list), function(name) {
    cluster_label <- cluster_list[[name]]$Elements %>%
      as.character() %>%
      label_matrix[, .] %>%
      as.numeric() %>%
      table() %>%
      sort(., decreasing = TRUE) %>%
      names() %>%
      .[1]
    return(cluster_label)
  }) %>%
    unlist() %>%
    setNames(., names(cluster_list))
  # Merge the cluster events which have the same standard label.
  merged_cluster_list <- lapply(label_types, function(label) {
    clusters_selected <- cluster_labels[cluster_labels == label] %>% names()
    events <- lapply(clusters_selected, function(label) {
      cluster_list[[label]]$Elements
    }) %>%
      unlist() %>%
      as.numeric() %>%
      sort() %>%
      as.character()
    return(events)
  }) %>% setNames(., as.character(label_types))
  return(merged_cluster_list)
}


"set_cluster_labels" <- function(cluster_list, label_matrix) {
  cluster_labels <- lapply(names(cluster_list), function(name) {
    cluster_label <- cluster_list[[name]]$Elements %>%
      as.character() %>%
      label_matrix[, .] %>%
      as.numeric() %>%
      table() %>%
      sort(., decreasing = TRUE) %>%
      names() %>%
      .[1]
    return(cluster_label)
  }) %>%
    unlist() %>%
    setNames(., names(cluster_list))
  return(cluster_labels)
}
