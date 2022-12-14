#' @description
#' Based on the new design of accracy: Confusion Matrix
#' StandardSet should be numeric
#' list.cluster still be list of string
#' TP = c()      # Common points in Author and mine
#' FN = c()      # Author's points, but not in mine
#' FP = c()      # My points, not in Ahtor's
#' TN = c()      # Neither has the points
#' @param merged_cluster_list: the lists merged from the cluster of end_cluster_num into the clusers of the number of labels
#' @param label_matrix: the matrix including all the events and the types
#' @param cluster_list_by_label: the events clustered by event types
#' @returns the accuracy value of Confustion Matrix
"calcDDCAccuracy" <- function(merged_cluster_list, label_matrix, cluster_list_by_label) {
  # try to calculate the elements in every group and find the percent of these elements in author's group
  label_types <- as.numeric(label_matrix) %>%
    unique() %>%
    sort() %>%
    as.character()

  average_accu <- lapply(label_types, function(label) {
    AuthorArray <- cluster_list_by_label[[label]]$Elements
    NotInAuthor <- unlist(cluster_list_by_label) %>% .[!. %in% AuthorArray]

    PrivateArray <- merged_cluster_list[[label]]
    NotInPrivateArray <- unlist(cluster_list_by_label) %>% .[!. %in% PrivateArray]

    createConfusionMatrix.value <- createConfusionMatrix(AuthorArray = AuthorArray, NotInAuthor = NotInAuthor, Array = PrivateArray, NotInArray = NotInPrivateArray)
    accuracy <- (createConfusionMatrix.value$TP + createConfusionMatrix.value$TN) / (createConfusionMatrix.value$TP + createConfusionMatrix.value$TN + createConfusionMatrix.value$FP + createConfusionMatrix.value$FN)
    return(accuracy)
  }) %>% mean()
  return(average_accu)
}

#' @description
#' To create the confusion matrix, which will categorize the predictions against the actual values
"createConfusionMatrix" <- function(AuthorArray, NotInAuthor, Array, NotInArray) {
  TP <- length(intersect(Array, AuthorArray))
  FN <- length(intersect(NotInArray, AuthorArray))
  FP <- length(intersect(Array, NotInAuthor))
  TN <- length(intersect(NotInAuthor, NotInArray))
  value <- data.frame(TP, FN, FP, TN)
  return(value)
}
