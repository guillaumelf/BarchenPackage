#' Displays cluster description
#'
#' This function will display the results of the \code{v.test} on each cluster, showing variables that distinguish these individuals from others the most.
#'
#' @param tab A data.table object containing the original data with cluster labels associated to each individual.
#' @param cluster.label A numeric indicating which cluster's characteristics you want to display. Default value is 1.
#' @return The data.frame with the results.
#' @examples
#' ## You want to see more details about the cluster with label 5 
#' ## of the dataset 'tab' ?
#' 
#' cluster_description(tab,cluster.label = 5)
#' @export

cluster_description <- function(tab, cluster.label = 1){
  desc <- FactoMineR::catdes(tab, num.var = which(names(tab)=="cluster"))
  n <- length(levels(tab$cluster))
  liste <- list()
  for(i in 1:n){
    assign(paste0("cluster",i),desc[[2]][i])
    liste[[i]] <- get(paste0("cluster",i))
  }
  df <- data.frame(liste[[cluster.label]])
  names(df) <- c("v.test","Mean in category","Overall mean","sd in catgory","Overall sd","p-value")
  return(df)
}