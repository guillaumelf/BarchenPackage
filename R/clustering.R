#' Performs a clustering on PCA results
#'
#' This function will perform a clustering from PCA results.
#'
#' @param tab A data.table object containing the data on which the PCA has been performed.
#' @param tab2 A data.table object containing variables you want to add after performing the clustering for the description in each group.
#' @param results A list with the results of a PCA.
#' @param nb.max.clusters A numeric indicating the maximum number of clusters you want to have. Default value is 10.
#' @param size.min A numeric indicating how many individuals a cluster must contain at least. Default value is 30.
#' @return A dataframe with original data and the clusters associated to each individual.
#' @examples
#' ## You want to get a maximum of 8 clusters with at least 20 individuals in each of them :
#' 
#' donnees <- clustering(res_pca,nb.max.clusters = 8, size.min = 20)
#' @note  Clusters with a size inferior to what is specified inside the argument \code{size.min} will be removed.
#' Results will be added only after the clustering is done, as it is a non-supervised one.
#' @export

clustering <- function(tab,tab2,results,nb.max.clusters = 10,size.min = 30){
  res_clust <- FactoMineR::HCPC(results,nb.clust = nb.max.clusters, graph = FALSE)
  cluster <- c()
  size <- c()
  for(i in 1:nb.max.clusters){
    cluster <- c(cluster,i)
    size <- c(size,summary(res_clust$data.clust$clust)[[i]])
  }
  res <- data.table(cluster,size)
  retenus <- res[size >= size.min]$cluster
  retenus <- as.character(retenus)
  donnees <- data.table(tab,cluster = res_clust$data.clust$clust,keep.rownames = TRUE)
  donnees <- donnees[cluster %in% retenus]
  donnees$cluster <- factor(donnees$cluster)
  row.names(donnees) <- donnees$rn
  donnees[, userid := as.character(rn)][, rn := NULL]
  tab2$userid <- as.character(tab2$userid)
  liste <- list(donnees,tab2)
  donnees <- left_join(list_tab = liste)
  donnees[, userid := NULL]
  return(donnees)
}

#' Changes cluster labels
#'
#' This function will change the cluster labels.
#'
#' @param tab A data.table object containing the cluster labels.
#' @return A data.table with the new cluster labels.
#' @examples
#' ## You perform a clustering and get these groups :
#' 
#' test <- clustering(tab,res_pca,nb.max.clusters = 8,size.min = 30)
#' summary(test$cluster)
#' ##   1   2   3   4   6 
#' ## 184  82  34 126  25
#' 
#' test <- change_labels(test)
#' summary(test$cluster)
#' ##   1   2   3   4   5 
#' ## 184  82  34 126  25
#' @export

change_labels <- function(tab){
  nb <- length(summary(tab$cluster))
  levels(tab$cluster) <- factor(1:nb)
  return(tab)
}

#' Plots clusters's size
#'
#' This function will perform a barplot to represent each cluster's size.
#'
#' @param tab A data.table or data.frame object containing the data with clusters.
#' @return The barplot representing the results.
#' @export

plot.clusters <- function(tab){
  cluster <- c()
  size <- c()
  for(i in 1:length(levels(tab$cluster))){
    cluster <- c(cluster,i)
    size <- c(size,summary(tab$cluster)[[i]])
  }
  cluster <- factor(cluster)
  res <- data.frame(cluster,size)
  rAmCharts::amBarplot(x = "cluster", y = "size",data = res, ylim = c(0,max(res$size)+1), main = "Nombre d'individus au sein des clusters retenus")
}