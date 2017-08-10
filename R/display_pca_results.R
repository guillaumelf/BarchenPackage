#' Displays PCA results
#'
#' This function will plot the factorial plan of your choice as well as the table with contribution (CTR) and \code{COS²} of each individual on the correspondant axis.
#'
#' @param res_pca A list containing the PCA results.
#' @param axe1 A numeric indicating the first axis you want to use to plot the factorial plan. Default value is 1.
#' @param axe2 A numeric indicating the second axis you want to use to plot the factorial plan. Default value is 2.
#' @return Plots the factorial plan you chose as well as PCA results for these axis.
#' @examples
#' ## You want to see the results on the first factorial plan
#' 
#' display_pca_results(res_pca, axe1 = 1, axe2 = 2)
#' @export

display_pca_results <- function(res_pca, axe1 = 1, axe2 = 2){
  FactoMineR::plot.PCA(res_pca,axes = c(axe1,axe2), choix = "ind")
  
  axe1_ind <- data.frame(CTR = res_pca$ind$contrib[,axe1], COS2 = res_pca$ind$cos2[,axe1], coord = res_pca$ind$coord[,axe1])
  axe1_ind <- data.table(axe1_ind, keep.rownames=TRUE)
  axe1_ind[, seuil := 1/nrow(axe1_ind)]
  axe1_ind <- axe1_ind[CTR > seuil][order(-CTR)]
  View(axe1_ind)
  
  axe2_ind <- data.frame(CTR = res_pca$ind$contrib[,axe2], COS2 = res_pca$ind$cos2[,axe2], coord = res_pca$ind$coord[,axe2])
  axe2_ind <- data.table(axe2_ind, keep.rownames=TRUE)
  axe2_ind[, seuil := 1/nrow(axe2_ind)]
  axe2_ind <- axe2_ind[CTR > seuil][order(-CTR)]
  View(axe2_ind)
}