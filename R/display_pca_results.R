#' Displays PCA results
#'
#' This function will display the table with contribution (CTR) and \code{COS²} of each individual on the axis of your choice.
#'
#' @param res_pca A list containing the PCA results.
#' @param axe A numeric indicating the first axis you want to use to plot the factorial plan. Default value is 1.
#' @return A data.frame object with the results of the axis.
#' @examples
#' ## You want to see the results of the first axis :
#' 
#' display_pca_results(res_pca, axe = 1)
#' @export

display_pca_results <- function(res_pca, axe = 1){
  
  axe1_ind <- data.frame(CTR = res_pca$ind$contrib[,axe1], COS2 = res_pca$ind$cos2[,axe1], coord = res_pca$ind$coord[,axe1])
  axe1_ind <- data.table(axe1_ind, keep.rownames=TRUE)
  axe1_ind[, seuil := 1/nrow(axe1_ind)]
  axe1_ind <- axe1_ind[CTR > seuil][order(-CTR)]
  axe1_ind[, seuil := NULL]
  axe1_ind <- as.data.frame(axe1_ind)
  return(axe1_ind)
}