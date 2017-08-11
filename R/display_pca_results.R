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
  
  axe1_ind <- data.frame(CTR = round(res_pca$ind$contrib[,axe],2), COS2 = round(res_pca$ind$cos2[,axe],2), coord = round(res_pca$ind$coord[,axe],2))
  axe1_ind <- data.table(axe1_ind, keep.rownames=TRUE)
  axe1_ind[, seuil := 1/nrow(axe1_ind)]
  axe1_ind <- axe1_ind[CTR > seuil][order(-CTR)]
  axe1_ind[, seuil := NULL]
  axe1_ind <- as.data.frame(axe1_ind)
  names(axe1_ind) <- c("Individu","CTR","COS2","Coordonnée")
  return(axe1_ind)
}