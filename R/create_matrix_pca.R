#' Creates a matrix for a PCA
#'
#' This function will transform your data into a matrix that can be used to realise a PCA.
#'
#' @param tab A data.table object containing the data you want to analyse.
#' @param population A character which indicates which population you want to analyse. Default value is "banque_detail", other possible value is "autre".
#' @return A data frame with only numeric variables and userid as row names.
#' @examples
#' tab <- create_matrix_pca(tab)
#' @export

create_matrix_pca <- function(tab,population = "banque_detail"){
  tab$categorie <- as.factor(tab$categorie)
  tab <- tab[categorie == population]
  userid <- tab$userid
  tab[, c("userid","categorie") := NULL]
  mat <- model.matrix(resultat~.-1,tab)
  tab <- data.frame(mat,row.names = userid)
  return(tab)
}