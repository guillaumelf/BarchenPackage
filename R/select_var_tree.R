#' Selects variables
#'
#' This function will select variables from the general dataset to include them in the decision tree.
#'
#' @param tab A data.table object containing the clustering data.
#' @return A data.table object with the data to analyse.
#' @examples
#' select_var_tree(tab)
#' @export

select_var_tree <- function(tab){
  decision <- data.table(variable = names(tab),
                         presence = (stringr::str_detect(names(tab),"userid") | stringr::str_detect(names(tab),"examblanc") | stringr::str_detect(names(tab),"nb_moy_questions") | stringr::str_detect(names(tab),"type_exam") ) )
  retenues <- decision[presence == FALSE]$variable
  tab <- as.data.frame(tab)
  tab <- tab[, retenues]
  tab <- as.data.table(tab)
  tab$resultat <- factor(tab$resultat)
  return(tab)
}