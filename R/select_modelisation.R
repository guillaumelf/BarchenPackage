#' Selects variables
#'
#' This function will select variables from the general dataset, to keep only the first 3 phases informations.
#'
#' @param tab A data.table object containing the clustering data.
#' @return A data.table object with the data to analyse.
#' @examples
#' select_modelisation(tab)
#' @export

select_modelisation <- function(tab){
  decision <- data.table(variable = names(tab),
                         presence = (stringr::str_detect(names(tab),"userid") | stringr::str_detect(names(tab),"t4") | stringr::str_detect(names(tab),"t5") | stringr::str_detect(names(tab),"resultat") | stringr::str_detect(names(tab),"duree_totale_plateforme") | stringr::str_detect(names(tab),"nb_total_mooc_used")| stringr::str_detect(names(tab),"nb_total_mooc_prepared") | stringr::str_detect(names(tab),"proportion_mooc_prepared") | stringr::str_detect(names(tab),"proportion_mooc_used") ) )
  retenues <- decision[presence == FALSE]$variable
  tab <- as.data.frame(tab)
  tab <- tab[, retenues]
  tab <- as.data.table(tab)
  return(tab)
}

