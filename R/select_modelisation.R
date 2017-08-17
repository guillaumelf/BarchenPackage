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
                         presence = (stringr::str_detect(names(tab),"userid") | stringr::str_detect(names(tab),"t4") | stringr::str_detect(names(tab),"t5") | stringr::str_detect(names(tab),"resultat") ) )
  retenues <- decision[presence == FALSE]$variable
  tab <- as.data.frame(tab)
  tab <- tab[, retenues]
  tab <- as.data.table(tab)
  return(tab)
}

