#' Creates new data to make predictions
#'
#' This function will create a dataframe in order to perform predictions.
#'
#' @param tab A data.table object containing the train dataset.
#' @param newdata A data.table object containing the test dataset.
#' @return A data.table object with the results wanted.
#' @examples
#' tab <- create_dtest(tab,newdata)
#' @export

create_dtest <- function(tab,newdata){
  tableau <- plyr::rbind.fill(tab,newdata)
  tableau <- data.table(tableau)
  
  infos <- tab[, .(cluster,resultat)]
  infos$userid <- rownames(infos)
  decision <- data.table(variable = names(tableau),
                         presence = (stringr::str_detect(names(tableau),"userid") | stringr::str_detect(names(tableau),"t4") | stringr::str_detect(names(tableau),"t5") | stringr::str_detect(names(tableau),"resultat") | stringr::str_detect(names(tableau),"duree_totale_plateforme") | stringr::str_detect(names(tableau),"nb_total_mooc_used")| stringr::str_detect(names(tableau),"nb_total_mooc_prepared") | stringr::str_detect(names(tableau),"proportion_mooc_prepared") | stringr::str_detect(names(tableau),"proportion_mooc_used") ) )
  retenues <- decision[presence == FALSE]$variable
  tableau <- as.data.frame(tableau)
  tableau <- tableau[, retenues]
  tableau <- as.data.table(tableau)
  tableau[, c("cluster","nb_total_questions") :=NULL]
  names(tableau)
  tableau <- na_replace(tableau,0)
  return(tableau)
}
