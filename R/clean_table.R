#' Cleans the table
#'
#' This function will clean the table by removing outliers.
#'
#' @param tab A data.table object containing the table you want to clean.
#' @return A data.table object with the results wanted.
#' @examples
#' tab_clean <- clean_table(tab)
#' @export

clean_table <- function(tab){
  examblanc <- dplyr::select(tab,contains("nb_examblanc_attempt"))
  userid <- tab$userid
  res <- apply(examblanc, MARGIN = 1, FUN = sum)
  table <- data.table(userid,res)
  retenus <- table[res <= 6]$userid
  tab <- tab[userid %in% retenus]
  
  return(tab)
}