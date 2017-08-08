#' Merges tables
#'
#' This function will perform a \code{LEFT JOIN} on the userid key.
#'
#' @param list_tab A list of the tables you want to merge.
#' @return A unique data.table object which will be the result of the \code{LEFT JOIN}.
#' @examples
#' tab <- left_join(liste_tabs)
#' @export

left_join <- function(list_tab){
  
  tranches <- length(list_tab)
  for(i in 1:tranches){
    assign(paste0("tab",i), list_tab[[i]])
  }
  
  tab <- tab1
  setkey(tab,userid)
  for(i in 2:tranches){
    setkey(get(paste0("tab", i)),userid)
    assign("tab", tab[get(paste0("tab", i)),nomatch=0])
  }
  tab <- unique(tab)
  return(tab)
}