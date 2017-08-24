#' Plots cluster infos
#'
#' This function will calculate the proportion of people who attend each kind of exam for every cluster.
#'
#' @param tab A data.table object containing the clustering dataset.
#' @return A data.table object with the results for each cluster..
#' @examples
#' res <- get_exam_types(tab)
#' @export

get_exam_types <- function(tab){
  decision <- data.table(variable = names(tab),
                         presence = (stringr::str_detect(names(tab),"cluster") | stringr::str_detect(names(tab),"type_exam")))
  retenues <- decision[presence == TRUE]$variable
  tab <- as.data.frame(tab)
  tab <- tab[, retenues]
  tab <- as.data.table(tab)
  
  size <- tab[, .N, by = cluster][order(cluster)]
  res <- tab[, lapply(.SD, sum), by=cluster][order(cluster)]
  setkey(size,cluster)
  setkey(res,cluster)
  res <- res[size,nomatch=0]
  names(res) <- c("cluster","Double echelle","Examen certifie","Validation Interne","Size")
  res[, c("Double echelle","Examen certifie","Validation Interne") := list(100*round((`Double echelle`/Size),2),
                                                                           100*round((`Examen certifie`/Size),2),
                                                                           100*round((`Validation Interne`/Size),2))]
  return(res)
}