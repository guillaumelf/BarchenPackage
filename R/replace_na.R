#' Replaces NA in a data.table
#'
#' This function will replace NAs in the data.table with the value of your choice.
#'
#' @param DT A data.table object.
#' @param value A length-one numeric, character or factor vector indicating which value you want to use to replace \code{NAs} in your data.table.
#' @return The data.table without NAs.
#' @examples
#' ## You want to replace all you NAs by 0
#' 
#' DT <- na_replace(DT,0)
#' @export


na_replace <- function(DT,value) {
  DT[is.na(DT)] <- value
  return(DT)
}