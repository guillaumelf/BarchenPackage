#' Extracts a table
#'
#' Performs SQL query to extract a table from a database and can select the lines by using a condition (optional).
#'
#' @param connection An object that inherits from the \code{dbConnect()} function.
#' @param table A string indicating which table you want to extract the data from.
#' @param selection A string indicating the condition you want to use to select lines in the table (optional). \code{NULL} is the default value, if nothing is specified the entire table is returned.
#' @return The dataset which was the subject of the extraction, is returned as a data.table object.
#' @note It is important to leave spaces in the condition as the string is splitted on this pattern. Look at the next section for examples of valid conditions.
#' @examples
#' ## Say for example you want to track the activities from the
#' ## Datastorm user in the log table this is how you are going to proceed :
#'
#' mydb <- DBI::dbConnect(MySQL(), user=username, password=mdp,dbname=nom_base, host=port_hote)
#' condition <- "userid = 46501"
#' log_datastorm <- extract_table_by_userid(mydb,"mdl_log",condition)
#'
#' ## Now you want to extract the logs
#' ## since 01/08/2017, this is the code you must run :
#'
#' condition <- "time >= '2017-08-01'"
#' logs2015 <- extract_table(mydb,"mdl_log",condition)
#'
#' ## Or you just want every log from the start of the period :
#'
#' logs <- extract_table(mydb,"mdl_log")
#' @export
extract_table <- function(connection,table,selection=NULL){
  if(is.null(selection)){
    statement <- paste("SELECT * from",table)
  } else {
    condition <- stringr::str_split(selection," ")
    argument <- as.character(condition[[1]][1])
    symbol <- as.character(condition[[1]][2])
    value <- as.character(condition[[1]][3])
    if(stringr::str_detect(value,"-")) {
      statement <- paste0("SELECT * from ",table," WHERE ",argument," ",symbol," UNIX_TIMESTAMP(",value,")")
    } else {
      statement <- paste("SELECT * from",table,"WHERE",argument,symbol,value)
    }
  }
  tab <- data.table::data.table(DBI::dbGetQuery(connection,statement))
  return(tab)
}
