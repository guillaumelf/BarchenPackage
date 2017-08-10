#' Adds informations about the formation
#'
#' This function will create the variable \code{formation_presentielle} which will take either 1 or 0 for value, indicating whether each individual has been involved or not in a 2-day formation.
#'
#' @param tab1 A data.table object with the \code{users.rds} table.
#' @param tab2 A data.table object with the \code{Stats formation.xlsx} table.
#' @param tab3 A data.table object to which you want to add this information.
#' @return A data.table object with the new variable created.
#' @examples
#' tab <- add_fp(users,infos_fp,tab)
#' @export

add_fp <- function(tab1,tab2,tab3){
  users <- tab1[, .(firstname = tolower(firstname),
                     lastname = tolower(lastname),
                     email = tolower(email),
                     userid = id)]

  infos_fp <- tab2[, .(email = tolower(`@`))]
  infos_fp[, formation_presentielle := 1]
  
  # On fusionne
  
  setkey(infos_fp,email)
  setkey(users,email)
  users <- merge(users,infos_fp,all.x = T)
  
  # On fusionne avec la matrice de travail et on remplace les valeurs manquantes par 0
  
  join <- tab3[, .(userid)]
  users <- users[, .(userid,formation_presentielle)]
  
  join$userid <- as.character(join$userid)
  users$userid <- as.character(users$userid)
  setkey(join,userid)
  setkey(users,userid)
  join <- join[users,nomatch=0]
  
  join <- unique(join)
  join[is.na(join)] <- 0
  
  tab3$userid <- as.character(tab3$userid)
  setkey(tab3,userid)
  donnees <- tab3[join,nomatch=0]
  return(donnees)
}