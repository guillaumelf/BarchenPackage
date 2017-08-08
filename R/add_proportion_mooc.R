#' Adds informations about the MOOC
#'
#' This function will calculate the total proportion of MOOC chapters used for each individual.
#'
#' @param table1 The table containing all the logs.
#' @param table2 The table containing the number of mooc used.
#' @return A data.table object with the results for each indivdual.
#' @examples
#' tab <- add_proportion_mooc(log,tab_mooc)
#' @export

add_proportion_mooc <- function(table1,table2){
  
  last_co <- table1[, .(lastaccess = max(time)), by = userid]
  last_co[, lastaccess := as.IDate(lastaccess)]
  
  # On ajoute les variables "possibilite_acces_mooc" et "nb_mooc_used"
  
  infos <- table2[, .(nb_total_mooc_used,userid)]
  infos$userid <- as.character(infos$userid)
  setkey(infos,userid)
  last_co$userid <- as.character(last_co$userid)
  setkey(last_co,userid)
  last_co <- last_co[infos,nomatch=0]
  
  # On ajoute la variable nb_chap_dispo
  
  s1 <- as.IDate("2016-04-18")
  s2 <- as.IDate("2016-06-29")
  s3 <- as.IDate("2016-11-30")
  s4 <- as.IDate("2017-01-04")
  s5 <- as.IDate("2017-01-15")
  s6 <- as.IDate("2017-03-15")
  
  
  last_co[lastaccess < s1, nb_chap_dispo := 0]
  last_co[lastaccess >= s1 & lastaccess < s2, nb_chap_dispo := 6]
  last_co[lastaccess >= s2 & lastaccess < s3, nb_chap_dispo := 8]
  last_co[lastaccess >= s3 & lastaccess < s4, nb_chap_dispo := 9]
  last_co[lastaccess >= s4 & lastaccess < s5, nb_chap_dispo := 10]
  last_co[lastaccess >= s5 & lastaccess < s6, nb_chap_dispo := 11]
  last_co[lastaccess >= s6, nb_chap_dispo := 12]
  
  
  # Enfin on calcule la proportion d'utilisation pour chaque individu
  
  last_co[, proportion_mooc_used := round((nb_total_mooc_used/nb_chap_dispo),2)]
  last_co <- last_co[, .(userid,proportion_mooc_used)]
  return(last_co)
}

