#' Adds informations about the initial level
#'
#' This function will calculate the initial level in terms of scores after 100 questions for each individual.
#'
#' @param list_tab A list of tables.
#' @param userid A character indicating the userid you want to create. Default value is \code{NULL}.
#' @return A data.table object with the results for each indivdual.
#' @examples
#' tab <- get_initial_level(liste_qr)
#' @export

get_initial_level <- function(list_tab,userid = NULL){
  tab_qr1 <- list_tab[[1]]
  nb_q <- tab_qr1[, .N, by = userid]
  
  # On ne retient que les individus ayant fait au moins 100 questions au cours de cette phase
  
  ind_retenus <- nb_q[N >= 100]$userid
  tab_qr1 <- tab_qr1[userid %in% ind_retenus]
  tab_qr1[, fictif := 1:nrow(tab_qr1)]
  seuil <- tab_qr1[, .(seuil = min(fictif)+100 ), by = userid]
  
  setkey(seuil,userid)
  setkey(tab_qr1,userid)
  tab_qr1 <- merge(tab_qr1,seuil,by="userid",allow.cartesian = TRUE)
  
  tab_qr1 <- tab_qr1[fictif < seuil]
  
  # On va regarder le taux de réussite après 100 questions, cela nous donnera le score initial
  
  if(nrow(tab_qr1) == 0){
    res <- data.table(userid,niveau_initial_A = 0,niveau_initial_C = 0)
  } else {
    res_A <- tab_qr1[niveau == "A", .(niveau_initial_A = round(mean(grade),2)), by = .(userid)]
    res_C <- tab_qr1[niveau == "C", .(niveau_initial_C = round(mean(grade),2)), by = .(userid)]
    setkey(res_A,userid)
    setkey(res_C,userid)
    res <- merge(res_A,res_C,all.x = TRUE)
  }
  return(res)
  
}