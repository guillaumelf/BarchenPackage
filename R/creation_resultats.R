#' Merges results
#'
#' This function merges results coming from different excel files into one.
#'
#' @param succes A data.table object with the successful attempts at the AMF exam.
#' @param echecs A data.table object with the failed attempts at the AMF exam..
#' @param resultats_vi A data.table object with the results of the "Validation Interne" exam.
#' @param users A data.table object which contains informations about the userid and name of an individual.
#' @return A unique dataset including the scores, final result, inscription and exam's date. This is returned as a data.table object.
#' @note It is important to leave spaces in the condition as the string is splitted on this pattern. Look at the next section for examples of valid conditions.
#' @examples
#' ## A quick example of how to call the function :
#' succes <- data.table(read_excel(paste0(path,"certificats.xlsx"), sheet = "Feuil1"))
#' echec <- data.table(read_excel(paste0(path,"envoiEchecs.xlsm"), sheet = "Feuil1", col_names = TRUE, skip = 1))
#' res_vi <- data.table(read_excel(paste0(path,"Extraction Résultats Validation Interne.xlsx")))
#' resultats <- create_results(succes,echecs,resultats_vi)
#' tail(resultats,3)
#' 
#' ## This is was you'll get :
#' ##   userid  %A %C resultat         inscription     examen
#' ##1:  49615  47 43        0 2017-04-14 10:17:56 2017-04-19
#' ##2:  50405  88 88        1 2017-05-18 13:20:29 2011-10-01
#' ##3:  51183 100 91        1 2017-06-15 15:55:02 2014-08-20
#' @note Lines with missing values will be omitted automatically.
#' @export

create_results <- function(succes,echec,resultats_vi,users){
  
  # Il faut retraiter les variables de pourcentage de réussite qui sont en caractères

  echec[, `%A` := stringr::str_replace(`%A`,",",".00")][, `%A` := as.numeric(`%A`)]
  echec[, `%B` := stringr::str_replace(`%B`,",",".00")][, `%C` := as.numeric(`%B`)][, `%B` := NULL]
  
  succes[, `%C` := `%B`][, `%B` := NULL]
  
  # On garde les colonnes qui peuvent nous servir
  
  echec[, `J session` := ifelse(`J session` < 10, paste0("0",`J session`),`J session`) ]
  echec[, `M session` := ifelse(`M session` < 10, paste0("0",`M session`),`M session`) ]
  echec <- echec[, .(tolower(nom),
                     tolower(prenom),
                     as.IDate(paste(`A session`, `M session`, `J session`, sep ="-")),
                     `%A`,
                     `%C`)]
  names(echec) <- c("nom","prenom","date","%A","%C")
  
  succes[, `J session` := as.character(`J session`)][, `J session` := ifelse(stringr::str_length(`J session`) < 2, paste0("0",`J session`),`J session`)]
  succes[, `M session` := as.character(`M session`)][, `M session` := ifelse(stringr::str_length(`M session`) < 2, paste0("0",`M session`),`M session`)]
  succes <- succes[, .(tolower(nom),
                       tolower(prenom),
                       as.IDate(paste(`A session`, `M session`, `J session`, sep ="-")),
                       `%A`,
                       `%C`)]
  names(succes) <- c("nom","prenom","date","%A","%C")
  
  res_vi[, `J-session` := ifelse(`J-session` < 10, paste0("0",`J-session`),`J-session`) ]
  res_vi[, `M-session` := ifelse(`M-session` < 10, paste0("0",`M-session`),`M-session`) ]
  res_vi <- res_vi[, .(tolower(lastname),
                       tolower(firstname),
                       as.IDate(paste(`A-session`, `M-session`, `J-session`, sep ="-")),
                       `%A`,
                       `%C`,
                       `Statut Validation Interne`)]
  names(res_vi) <- c("nom","prenom","date","%A","%C","resultat")
  
  # On rajoute la variable "resultat" pour permettre de différencier les résultats quand on aura fait la fusion des echecs et succès
  
  echec[, resultat := 0]
  succes[, resultat := 1]
  res_vi[, resultat := ifelse(resultat == "Réussite",1,0)]
  
  # On fusionne les tableaux de réussite et d'échec
  
  resultats <- rbind(succes,echec,res_vi)
  
  # Il reste maintenant à joindre le numéro d'identifiant userid à chaque candidat. On va faire la jointure sur la clé nom, prénom
  # On va donc modifier la casse pour éviter les soucis
  
  users <- users[, .(id,tolower(lastname),tolower(firstname),as.POSIXct(firstaccess, origin = "1970-01-01"))]
  names(users) <- c("userid","nom","prenom","inscription")
  
  # On rajoute le numéro d'identifiant
  
  setkey(users,nom,prenom)
  setkey(resultats,nom,prenom)
  resultats <- resultats[users,nomatch=0]
  
  # On ordonne par userid : certains ont plusieurs tentatives forcément, on ne va garder que la première
  
  resultats <- resultats[order(userid)]
  
  # Il y a des lignes qui sont répliquées à l'identique, on va procéder à un premier nettoyage en les éliminant
  
  setkey(resultats,nom,prenom,date)
  resultats <- unique(resultats)
  resultats <- resultats[order(userid)]
  
  # On a des individus qui ont fait 2 tentatives ou plus. On va faire l'extraction en utilisant la variable date et en gardant
  # pour chaque individu la date minimum : on rajoute une variable intermédiaire
  
  resultats[, first_attempt := min(date), by = userid]
  resultats <- resultats[date == first_attempt][, first_attempt := NULL]
  resultats <- na.omit(resultats)
  resultats <- resultats[!(stringr::str_detect(inscription,"1970"))]
  resultats <- resultats[, .(userid,`%A`,`%C`,resultat,inscription,date)]
  names(resultats) <- c("userid","%A","%C","resultat","inscription","examen")
  return(resultats)
}