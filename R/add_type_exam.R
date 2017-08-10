#' Adds informations about the nature of the exam
#'
#' This function will create the variable \code{type_exam_passed} which will be a 3-level factor taking either ".
#'
#' @param list_tabs A list data.table objects with the results of exams.
#' @param users A data.table object with the \code{users.rds} table.
#' @param tab A data.table object to which you want to add the information
#' @return A data.table object with the new variable created.
#' @examples
#' tab <- add_type_exam(list(exams2016,exams2017),users,tab)
#' @export

add_type_exam <- function(list_tabs,users,tab){
  tranches <- length(list_tabs)
  for(i in 1:tranches){
    assign(paste0("tab",i), list_tabs[[i]])
    assign(paste0("tab",i),get(paste0("tab",i))[, .(Examen, nom = tolower(nom), prenom = tolower(prénom))])
    assign(paste0("tab",i),na.omit(get(paste0("tab",i))))
  }
  
  users <- users[, .(prenom = tolower(firstname),
                     nom = tolower(lastname),
                     userid = id)]
  infos_exam <- tab1
  if(tranches >= 2){
    for(i in 2:tranches){
      assign("infos_exam",rbind(infos_exam,get(paste0("tab",i))))
    }
  }
  
  setkey(infos_exam,nom,prenom)
  setkey(users,nom,prenom)
  
  infos <- merge(users, infos_exam, all.x = TRUE)
  infos <- unique(infos)
  infos <- na.omit(infos)
  
  # On joint les infos à nos individus
  
  join <- tab[, .(userid)]
  infos <- infos[, .(userid,type_exam = Examen)]
  
  join$userid <- as.character(join$userid)
  infos$userid <- as.character(infos$userid)
  setkey(join,userid)
  setkey(infos,userid)
  join <- join[infos,nomatch=0]
  join[type_exam == "examen certifié", type_exam := "Examen certifie"]
  join[type_exam == "examen certifie", type_exam := "Examen certifie"]
  join[type_exam == "Examen certifié", type_exam := "Examen certifie"]
  join[type_exam == "double echelle", type_exam := "Double echelle"]
  join[type_exam == "double échelle", type_exam := "Double echelle"]
  join[type_exam == "Double Echelle", type_exam := "Double echelle"]
  join[type_exam == "Double échelle", type_exam := "Double echelle"]
  join[type_exam == "double echelle", type_exam := "Double echelle"]
  
  setkey(tab,userid)
  setkey(join,userid)
  tab <- merge(tab,join,all.x = TRUE)
  tab <- na_replace(tab,"Examen certifie")
  tab <- unique(tab)
  tab$type_exam <- as.factor(tab$type_exam)
  return(tab)
}