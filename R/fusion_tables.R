#' Merges several tables into one
#'
#' This function will create a single table of results from 3 different tables.
#'
#' @param tab1 A data.table or data.frame object containing the \code{qs.rds} file.
#' @param tab2 A data.table or data.frame object containing the \code{question.rds} file
#' @param tab3 A data.table or data.frame object containing the \code{attempts.rds} file
#' @return The final version of the table relating attempts and scores.
#' @examples
#' qr <- create_qr(qs,question,attempts)
#' @export

create_qr <- function(tab1,tab2,tab3){
  
  tab1 <- data.table::data.table(tab1)
  tab2 <- data.table::data.table(tab2)
  tab3 <- data.table::data.table(tab3)
  
  tab1[, c("c1","c2") := tstrsplit(answer, "-", fixed=TRUE)]
  
  # On va maintenant couper sur le tiret pour récupérer l'identifiant de la question, et aussi le type de question
  # => cette info nous sert à savoir si le candidat faisait un examen blanc ou un entrainement.
  
  library(stringr)
  tab1[str_length(c1)== 10 | str_length(c1)== 21, joinid := str_sub(c1, str_length(c1)-3, str_length(c1))]
  tab1[str_length(c1)== 11 | str_length(c1)== 22, joinid := str_sub(c1, str_length(c1)-4, str_length(c1))]
  tab1[str_length(c1)== 10 | str_length(c1)== 21, typeQuest := str_sub(c1,0, str_length(c1)-4)]
  tab1[str_length(c1)== 11 | str_length(c1)== 22, typeQuest := str_sub(c1,0, str_length(c1)-5)]
  tab1[, exam := ifelse(typeQuest == "random","exam blanc","training")]
  tab1[, c2 := NULL]
  
  # on obtient bien notre identifiant (joinid) pour faire la jointure
  
  tab1$joinid <- as.character(tab1$joinid)
  setkey(tab1,joinid)
  tab2$id <- as.character(tab2$id)
  setkey(tab2,id)
  tab1 <- tab1[tab2, nomatch=0]
  
  # Deuxième jointure :
  # JOIN mdl_userquiz_attempts mqa ON mqa.uniqueid = qs.attempt
  
  setkey(tab1,attempt)
  setkey(tab3,uniqueid)
  tab <- tab1[tab3, nomatch=0]
  
  # Filtrage sur les lignes
  
  tab <- tab[seq_number == 2]
  
  # Création de la variable niveau
  # CASE q.defaultgrade WHEN '1000.000' THEN 'C' ELSE 'A' END AS niveau
  
  tab[,niveau := ifelse(defaultgrade == 1000,"C","A")]
  tab$niveau <- as.factor(tab$niveau)
  
  # Sélection des colonnes
  
  
  qr <- tab[,.(grade, timestamp, userid, niveau, name, attempt, exam )]
  qr[,c("idate","time") := tstrsplit(timestamp, " ", fixed=TRUE)][, time := NULL]
  qr[, idate := as.IDate(idate)]
  return(qr)
}