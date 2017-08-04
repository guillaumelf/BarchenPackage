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

#' Adds the origin of an individual
#'
#' This function will determine whether an individual comes from a retail banking entity or not and will assign a label as a result. It will also delete automatically the administrator users.
#'
#' @param tab1 A data.table or data.frame object from which the email address comes from.
#' @param tab2 A data.table or data.frame object to which you want to add the information.
#' @return The second argument is returned with the new information, as a data.table object.
#' @examples
#' ## The email is in the users table. You want to add it to the logs table
#' logs <- add_origin(users,logs)
#' @export

add_origin <- function(tab1,tab2){
  # si les tables rentrées en argument ne son pas des objets data.table on les transforme

  tab1 <- data.table::data.table(tab1)
  tab2 <- data.table::data.table(tab2)

  tab1[, c("c1","origine") := tstrsplit(email, "@", fixed=TRUE)]
  tab1[, c1 := NULL]
  tab1[, c("o1","o2") := tstrsplit(tolower(origine), ".fr", fixed=TRUE)]
  tab1[, o2 := NULL]
  tab1[, origine := NULL]
  tab1[, c("o2","o3") := tstrsplit(o1, ".com", fixed=TRUE)]
  tab1[, c("o1","o3") := NULL]
  tab1[, c("o1","o3") := tstrsplit(o2, ".org", fixed=TRUE)]
  tab1[, c("o2","o1") := NULL]
  tab1[, c("o2","o1") := tstrsplit(o3, ".net", fixed=TRUE)]
  tab1[, c("o3","o1") := NULL]
  tab1[, c("o3","o1") := tstrsplit(o2, ".lu", fixed=TRUE)]
  tab1[, c("o2","o1") := NULL]
  tab1[, origine := o3]
  tab1[, o3 := NULL]


  tab1 <- tab1[, .(id,origine)]
  setkey(tab2,userid)
  setkey(tab1,id)
  tab2 <- tab2[tab1, nomatch=0]
  origines <- tab2[, .N, by = origine][order(-N)]

  # On crée la liste des banques

  listeBanques <- c("axabanque","banquebcp","bfm","bdf","banquepopulaire","banquewormser","banque-sbe","barclays","bnpparibas","bnpparibaq","bpri","bpvf","bred",
                    "caisse-epargne","ceapc","cic","cmne","cera","crcam","boursorama","creditmutuel","cmb","cmcic","creditfoncier",
                    "credit-cooperatif","creditmaritime","ecureil-multicanal","groupama","lcl","labanquepostale","lazard",
                    "monabanq","palatine","socgen")

  origines <- origines[order(origine)]

  # Pour chaque suffixe on lui attribue maintenant le label "banque_detail" ou autre
  categorie <- sapply(1:length(origines$origine),
                      FUN = function(i) ifelse(sum(stringr::str_detect(origines$origine[i],listeBanques))>0,"banque_detail","autre"))
  origines <- cbind(origines,categorie)

  # On ne conserve que les variables qui nous intéressent

  origines <- origines[, .(origine, categorie)]

  # On fusionne avec la table des logs

  setkey(tab2,origine)
  setkey(origines,origine)
  tab2 <- tab2[origines, nomatch=0]
  tab2 <- na.omit(tab2)

  # On retourne la table
  return(tab2)
}

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
  return(qr)
}



create_results <- function
