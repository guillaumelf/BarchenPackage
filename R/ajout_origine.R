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