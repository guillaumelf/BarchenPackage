#' Adds informations about the MOOC
#'
#' This function will calculate the number of MOOC chapters used and prepared, during each phase for each individual.
#'
#' @param list_tab A list of the different logs tables.
#' @return A data.table object with the results for each indivdual, across the number of phases you have chosen.
#' @examples
#' tab <- gap_session(liste_log)
#' @export

get_mooc_infos <- function(list_tab){
  
  tranches <- length(list_tab)
  liste_id <- c()
  for(i in 1:tranches){
    assign(paste0("log",i), list_tab[[i]])
    liste_id <- c(liste_id, get(paste0("log",i))$userid)
  }
  
  id <- data.table(userid = liste_id)
  
  # On commence par ne sélectionner que les lignes qui nous intéressent
  
  listeMooc <- as.character(c(seq(1431,1435,by=1),1445,seq(1574,1579,by=1)))
  for (i in 1:tranches){
    assign(paste0("tab",i),get(paste0("log", i))[course %in% c(listeMooc,2)])
  }
  
  # Il s'agit maintenant de repérer pour chaque individu de cette table, et pour chaque chapitre consulté, la première fois où il a 
  # lancé la vidéo.
  
  for (i in 1:tranches){
    assign(paste0("infos",i),get(paste0("tab", i))[module == "scorm", .(first_attempt_scorm = min(time)), by = .(userid,course)])
  }
  
  # On sélectionne les colonnes et on ordonne
  
  for (i in 1:tranches){
    assign(paste0("tab",i),get(paste0("tab", i))[, .(userid,time,course,module,idate)])
    assign(paste0("tab",i),get(paste0("tab", i))[order(userid,time)])
  }
  
  # On enlève les utilisateurs qui ont accédé à la base de questions mais pas au MOOC
  
  for (i in 1:tranches){
    assign("indMooc",get(paste0("tab", i))[, mooc := ifelse(course %in% listeMooc,1,0)][, .(mooc_used = sum(mooc)), by = userid][mooc_used > 0]$userid)
    assign(paste0("tab",i),get(paste0("tab", i))[userid %in% indMooc])
    get(paste0("tab", i))[, mooc := NULL]
  }
  
  for (i in 1:tranches){
    assign(paste0("infosbis",i),get(paste0("tab", i))[module == "course" & course %in% listeMooc, .(first_attempt_course = min(time)), by = .(userid,course)])
    assign(paste0("infosbis",i),get(paste0("infosbis", i))[order(userid,course)])
  }
  
  # On va faire un LEFT JOIN sur infos 2 puisque les individus ont parfois ouvert des cours mais pas la vidéo
  
  for (i in 1:tranches){
    setkey(get(paste0("infos", i)),userid,course)
    setkey(get(paste0("infosbis", i)),userid,course)
    assign(paste0("infosbis",i),merge(get(paste0("infosbis", i)),get(paste0("infos", i)),all.x = TRUE))
  }
  
  # A partir de là on peut déjà savoir qui a vraiment utilisé le MOOC, dans le sens où on ne va compter un chapitre comme étant vraiment
  # utilisé qu'à partir du moment où il y a un un "course" et un "scorm" sur ce chapitre. On va recréer la variable "nb_mooc_used" à 
  # partir de la table "infos2"
  
  for (i in 1:tranches){
    assign(paste0("use_mooc", i),na.omit(get(paste0("infosbis", i))))
    assign(paste0("use_mooc", i),get(paste0("use_mooc", i))[, .N, by = userid][, .(userid,nb_mooc_used = N)][, N := NULL])
    l <- length(names(get(paste0("use_mooc", i))))
    new_names <- c("userid",paste0("nb_mooc_used_t",i))
    noms <- names(get(paste0("use_mooc", i)))
    for (j in 1:l){
      assign(paste0("use_mooc", i),setnames(get(paste0("use_mooc", i)),noms[j],new_names[j]) )
    }
  }
  
  # On calcul à partir de la table "infosbis" l'écart temporel entre le scorm et le course, en secondes
  
  for (i in 1:tranches){
    get(paste0("infosbis", i))[, clic_delay := difftime(first_attempt_scorm,first_attempt_course,units = "secs")]
    get(paste0("infosbis", i))[, c("clic_delay") := list(as.numeric(clic_delay))]
  }
  
  # La partie délicate consiste désormais à différencier les cas. Soit on a ceux qui ont un écart très faible => il va falloir quantifier
  # cette notion de faiblesse. On va fixer le seuil minimal de préparation à 10 minutes. Egalement on a des individus avec des ecarts
  # énormes, de plusieurs mois. On va donc également fixer un seuil maximum : au-delà de trois mois on rejette. 
  
  seuil1 <- 60*10
  seuil2 <- 3600*24*30*3
  
  # Si l'écart se situe entre les deux seuils on va attribuer 1 à la variable "mooc_prepared", 0 sinon.
  
  for (i in 1:tranches){
    get(paste0("infosbis", i))[, mooc_prepared := ifelse(clic_delay >= seuil1 & clic_delay <= seuil2, 1, 0)]
  }
  
  # On remplace les valeurs manquantes par 0
  
  for (i in 1:tranches){
    get(paste0("infosbis", i))[is.na(mooc_prepared),mooc_prepared := 0]
  }
  
  # On peut maintenant créer la variable "nb_mooc_prepared" et calculer la proportion de MOOC préparés
  
  for (i in 1:tranches){
    assign(paste0("infosbis", i),get(paste0("infosbis", i))[, .(nb_mooc_prepared = sum(mooc_prepared)), by = userid])
    l <- length(names(get(paste0("infosbis", i))))
    new_names <- c("userid",paste0("nb_mooc_prepared_t",i))
    noms <- names(get(paste0("infosbis", i)))
    for (j in 1:l){
      assign(paste0("infosbis", i),setnames(get(paste0("infosbis", i)),noms[j],new_names[j]) )
    }
  }
  
  # On merge
  
  id$userid <- as.character(id$userid)
  setkey(id,userid)
  for (i in 1:tranches){
    get(paste0("infosbis",i))[, userid := as.character(userid)]
    setkey(get(paste0("infosbis", i)),userid)
    assign("id",merge(id,get(paste0("infosbis", i)),all.x = TRUE))
  }
  id[is.na(id)] <- 0
  
  for (i in 1:tranches){
    get(paste0("use_mooc",i))[, userid := as.character(userid)]
    setkey(get(paste0("use_mooc", i)),userid)
    assign("id",merge(id,get(paste0("use_mooc", i)),all.x = TRUE))
  }
  
  id <- na_replace(id,0)
  id <- unique(id)
  decision <- data.table(variable = names(id),
                         presence = (stringr::str_detect(names(id),"used")) )
  retenues <- decision[presence == TRUE]$variable
  id <- as.data.frame(id)
  table1 <- id[, retenues]
  nb_total_mooc_used <- apply(table1, MARGIN = 1,sum)
  decision <- data.table(variable = names(id),
                         presence = (stringr::str_detect(names(id),"prepared")) )
  retenues <- decision[presence == TRUE]$variable
  table2 <- id[, retenues]
  nb_total_mooc_prepared <- apply(table2, MARGIN = 1,sum)
  id <- data.table(id,nb_total_mooc_used,nb_total_mooc_prepared)
  id[, proportion_mooc_prepared := round(nb_total_mooc_prepared/nb_total_mooc_used,2)]
  id <- na_replace(id,0)
  return(id)
}