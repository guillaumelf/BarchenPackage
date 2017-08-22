#' Adds working time during each phase
#'
#' This function will calculate weekly and total work duration (in minutes), during each phase for each individual.
#'
#' @param list_tab A list of the different logs tables.
#' @param table A data.table object which contains the following information : the duration between the inscription and the exam.
#' @return A data.table object with the results for each indivdual, across the number of phases you have chosen.
#' @examples
#' tab <- get_durations(liste_log,res_examens)
#' @export

get_durations <- function(list_tab,table){
  
  tranches <- length(list_tab)
  liste_id <- c()
  for(i in 1:tranches){
    assign(paste0("log",i), list_tab[[i]])
    liste_id <- c(liste_id, get(paste0("log",i))$userid)
  }
  
  id <- data.table(userid = liste_id)
  
  for (i in 1:tranches){
    if(nrow(get(paste0("log",i))) == 0){
      assign(paste0("log",i),data.table(userid = unique(liste_id),
                                            time = as.Date("2017-08-03 13:42:03")))
    }
  }
  
  for (i in 1:tranches){
    assign(paste0("tab_log",i),get(paste0("log",i))[, .(userid,time)])
    assign(paste0("tab_log",i),get(paste0("tab_log",i))[order(userid,time)])
    get(paste0("tab_log",i))[, c("time_lag") := .(shift(time, n = 1, type = "lead")), by = userid]
  }
  
  # On calcule maintenant la différence de temps en secondes pour chaque ligne
  
  for (i in 1:tranches){
    get(paste0("tab_log",i))[, ecart := difftime(time_lag,time,units = "secs")]
  }
  
  for (i in 1:tranches){
    if(nrow(get(paste0("tab_log",i))) == 1){
      get(paste0("tab_log",i))[, ecart := 0]
    }
  }
  
  # On a récupéré les écarts entre actions pour chaque individu, l'enjeu va maintenant être de détecter le nombre de sessions pour
  # chacun, donc de fixer un seuil à partir duquel on considère que l'individu commence une nouvelle session.
  # Certaines vidéos durent 30 minutes, donc il peut y avoir une "période d'incativité" à ce moment là.
  # On va assouplir en fixant le seuil à 40 minutes
  
  seuil <- 60*40
  
  # On introduit une variable intermédiaire "new_session" codée en 0 ou 1 pour détecter le nombre de sessions de chaque individu :
  # on n'aura plus qu'à faire la somme de cette variable par utilisateur
  
  for (i in 1:tranches){
    get(paste0("tab_log",i))[, new_session := ifelse(ecart > seuil , 1, 0)]
    get(paste0("tab_log",i))[is.na(new_session), new_session := 0]
    get(paste0("tab_log",i))[, .(nb_sessions = sum(new_session)), by = userid]
  }
  
  # La variable "new_session" va nous permettre de faire un filtrage sur les lignes de la tables des logs :
  # ce qui nous intéresse ici ce n'est pas d'avoir toutes les lignes, mais plutôt de calculer l'écart entre le min(time) et le max(time)
  # pour chaque session et pour chaque individu. On va faire le filtrage en prenant pour chacun la première ligne de log, puis toutes
  # les lignes qui correspondent au passage à une nouvelle
  
  for (i in 1:tranches){
    assign(paste0("first",i),get(paste0("tab_log",i))[, .(first_log = min(time)), by = userid])
    setkey(get(paste0("first",i)),userid)
    setkey(get(paste0("tab_log",i)),userid)
    assign(paste0("log",i),merge(get(paste0("tab_log",i)),get(paste0("first",i)),by="userid",allow.cartesian = TRUE))
    assign(paste0("log",i),get(paste0("log",i))[order(userid,time)])
  }
  
  # Il n'y a plus qu'à filtrer et calculer les temps de sessions en minutes
  
  for (i in 1:tranches){
    assign(paste0("tab",i),get(paste0("log",i))[time == first_log | new_session == 1])
    assign(paste0("tab",i),get(paste0("tab",i))[, .(userid,time)])
    get(paste0("tab",i))[, c("time_lag") := .(shift(time, n = 1, type = "lead")), by = userid]
    assign(paste0("tab",i),na.omit(get(paste0("tab",i))))
    get(paste0("tab",i))[, duree := difftime(time_lag,time,units = "mins")]
  }
  
  # On supprime les anomalies du type session qui dure plus de 15h
  
  for (i in 1:tranches){
    assign(paste0("tab",i),get(paste0("tab",i))[duree < 15*60])
  }
  
  for (i in 1:tranches){
    get(paste0("tab",i))[,c("duree") := list(as.numeric(duree))]
  }
  
  # On fait la somme
  
  for (i in 1:tranches){
    assign(paste0("res",i),get(paste0("tab",i))[, .(duree_totale_plateforme = round(sum(duree))), by = userid])
    l <- length(names(get(paste0("res", i))))
    new_names <- c("userid",paste0("duree_totale_plateforme_t",i))
    noms <- names(get(paste0("res", i)))
    for (j in 1:l){
      assign(paste0("res", i),setnames(get(paste0("res", i)),noms[j],new_names[j]) )
    }
  }
  
  # Puis on calcule le temps passé par semaine.
  
  periode <- table[, .(userid,delta)]
  periode[, jours := delta/tranches]
  periode[, jours := round(jours)]
  periode[, delta := NULL]
  periode$userid <- as.character(periode$userid)
  setkey(periode,userid)

  for (i in 1:tranches){
    get(paste0("res",i))[, userid := as.character(userid)]
    setkey(get(paste0("res", i)),userid)
    assign(paste0("res", i), get(paste0("res", i))[periode,nomatch=0])
  }

  for (i in 1:tranches){
    get(paste0("res",i))[, duree_par_semaine := round((get(paste0("duree_totale_plateforme_t",i))*7/jours),1)]
    l <- length(names(get(paste0("res", i))))
    new_names <- c("userid",paste0("duree_totale_plateforme_t",i),"jours",paste0("duree_par_semaine_t",i))
    noms <- names(get(paste0("res", i)))
    for (j in 1:l){
      assign(paste0("res", i),setnames(get(paste0("res", i)),noms[j],new_names[j]) )
    }
    get(paste0("res", i))[, jours := NULL]
  }

  # On merge

  id$userid <- as.character(id$userid)
  setkey(id,userid)
  for (i in 1:tranches){
    get(paste0("res",i))[, userid := as.character(userid)]
    setkey(get(paste0("res", i)),userid)
    assign(paste0("res", i),unique(get(paste0("res", i))))
    assign("id",merge(id,get(paste0("res", i)),all.x = TRUE))
  }
  id <- na_replace(id,0)
  id <- unique(id)
  return(id)
}