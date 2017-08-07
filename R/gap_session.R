#' Adds gaps between sessions
#'
#' This function will calculate gaps between sessions (in hours) for each individual.
#'
#' @param list_tab A list of the different logs tables.
#' @return A data.table object with the gaps between sessions for each indivdual, across the number of phases you have chosen.
#' @examples
#' tab <- gap_session(liste_log,divisions)
#' @export

gap_session <- function(list_tab){
  tranches <- length(list_tab)
  liste_id <- c()
  for(i in 1:tranches){
    assign(paste0("log",i), list_tab[[i]])
    liste_id <- c(liste_id, get(paste0("log",i))$userid)
  }
  
  id <- data.table(userid = liste_id)
  
  for (i in 1:tranches){
    assign(paste0("session",i), get(paste0("log",i))[action == "login" | action == "logout"])
  }
  
  # On garde les variables qui vont nous servir : userid, time, action et idate
  
  for (i in 1:tranches){
    assign(paste0("session",i), get(paste0("session",i))[, .(userid, time, action, idate)])
  }
  
  # On crée maintenant les décalages
  
  for (i in 1:tranches){
    get(paste0("session",i))[, c("action_lag",
                                 "date_lag",
                                 "time_lag") := .(shift(action, n = 1, type = "lag"),
                                                  shift(idate, n = 1, type = "lag"),
                                                  shift(time, n = 1, type = "lag")), by = userid]
  }
  
  # On a bien des NA à chaque fois qu'on passe à un nouvel utilisateur, on va donc retirer cette première ligne à chaque fois
  # Il faut filtrer 2 cas : 
  # - Soit l'apprenant s'est connecté (login), a travaillé et s'est déconnecté "proprement" (logout)
  # - Soit l'apprenant s'est connecté (login) mais n'a pas fermé sa session (il a cliqué sur la croix)
  # => on va donc faire les calculs sur les lignes où on n'a pas deux fois login avec la même date
  
  for (i in 1:tranches){
    assign(paste0("session",i), get(paste0("session",i))[!(idate == date_lag & action == action_lag)])
  }
  
  # On ne cherche pas à calculer le temps d'une session ici, on ne va donc pas s'intéresser aux lignes où :
  # action == logout & action_lag == login qui représente une session
  
  for (i in 1:tranches){
    assign(paste0("session",i), get(paste0("session",i))[!(action == "logout" & action_lag == "login")])
  }
  head(session1,15)
  
  # Il ne reste plus qu'à calculer l'écart entre la colonne time et time_lag
  
  for (i in 1:tranches){
    get(paste0("session",i))[, delta_connexion := difftime(time,time_lag,units = "hours")]
    get(paste0("session",i))[, delta_connexion := as.numeric(delta_connexion)]
    get(paste0("session",i))[, delta_connexion := round(delta_connexion,2)]
  }
  
  # Maintenant on peut calculer la moyenne
  
  for (i in 1:tranches){
    assign(paste0("session",i),
           get(paste0("session",i))[, .(mean(delta_connexion)), by = userid])
    l <- length(names(get(paste0("session", i))))
    new_names <- c("userid",paste0("ecart_moy_session_t",i))
    noms <- names(get(paste0("session", i)))
    for (j in 1:l){
      assign(paste0("session", i),setnames(get(paste0("session", i)),noms[j],new_names[j]) )
    }
    
    get(paste0("session",i))[, c(paste0("ecart_moy_session_t",i)) := list(round(get(paste0("ecart_moy_session_t",i)),3))]
  }
  
  setkey(id,userid)
  for (i in 1:tranches){
    setkey(get(paste0("session", i)),userid)
    assign("id", merge(id,get(paste0("session", i)), all.x = TRUE))
  }
  
  id <- na_replace(id,0)
  id <- unique(id)
  return(id)
}