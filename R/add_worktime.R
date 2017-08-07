#' Adds the total time spent on the platform
#'
#' This function will calculate the total amount of time each individual has spent working on the platform.
#'
#' @param log A data.table object containing the logs.
#' @return A data.table object with the results wanted.
#' @examples
#' tab <- add_worktime(log)
#' @export

add_worktime <- function(log){
  # Dans un premier temps on va encore utiliser un décalage pour calculer la différence entre deux logs successifs
  # On commence par sélectionner les variables utiles et ordonner la table
  
  log <- log[, .(userid,time)]
  log <- log[order(userid,time)]
  log[, c("time_lag") := .(shift(time, n = 1, type = "lead")), by = userid]
  
  # On calcule maintenant la différence de temps en secondes pour chaque ligne
  
  log[, ecart := difftime(time_lag,time,units = "secs")]
  head(log,10)
  log[is.na(ecart)]
  
  # On a récupéré les écarts entre actions pour chaque individu, l'enjeu va maintenant être de détecter le nombre de sessions pour
  # chacun, donc de fixer un seuil à partir duquel on considère que l'individu commence une nouvelle session.
  # Certaines vidéos durent 30 minutes, donc il peut y avoir une "période d'incativité" à ce moment là.
  # On va assouplir en fixant le seuil à 40 minutes
  
  seuil <- 60*40
  
  # On introduit une variable intermédiaire "new_session" codée en 0 ou 1 pour détecter le nombre de sessions de chaque individu :
  # on n'aura plus qu'à faire la somme de cette variable par utilisateur
  
  log[, new_session := ifelse(ecart > seuil , 1, 0)]
  log[is.na(new_session), new_session := 0]
  log[, .(nb_sessions = sum(new_session)), by = userid]
  
  # La variable "new_session" va nous permettre de faire un filtrage sur les lignes de la tables des logs :
  # ce qui nous intéresse ici ce n'est pas d'avoir toutes les lignes, mais plutôt de calculer l'écart entre le min(time) et le max(time)
  # pour chaque session et pour chaque individu. On va faire le filtrage en prenant pour chacun la première ligne de log, puis toutes
  # les lignes qui correspondent au passage à une nouvelle
  
  first <- log[, .(first_log = min(time)), by = userid]
  first$userid <- as.character(first$userid)
  log$userid <- as.character(log$userid)
  setkey(first,userid)
  setkey(log,userid)
  log <- merge(log,first,by="userid",allow.cartesian = TRUE)
  log <- log[order(userid,time)]
  
  # On peut maintenant filtrer la première ligne, et chaque ligne correspondant à une nouvelle session : on va retenir la ligne se
  # trouvant avant chaque nouvelle session afin d'avoir le min et le max de time pour chaque session.
  
  # Il n'y a plus qu'à filtrer et calculer les temps de sessions en minutes
  
  tab <- log[time == first_log | new_session == 1]
  tab <- tab[, .(userid,time)]
  tab[, c("time_lag") := .(shift(time, n = 1, type = "lead")), by = userid]
  tab <- na.omit(tab)
  tab[, duree := difftime(time_lag,time,units = "mins")]
  
  # On supprime les anomalies du type session qui dure plus de 15h
  
  tab <- tab[duree < 15*60]
  
  # On fait la somme
  
  tab$duree <- as.numeric(tab$duree)
  res <- tab[, .(duree_totale_plateforme = round(sum(duree))), by = userid]
  return(res)
}