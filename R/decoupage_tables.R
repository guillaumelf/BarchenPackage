#' Divides a table into several parts
#'
#' This function will divide a table into as many subsets as you want. The divisions will be made in a chronological order.
#'
#' @param tab1 A data.table object containing the table you want to divide.
#' @param tab2 A data.table  object containing the inscription date.
#' @param divisions A number which indicates how many parts you want to divide your tables into.
#' @return A list of subsets from the main table.
#' @examples
#' ## You want to divide the logs  into 5 parts
#' 
#' liste_logs <- divide_tables(log,res_examen,5)
#' @export

divide_tables <- function(tab1,tab2,divisions){
  
  tranches <- divisions
  
  # Deuxième étape : pour les individus sur lesquels on travaille on va récupérer leur delta
  
  periode <- tab2[, .(userid,delta)]
  
  # On va donc calculer des tranches proportionnelles au delta pour chaque individu. Le calcul va nous donner un nombre de jours
  # pour chaque individu
  
  periode[, jours := delta/tranches]
  periode[, jours := round(jours)]
  
  # On sélectionne seulement les individus qui ont un résultat à l'examen
  
  log <- tab1[userid %in% tab2$userid]
  
  # On rajoute la variable jours dans la table des logs et des questions 
  
  periode[, delta := NULL]
  log$userid <- as.character(log$userid)
  periode$userid <- as.character(periode$userid)
  setkey(log,userid)
  setkey(periode,userid)
  log <- merge(log,periode,by="userid",allow.cartesian = TRUE)
  firstlog <- tab2[, .(userid,inscription)][, first_log := inscription][, inscription := NULL]
  firstlog$userid <- as.character(firstlog$userid)
  
  # On rajoute les infos pour faire le découpage : on va créer x variables (d1,d2,...,dx) qui vont nous permettre de délimiter les 
  # périodes pour chaque individu.
  
  setkey(periode,userid)
  setkey(firstlog,userid)
  firstlog <- firstlog[periode,nomatch=0]
  for (i in 1:tranches){
    firstlog[, paste0("d",i) := as.IDate(first_log + i*jours)]
  }
  
  # On ne garde que les variables qui servent à la fusion avec la table des logs et à faire le découpage
  
  firstlog[, jours := NULL]
  setkey(log,userid)
  setkey(firstlog,userid)
  log <- merge(log,firstlog,by="userid",allow.cartesian = TRUE)
  
  # On ajoute maintenant la variable phase : t1, t2, ..., tx
  
  log[idate >= first_log & idate <= d1, phase := "t1"]
  for(i in 2:tranches){
    log[idate > get(paste0("d",i-1)) & idate <= get(paste0("d",i)), phase := paste0("t",i)]
  }
  log <- na.omit(log)
  #return(log)
  liste <- list()
  for (i in 1:tranches){
    assign(paste0("log", i), log[phase == paste0("t",i)])
    liste[[i]] <- get(paste0("log", i))
  }
  return(liste)
}

