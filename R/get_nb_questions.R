#' Adds the number of questions made throughout the period
#'
#' This function will calculate the number of questions made by each individual in each phase.
#'
#' @param table A data.table object containing the scores.
#' @param phases A number indicating how many phases you have chosen to split the analysis into.
#' @return A data.table object with the results for each indivdual, across the number of phases you have chosen.
#' @examples
#' tab <- progression_score(table,5)
#' @export

get_nb_questions <- function(table,phases){
  liste_phases <- c()
  for(i in 1:phases){
    liste_phases <- c(liste_phases,paste0("t",i))
  }
  tab1 <- table[, .(userid)]
  table <- as.data.frame(table)
  for(i in 1:phases){
    exp1 <- paste0("nb_training_attempt_",liste_phases[i])
    exp2 <- paste0("nb_moy_questions_A_",liste_phases[i])
    exp3 <- paste0("nb_moy_questions_C_",liste_phases[i])
    decision <- data.table(variable = names(table),
                           presence = (stringr::str_detect(names(table),exp1) | stringr::str_detect(names(table),exp2) | stringr::str_detect(names(table),exp3) ) )
    retenues <- decision[presence == TRUE]$variable
    tab <- table[,retenues]
    tab <- as.data.table(tab)
    tab[, paste0("nb_total_questions_",liste_phases[i]) := list(get(exp1)*(get(exp2)+get(exp3)))]
    tab <- tab[, .(get(paste0("nb_total_questions_",liste_phases[i])))]
    names(tab) <- paste0("nb_total_questions_",liste_phases[i])
    tab1 <- data.table(tab1,tab)
  }
  userid <- tab1$userid
  tab1[, userid := NULL]
  tab1 <- na_replace(tab1,0)
  nb_total_questions <- apply(tab1, MARGIN = 1, FUN = sum)
  tab1 <- data.table(userid,tab1,nb_total_questions)
  return(tab1)
}