#' Adds scores progressions through the working period
#'
#' This function will calculate score progressions for each level (A and C). The result will be a percentage.
#'
#' @param table A data.table object containing the scores.
#' @param phases A number indicating how many phases you have chosen to split the analysis into.
#' @return A data.table object with the results for each indivdual, across the number of phases you have chosen.
#' @examples
#' tab <- progression_score(table,5)
#' @export

progression_score <- function(table,phases){
  
  liste_phases <- c()
  for(i in 1:phases){
    liste_phases <- c(liste_phases,paste0("t",i))
  }

  tab <- table[,.(niveau_initial_A,niveau_initial_C,score_moy_A_t1,score_moy_C_t1)]
  tab1 <- tab[, c("progression_A_t1",
                  "progression_C_t1") := list(100*(score_moy_A_t1-niveau_initial_A),
                                              100*(score_moy_C_t1-niveau_initial_C))][, .(progression_A_t1,progression_C_t1)]
  table <- as.data.frame(table)
  for(i in 2:phases){
    exp1 <- paste0("score_moy_A_",liste_phases[i])
    exp2 <- paste0("score_moy_A_",liste_phases[i-1])
    exp3 <- paste0("score_moy_C_",liste_phases[i])
    exp4 <- paste0("score_moy_C_",liste_phases[i-1])
    decision <- data.table(variable = names(table),
                           presence = (stringr::str_detect(names(table),exp1) | stringr::str_detect(names(table),exp2) | stringr::str_detect(names(table),exp3) | stringr::str_detect(names(table),exp4) ) )
    retenues <- decision[presence == TRUE]$variable
    tab <- table[,retenues]
    tab <- as.data.table(tab)
    tab[, paste0("progression_A_",liste_phases[i]) := list(100*(get(exp1)-get(exp2)))]
    tab[, paste0("progression_C_",liste_phases[i]) := list(100*(get(exp3)-get(exp4)))]
    tab <- tab[, .(get(paste0("progression_A_",liste_phases[i])),get(paste0("progression_C_",liste_phases[i])))]
    names(tab) <- c(paste0("progression_A_",liste_phases[i]),paste0("progression_C_",liste_phases[i]))
    tab1 <- data.table(tab1,tab)
  }
  tab1 <- na_replace(tab1,0)
  return(tab1)
}
