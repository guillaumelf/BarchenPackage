#' Calculates euclidean distance
#'
#' This function will return the euclidean distance between an individual and others .
#'
#' @param tab A data.table object containing the clustering data.
#' @param ind A character indicating the userid number of the individual you want to compare to others.
#' @param infos A data.table object containing the clusters and results at the exam.
#' @param k A numeric indicating how much nearest neighbours you want to keep.
#' @return A dataset with the k-nearest neighbours.
#' @examples
#' get_distance(tab)
#' @export

get_distance <- function(tab, infos, ind, k){
  tab <- scale(tab, center = FALSE, scale = TRUE)
  tab <- data.table(tab)
  tab[is.na(tab)] <- 0
  tab[, c("delta",
          "duree_par_semaine_t1",
          "duree_par_semaine_t2",
          "duree_par_semaine_t3",
          "nb_total_questions_t1",
          "nb_total_questions_t2",
          "nb_total_questions_t3",
          "nb_mooc_prepared_t1",
          "nb_mooc_prepared_t2",
          "nb_mooc_prepared_t3",
          "nb_mooc_used_t1",
          "nb_mooc_used_t2",
          "nb_mooc_used_t3",
          "nb_examblanc_attempt_t3") := list(100*delta,
                                             100*duree_par_semaine_t1,
                                             100*duree_par_semaine_t2,
                                             100*duree_par_semaine_t3,
                                             100*nb_total_questions_t1,
                                             100*nb_total_questions_t2,
                                             100*nb_total_questions_t3,
                                             100*nb_mooc_prepared_t1,
                                             100*nb_mooc_prepared_t2,
                                             100*nb_mooc_prepared_t3,
                                             100*nb_mooc_used_t1,
                                             100*nb_mooc_used_t2,
                                             100*nb_mooc_used_t3,
                                             100*nb_examblanc_attempt_t3)]
  
  noms <- as.character(c(1:(nrow(tab)-1),ind))
  
  mat_dist <- as.matrix(dist(tab,method="euclidean", diag = FALSE, upper = FALSE))
  mat_dist <- as.data.frame(mat_dist)
  names(mat_dist) <- noms
  
  test <- data.table(userid = rownames(mat_dist),distances = mat_dist[,ind])
  test <- test[distances != 0]
  setkey(test,userid)
  setkey(infos,userid)
  test <- test[infos,nomatch=0]
  test <- test[order(distances)]
  
  # On ne conserve que les 20 premières lignes puis on fait un vote à la majorité
  
  test <- test[1:k]
  return(test)
}