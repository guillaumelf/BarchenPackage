#' Performs a Principal Component Analysis
#'
#' This function will perform a PCA on your data.
#'
#' @param tab A data.table object containing the data you want to analyse.
#' @param inertie_percentage A numeric between 0 and 100 indicating the percentage of variance you want to keep. Default value is 95.
#' @return A list with the PCA results.
#' @examples
#' ## You want to keep only 85% of variance :
#' 
#' res_pca <- do_pca(tab,inertie_percentage = 85)
#' @export

do_pca <- function(tab,inertie_percentage = 95){
  res_pca <- FactoMineR::PCA(tab, scale.unit = TRUE)
  inertie <- data.table(inertie = res_pca$eig$`cumulative percentage of variance`)
  inertie[, fictif := ifelse(inertie < inertie_percentage, 1,0)]
  inertie <- inertie[fictif == 1]
  res_pca <- FactoMineR::PCA(tab, scale.unit = TRUE, ncp = nrow(inertie))
  return(res_pca)
}

#' Removes outliers from a PCA
#'
#' This function will remove outliers from your analysis.
#'
#' @param tab A data.table object containing the data you want to analyse.
#' @param results A list with the results of a PCA.
#' @param min.inertie A numeric between 0 and 100 indicating the minimum iniertia an axis must have to be taken into consideration. Default value is 5
#' @param ctr.min A numeric indicating the CTR threshold above which you consider that an individual is an outlier. Default value is 2.
#' @return A data.table object without outliers.
#' @examples
#' ## You want to remove all the indivuals that have a CTR > 1.90 on all the 
#' ## axis which represent at least 4% of variance :
#' 
#' tab_clean <- remove_outliers(tab,res_pca,min.inertie = 4, ctr.min = 1.90)
#' @export

remove_outliers <- function(tab,results,min.inertie = 5,ctr.min = 2){
  
  inertie <- data.table(inertie = res_pca$eig$`percentage of variance`)
  inertie[, fictif := ifelse(inertie >= min.inertie, 1,0)]
  inertie <- inertie[fictif == 1]
  
  liste_outliers <- c()
  for(i in 1:nrow(inertie)){
    axe_ind <- data.frame(CTR = res_pca$ind$contrib[,i], COS2 = res_pca$ind$cos2[,i], coord = res_pca$ind$coord[,i])
    
    # On ne garde que les individus dont la contribution est > à 1/n
    
    axe_ind <- data.table(axe_ind, keep.rownames=TRUE)
    axe_ind[, seuil := 1/nrow(axe_ind)]
    
    # Pour l'interprétation on classe les individus par CTR décroissante
    
    axe_ind <- axe_ind[CTR > seuil][order(-CTR)]
    liste_outliers <- c(liste_outliers,axe_ind[CTR > ctr.min]$rn)
  }
  
  liste_outliers <- unique(liste_outliers)
  tab <- tab[!(rownames(tab) %in% liste_outliers),]
  return(tab)
  
}












