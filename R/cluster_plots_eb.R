#' Plots blank exam informations
#'
#' This function will plot the proportion of individuals having succeeded at the blank exam among those who hve attempted at least one, for each cluster.
#'
#' @param tab A data.table object containing the clustering data.
#' @return A bar chart.
#' @examples
#' affiche_examb_success(tab)
#' @export

affiche_examb_success <- function(tab){
  names <- c("cluster")
  for (i in 3:5){
    names <- c(names,paste0("score_moy_examblanc_A_t",i),paste0("score_moy_examblanc_C_t",i))
  }
  tab <- data.frame(tab)
  eb <- tab[,names]
  eb <- data.table(eb)
  eb[, c("reussite_eb_t3",
         "reussite_eb_t4",
         "reussite_eb_t5") := list(ifelse(score_moy_examblanc_A_t3 >= 0.85 & score_moy_examblanc_C_t3 >= 0.75,
                                          1, 0),
                                   ifelse(score_moy_examblanc_A_t4 >= 0.85 & score_moy_examblanc_C_t4 >= 0.75,
                                          1, 0),
                                   ifelse(score_moy_examblanc_A_t5 >= 0.85 & score_moy_examblanc_C_t5 >= 0.75,
                                          1, 0))]
  res <- eb[, .(mean(reussite_eb_t3, na.rm = T),
                mean(reussite_eb_t4, na.rm = T),
                mean(reussite_eb_t5, na.rm = T)), by = cluster][order(cluster)]
  
  noms <- c("cluster")
  for (i in 3:5){
    noms <- c(noms,paste0("t",i))
  }
  names(res) <- noms
  new_data <- reshape2::melt(res,id.vars = "cluster")
  new_data <- data.table(new_data)
  new_data[, color := ifelse(value >= 0.5,"oui","non")]
  
  
  ggplot() +
    geom_bar(data=new_data,
             aes(x = variable, y = value, fill = color),
             width=0.5, colour="grey40", size=0.4, stat = "identity") + 
    scale_fill_discrete(drop=FALSE) +
    labs(x="phases",y="") + ggtitle("Proportion d'individus ayant réussi à l'examen blanc") +
    theme(plot.title = element_text(hjust = 0.5),legend.position = 'none') + scale_fill_manual(values=c("#FB1100","#00FB00")) +
    facet_wrap(~cluster,nrow=2)
}