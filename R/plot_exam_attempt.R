#' Plots blank exam informations
#'
#' This function will plot the proportion of individuals having attempted at least one blank exam, for each cluster.
#'
#' @param tab A data.table object containing the clustering data.
#' @param phases A numeric indicating the number of phases. 5 is the default value.
#' @return A bar chart.
#' @examples
#' affiche_examb_att(tab)
#' @export

affiche_examb_att <- function(tab,phases = 5){
  names <- c("cluster")
  for (i in 1:phases){
    names <- c(names,paste0("nb_examblanc_attempt_t",i))
  }
  size <- tab[, .N, by = cluster][order(cluster)]$N
  tab <- data.frame(tab)
  eb <- tab[,names]
  eb <- data.table(eb)
  res <- levels(eb$cluster)
  for (i in 1:phases){
    if(nrow(eb[get(paste0("nb_examblanc_attempt_t",i)) > 0]) == 0){
      res <- data.frame(res,rep(0,length(levels(eb$cluster))))
      names(res)[length(names(res))] <- paste0("V",i)
    } else {
      res <- data.frame(res,round((eb[get(paste0("nb_examblanc_attempt_t",i)) > 0, .N, by = cluster][order(cluster)]$N)/size,2))
      names(res)[length(names(res))] <- paste0("V",i)
    }
  }
  
  noms <- c("cluster")
  for (i in 1:phases){
    noms <- c(noms,paste0("t",i))
  }
  names(res) <- noms
  new_data <- reshape2::melt(res,id.vars = "cluster")
  new_data <- data.table(new_data)
  new_data$type <- rep("x",nrow(new_data))
  
  
  ggplot() +
    geom_bar(data=new_data,
             aes(x = variable, y = value, fill = type),
             width=0.5, colour="grey40", size=0.4, stat = "identity") + 
    scale_fill_discrete(drop=FALSE) +
    labs(x="phases",y="") + ggtitle("Proportion d'individus dans chaque cluster \n ayant au moins 1 tentative à l'examen blanc") +
    theme(plot.title = element_text(hjust = 0.5),legend.position = 'none') + scale_fill_manual(values=c("#F9F900")) +
    facet_wrap(~cluster,nrow=2)
}