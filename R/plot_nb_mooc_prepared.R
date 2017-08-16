#' Plots MOOC informations
#'
#' This function will plot the average number of MOOC chapters prepared during each phase, for each cluster.
#'
#' @param tab A data.table object containing the clustering data.
#' @param phases A numeric indicating the number of phases. 5 is the default value.
#' @return A bar chart.
#' @examples
#' affiche_nb_mooc_prepared(tab)
#' @export

affiche_nb_mooc_prepared <- function(tab,phases = 5){
  names <- c("cluster")
  for (i in 1:phases){
    names <- c(names,paste0("nb_mooc_prepared_t",i))
  }
  noms <- c("cluster")
  for (i in 1:phases){
    noms <- c(noms,paste0("t",i))
  }
  tab <- data.frame(tab)
  res <- tab[,names]
  res <- data.table(res)
  res <- res[, lapply(.SD, mean), by=cluster][order(cluster)]
  names(res) <- noms
  new_data <- reshape2::melt(res,id.vars = "cluster")
  new_data <- data.table(new_data)
  new_data$type <- rep("x",nrow(new_data))
  
  
  ggplot() +
    geom_bar(data=new_data,
             aes(x = variable, y = value, fill = type),
             width=0.5, colour="grey40", size=0.4, stat = "identity") + 
    scale_fill_discrete(drop=FALSE) +
    labs(x="phases",y="") + ggtitle("Evolution au cours du temps du \n nombre moyen de chapitres du MOOC préparés") +
    theme(plot.title = element_text(hjust = 0.5),legend.position = 'none') + scale_fill_manual(values=c("#FAF216")) +
    facet_wrap(~cluster,nrow=2)
}