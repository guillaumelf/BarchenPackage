#' Plots training informations
#'
#' This function will plot the number of training attempts in each phase, for each cluster.
#'
#' @param tab A data.table object containing the clustering data.
#' @param phases A numeric indicating the number of phases. 5 is the default value.
#' @return A bar chart.
#' @examples
#' affiche_tr_attempt(tab)
#' @export

affiche_tr_attempt <- function(tab,phases = 5){
  names <- c("cluster")
  for (i in 1:phases){
    names <- c(names,paste0("nb_training_attempt_t",i))
  }
  tab <- data.frame(tab)
  training <- tab[,names]
  training <- data.table(training)
  res <- training[, lapply(.SD, mean), by=cluster][order(cluster)]
  res <- data.frame(res)
  noms <- c("cluster")
  for (i in 1:phases){
    noms <- c(noms,paste0("t",i))
  }
  names(res) <- noms
  new_data <- reshape2::melt(res,id.vars = "cluster")
  new_data$fictif <- rep("Nombre de tentatives",nrow(new_data))
  
  ggplot() +
    geom_bar(data=new_data,
             aes(x = variable, y = value, fill = fictif),
             width=0.5, colour="grey40", size=0.4, stat = "identity") + 
    scale_fill_discrete(drop=FALSE) +
    labs(x="phases",y="") + ggtitle("Evolution au cours du temps du nombre \n moyen de tentatives effectuées par session") +
    theme(plot.title = element_text(hjust = 0.5),legend.title = element_blank()) + scale_fill_manual(values=c("#F97400")) +
    facet_wrap(~cluster,nrow=2)
}