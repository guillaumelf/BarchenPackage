#' Plots training informations
#'
#' This function will plot the total number of questions attempted in each phase, for each cluster.
#'
#' @param tab A data.table object containing the clustering data.
#' @param phases A numeric indicating the number of phases. 5 is the default value.
#' @return A bar chart.
#' @examples
#' affiche_tr_quest_tot(tab)
#' @export

affiche_tr_quest_tot <- function(tab,phases = 5){
  names <- c("cluster")
  for (i in 1:phases){
    names <- c(names,paste0("nb_total_questions_t",i))
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
  new_data <- data.table(new_data)
  new_data$type <- rep("Nombre total de questions",nrow(new_data))
  
  
  ggplot() +
    geom_bar(data=new_data,
             aes(x = variable, y = value, fill = type),
             width=0.5, colour="grey40", size=0.4, stat = "identity") + 
    scale_fill_discrete(drop=FALSE) +
    labs(x="phases",y="") + ggtitle("Evolution du nombre total de questions \n effectuées par période dans chaque cluster") +
    theme(plot.title = element_text(hjust = 0.5),legend.position = 'none') + scale_fill_manual(values=c("#1100F9")) +
    facet_wrap(~cluster,nrow=2)
}