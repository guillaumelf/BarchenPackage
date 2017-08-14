#' Plots training informations
#'
#' This function will plot the average number of questions attempted per session, for each cluster.
#'
#' @param tab A data.table object containing the clustering data.
#' @param phases A numeric indicating the number of phases. 5 is the default value.
#' @return A bar chart.
#' @examples
#' affiche_tr_questions(tab)
#' @export

affiche_tr_questions <- function(tab,phases = 5){
  names <- c("cluster")
  for (i in 1:phases){
    names <- c(names,paste0("nb_moy_questions_A_t",i))
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
  new_data1 <- reshape2::melt(res,id.vars = "cluster")
  new_data1$type <- rep("Nombre de questions A",nrow(new_data1))
  
  names <- c("cluster")
  for (i in 1:phases){
    names <- c(names,paste0("nb_moy_questions_C_t",i))
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
  new_data2 <- reshape2::melt(res,id.vars = "cluster")
  new_data2$type <- rep("Nombre de questions C",nrow(new_data2))
  new_data <- rbind(new_data1,new_data2)
  
  ggplot() +
    geom_bar(data=new_data,
             aes(x = variable, y = value, fill = type),
             width=0.5, colour="grey40", size=0.4, stat = "identity") + 
    scale_fill_discrete(drop=FALSE) +
    labs(x="phases",y="", title = "Evolution au cours du temps du nombre moyen de questions effectuées par session") +
    theme(plot.title = element_text(hjust = 0.5),legend.title = element_blank()) +
    facet_wrap(~cluster,nrow=2)+
    scale_fill_brewer(palette="Greens")
}