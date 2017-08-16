#' Plots cumulated informations
#'
#' This function will plot the cumulated numbers of questions attempted in each phase.
#'
#' @param tab A data.table object containing the clustering data.
#' @param phases A numeric indicating the number of phases. 5 is the default value.
#' @return A bar chart.
#' @examples
#' affiche_nb_quest_cumul(tab)
#' @export

affiche_nb_quest_cumul <- function(tab,phases = 5){
  names <- c("cluster")
  for (i in 1:phases){
    names <- c(names,paste0("nb_total_questions_t",i))
  }
  tab <- data.frame(tab)
  res <- tab[,names]
  res <- data.table(res)
  res <- res[, lapply(.SD, mean), by=cluster][order(cluster)]
  noms <- c("cluster")
  for (i in 1:phases){
    noms <- c(noms,paste0("t",i))
  }
  names(res) <- noms
  new_data <- reshape2::melt(res,id.vars = "cluster")
  new_data <- data.table(new_data)
  value <- new_data$value
  n <- as.numeric(length(levels(tab$cluster)))
  j = 1
  value_t1 <- value[j:n]
  for(i in 2:phases){
    j <- j + n
    borne_sup <- i*n
    assign(paste0("value_t",i),value[j:borne_sup])
  }
  
  value <- value_t1
  for(i in 2:phases){
    x <- get(paste0("value_t",i-1))+get(paste0("value_t",i))
    assign(paste0("value_t",i),x)
    value <- c(value,get(paste0("value_t",i)))
  }
  new_data$value <- value
  
  ggplot() +
    geom_line(data=new_data,
              aes(x = variable, y = value, color = cluster, group=cluster),size=2) +
    scale_fill_discrete(drop=FALSE) +
    labs(x="phases",y="") + ggtitle("Evolution du nombre cumulé de questions \n effectuées par période dans chaque cluster") +
    theme(plot.title = element_text(hjust = 0.5),legend.title = element_blank()) +
    facet_wrap(~cluster,nrow=2)
}