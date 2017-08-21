#' Plots average duration of weekly work
#'
#' This function will plot the average duration of work per week, in each group.
#'
#' @param tab A data.table object containing the clustering data.
#' @param phases A numeric vector indicating the phases you want to plot.
#' @return A bar chart.
#' @examples
#' ## Display the summary of the first 3 phases
#' description_duration(tab,phases = 1:3)
#' @export

description_duration <- function(tab,phases){
  names <- c("resultat")
  for (i in phases){
    names <- c(names,paste0("duree_par_semaine_t",i))
  }
  tab <- data.frame(tab)
  training <- tab[,names]
  training <- data.table(training)
  res <- training[, lapply(.SD, mean), by=resultat][order(resultat)]
  res <- data.frame(res)
  noms <- c("resultat")
  for (i in phases){
    noms <- c(noms,paste0("t",i))
  }
  names(res) <- noms
  new_data <- reshape2::melt(res,id.vars = "resultat")
  new_data <- data.table(new_data)
  new_data[, color := ifelse(value >= 210,"oui","non")]
  
  
  ggplot() +
    geom_bar(data=new_data,
             aes(x = variable, y = value, fill = color),
             width=0.5, colour="grey40", size=0.4, stat = "identity") + 
    scale_fill_discrete(drop=FALSE) +
    labs(x="phases",y="") + ggtitle("Evolution du temps moyen passé (en minutes) \n par semaine sur la plateforme par phase") +
    theme(plot.title = element_text(hjust = 0.5),legend.position='none') + geom_hline(yintercept=210,color = "blue", size=1.5) + 
    scale_fill_manual(values=c("#FB1100","#00FB00")) +
    facet_wrap(~resultat,nrow=2) 
}