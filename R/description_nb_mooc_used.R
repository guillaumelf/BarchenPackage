#' Plots MOOC informations
#'
#' This function will plot the average number of MOOC chapters used during each phase, for each group.
#'
#' @param tab A data.table object containing the clustering data.
#' @param phases A numeric vector indicating the phases you want to plot.
#' @return A bar chart.
#' @examples
#' ## Display the summary of the first 3 phases
#' description_nb_mooc_used(tab,phases = 1:3)
#' @export

description_nb_mooc_used <- function(tab,phases){
  names <- c("resultat")
  for (i in phases){
    names <- c(names,paste0("nb_mooc_used_t",i))
  }
  noms <- c("resultat")
  for (i in phases){
    noms <- c(noms,paste0("t",i))
  }
  tab <- data.frame(tab)
  res <- tab[,names]
  res <- data.table(res)
  res <- res[, lapply(.SD, mean), by=resultat][order(resultat)]
  names(res) <- noms
  new_data <- reshape2::melt(res,id.vars = "resultat")
  new_data <- data.table(new_data)
  new_data$type <- rep("x",nrow(new_data))
  
  
  ggplot() +
    geom_bar(data=new_data,
             aes(x = variable, y = value, fill = type),
             width=0.5, colour="grey40", size=0.4, stat = "identity") + 
    scale_fill_discrete(drop=FALSE) +
    labs(x="phases",y="") + ggtitle("Evolution au cours du temps du \n nombre moyen de chapitres du MOOC utilisés") +
    theme(plot.title = element_text(hjust = 0.5),legend.position = 'none') + scale_fill_manual(values=c("#590086")) +
    facet_wrap(~resultat,nrow=1)
}