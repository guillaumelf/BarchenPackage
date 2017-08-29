#' Plots cumulated working time
#'
#' This function will plot the cumulated working time (in hours) during each phase, for each cluster.
#'
#' @param tab A data.table object containing the clustering data.
#' @param phases A numeric indicating the number of phases. 5 is the default value.
#' @return A bar chart.
#' @examples
#' plot_cumulated_time(tab)
#' @export

plot_cumulated_time <- function(tab,phases = 5){
  names <- c("cluster")
  for (i in 1:phases){
    names <- c(names,paste0("duree_totale_plateforme_t",i))
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
  new_data3 <- reshape2::melt(res,id.vars = "cluster")
  new_data3 <- data.table(new_data3)
  new_data3[, value := value/60]
  new_data3$type <- rep("Durées cumulées (en heures)",nrow(new_data3))
  value <- new_data3$value
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
  new_data3$value <- value
  
  ggplot() +
    geom_line(data=new_data3,
              aes(x = variable, y = value, color = type, group=type),size=1) +
    scale_fill_discrete(drop=FALSE) +
    labs(x="phases",y="") + ggtitle("Temps passé sur la plateforme (en heures cumulées)") +
    theme(plot.title = element_text(hjust = 0.5),legend.position = 'none') +
    scale_y_continuous(name="Durée (en heures)",breaks = seq(0,max(new_data3$value),by = 20)) +
    facet_wrap(~cluster,nrow=2) + scale_color_manual(values=c("#F30800"))
}