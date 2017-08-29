#' Plots cumulated informations
#'
#' This function will plot the cumulated improvement and working time during each phase, for each cluster.
#'
#' @param tab A data.table object containing the clustering data.
#' @param phases A numeric indicating the number of phases. 5 is the default value.
#' @return A bar chart.
#' @examples
#' affiche_progression(tab)
#' @export

affiche_progression <- function(tab,phases = 5){
  names <- c("cluster")
  for (i in 1:phases){
    names <- c(names,paste0("progression_A_t",i))
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
  new_data1 <- reshape2::melt(res,id.vars = "cluster")
  new_data1 <- data.table(new_data1)
  new_data1$type <- rep("Progression niveau A (en %)",nrow(new_data1))
  value <- new_data1$value
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
  new_data1$value <- value
  remove(names)
  
  names <- c("cluster")
  for (i in 1:phases){
    names <- c(names,paste0("progression_C_t",i))
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
  new_data2 <- reshape2::melt(res,id.vars = "cluster")
  new_data2 <- data.table(new_data2)
  new_data2$type <- rep("Progression niveau C (en %)",nrow(new_data2))
  value <- new_data2$value
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
  new_data2$value <- value
  remove(names)
  
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
  new_data3[, value := value/(60*24)]
  new_data3$type <- rep("Durée totale (en jours)",nrow(new_data3))
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
  
  new_data <- rbind(new_data1,new_data2,new_data3)
  
  ggplot() +
    geom_line(data=new_data,
              aes(x = variable, y = value, color = type, group=type),size=1) +
    scale_fill_discrete(drop=FALSE) +
    labs(x="phases",y="") + ggtitle("Progression des scores aux entraînement \n et temps passé sur la plateforme (en cumulé)") +
    theme(plot.title = element_text(hjust = 0.5),legend.title = element_blank()) +
    theme(plot.title = element_text(hjust = 0.5),legend.title = element_blank()) +
    facet_wrap(~cluster,nrow=2) + scale_color_manual(values=c("#F30800","#2100C6","#00A40B"))
}