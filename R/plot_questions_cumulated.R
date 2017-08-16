#' Plots cumulated informations
#'
#' This function will plot the cumulated average number of questions (A and C) attempted during each phase, for each cluster.
#'
#' @param tab A data.table object containing the clustering data.
#' @param phases A numeric indicating the number of phases. 5 is the default value.
#' @return A bar chart.
#' @examples
#' affiche_questions_cumul(tab)
#' @export

affiche_questions_cumul <- function(tab,phases = 5){
  var <- c("cluster")
  for (i in 1:phases){
    var <- c(var,paste0("nb_training_attempt_t",i))
  }
  tab <- data.frame(tab)
  inter <- tab[,var]
  inter <- data.table(inter)
  inter <- inter[, lapply(.SD, mean), by=cluster][order(cluster)]
  attempts <- reshape2::melt(inter,id.vars = "cluster")
  attempts <- attempts[, .(x = value)]
  
  
  names <- c("cluster")
  for (i in 1:phases){
    names <- c(names,paste0("nb_moy_questions_A_t",i))
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
  new_data1 <- data.table(new_data1,attempts)
  new_data1[, value := value*x][, x := NULL]
  new_data1$type <- rep("Niveau A",nrow(new_data1))
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
    names <- c(names,paste0("nb_moy_questions_C_t",i))
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
  new_data2 <- data.table(new_data2,attempts)
  new_data2[, value := value*x][, x := NULL]
  new_data2$type <- rep("Niveau C",nrow(new_data2))
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
  
  new_data <- rbind(new_data1,new_data2)
  
  ggplot() +
    geom_line(data=new_data,
              aes(x = variable, y = value, color = type, group=type),size=1.5) +
    scale_fill_discrete(drop=FALSE) +
    labs(x="phases",y="") + ggtitle("Evolution du nombre cumulé de questions A et C \n effectuées par période dans chaque cluster") +
    theme(plot.title = element_text(hjust = 0.5),legend.title = element_blank()) +
    facet_wrap(~cluster,nrow=2) + scale_color_manual(values=c("#F90019","#1000F3"))
}