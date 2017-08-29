#' Plots repartition in clusters
#'
#' This function will represent the percentage of individuals from the sample in each cluster, as a pie chart.
#'
#' @param tab A data.table object containing the clustering data.
#' @return A pie chart.
#' @examples
#' affiche_repartition(tab)
#' @export

affiche_repartition <- function(tab){
  data_pie <- tab[,.(value = .N), by = cluster][, .(label = cluster, value = 100*round(value/nrow(tab),2))][order(label)]
  data_pie[, label := paste("Cluster",label)]
  
  rAmCharts::amPie(data = data_pie,
                   legend = FALSE,
                   main = "Part des individus de l'échantillon présente dans chaque cluster",
                   export = TRUE)
}

#' Plots success rate for each cluster
#'
#' This function will plot the success rate at the exam for each cluster, as a bar chart.
#'
#' @param tab A data.table object containing the clustering data.
#' @return A bar chart.
#' @examples
#' affiche_resultats(tab)
#' @export

affiche_resultats <- function(tab){
  resultat <- tab[, .(success_rate = round(mean(resultat),2)), by = cluster][order(cluster)]
  resultat[success_rate <= 0.7, color := "#FB1900"]
  resultat[success_rate > 0.7 & success_rate < 0.80, color := "#FB7500"]
  resultat[success_rate >= 0.80, color := "#00FB00"]
  amBarplot(x = "cluster", y = "success_rate",
            data = resultat, legend = FALSE, ylim = c(0,1),
            show_values = TRUE,
            main = "Taux de réussite à l'examen", export = TRUE)
}

#' Plots average duration of weekly work
#'
#' This function will plot the average duration of work per week, in each cluster.
#'
#' @param tab A data.table object containing the clustering data.
#' @param phases A numeric indicating the number of phases. 5 is the default value.
#' @return A bar chart.
#' @examples
#' affiche_duration(tab)
#' @export

affiche_duration <- function(tab,phases = 5){
  names <- c("cluster")
  for (i in 1:phases){
    names <- c(names,paste0("duree_par_semaine_t",i))
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
  new_data[, color := ifelse(value >= 210,"oui","non")]
  
  
  ggplot() +
    geom_bar(data=new_data,
             aes(x = variable, y = value, fill = color),
             width=0.5, colour="grey40", size=0.4, stat = "identity") + 
    scale_fill_discrete(drop=FALSE) +
    labs(x="phases",y="") + ggtitle("Evolution du temps moyen passé (en minutes) \n par semaine sur la plateforme par phase") +
    theme(plot.title = element_text(hjust = 0.5),legend.position='none') + geom_hline(yintercept=210,color = "blue", size=1.5) + 
    scale_fill_manual(values=c("#FB1100","#00FB00")) +
    facet_wrap(~cluster,nrow=2) 
}

#' Plots MOOC informations
#'
#' This function will plot the proportion of MOOC chapters used and prepared in each cluster.
#'
#' @param tab A data.table object containing the clustering data.
#' @return A bar chart.
#' @examples
#' affiche_mooc(tab)
#' @export

affiche_mooc <- function(tab){
  resultat <- tab[, .(Used = round(mean(proportion_mooc_used, na.rm = TRUE),2),
                      Prepared = round(mean(proportion_mooc_prepared),2)), by = cluster][order(cluster)]
  amBarplot(x = "cluster", y = c("Used", "Prepared"),
            data = resultat, legend = TRUE, ylim = c(0,1),
            groups_color = c("#590086", "#FAF216"),
            main = "Proportion de MOOC utilisés et préparés dans chaque cluster", export = TRUE)
}

#' Plots 'formation présentielle' infos
#'
#' This function will plot the proportion of indivudals who have been involved in the 2-day formation, in each cluster.
#'
#' @param tab A data.table object containing the clustering data.
#' @param graph.number A numeric indicating which graph you want to plot. 1 will plot the general participation proportion in each group. 2 will plot the results ~ participation.
#' @return A bar chart.
#' @examples
#' affiche_forma(tab)
#' @export

affiche_forma <- function(tab,graph){
  resultat <- tab[, .(cluster,formation_presentielle,resultat)]
  if(graph == 1){
    resultat$formation_presentielle <- as.numeric(resultat$formation_presentielle)
    
    res_forma_pres <- resultat[,.(`Proportion ayant suivi la formation` = round(mean(formation_presentielle),2)),by = cluster][order(cluster)]
    amBarplot(x = "cluster", y = "Proportion ayant suivi la formation",
              data = res_forma_pres, legend = TRUE, ylim = c(0,1),
              groups_color = c("#1200B6"),
              main = "Taux de participation à la formation de 2 jours", export = TRUE)
  } else {
    resultat$formation_presentielle <- as.factor(resultat$formation_presentielle)
    resultat$cluster <- factor(resultat$cluster)
    levels(resultat$formation_presentielle) <- c("Non","Oui")
    res <- resultat[, .(`Taux de réussite` = round(mean(resultat),2)), by = .(cluster,formation_presentielle)][order(cluster,formation_presentielle)]
    res[, color := ifelse(formation_presentielle == "Non","non","oui")]
    ggplot() +
      geom_bar(data=res,
               aes(x = formation_presentielle, y = `Taux de réussite`,fill = color),
               width=0.5, colour="grey40", size=0.4, stat = "identity") + 
      scale_fill_discrete(drop=FALSE) +
      labs(x="Formation Présentielle",y="")+ ggtitle("Taux de réussite selon la participation \n ou non à la formation présentielle") +
      theme(plot.title = element_text(hjust = 0.5),legend.position='none') + scale_fill_manual(values=c("#FB1100","#00FB00")) +
      facet_wrap(~cluster,nrow=2)
  }
  
}