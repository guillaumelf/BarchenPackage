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
  resultat[success_rate > 0.7 & success_rate < 0.85, color := "#FB7500"]
  resultat[success_rate >= 0.85, color := "#00FB00"]
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
#' @return A bar chart.
#' @examples
#' affiche_duration(tab)
#' @export

affiche_duration <- function(tab){
  inter <- tab[, .(duree_totale_plateforme,delta,cluster)]
  inter[, nb_semaines := round(delta/7)]
  inter[, duree_par_semaine := duree_totale_plateforme/nb_semaines ]
  resultat <- inter[, .(duree_moy_per_week = round(mean(duree_par_semaine))), by = cluster][order(cluster)]
  resultat[, `Recommandation suivie ?` := ifelse(duree_moy_per_week >= 210,"oui","non")]
  
  ggplot(resultat)+aes(x = cluster,
                       y = duree_moy_per_week,fill = `Recommandation suivie ?`)+geom_bar(stat="identity")+ggtitle("Durée moyenne (en minutes) passée \n par semaine sur la plateforme")+labs(x = "Clusters",y = "Durée")+theme(plot.title = element_text(hjust = 0.5))+geom_hline(yintercept=210,color = "blue", size=2)+scale_fill_manual(values=c("#FB1100","#00FB00"))
  
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