#' Plots duration informations
#'
#' This function will plot the average and median duration of a phase, for each cluster.
#'
#' @param tab A data.table object containing the clustering data.
#' @param phases A numeric indicating the number of phases. 5 is the default value.
#' @return A bar chart.
#' @examples
#' affiche_phase_duration(tab)
#' @export

affiche_phase_duration <- function(tab,phases = 5){
  delta <- tab[, .(`Durée moyenne` = round(mean(delta)/phases),
                   `Durée médiane` = round(median(delta)/phases)), by = cluster][order(cluster)]
  ylim_max <- max(delta$`Durée moyenne`,delta$`Durée médiane`)
  amBarplot(x = "cluster", y = c("Durée moyenne", "Durée médiane"),
            data = delta, legend = TRUE, ylim = c(0,ylim_max),
            groups_color = c("#080072", "#00F3DB"),
            main = "Durée moyenne et médiane (en jours) d'une phase pour chaque cluster", export = TRUE)
}