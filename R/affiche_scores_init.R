#' Plots initial scores for each cluster
#'
#' This function will plot the initial scores after 100 questions for each cluster, as a bar chart.
#'
#' @param tab A data.table object containing the clustering data.
#' @return A bar chart.
#' @examples
#' affiche_score_init(tab)
#' @export

affiche_score_init <- function(tab){
  resultat <- tab[niveau_initial_A > 0 & niveau_initial_C > 0, .(`Score A` = round(mean(niveau_initial_A),2),
                                                                 `Score C` = round(mean(niveau_initial_C),2)), by = cluster][order(cluster)]
  amBarplot(x = "cluster", y = c("Score A","Score C"),
            data = resultat, legend = TRUE, ylim = c(0,1),
            show_values = TRUE, groups_color = c("#FD0800","#1C00D0"),
            main = "Scores après 100 questions", export = TRUE)
}
