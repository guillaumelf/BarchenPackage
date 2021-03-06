% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/plot_cumulated_quest.R
\name{affiche_nb_quest_cumul}
\alias{affiche_nb_quest_cumul}
\title{Plots cumulated informations}
\usage{
affiche_nb_quest_cumul(tab, phases = 5)
}
\arguments{
\item{tab}{A data.table object containing the clustering data.}

\item{phases}{A numeric indicating the number of phases. 5 is the default value.}
}
\value{
A bar chart.
}
\description{
This function will plot the cumulated numbers of questions attempted in each phase.
}
\examples{
affiche_nb_quest_cumul(tab)
}

