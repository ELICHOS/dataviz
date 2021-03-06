% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/kahtml.R
\name{kahtml}
\alias{kahtml}
\title{kahtml}
\usage{
kahtml(data, vars, row_col, struct_data, alter.datanames = NULL)
}
\arguments{
\item{data}{data.frame. Voir table.con}

\item{vars}{voir table.con}

\item{row_col}{si "row" affiche la somme par ligne. Si "col", affiche la somme par colonnes. Tous les noms utilisés dans l'output de table.con peuvent être utilisés.}

\item{struct_data}{éventuellement, data.frame avec les données de structure du questionnaire (comprenant struct_data$question.text et struct_data$names).}

\item{alter.datanames}{manuel : c("Libellé pour la variable 1", "Libellé pour la variable 2")}
}
\description{
calcul ( avec basiques::table.con() ) et exporte au format html via kable() une table de contingence.
}
