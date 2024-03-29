% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/affect_donnees_externes.R
\name{precipitation}
\alias{precipitation}
\title{Affecte à chaque placette et chaque arbres de la base de données IFN une valeur de précipitation issue d'un raster de précipitation}
\usage{
precipitation(IFN_data, prec)
}
\arguments{
\item{IFN_data}{base de donnnées IFN (du même type que celle issue de la fonction import_IFN(...))}

\item{prec}{cartographie des précipitations au format raster et nécessairement projeté en lambert 93 (code EPSG : 2154)}
}
\value{
renvoie la base de données IFN avec un ajout de la donnée de précipitation sur une placette (prec).
Donnée ajoutée aux tableux de données par placette et par arbre.
}
\description{
Affecte à chaque placette et chaque arbres de la base de données IFN une valeur de précipitation issue d'un raster de précipitation
}
