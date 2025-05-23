% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wikiTools.R
\name{selectLang}
\alias{selectLang}
\title{Convert names of a wikiTools data frame to English or Spanish}
\usage{
selectLang(dbase, fields = names(dbase), language = "en")
}
\arguments{
\item{dbase}{dataframe obtained by a wikiTools function .}

\item{fields}{names of the dataframe to be translated (default: names of dbase).}

\item{language}{default: "en". Also accept "es".}
}
\value{
the input dataframe with changed names
}
\description{
Convert names of a wikiTools data frame to English or Spanish
}
\author{
Modesto Escobar, Department of Sociology and Communication, University of Salamanca
}
