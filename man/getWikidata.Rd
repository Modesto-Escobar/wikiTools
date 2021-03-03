\name{getWikidata}
\alias{getWikidata}
\title{
Get info from Wikidata
}
\description{
Gets information from Wikidata for an entry or name.
}
\usage{
getWikidata(namesVector)
}
\arguments{
  \item{namesVector}{A vector consisting of one or more Wikipedia's entry (i.e., topic or person).}
}
\details{
This function allows extract the available information of a name or vector of names from Wikidata. 

This informations includes, when available;  gender, birthdata and deathdate, birthplace and deathplace, citizenship, link to pics, occupation, movement, genres, from whom is influenced by and to whom influenced and a selection of notable work.
}
\value{
A data.frame of 15 variables containing all the available information from the search (including, among others: sex, birthdate and deathdate, citizenship, influences and occupation). It will be empty in case of error (there is not a Wikidata page available for such name).
}
\author{
Modesto Escobar, Department of Sociology and Communication, University of Salamanca. See \url{https://sociocav.usal.es/blog/modesto-escobar/}
}
\examples{
## For a single name:
getWikidata("Monet")

## For a larger list:
A <- c("Monet", "Renoir", "Caillebotte")
getWikidata(A)
}
