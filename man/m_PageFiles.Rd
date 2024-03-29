% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wiki_utils.R
\name{m_PageFiles}
\alias{m_PageFiles}
\title{Get URL of files in Wikipedia pages}
\usage{
m_PageFiles(titles, project = "en.wikipedia.org", exclude_ext = "svg|webp|xcf")
}
\arguments{
\item{titles}{A vector of article titles to search for.}

\item{project}{Wikimedia project, defaults "en.wikipedio.org".}

\item{exclude_ext}{File extensions excluded in results. Default 'svg|webp|xcf'}
}
\value{
A data-frame with original titles, the normalized ones, status
for the page and the URL files of the Wikipedia pages, using use "|" to
separate ones) or NA if files do not exits or are excluded.
}
\description{
Search for URL of files inserted in Wikipedia pages. Exclude extensions
in exclude_ext. Note that the query API named this search as 'images',
but all source files in the page are returned. The function only return URL
that not end with extensions in exclude_ext parameter (case insensitive).
Automatically resolves redirects, the "normalized" column of the returned
data-frame contains the destiny page of the redirection.
See https://en.wikipedia.org/w/api.php?action=help&modules=query\%2Bimages
}
\examples{
f <- m_PageFiles(c('Max Planck', URLdecode("a\%CC\%8C"), 'Max', 'Cervante', 'humanist'),
                  exclude_ext = "svg|webp|xcf")
}
\author{
Angel Zazo, Department of Computer Science and Automatics, University of Salamanca
}
