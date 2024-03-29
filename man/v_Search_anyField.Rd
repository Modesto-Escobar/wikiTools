% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wiki_utils.R
\name{v_Search_anyField}
\alias{v_Search_anyField}
\title{Run a CQL Query (cql.any) in VIAF}
\usage{
v_Search_anyField(name)
}
\arguments{
\item{name}{It is the name o search.}
}
\value{
A list with the records found.
}
\description{
This function is a wrapper to v_Search, using this CQL_Query:
'cql.any = "string"'
Search preferred Name - names which are the preferred form in an authority record
(1xx fields of the MARC records).
1xx: Main Entry: Personal name (100), corporate name (110), meeting name (111), uniform title (130)
The search is performed with the "=" operator (all terms are searched, and only
those ones).
}
\note{
Note that the search is performed with the "=" operator, it is not
an "all" search. The "=" operator searches for all words and only those words
in the order that words appear. With the "all" operator, more words can be in
the field and in any order. Search is case insensitive.
}
