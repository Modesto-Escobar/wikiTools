#### wiki_utils.R
#### Angel Zazo <angelzazo@usal.es>
#### ver. 0.0 2021-11-09
#### ver. 0.1 2023-05-12
#### ver. 0.2 2023-07-27

# General user_agent header for Wikimedia, Wikidata, MediaWiki and VIAF requests
# See https://meta.wikimedia.org/wiki/User-Agent_policy.
user_agent <- paste('netCoincidenceAnalysis Bot (<https://sociocav.usal.es/me/GAS/>),', R.version.string)

#' limit_requester
#' Limits the rate at which a function will execute
#' @param f The original function
#' @param n Number of allowed events within a period
#' @param period Length (in seconds) of measurement period
#' @return If 'f' is a single function, then a new function with the same
#' signature and (eventual) behavior as the original function, but rate limited.
#' If 'f' is a named list of functions, then a new list of functions with the
#' same names and signatures, but collectively bound by a shared rate limit.
#' Used only for WikiData Query Service (WDQS).
#' @author Angel Zazo, Department of Computer Science and Automatics, University of Salamanca
#' @seealso ratelimitr
#' @importFrom ratelimitr limit_rate rate
limit_requester <- function(f, n, period) {
  return(ratelimitr::limit_rate(f, ratelimitr::rate(n = n, period = period)))
}




#' reqWDQS
#' Retrieves responses from Wikidata Query Service (WDQS)
#' @param sparql_query A string with the query in SPARQL language.
#' @param format  A string with the query response format. See
#' https://www.mediawiki.org/wiki/Wikidata_Query_Service/User_Manual#SPARQL_endpoint.
#' Only  'json', 'xml' or 'csv' formats are permitted, default 'json'.
#' @param method The method used in the httr request. Default 'GET'.
#' @return The response in the format selected. Please check httr::stop_for_status(response)
#' @author Angel Zazo, Department of Computer Science and Automatics, University of Salamanca
#' @importFrom httr GET POST user_agent add_headers
#' @note For short queries GET method is better, POST for long ones. Only GET queries as cached.
reqWDQS <- function(sparql_query, format='json', method='GET') {
  if (format=='json')      wdqs_format <- "application/sparql-results+json"
  else if (format=='xml')  wdqs_format <- "application/sparql-results+xml"
  else if (format=='csv')  wdqs_format <- "text/csv"
  else stop(paste0("ERROR: format '", format, "' is not supported."))

  if (method=='GET')
    httr::GET(
      url = 'https://query.wikidata.org/sparql',
      query = list(query = sparql_query),
      httr::user_agent(user_agent),
      httr::add_headers(Accept = wdqs_format)
    )
  else if (method=='POST')
    httr::POST(
      url = 'https://query.wikidata.org/sparql',
      body = list(query = sparql_query),
      httr::add_headers(Accept = wdqs_format, user_agent = user_agent),
      encode = 'form'
    )
  else stop(paste0("ERROR: method '", method, "' is not supported."))
}

#' WDQS_query
#' Retrieves responses from Wikidata Query Service (WDQS). Uses ratelimitr if
#' param limit_requester = TRUE.
#' @param sparql_query A string with the query in SPARQL language.
#' @param format A string with the query response format.
#' See https://www.mediawiki.org/wiki/Wikidata_Query_Service/User_Manual#SPARQL_endpoint.
#' Only  'json', 'xml' or 'csv' formats are permitted, default 'csv'.
#' @param method The method used in the httr request. Default 'GET'.
#' @param limit_requester If True, uses ratelimitr to limit the requests.
#' @return The response in selected format or NULL on errors.
#' @author Angel Zazo, Department of Computer Science and Automatics, University of Salamanca
#' @importFrom httr stop_for_status content
#' @importFrom jsonlite fromJSON
#' @importFrom utils read.csv
WDQS_query <- function(sparql_query, format='csv', method='GET',
                       limit_requester = FALSE) {
  # reqWDQS_rated: The ratelimitr version of reqWDQS
  # https://www.mediawiki.org/wiki/Wikidata_Query_Service/User_Manual#SPARQL_endpoint
  if (limit_requester)
    reqWDQS_rated <- limit_requester(reqWDQS, n=60, period=60)
  else
    reqWDQS_rated <- reqWDQS
  #
  tryCatch(
    {
      r <- reqWDQS_rated(sparql_query, format=format, method=method)
      httr::stop_for_status(r)
      content <- httr::content(r, as = "text", encoding = "UTF-8")
      rtype <- httr::http_type(r)
      if (format == 'json' && startsWith(rtype, "application/sparql-results+json"))
        output <- jsonlite::fromJSON(content, simplifyVector = FALSE)
      else if (format == 'xml' && startsWith(rtype, "application/sparql-results+xml"))
        output <- content
      else if (format == 'csv' && startsWith(rtype, "text/csv"))
        output <- read.csv(text = content)
      else
        stop(paste0("ERROR: format '", format, "' or response type '",rtype,"' is incorrect."))
      return(output)
    }, error = function(e){
      cat(as.character(e), file=stderr())
      return(NULL)
    }
  )
}

#' WDQS_isInstanceOf(entity_list, instanceof)
#' Search using WDQS if the Wikidata entities in entity_list are instances of
#' "instanceof" Wikipedia class. For example, if instanceof="Q5", search if
#' entities are instances of the Wikidata class Q5, i.e, are humans.
#' Duplicated entities are deleted before search.
#' @param entity_list A vector with de Wikidata entities.
#' @param instanceof The Wikidata class to check, madatory.
#' @return A data-frame with two columns, first Wikidata entity, second True
#' if that entity is instance of the "instanceof" entity, else False. Index of
#' data-frame are also set to entity_list.
#' @author Angel Zazo, Department of Computer Science and Automatics, University of Salamanca
#' @export
WDQS_isInstanceOf <- function(entity_list, instanceof) {
  if (instanceof == '')
    stop(paste0("ERROR: instanceof is mandatory."))
  #
  entity_list <- unique(entity_list)
  values <- paste0("wd:",paste0(entity_list, collapse = ' wd:'))
  query <- paste0('SELECT ?entity WHERE {VALUES ?entity {', values,'}',
                  ' ?entity wdt:P31 wd:', instanceof, '}')
  r <- WDQS_query(query, format="csv", method='POST')
  r$entity <- sub('^http://www.wikidata.org/entity/', '', r$entity)
  #
  data <- ifelse(entity_list %in% r$entity, T, F)
  d <- data.frame(data)
  colnames(d) <- c("entity", paste0("instanceof_", instanceof))
  rownames(d) <- entity_list
  return(d)
}

#' WDQS_Wikipedias(entity_list, instanceof, wikilangs, nlimit)
#' Gets from Wikidata all Wikipedia pages of the Wikidata entities in
#' entity_list. If set "instanceof", then only returns the pages for Wikidata
#' entities which are instances of that Wikidata class. If langs='', then
#' returns all Wikipedia pages, else only the languages in langs.
#' Duplicated entities are deleted before search.
#' @param entity_list A vector with de Wikidata entities.
#' @param instanceof Wikidata entity class to limit the result to the instances
#' of that class. For example, if instanceof='Q5', limit the results to "human".
#' @param wikilangs List of languages to limit the search, using "|" as
#' separator. Wikipedias are returned in same order as languages in this
#' parameter. If wikilangs=='' the function returns Wikipedia pages in any
#' language, not sorted.
#' @param nlimit If the number of entities exceeds this number, do chunked
#' queries. This is the number of entities requested in each chunk.
#' @return A data-frame with four columns, first the count of Wikipedia pages,
#' second, the langs, third, the page name, and last, the URL of the
#' wikipedia pages. Last three use "|" as separator. Index of the data-frame is
#' also set to entity_list.
#' @author Angel Zazo, Department of Computer Science and Automatics, University of Salamanca
#' @export
WDQS_Wikipedias <- function(entity_list, instanceof = '',
                            wikilangs = 'es|en|ca|eu|gl|pt|fr|it|de|ru|ko|ja|zh',
                            nlimit = 1500) {
  # Eliminate duplicates
  entity_list <- unique(entity_list)
  # Check limits to make chunked queries
  n <- length(entity_list)
  if (n > nlimit) {
    print(paste0("INFO: The number of entities (", n, ") exceeds nlimit (", nlimit,"): do chunked queries."))
    nlim <- as.integer(n/nlimit)
    for (k in 0:nlim) {
      offset <- nlimit * k
      q_list <- entity_list[(offset+1):min(n,offset+nlimit)]
      print(paste0(" INFO: get from ", offset+1," to ", offset+length(q_list)))
      # print(q_list)
      d <- WDQS_Wikipedias(q_list, instanceof, wikilangs, nlimit)
      if (k==0)
        output <- d
      else
        output <- rbind(output, d)
    }
    return(output)
  }
  #
  values <- paste0("wd:", paste0(entity_list, collapse = ' wd:'))
  if (wikilangs != "") {
    wikiorder  <- strsplit(wikilangs, '|', fixed = T)[[1]]
    wikifilter <- paste0("'",gsub('|', "','", wikilangs, fixed=T), "'")
  }
  filterq <- ifelse(instanceof != "", TRUE, FALSE)
  query <- paste0("SELECT ?entity (COUNT(?page) as ?count)
(GROUP_CONCAT(?lang;separator='|') as ?langs)
(GROUP_CONCAT(?name;separator='|') as ?names)
(GROUP_CONCAT(?page;separator='|') as ?urls)
WHERE {\n",
  ifelse(filterq, '', 'OPTIONAL {\n'),
  "  VALUES ?entity {", values, "}\n",
  ifelse(filterq, '  ', '  # '),"?entity wdt:P31 wd:", instanceof, ".
  OPTIONAL {
    ?page schema:about ?entity;
          schema:inLanguage ?lang;
          schema:name ?name;
          schema:isPartOf [wikibase:wikiGroup \"wikipedia\"].\n",
  ifelse(wikilangs!="", paste0("  FILTER(?lang in (", wikifilter, "))}\n"), "}\n"),
  ifelse(filterq,'', "}\n"),
  "} GROUP BY ?entity")
  r <- WDQS_query(query, format='csv', method='POST')
  r$entity <- sub('http://www.wikidata.org/entity/', '', r$entity)
  rownames(r) <- r$entity

  # Order by wikilangs
  for (entity in r$entity) {
    if (wikilangs != "" & r[entity, 'count'] > 1) {
      l <- strsplit(r[entity, 'langs'], '|', fixed = T)[[1]]
      o <- match(wikiorder, l)
      o <- o[!is.na(o)]
      r[entity, "langs"] <- paste0(l[o], collapse = '|')
      l <- strsplit(r[entity, 'names'], '|', fixed = T)[[1]]
      r[entity, "names"] <- paste0(l[o], collapse = '|')
      l <- strsplit(r[entity, 'urls'], '|', fixed = T)[[1]]
      r[entity, "urls"] <- paste0(l[o], collapse = '|')
    }
  }
  # Create a NA data-frame and fill it with retrieved values
  q <- data.frame(entity=entity_list, count=NA, langs=NA, names=NA, urls=NA)
  rownames(q) <- q$entity
  q[r$entity, ] <- r
  return(q)
}

#' WDQS_Redirection(entity_list)
#' Returns the Wikidata entities which are the redirection of the entities in
#' entity_list. Return a data-frame with entities and values the redirection
#' (destiny) or NA if not exists the destiny or entity not exits or it is
#' deleted. Duplicated entities are deleted before search.
#' @param entity_list A vector with de Wikidata entities.
#' @return: A data-frame with properties of entity about redirections. Index of
#' data-frame are also set to entity_list.
#'  WDQS_Redirection(c("Q9021", "Q115637688", "Q105660123"))
#'  Q9021       -> NA \[Not redirect\]
#'  Q115637688  -> NA \[Was Deleted\]
#'  Q105660123  -> Q97352588 \[Redirect to Q97352588\]
#' @author Angel Zazo, Department of Computer Science and Automatics, University of Salamanca
#' @export
WDQS_Redirection <- function(entity_list) {
  entity_list <- unique(entity_list)
  values <- paste0("wd:", paste0(entity_list, collapse = ' wd:'))
  query <- paste0('SELECT ?entity ?destiny WHERE {VALUES ?entity {', values, '} ?entity owl:sameAs ?destiny}')
  #cat(query)
  #
  r <- WDQS_query(query, format = "csv", method = 'POST')
  for (c in c('entity',  'destiny'))
    r[[c]] <- gsub('http://www.wikidata.org/entity/', '', r[[c]])
  #
  q <- data.frame(entity=entity_list, destiny=NA)
  rownames(q) <- q$entity
  q[r$entity, ] <- r
  return(q)
}

#' WDQS_OccupationCount(Qoc)
#' Search in WDQS to know the number of Wikidata entities with P106 property
#' (occupation) set to Qoc.
#' @param Qoc The Wikidata entity of the occupation. For example, Q2306091
#' sociologist, Q2526255 Film director, etc.
#' @return The number of entities with that occupation (integer).
#' @author Angel Zazo, Department of Computer Science and Automatics, University of Salamanca
#' @export
WDQS_OccupationCount <- function(Qoc) {
 query <- paste0('SELECT (COUNT(DISTINCT ?human) AS ?count) WHERE {?human wdt:P106 wd:',Qoc,'}')
 r <- WDQS_query(query, format="csv")
 return(r$count)
}

#' WDQS_OccupationEntities(Qoc, nlimit)
#' Returns the Wikidata entities of the occupation Qoc. Use chunked requests.
#' @param Qoc The Wikidata entity of the occupation. For example, Q2306091 (sociologist)
#' @param nlimit If the number of entities found with that occupation exceeds
#' this number, do chunked queries. This is the number of entities requested
#' in each chunk.
#' @return A vector with the Wikidata entities with that occupation.
#' @author Angel Zazo, Department of Computer Science and Automatics, University of Salamanca
#' @export
WDQS_OccupationEntities <- function(Qoc, nlimit=10000) {
  entity <- character()
  nq <- WDQS_OccupationCount(Qoc)
  nlim <- as.integer(nq/nlimit)
  if (nlim > 0)
    print(paste0("INFO: The number of entities (", nq, ") exceeds nlimit (", nlimit,"): do chunked queries."))
  for (k in 0:nlim) {
    offset <- nlimit * k
    if (nlim > 0)
      print(paste0(" INFO: sending query from ", offset+1," to ", offset+nlimit))
    query <- paste0("SELECT ?entity WHERE {?entity wdt:P106 wd:", Qoc,
                    "} ORDER BY ?entity LIMIT ", nlimit, " OFFSET ", offset)
    r <- WDQS_query(query, format="csv")
    r$entity <- sub('^http://www.wikidata.org/entity/', '', r$entity)
    entity <- append(entity, r$entity)
  }
  entity <- unique(entity)
  return(entity)
}

#' WDQS_OccupationEntitiesyWikipedias(Qoc, wikilangs, nlimit)
#' Returns the Wikidata entities of the occupation Qoc, plus the Wikipedia pages
#' of them. Return all Wikipedias pages, not limited by languages. Use chunked
#' requests. Use chunked requests.
#' Note that WDQS_OccupationEntityWikipedias is similar to first launch
#' WDQS_OccupationEntity and then launch WDQS_Wikipedias, but is more efficient.
#' @param Qoc The Wikidata entity of the occupation. For example, Q2306091
#' (sociologist)
#' @param nlimit If the number of entities found with that occupation exceeds
#' this number, do chunked queries. This is the number of entities requested
#' in each chunk.
#' @return A data-frame with Wikidata entities and Wikipedia pages concatenated with "|".
#' @author Angel Zazo, Department of Computer Science and Automatics, University of Salamanca
#' @export
WDQS_OccupationEntitiesyWikipedias <- function(Qoc, nlimit=5000) {
  nq <- WDQS_OccupationCount(Qoc)
  nlim <- as.integer(nq/nlimit)
  for (k in 0:nlim) {
    offset <- nlimit * k
    query <- paste0("SELECT ?entity (GROUP_CONCAT(?page;separator='|') as ?pages)
WITH {
    SELECT ?entity
    WHERE {?entity wdt:P106 wd:", Qoc, ".}
    ORDER BY ?entity
    LIMIT ", nlimit, " OFFSET ", offset, "
    } AS %results
WHERE {
   INCLUDE %results.
   OPTIONAL {?page schema:about ?entity;
            schema:isPartOf [ wikibase:wikiGroup 'wikipedia' ] .}
} GROUP BY ?entity")
    r <- WDQS_query(query, format = "csv")
    r$entity <- sub('^http://www.wikidata.org/entity/', '', r$entity)
    rownames(r) <- r$entity
    if (k==0)
      output <- r
    else
      output <- rbind(output, r)
  }
  return(output)
}

#' WDQS_SearchByLabel_Exact(string, instanceof, Pproperty, langs)
#' Search Wikidata entities by exact search using case sensitive and
#' diferentiate diacritics in label and altLabel ("Also known as") in the
#' languages indicated in langs.
#' @param string string (label or altLabel) to search.
#' @param instanceof Wikidata entity of which the entities searched for are an
#' example or member of it (class). For example, if instanceof=Q5 the
#' search are filtered to Wikidata entities of class "Q5 = human".
#' @param Pproperty Wikidata properties, separated with '|', to optionally
#' include in the search. For example, is Pproperty="P21", the results
#' include information of the sex of entities found as STRING.
#' @param langs Languages in which the information will be searched. It also
#' is the order of languages to obtain information.
#' @return: A data-frame with 'entity', 'entityLabel', 'entityDescription' and,
#' additionally the properties of Pproperty.
#' @author Angel Zazo, Department of Computer Science and Automatics, University of Salamanca
#' @export
WDQS_SearchByLabel_Exact <- function(string, instanceof="", Pproperty="",
                                     langs='es|en|ca|eu|gl|pt|fr|it|de|ru|ko|ja|zh') {
  #
  string  <- gsub("'", "\\'", string, fixed = T)
  filterq <- if (instanceof != "") TRUE else FALSE
  searchp <- if (Pproperty != "") TRUE else FALSE
  langsw  <- gsub("|", ",", langs, fixed = T)
  #
  searchlang <- paste0('\nSERVICE wikibase:label {bd:serviceParam wikibase:language "', langsw, '".\n',
                       ' ?instanc rdfs:label ?instancLabel.')
  #
  if (searchp) {
    group_concat <- character()
    search <- character()
    for (p in strsplit(Pproperty, '|', fixed = T)[[1]]) {
      group_concat <- append(group_concat, paste0("(GROUP_CONCAT(DISTINCT STR(?", p ,"Label);separator='|') as ?",p,")"))
      search <- append(search, paste0('OPTIONAL {?entity wdt:', p, ' ?', p, '.}'))
      searchlang <- append(searchlang, paste0(' ?', p,' rdfs:label ?', p, 'Label.'))
    }
    group_concat <- paste(group_concat, collapse = "\n")
    search <- paste0(search, collapse = "\n")
    searchlang <- paste0(searchlang, collapse = "\n")
  }
  # UNION sentences with each language
  l <- strsplit(langs, '|', fixed = T)[[1]]
  units <- paste0("{?entity rdfs:label '", string, "'@", l, '}')
  unionlabel <- paste0(units, collapse = "\nUNION\n")
  unitsalt <- paste0("{?entity skos:altLabel '", string, "'@", l, '}')
  unionaltLabel <- paste0(unitsalt, collapse = "\nUNION\n")
  #
  query <- paste0("SELECT DISTINCT ?entity ?entityLabel ?entityDescription
(GROUP_CONCAT(DISTINCT ?instanc; separator='|') as ?instance)
(GROUP_CONCAT(DISTINCT ?instancLabel; separator='|') as ?instanceLabel)
(GROUP_CONCAT(DISTINCT STR(?altlabel);separator='|') as ?altLabel)\n",
if (searchp) group_concat else "",
"\nWHERE {\n",
unionlabel,
"\n UNION \n",
unionaltLabel,
"\nSERVICE wikibase:label {bd:serviceParam wikibase:language '", langsw,"'}",
"\n?entity wdt:P31 ?instanc.\n",
if (filterq) paste0('FILTER(?instanc = wd:',instanceof,').\n') else "",
"OPTIONAL {?entity skos:altLabel ?altlabel}\n",
if (searchp) paste0(search, searchlang, '}') else "",
"\n} GROUP BY ?entity ?entityLabel ?entityDescription")
  #
  # cat(query)
  #
  r <- WDQS_query(query, format = "csv")
  for (c in c('entity',  'instance'))
    r[[c]] <- gsub('http://www.wikidata.org/entity/', '', r[[c]])
  rownames(r) <- r$entity
  return(r)
}

#' WDQS_SearchByLabel_Startswith(string, instanceof, Pproperty, lang, langsorder)
#' Search Wikidata entities which label or altLabel starts with "string" in
#' language "lang". It's similar to a wildcard search: "string*". Diacritics and
#' case are ignored. Search "string" in language "lang" (mandatory) in label,
#' but in any language in altLabel.
#'
#' @param string string to search.
#' @param instanceof Wikidata entity of which the entities searched for are an
#' example or member of it (class). For example, if instanceof=Q5 the
#' search are filtered to Wikidata entities of class Q5 (human).
#' @param Pproperty Wikidata properties, separated with '|', to optionally
#' include in the search. For example, is Pproperty="P21", the results
#' include information of the sex of entities found.
#' @param lang language to search in label, mandatory.
#' @param langsorder Order of languages in which the information will be returned.
#' @return: A data-frame with 'entity', 'entityLabel', 'entityDescription',
#' 'instance', 'instanceLabel', 'altLabel' and, additionally, the properties of
#' Pproperty param.
#' @author Angel Zazo, Department of Computer Science and Automatics, University of Salamanca
#' @export
WDQS_SearchByLabel_Startswith <- function(string, instanceof="", Pproperty="",
                                          lang='es', langsorder='es|en|ca|eu|gl|pt|fr|it|de|ru|ko|ja|zh') {
  #
  string <- gsub("'", "\\'", string, fixed = T)
  filterq <- if (instanceof != "") TRUE else FALSE
  searchp <- if (Pproperty != "") TRUE else FALSE
  langsorderw <- gsub("|", ",", langsorder, fixed = T)
  #
  searchlang <- paste0('\nSERVICE wikibase:label {bd:serviceParam wikibase:language "', langsorderw, '".\n',
                       ' ?instanc rdfs:label ?instancLabel.')
  #
  if (searchp) {
    group_concat <- character()
    search <- character()
    for (p in strsplit(Pproperty, '|', fixed = T)[[1]]) {
      group_concat <- append(group_concat, paste0("(GROUP_CONCAT(DISTINCT STR(?", p ,"Label);separator='|') as ?",p,")"))
      search <- append(search, paste0('OPTIONAL {?entity wdt:', p, ' ?', p, '.}'))
      searchlang <- append(searchlang, paste0(' ?', p,' rdfs:label ?', p, 'Label.'))
    }
    group_concat <- paste(group_concat, collapse = "\n")
    search <- paste0(search, collapse = "\n")
    searchlang <- paste0(searchlang, collapse = "\n")
  }
  #
  query <- paste0("SELECT DISTINCT ?entity ?entityLabel ?entityDescription
(GROUP_CONCAT(DISTINCT ?instanc; separator='|') as ?instance)
(GROUP_CONCAT(DISTINCT ?instancLabel; separator='|') as ?instanceLabel)
(GROUP_CONCAT(DISTINCT STR(?altlabel);separator='|') as ?altLabel)\n",
if (searchp) group_concat else "",
'\nWHERE {
 SERVICE wikibase:mwapi {
  bd:serviceParam wikibase:api "EntitySearch";
                  wikibase:endpoint "www.wikidata.org";
                  mwapi:language "', lang, '";
                  mwapi:search "', string, '" .
  ?entity wikibase:apiOutputItem mwapi:item.
  }
 SERVICE wikibase:label {bd:serviceParam wikibase:language \'', langsorderw, "'}",
  "\n?entity wdt:P31 ?instanc.\n",
  if (filterq) paste0('FILTER(?instanc = wd:',instanceof,').\n') else "",
  "OPTIONAL {?entity skos:altLabel ?altlabel}\n",
  if (searchp) paste0(search, searchlang, '}') else "",
  "\n} GROUP BY ?entity ?entityLabel ?entityDescription")
  #
  # cat(query)
  #
  r <- WDQS_query(query, format = "csv")
  for (c in c('entity',  'instance'))
    r[[c]] <- gsub('http://www.wikidata.org/entity/', '', r[[c]])
  rownames(r) <- r$entity
  return(r)
}

#' WDQS_SearchByLabel_Inlabel(string, instanceof, Pproperty, lang, langsorder)
#' Search Wikidata entities matching whole words in any position in label and
#' altLabel. Diacritics and case are ignored. If lang has a value (es, en...)
#' then the search is only in that language, otherwise any.
#' @param string string (label or altLabel) to search.
#' @param instanceof Wikidata entity of which the entities searched for are an
#' example or member of it (class). For example, if instanceof=Q5 the
#' search are filtered to Wikidata entities of class "Q5 = human".
#' @param Pproperty Wikidata properties, separated with '|', to optionally
#' include in the search. For example, is Pproperty="P21", the results
#' include information of the sex of entities found as STRING.
#' @param lang the language to search. If lang="", search is in any language.
#' @param langsorder Order of languages in which the information will be returned.
#' @return: A data-frame with 'entity', 'entityLabel', 'entityDescription' and,
#' additionally the properties of Pproperty.
#' @author Angel Zazo, Department of Computer Science and Automatics, University of Salamanca
#' @export
WDQS_SearchByLabel_Inlabel <- function(string, instanceof="", Pproperty="",
                                       lang="", langsorder='es|en|ca|eu|gl|pt|fr|it|de|ru|ko|ja|zh') {
  #
  string <- gsub("'", "\\'", string, fixed = T)
  filterq <- if (instanceof != "") TRUE else FALSE
  searchp <- if (Pproperty != "") TRUE else FALSE
  langsorderw <- gsub("|", ",", langsorder, fixed = T)
  #
  if (lang != "")
    string <- paste0(string, '@', lang)
  #
  searchlang <- paste0('\nSERVICE wikibase:label {bd:serviceParam wikibase:language "', langsorderw, '".\n',
                       ' ?instanc rdfs:label ?instancLabel.')
  #
  if (searchp) {
    group_concat <- character()
    search <- character()
    for (p in strsplit(Pproperty, '|', fixed = T)[[1]]) {
      group_concat <- append(group_concat, paste0("(GROUP_CONCAT(DISTINCT STR(?", p ,"Label);separator='|') as ?",p,")"))
      search <- append(search, paste0('OPTIONAL {?entity wdt:', p, ' ?', p, '.}'))
      searchlang <- append(searchlang, paste0(' ?', p,' rdfs:label ?', p, 'Label.'))
    }
    group_concat <- paste(group_concat, collapse = "\n")
    search <- paste0(search, collapse = "\n")
    searchlang <- paste0(searchlang, collapse = "\n")
  }
  #
  query <- paste0("SELECT DISTINCT ?entity ?entityLabel ?entityDescription
(GROUP_CONCAT(DISTINCT ?instanc; separator='|') as ?instance)
(GROUP_CONCAT(DISTINCT ?instancLabel; separator='|') as ?instanceLabel)
(GROUP_CONCAT(DISTINCT STR(?altlabel);separator='|') as ?altLabel)\n",
if (searchp) group_concat else "",
'\nWHERE {
 SERVICE wikibase:mwapi {
  bd:serviceParam wikibase:api "Search";
                  wikibase:endpoint "www.wikidata.org";
                  mwapi:srsearch \'inlabel:"', string, '"\'.
  ?entity wikibase:apiOutputItem mwapi:title.
 }
 SERVICE wikibase:label {bd:serviceParam wikibase:language \'', langsorderw, "'}",
"\n?entity wdt:P31 ?instanc.\n",
if (filterq) paste0('FILTER(?instanc = wd:',instanceof,').\n') else "",
"OPTIONAL {?entity skos:altLabel ?altlabel}\n",
if (searchp) paste0(search, searchlang, '}') else "",
"\n} GROUP BY ?entity ?entityLabel ?entityDescription")
  #
  # cat(query)
  #
  r <- WDQS_query(query, format = "csv")
  for (c in c('entity',  'instance'))
    r[[c]] <- gsub('http://www.wikidata.org/entity/', '', r[[c]])
  rownames(r) <- r$entity
  return(r)
}

#' WDQS_Property(entity_list, Pproperty, langsorder)
#' Search the entities of the entity_list for property or properties. Return the
#' properties in langsorder order. Duplicated entities are deleted before search.
#' @param entity_list A vector with de Wikidata entities.
#' @param Pproperty Wikidata properties to search, separated with '|'. For
#' example, is Pproperty="P21", the results contain information of the sex
#' of entities. If Pproperty="P21|P569" also searches for birthdate. If
#' Pproperty='P21|P569|P214' also searches for VIAF identifier.
#' @param langsorder Order of languages in which the information will be returned.
#' @return: A data-frame with 'entity', 'entityLabel', 'entityDescription' and,
#' additionally, the properties of Pproperty. Index of the data-frame is also
#' set to entity_list.
#' @author Angel Zazo, Department of Computer Science and Automatics, University of Salamanca
#' @export
WDQS_Property <- function(entity_list, Pproperty,
                          langsorder = 'es|en|ca|eu|gl|pt|fr|it|de|ru|ko|ja|zh') {
  #
  if (Pproperty == "")
      stop(paste0("ERROR: Pproperty is mandatory."))
  langsorderw <- gsub("|", ",", langsorder, fixed = T)
  #
  searchlang <- paste0('\n SERVICE wikibase:label {bd:serviceParam wikibase:language "', langsorderw, '".')
  #
  group_concat <- character()
  search <- character()
  for (p in strsplit(Pproperty, '|', fixed = T)[[1]]) {
    group_concat <- append(group_concat, paste0("(GROUP_CONCAT(DISTINCT STR(?", p ,"Label);separator='|') as ?",p,")"))
    search <- append(search, paste0(' OPTIONAL {?entity wdt:', p, ' ?', p, '.}'))
    searchlang <- append(searchlang, paste0('  ?', p,' rdfs:label ?', p, 'Label.'))
  }
  group_concat <- paste(group_concat, collapse = "\n")
  search <- paste0(search, collapse = "\n")
  searchlang <- paste0(searchlang, collapse = "\n")
  #
  entity_list <- unique(entity_list)
  values <- paste0("wd:",paste0(entity_list, collapse = ' wd:'))
  query <- paste0("SELECT DISTINCT ?entity\n",
group_concat,
"\nWHERE {
 VALUES ?entity {",values,"}",
paste0(search, searchlang, '}'),
"\n} GROUP BY ?entity")
  #
  # cat(query)
  #
  r <- WDQS_query(query, format = "csv", method = 'POST')
  r$entity   <- gsub('http://www.wikidata.org/entity/', '', r$entity)
  rownames(r) <- r$entity
  return(r)
}

#' WDQS_SearchByAuthority(authority, instanceof, langsorder)
#' Search for Wikidata entities that have an identifier in the Wikidata
#' authority class "authority". Return the properties in the language order
#' indicated in langsorder. If instanceof has a value, then response is limited
#' to entities which are instance of it.
#'
#' @param authority Wikidata identifier class for authority. For example, is
#' authority = "P4439", then search Wikidata entities that have an identifier
#' in the MNCARS (Museo Nacional Centro de Arte Reina Sofía) database.
#' @param instanceof Wikidata entity of which the entities searched for are an
#' example or member of it (class). For example, if instanceof=Q5 the
#' search are filtered to Wikidata entities of class "Q5 = human".
#' @param langsorder Order of languages in which the information will be returned.
#' @return: A data-frame with 'entity', 'entityLabel', 'entityDescription' and,
#' additionally the properties of Pproperty.
#' @author Angel Zazo, Department of Computer Science and Automatics, University of Salamanca
#' @export
WDQS_SearchByAuthority <- function(authority, instanceof = "",
                                   langsorder='es|en|ca|eu|gl|pt|fr|it|de|ru|ko|ja|zh') {
  #
  if (authority == "")
    stop(paste0("ERROR: authority is mandatory."))
  #
  filterq <- if (instanceof != "") TRUE else FALSE
  langsorderw <- gsub("|", ",", langsorder, fixed = T)
  #
  query <- paste0("SELECT DISTINCT ?entity ?entityLabel ?entityDescription
(GROUP_CONCAT(DISTINCT STR(?authid);separator='|') as ?", authority, ")
WHERE {
  ?entity wdt:", authority," ?authid.\n",
if (filterq) paste0('  ?entity wdt:P31 wd:',instanceof,'.\n') else "",
'  SERVICE wikibase:label {bd:serviceParam wikibase:language "', langsorderw, '"}
} GROUP BY ?entity ?entityLabel ?entityDescription')
  #cat(query)
  #
  r <- WDQS_query(query, format = "csv", method = 'POST')
  r$entity   <- gsub('http://www.wikidata.org/entity/', '', r$entity)
  rownames(r) <- r$entity
  return(r)
}

#' WDQS_EntityInfo(entity, langs, wikilangs)
#' Gets some properties of the Wikidata "entity" related to birth and death
#' dates, places, occupations, works, education, awards, identificator in some
#' libraries, wikipedia pages (limit languages to wikilangs), etc.
#' @param entity Wikidata entity to search for properties.
#' @param langs Language order of the "SERVICE wikibase:label", mandatory.
#' Note: sometimes not label in any language of langs is assigned to a entity,
#' so an additional search is used to obtain almost one label for it
#' (?entitylab) with LIMIT 1.
#' @param wikilangs only return the Wikipedia pages of the entity in those
#' languages and in its order. If wikilangs="" retrieve all wikipedias pages,
#' not sorted.
#' @return: A data-frame with all properties of entity.
#' @author Angel Zazo, Department of Computer Science and Automatics, University of Salamanca
#' @export
WDQS_EntityInfo <- function(entity, langs = 'es|en|ca|eu|gl|pt|fr|it|de|ru|ko|ja|zh',
                             wikilangs = 'es|en|ca|eu|gl|pt|fr|it|de|ru|ko|ja|zh') {
  #
  langs     <- gsub("|", ",", langs, fixed = T)
  if (wikilangs != "") {
    wikiorder  <- strsplit(wikilangs, '|', fixed = T)[[1]]
    wikifilter <- paste0("'",gsub('|', "','", wikilangs, fixed=T), "'")
  }
  #
  qid <- entity
  query <- paste0('SELECT DISTINCT ?entity ?entityLabel (LANG(?entitylab) as ?entitylablang) ?entitylab
  ?entityDescription (LANG(?entitydesc) as ?entitydesclang) ?entitydesc
  ?bdate (YEAR(?bdate) AS ?byear) ?bplaceLabel ?bactualplaceLabel ?bplaceCoord ?bcountryLabel
  ?ddate (YEAR(?ddate) AS ?dyear) ?dplaceLabel ?dactualplaceLabel ?dplaceCoord ?dcountryLabel
  (GROUP_CONCAT(DISTINCT ?sxLabel;separator="|") as ?gender)
  (GROUP_CONCAT(DISTINCT ?pic;separator="|")     as ?pics)
  (GROUP_CONCAT(DISTINCT ?eaLabel;separator="|") as ?educatedat)
  (GROUP_CONCAT(DISTINCT ?ea;separator="|")      as ?educatedatQ)
  (GROUP_CONCAT(DISTINCT ?ocLabel;separator="|") as ?occupation)
  (GROUP_CONCAT(DISTINCT ?oc;separator="|")      as ?occupationQ)
  (GROUP_CONCAT(DISTINCT ?fwLabel;separator="|") as ?fieldofwork)
  (GROUP_CONCAT(DISTINCT ?fw;separator="|")      as ?fieldofworkQ)
  (GROUP_CONCAT(DISTINCT ?moLabel;separator="|") as ?movement)
  (GROUP_CONCAT(DISTINCT ?mo;separator="|")      as ?movementQ)
  (GROUP_CONCAT(DISTINCT ?geLabel;separator="|") as ?genres)
  (GROUP_CONCAT(DISTINCT ?ge;separator="|")      as ?genresQ)
  (GROUP_CONCAT(DISTINCT ?inLabel;separator="|") as ?influencedby)
  (GROUP_CONCAT(DISTINCT ?in;separator="|")      as ?influencedbyQ)
  (GROUP_CONCAT(DISTINCT ?noLabel;separator="|") as ?notablework)
  (GROUP_CONCAT(DISTINCT ?no;separator="|")      as ?notableworkQ)
  (GROUP_CONCAT(DISTINCT ?meLabel;separator="|") as ?memberof)
  (GROUP_CONCAT(DISTINCT ?me;separator="|")      as ?memberofQ)
  (GROUP_CONCAT(DISTINCT ?awLabel;separator="|") as ?award)
  (GROUP_CONCAT(DISTINCT ?aw;separator="|")      as ?awardQ)
  (GROUP_CONCAT(DISTINCT ?viafid;separator="|")  as ?viaf)
  (GROUP_CONCAT(DISTINCT ?bneid;separator="|")   as ?bne)
  (GROUP_CONCAT(DISTINCT ?mncarsid;separator="|") as ?mncars)
  (GROUP_CONCAT(DISTINCT ?lang;separator="|")    as ?wikilangs)
  (GROUP_CONCAT(DISTINCT ?page;separator="|")    as ?wikipedias)
WHERE {
  BIND(wd:', qid, " AS ?entity)
  SERVICE wikibase:label {bd:serviceParam wikibase:language \"",langs,"\"}
  OPTIONAL {
    SELECT ?entitylab WHERE {wd:", qid, " rdfs:label ?entitylab.} LIMIT 1
  }
  OPTIONAL {
    SELECT ?entitydesc WHERE {wd:", qid, " schema:description ?entitydesc.} LIMIT 1
  }
  OPTIONAL
  {
    SELECT ?bdate (COUNT(?refP569) AS ?cP569)
    WHERE {
      wd:", qid, " wdt:P569 ?bdate.
      OPTIONAL {wd:", qid, " p:P569 [ps:P569 ?bdate; prov:wasDerivedFrom [(pr:P248|pr:P854|pr:P143) ?refP569]].}
    } GROUP BY ?bdate ORDER BY DESC(?cP569) LIMIT 1
  }
  OPTIONAL
  {
    SELECT ?bplace ?bactualplace ?bplaceCoord ?bcountry ?starttime1 (COUNT(?refP19) AS ?cP19)
    WHERE {
      wd:", qid, " wdt:P19 ?bplace.
      OPTIONAL {wd:", qid, " wdt:P19 ?bplace.
                ?bplace wdt:P1366 ?bplacelast.}
      BIND(COALESCE(?bplacelast, ?bplace) AS ?bactualplace).
      OPTIONAL {?bactualplace wdt:P625 ?bplaceCoord.
                ?bactualplace wdt:P17  ?bcountry.}
      OPTIONAL {wd:", qid, " p:P19 [ps:P19 ?bplace; prov:wasDerivedFrom [(pr:P248|pr:P854|pr:P143) ?refP19]].}
    } GROUP BY ?bplace ?bactualplace ?bplaceCoord ?bcountry ?starttime1
      ORDER BY DESC(?cP19) DESC(?starttime1) LIMIT 1
  }
  OPTIONAL
  {
    SELECT ?ddate (COUNT(?refP570) AS ?cP570)
    WHERE {
      wd:", qid, " wdt:P570 ?ddate.
      OPTIONAL {wd:", qid, " p:P570 [ps:P570 ?ddate; prov:wasDerivedFrom [(pr:P248|pr:P854|pr:P143) ?refP570]].}
    } GROUP BY ?ddate ORDER BY DESC(?cP570) LIMIT 1
  }
  OPTIONAL
  {
    SELECT ?dplace ?dactualplace ?dplaceCoord ?dcountry ?starttime2 (COUNT(?refP20) AS ?cP20)
    WHERE {
      wd:", qid, " wdt:P20 ?dplace.
      OPTIONAL {wd:", qid, " wdt:P20 ?dplace.
                ?dplace wdt:P1366 ?dplacelast.}
      BIND(COALESCE(?dplacelast, ?dplace) AS ?dactualplace).
      OPTIONAL {?dactualplace wdt:P625 ?dplaceCoord.
                ?dactualplace wdt:P17  ?dcountry.}
      OPTIONAL {wd:", qid, " p:P20 [ps:P20 ?dplace; prov:wasDerivedFrom [(pr:P248|pr:P854|pr:P143) ?refP20]].}
    } GROUP BY ?dplace ?dactualplace ?dplaceCoord ?dcountry ?starttime2
      ORDER BY DESC(?cP20) DESC(?starttime2)
      LIMIT 1
  }
  OPTIONAL {wd:", qid, " wdt:P18  ?pic.}
  OPTIONAL {wd:", qid, " wdt:P21  ?sx.}
  OPTIONAL {wd:", qid, " wdt:P69  ?ea.}
  OPTIONAL {wd:", qid, " wdt:P106 ?oc.}
  OPTIONAL {wd:", qid, " wdt:P101 ?fw.}
  OPTIONAL {wd:", qid, " wdt:P135 ?mo.}
  OPTIONAL {wd:", qid, " wdt:P136 ?ge.}
  OPTIONAL {wd:", qid, " wdt:P737 ?in.}
  OPTIONAL {wd:", qid, " wdt:P800 ?no.}
  OPTIONAL {wd:", qid, " wdt:P463 ?me.}
  OPTIONAL {wd:", qid, " wdt:P166 ?aw.}
  SERVICE wikibase:label {bd:serviceParam wikibase:language \"", langs, "\".
    ?sx rdfs:label ?sxLabel.
    ?ea rdfs:label ?eaLabel.
    ?oc rdfs:label ?ocLabel.
    ?fw rdfs:label ?fwLabel.
    ?mo rdfs:label ?moLabel.
    ?ge rdfs:label ?geLabel.
    ?in rdfs:label ?inLabel.
    ?no rdfs:label ?noLabel.
    ?me rdfs:label ?meLabel.
    ?aw rdfs:label ?awLabel.
  }
  OPTIONAL {wd:", qid, " wdt:P214 ?viafid.}
  OPTIONAL {wd:", qid, " wdt:P950 ?bneid.}
  OPTIONAL {wd:", qid, " wdt:P4439 ?mncarsid.}
  OPTIONAL {?page schema:about wd:", qid, ";
                    # schema:name ?name;
                    schema:isPartOf [ wikibase:wikiGroup \"wikipedia\" ]",
if (wikilangs != '') paste0(";
                    schema:inLanguage ?lang.
            FILTER(?lang in (",wikifilter,")).") else ".",
"}\n",
"} GROUP BY ?entity ?entityLabel ?entitylab ?entityDescription ?entitydesc ?sexLabel
  ?bdate ?bplaceLabel ?bactualplaceLabel ?bplaceCoord ?bcountryLabel
  ?ddate ?dplaceLabel ?dactualplaceLabel ?dplaceCoord ?dcountryLabel")
  #
  # cat(query)
  #
  r <- WDQS_query(query, format = "csv", method = 'POST')
  # Eliminate 'http://www.wikidata.org/entity/' in columns that ends with 'Q'
  forgsub <- append(c('entity'), colnames(r)[endsWith(colnames(r), 'Q')])
  for (c in forgsub)
    r[[c]] <- gsub('http://www.wikidata.org/entity/', '', r[[c]])
  #
  rownames(r) <- r$entity
  # Order by wikilangs
  for (entity in r$entity) {
    if (wikilangs != "") {
      l <- strsplit(r[entity, 'wikilangs'], '|', fixed = T)[[1]]
      if (length(l) > 1) {
        o <- match(wikiorder, l)
        o <- o[!is.na(o)]
        r[entity, "wikilangs"] <- paste0(l[o], collapse = '|')
        l <- strsplit(r[entity, 'wikipedias'], '|', fixed = T)[[1]]
        r[entity, "wikipedias"] <- paste0(l[o], collapse = '|')
      }
    }
  }
  #
  return(r)
}

#' WDQS_EntityInfo_tiny(entity, langs, wikilangs)
#' Same as WDQS_EntityInfo function but less properties are requested and
#' less checks are done.
#' Gets some properties of the Wikidata "entity" related to birth and death
#' dates, places, occupations, works, education, awards, identificator in some
#' libraries, wikipedia pages (only languages in wikilangs parameter), etc.
#' @param entity Wikidata entity to search for properties.
#' @param langs Language order of the "SERVICE wikibase:label", mandatory.
#' Note: sometimes not label in any language of langs is assigned to a entity,
#' so an additional search is used to obtain almost one label for it
#' (?entitylab) with LIMIT 1.
#' @param wikilangs only return the Wikipedia pages of the entity in those
#' languages and in its order. If wikilangs="" retrieve all wikipedias pages,
#' not sorted.
#' @return: A data-frame with all properties of entity
#' @author Angel Zazo, Department of Computer Science and Automatics, University of Salamanca
#' @export
WDQS_EntityInfo_tiny <- function(entity, langs = 'es|en|ca|eu|gl|pt|fr|it|de|ru|ko|ja|zh',
                                 wikilangs = 'es|en|ca|eu|gl|pt|fr|it|de|ru|ko|ja|zh') {
  #
  langs     <- gsub("|", ",", langs, fixed = T)
  if (wikilangs != "") {
    wikiorder  <- strsplit(wikilangs, '|', fixed = T)[[1]]
    wikifilter <- paste0("'",gsub('|', "','", wikilangs, fixed=T), "'")
  }
  #
  qid <- entity
  query <- paste0('SELECT DISTINCT ?entity ?entityLabel (LANG(?entitylab) as ?entitylablang) ?entitylab
  ?entityDescription (LANG(?entitydesc) as ?entitydesclang) ?entitydesc
  ?bdate (YEAR(?bdate) AS ?byear) ?bplaceLabel ?bplaceCoord ?bcountryLabel
  ?ddate (YEAR(?ddate) AS ?dyear) ?dplaceLabel ?dplaceCoord ?dcountryLabel
  (GROUP_CONCAT(DISTINCT ?sxLabel;separator="|") as ?gender)
  (GROUP_CONCAT(DISTINCT ?pic;separator="|")     as ?pics)
  # (GROUP_CONCAT(DISTINCT ?eaLabel;separator="|") as ?educatedat)
  # (GROUP_CONCAT(DISTINCT ?ea;separator="|")      as ?educatedatQ)
  (GROUP_CONCAT(DISTINCT ?ocLabel;separator="|") as ?occupation)
  (GROUP_CONCAT(DISTINCT ?oc;separator="|")      as ?occupationQ)
  # (GROUP_CONCAT(DISTINCT ?fwLabel;separator="|") as ?fieldofwork)
  # (GROUP_CONCAT(DISTINCT ?fw;separator="|")      as ?fieldofworkQ)
  # (GROUP_CONCAT(DISTINCT ?moLabel;separator="|") as ?movement)
  # (GROUP_CONCAT(DISTINCT ?mo;separator="|")      as ?movementQ)
  # (GROUP_CONCAT(DISTINCT ?geLabel;separator="|") as ?genres)
  # (GROUP_CONCAT(DISTINCT ?ge;separator="|")      as ?genresQ)
  # (GROUP_CONCAT(DISTINCT ?inLabel;separator="|") as ?influencedby)
  # (GROUP_CONCAT(DISTINCT ?in;separator="|")      as ?influencedbyQ)
  # (GROUP_CONCAT(DISTINCT ?noLabel;separator="|") as ?notablework)
  # (GROUP_CONCAT(DISTINCT ?no;separator="|")      as ?notableworkQ)
  # (GROUP_CONCAT(DISTINCT ?meLabel;separator="|") as ?memberof)
  # (GROUP_CONCAT(DISTINCT ?me;separator="|")      as ?memberofQ)
  # (GROUP_CONCAT(DISTINCT ?awLabel;separator="|") as ?award)
  # (GROUP_CONCAT(DISTINCT ?aw;separator="|")      as ?awardQ)
  (GROUP_CONCAT(DISTINCT ?viafid;separator="|")  as ?viaf)
  (GROUP_CONCAT(DISTINCT ?bneid;separator="|")   as ?bne)
  (GROUP_CONCAT(DISTINCT ?mncarsid;separator="|") as ?mncars)
  (GROUP_CONCAT(DISTINCT ?lang;separator="|")    as ?wikilangs)
  (GROUP_CONCAT(DISTINCT ?page;separator="|")    as ?wikipedias)
WHERE {
  BIND(wd:', qid, " AS ?entity)
  SERVICE wikibase:label {bd:serviceParam wikibase:language \"",langs,"\"}
  OPTIONAL {
    SELECT ?entitylab WHERE {wd:", qid, " rdfs:label ?entitylab.} LIMIT 1
  }
  OPTIONAL {
    SELECT ?entitydesc WHERE {wd:", qid, " schema:description ?entitydesc.} LIMIT 1
  }
  OPTIONAL
  {
    SELECT ?bdate WHERE {wd:", qid, " wdt:P569 ?bdate.} LIMIT 1
  }
  OPTIONAL
  {
    SELECT ?bplace ?bplaceCoord ?bcountry
    WHERE {
      wd:", qid, " wdt:P19 ?bplace.
      OPTIONAL {?bplace wdt:P625 ?bplaceCoord.}
      OPTIONAL {?bplace wdt:P17  ?bcountry.}
    } LIMIT 1
  }
  OPTIONAL
  {
    SELECT ?ddate WHERE {wd:", qid, " wdt:P570 ?ddate.} LIMIT 1
  }
  OPTIONAL
  {
    SELECT ?dplace ?dplaceCoord ?dcountry
    WHERE {
      wd:", qid, " wdt:P20 ?dplace.
      OPTIONAL {?dplace wdt:P625 ?dplaceCoord.}
      OPTIONAL {?dplace wdt:P17  ?dcountry.}
    } LIMIT 1
  }
  OPTIONAL {wd:", qid, " wdt:P18  ?pic.}
  OPTIONAL {wd:", qid, " wdt:P21  ?sx.}
  # OPTIONAL {wd:", qid, " wdt:P69  ?ea.}
  OPTIONAL {wd:", qid, " wdt:P106 ?oc.}
  # OPTIONAL {wd:", qid, " wdt:P101 ?fw.}
  # OPTIONAL {wd:", qid, " wdt:P135 ?mo.}
  # OPTIONAL {wd:", qid, " wdt:P136 ?ge.}
  # OPTIONAL {wd:", qid, " wdt:P737 ?in.}
  # OPTIONAL {wd:", qid, " wdt:P800 ?no.}
  # OPTIONAL {wd:", qid, " wdt:P463 ?me.}
  # OPTIONAL {wd:", qid, " wdt:P166 ?aw.}
  SERVICE wikibase:label {bd:serviceParam wikibase:language \"", langs, "\".
    ?sx rdfs:label ?sxLabel.
    # ?ea rdfs:label ?eaLabel.
    ?oc rdfs:label ?ocLabel.
    # ?fw rdfs:label ?fwLabel.
    # ?mo rdfs:label ?moLabel.
    # ?ge rdfs:label ?geLabel.
    # ?in rdfs:label ?inLabel.
    # ?no rdfs:label ?noLabel.
    # ?me rdfs:label ?meLabel.
    # ?aw rdfs:label ?awLabel.
  }
  OPTIONAL {wd:", qid, " wdt:P214 ?viafid.}
  OPTIONAL {wd:", qid, " wdt:P950 ?bneid.}
  OPTIONAL {wd:", qid, " wdt:P4439 ?mncarsid.}
  OPTIONAL {?page schema:about wd:", qid, ";
                    # schema:name ?name;
                    schema:isPartOf [ wikibase:wikiGroup \"wikipedia\" ]",
                  if (wikilangs != '') paste0(";
                    schema:inLanguage ?lang.
            FILTER(?lang in (",wikifilter,")).") else ".",
                  "}\n",
                  "} GROUP BY ?entity ?entityLabel ?entitylab ?entityDescription ?entitydesc ?sexLabel
  ?bdate ?bplaceLabel ?bplaceCoord ?bcountryLabel
  ?ddate ?dplaceLabel ?dplaceCoord ?dcountryLabel")
  # cat(query)
  #
  r <- WDQS_query(query, format = "csv", method = 'POST')
  # Eliminate 'http://www.wikidata.org/entity/' in columns that ends with 'Q'
  forgsub <- append(c('entity'), colnames(r)[endsWith(colnames(r), 'Q')])
  for (c in forgsub)
    r[[c]] <- gsub('http://www.wikidata.org/entity/', '', r[[c]])
  #
  rownames(r) <- r$entity
  # Order by wikilangs
  for (entity in r$entity) {
    if (wikilangs != "") {
      l <- strsplit(r[entity, 'wikilangs'], '|', fixed = T)[[1]]
      if (length(l) > 1) {
        o <- match(wikiorder, l)
        o <- o[!is.na(o)]
        r[entity, "wikilangs"] <- paste0(l[o], collapse = '|')
        l <- strsplit(r[entity, 'wikipedias'], '|', fixed = T)[[1]]
        r[entity, "wikipedias"] <- paste0(l[o], collapse = '|')
      }
    }
  }
  #
  return(r)
}

#' WDQS_FilmInfo(film_entity, langs, wikilangs)
#' Gets some properties of the Wikidata "film_entity" related to information
#' about that film entity.
#' @param film_entity Wikidata entity to search for properties.
#' @param langs Language order of the "SERVICE wikibase:label", mandatory.
#' Note: sometimes not label in any language of langs is assigned to a entity,
#' so an additional search is used to obtain almost one label for it
#' (?entitylab) with LIMIT 1.
#' @param wikilangs only return the Wikipedia pages of the entity in those
#' languages and in its order. If wikilangs="" retrieve all wikipedias pages,
#' not sorted.
#' @return: A data-frame with all properties of film_entity
#' @author Angel Zazo, Department of Computer Science and Automatics, University of Salamanca
#' @export
WDQS_FilmInfo <- function(film_entity,
                          langs = 'es|en|ca|eu|gl|pt|fr|it|de|ru|ko|ja|zh',
                          wikilangs = 'es|en|ca|eu|gl|pt|fr|it|de|ru|ko|ja|zh') {
  #
  langs     <- gsub("|", ",", langs, fixed = T)
  if (wikilangs != "") {
    wikiorder  <- strsplit(wikilangs, '|', fixed = T)[[1]]
    wikifilter <- paste0("'",gsub('|', "','", wikilangs, fixed=T), "'")
  }
  #
  qid <- film_entity
  query <- paste0('SELECT DISTINCT ?entity ?entityLabel ?entityLabelES # ?entityDescription ?pubdate
  (YEAR(?pubdate) AS ?pubyear)
  (GROUP_CONCAT(DISTINCT ?pos;separator="|") as ?poster)
  (GROUP_CONCAT(DISTINCT ?pic;separator="|") as ?pics)
  (GROUP_CONCAT(DISTINCT ?vid;separator="|") as ?video)
  (GROUP_CONCAT(DISTINCT ?dur;separator="|") as ?duration)
  (GROUP_CONCAT(DISTINCT ?titLabel;separator="|") as ?title)
  # (GROUP_CONCAT(DISTINCT ?bonLabel;separator="|") as ?baseon)
  # (GROUP_CONCAT(DISTINCT ?bon;separator="|")      as ?baseonQ)
  # (GROUP_CONCAT(DISTINCT ?movLabel;separator="|") as ?movement)
  # (GROUP_CONCAT(DISTINCT ?mov;separator="|")      as ?movementQ)
  (GROUP_CONCAT(DISTINCT ?genLabel;separator="|") as ?genres)
  (GROUP_CONCAT(DISTINCT ?gen;separator="|")      as ?genresQ)
  (GROUP_CONCAT(DISTINCT ?couLabel;separator="|") as ?country)
  (GROUP_CONCAT(DISTINCT ?cou;separator="|")      as ?countryQ)
  (GROUP_CONCAT(DISTINCT ?olaLabel;separator="|") as ?originallanguage)
  (GROUP_CONCAT(DISTINCT ?ola;separator="|")      as ?originallanguageQ)
  (GROUP_CONCAT(DISTINCT ?dirLabel;separator="|") as ?director)
  (GROUP_CONCAT(DISTINCT ?dir;separator="|")      as ?directorQ)
  (GROUP_CONCAT(DISTINCT ?scwLabel;separator="|") as ?screenwriter)
  (GROUP_CONCAT(DISTINCT ?scw;separator="|")      as ?screenwriterQ)
  (GROUP_CONCAT(DISTINCT ?casLabel;separator="|") as ?castmember)
  (GROUP_CONCAT(DISTINCT ?cas;separator="|")      as ?castmemberQ)
  # (GROUP_CONCAT(DISTINCT ?eprLabel;separator="|") as ?executiveproducer)
  # (GROUP_CONCAT(DISTINCT ?epr;separator="|")      as ?executiveproducerQ)
  # (GROUP_CONCAT(DISTINCT ?pdiLabel;separator="|") as ?photographdirector)
  # (GROUP_CONCAT(DISTINCT ?pdi;separator="|")      as ?photographdirectorQ)
  # (GROUP_CONCAT(DISTINCT ?fedLabel;separator="|") as ?filmeditor)
  # (GROUP_CONCAT(DISTINCT ?fed;separator="|")      as ?filmeditorQ)
  # (GROUP_CONCAT(DISTINCT ?pdeLabel;separator="|") as ?productiondesigner)
  # (GROUP_CONCAT(DISTINCT ?pde;separator="|")      as ?productiondesignerQ)
  (GROUP_CONCAT(DISTINCT ?comLabel;separator="|") as ?composer)
  (GROUP_CONCAT(DISTINCT ?com;separator="|")      as ?composerQ)
  (GROUP_CONCAT(DISTINCT ?proLabel;separator="|") as ?producer)
  (GROUP_CONCAT(DISTINCT ?pro;separator="|")      as ?producerQ)
  # (GROUP_CONCAT(DISTINCT ?pcoLabel;separator="|") as ?productioncompany)
  # (GROUP_CONCAT(DISTINCT ?pco;separator="|")      as ?productioncompanyQ)
  # (GROUP_CONCAT(DISTINCT ?colLabel;separator="|") as ?color)
  # (GROUP_CONCAT(DISTINCT ?col;separator="|")      as ?colorQ)
  # (GROUP_CONCAT(DISTINCT ?depLabel;separator="|") as ?depicts)
  # (GROUP_CONCAT(DISTINCT ?dep;separator="|") as ?depictsQ)
  (GROUP_CONCAT(DISTINCT ?msuLabel;separator="|") as ?mainsubject)
  (GROUP_CONCAT(DISTINCT ?msu;separator="|") as ?mainsubjectQ)
  # (GROUP_CONCAT(DISTINCT ?awrLabel;separator="|") as ?award)
  # (GROUP_CONCAT(DISTINCT ?awr;separator="|") as ?awardQ)
  # (GROUP_CONCAT(DISTINCT ?rev;separator="|") as ?reviewscore)
  (GROUP_CONCAT(DISTINCT ?lang;separator="|") as ?wikilangs)
  (GROUP_CONCAT(DISTINCT ?page;separator="|") as ?wikipedias)
WHERE {
  BIND(wd:', qid, ' AS ?entity)
  SERVICE wikibase:label {bd:serviceParam wikibase:language "', langs, '"}
  OPTIONAL {wd:', qid, ' rdfs:label ?entityLabelES. FILTER(LANG(?entityLabelES)="es")}
  OPTIONAL {SELECT ?pubdate WHERE {wd:', qid, ' wdt:P577 ?pubdate.} LIMIT 1}
  OPTIONAL {wd:', qid, ' wdt:P3383  ?pos.} # poster
  OPTIONAL {wd:', qid, ' wdt:P18    ?pic.} # pic
  OPTIONAL {wd:', qid, ' wdt:P10    ?vid.} # video
  OPTIONAL {wd:', qid, ' wdt:P1476  ?tit.} # title
  OPTIONAL {wd:', qid, ' wdt:P2047  ?dur.} # duration
# OPTIONAL {wd:', qid, ' wdt:P144   ?bon.} # based on
# OPTIONAL {wd:', qid, ' wdt:P135   ?mov.} # movement
  OPTIONAL {wd:', qid, ' wdt:P136   ?gen.} # genre
  OPTIONAL {wd:', qid, ' wdt:P495   ?cou.} # country
  OPTIONAL {wd:', qid, ' wdt:P364   ?ola.} # originallanguage
  OPTIONAL {wd:', qid, ' wdt:P57    ?dir.} # director
  OPTIONAL {wd:', qid, ' wdt:P58    ?scw.} # screenwriter
  OPTIONAL {wd:', qid, ' wdt:P161   ?cas.} # castmember
# OPTIONAL {wd:', qid, ' wdt:P1431  ?epr.} # executiveproducer
# OPTIONAL {wd:', qid, ' wdt:P344   ?pdi.} # photographdirector
# OPTIONAL {wd:', qid, ' wdt:P1040  ?fed.} # filmeditor
# OPTIONAL {wd:', qid, ' wdt:P2554  ?pde.} # productiondesigner
  OPTIONAL {wd:', qid, ' wdt:P86    ?com.} # composer
  OPTIONAL {wd:', qid, ' wdt:P162   ?pro.} # producer
# OPTIONAL {wd:', qid, ' wdt:P272   ?pco.} # productioncompany
# OPTIONAL {wd:', qid, ' wdt:P462   ?col.} # color
# OPTIONAL {wd:', qid, ' wdt:P180   ?dep.} # depicts
  OPTIONAL {wd:', qid, ' wdt:P921   ?msu.} # mainsubject
# OPTIONAL {wd:', qid, ' wdt:P166   ?awr.} # award
# OPTIONAL {wd:', qid, ' wdt:P444   ?rev.} # reviewscore
  SERVICE wikibase:label {bd:serviceParam wikibase:language "', langs, '".
   ?tit rdfs:label ?titLabel.
#  ?bon rdfs:label ?bonLabel.
#  ?mov rdfs:label ?movLabel.
   ?gen rdfs:label ?genLabel.
   ?cou rdfs:label ?couLabel.
   ?ola rdfs:label ?olaLabel.
   ?dir rdfs:label ?dirLabel.
   ?scw rdfs:label ?scwLabel.
   ?cas rdfs:label ?casLabel.
#  ?epr rdfs:label ?eprLabel.
#  ?pdi rdfs:label ?pdiLabel.
#  ?fed rdfs:label ?fedLabel.
#  ?pde rdfs:label ?pdeLabel.
   ?com rdfs:label ?comLabel.
   ?pro rdfs:label ?proLabel.
#  ?pco rdfs:label ?pcoLabel.
#  ?col rdfs:label ?colLabel.
#  ?dep rdfs:label ?depLabel.
   ?msu rdfs:label ?msuLabel.
#  ?awr rdfs:label ?awrLabel.
  }
  OPTIONAL {?page schema:about wd:', qid, ';
                    # schema:name ?name;
                    schema:isPartOf [ wikibase:wikiGroup "wikipedia" ]',
if (wikilangs != '') paste0(";
                    schema:inLanguage ?lang.
            FILTER(?lang in (",wikifilter,")).") else ".",
"}\n",
"} GROUP BY  ?entity ?entityLabel ?entityLabelES ?pubdate # ?entityDescription ")
  # cat(query)
  #
  r <- WDQS_query(query, format = "csv", method = 'POST')

  forgsub <- append(c('entity'), colnames(r)[endsWith(colnames(r), 'Q')])
  # Eliminate 'http://www.wikidata.org/entity/' in columns
  for (c in forgsub)
    r[[c]] <- gsub('http://www.wikidata.org/entity/', '', r[[c]])
  #
  rownames(r) <- r$entity
  # Order by wikilangs
  for (entity in r$entity) {
    if (wikilangs != "") {
      l <- strsplit(r[entity, 'wikilangs'], '|', fixed = T)[[1]]
      if (length(l) > 1) {
        o <- match(wikiorder, l)
        o <- o[!is.na(o)]
        r[entity, "wikilangs"] <- paste0(l[o], collapse = '|')
        l <- strsplit(r[entity, 'wikipedias'], '|', fixed = T)[[1]]
        r[entity, "wikipedias"] <- paste0(l[o], collapse = '|')
      }
    }
  }
  #
  return(r)
}

# ----------------------------------------------------------------------------
# MediaWiki API
# See https://www.mediawiki.org/wiki/API:Main_page
# See https://en.wikipedia.org/w/api.php
# ----------------------------------------------------------------------------

#' MediaWiki_query
#' Uses httr package to retrieve responses using the MediaWiki API.
#' For MediaWiki requests only user_agent is necessary in the request headers.
#' See https://www.mediawiki.org/wiki/API:Etiquette. The standard and default
#' output format in MediaWiki is JSON. All other formats are discouraged. The
#' output format should always be specified using the request param "format"
#' in the "query" request.
#' See https://www.mediawiki.org/wiki/API:Data_formats#Output.
#' @param query A list with de (key, values) pairs with the search.
#' @note  Note that is titles is used in queries, the MediaWiki API has a limit
#' of 50 titles en each query. In that case a error response is achieved.
#' @param project The Wikimedia project to search. Default en.wikipedia.org.
#' @param method The method used in the httr request. Default 'GET'.
#' Note in "https://www.mediawiki.org/wiki/API:Etiquette#Request_limit":
#' Whenever you're reading data from the web service API, you should try to use
#' GET requests if possible, not POST, as the latter are not cacheable.
#' @param attempts On ratelimit errors, the number of times the request is
#' retried using a 60 seconds interval between retries. Default 2. If 0 no
#' retries are done.
#' @return The response in JSON format or NULL on errors.
#' @author Angel Zazo, Department of Computer Science and Automatics, University of Salamanca
#' @importFrom httr GET POST content add_headers stop_for_status
#' @importFrom jsonlite fromJSON
MediaWiki_query <- function(query, project='en.wikipedia.org',
                            method='GET', attempts=2) {

  if ('titles' %in% names(query)) {
    titles <- strsplit(query$titles, '|', fixed = T)[[1]]
    n <- length(titles)
    nlimit = 50
    if (n > nlimit) {
      cat(paste0("ERROR: The number of titles (", n,
                 ") exceeds Wikipedia API limit (",
                 nlimit,").\n"),
          file = stderr())
      return(NULL)
    }
  }
  #
  url = paste0('https://', project, "/w/api.php")
  nt <- 0
  tryCatch( {
    repeat {
      nt <- nt + 1
      if (method=='GET')
        r <- httr::GET(
          url = url,
          query = query,
          httr::user_agent(user_agent)
          )
      else if (method=='POST')
        r <- httr::POST(
          url = url,
          body = query,
          httr::add_headers(user_agent = user_agent),
          encode = 'form'
          )
      else stop(paste0("ERROR: method '", method, "' is not supported."))
      #
      # Converts http errors to R errors or warnings
      httr::stop_for_status(r)
      content <- httr::content(r, as = "text", encoding = "UTF-8")
      j <- jsonlite::fromJSON(content, simplifyVector = FALSE)
      #
      # See https://www.mediawiki.org/wiki/API:Etiquette#Request_limit
      if ( !is.null(j$error) && j$error$code == 'ratelimited') {
        if (nt > attempts)
          stop(paste0(as.character(nt)," ratelimited attempts achieved, aborting."))
        else {
          t <- 60*nt
          cat(paste0("INFO: ", nt," ratelimited attempt(s) error. Sleeping", as.character(t), "seconds.\n"),
              file = stderr())
          Sys.sleep(t)
        }
      }
      else {
        return(j)
      }
    }
  }, error = function(e){
    cat(as.character(e), file=stderr())
    return(NULL)
  } ) # End tryCatch
}

#' NormalizedTitle(title, q)
#' Return de normalized or redirect title from the JSON response of a MediaWiki
#' search that uses titles.
#' @param title the title to possibly found in q.
#' @param q (j$query) Query part of the JSON response from a Mediawiki search.
#' @return The normalized or redirect title found in q for title, else title
#' itself.
NormalizedTitle <- function(title, q) {
  # if ( !(jsonlite::validate(jsonlite::toJSON(q))) ) ## Very slow!!!
  #    stop("ERROR: j is not a valid JSON structure.")
  #
  anorm <- title
  # Is normalized (and possibly encoded)? # See https://phabricator.wikimedia.org/T29849#2594624
  if (!is.null(q$normalized)) {
    for (nn in q$normalized) {
      if (nn$fromencoded & URLencode(anorm) == nn$from)  # NFC normalization
        anorm <- nn$to
      if (nn$from == anorm)
        anorm <- nn$to
    }
  }
  # # ¿Is a redirect?
  if (!is.null(q$redirects)) {
    for (nn in q$redirects)
      if (nn$from == anorm)
        anorm <- nn$to
  }
  return(anorm)
}

#' CheckTitles(titles)
#' Check if titles are valid. Return TRUE is all titles are valid, else FALSE.
#' See:See https://en.wikipedia.org/wiki/Wikipedia:Page_name#Technical_restrictions_and_limitations
#' @param titles A vector of titles to check.
CheckTitles <- function(titles){
  if (length(titles) == 0)
    return(FALSE)
  #
  for (title in titles) { # Escaped REGEX: #<>[]|{} ==> #<>\\[\\]\\\{\\}
    if (grepl('[#<>\\[\\]|\\{\\}]', title)) {
      print(paste0('ERROR: the title ', title, ' has a not permited character'))
      return(FALSE)
    }
  }
  return(TRUE)
}

#' MW_Opensearch(string, project, profile, redirects)
#' Search y string in article title using OpenSearch. Only in namespace 0.
#' @param string String to search.
#' @param project Wikimedia project, defaults "en.wikipedio.org".
#' @param profile See: https://www.mediawiki.org/wiki/API:Opensearch. This
#' parameter sets the search type: strict, normal, fuzzy, fast-fuzzy, classic
#' and engine_autoselect (default).
#' @param redirects See: https://www.mediawiki.org/wiki/API:Opensearch. If
#' redirects='return', the title is the normalized one (also the url). If
#' redirets='resolve", the title is the normalized and resolved redirection
#' is in effect (also the url). Note that in both cases the API performs a NFC
#' Unicode normalization on search string.
#' @return A data-frame of titles and URL returned. If error, return Null.
#' @author Angel Zazo, Department of Computer Science and Automatics, University of Salamanca
#' @note Only for namespace 0. The function also obtains redirections for disambiguation pages.
#' @export
MW_Opensearch <- function(string, project='en.wikipedia.org',
                          profile="engine_autoselect", redirects="resolve") {
  #
  query = list(format        = 'json',
               formatversion = '2',
               action        = 'opensearch',
               namespace     = '0',
               limit         = 'max',  # "max" is 500
               profile       = profile,
               redirects     = redirects,
               search        = string)
  #
  j <- MediaWiki_query(query, project)
  #
  if (!is.null(j$error)) {
    cat(paste0('ERROR: ', j$error$code,': ', j$error$info,'\n'),
        file = stderr())
    return(NULL)
  }
  # NFC unicode normalization is carried out on string
  # if (j[[1]][1] != stringi::stri_trans_nfc(string)) {
  #   cat('ERROR: return string is different to the search string.\n',
  #       file = stderr())
  #   return(NULL)
  # }
  #
  output <- data.frame(title = unlist(j[[2]]), urls = unlist(j[[4]]))
  return(output)
}

#' MW_GetWikidataItem(titles, project, resolv_redirects)
#' Use MediaWiki_query to obtain the Wikidata entity of a article title from a Wikimedia project.
#' Automatically resolves redirects if parameter resolv_redirects = TRUE (default).
#' @param titles A vector of article titles to search for.
#' @param project Wikimedia project, defaults "en.wikipedia.org"
#' @param resolv_redirects If article redirects must be resolved (defaults TRUE)
#' @return Null if errors in response, else a data.frame with four columns:
#' first, the original article string, second, the normalized one, third,
#' logical FALSE or TRUE if the wikidata entity exists, last, the wikidata
#' entity itself or a clarification of the error.
#' @note It is not expected that there will be "continue" type responses,
#' nevertheless, their handling has been included in the code.
#' @author Angel Zazo, Department of Computer Science and Automatics, University of Salamanca
#' @examples
#' MW_GetWikidataItem('Max Planck', project='es.wikipedia.org')
#'
#' # Note that "a%CC%8C" is an "a" + combining caron.
#' MW_GetWikidataItem(c(URLdecode("a%CC%8C"), 'Max', 'Cervante', 'humanist'))
#' @export
MW_GetWikidataItem <- function(titles, project = 'en.wikipedia.org',
                               resolv_redirects = TRUE) {
  if (!CheckTitles(titles))
    return(NULL)
  #
  # Check the limit on number of titles of Wikidata API (50 titles)
  n <- length(titles)
  nlimit = 50
  if (n > nlimit) {
    cat(paste0("INFO: The number of titles (", n,") exceeds Wikipedia API limit (",
               nlimit,"): do chunked queries.\n"),
        file = stderr())
    nlim <- as.integer(n/nlimit)
    for (k in 0:nlim) {
      offset <- nlimit * k
      if ((offset+1) > n)
        break
      t_list <- titles[(offset+1):min(n,offset+nlimit)]
      cat(paste0(" INFO: get from ", offset+1," to ", offset+length(t_list), '.\n'),
          file = stderr())
      # print(t_list)
      aux <- MW_GetWikidataItem(t_list, project, resolv_redirects)
      #
      if (k==0)
        output <- aux
      else
        output <- rbind(output, aux)
    }
    return(output)
  }
  #
  query <- list(format        = 'json',
                formatversion = '2',
                action        = 'query',
                # redirects     = '1',
                prop          = 'pageprops',
                ppprop        = 'wikibase_item|disambiguation',
                titles        = paste0(titles, collapse = '|'))
  if (resolv_redirects)
    query["redirects"] = 1
  #
  # vectors for loading the return information
  normalized <- character()
  error   <- logical()
  entity  <- character()

  repeat {
    j <- MediaWiki_query(query, project)
    #
    if (is.null(j) | is.null(j$query)){
      cat("Error in MW_GetWikidataItem: MediaWiki_query returns an improper JSON.",
          file = stderr())
      return(NULL)
    }
     #
    q <- j$query
    #
    for (a in titles) {
      anorm <- NormalizedTitle(a, q)
      # Check if a different title with identical previously normalized title
      # is already in list. It's necessary for stability on number of elements.
      index <- match(anorm, normalized)
      if (!is.na(index)) {
        cat(paste0("   INFO: '",anorm,"' there was already in list.\n"), file = stderr())
        normalized <- append(normalized, anorm)
        error      <- append(error, error[index])
        entity     <- append(entity, entity[index])
        next
      }
      #
      # for (page in q$pages) {   With formatversion=2, j$json$query$pages is a vector
      for (i in 1:length(q$pages)) {
        page <- q$pages[[i]]
        if (anorm == page$title) {
          aerror <- TRUE
          if (!is.null(page$invalid))  # ¿malformed title?
            status <- 'invalid'
          else if (!is.null(page$missing))
            status <- 'missing'
          else if (is.null(page$pageprops))
            status <- 'no_pagepros'
          else if (!is.null(page$pageprops$disambiguation))
            status <- 'disambiguation'
          else if (is.null(page$pageprops$wikibase_item))
            status <- "no_wikibase_item"
          else {
            aerror <- FALSE
            status <- page$pageprops$wikibase_item
          }
          # Feed the output vectors
          normalized <- append(normalized, anorm)
          error      <- append(error, aerror)
          entity     <- append(entity, status)
          q$pages[[i]] <- NULL
          break
        }
      }
    }
    if (!is.null(j$continue)) {
      cat(paste("  INFO: continue response\n"), file = stderr())
      for (m in names(j$continue))
        query[[m]] <- j$continue[[m]]
    }
    else {
      output <- data.frame(original=titles, normalized=normalized,
                           error=error, entity=entity)    }
      return(output)
  }
}

#' MW_GetRedirects(titles, project)
#' Obtains the redirection pages (from namespace 0) to the article titles in
#' the Wikimedia project.
#' @param titles A vector of article titles to search for.
#' @param project Wikimedia project, defaults "en.wikipedio.org"
#' @return A list whose names are the titles searched and elements are all
#' redirections to the first element (which is the normalized title).
#' @author Angel Zazo, Department of Computer Science and Automatics, University of Salamanca
#' @note Only for namespace 0. The function also obtains redirections for disambiguation pages.
#' @export
MW_GetRedirects <- function(titles, project = "en.wikipedia.org") {
  if (!CheckTitles(titles))
    return(NULL)
  #
  # Check the limit on number of titles of Wikidata API (50 titles)
  n <- length(titles)
  nlimit = 50
  if (n > nlimit) {
    cat(paste0("INFO: The number of titles (", n,") exceeds Wikipedia API limit (",
               nlimit,"): do chunked queries.\n"),
        file = stderr())
    nlim <- as.integer(n/nlimit)
    for (k in 0:nlim) {
      offset <- nlimit * k
      if ((offset+1) > n)
        break
      t_list <- titles[(offset+1):min(n,offset+nlimit)]
      cat(paste0(" INFO: get from ", offset+1," to ", offset+length(t_list), '.\n'),
          file = stderr())
      # print(t_list)
      aux <- MW_GetRedirects(t_list, project)
      #
      if (k==0)
        output <- aux
      else
        output <- append(output, aux)
    }
    return(output)
  }
  #
  query = list(format        = 'json',
               formatversion = '2',
               action        = 'query',
               redirects     = '1',
               prop          = 'redirects',
               rdnamespace   = '0',             # Only from namespace = 0
               rdlimit       = 'max',           # max: Change to 10 for testing
               rdprop        = 'title',
               titles        = paste0(titles, collapse = '|'))
    # Get a list with list names but without list entries.
  output <- sapply(titles, function(x) NULL)
  #
  repeat {
    j <- MediaWiki_query(query, project)
    #
    if (is.null(j) | is.null(j$query)){
      cat("Error in MW_GetRedirects: MediaWiki_query returns an improper JSON.",
          file = stderr())
      return(NULL)
    }
    #
    r <- j$query
    #
    for (a in titles) {
      anorm <- NormalizedTitle(a, r)
      #  query is the original search query, before possible continue responses.
      # Any continue response includes the same "normalized" and "redirects" info
      if (is.null(query$continue))
        output[[a]] <- append(output[[a]], anorm)

      # With formatversion=2, j$json$query$redirects is a vector
      for (page in r$pages) {
        if (anorm == page$title) {
          if (!is.null(page$missing))   # missing, NULL
            output[[a]] <- NA
          if (!is.null(page$redirects))
            for (redirect in page$redirects)
              output[[a]] <- append(output[[a]], redirect$title)
        }
      }
    }
    if (!is.null(j$continue)) {
      cat(paste("  INFO: continue response\n"), file = stderr())
      for (m in names(j$continue))
        query[[m]] <- j$continue[[m]]
    }
    else
      return(output)
  }
}

#' MW_GetPrimaryImage(titles, project)
#' Return URL of image associated with the Wikipedia pages, if any.
#' See https://www.mediawiki.org/w/api.php?action=help&modules=query%2Bpageimages
#' @param titles A vector of article titles to search for.
#' @param project Wikimedia project, defaults "en.wikipedio.org".
#' @return A data-frame with original titles, normlized ones, logical TRUE or
#' FALSE and the primary image of wikipedia page or NA if does not exits.
#' @author Angel Zazo, Department of Computer Science and Automatics, University of Salamanca
#' @note Only for namespace 0.
#' @export
MW_GetPrimaryImage <- function(titles, project = "en.wikipedia.org") {
  if (!CheckTitles(titles))
    return(NULL)
  #
  # Check the limit on number of titles of Wikidata API (50 titles)
  n <- length(titles)
  nlimit = 50
  if (n > nlimit) {
    cat(paste0("INFO: The number of titles (", n,") exceeds Wikipedia API limit (",
               nlimit,"): do chunked queries.\n"),
        file = stderr())
    nlim <- as.integer(n/nlimit)
    for (k in 0:nlim) {
      offset <- nlimit * k
      if ((offset+1) > n)
        break
      t_list <- titles[(offset+1):min(n,offset+nlimit)]
      cat(paste0(" INFO: get from ", offset+1," to ", offset+length(t_list), '.\n'),
          file = stderr())
      # print(t_list)
      aux <- MW_GetPrimaryImage(t_list, project)
      #
      if (k==0)
        output <- aux
      else
        output <- rbind(output, aux)
    }
    return(output)
  }
  #
  query = list(format        = 'json',
               formatversion = '2',
               action        = 'query',
               redirects     = '1',
               prop          = 'pageimages',
               piprop        = 'original',
               pilimit       = 'max',           # max(=50): Change to 10 for testing
               titles        = paste0(titles, collapse = '|'))
    # vectors for loading the return information
  normalized <- character()
  error      <- logical()
  pageimage  <- character()
  #
  repeat {
    j <- MediaWiki_query(query, project)
    #
    if (is.null(j) | is.null(j$query)){
      cat("Error in MW_GetPrimaryImage: MediaWiki_query returns an improper JSON.",
          file = stderr())
      return(NULL)
    }
    #
    q <- j$query
    #
    for (a in titles) {
      anorm <- NormalizedTitle(a, q)
      # check if a normalized register is just loaded
      # If continue response, not overwrite previous search
      index <- match(anorm, normalized)
      if (!is.na(index)) {
        normalized <- append(normalized, anorm)
        error      <- append(error, error[index])
        pageimage  <- append(pageimage, pageimage[index])
        next
      }
      #
      # With formatversion=2, j$json$query$pages is a vector
      #for (page in q$pages) {
      for (i in 1:length(q$pages)) {
        page <- q$pages[[i]]
        if (anorm == page$title) {
          if (!is.null(page$invalid) | !is.null(page$missing)) { # malformed title or missing
            aerror <- TRUE
            image  <- NA
          }
          else if (is.null(page$original) | is.null(page$original$source)) {
            aerror <- FALSE
            image  <- NA
          }
          else {
            aerror <- FALSE
            image  <- page$original$source
          }
          # Feed the output variables
          normalized <- append(normalized, anorm)
          error      <- append(error, aerror)
          pageimage  <- append(pageimage, image)
          q$pages[[i]] <- NULL
          break
        }
      }
    }
    #
    if (!is.null(j$continue)) {
      cat(paste("  INFO: continue response\n"), file = stderr())
      for (m in names(j$continue))
        query[[m]] <- j$continue[[m]]
    }
    else {
      output <- data.frame(original=titles, normalized=normalized,
                           error=error, pageimage=pageimage)
      return(output)
    }
  }
}

#' MW_GetImages(titles, project, exclude_ext)
#'  Search for URL images (files) in the Wikipedia pages. Exclude extensions in
#'  exclude_ext. See https://en.wikipedia.org/w/api.php?action=help&modules=query%2Bimages
#' @param titles A vector of article titles to search for.
#' @param project Wikimedia project, defaults "en.wikipedio.org".
#' @param exclude_ext extensions excluded in results. Default 'svg|webp|xcf'
#' @return A data-frame with original titles, normalized ones, logical TRUE or
#' FALSE and the images of wikipedia page (use "|" to separate ones) or NA if
#' images do not exits.
#' @author Angel Zazo, Department of Computer Science and Automatics, University of Salamanca
#' @note Only for namespace 0.
#' @export
MW_GetImages <- function(titles, project = "en.wikipedia.org",
                         exclude_ext = 'svg|webp|xcf') {
  if (!CheckTitles(titles))
    return(NULL)
  #
  exts <- strsplit(tolower(exclude_ext), "|", fixed=T)[[1]]
  #
  # Check the limit on number of titles of Wikidata API (50 titles)
  n <- length(titles)
  nlimit = 50
  if (n > nlimit) {
    cat(paste0("INFO: The number of titles (", n,") exceeds Wikipedia API limit (",
               nlimit,"): do chunked queries.\n"),
        file = stderr())
    nlim <- as.integer(n/nlimit)
    for (k in 0:nlim) {
      offset <- nlimit * k
      if ((offset+1) > n)
        break
      t_list <- titles[(offset+1):min(n,offset+nlimit)]
      cat(paste0(" INFO: get from ", offset+1," to ", offset+length(t_list), '.\n'),
          file = stderr())
      # print(t_list)
      aux <- MW_GetImages(t_list, project, exclude_ext)
      #
      if (k==0)
        output <- aux
      else
        output <- rbind(output, aux)
    }
    return(output)
  }
  #
  query = list(format        = 'json',
               formatversion = '2',
               action        = 'query',
               redirects     = '1',
               prop          = 'images',
               imlimit       = 'max',           # max(=50): Change to 10 for testing
               titles        = paste0(titles, collapse = '|'))
  # vectors for loading the return information
  normalized <- character()
  error      <- logical()
  pageimages <- character()
  #
  repeat {
    j <- MediaWiki_query(query, project)
    #
    if (is.null(j) | is.null(j$query)){
      cat("Error in MW_GetImages: MediaWiki_query returns an improper JSON.",
          file = stderr())
      return(NULL)
    }
    #
    q <- j$query
    #
    for (a in titles) {
      anorm <- NormalizedTitle(a, q)
      # check if a normalized register is just loaded
      # If continue response, not overwrite previous search
      index <- match(anorm, normalized)
      if (!is.na(index)) {
        normalized <- append(normalized, anorm)
        error      <- append(error, error[index])
        pageimages <- append(pageimages, pageimages[index])
        next
      }
      #
      # With formatversion=2, j$json$query$pages is a vector
      #for (page in q$pages) {
      for (i in 1:length(q$pages)) {
        page <- q$pages[[i]]
        if (anorm == page$title) {
          if (!is.null(page$invalid) | !is.null(page$missing)) { # malformed title or missing
            aerror <- TRUE
            images  <- NA
          }
          else if (is.null(page$images)) {
            aerror <- FALSE
            images  <- NA
          }
          else {
            aerror <- FALSE
            images <- character()
            for (im in page$images) {
              ext <-strsplit(im$title, ".", fixed = T)[[1]]
              ext <- tolower(ext[length(ext)])    # last is the extension
              if (ext %in% exts)
                next
              img <- paste0('https://', project, '/wiki/', im$title)
              img <- gsub(" ", "_", img)
              # concatenate to previous image
              images <- append(images, img)
            }
          }
          # Feed the output variables
          normalized <- append(normalized, anorm)
          error      <- append(error, aerror)
          pageimages <- append(pageimages, paste0(images, collapse = '|'))
          q$pages[[i]] <- NULL
          break
        }
      }
    }
    #
    if (!is.null(j$continue)) {
      cat(paste("  INFO: continue response\n"), file = stderr())
      for (m in names(j$continue))
        query[[m]] <- j$continue[[m]]
    }
    else {
      output <- data.frame(original=titles, normalized=normalized,
                           error=error, pageimages=pageimages)
      return(output)
    }
  }
}

# ----------------------------------------------------------------------------
# WikiMedia API - Used to obtain metrics of Wikipedia pages.
# See https://www.mediawiki.org/wiki/Wikimedia_REST_API
# See https://en.wikipedia.org/api/rest_v1/
# See https://www.mediawiki.org/wiki/XTools/API/Page [xtools.wmflabs.org]
# ----------------------------------------------------------------------------

#' req_wikimedia_metrics
#' Retrieve responses in JSON format from Wikimedia metrics API:
#'   https://wikimedia.org/api/rest_v1/
#'   https://www.mediawiki.org/wiki/XTools/API/Page \[xtools.wmflabs.org\]
#' @param url The URL with the query
#' @return A JSON response. Please check httr::stop_for_status(response)
#' @author Angel Zazo, Department of Computer Science and Automatics, University of Salamanca
#' @importFrom httr GET user_agent add_headers
#' @note Used in WM_Pageviews
req_wikimedia_metrics <- function(url) {
  httr::GET(
    url = url,
    httr::user_agent(user_agent),
    httr::add_headers(Accept = "application/json")
  )
}

#' WM_Pageviews
#' Use "Wikimedia REST API (https://wikimedia.org/api/rest_v1/) to search the
#' number of views one article has in a Wikimedia project in a date interval
#' (see granularity). Optionally include redirections to the article page.
#' @param article The title of the article to search
#' @param start,end First and last day to include (format YYYYMMDD or YYYYMMDDHH)
#' @param project The Wikimedia project, defaults en.wikipedia.org
#' @param access  Filter by access method: all-access (default), desktop, mobile-app, mobile-web
#' @param agent   Filter by agent type: all-agents, user (default), spider, automated
#' @param granularity  Time unit for the response data: daily, monthly (default)
#' @param include_redirects Boolean to include redirection to the article page (defaults: False)
#' @return A vector with the number of visits by granularity
#' @author Angel Zazo, Department of Computer Science and Automatics, University of Salamanca
#' @importFrom httr stop_for_status content
#' @importFrom jsonlite fromJSON
#' @export
WM_Pageviews <- function(article, start, end, project = "en.wikipedia.org",
                         access = "all-access", agent = "user",
                         granularity = "monthly", include_redirects = FALSE) {
  # req_wikimedia_metrics_rated
  # The limitratedr version of req_wikimedia_metrics.
  # Limit is 100 req/s (See https://wikimedia.org/api/rest_v1/#/Pageviews%20data)
  req_wikimedia_metrics_rated <- limit_requester(req_wikimedia_metrics, n=100, period=1)
  article <- gsub(" ", "_", article)
  url <- "https://wikimedia.org/api/rest_v1/metrics/pageviews/per-article"
  url <- paste(url, project, access, agent, article, granularity, start, end, sep="/", collapse = "")
  # print(url)
  a <- list()  # a <- integer()
  if (include_redirects == TRUE) {
    for (art in MW_GetRedirects(article, project)) {
      b <- WM_Pageviews(art, project, start, end, access, agent,
                        granularity, include_redirects=FALSE)
      for (n in union(names(a), names(b))) {
        a[n] <- ifelse(n %in% names(a), a[n][[1]], 0) + ifelse(n %in% names(b), b[n][[1]], 0)
      }
    }
  }
  else {
    r <- req_wikimedia_metrics_rated(url)
    httr::stop_for_status(r)
    content <- httr::content(r, as = "text", encoding = "UTF-8")
    j <- jsonlite::fromJSON(content, simplifyVector = FALSE)
    for (v in j$items)
      a[v$timestamp] <- v$views
  }
  return(a)
}


#' MW_XtoolsInfo
#' Obtains information about an article in the Wikimedia project using wmflabs
#' in JSON format, or NULL on error. The XTools Page API endpoints offer data
#' related to a single page. See https://www.mediawiki.org/wiki/XTools/API/Page
#' \[xtools.wmflabs.org\]
#' @param article The title of the article to search
#' @param project The Wikimedia project, defaults en.wikipedia.org
#' @param infotype The type of information to request: articleinfo, prose, links
#' @param inlinks_redirects If infotype==links, if redirects are included or not
#' @return The information about the article.
#' @author Angel Zazo, Department of Computer Science and Automatics, University of Salamanca
#' @note Is important that the article don't be a redirection: if infotype=prose
#' the function gets information of the target article, but with infotype=articleinfo
#' or infotype=links the information is about the redirection.
#' @importFrom httr stop_for_status content
#' @importFrom jsonlite fromJSON
#' @export
MW_XtoolsInfo <- function(article, infotype = "articleinfo",
                          project = "en.wikipedia.org", inlinks_redirects = FALSE) {
  # req_wikimedia_metrics_rated
  # The limitratedr version of req_wikimedia_metrics.
  # Limit is 100 req/s.
  req_wikimedia_metrics_rated <- limit_requester(req_wikimedia_metrics, n=100, period=1)
  d <- list()
  if (infotype == "links" & inlinks_redirects) {
    for (art in MW_GetRedirects(article, project)) {
      b <- MW_XtoolsInfo(art, infotype = "links", project = project,
                         inlinks_redirects = FALSE)
      if ("links_in_count" %in% names(d))
        d["links_in_count"] <-  d["links_in_count"][[1]] + b["links_in_count"][[1]]
      else
        d <- b
    }
  }
  else {
    url <- 'https://xtools.wmflabs.org/api/page'
    url <- paste(url, infotype, project, article, sep="/", collapse = "")
    r <- req_wikimedia_metrics_rated(url)
    httr::stop_for_status(r)
    content <- httr::content(r, as = "text", encoding = "UTF-8")
    d <- jsonlite::fromJSON(content, simplifyVector = FALSE)
  }
  return(d)
}

#' MW_XtoolsInfoAll
#' Obtains information about an article in the Wikimedia project using wmflabs
#' in JSON format, or NULL on error. The XTools Page API endpoints offer data
#' related to a single page. See https://www.mediawiki.org/wiki/XTools/API/Page
#' Obtains all information (articleinfo, prose and links) about an article in
#' the wikimedia project.
#' @param article The title of the article to search
#' @param project The Wikimedia project, defaults en.wikipedia.org
#' @param inlinks_redirects If infotype==links, if redirects are included or not
#' @return The information about the article.
#' Note: use the MW_XtoolsInfo function.
#' @export
MW_XtoolsInfoAll <- function(article, project = "en.wikipedia.org",
                             inlinks_redirects = FALSE){
  # first: articleinfo
  r <- MW_XtoolsInfo(article, infotype = 'articleinfo', project = project)
  # Error in response
  if (!is.null(r$error)) {
    cat(paste0("Error in response from MW_XtoolsInfo: ", r$error, '.\n'),
        file = stderr())
    return(NULL)
  }
  # Second: prose
  b <- MW_XtoolsInfo(article, infotype = "prose", project = project)
  for (n in names(b))
    r[n] <- b[n]
  # Third: links (include inlinks_redirects parameter)
  c <- MW_XtoolsInfo(article, infotype = "links", project = project,
                        inlinks_redirects = inlinks_redirects)
  for (n in names(c))
    r[n] <- c[n]
  #
  return(r)
}

# ---------------------------------------------------------------------------
# VIAF API
# https://www.oclc.org/developer/api/oclc-apis/viaf/authority-cluster.en.html
# ---------------------------------------------------------------------------

#' VIAF_AutoSuggest(name)
#' Search the name string from the VIAF AutoSuggest API and returns
#' information in JSON format of the records found. Note that only
#' returns a maximum of 10 records. Note that those records are not
#' VIAF cluster records.
#' @note Note the structure of the string name in the search:
#'   auto: last name, first name\[,\] \[(\[year_of_bird\]\[-year_of_death\])\]
#' @param name string to search
#' @return A list with the records returned.
#' @seealso https://www.oclc.org/developer/api/oclc-apis/viaf/authority-cluster.en.html
#' @export
VIAF_AutoSuggest <- function(name) {
  url <- "https://www.viaf.org/viaf/AutoSuggest"
  query <- list(query = name)
  #
  tryCatch(
    {
      r <- httr::GET(url = url,
                     httr::user_agent(user_agent),
                     query = query)
      httr::stop_for_status(r)
      content <- httr::content(r, as = "text", encoding = "UTF-8")
      d <- jsonlite::fromJSON(content, simplifyVector = FALSE)
      return(d$result)
    }, error = function(e) {
      cat(as.character(e), file = stderr())
      return(NULL)
    }
  )
}

#' VIAF_Search(CQL_Query)
#' Run the CQL_Query using the VIAF Search API and return a list of records
#' found. The search string is formed using the CQL_Query syntax of the API.
#' Note that returned records use the http://viaf.org/BriefVIAFCluster record
#' schema. Also note that only returns a maximum of 250 records.
#' @param CQL_Query string with the search. See https://www.oclc.org/developer/api/oclc-apis/viaf/authority-cluster.en.html
#' @return A list with the records found.
#' @export
VIAF_Search <- function(CQL_Query) {
  maxrecords <- 250
  url <- "https://www.viaf.org/viaf/search"
  query <- list(httpAccept = 'application/json',
                maximunRecords = maxrecords,
                recordSchema = 'http://viaf.org/BriefVIAFCluster',
                startRecords = 1,
                query = CQL_Query)
  #
  tryCatch(
    {
      r <- httr::GET(url = url,
                     httr::user_agent(user_agent),
                     query = query)
      httr::stop_for_status(r)
      content <- httr::content(r, as = "text", encoding = "UTF-8")
      d <- jsonlite::fromJSON(content, simplifyVector = FALSE)
      records <- d$searchRetrieveResponse$records
      return(records)
    }, error = function(e) {
      cat(as.character(e), file = stderr())
      return(NULL)
    }
  )
}

#' VIAF_GetRecord(viafid, record_format)
#' Obtains the record cluster identified by viafid from VIAF, in the format
#' indicated in record_format. Note that the returned record may be a VIAF
#' cluster record or a redirect/scavenged record: the function returns the
#' record as is.
#' @param viafid The VIAF identification.
#' @param record_format 'viaf.json' (default) or others in https://www.oclc.org/developer/api/oclc-apis/viaf/authority-cluster.en.html.
#' @return The VIAF record cluster in the format indicated in record_format.
#' @export
VIAF_GetRecord <- function(viafid, record_format='viaf.json') {
  r <- httr::GET(
    url = url <- paste0("https://viaf.org/viaf/", viafid, '/', record_format),
    httr::user_agent(user_agent)
    )
  #
  tryCatch(
    {
      r <- httr::GET(url = url,
                     httr::user_agent(user_agent))
      httr::stop_for_status(r)
      content <- httr::content(r, as = "text", encoding = "UTF-8")
      # JSON formats:
      if (grepl('json', record_format)) {
        d <- jsonlite::fromJSON(content, simplifyVector = FALSE)
        return(d)
      }
      # Other formats:
      return(content)
    }, error = function(e) {
      cat(as.character(e), file = stderr())
      return(NULL)
    }
  )
}

#' VIAF_Titles(viaf)
#' Returns titles of works from the VIAF record. Note that the VIAF record musts
#' be in JSON format.
#' @param viaf VIAF cluster record (in JSON format).
#' @return A list with titles.
#' @export
VIAF_Titles <- function(viaf) {
  if (is.null(viaf$titles) | is.null(viaf$titles$work))
    return(NULL)
  if (!is.null(viaf$titles$work$title))
    return(viaf$titles$work$title)
  titles = character()
  for (w in viaf$titles$work) {
    titles <- append(titles, w$title)
  }
  return(titles)
}

#' VIAF_Gender(viaf)
#' Return the gender of the author from the VIAF record. Note that the VIAF
#' record musts be in JSON format.
#' @param viaf VIAF cluster record (in JSON format).
#' @return The gender of the author o NULL if not exits in the record.
#' @export
VIAF_Gender <- function(viaf) {
  if (is.null(viaf$fixed))
    return(NULL)
  if (viaf$fixed$gender == 'a')
    return('female')
  else if (viaf$fixed$gender == 'b')
    return('male')
  else
    return(viaf$fixed$gender)
}

#' VIAF_Dates(viaf)
#' Returns bird year and death year from the VIAF cluster record with this
#' format byear:dyear. Note that the VIAF record musts be in JSON format.
#' @param viaf VIAF cluster record (in JSON format).
#' @return The bird year and death year in format byear:dyear.
#' @export
VIAF_Dates <- function(viaf) {
  if (is.null(viaf$birthDate))
    byear <- ''
  else
    byear <- substr(viaf$birthDate, 1, 4)
  #
  if (is.null(viaf$deathDate))
    dyear <- ''
  else
    dyear <- substr(viaf$deathDate, 1, 4)
  #
  return(paste0(byear,":", dyear))
}

#' VIAF_Occupations(viaf)
#' Return the occupations from the VIAF record. Note that the VIAF record musts
#' be in JSON format.
#' @param viaf VIAF cluster record (in JSON format).
#' @return A data-frame with sources and occupations from each source or NULL if
#' occupations do not exist in the record.
#' @export
VIAF_Occupations <- function(viaf) {
  if (is.null(viaf$occupation))
    return(NULL)
  #
  dd <- viaf$occupation$data
  if (!is.null(dd$text))
    dd <- list(dd)
  # df1 is the returned data-frame.
  df1 = data.frame()
  for (l in dd) {
    occ <- list()
    for (s in l$sources$s) {
      occ[[s]] <- l$text
    }
    # occ[[l$sources$s]] <- l$text
    # texts <- rbind(texts, c(l$text, paste(l$sources$sid, collapse = ';')))
    # texts <- rbind(texts, c(l$text, sids))
    df2 <- data.frame(occ)
    ## Unify colname of first column
    # colnames(df2)[1] <- c('text')
    if (nrow(df1) == 0)
      df1 <- df2
    else
      # https://stackoverflow.com/questions/3402371/combine-two-data-frames-by-rows-rbind-when-they-have-different-sets-of-columns
      df1 <- rbind(
        data.frame(c(df1, sapply(setdiff(names(df2), names(df1)), function(x) NA))),
        data.frame(c(df2, sapply(setdiff(names(df1), names(df2)), function(x) NA)))
      )
  }
  return(df1)
}

#' VIAF_Sources(viaf)
#' Return the text of all sources id from the VIAF record. Note that the VIAF
#' record musts be in JSON format.
#' @param viaf VIAF cluster record (in JSON format).
#' @return A data-frame with text and sources.
#' @export
VIAF_Sources <- function(viaf) {
  vv <- viaf$mainHeadings$data
  if (!is.null(vv$text))
    vv <- list(vv)
  # df1 is the returned data-frame.
  df1 = data.frame()
  for (l in vv) {
    sids <- list()
    for (sid in l$sources$sid) {
      s <- strsplit(sid, '|', fixed = TRUE)[[1]]
      sids[[s[1]]] <- s[2]
    }
    # texts <- rbind(texts, c(l$text, paste(l$sources$sid, collapse = ';')))
    # texts <- rbind(texts, c(l$text, sids))
    df2 <- data.frame(c(l$text, sids))
    ## Unify colname of first column
    colnames(df2)[1] <- c('text')
    if (nrow(df1) == 0)
      df1 <- df2
    else
    # https://stackoverflow.com/questions/3402371/combine-two-data-frames-by-rows-rbind-when-they-have-different-sets-of-columns
      df1 <- rbind(
        data.frame(c(df1, sapply(setdiff(names(df2), names(df1)), function(x) NA))),
        data.frame(c(df2, sapply(setdiff(names(df1), names(df2)), function(x) NA)))
    )
  }
  return(df1)
}

#' VIAF_SourceId(viaf, source)
#' Return the text and identification of the source from the VIAF record.
#' Note that the VIAF record musts be in JSON format.
#' @param viaf VIAF cluster record (in JSON format).
#' @param source the identificator of the source (LC, WKP, JPG, BNE...)
#' @return A data-frame with columns text and source, or NULL if the source does
#' no exist in the viaf record.
#' @export
VIAF_SourceId <- function(viaf, source) {
  texts <- VIAF_Sources(viaf)
  if (is.null(texts[[source]]))
    return(NULL)
  else
    return(texts[!is.na(texts[[source]]), c('text', source)])
}

#' VIAF_Wikipedias(viaf)
#' Return the Wikipedia pages (URL) from the VIAF record. Note that the VIAF
#' record musts be in JSON format.
#' @param viaf VIAF cluster record (in JSON format).
#' @return A vector with the URL of the Wikipedias.
#' @export
VIAF_Wikipedias <- function(viaf) {
  if (is.null(viaf$xLinks))
    return(NULL)
  #
  wikis <- character()
  for (l in viaf$xLinks$xLink)
    wikis <- append(wikis, l[['#text']])
  return(wikis)
}

