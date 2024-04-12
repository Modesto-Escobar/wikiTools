#### wiki_utils.R
#### Angel Zazo <angelzazo@usal.es>
#### ver. 0.0 2021-11-09
#### ver. 0.1 2023-05-12
#### ver. 0.2 2023-07-27
#### ver. 0.3 2023-09-19

# General user_agent header for Wikimedia, Wikidata, MediaWiki and VIAF requests ----
#' See https://meta.wikimedia.org/wiki/User-Agent_policy.
user_agent <- paste('netCoincidenceAnalysis Bot (<https://sociocav.usal.es/me/GAS/>),', R.version.string)

#' Limits the rate at which a function will execute
#' 
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
limitRequester <- function(f, n, period) {
  return(ratelimitr::limit_rate(f, ratelimitr::rate(n = n, period = period)))
}


# WikiData Query Service ----
# See https://query.wikidata.org/
# See https://www.wikidata.org/wiki/Wikidata:SPARQL_tutorial
# See https://www.mediawiki.org/wiki/Wikidata_Query_Service/User_Manual
# --------------------------------------------------------------------.

#' Get responses from Wikidata Query Service
#' 
#' Retrieves responses from Wikidata Query Service (WDQS)
#' @param sparql_query A string with the query in SPARQL language.
#' @param format  A string with the query response format, mandatory. See
#' https://www.mediawiki.org/wiki/Wikidata_Query_Service/User_Manual#SPARQL_endpoint.
#' Only  'json', 'xml' or 'csv' formats are allowed, default 'json'.
#' @param method The method used in the httr request, GET or POST, mandatory.
#' Default 'GET'.
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

#' Responses from Wikidata Query Service
#' 
#' Retrieves responses from Wikidata Query Service (WDQS). Uses ratelimitr if
#' param limitRequester = TRUE.
#' @param sparql_query A string with the query in SPARQL language.
#' @param format A string with the query response format. Mandatory.
#' See https://www.mediawiki.org/wiki/Wikidata_Query_Service/User_Manual#SPARQL_endpoint.
#' Only  'json', 'xml' or 'csv' formats are allowed, default 'csv'.
#' @param method The method used in the httr request, GET or POST, mandatory.
#' Default 'GET'.
#' @param limitRequester If True, uses ratelimitr to limit the requests.
#' @return The response in selected format or NULL on errors.
#' @author Angel Zazo, Department of Computer Science and Automatics, University of Salamanca
#' @importFrom httr stop_for_status content
#' @importFrom jsonlite fromJSON
#' @importFrom utils read.csv
w_query <- function(sparql_query, format="csv", method="GET",
                       limitRequester=FALSE) {
  # reqWDQS_rated: The ratelimitr version of reqWDQS
  # https://www.mediawiki.org/wiki/Wikidata_Query_Service/User_Manual#SPARQL_endpoint
  if (limitRequester)
    reqWDQS_rated <- limitRequester(reqWDQS, n=60, period=60)
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

#' Check if a Wikidata entity is an instance of a class
#' 
#' Check using WDQS if the Wikidata entities in entity_list are instances of
#' "instanceof" Wikidata entity class. For example, if instanceof="Q5", check if
#' entities are instances of the Wikidata entity class Q5, i.e, are humans.
#' Duplicated entities are deleted before search.
#' @param entity_list A vector with de Wikidata entities.
#' @param instanceof The Wikidata class to check, mandatory.
#' @return A data-frame with two columns, first Wikidata entity, second TRUE
#' if that entity is instance of the "instanceof" entity, else FALSE. Index of
#' data-frame are also set to entity_list.
#' @examples
#' # aux: get a vector of entities (l).
#' df <- w_SearchByLabel(string='Iranzo', langsorder='es|en', mode='inlabel')
#' l <- df$entity
#' 
#' df <- w_isInstanceOf(entity_list=l, instanceof='Q5')
#' # Not TRUE
#' df[!df$instanceof_Q5,]
#' @author Angel Zazo, Department of Computer Science and Automatics, University of Salamanca
#' @export
w_isInstanceOf <- function(entity_list, instanceof) {
  if (instanceof == '')
    stop(paste0("ERROR: instanceof is mandatory."))
  #
  entity_list <- unique(entity_list)
  values <- paste0("wd:",paste0(entity_list, collapse = ' wd:'))
  query <- paste0('SELECT ?entity WHERE {VALUES ?entity {', values,'}',
                  ' ?entity wdt:P31 wd:', instanceof, '}')
  r <- w_query(query, format="csv", method='POST')
  r$entity <- sub('^http://www.wikidata.org/entity/', '', r$entity)
  #
  data <- ifelse(entity_list %in% r$entity, T, F)
  d <- data.frame(entity_list, data)
  colnames(d) <- c("entity", paste0("instanceof_", instanceof))
  rownames(d) <- entity_list
  return(d)
}

#' Gets Wikipedia pages from a Q list.
#' 
#' Gets from Wikidata all Wikipedia page titles of the Wikidata entities in
#' entity_list. If set "instanceof", then only returns the pages for Wikidata
#' entities which are instances of that Wikidata class. If wikilangs='', then
#' returns all Wikipedia page titles, else only the languages in wikilangs.
#' Duplicated entities are deleted before search.
#' @param entity_list A vector of Wikidata entities.
#' @param wikilangs List of languages to limit the search, using "|" as
#' separator. Wikipedias page titles are returned in same order as languages in
#' this parameter. If wikilangs='' the function returns Wikipedia page titles
#' in any language, not sorted.
#' @param instanceof Wikidata entity class to limit the result to the instances
#' of that class. For example, if instanceof='Q5', limit the results to "human".
#' @param nlimit If the number of entities exceeds this number, chunked queries
#' are done. This is the number of entities requested in each chunk.
#' @return A data-frame with four columns, first the count of Wikipedia pages,
#' second, the the langs, page title, and URL's of the wikipedia pages. Last
#' three use "|" as separator. Index of the data-frame is also set to the
#' entity_list.
#' @examples
#' # aux: get a vector of entities (l).
#' df <- w_SearchByLabel(string='Iranzo', langsorder='es|en', mode='inlabel')
#' l <- df$entity
#' 
#' w <- w_Wikipedias(entity_list=l)
#' w <- w_Wikipedias(entity_list=l, wikilangs='es|en|fr')
#' w <- w_Wikipedias(entity_list=l, wikilangs='es|en|fr', instanceof="Q5")
#' @author Angel Zazo, Department of Computer Science and Automatics, University of Salamanca
#' @export
w_Wikipedias <- function(entity_list, wikilangs="", instanceof="",
                            nlimit=1500) {
  # Eliminate duplicates
  entity_list <- unique(entity_list)
  # Check limits to make chunked queries
  n <- length(entity_list)
  if (n > nlimit) {
    cat(paste0("INFO: The number of entities (", n, ") exceeds nlimit (",
               nlimit,"): doing chunked queries.\n"), file=stderr())
    nlim <- as.integer(n/nlimit)
    for (k in 0:nlim) {
      offset <- nlimit * k
      if ((offset+1) > n)
        break
      q_list <- entity_list[(offset+1):min(n,offset+nlimit)]
      cat(paste0(" INFO: Getting Wikipedias of entities from ", offset+1," to ", offset+length(q_list),".\n"), file=stderr())
      d <- w_Wikipedias(q_list, wikilangs, instanceof, nlimit)
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
  #
  if (filterq) {
    if (grepl('|', instanceof, fixed = T)) {
      filters <- paste0('?entity wdt:P31 ?instanc. FILTER(?instanc IN (wd:', sub('|', ', wd:', instanceof, fixed = T),')).\n')
    }
    else {
      filters <- paste0('?entity wdt:P31 ?instanc. FILTER(?instanc IN (wd:', instanceof, ', wd:', instanceof,')).\n')
    }
  }
  query <- paste0("SELECT ?entity (COUNT(?page) as ?count)
(GROUP_CONCAT(?lang;separator='|') as ?langs)
(GROUP_CONCAT(?name;separator='|') as ?names)
(GROUP_CONCAT(?page;separator='|') as ?pages)
WHERE {\n",
  ifelse(filterq, '', 'OPTIONAL {\n'),
  "  VALUES ?entity {", values, "}\n",
  if (filterq) filters else "",
  "OPTIONAL {
    ?page schema:about ?entity;
          schema:inLanguage ?lang;
          schema:name ?name;
          schema:isPartOf [wikibase:wikiGroup \"wikipedia\"].\n",
  ifelse(wikilangs!="", paste0("  FILTER(?lang in (", wikifilter, "))}\n"), "}\n"),
  ifelse(filterq,'', "}\n"),
  "} GROUP BY ?entity")
  # cat(query)
  r <- w_query(query, format='csv', method='POST')
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
      l <- strsplit(r[entity, 'pages'], '|', fixed = T)[[1]]
      r[entity, "pages"] <- paste0(l[o], collapse = '|')
    }
  }
  # Create a NA data-frame and fill it with retrieved values
  q <- data.frame(entity=r$entity, count=NA, langs=NA, names=NA, pages=NA)
  rownames(q) <- q$entity
  q[r$entity, ] <- r
  return(q)
}

#' Check if a Wikidata entity is valid
#'
#' Check if the Wikidata entities are valid. A entity is valid if it has a label
#' or has a description. If one entity exists but is not valid, is possible that
#' it has a redirection to other entity, in that case, the redirection is
#' obtained. Other entities may have existed in the past, but have been deleted.
#' Duplicated entities in entity_list are deleted before checking. Index of the
#' data-frame returned are also set to entity_list.
#' @param entity_list A vector with de Wikidata entities.
#' @param nlimit If the number of entities exceeds this number, chunked queries
#' are done. This is the number of entities requested in each chunk.
#' @return A data-frame with three columns: firts, the entity itself, second,
#' if that entity is valid in Wikidata (TRUE or FALSE), last, if the entity
#' redirects to another Wikidata entity, this entity.
#' @examples
#' \dontrun{
#' w_isValid(c("Q9021", "Q115637688", "Q105660123"))
#' 
#' l  <- w_OccupationEntities(Qoc='Q2306091')
#' l2 <- append(l, c("Q115637688", "Q105660123"))  # Note: adding two new entities
#' v <- w_isValid(l2)
#' # Not valid
#' v[!v$valid,]
#' }
#' @author Angel Zazo, Department of Computer Science and Automatics, University of Salamanca
#' @export
w_isValid <- function(entity_list, nlimit=50000) {
  entity_list <- unique(entity_list)
  # Check limits to make chunked queries
  n <- length(entity_list)
  if (n > nlimit) {
    cat(paste0("INFO: The number of entities (", n, ") exceeds nlimit (",
               nlimit,"): doing chunked queries.\n"), file=stderr())
    nlim <- as.integer(n/nlimit)
    for (k in 0:nlim) {
      offset <- nlimit * k
      if ((offset+1) > n)
        break
      q_list <- entity_list[(offset+1):min(n,offset+nlimit)]
      cat(paste0(" INFO: Getting information of entities from ", offset+1," to ", offset+length(q_list),"\n"), file=stderr())
      d <- w_isValid(q_list, nlimit)
      if (k==0)
        output <- d
      else
        output <- rbind(output, d)
    }
    return(output)
  }
  values <- paste0("wd:", paste0(entity_list, collapse = ' wd:'))
  query <- paste0('SELECT ?entity ?valid ?redirection WHERE
{VALUES ?entity {', values, '}
# BIND(EXISTS{?entity rdfs:label []} AS ?valid)
BIND(EXISTS{?entity rdfs:label []} || EXISTS{?entity schema:description ?d} AS ?valid).
OPTIONAL {?entity owl:sameAs ?redirection}
}')
  #cat(query)
  #
  r <- w_query(query, format = "csv", method = 'POST')
  for (c in c('entity',  'redirection'))
    r[[c]] <- gsub('http://www.wikidata.org/entity/', '', r[[c]])
  r$valid <- ifelse(r$valid == 'true', T, F)
  # Convert NA -> ""
  r$redirection <- ifelse(is.na(r$redirection),"",r$redirection)
  rownames(r) <- r$entity
  return(r)
}

#' Gets Wikidata entities with a certain occupation
#' 
#' Returns the Wikidata entities which have the occupation indicated in Qoc, the
#' Wikidata entity for that occupation. Use chunked requests.
#' @param Qoc The Wikidata entity of the occupation. For example, Q2306091
#' sociologist, Q2526255 Film director, etc.
#' @param nlimit If the number of entities found with that occupation exceeds
#' this number, chunked queries are done. This is the number of entities requested
#' in each chunk. No effects in mode='count'. (default=10000, 5000 if mode='wikipedias')
#' @param mode The results you want to obtain: 'default' returns the Wikidata
#' entities which have the occupation indicated; 'count' search in WDQS to know
#' the number of Wikidata entities with P106 property (occupation) set to Qoc;
#' 'wikipedias' returns the Wikidata entities which have the occupation
#' indicated in Qoc, plus the Wikipedia page titles of them.
#' Note that mode='wikipedias' is similar to first launch w_OccupationEntities
#' and then launch w_Wikipedias, but is more efficient. (default='default')
#' @return A vector with the Wikidata entities with that occupation by default;
#' if mode='count' the number of entities with that occupation (integer);
#' if mode='wikipedias' a data-frame with Wikidata entities, the number of
#' Wikipedias in which they have page, the Wikipedia languages, the page titles,
#' and finally, the URL to the pages. Last three columns concatenated with "|".
#' Return all Wikipedias pages, not limited by languages.
#' @examples
#' w_OccupationEntities(Qoc='Q2306091', mode='count') # Qoc for Sociologist
#' l <- w_OccupationEntities(Qoc='Q2306091')
#' \dontrun{
#' lw <- w_OccupationEntities(Qoc='Q2306091', mode='wikipedias')
#' }
#' @author Angel Zazo, Department of Computer Science and Automatics, University of Salamanca
#' @export
w_OccupationEntities <- function(Qoc, nlimit=NULL, mode=c('default','count','wikipedias')) {
  mode <- mode[1]
  args <- list(Qoc = Qoc)
  if(!is.null(nlimit)){
    args$nlimit <- nlimit
  }
  occupationFn <- w_OccupationEntitiesDefault
  if(mode=='count'){
    occupationFn <- w_OccupationCount
    args$nlimit <- NULL
  }else if(mode=='wikipedias'){
    occupationFn <- w_OccupationEntitiesWikipedias
  }
  return(do.call(occupationFn,args))
}

# Gets number of Wikidata entities with an occupation
w_OccupationCount <- function(Qoc) {
 query <- paste0('SELECT (COUNT(DISTINCT ?human) AS ?count) WHERE {?human wdt:P106 wd:',Qoc,'}')
 r <- w_query(query, format="csv")
 return(r$count)
}

# Gets Wikidata entities with a certain occupation
w_OccupationEntitiesDefault <- function(Qoc, nlimit=10000) {
  entity <- character()
  nq <- w_OccupationCount(Qoc)
  nlim <- as.integer(nq/nlimit)
  if (nlim > 0)
    cat(paste0("INFO: The number of entities (", nq, ") exceeds nlimit (",
               nlimit,"): doing chunked queries.\n"), file=stderr())
  for (k in 0:nlim) {
    offset <- nlimit * k
    if (nlim > 0)
      cat(paste0(" INFO: sending query from ", offset+1," to ", min(offset+nlimit,nq),"\n"), file=stderr())
    query <- paste0("SELECT ?entity WHERE {?entity wdt:P106 wd:", Qoc,
                    "} ORDER BY ?entity LIMIT ", nlimit, " OFFSET ", offset)
    r <- w_query(query, format="csv")
    r$entity <- sub('^http://www.wikidata.org/entity/', '', r$entity)
    entity <- append(entity, r$entity)
  }
  entity <- unique(entity)
  return(entity)
}


# Gets Wikipedias entries with a certain occupation
w_OccupationEntitiesWikipedias <- function(Qoc, nlimit=5000) {
  nq <- w_OccupationCount(Qoc)
  nlim <- as.integer(nq/nlimit)
  if (nlim > 0)
    cat(paste0("INFO: The number of entities (", nq, ") exceeds nlimit (",
               nlimit,"): doing chunked queries.\n"), file=stderr())
  for (k in 0:nlim) {
    offset <- nlimit * k
    if (nlim > 0)
      cat(paste0(" INFO: sending query from ", offset+1," to ", min(offset+nlimit,nq),"\n"), file=stderr())
    query <- paste0("SELECT ?entity (COUNT(?page) as ?npages)
(GROUP_CONCAT(?lang;separator='|') as ?langs)
(GROUP_CONCAT(?name;separator='|') as ?names)
(GROUP_CONCAT(?page;separator='|') as ?pages)
WITH {
    SELECT ?entity
    WHERE {?entity wdt:P106 wd:", Qoc, ".}
    ORDER BY ?entity
    LIMIT ", nlimit, " OFFSET ", offset, "
    } AS %results
WHERE {
   INCLUDE %results.
   OPTIONAL {?page schema:about ?entity;
                   schema:inLanguage ?lang;
                   schema:name ?name;
                   schema:isPartOf [ wikibase:wikiGroup 'wikipedia' ] .}
} GROUP BY ?entity")
    # cat(query)
    r <- w_query(query, format = "csv")
    r$entity <- sub('^http://www.wikidata.org/entity/', '', r$entity)
    rownames(r) <- r$entity
    if (k==0)
      output <- r
    else
      output <- rbind(output, r)
  }
  return(output)
}

#' Search Wikidata entities
#' 
#' Search Wikidata entities in label and altLabel ("Also known as")
#' @param string string (label or altLabel) to search.
#' @param langsorder Order of languages in which the information will be
#' returned, separated with '|'. If no information is given in the first
#' language, next is used. This parameter is mandatory, at least one language is
#' required, default, 'en'.
#' @param lang The language to search, only one. If lang="" and mode="inlabel",
#' search is in any language. Mandatory in mode="startswith", no effects in
#' mode="exact".
#' @param instanceof Wikidata entity of which the entities searched for are an
#' example or member of it (class). For example, if instanceof=Q5 the
#' search are filtered to Wikidata entities of class Q5 (human). Some
#' entity classes are allowed, separated with '|'.
#' @param Pproperty Wikidata properties, separated with '|', to optionally
#' include in the search. For example, is Pproperty="P21", the results
#' include information of the sex of entities found as STRING.
#' @param mode The mode to perform search: 'exact' for search using case
#' sensitive and differentiate diacritics; 'startswith' for entities which label
#' or altLabel starts with "string", similar to a wildcard search "string*",
#' searchs "string" in language "lang" in label, but in any language in
#' altLabel; 'inlabel' for matching whole words in any position. ('exact' by
#' default).
#' @return A data-frame with 'entity', 'entityLabel', 'entityDescription',
#' (including 'instance', 'instanceLabel', 'altLabel' if mode="startswith")
#' and additionally the properties of Pproperty.
#' @examples
#' df <- w_SearchByLabel(string='Iranzo', langsorder='es|en', mode="exact")
#' df <- w_SearchByLabel(string='Iranzo', langsorder='es|en', instanceof = 'Q5|Q101352', mode="exact")
#' ## Search entities which label or altLabel starts with "string"
#' df <- w_SearchByLabel(string='Iranzo', lang='en', langsorder='es|en',
#' mode='startswith')
#' df <- w_SearchByLabel(string='Iranzo', lang='en', langsorder='en',
#' instanceof = 'Q5', Pproperty = 'P21|P569|P570', mode='startswith')
#' ## Search in any position in Label or AltLabel (diacritics and case are ignored)
#' # If lang=='' search in any language, else the search is performed only in the
#' # language indicated.
#' df <- w_SearchByLabel(string='Iranzo', langsorder='es|en', mode='inlabel')
#' # Search in Chinese (Simplified) (language code: zh):
#' df <- w_SearchByLabel(string='Iranzo', langsorder='zh|es', lang='zh',
#' mode='inlabel')
#' @author Angel Zazo, Department of Computer Science and Automatics, University of Salamanca
#' @export
w_SearchByLabel <- function(string, langsorder='en', lang="", instanceof="", Pproperty="", mode=c("exact","startswith","inlabel")) {
  mode <- mode[1]
  #
  string <- gsub("'", "\\'", string, fixed = T)
  filterq <- if (instanceof != "") TRUE else FALSE
  searchp <- if (Pproperty != "") TRUE else FALSE
  langsorderw <- gsub("|", ",", langsorder, fixed = T)
  #
  searchlang <- paste0('\nSERVICE wikibase:label {bd:serviceParam wikibase:language "', langsorderw, '".\n',
                       ' ?instanc rdfs:label ?instancLabel.')
  #
  if (filterq) {
    if (grepl('|', instanceof, fixed = T)) {
      filters <- paste0('FILTER(?instanc IN (wd:', sub('|', ', wd:', instanceof, fixed = T),')).\n')
    }
    else {
      filters <- paste0('FILTER(?instanc IN (wd:', instanceof, ', wd:', instanceof,')).\n')
    }
  }
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
  if(mode=="exact"){
    l <- strsplit(langsorder, '|', fixed = T)[[1]]
    units <- paste0("{?entity rdfs:label '", string, "'@", l, '}')
    unionlabel <- paste0(units, collapse = "\nUNION\n")
    unitsalt <- paste0("{?entity skos:altLabel '", string, "'@", l, '}')
    unionaltLabel <- paste0(unitsalt, collapse = "\nUNION\n")

    querywhere <- paste0("\n",unionlabel,"\n UNION \n",unionaltLabel)
  }else if(mode=="startswith"){
    querywhere <- paste0('SERVICE wikibase:mwapi {
  bd:serviceParam wikibase:api "EntitySearch";
                  wikibase:endpoint "www.wikidata.org";
                  mwapi:language "', lang, '";
                  mwapi:search "', string, '" .
  ?entity wikibase:apiOutputItem mwapi:item.
  }')
  }else if(mode=="inlabel"){
    if (lang != ""){
      string <- paste0(string, '@', lang)
    }
    querywhere <- paste0('SERVICE wikibase:mwapi {
  bd:serviceParam wikibase:api "Search";
                  wikibase:endpoint "www.wikidata.org";
                  mwapi:srsearch \'inlabel:"', string, '"\'.
  ?entity wikibase:apiOutputItem mwapi:title.
  }')
  }else{
    stop("wrong mode specified")
  }
  #
  query <- paste0("SELECT DISTINCT ?entity ?entityLabel ?entityDescription
(GROUP_CONCAT(DISTINCT ?instanc; separator='|') as ?instance)
(GROUP_CONCAT(DISTINCT ?instancLabel; separator='|') as ?instanceLabel)
(GROUP_CONCAT(DISTINCT STR(?altlabel);separator='|') as ?altLabel)\n",
if (searchp) group_concat else "",
"\nWHERE {",
querywhere,
"\nSERVICE wikibase:label {bd:serviceParam wikibase:language '", langsorderw,"'}",
"\n?entity wdt:P31 ?instanc.\n",
if (filterq) filters else "",
"OPTIONAL {?entity skos:altLabel ?altlabel}\n",
if (searchp) search else "",
paste0(searchlang, '}'),
"\n} GROUP BY ?entity ?entityLabel ?entityDescription")
  #
  r <- w_query(query, format = "csv")
  for (c in c('entity',  'instance'))
    r[[c]] <- gsub('http://www.wikidata.org/entity/', '', r[[c]])
  rownames(r) <- r$entity
  return(r)
}

#' Searching for properties of the entity list
#' 
#' Search the entities of the entity_list for property or properties. Return the
#' properties in langsorder order. Duplicated entities are deleted before search.
#' @param entity_list A vector with de Wikidata entities.
#' @param Pproperty Wikidata properties to search, separated with '|', mandatory.
#' For example, is Pproperty="P21", the results contain information of the sex
#' of entities. If Pproperty="P21|P569" also searches for birthdate. If
#' Pproperty='P21|P569|P214' also searches for VIAF identifier.
#' @param langsorder Order of languages in which the information will be
#' returned, separated with '|'. If no information is given in the first
#' language, next is used. This parameter is mandatory, at least one language is
#' required, default, 'en'
#' @param nlimit If the number of entities exceeds this number, chunked queries
#' are done. This is the number of entities requested in each chunk.
#' @return A data-frame with 'entity', 'entityLabel', 'entityDescription' and,
#' additionally, the properties of Pproperty. Index of the data-frame is also
#' set to entity_list.
#' @examples
#' \dontrun{
#' l <- w_OccupationEntities(Qoc='Q2306091')
#' p <- w_Property(l, Pproperty = 'P21|P569|P214', langsorder = 'es|en')
#' }
#' @author Angel Zazo, Department of Computer Science and Automatics, University of Salamanca
#' @export
w_Property <- function(entity_list, Pproperty, langsorder='en', nlimit=10000) {
  #
  if (Pproperty == "")
      stop(paste0("ERROR: Pproperty is mandatory."))
  # Eliminate duplicates
  entity_list <- unique(entity_list)
  # Check limits to make chunked queries
  n <- length(entity_list)
  if (n > nlimit) {
    cat(paste0("INFO: The number of entities (", n, ") exceeds nlimit (",
               nlimit,"): doing chunked queries.\n"), file=stderr())
    nlim <- as.integer(n/nlimit)
    for (k in 0:nlim) {
      offset <- nlimit * k
      if ((offset+1) > n)
        break
      q_list <- entity_list[(offset+1):min(n,offset+nlimit)]
      cat(paste0(" INFO: Getting properties of entities from ", offset+1," to ", offset+length(q_list),"\n"), file=stderr())
      d <- w_Property(q_list, Pproperty, langsorder, nlimit)
      if (k==0)
        output <- d
      else
        output <- rbind(output, d)
    }
    return(output)
  }
  #
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
  # cat(query, file='borrar.txt')
  #
  r <- w_query(query, format = "csv", method = 'POST')
  r$entity   <- gsub('http://www.wikidata.org/entity/', '', r$entity)
  rownames(r) <- r$entity
  return(r)
}

#' Search for Wikidata entities that have a property identifier
#' 
#' Search for Wikidata entities that have an identifier in the Wikidata
#' authority property "Pauthority". Return the entities and information (label,
#' description) in the language order indicated in langsorder. If instanceof
#' has a value, then response is limited to entities which are instance of it.
#' @param Pauthority Wikidata property identifier for the authority, i.e, the
#' property of Wikidata for the the database of the authority. For example, is
#' Pauthority = "P4439", then search Wikidata entities that have an identifier
#' in the MNCARS (Museo Nacional Centro de Arte Reina Sofía) authority database.
#' @param langsorder Order of languages in which the information will be
#' returned, separated with '|'. If no information is given in the first
#' language, next is used. This parameter is mandatory, at least one language is
#' required, default, 'en'.
#' @param instanceof Wikidata entity of which the entities searched for are an
#' example or member of it (class). Optional. For example, if instanceof="Q5"
#' the search are filtered to Wikidata entities of class Q5 (human). Some
#' entity classes are allowed, separated with '|'.
#' @return A data-frame with 'entity', 'entityLabel', 'entityDescription' and
#' the identifier in the "Pauthority" database,
#' @examples
#' \dontrun{
#' # Example: Pauthority=P4439 (has identificator in the Museo Nacional Centro de
#' # Arte Reina Sofía)
#' mncars   <- w_IdentifiersOfAuthority(Pauthority="P4439",
#' langsorder = 'es|en')  # 1286  [human, groups, etc.]
#' mncarsQ5 <- w_IdentifiersOfAuthority(Pauthority="P4439", langsorder = 'es|en',
#' instanceof = 'Q5')  # 1280
#' # Wikidata entities are not 'human' (Q5) (see entityDescription column):
#' mncars[!(mncars$entity %in% mncarsQ5$entity),]  # not instance of Q5.
#' }
#' @author Angel Zazo, Department of Computer Science and Automatics, University of Salamanca
#' @export
w_IdentifiersOfAuthority <- function(Pauthority, langsorder='en', instanceof="") {
  #
  if (Pauthority == "")
    stop(paste0("ERROR: Pauthority is mandatory."))
  #
  filterq <- if (instanceof != "") TRUE else FALSE
  #
  if (filterq) {
    if (grepl('|', instanceof, fixed = T)) {
      filters <- paste0('?entity wdt:P31 ?instanc. FILTER(?instanc IN (wd:', sub('|', ', wd:', instanceof, fixed = T),')).\n')
    }
    else {
      filters <- paste0('?entity wdt:P31 ?instanc. FILTER(?instanc IN (wd:', instanceof, ', wd:', instanceof,')).\n')
    }
  }
  langsorderw <- gsub("|", ",", langsorder, fixed = T)
  #
  query <- paste0("SELECT DISTINCT ?entity ?entityLabel ?entityDescription
(GROUP_CONCAT(DISTINCT STR(?authid);separator='|') as ?", Pauthority, ")
WHERE {
  ?entity wdt:", Pauthority," ?authid.\n",
if (filterq) filters else "",
'  SERVICE wikibase:label {bd:serviceParam wikibase:language "', langsorderw, '"}
} GROUP BY ?entity ?entityLabel ?entityDescription')
  #cat(query)
  #
  r <- w_query(query, format = "csv", method = 'POST')
  r$entity   <- gsub('http://www.wikidata.org/entity/', '', r$entity)
  rownames(r) <- r$entity
  return(r)
}

#' Get some personal properties of one Wikidata entity
#' 
#' Gets some properties of the Wikidata "entity" related to birth and death
#' dates, places, occupations, works, education, awards, identifier in some
#' libraries, Wikipedia page titles (which can be limited to the languages in
#' the "wikilangs" parameter), etc.
#' @param entity The Wikidata entity to search for properties. Only one entity
#' is allowed.
#' @param langsorder Order of languages in which the information will be
#' returned, separated with '|'. If no information exists in the first language,
#' next is used. This parameter is mandatory, at least one language is required,
#' default, 'en'.
#' Note: sometimes not label in any language of langsorder is assigned to an
#' entity, so an additional search is used to obtain almost one label for it
#' (?entitylab) with LIMIT 1.
#' @param wikilangs List of languages to limit the search of Wikipedia pages,fi
#' using "|" as separator. Wikipedias pages are returned in same order as
#' languages in this parameter. If wikilangs='' the function returns Wikipedia
#' pages in any language, not sorted.
#' @param format Wikipedia address format. By default is reduced, Otherwise, format is regular.
#' @param mode The mode to obtain results: 'tiny' less properties are requested
#' and less checks are done; 'film' gets some properties of the Wikidata
#' "entity" related to information about film ('default' by default).
#' @return A data-frame with the properties of the entity.
#' @examples
#' df1 <- w_EntityInfo(entity='Q134644', langsorder = 'es|en')
#' df2 <- w_EntityInfo(entity='Q134644', langsorder = 'es|en', mode = 'tiny')
#' \dontrun{
#' films <- w_EntityInfo(entity=c('Q180098','Q151895'), langsorder='es|en',
#' wikilangs='es|fr|en', mode='film')
#' }
#' @author Angel Zazo, Department of Computer Science and Automatics, University of Salamanca
#' @export
w_EntityInfo <- function(entity, langsorder='en', wikilangs="", format="reduced", mode=c('default','tiny','film')) {
  mode <- mode[1]
  #
  langsorder <- gsub("|", ",", langsorder, fixed = T)
  wikifilter <- ''
  if (wikilangs != "") {
    wikiorder  <- strsplit(wikilangs, '|', fixed = T)[[1]]
    wikifilter <- paste0("'",gsub('|', "','", wikilangs, fixed=T), "'")
  }
  #
  if(mode=="tiny"){
    get_query <- w_EntityInfo_tiny_query
  }else if(mode=="film"){
    get_query <- w_FilmInfo_query
  }else{
    get_query <- w_EntityInfo_query
  }
  #
  res <- lapply(entity,function(qid){
    query <- paste0(get_query(qid,langsorder,wikifilter))
    r <- w_query(query, format = "csv", method = 'POST')
    # Eliminate 'http://www.wikidata.org/entity/' in columns that ends with 'Q'
    forgsub <- append(c('entity'), colnames(r)[endsWith(colnames(r), 'Q')])
    for (c in forgsub)
      r[[c]] <- gsub('http://www.wikidata.org/entity/', '', r[[c]])
    #
    rownames(r) <- r$entity
    # Order by wikilangs
    for (entity in r$entity) {
      if (wikilangs != "" && !is.na(r[entity, 'wikilangs'])) {
        l <- strsplit(r[entity, 'wikilangs'], '|', fixed = T)[[1]]
        if (length(l) > 1) {
          o <- match(wikiorder, l)
          o <- o[!is.na(o)]
          r[entity, "wikilangs"] <- paste0(l[o], collapse = '|')
          l <- strsplit(r[entity, 'wikipedias'], '|', fixed = T)[[1]]
          r[entity, "wikipedias"] <- paste0(l[o], collapse = '|')
        }
      }
      if(format=="reduced") {
        r[entity, "wikipedias"] <- gsub("\\.wikipedia\\.org","\\.m\\.wikipedia\\.org", r[entity, "wikipedias"])
      }
    }
    #
    return(r)
  })
  res <- do.call(rbind,res)
  return(res)
}

# Get some personal properties of one Wikidata entity
w_EntityInfo_query <- function(qid, langsorder, wikifilter) {
  paste0('SELECT DISTINCT ?entity ?entityLabel (LANG(?entitylab) as ?entitylablang) ?entitylab
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
  SERVICE wikibase:label {bd:serviceParam wikibase:language \"",langsorder,"\"}
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
  SERVICE wikibase:label {bd:serviceParam wikibase:language \"", langsorder, "\".
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
                    schema:isPartOf [ wikibase:wikiGroup \"wikipedia\" ];
                    schema:inLanguage ?lang.\n",
  if (wikifilter != '') paste0("FILTER(?lang in (",wikifilter,")).") else "",
"}\n",
"} GROUP BY ?entity ?entityLabel ?entitylab ?entityDescription ?entitydesc ?sexLabel
  ?bdate ?bplaceLabel ?bactualplaceLabel ?bplaceCoord ?bcountryLabel
  ?ddate ?dplaceLabel ?dactualplaceLabel ?dplaceCoord ?dcountryLabel")
}

# Get a few personal properties from a Wikidata entity
w_EntityInfo_tiny_query <- function(qid, langsorder, wikifilter) {
  paste0('SELECT DISTINCT ?entity ?entityLabel (LANG(?entitylab) as ?entitylablang) ?entitylab
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
  SERVICE wikibase:label {bd:serviceParam wikibase:language \"",langsorder,"\"}
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
  SERVICE wikibase:label {bd:serviceParam wikibase:language \"", langsorder, "\".
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
                    schema:isPartOf [ wikibase:wikiGroup \"wikipedia\" ];
                    schema:inLanguage ?lang.\n",
                  if (wikifilter != '') paste0("FILTER(?lang in (",wikifilter,")).") else "",
                  "}\n",
                  "} GROUP BY ?entity ?entityLabel ?entitylab ?entityDescription ?entitydesc ?sexLabel
  ?bdate ?bplaceLabel ?bplaceCoord ?bcountryLabel
  ?ddate ?dplaceLabel ?dplaceCoord ?dcountryLabel")
}

# Gets films' properties from Wikidata
w_FilmInfo_query <- function(qid, langsorder, wikifilter) {
  paste0('SELECT DISTINCT ?entity ?entityLabel ?entityLabelES # ?entityDescription ?pubdate
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
  SERVICE wikibase:label {bd:serviceParam wikibase:language "', langsorder, '"}
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
  SERVICE wikibase:label {bd:serviceParam wikibase:language "', langsorder, '".
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
if (wikifilter != '') paste0(";
                    schema:inLanguage ?lang.
            FILTER(?lang in (",wikifilter,")).") else ".",
"}\n",
"} GROUP BY  ?entity ?entityLabel ?entityLabelES ?pubdate # ?entityDescription ")
}


# MediaWiki API ----
# See https://www.mediawiki.org/wiki/API:Main_page
# See https://en.wikipedia.org/w/api.php  (xtools.wmcloud.org)
# -----------------------------------------------------------.

#' Uses httr package to retrieve responses using the MediaWiki API.
#'
#' For MediaWiki requests only user_agent is necessary in the request headers.
#' See https://www.mediawiki.org/wiki/API:Etiquette. The standard and default
#' output format in MediaWiki is JSON. All other formats are discouraged. The
#' output format should always be specified using the request param "format"
#' in the "query" request.
#' See https://www.mediawiki.org/wiki/API:Data_formats#Output.
#' @param query A list with de (key, values) pairs with the search.
#' Note that if titles are included in the query, the MediaWiki API has a
#' limit of 50 titles en each query. In that case a error response is achieved.
#' @param project The Wikimedia project to search. Default en.wikipedia.org.
#' @param method The method used in the httr request. Default 'GET'.
#' Note in "https://www.mediawiki.org/wiki/API:Etiquette#Request_limit":
#' "Whenever you're reading data from the web service API, you should try to use
#'  GET requests if possible, not POST, as the latter are not cacheable."
#' @param attempts On ratelimit errors, the number of times the request is
#' retried using a 60 seconds interval between retries. Default 2. If 0 no
#' retries are done.
#' @return The response in JSON format or NULL on errors.
#' @author Angel Zazo, Department of Computer Science and Automatics, University of Salamanca
#' @importFrom httr GET POST content add_headers stop_for_status
#' @importFrom jsonlite fromJSON
reqMediaWiki <- function(query, project='en.wikipedia.org', method='GET',
                            attempts=2) {
  # Check for titles in query
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

#' normalizedTitle(title, q)
#' Return de normalized or redirect title (also normalized) from the query part
#' of the JSON response of a MediaWiki search that uses titles.
#' @param title the title to possibly found in q.
#' @param q (j$query) Query part of the JSON response from a Mediawiki search.
#' @return The normalized or redirect title found in q for title, else title
#' itself.
normalizedTitle <- function(title, q) {
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

#' checkTitles(titles)
#' Check if titles are valid. Return TRUE is all titles are valid, else FALSE.
#' See:See https://en.wikipedia.org/wiki/Wikipedia:Page_name#Technical_restrictions_and_limitations
#' @param titles A vector of titles to check.
checkTitles <- function(titles){
  if (length(titles) == 0)
    return(FALSE)
  #
  for (title in titles) { # Escaped REGEX: #<>[]|{} ---> #<>\\[\\]\\\{\\}
    if (grepl('[#<>\\[\\]|\\{\\}]', title)) {
      cat(paste0('ERROR: the title ', title, ' has a not allowed character\n'), file=stderr())
      return(FALSE)
    }
  }
  return(TRUE)
}

#' Open search of a string
#' 
#' Search string in the content of the project page using OpenSearch. Only in
#' namespace 0. Please, see https://www.mediawiki.org/wiki/API:Opensearch for
#' further information.
#' @param string String to search.
#' @param project Wikimedia project, defaults "en.wikipedio.org".
#' @param profile This parameter sets the search type: classic,
#' engine_autoselect (default), fast-fuzzy, fuzzy, fuzzy-subphrases, normal,
#' normal-subphrases, and strict.
#' @param redirects If redirects='return', the page title is the normalized one
#' (also the URL). If redirects='resolve", the page title is the normalized and
#' resolved redirection is in effect (also the URL). Note that in both cases the
#' API performs a NFC Unicode normalization on search string.
#' @return A data-frame of page titles and URL returned. If error, return Null.
#' @note Only for namespace 0. The function also obtains redirections for
#' disambiguation pages.
#' @examples
#' # Some search profiles:
#' df <- m_Opensearch(string='Duque de Alba', project='es.wikipedia.org',
#'                     profile="engine_autoselect", redirects="resolve")
#' df <- m_Opensearch(string='Duque de Alba', project='es.wikipedia.org', profile="strict")
#' df <- m_Opensearch(string='Duque de Alba', project='es.wikipedia.org', profile="fuzzy")
#' @author Angel Zazo, Department of Computer Science and Automatics, University of Salamanca
#' @export
m_Opensearch <- function(string, project='en.wikipedia.org',
                          profile="engine_autoselect", redirects="resolve") {
  #
  query = list(format        = 'json',
               formatversion = '2',
               action        = 'opensearch',
               namespace     = '0',
               limit         = 'max',  # "max" is 500, but in practice it is 100
               profile       = profile,
               redirects     = redirects,
               search        = string)
  #
  j <- reqMediaWiki(query, project)
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

#' Retrieve responses using the MediaWiki API.
#'
#' Use the MediaWiki API to check Wikipedia pages titles, get redirections of
#' Wikipedia pages, get image URL of Wikipedia pages or get URL of files in
#' Wikipedia pages
#' @param titles A vector of page titles to search for.
#' @param mode Select an action to perform:
#' 'wikidataEntity' -> 
#' Use reqMediaWiki to check if page titles are in a Wikimedia project and returns
#' the Wikidata entity for them. Automatically resolves redirects if parameter
#' redirects = TRUE (default). If a page title exists in the Wikimedia project,
#' the status column in the returned data-frame is set to 'OK'. If a page is a
#' disambiguation page, that column is set to 'disambiguation', and if a title
#' is not in the Wikimedia project, it is set to 'missing' and no Wikidata
#' entity is returned;
#' 'redirects' -> 
#' Obtains redirection of pages of the article titles in the Wikimedia project
#' restricted to namespace 0. Returns a vector for each title, in each vector the
#' first element is the page destiny, the rest are all pages that redirect to it. If
#' a title is not in the Wikimedia project its list is NA;
#' 'pagePrimaryImage' -> 
#' Return the URL of the image associated with the Wikipedia pages of the titles,
#' if pages has one. Automatically resolves redirects, the "normalized" column
#' of the returned data-frames contains the destiny page of the redirection.
#' See https://www.mediawiki.org/w/api.php?action=help&modules=query%2Bpageimages;
#' 'pageFiles' ->
#' Search for URL of files inserted in Wikipedia pages. Exclude extensions
#' in exclude_ext. Note that the query API named this search as 'images',
#' but all source files in the page are returned. The function only return URL
#' that not end with extensions in exclude_ext parameter (case insensitive).
#' Automatically resolves redirects, the "normalized" column of the returned
#' data-frame contains the destiny page of the redirection.
#' See https://en.wikipedia.org/w/api.php?action=help&modules=query%2Bimages
#' @param project Wikimedia project, defaults "en.wikipedia.org"
#' @param redirects If page redirects must be resolved. If redirects=TRUE
#' (default) then the "normalized" column of the returned data-frames contains
#' the destiny page title of the redirection. Only for mode=wikidataEntity.
#' @param exclude_ext File extensions excluded in results.
#' Only for mode=PageFiles. Default 'svg|webp|xcf'
#' @return depends on the mode selected:
#' 'wikidataEntity' Null if there is any error in response, else a data-frame
#' with four columns: first, the original page title string, second, the
#' normalized one, third, logical error=FALSE, if Wikidata entity exists for
#' the page, or error=TRUE it does not, last, the Wikidata entity itself or a
#' clarification of the error;
#' 'redirects' A vector for each title, with all pages that are redirects to the
#' first element;
#' 'pagePrimaryImage' A data-frame with original titles, normalized ones, the
#' status of the pages and the primary image of the page or NA if it does not
#' exist;
#' 'pageFiles' A data-frame with original titles, the normalized ones, status
#' for the page and the URL files of the Wikipedia pages, using use "|" to
#' separate ones) or NA if files do not exits or are excluded.
#' @examples
#' # Note that URLdecode("a%CC%8C") is
#' # the letter "a" with the combining caron
#' df <- m_reqMediaWiki(c('Max Planck', URLdecode("a%CC%8C"), 'Max', 'Cervante', 'humanist'),
#'                     mode='wikidataEntity', project='en.wikipedia.org')
#' a <- m_reqMediaWiki(c('Cervantes', 'Planck', 'Noexiste'), mode='redirects',
#'                     project='es.wikipedia.org')
#' i <- m_reqMediaWiki(c('Max Planck', URLdecode("a%CC%8C"), 'Max', 'Cervante', 'humanist'),
#'                     mode='pagePrimaryImage')
#' f <- m_reqMediaWiki(c('Max Planck', URLdecode("a%CC%8C"), 'Max', 'Cervante', 'humanist'),
#'                     mode='pageFiles', exclude_ext = "svg|webp|xcf")
#' @author Angel Zazo, Department of Computer Science and Automatics, University of Salamanca
#' @export
m_reqMediaWiki <- function(titles,
    mode=c('wikidataEntity','redirects','pagePrimaryImage','pageFiles'),
    project='en.wikipedia.org', redirects=TRUE, exclude_ext='svg|webp|xcf'){
  mode <- mode[1]
  if(mode=='wikidataEntity'){
    return(m_WikidataEntity(titles,project,redirects))
  }else if(mode=='redirects'){
    return(m_Redirects(titles,project))
  }else if(mode=='pagePrimaryImage'){
    return(m_PagePrimaryImage(titles,project))
  }else if(mode=='pageFiles'){
    return(m_PageFiles(titles,project,exclude_ext))
  }else{
    stop("mode: wrong mode specified")
  }
}

# Check Wikipedia pages titles
m_WikidataEntity <- function(titles, project='en.wikipedia.org',
                               redirects=TRUE) {
  if (!checkTitles(titles))
    return(NULL)
  #
  # Check the limit on number of titles of Wikidata API (50 titles)
  n <- length(titles)
  nlimit <- 50
  if (n > nlimit) {
    cat(paste0("INFO: The number of titles (", n,") exceeds Wikipedia API limit (",
               nlimit,"): doing chunked queries.\n"),
        file = stderr())
    nlim <- as.integer(n/nlimit)
    for (k in 0:nlim) {
      offset <- nlimit * k
      if ((offset+1) > n)
        break
      t_list <- titles[(offset+1):min(n,offset+nlimit)]
      cat(paste0(" INFO: Getting information of entities from ", offset+1," to ", offset+length(t_list), '.\n'),
          file = stderr())
      aux <- m_WikidataEntity(t_list, project, redirects)
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
  if (redirects)
    query["redirects"] = 1
  #
  # vectors for loading the return information
  normalized <- character()
  status     <- character()
  entity     <- character()

  repeat {
    j <- reqMediaWiki(query, project)
    #
    if (is.null(j) | is.null(j$query)){
      cat("Error in m_WikidataEntity: reqMediaWiki returns an improper JSON.",
          file = stderr())
      return(NULL)
    }
     #
    q <- j$query
    #
    for (a in titles) {
      anorm <- normalizedTitle(a, q)
      # Check if a different title with identical previously normalized title
      # is already in list. It's necessary for stability on number of elements.
      index <- match(anorm, normalized)
      if (!is.na(index)) {
        cat(paste0("   INFO: '",anorm,"' there was already in list.\n"), file = stderr())
        normalized <- append(normalized, anorm)
        status     <- append(status, status[index])
        entity     <- append(entity, entity[index])
        next
      }
      #
      for (i in 1:length(q$pages)) { # With formatversion=2, j$json$query$pages is a vector
        page <- q$pages[[i]]
        item <- NA
        if (anorm == page$title) {
          aerror <- TRUE
          if (!is.null(page$invalid))   # ¿malformed title?
            s <- 'invalid'
          else if (!is.null(page$missing))
            s <- 'missing'
          else if (is.null(page$pageprops))
            s <- 'no_pagepros'
          else {
            if (is.null(page$pageprops$wikibase_item))
              s <- "no_wikibase_item"
            else
              item <- page$pageprops$wikibase_item
              s <- ifelse(!is.null(page$pageprops$disambiguation), 'disambiguation', 'OK')
          }
          # Feed the output vectors
          normalized <- append(normalized, anorm)
          status     <- append(status, s)
          entity     <- append(entity, item)
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
                           status=status, entity=entity)    }
      return(output)
  }
}

# Get redirections of Wikipedia pages
m_Redirects <- function(titles, project="en.wikipedia.org") {
  if (!checkTitles(titles))
    return(NULL)
  #
  # Check the limit on number of titles of Wikidata API (50 titles)
  n <- length(titles)
  nlimit = 50
  if (n > nlimit) {
    cat(paste0("INFO: The number of titles (", n,") exceeds Wikipedia API limit (",
               nlimit,"): doing chunked queries.\n"),
        file = stderr())
    nlim <- as.integer(n/nlimit)
    for (k in 0:nlim) {
      offset <- nlimit * k
      if ((offset+1) > n)
        break
      t_list <- titles[(offset+1):min(n,offset+nlimit)]
      cat(paste0(" INFO: Getting redirects of entities from ", offset+1," to ", offset+length(t_list), '.\n'),
          file = stderr())
      aux <- m_Redirects(t_list, project)
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
    j <- reqMediaWiki(query, project)
    #
    if (is.null(j) | is.null(j$query)){
      cat("Error in m_Redirects: reqMediaWiki returns an improper JSON.",
          file = stderr())
      return(NULL)
    }
    #
    r <- j$query
    #
    for (a in titles) {
      anorm <- normalizedTitle(a, r)
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

# Get image URL of Wikipedia pages
m_PagePrimaryImage <- function(titles, project="en.wikipedia.org") {
  if (!checkTitles(titles))
    return(NULL)
  #
  # Check the limit on number of titles of Wikidata API (50 titles)
  n <- length(titles)
  nlimit = 50
  if (n > nlimit) {
    cat(paste0("INFO: The number of titles (", n,") exceeds Wikipedia API limit (",
               nlimit,"): doing chunked queries.\n"),
        file = stderr())
    nlim <- as.integer(n/nlimit)
    for (k in 0:nlim) {
      offset <- nlimit * k
      if ((offset+1) > n)
        break
      t_list <- titles[(offset+1):min(n,offset+nlimit)]
      cat(paste0(" INFO: Getting primary images of entities from ", offset+1," to ", offset+length(t_list), '.\n'),
          file = stderr())
      aux <- m_PagePrimaryImage(t_list, project)
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
               prop          = 'pageimages', # return URL and dimensions
               piprop        = 'original',
               pilimit       = 'max',        # max(=50): Change to 10 for testing
               titles        = paste0(titles, collapse = '|'))
  # vectors for loading the returned information
  normalized <- character()
  status     <- character()
  pageimage  <- character()
  #
  repeat {
    j <- reqMediaWiki(query, project)
    #
    if (is.null(j) | is.null(j$query)){
      cat("Error in m_PagePrimaryImage: reqMediaWiki returns an improper JSON.",
          file = stderr())
      return(NULL)
    }
    #
    q <- j$query
    #
    for (a in titles) {
      anorm <- normalizedTitle(a, q)
      # check if a normalized register is just loaded
      # If continue response, not overwrite previous search
      index <- match(anorm, normalized)
      if (!is.na(index)) {
        normalized <- append(normalized, anorm)
        status     <- append(status, status[index])
        pageimage  <- append(pageimage, pageimage[index])
        next
      }
      #
      # With formatversion=2, j$json$query$pages is a vector
      #for (page in q$pages) {
      for (i in 1:length(q$pages)) {
        page <- q$pages[[i]]
        if (anorm == page$title) {
          image <- NA
          if (!is.null(page$invalid))
            s <- 'invalid'
          else if (!is.null(page$missing))
            s <- 'missing'
          else if (is.null(page$pageid))
            s <- 'no pageid'
          else
            s <- 'OK'
          #
          if (!is.null(page$original) & !is.null(page$original$source))
            image <- page$original$source
          # Feed the output variables
          normalized <- append(normalized, anorm)
          status     <- append(status, s)
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
                           status=status, pageimage=pageimage)
      return(output)
    }
  }
}

# Get URL of files in Wikipedia pages
m_PageFiles <- function(titles, project = "en.wikipedia.org",
                         exclude_ext = 'svg|webp|xcf') {
  if (!checkTitles(titles))
    return(NULL)
  #
  exts <- strsplit(tolower(exclude_ext), "|", fixed=T)[[1]]
  #
  # Check the limit on number of titles of Wikidata API (50 titles)
  n <- length(titles)
  nlimit = 50
  if (n > nlimit) {
    cat(paste0("INFO: The number of titles (", n,") exceeds Wikipedia API limit (",
               nlimit,"): doing chunked queries.\n"),
        file = stderr())
    nlim <- as.integer(n/nlimit)
    for (k in 0:nlim) {
      offset <- nlimit * k
      if ((offset+1) > n)
        break
      t_list <- titles[(offset+1):min(n,offset+nlimit)]
      cat(paste0(" INFO: Getting inserted files of entities from ", offset+1," to ", offset+length(t_list), '.\n'),
          file = stderr())
      aux <- m_PageFiles(t_list, project, exclude_ext)
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
  status     <- character()
  pageimages <- character()
  #
  repeat {
    j <- reqMediaWiki(query, project)
    #
    if (is.null(j) | is.null(j$query)){
      cat("Error in m_PageFiles: reqMediaWiki returns an improper JSON.",
          file = stderr())
      return(NULL)
    }
    #
    q <- j$query
    #
    for (a in titles) {
      anorm <- normalizedTitle(a, q)
      # check if a normalized register is just loaded
      # If continue response, not overwrite previous search
      index <- match(anorm, normalized)
      if (!is.na(index)) {
        normalized <- append(normalized, anorm)
        status     <- append(status, status[index])
        pageimages <- append(pageimages, pageimages[index])
        next
      }
      #
      # With formatversion=2, j$json$query$pages is a vector
      #for (page in q$pages) {
      for (i in 1:length(q$pages)) {
        page <- q$pages[[i]]
        images <- NA
        if (anorm == page$title) {
          if (!is.null(page$invalid))
            s <- 'invalid'
          else if (!is.null(page$missing))
            s <- 'missing'
          else if (is.null(page$pageid))
            s <- 'no pageid'
          else
            s <- 'OK'
          #
          if (!is.null(page$images)) {
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
            if (length(images) == 0)  # some images may be in exclude_ext
              images <- NA
            else
              images <- paste0(images, collapse = '|')
          }
          # Feed the output variables
          normalized <- append(normalized, anorm)
          status     <- append(status, s)
          pageimages <- append(pageimages, images)
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
                           status=status, pageimages=pageimages)
      return(output)
    }
  }
}


# WikiMedia API ----
# Used to obtain metrics of Wikipedia pages
# See https://www.mediawiki.org/wiki/Wikimedia_REST_API
# See https://en.wikipedia.org/api/rest_v1/
# See https://www.mediawiki.org/wiki/XTools/API/Page (xtools.wmcloud.org)
#
# Note that this API uses "article" to refer a page title. We also use this
# approach. In this API only one article is allowed in each request.
# -------------------------------------------------------------------------.

#' httrGetJSON
#' Retrieve responses in JSON format using httr::GET. It is a generic function
#' to use for request these Wikimedia metrics API:
#'   https://wikimedia.org/api/rest_v1/
#'   https://www.mediawiki.org/wiki/XTools/API/Page (xtools.wmflabs.org)
#' @param url The URL with the query to the API.
#' @return A JSON response. Please check httr::stop_for_status(response)
#' @author Angel Zazo, Department of Computer Science and Automatics, University of Salamanca
#' @importFrom httr GET user_agent add_headers
#' @note Used in m_Pageviews
httrGetJSON <- function(url) {
  # URL must be encode
  httr::GET(
    url = URLencode(url),
    httr::user_agent(user_agent),
    httr::add_headers(Accept = "application/json")
  )
}

#' Get number of views of a Wikipedia article
#' 
#' Use the Wikimedia REST API (https://wikimedia.org/api/rest_v1/) to get the
#' number of views one article has in a Wikimedia project in a date interval
#' (see granularity). If redirect=TRUE, then get the number of views of all
#' articles that redirects to the article which is the destiny of actual page.
#' @param article The title of the article to search. Only one article is allowed.
#' @param start,end First and last day to include (format YYYYMMDD or YYYYMMDDHH)
#' @param project The Wikimedia project, defaults en.wikipedia.org
#' @param access  Filter by access method: all-access (default), desktop, mobile-app, mobile-web
#' @param agent   Filter by agent type: all-agents, user (default), spider, automated
#' @param granularity  Time unit for the response data: daily, monthly (default)
#' @param redirects Boolean to include the views of all redirections of the page
#' (defaults: False). If redirects=TRUE then the "normalized" element of the
#' returned vector contains the destiny of the redirection, and the "original"
#' element contains the original title of the article.
#' If a page is just a destiny of other pages, and you want to know the total
#' number of views that page have (including views of redirections), it is also
#' necessary set redirects=TRUE,  otherwise only you have the views of that page.
#' @return A vector with the number of visits by granularity.
#' @examples
#' v <-  m_Pageviews(article="Cervantes", start="20230101", end="20230501",
#'                    project="es.wikipedia.org", granularity="monthly")
#' vv <- m_Pageviews(article="Cervantes", start="20230101", end="20230501",
#'                    project="es.wikipedia.org", granularity="monthly",
#'                    redirects=TRUE)
#' @author Angel Zazo, Department of Computer Science and Automatics, University of Salamanca
#' @importFrom httr stop_for_status content
#' @importFrom jsonlite fromJSON
#' @export
m_Pageviews <- function(article, start, end, project="en.wikipedia.org",
                         access="all-access", agent="user",
                         granularity="monthly", redirects=FALSE) {
  # httrGetJSON_rated: the limitated version of httrGetJSON.
  # Limit is 100 req/s (See https://wikimedia.org/api/rest_v1/#/Pageviews%20data)
  httrGetJSON_rated <- limitRequester(httrGetJSON, n=100, period=1)
  article <- gsub(" ", "_", article)
  url <- "https://wikimedia.org/api/rest_v1/metrics/pageviews/per-article"
  url <- paste(url, project, access, agent, article, granularity, start, end, sep="/", collapse = "")
  a <- list()
  a[['original']] <- article
  if (redirects) {
    arts <- m_Redirects(article, project)   # vector of lists
    arts <- arts[[article]]
    article <- arts[1]
    a[['normalized']] <- article
  }
  if (redirects == TRUE) {
    for (art in arts) {
      if (length(arts) > 1)
        cat(paste0(" INFO: Getting the number of views of '", art,"' page.\n"), file=stderr())
      b <- m_Pageviews(art, start, end, project, access, agent,
                        granularity, redirects=FALSE)
      for (n in union(names(a), names(b))) {
        if (!(n %in% c('original', 'normalized')))
          a[n] <- ifelse(n %in% names(a), a[n][[1]], 0) + ifelse(n %in% names(b), b[n][[1]], 0)
      }
    }
  }
  else {
    r <- httrGetJSON_rated(url)
    httr::stop_for_status(r)
    content <- httr::content(r, as = "text", encoding = "UTF-8")
    j <- jsonlite::fromJSON(content, simplifyVector = FALSE)
    for (v in j$items)
      a[v$timestamp] <- v$views
  }
  return(a)
}


#' Gets various information from a Wikimedia page
#' 
#' Obtains information in JSON format about an article in the Wikimedia project
#' or NULL on errors. Use the wmflabs API. The XTools Page API endpoints offer data
#' related to a single page. See https://www.mediawiki.org/wiki/XTools/API/Page.
#' The URL of the API starts with 'https://xtools.wmcloud.org/api/page/'
#' @param article The title of the article to search. Only one article is allowed.
#' @param project The Wikimedia project, defaults en.wikipedia.org.
#' @param infotype The type of information to request: articleinfo, prose, links.
#' You also can type 'all' to retrieve all.
#' Note that the API also offer theses options: top_editors, assessments,
#' bot_data and automated_edits.
#' @param redirects If redirects==TRUE, then the information is obtained
#' of the destiny of the page. In that case, then the "original" element of
#' the returned list contains the original page, and the "page" element the
#' destiny page. Also, if infotype=='links, the sum of the in-links of all
#' redirections is assigned to links_in_count.
#' @return A list with the information about the article.
#' @examples
#' \dontrun{
#' x <-  m_XtoolsInfo(article="Cervantes", infotype="articleinfo", project="es.wikipedia.org")
#' xx <- m_XtoolsInfo(article="Cervantes", infotype="articleinfo", project="es.wikipedia.org",
#'                    redirects=TRUE)
#' 
#' y <-  m_XtoolsInfo(article="Miguel de Cervantes", infotype="links", project="es.wikipedia.org")
#' yy <- m_XtoolsInfo(article="Cervantes", infotype="links", project="es.wikipedia.org",
#'                     redirects=TRUE)

#' z  <- m_XtoolsInfo(article="Miguel de Cervantes", infotype="all", project="es.wikipedia.org")
#' zz <- m_XtoolsInfo(article="Cervantes", infotype="all", project="es.wikipedia.org",
#'                        redirects=TRUE)
#' }
#' @author Angel Zazo, Department of Computer Science and Automatics, University of Salamanca
#' @importFrom httr stop_for_status content
#' @importFrom jsonlite fromJSON
#' @export
m_XtoolsInfo <- function(article, infotype=c("articleinfo", "prose", "links"),
                          project="en.wikipedia.org", redirects=FALSE) {
  infotype <- infotype[1]
  if(infotype=="all"){
    return(m_XtoolsInfoAll(article,project,redirects))
  }else{
    return(m_XtoolsInfoDefault(article,infotype,project,redirects))
  }
}

# Gets various information from a Wikimedia page
m_XtoolsInfoDefault <- function(article, infotype=c("articleinfo", "prose", "links"),
                          project="en.wikipedia.org", redirects=FALSE){
  if(!(infotype %in% c("articleinfo", "prose", "links"))){
    stop("infotype: wrong infotype specified")
  }
  # httrGetJSON_rated: the limitratedr version of httrGetJSON.
  # Limit is 100 req/s.
  httrGetJSON_rated <- limitRequester(httrGetJSON, n=100, period=1)
  originalarticle <- article
  if (redirects) {
    arts <- m_Redirects(article, project)   # vector of lists
    arts <- arts[[article]]
    article <- arts[1]
    cat(paste0(" INFO: Getting information of the '", article,"' page.\n"), file=stderr())
  }
  #
  url <- 'https://xtools.wmcloud.org/api/page'
  url <- paste(url, infotype, project, article, sep="/", collapse = "")
  r <- httrGetJSON_rated(url)
  httr::stop_for_status(r)
  content <- httr::content(r, as = "text", encoding = "UTF-8")
  d <- jsonlite::fromJSON(content, simplifyVector = FALSE)
  #
  if (infotype == "links" & redirects) {
    l <- length(arts)
    for (i in 2:l) {
      cat(paste0("  INFO: Adding the number of links_in_count of the '", arts[i],"' page.\n"), file=stderr())
      b <- m_XtoolsInfo(arts[i], infotype="links", project=project,
                         redirects=FALSE)
      d["links_in_count"] <- d["links_in_count"][[1]] + b["links_in_count"][[1]]
      d["elapsed_time"] <- d["elapsed_time"][[1]] + b["elapsed_time"][[1]]
    }
  }
  if (redirects)
    d['original'] <- originalarticle
  return(d)
}

# Gets more diverse information from a Wikimedia page
m_XtoolsInfoAll <- function(article, project="en.wikipedia.org",
                             redirects=FALSE){
  # first: articleinfo
  r <- m_XtoolsInfo(article, infotype='articleinfo', project=project,
                     redirects=redirects)
  # Error in response
  if (!is.null(r$error)) {
    cat(paste0("Error in response from m_XtoolsInfo: ", r$error, '.\n'),
        file = stderr())
    return(NULL)
  }
  # Second: prose
  b <- m_XtoolsInfo(article, infotype="prose", project=project,
                     redirects=redirects)
  for (n in names(b))
    r[n] <- b[n]
  # Third: links
  c <- m_XtoolsInfo(article, infotype="links", project=project,
                     redirects=redirects)
  for (n in names(c))
    r[n] <- c[n]
  #
  return(r)
}


# VIAF API ----
# https://www.oclc.org/developer/api/oclc-apis/viaf/authority-cluster.en.html
# --------------------------------------------------------------------------.

#' Suggests VIAF id from a name
#' 
#' Search the name of the author from the VIAF AutoSuggest API and returns
#' information in JSON format of the records found. Note that only
#' returns a maximum of 10 records. Note that those records are not
#' VIAF cluster records.
#' A VIAF record is considered a "cluster record," which is the result of
#' combining records from many libraries around the world into a single record.
#' @param author String to search. Please, see the structure of the author
#' string to obtain better results:
#'   author: last name, first name\[,\] \[(\[year_of_bird\]\[-year_of_death\])\]
#' @return A data-frame with four columns from the elements "term", "score",
#' "nametype" and "viafid" of the Autosuggest API response.
#' @seealso https://www.oclc.org/developer/api/oclc-apis/viaf/authority-cluster.en.html
#' @examples
#' v_AutoSuggest('Iranzo')
#' v_AutoSuggest('Esparza, María')
#' # Four rows, only two viafid:
#' v_AutoSuggest('Escobar, Modesto')
#' @export
v_AutoSuggest <- function(author) {
  url <- "https://www.viaf.org/viaf/AutoSuggest"
  query <- list(query = author)
  #
  tryCatch(
    {
      r <- httr::GET(url = url,
                     httr::user_agent(user_agent),
                     query = query)
      httr::stop_for_status(r)
      content <- httr::content(r, as = "text", encoding = "UTF-8")
      d <- jsonlite::fromJSON(content, simplifyVector = FALSE)
      # Only return (as data-frame) this columns: "term", "score", "nametype", "viafid"
      output <- t(sapply(d$result, function(x) x[c("term", "score", "nametype", "viafid")]))
      return(output)
    }, error = function(e) {
      cat(as.character(e), file = stderr())
      return(NULL)
    }
  )
}

#' Run a CQL Query in VIAF
#' 
#' Run the CQL_Query using the VIAF Search API and return a list of records
#' found. The search string is formed using the CQL_Query syntax of the API.
#' Note that returned records use the "info:srw/schema/1/JSON" record schema,
#' i.e., are complete cluster records packed in JSON format. If the number
#' of records found is greater than 250 (API restrictions), successive requests
#' are made.
#' @param CQL_Query String with the search or a name if mode is specified.
#' See https://www.oclc.org/developer/api/oclc-apis/viaf/authority-cluster.en.html
#' @param mode apply a predefined query:
#' 'anyField' -> 'cql.any = "string"'
#' Search preferred Name - names which are the preferred form in an authority
#' record (1xx fields of the MARC records);
#' 'allmainHeadingEl' -> 'local.mainHeadingEl all "name"'
#' Search the same as previous, but all terms are searched;
#' 'allNames' -> 'local.names all "name"'
#' Search Names - any name preferred or alternate (1xx, 4xx, 5xx fields of the
#' MARC records);
#' 'allPersonalNames' -> 'local.personalNames all "name"'
#' Search Personal Names within the authority record (100, 400, 500 fields of
#' MARC records);
#' 'allTitle' -> 'local.title all "title"'
#' Search for titles.
#' By 'default', no predefined query will be applied.
#' @return A list with the records found.
#' @examples
#' \dontrun{
#' ## Search in any field (cql.any)
#' # Operator is "=": so search one or more terms:
#' CQL_Query <- 'cql.any = "García Iranzo, Juan"'
#' r <- v_Search(CQL_Query)
#' # r contains complete VIAF records (sometimes seen as a "cluster record",
#' # which is unified by combining records from many libraries around the world)

#' # Search in 1xx, 4xx, 5xx fields of MARC record (local.names)
#' # Operator is "all": search all terms
#' CQL_Query <- 'local.names all "Modesto Escobar"'
#' r <- v_Search(CQL_Query)
#' 
#' # Search in 100, 400, 500 fields of MARC record (local.personalNames)
#' # Operator is "all": search all terms
#' CQL_Query <- 'local.personalNames all "Modesto Escobar"'
#' r <- v_Search(CQL_Query)
#' 
#' # Search in Titles
#' CQL_Query <- 'local.title all "Los pronósticos electorales con encuestas"'
#' r <- v_Search(CQL_Query)
#' }
#' @export
v_Search <- function(CQL_Query, mode=c('default', 'anyField', 'allmainHeadingEl', 'allNames', 'allPersonalNames', 'allTitle')) {
  mode <- mode[1]
  if(mode=='anyField'){
    CQL_Query <- paste0("cql.any = ", CQL_Query)
  }else if(mode=='allmainHeadingEl'){
    CQL_Query <- paste0("local.mainHeadingEl all ", CQL_Query)
  }else if(mode=='allNames'){
    CQL_Query <- paste0("local.names all ", CQL_Query)
  }else if(mode=='allPersonalNames'){
    CQL_Query <- paste0("local.personalNames all ", CQL_Query)
  }else if(mode=='allTitle'){
    CQL_Query <- paste0("local.title all ", CQL_Query)
  }

  searchFn <- function(cql_query){
  maxrecords <- 250
  url <- "https://www.viaf.org/viaf/search"
  query <- list(httpAccept = 'application/json',
                maximumRecords = maxrecords,
                # recordSchema = paste0('http://viaf.org/', recordSchema),
                recordSchema = 'info:srw/schema/1/JSON',
                startRecord = 1,
                query = cql_query)
  #
  tryCatch(
    {
      output <- list()
      while(TRUE) {
        r <- httr::GET(url = url,
                       httr::user_agent(user_agent),
                       query = query)
        httr::stop_for_status(r)
        # cat(r$url)
        content <- httr::content(r, as = "text", encoding = "UTF-8")
        d <- jsonlite::fromJSON(content, simplifyVector = FALSE)
        nrecords <- d$searchRetrieveResponse$numberOfRecords
        records  <- d$searchRetrieveResponse$records
        output <- append(output, records)
        if (length(output) >= as.integer(nrecords)) {
          if (query$startRecord > 1)
            cat(paste0(' INFO: Retrieved ', length(output), ' records.\n'), file = stderr())
          return(output)
        }
        if (query$startRecord == 1)
           cat(paste0("INFO: Number of records found (", nrecords,
                      ") excedes the maximun per request API limit (", maxrecords,
                      "). Doing sucesivelly requests.\n"), file = stderr())
        cat(paste0(' INFO: Retrieved ', length(output), ' records.\n'), file = stderr())
        query$startRecord <- query$startRecord + maxrecords
      }
    }, error = function(e) {
      cat(as.character(e), file = stderr())
      return(NULL)
    }
  )
  }

  return(searchFn(CQL_Query))
}


#' Gets record clusters
#' 
#' Obtains the record cluster identified by viafid from VIAF, in the format
#' indicated in record_format. Note that the returned record may be a VIAF
#' cluster record or a redirect/scavenged record: the function returns the
#' record as is.
#' @param viafid The VIAF identifier.
#' @param record_format 'viaf.json' (default) or others in
#' https://www.oclc.org/developer/api/oclc-apis/viaf/authority-cluster.en.html.
#' @return The VIAF record cluster in the format indicated in record_format.
#' @export
v_GetRecord <- function(viafid, record_format='viaf.json') {
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

#' Gets information from a VIAF record
#' 
#' Returns information from the VIAF record. Note that the VIAF record musts
#' be in JSON format.
#' @param viaf VIAF cluster record (in JSON format).
#' @param info is mandatory to select which information you want to retrieve.
#' The options are 'titles', 'gender', 'dates', 'occupations', 'sources',
#' 'sourceId' or 'wikipedias'.
#' @param source the identifier of the source (LC, WKP, JPG, BNE...)
#' Only if info=sourceId.
#' @return depends on the info selected:
#' 'titles' A list with titles;
#' 'gender' The gender of the author o NULL if not exits in the record;
#' 'dates' The bird year and death year in format byear:dyear;
#' 'occupations' A data-frame with sources and occupations from each source or NULL if
#' occupations do not exist in the record;
#' 'sources' A data-frame with text and sources;
#' 'sourceId' A data-frame with columns text and source, or NULL if the source does
#' no exist in the viaf record;
#' 'wikipedias' A vector with the URL of the Wikipedias.
#' @export
v_Extract <- function(viaf, info, source=NULL){
  opts <- c('titles', 'gender', 'dates', 'occupations', 'sources', 'sourceId', 'wikipedias')
  if(!(info %in% opts)){
    stop(paste0("info: must select an option between '",paste0(opts,collpase="', '"),"'"))
  }

  if(info=="sourceId"){
    if(is.null(source)){
      stop("source: the identifier of the source must be supplied (LC, WKP, JPG, BNE...)")
    }
    return(v_sourceId(viaf,source))
  }else{
    retrieveFn <- get(paste0('v_',info))
    return(retrieveFn(viaf))
  }
}

# Gets works titles from a VIAF record
v_titles <- function(viaf) {
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

# Gets author's gender from a VIAF record
v_gender <- function(viaf) {
  if (is.null(viaf$fixed))
    return(NULL)
  if (viaf$fixed$gender == 'a')
    return('female')
  else if (viaf$fixed$gender == 'b')
    return('male')
  else
    return(viaf$fixed$gender)
}

# Gets bird and death years from a VIAF record
v_dates <- function(viaf) {
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

# Gets occupations from a VIAF record
v_occupations <- function(viaf) {
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

# Gets text of all sources from a VIAF record
v_sources <- function(viaf) {
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

# Gets text and identifier of the source from a VIAF record
v_sourceId <- function(viaf, source) {
  texts <- v_sources(viaf)
  if (is.null(texts[[source]]))
    return(NULL)
  else
    return(texts[!is.na(texts[[source]]), c('text', source)])
}

# Gets Wikipedia pages (URL) from a VIAF record
v_wikipedias <- function(viaf) {
  if (is.null(viaf$xLinks) | is.null(viaf$xLinks$xLink))
    return(NULL)
  #
  wikis <- character()
  xLinks <- viaf$xLinks$xLink
  if (!is.null(xLinks$`#text`)) {
    url <- xLinks[['#text']]
    if (grepl('https?://[^.]+.wikipedia.org', url))
      wikis <- append(wikis, url)
  }
  else {
    for (l in xLinks) {
      url <- l[['#text']]
      if (grepl('https?://[^.]+.wikipedia.org', url))
        wikis <- append(wikis, url)
    }
  }
  if (length(wikis) == 0)
    return(NULL)
  return(wikis)
}
