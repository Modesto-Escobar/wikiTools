#### wiki_utils.R
#### Angel Zazo <angelzazo@usal.es>
#### ver. 0.0 2021-11-09
#### ver. 0.1 2023-05-12
#### ver. 0.2 2023-07-27
#### ver. 0.3 2023-09-19
#### ver. 1.2.5 2024-04-18

# General user_agent header for Wikimedia, Wikidata, MediaWiki and VIAF requests ----
#' See https://meta.wikimedia.org/wiki/User-Agent_policy
#'     https://www.mediawiki.org/wiki/API:Etiquette
user_agent <- paste('wikiTools Package, ', R.version.string)

# In MediaWiki API specifying titles through titles or pageids in the query API
# is limited to 50 titles per query, or 500 for those with the "apihighlimits" right.
# See https://www.mediawiki.org/wiki/API:Query#Additional_notes.
MW_LIMIT <- 50

# VIAF API restriction is 250 maximum returned records.
# See https://developer.api.oclc.org/viaf-api#/Authority%20Cluster
VIAF_LIMIT <- 250

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

#' Execute a function in chunks.
#'
#' Execute the function f(x,...) in chunks of chunk-size elements each.
#' Wikidata and Wikimedia API have limits to execute a query. Wikidata has
#' timeout limits, Wikimedia about the number of titles or pageIds. This function
#' executes sequentially the function `f` over chunks of elements to prevent
#' errors.
#' @param f The function to execute.
#' @param x Vector of entities or titles/pageids.
#' @param chunksize The number of elements in `x` to execute the function.
#' @param ... The `f` arguments.
#' @return The results of execute `f` using all values of `x`
#' @author Angel Zazo, Department of Computer Science and Automatics, University of Salamanca
doChunks <- function(f, x, chunksize, ...){
  l <- list(...)  # to obtain `debug` parameter
  n <- length(x)
  nlim <- as.integer(n/chunksize)
  output <- NULL
  for (k in 0:nlim){
    offset <- k*chunksize
    if ((offset+1) > n)
      break
    x_vect <- x[(offset+1):min(n,offset+chunksize)]
    if (l$debug !=FALSE)
      cat(paste0("  INFO: Executing the function on elements from ", offset+1,
                 " to ", offset+length(x_vect)),"\n", file=stderr())
    d <- f(x_vect, nlimit=chunksize, ...)
    if (k==0)
      output <- d
    else
      output <- rbind(output, d)
  }
  return(output)
}

#' Check if all Wikidata entities in entity_list have valid values
#'
#' Return a vector of entities with duplicates or void entities removed. A valid
#' entity is a wikibase item (Qxxx, x is a digit) or a wikibase property (Pxxx).
#' @param entity_list A vector with the Wikidata entities.
#' @return The list of entities or raise an error.
#' @author Angel Zazo, Department of Computer Science and Automatics, University of Salamanca
checkEntities <- function(entity_list) {
  # Trim and remove ''
  entity_list <- trimws(entity_list)
  entity_list <- entity_list[!entity_list=='']
  p <- grepl("^(?:Q|P)\\d+$", entity_list)
  if (!all(p)) {
    stop(paste0("ERROR: Detected incorrect entities: ", paste0(entity_list[!p], collapse = ', ')))
  }
  return(unique(entity_list))
}

# WikiData Query Service ----
# See https://query.wikidata.org/
# See https://www.wikidata.org/wiki/Wikidata:SPARQL_tutorial
# See https://www.mediawiki.org/wiki/Wikidata_Query_Service/User_Manual
# --------------------------------------------------------------------.

#' Get responses from Wikidata Query Service
#'
#' Retrieve responses from Wikidata Query Service (WDQS)
#' @param sparql_query A string with the query in SPARQL language (SELECT query).
#' @param format A string with the query response format, mandatory. See
#' https://www.mediawiki.org/wiki/Wikidata_Query_Service/User_Manual#SPARQL_endpoint.
#' Only  'json', 'xml' or 'csv' formats are allowed, default 'json'.
#' @param method The method used in the httr request, GET or POST, mandatory.
#' Default 'GET'. Use 'POST' method for long SELECT clauses.
#' @return The response in the format selected. Please check httr::stop_for_status(response)
#' @author Angel Zazo, Department of Computer Science and Automatics, University of Salamanca
#' @importFrom httr GET POST user_agent add_headers
#' @note For short queries GET method is better, POST for long ones. Only GET queries as cached.
reqWDQS <- function(sparql_query, format='json', method='GET') {
  if (format=='json')      wdqs_format <- "application/sparql-results+json"
  else if (format=='xml')  wdqs_format <- "application/sparql-results+xml"
  else if (format=='csv')  wdqs_format <- "text/csv"
  else stop(paste0("ERROR: format '", format, "' is not supported."))

  url <- 'https://query.wikidata.org/sparql'
  if(!curl::has_internet() || httr::http_error(url)){
    message("No internet connection or data source broken.")
    return(NULL)
  }

  if (method=='GET')
    httr::GET(
      url = url,
      query = list(query = sparql_query),
      httr::user_agent(user_agent),
      httr::add_headers(Accept = wdqs_format)
    )
  else if (method=='POST')
    httr::POST(
      url = url,
      body = list(query = sparql_query),
      httr::add_headers(Accept = wdqs_format, user_agent = user_agent),
      encode = 'form'
    )
  else stop(paste0("ERROR: method '", method, "' is not supported."))
}

#' Response from Wikidata Query Service
#'
#' Retrieve responses from Wikidata Query Service (WDQS). Uses ratelimitr if
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
      if(is.null(r)){
        return(NULL)
      }
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
#' `instanceof` Wikidata entity class. For example, if instanceof="Q5", check if
#' entities are instances of the Wikidata entity class Q5, i.e, are humans.
#' Some entity classes are allowed, separately by '|'; in this case, the OR
#' operator is considered. If instanceof='' then no filter is applied: the
#' function returns all Wikidata entities class of which each of the entities in
#' the list are instances.
#' Duplicated entities are deleted before search.
#' Note that no labels or descriptions of the entities are returned. Please, use
#' function `w_LabelDesc` for this.
#'
#' @param entity_list A vector with the Wikidata entities.
#' @param instanceof The Wikidata class to check, mandatory. Some entity classes
#' separated by '|' are allowed, in this case, the OR operator is considered.
#' @param nlimit If the number of entities exceeds this number, chunked queries
#' are done. This is the number of entities requested in each chunk. Please,
#' reduce the default value if error is raised.
#' @param debug For debugging purposes (default FALSE). If debug='info'
#' information about chunked queries is shown. If debug='query' also the query
#' launched is shown.
#' @return A data-frame with three columns, first Wikidata entity, second all
#' Wikidata class each instance is instance of them, last TRUE or FALSE if each
#' entity is instance of the `instanceof` parameter, if this one is set.
#' @examples
#' \dontrun{
#' # aux: get a vector of entities (l).
#' df <- w_SearchByLabel(string='Iranzo', langsorder='es|en', mode='inlabel')
#' l <- df$entity
#'
#' df <- w_isInstanceOf(entity_list=l, instanceof='Q5')
#' # Not TRUE
#' df[!df$instanceof_Q5,]
#' }
#' @author Angel Zazo, Department of Computer Science and Automatics, University of Salamanca
#' @export
w_isInstanceOf <- function(entity_list, instanceof='', nlimit=50000, debug=FALSE) {
  # check entities
  entity_list <- checkEntities(entity_list)
  #
  n <- length(entity_list)
  # Check limits to make chucked queries
  if (n > nlimit) {
    if (debug != FALSE)
      cat(paste0("INFO: The number of entities (", n,") exceeds nlimit (",
                 nlimit,')\n'), file=stderr())
    return (doChunks(w_isInstanceOf, entity_list, nlimit, instanceof=instanceof,
                     debug=debug))
  }
  #
  values <- paste0("wd:", paste0(entity_list, collapse = ' wd:'))
  #
  query <- paste0('SELECT ?entity
(GROUP_CONCAT(DISTINCT ?instanc;separator="|") as ?instanceof)
WHERE {
  OPTIONAL {
    VALUES ?entity {', values,'}
    OPTIONAL {?entity wdt:P31 ?instanc.}
  }
} GROUP BY ?entity\n')
  #
  if (debug == "query")
    cat(query, file=stderr())
  #
  r <- w_query(query, format='csv', method='POST')
  if(is.null(r)){
    return(NULL)
  }
  r$entity <- sub('^http://www.wikidata.org/entity/', '', r$entity)
  r$instanceof <- gsub('http://www.wikidata.org/entity/', '', r$instanceof)
  #
  # Add new column for instanceof parameter:
  if (instanceof!='')
    r[[paste0('instanceof_', instanceof)]] = grepl(paste0('\\b(?:', instanceof, ")\\b"), r$instanceof)
  #
  rownames(r) <- r$entity
  return(r)
}

#' Get Wikipedia pages of Wikidata entities
#'
#' Get from Wikidata all Wikipedia page titles and URL of the Wikidata entities
#' in entity_list. If parameter `wikilangs`='', then returns all Wikipedia page
#' titles, else only the languages in `wikilangs`. The returned dataframe also
#' includes the Wikidata entity classes of which the searched entity is
#' an instance. If set the parameter `instanceof`, then only returns the pages
#' for Wikidata entities which are instances of the Wikidata class indicated in
#' it. The data-frame doesn't return labels or descriptions about entities: the
#' function `w_LabelDesc` can be used for this. Duplicated entities are deleted
#' before search. Index of the data-frame returned are also set to entity_list.
#' @param entity_list A vector of Wikidata entities.
#' @param wikilangs List of languages to limit the search, using "|" as
#' separator. Wikipedias page titles are returned in same order as languages in
#' this parameter. If wikilangs='' the function returns Wikipedia page titles
#' in any language, not sorted.
#' @param instanceof Wikidata entity class to limit the result to the instances
#' of that class. For example, if instanceof='Q5', limit the results to "human".
#' @param nlimit If the number of entities exceeds this number, chunked queries
#' are done. This is the number of entities requested in each chunk. Please,
#' reduce the default value if error is raised.
#' @param debug For debugging purposes (default FALSE). If debug='info'
#' information about chunked queries is shown. If debug='query' also the query
#' launched is shown.
#' @return A data-frame with five columns: entities, instanceof, npages, page
#' titles and page URLs. Last three use "|" as separator. Index of data-frame is
#' also set to the entity_list.
#' @examples
#' \dontrun{
#' # aux: get a vector of entities (l).
#' df <- w_SearchByLabel(string='Napoleon', langsorder='en', mode='inlabel')
#' l <- df$entity  # aprox. 3600
#'
#' w <- w_Wikipedias(entity_list=l, debug='info')
#' w <- w_Wikipedias(entity_list=l, wikilangs='es|en|fr', debug='info')
#' # Filter instanceof=Q5 (human):
#' w_Q5 <- w[grepl("\\bQ5\\b", w$instanceof), ]
#' w_Q5b <- w_Wikipedias(entity_list=l, wikilangs='es|en|fr', instanceof='Q5', debug='info')
#' }
#' @author Angel Zazo, Department of Computer Science and Automatics, University of Salamanca
#' @export
w_Wikipedias <- function(entity_list, wikilangs="", instanceof='', nlimit=1500, debug=FALSE) {
  # check entities
  entity_list <- checkEntities(entity_list)
  #
  n <- length(entity_list)
  # Check limits to make chucked queries
  if (n > nlimit) {
    if (debug != FALSE)
      cat(paste0("INFO: The number of entities (", n,") exceeds nlimit (",
                 nlimit,')\n'), file=stderr())
    return (doChunks(w_Wikipedias, entity_list, nlimit, wikilangs=wikilangs,
                     instanceof=instanceof, debug=debug))
  }
  #
  values <- paste0("wd:", paste0(entity_list, collapse = ' wd:'))
  #
  wikilangs <- trimws(wikilangs)
  if (wikilangs=="")
    w_filter <- ''
  else {
    wikiorder <- strsplit(wikilangs, '|', fixed = T)[[1]]
    w_filter  <- paste0("FILTER(?lang IN ('",gsub('|', "','", wikilangs, fixed=T), "'))")
  }
  #
  query <- paste0("SELECT DISTINCT ?entity
(GROUP_CONCAT(DISTINCT ?instanc;separator=\"|\") as ?instanceof)
(COUNT(DISTINCT ?page) as ?count)
(GROUP_CONCAT(DISTINCT ?lang;separator=\"|\") as ?langs)
(GROUP_CONCAT(DISTINCT ?name;separator=\"|\") as ?names)
(GROUP_CONCAT(DISTINCT ?page;separator=\"|\") as ?pages)
WHERE {
  OPTIONAL {
    VALUES ?entity {", values, "}
    OPTIONAL {?entity wdt:P31 ?instanc.}
    OPTIONAL {
      ?page schema:about ?entity;
            schema:inLanguage ?lang;
            schema:name ?name;
            schema:isPartOf [wikibase:wikiGroup \"wikipedia\"].\n",
                  w_filter,"
    }
  }
} GROUP BY ?entity\n")
  #
  if (debug == "query")
    cat(query, file=stderr())
  #
  r <- w_query(query, format='csv', method='POST')
  if(is.null(r)){
    return(NULL)
  }
  r$entity <- sub('^http://www.wikidata.org/entity/', '', r$entity)
  r$instanceof <- gsub('http://www.wikidata.org/entity/', '', r$instanceof)
  # Filter instanceof
  if (instanceof!='')
    r <- r[grepl(paste0('\\b(?:', instanceof, ")\\b"), r$instanceof), ]
  # If all entities are excluded:
  if (nrow(r) == 0)
    return(r)
  #
  r$count <- as.integer(r$count)
  rownames(r) <- r$entity
  # Order by wikilangs
  if (wikilangs!='') {
    for (entity in r$entity) {
      if (r[entity, 'count'] > 1) {
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
  }
  # Create a NA data-frame and fill it with retrieved values
  q <- data.frame(entity=r$entity, instanceof=NA, count=NA, langs=NA, names=NA, pages=NA)
  rownames(q) <- q$entity
  q[r$entity, ] <- r
  return(q)
}

#' Check if Wikidata entities are valid
#'
#' A entity is valid if it has a label or has a description. If one entity
#' exists but is not valid, is possible that it has a redirection to other
#' entity, in that case, the redirection is obtained. Other entities may have
#' existed in the past, but have been deleted. The returned dataframe also
#' includes the Wikidata class (another Wikidata entity) of which the searched
#' entity are instances of. The data-frame no contains labels or descriptions
#' about entities: the function `w_LabelDesc` can be used for valid entities.
#' Duplicated entities are deleted before search. Index of the data-frame
#' returned are also set to entity_list.
#'
#' @param entity_list A vector with de Wikidata entities.
#' @param nlimit If the number of entities exceeds this number, chunked queries
#' are done. This is the number of entities requested in each chunk. Please,
#' reduce the default value if error is raised.
#' @param debug For debugging purposes (default FALSE). If debug='info'
#' information about chunked queries is shown. If debug='query' also the query
#' launched is shown.
#' @return A data-frame with four columns: entity, valid (TRUE or FALSE),
#' instanceof and redirection (if the entity redirects to another Wikidata
#' entity, the redirection column contains the last).
#' @examples
#' \dontrun{
#' w_isValid(c("Q9021", "Q115637688", "Q105660123"))
#' # Large list
#' l  <- w_SearchByOccupation(Qoc='Q2306091')
#' l2 <- append(l$entity, c("Q115637688", "Q105660123"))  # Note: adding two new entities
#' v <- w_isValid(l2)
#' # Not valid
#' v[!v$valid, ]
#' }
#' @author Angel Zazo, Department of Computer Science and Automatics, University of Salamanca
#' @export
w_isValid <- function(entity_list, nlimit=50000, debug=FALSE) {
  # check entities
  entity_list <- checkEntities(entity_list)
  #
  n <- length(entity_list)
  # Check limits to make chucked queries
  if (n > nlimit) {
    if (debug != FALSE)
      cat(paste0("INFO: The number of entities (", n,") exceeds nlimit (",
                 nlimit,')\n'), file=stderr())
    return (doChunks(w_isValid, entity_list, nlimit, debug=debug))
  }
  #
  values <- paste0("wd:", paste0(entity_list, collapse = ' wd:'))
  #
  query <- paste0("SELECT ?entity ?valid
(GROUP_CONCAT(DISTINCT ?instanc; separator='|') as ?instanceof) ?redirection
WHERE {
  OPTIONAL {
   VALUES ?entity {", values, '}
   BIND(EXISTS{?entity rdfs:label []} || EXISTS{?entity schema:description []} AS ?valid).
   OPTIONAL {?entity wdt:P31 ?instanc.}
   OPTIONAL {?entity owl:sameAs ?redirection}
  }
} GROUP BY ?entity ?valid ?redirection\n')
  #
  if (debug == "query")
    cat(query, file=stderr())
  #
  r <- w_query(query, format = "csv", method = 'POST')
  if(is.null(r)){
    return(NULL)
  }
  #
  for (q in c('entity', 'instanceof', 'redirection'))
    r[[q]] <- gsub('http://www.wikidata.org/entity/', '', r[[q]])
  r$valid <- ifelse(r$valid == 'true', T, F)
  # Convert NA -> ""
  r$redirection <- ifelse(is.na(r$redirection),"",r$redirection)
  rownames(r) <- r$entity
  return(r)
}

#' Get properties of Wikidata entities
#'
#' Search the entities of the `entity_list` for property or properties. If
#' searched properties can have more than one language, then the parameter
#' `langsorder` set the order of language used. If parameter `includeQ` is TRUE,
#' also is returned the Wikidata entities for the properties. The Wikidata class
#' of which the entities are instances of are returned too. Duplicated entities
#' are deleted before search. Index of the data-frame is also set to
#' entity_list.
#'
#' @param entity_list A vector with de Wikidata entities.
#' @param Pproperty Wikidata properties to search, separated with '|', mandatory.
#' For example, is Pproperty="P21", the results contain information of the sex
#' of entities. If Pproperty="P21|P569" also searches for birthdate. If
#' Pproperty='P21|P569|P214' also searches for VIAF identifier.
#' @param includeQ If the value is TRUE the function returns the Wikidata entity
#' (Qxxx) of the Pproperty. If also `langsorder` has language(s), the labels, if
#' any, are returned too. Note that includeQ is only effective if `Pproperty`
#' corresponds with a Wikidata entity, else the same values that label are
#' returned.
#' @param langsorder Order of languages in which the information will be
#' returned, separated with '|'. If no information is given in the first
#' language, next is used. This parameter is mandatory if parameter `includeQ`
#' if FALSE. If includeQ=TRUE and langsorder='' no labels are returned.
#' @param nlimit If the number of entities exceeds this number, chunked queries
#' are done. This is the number of entities requested in each chunk. Please,
#' reduce the default value if error is raised.
#' @param debug For debugging purposes (default FALSE). If debug='info'
#' information about chunked queries is shown. If debug='query' also the query
#' launched is shown.
#' @return A data-frame with the entity, the entities of the properties and the
#' labels in langsorder for them.
#' @examples
#' \dontrun{
#' w_Property(c("Q1252859", "Q712609", "Q381800"), Pproperty='P21|P569|P214', langsorder='en|es')
#' # Large list
#' df <- w_SearchByOccupation(Qoc='Q2306091') # ~ 20000
#' l <- df$entity
#' p <- w_Property(l, Pproperty='P21|P569|P214', langsorder='es|en', debug='info')
#' # Get birth-place (P19)
#' p <- w_Property(l, Pproperty='P19', langsorder='es|en', includeQ=TRUE, debug='info')
#' }
#' @author Angel Zazo, Department of Computer Science and Automatics, University of Salamanca
#' @export
w_Property <- function(entity_list, Pproperty, includeQ=FALSE, langsorder='en',
                       nlimit=10000, debug=FALSE) {
  #
  Pproperty <- trimws(Pproperty)
  langsorder <- trimws(langsorder)
  if (Pproperty == "" || (langsorder == '' && !includeQ))
    stop(paste0("ERROR: one or more parameters 'Pproperty', 'includeQ' or 'langsorder' are incorrect."))
  # check entities
  entity_list <- checkEntities(entity_list)
  #
  n <- length(entity_list)
  # Check limits to make chucked queries
  if (n > nlimit) {
    if (debug != FALSE)
      cat(paste0("INFO: The number of entities (", n,") exceeds nlimit (",
                 nlimit,')\n'), file=stderr())
    return (doChunks(w_Property, entity_list, nlimit, Pproperty=Pproperty,
                     includeQ=includeQ, langsorder=langsorder, debug=debug))
  }
  #
  if (langsorder!='') {
    langsorder <- gsub("|", ",", langsorder, fixed = T)
    searchlang <- paste0('\n   SERVICE wikibase:label {bd:serviceParam wikibase:language "', langsorder, '".')
  }
  #
  group_concat <- character()
  search <- character()
  #
  props <- strsplit(Pproperty, '|', fixed = T)[[1]]
  for (p in props) {
    if (includeQ)
      group_concat <- append(group_concat, paste0("(GROUP_CONCAT(DISTINCT ?", p ,"p;separator='|') as ?",p,")"))
    if (langsorder!='') {
      group_concat <- append(group_concat, paste0("(GROUP_CONCAT(DISTINCT STR(?", p ,"label);separator='|') as ?",p,"Label)"))
      searchlang <- append(searchlang, paste0('      ?', p,'p rdfs:label ?', p, 'label.'))
    }
    search <- append(search, paste0('    OPTIONAL {?entity wdt:', p, ' ?', p, 'p.}'))
  }
  group_concat <- paste0(group_concat, collapse = "\n")
  search       <- paste0(search, collapse = "\n")
  if (langsorder!='')
    searchlang <- paste0(paste0(searchlang, collapse = "\n"), '}')
  else
    searchlang <- ''
  #
  values <- paste0("wd:", paste0(entity_list, collapse = ' wd:'))
  query <- paste0("SELECT DISTINCT ?entity (GROUP_CONCAT(DISTINCT ?instanc; separator='|') as ?instanceof)\n",
                  group_concat,"
WHERE {
  OPTIONAL {
    VALUES ?entity {",values,"}
    OPTIONAL {?entity wdt:P31 ?instanc.}\n",
                  search,
                  searchlang, '
  }
} GROUP BY ?entity\n')
  #
  if (debug == "query")
    cat(query, file=stderr())
  #
  r <- w_query(query, format = "csv", method = 'POST')
  if(is.null(r)){
    return(NULL)
  }
  if (includeQ) {
    for (p in props)
      r[[p]] <- gsub('http://www.wikidata.org/entity/', '', r[[p]])
  }
  r$entity <- gsub('http://www.wikidata.org/entity/', '', r$entity)
  r$instanceof <- gsub('http://www.wikidata.org/entity/', '', r$instanceof)
  rownames(r) <- r$entity
  return(r)
}

#' Get Latitude and Longitude coordinates, and Country of places
#'
#' Get Latitude and Longitude coordinates of the Wikidata entities which are
#' places. Also the countries they belong are returned.
#'
#' @param entity_list A vector with de Wikidata entities (places).
#' @param langsorder Order of languages in which the information will be
#' returned, separated with '|'. If no information is given in the first
#' language, next is used. If langsorder=='', then labels or descriptions
#' are not returned.
#' @param nlimit If the number of entities exceeds this number, chunked queries
#' are done. This is the number of entities requested in each chunk. Please,
#' reduce the default value if error is raised.
#' @param debug For debugging purposes (default FALSE). If debug='info'
#' information about chunked queries is shown. If debug='query' also the query
#' launched is shown.
#' @return A data-frame with 'entity', label, Latitude and Longitude, country
#' and label of the country.
#' @examples
#' \dontrun{
#' w_Geoloc(c("Q57860", "Q90", "Q15695"), langsorder="")
#' w_Geoloc(c("Q57860", "Q90", "Q15695"), langsorder="se") # Note label of place for Q15695
#' w_Geoloc(c("Q57860", "Q90", "Q15695"), langsorder="se|fr")
#' df <- w_SearchByOccupation(Qoc='Q2306091') # aprox. 20000
#' l <- df$entity
#' # Get birth-place (P19)
#' p <- w_Property(l, Pproperty = 'P19', includeQ=TRUE, langsorder='es|en', debug='info')
#' # Filter entities that have places
#' places <- p[grepl("^Q\\d+$", p$P19), ]$P19
#' g <- w_Geoloc(places, langsorder='en|es', debug='info')
#' }
#' @author Angel Zazo, Department of Computer Science and Automatics, University of Salamanca
#' @export
w_Geoloc <- function(entity_list, langsorder='', nlimit=1000, debug=FALSE) {
  # check entities
  entity_list <- checkEntities(entity_list)
  #
  n <- length(entity_list)
  # Check limits to make chucked queries
  if (n > nlimit) {
    if (debug != FALSE)
      cat(paste0("INFO: The number of entities (", n,") exceeds nlimit (",
                 nlimit,')\n'), file=stderr())
    return (doChunks(w_Geoloc, entity_list, nlimit, langsorder=langsorder, debug=debug))
  }
  #
  langsorder <- trimws(langsorder)
  if (langsorder == '') {
    sss <- '?place (STR(SAMPLE(?clat)) as ?placeLat) (STR(SAMPLE(?clon)) as ?placeLon) ?country'
    sqq <- ''
    sgg <- '?place ?country'
  }
  else {
    sss <- '?place ?placeLabel (STR(SAMPLE(?clat)) as ?placeLat) (STR(SAMPLE(?clon)) as ?placeLon) ?country ?countryLabel'
    langsorder <- gsub("|", ",", langsorder, fixed = T)
    sqq <- paste0('SERVICE wikibase:label {bd:serviceParam wikibase:language "',
                  langsorder,'".\n    ?place rdfs:label ?placeLabel.\n    ?country rdfs:label ?countryLabel.}')
    sgg <- '?place ?placeLabel ?country ?countryLabel'
  }
  #
  values <- paste0("wd:",paste0(entity_list, collapse = ' wd:'))
  query <- paste0("SELECT DISTINCT ",sss, "
WHERE {
  OPTIONAL {
    VALUES ?place {", values, "}
    OPTIONAL {?place wdt:P1366+ ?placelast.}
    OPTIONAL {?place wdt:P625 ?c1.}
    OPTIONAL {?placelast wdt:625 ?c2.}
    BIND(COALESCE(?c1, ?c2) AS ?c).
    BIND(geof:longitude(?c) AS ?clon)
    BIND(geof:latitude(?c)  AS ?clat)
    BIND(COALESCE(?placelast, ?place) AS ?actualplace).
    OPTIONAL {
      ?actualplace wdt:P17 ?country.
      ?country wdt:P31 ?instance.
      FILTER (?instance in (wd:Q3624078, wd:Q7275, wd:Q6256)).
      #not a former country
      # FILTER NOT EXISTS {{?country wdt:P31 wd:Q3024240}}
      #and no an ancient civilisation (needed to exclude ancient Egypt)
      # FILTER NOT EXISTS {{?country wdt:P31 wd:Q28171280}}
      FILTER (?instance not in (wd:Q3024240, wd:Q28171280)).
    }
  }
  ",sqq,"
} GROUP BY ", sgg, "\n")
  #
  if (debug == "query")
    cat(query, file=stderr())
  #
  r <- w_query(query, format='csv', method='POST')
  if(is.null(r)){
    return(NULL)
  }
  r$place   <- sub('^http://www.wikidata.org/entity/', '', r$place)
  r$country <- sub('^http://www.wikidata.org/entity/', '', r$country)
  #
  # Sometimes an ancient/old place corresponds to several present-day places and
  # several countries. Other times the place is a border between two countries.
  # But for a given place, the function should only return one country. There
  # are some approaches that can be applied in Wikidata, but they are not
  # uniform (number of references, time-stamps, etc.). In general, this happens
  # very rarely, so removing duplicates may be a good option.
  r <- r[!duplicated(r$place),]
  rownames(r) <- r$place
  return(r)
}

#' Return label and/or descriptions of Wikidata entities
#'
#' Return label and/or descriptions of the entities in entity_list in language
#' indicated in `langsorder`. Note that entities can be Wikidata entities (Qxxx)
#' or Wikidata properties (Pxxx).
#'
#' @param entity_list A vector with de Wikidata entities.
#' @param what Retrieve only Labels (L), only Descriptions (D) or both (LD).
#' @param langsorder Order of languages in which the information will be
#' returned, separated with '|'. If no information is given in the first
#' language, next is used. This parameter is mandatory, at least one language is
#' required, default 'en'.
#' @param nlimit If the number of entities exceeds this number, chunked queries
#' are done. This is the number of entities requested in each chunk. Please,
#' reduce the default value if error is raised.
#' @param debug For debugging purposes (default FALSE). If debug='info'
#' information about chunked queries is shown. If debug='query' also the query
#' launched is shown.
#' @return A data-frame with one column for the entities, and others for the
#' language and the labels and/or descriptions. The index of the dataframe is
#' also set to the entity list.
#' @examples
#' \dontrun{
#' w_LabelDesc(c("Q57860", "Q712609", "Q381800", "P569"), what='LD', langsorder = 'se|es|en')
#' }
#' @author Angel Zazo, Department of Computer Science and Automatics, University of Salamanca
#' @export
w_LabelDesc <- function(entity_list, what='LD', langsorder='en', nlimit=25000,
                        debug=FALSE) {
  langsorder <- trimws(langsorder)
  if (langsorder=='' || (!grepl('L', what) && (!grepl('D', what))))
    stop(paste0("ERROR: one or both parameters 'what' or 'langsorder' are incorrect."))
  #
  # check entities
  entity_list <- checkEntities(entity_list)
  #
  n <- length(entity_list)
  # Check limits to make chucked queries
  if (n > nlimit) {
    if (debug != FALSE)
      cat(paste0("INFO: The number of entities (", n,") exceeds nlimit (",
                 nlimit,')\n'), file=stderr())
    return (doChunks(w_LabelDesc, entity_list, nlimit, what=what, langsorder=langsorder, debug=debug))
  }
  #
  ss <- ''
  qs <- ''
  if (grepl('L', what)) {
    ss <- paste0(ss, '(LANG(?label) as ?labellang) ?label')
    qs <- paste0(qs, '    ?entity rdfs:label ?label.\n')
  }
  if (grepl('D', what)) {
    ss <- paste0(ss, ' (LANG(?description) as ?descriptionlang) ?description')
    qs <- paste0(qs, '    ?entity schema:description ?description.\n')
  }
  #
  values <- paste0("wd:",paste0(entity_list, collapse = ' wd:'))
  langsorder <- gsub("|", ",", langsorder, fixed = T)
  #
  query <- paste0('SELECT ?entity ',ss,'
WHERE {
  VALUES ?entity {', values, '}
  SERVICE wikibase:label {
    bd:serviceParam wikibase:language "', langsorder, '".\n', qs,
                  '  }
}\n')
  #
  if (debug == "query")
    cat(query, file=stderr())
  #
  r <- w_query(query, format='csv', method='POST')
  if(is.null(r)){
    return(NULL)
  }
  r$entity <- sub('^http://www.wikidata.org/entity/', '', r$entity)
  rownames(r) <- r$entity
  return(r)
}

#' Get Wikidata entities with a certain occupation
#'
#' Return the Wikidata entities which have the occupation indicated in `Qoc`,
#' the Wikidata entity for that occupation. For example, if Qoc='Q2306091',
#' returns the Wikidata entities which occupation is "Sociologist", among
#' others. Also returns the Wikidata class of which the entities are instances
#' of. If parameter langsorder='', then no labels or descriptions of the
#' entities are returned, otherwise the function returns them in the language
#' order indicated in `langsorder`. If wikilangs='' (if mode='wikipedias') then
#' the Wikipedia pages are not filtered by language, else only Wikipedias of
#' languages in this parameter are returned.
#'
#' @param Qoc The Wikidata entity of the occupation. For example, Q2306091 for
#' sociologist, Q2526255 for Film director, etc.
#' @param mode The results you want to obtain: 'default' returns the Wikidata
#' entities which have the occupation indicated; 'count' search in WDQS to know
#' the number of Wikidata entities with that occupation); 'wikipedias' also the
#' Wikipedia page of the entities are returned.
#' @param langsorder Order of languages in which the information will be
#' returned, separated with '|'. If no information is given in the first
#' language, next is used. If langsorder='', then labels or descriptions
#' are not returned.
#' @param wikilangs List of languages in Wikipedias to limit the search, using
#' "|" as separator (only if mode='wikipedias'). Wikipedias page titles are
#' returned in same order as languages in this parameter. If wikilangs='' the
#' function returns Wikipedia page titles of entities in any language, not
#' sorted.
#' @param nlimit If the number of entities in that occupation exceeds this
#' number, then query are made in chunks. The value can increase if
#' `langorder`=''. Please, reduce the default value if error is raised.
#' @param debug For debugging purposes (default FALSE). If debug='info'
#' information about chunked queries is shown. If debug='query' also the query
#' launched is shown. If debug='count' the function only returns the number of
#' entities with that occupation.
#' @return A data-frame with 'entity' and  'entityLabel', 'entityDescription',
#' 'instanceof' and 'instanceofLabel' columns. Index of the data-frame
#' is also set to the list of entities found.
#' @examples
#' \dontrun{
#' # "Q2306091" Qoc for Sociologist
#' w_SearchByOccupation(Qoc="Q2306091", mode='count')
#' q <- w_SearchByOccupation(Qoc="Q2306091", langsorder="")
#' q <- w_SearchByOccupation(Qoc="Q2306091", langsorder="en|es|fr")
#' q <- w_SearchByOccupation(Qoc="Q2306091", mode='wikipedias', debug='info')
#' q <- w_SearchByOccupation(Qoc="Q2306091", mode='wikipedias', wikilangs='en|es|fr', debug='info')
#' }
#' @author Angel Zazo, Department of Computer Science and Automatics, University of Salamanca
#' @export
w_SearchByOccupation <- function(Qoc, mode=c('default','count','wikipedias'),
                                 langsorder='', wikilangs='', nlimit=10000, debug=FALSE) {
  mode <- mode[1]
  #
  # First: known the number of entities
  query <- paste0('SELECT (COUNT(DISTINCT ?entity) AS ?count) WHERE {?entity wdt:P106 wd:',Qoc,'}')
  d <- w_query(query, method='GET', format="csv")
  if(is.null(d)){
    return(NULL)
  }
  nq <- d$count[1]
  #
  if (debug!=FALSE) {
    cat(paste0("INFO: The number of entities with that occupation is ",nq,".\n"), file=stderr())
  }
  if (mode == 'count')
    return(nq)
  #
  langsorder <- trimws(langsorder)
  if (langsorder == '') {
    ss1 <- ''
    sq  <- ''
    ss2 <- ''
  }
  else {
    ss1 <- "?entityLabel ?entityDescription"
    ss2 <- "(GROUP_CONCAT(DISTINCT ?instancLabel; separator='|') as ?instanceofLabel)"
    langsorder <- gsub("|", ",", langsorder, fixed = T)
    sq = paste0('SERVICE wikibase:label {bd:serviceParam wikibase:language "',langsorder,'".
      ?entity rdfs:label ?entityLabel.
      ?entity schema:description ?entityDescription.
      ?instanc rdfs:label ?instancLabel.}')
  }
  #
  nlim <- as.integer(nq/nlimit)
  if (nlim>0 && debug!=FALSE)
    cat(paste0("INFO: The number of entities (",nq ,") exceeds chunksize (", nlimit, ").\n"), file=stderr())
  output <- NULL
  for (k in 0:nlim){
    offset <- k*nlimit
    if ((offset+1) > nq)
      break
    if (nlim>0 && debug!=FALSE)
      cat(paste0("  INFO: Requesting elements from ", offset+1, " to ",
                 min(offset+nlimit, nq),"\n"), file=stderr())
    #
    query <- paste0("SELECT DISTINCT ?entity ",ss1,"
(GROUP_CONCAT(DISTINCT ?instanc; separator='|') as ?instanceof)
",ss2,"
WITH {
  SELECT DISTINCT ?entity
  WHERE {?entity wdt:P106 wd:",Qoc,".}
  ORDER BY ?entity
  LIMIT ",nlimit," OFFSET ",offset,"
  } AS %results
WHERE {
  INCLUDE %results.
  ",sq,"
  OPTIONAL {?entity wdt:P31 ?instanc.}
} GROUP BY ?entity ", ss1 ,"\n")
    #
    if (debug == "query")
      cat(query, file=stderr())
    #
    r <- w_query(query, format='csv', method='POST')
    if(is.null(r)){
      next
    }
    r$entity  <- gsub('http://www.wikidata.org/entity/', '', r$entity)
    r$instanceof  <- gsub('http://www.wikidata.org/entity/', '', r$instanceof)
    rownames(r) <- r$entity
    if (k==0)
      output <- r
    else
      output <- rbind(output, r)
  }
  #
  if (mode=='wikipedias') {
    if (debug!=FALSE)
      cat("INFO: Searching for Wikipedias.\n", file=stderr())
    w <- w_Wikipedias(output$entity, wikilangs = wikilangs, debug=debug)
    output <- cbind(output, subset(w, select = setdiff(names(w),c("entity","instanceof"))))
  }
  #
  return(output)
}


#' Search for entities that may match identifiers in a database or authorities'
#' catalog.
#'
#' The identifiers are in id_list. The database or authorities' catalog to which
#' these identifiers belong must be provided in parameter `Pauthority`.
#' If parameter langsorder='', then no labels or descriptions of the entities
#' are returned, otherwise the function returns them in the language order
#' indicated in `langsorder`. Duplicated entities are deleted before search.
#' Index of the data-frame returned are also set to id_list.
#'
#' @param id_list List of identifiers.
#' @param Pauthority Wikidata property identifier of the database or
#' authorities' catalog. For example, if Pauthority = "P4439", then the function
#' searches for entities that have the identifiers in the MNCARS (Museo Nacional
#' Centro de Arte Reina Sofía) database. Following library abbreviations for the
#' databases can be also used in the parameter 'Pauthority':
#'
#' library   : VIAF, LC,   BNE , ISNI, JPG,  ULAN, BNF,  GND, DNB,
#' Pauthority: P214, P244, P950, P213, P245, P245, P268, P227,P227,
#'
#' library   : SUDOC, NTA,  J9U,   ELEM,  NUKAT, MNCARS
#' Pauthority: P269, P1006, P8189, P1565, P1207, P4439
#'
#' @param langsorder Order of languages in which the information will be
#' returned, separated with '|'. If no information is given in the first
#' language, next is used. If langsorder='', then labels or descriptions are
#' not returned.
#' @param nlimit If the number of entities in the database or authorities'
#' catalog exceeds this number, then query are made in chunks. The value can
#' increase if langorder=''. Please, reduce the default value if error is raised.
#' @param debug For debugging purposes (default FALSE). If debug='info'
#' information about chunked queries is shown. If debug='query' also the query
#' launched is shown. If debug='count' the function only returns the number of
#' entities with have identifier in that authority.
#' @return A data-frame with columns: 'entity', 'entityLabel', 'entityDescription',
#' 'instanceof', instanceofLabel' and the identifier in the "Pauthority" database.
#' Index of the data-frame is also set to the list of entities found.
#' @examples
#' \dontrun{
#' w_SearchByIdentifiers(c("4938246", "36092166", "40787112"), Pauthority='P214')
#' w_SearchByIdentifiers(c("4938246", "36092166", "40787112"), Pauthority='P214', langsorder='en|fr')
#' }
#' @author Angel Zazo, Department of Computer Science and Automatics, University of Salamanca
#' @export
w_SearchByIdentifiers <- function(id_list, Pauthority, langsorder='', nlimit=3000,
                                  debug=FALSE) {
  #
  authorities <- c('P214', 'P244', 'P950', 'P213', 'P245', 'P245', 'P268',
                   'P227', 'P227', 'P269', 'P269', 'P1006', 'P8189',
                   'P1565', 'P1207', 'P3065', 'P8179', 'P4787', 'P1015',
                   'P1015', 'P9984','P9984', 'P7293', 'P409', 'P4439')
  names(authorities) <- c('VIAF', 'LC', 'BNE', 'ISNI', 'JPG', 'ULAN', 'BNF',
                          'GND', 'DNB', 'SUDOC', 'idRefID', 'NTA', 'J9U',
                          'ELEM', 'NUKAT', 'RERO', 'CAOONL', 'NII', 'BIBSYS',
                          'NORAF', 'BNC', 'CANTIC', 'PLWABN', 'NLA', 'MNCARS')
  #
  if (!grepl("^P\\d+$",Pauthority)) {
    if (!Pauthority %in% names(authorities))
      stop(paste0("ERROR: Invalid value '",Pauthority,"' for parameter 'Pauthority'"))
    Pauthority <- authorities[Pauthority]
  }
  #
  n <- length(id_list)
  # Check limits to make chucked queries
  if (n > nlimit) {
    if (debug != FALSE)
      cat(paste0("INFO: The number of entities (", n,") exceeds nlimit (",
                 nlimit,')\n'), file=stderr())
    return (doChunks(w_SearchByIdentifiers, id_list, nlimit, Pauthority=Pauthority,
                     langsorder=langsorder, debug=debug))
  }
  #
  if (langsorder == '') {
    ss1 <- ''
    sq  <- ''
    ss2 <- ''
  }
  else {
    ss1 <- "?entityLabel ?entityDescription"
    ss2 <- "(GROUP_CONCAT(DISTINCT ?instancLabel; separator='|') as ?instanceofLabel)"
    langsorder <- gsub("|", ",", langsorder, fixed = T)
    sq = paste0('SERVICE wikibase:label {bd:serviceParam wikibase:language "',langsorder,'".
      ?entity rdfs:label ?entityLabel.
      ?entity schema:description ?entityDescription.
      ?instanc rdfs:label ?instancLabel.}')
  }
  #
  values <- paste0('"',paste0(id_list, collapse = '" "'), '"')
  #
  query <- paste0("SELECT DISTINCT ?id ?entity ",ss1,"
(GROUP_CONCAT(DISTINCT ?instanc; separator='|') as ?instanceof)
",ss2,"
WHERE {
  OPTIONAL {
    VALUES ?id {",values,"}
    OPTIONAL {?entity wdt:", Pauthority," ?id;
                      wdt:P31 ?instanc.}
      ", sq,"
  }
} GROUP BY ?id ?entity ", ss1 ,"\n")

  if (debug == "query")
    cat(query, file=stderr())
  #
  r <- w_query(query, format='csv', method='POST')
  if(is.null(r)){
    return(NULL)
  }

  r$entity  <- gsub('http://www.wikidata.org/entity/', '', r$entity)
  r$instanceof  <- gsub('http://www.wikidata.org/entity/', '', r$instanceof)
  rownames(r) <- r$id
  return(r)
}

#' Get entities that have identifier in a database or authorities' catalog.
#'
#' Get all Wikidata entities that have identifier in the database or
#' authorities' catalog indicated in the parameter `Pauthority`. Returns the
#' Wikidata entities. If parameter `langsorder`='', then no labels or
#' descriptions of the entities are returned, otherwise the function returns
#' them in the language order indicated in `langsorder`. Filtering is possible
#' if parameter `instanceof`!=''.
#' If only the number of entities which have identifier in the database or
#' authorities' catalog is needed, set `debug`='count'.
#'
#' @param Pauthority Wikidata property identifier of the database or
#' authorities' catalog. For example, if Pauthority = "P4439", all entities
#' which have an identifier in the MNCARS (Museo Nacional Centro de Arte Reina
#' Sofía) database are returnd. Following libraries abbreviation for the
#' databases can be also used in the parameter 'Pauthority':
#'
#' library   : VIAF, LC,   BNE , ISNI, JPG,  ULAN, BNF,  GND, DNB,
#' Pauthority: P214, P244, P950, P213, P245, P245, P268, P227,P227,
#'
#' library   : SUDOC, NTA,  J9U,   ELEM,  NUKAT, MNCARS
#' Pauthority: P269, P1006, P8189, P1565, P1207, P4439
#'
#' @param langsorder Order of languages in which the information will be
#' returned, separated with '|'. If no information is given in the first
#' language, next is used. If langsorder=='', then labels or descriptions are
#' not returned.
#' @param instanceof Wikidata entity of which the entities searched for are an
#' example or member of it (class). Optional. For example, if instanceof="Q5"
#' the search are filtered to Wikidata entities of class Q5 (human). Some
#' entity classes are allowed, separated with '|'.
#' @param nlimit If the number of entities in the database or authorities'
#' catalog exceeds this number, then query are made in chunks. The value can
#' increase if langorder=''. Please, reduce the default value if error is raised.
#' @param debug For debugging purposes (default FALSE). If debug='info'
#' information about chunked queries is shown. If debug='query' also the query
#' launched is shown. If debug='count' the function only returns the number of
#' entities with have identifier in that authority.
#' @return A data-frame with columns: 'entity', 'entityLabel', 'entityDescription',
#' 'instanceof', instanceofLabel' and the identifier in the "Pauthority" database.
#' Index of the data-frame is also set to the list of entities found.
#' @examples
#' \dontrun{
#' # Example: Pauthority=P4439 (has identifier in the Museo Nacional Centro de
#' # Arte Reina Sofía)
#' w_SearchByAuthority(Pauthority="P4439", debug='count')
#' mncars <- w_SearchByAuthority(Pauthority="P4439")
#' mncars <- w_SearchByAuthority(Pauthority="MNCARS", langsorder = 'es|en')
#' # Wikidata entities are not 'human' (Q5):
#' mncars[!grepl("\\bQ5\\b", mncars$instanceof), ]
#' # Wikidata entities are 'human' (Q5):
#' mncars <- w_SearchByAuthority(Pauthority="MNCARS", langsorder = 'es|en', instanceof='Q5')
#' }
#' @author Angel Zazo, Department of Computer Science and Automatics, University of Salamanca
#' @export
w_SearchByAuthority <- function(Pauthority, langsorder='', instanceof='', nlimit=10000,
                                debug=FALSE) {
  #
  authorities <- c('P214', 'P244', 'P950', 'P213', 'P245', 'P245', 'P268',
                   'P227', 'P227', 'P269', 'P269', 'P1006', 'P8189',
                   'P1565', 'P1207', 'P3065', 'P8179', 'P4787', 'P1015',
                   'P1015', 'P9984','P9984', 'P7293', 'P409', 'P4439')
  names(authorities) <- c('VIAF', 'LC', 'BNE', 'ISNI', 'JPG', 'ULAN', 'BNF',
                          'GND', 'DNB', 'SUDOC', 'idRefID', 'NTA', 'J9U',
                          'ELEM', 'NUKAT', 'RERO', 'CAOONL', 'NII', 'BIBSYS',
                          'NORAF', 'BNC', 'CANTIC', 'PLWABN', 'NLA', 'MNCARS')
  #
  if (!grepl("^P\\d+$",Pauthority)) {
    if (!Pauthority %in% names(authorities))
      stop(paste0("ERROR: Invalid value '",Pauthority,"' for parameter 'Pauthority'"))
    Pauthority <- authorities[Pauthority]
  }
  #
  if (langsorder == '') {
    ss1 <- ''
    sq  <- ''
    ss2 <- ''
  }
  else {
    ss1 <- "?entityLabel ?entityDescription"
    ss2 <- "(GROUP_CONCAT(DISTINCT ?instancLabel; separator='|') as ?instanceofLabel)"
    langsorder <- gsub("|", ",", langsorder, fixed = T)
    sq = paste0('SERVICE wikibase:label {bd:serviceParam wikibase:language "',langsorder,'".
      ?entity rdfs:label ?entityLabel.
      ?entity schema:description ?entityDescription.
      ?instanc rdfs:label ?instancLabel.}')
  }
  #
  # First: known the number of entities
  query <- paste0('SELECT (COUNT(DISTINCT ?entity) AS ?count) WHERE {?entity wdt:',Pauthority,' [].}')
  d <- w_query(query, method='GET', format="csv")
  if(is.null(d)){
    return(NULL)
  }
  nq <- d$count[1]
  #
  if (debug=='count') {
    cat(paste0("INFO: The number of entities is ", nq, ".\n"), file=stderr())
    return(nq)
  }
  #
  nlim <- as.integer(nq/nlimit)
  if (nlim>0 && debug!=FALSE)
    cat(paste0("INFO: The number of entities (", nq, ") exceeds chunksize (", nlimit, ").\n"), file=stderr())
  output <- NULL
  for (k in 0:nlim){
    offset <- k*nlimit
    if ((offset+1) > nq)
      break
    if (nlim>0 && debug!=FALSE)
      cat(paste0("  INFO: Requesting elements from ", offset+1, " to ",
                 min(offset+nlimit, nq),"\n"), file=stderr())
    #
    query <- paste0("SELECT DISTINCT ?entity ",ss1,"
(GROUP_CONCAT(DISTINCT ?instanc; separator='|') as ?instanceof)
",ss2,"
(GROUP_CONCAT(DISTINCT STR(?authid);separator='|') as ?",Pauthority,")
WITH {
  SELECT DISTINCT ?entity ?authid
  WHERE {?entity wdt:",Pauthority," ?authid.}
  ORDER BY ?entity
  LIMIT ",nlimit," OFFSET ",offset,"
  } AS %results
WHERE {
  INCLUDE %results.
  ",sq,"
  OPTIONAL {?entity wdt:P31 ?instanc.}
} GROUP BY ?entity ", ss1 ,"\n")
    #
    if (debug == "query")
      cat(query, file=stderr())
    #
    r <- w_query(query, format='csv', method='POST')
    if(is.null(r)){
      next
    }
    r$entity  <- gsub('http://www.wikidata.org/entity/', '', r$entity)
    r$instanceof  <- gsub('http://www.wikidata.org/entity/', '', r$instanceof)
    rownames(r) <- r$entity
    if (k==0)
      output <- r
    else
      output <- rbind(output, r)
  }
  # Filter instanceof
  if (instanceof!='')
    output <- output[grepl(paste0('\\b(?:', instanceof, ")\\b"), output$instanceof), ]
  #
  return(output)
}

#' Get entities which are instance of a Wikidata entity
#'
#' Get all Wikidata entities which are instance of one o more Wikidata entities
#' like films, cities, etc. If parameter `langsorder`='', then no labels or
#' descriptions of the entities are returned, otherwise the function returns
#' them in the language order indicated in `langsorder`.
#' @param instanceof Wikidata entity of which the entities searched for are an
#' example or member of it (class). For example, if instanceof="Q229390" return
#' Wikidata entities of class Q229390 (3D films). More than one entities can be
#' included in the `instanceof` parameter, with '|' or '&' separator:
#'  - if '|' (instanceof='Q229390|Q202866') then the OR operator is used.
#'  - if '&' (instanceof='Q229390|Q202866') then the AND operator is used.
#'  Note that '|' and '&' cannot be present at the same time.
#' @param langsorder Order of languages in which the information will be
#' returned, separated with '|'. If no information is given in the first
#' language, next is used. If langsorder=='', then labels or descriptions are
#' not returned.
#' @param nlimit If the number of entities in the database or authorities'
#' catalog exceeds this number, then query are made in chunks. The value can
#' increase if langorder=''. Please, reduce the default value if error is raised.
#' @param debug For debugging purposes (default FALSE). If debug='info'
#' information about chunked queries is shown. If debug='query' also the query
#' launched is shown. If debug='count' the function only returns the number of
#' entities.
#' @return A data-frame. Index of the data-frame is also set to the list of entities found.
#' @examples
#' \dontrun{
#' w <- w_SearchByInstanceof('Q229390|Q25110269', langsorder = 'es|en')
#' w <- w_SearchByInstanceof('Q229390&Q25110269', langsorder = 'es|en')
#' }
#' @author Angel Zazo, Department of Computer Science and Automatics, University of Salamanca
#' @export
w_SearchByInstanceof <- function(instanceof, langsorder='', nlimit=2500, debug=FALSE) {
  #
  searchOR <- searchAND <- FALSE
  if (!grepl("^Q\\d+([|&]Q\\d+)*$",instanceof))
    stop(paste0("ERROR: Invalid value '",instanceof,"' for parameter 'instanceof'"))
  if (grepl('|', instanceof, fixed=TRUE))
    searchOR = TRUE
  if (grepl('&', instanceof, fixed=TRUE))
    searchAND = TRUE
  if (searchOR & searchAND)
    stop(paste0("ERROR: Invalid value '",instanceof,"' for parameter 'instanceof'"))
  #
  if (langsorder == '') {
    ss1 <- ''
    sq  <- ''
    ss2 <- ''
  }
  else {
    ss1 <- "?entityLabel ?entityDescription"
    ss2 <- "(GROUP_CONCAT(DISTINCT ?instancLabel; separator='|') as ?instanceofLabel)"
    langsorder <- gsub("|", ",", langsorder, fixed = T)
    sq = paste0('SERVICE wikibase:label {bd:serviceParam wikibase:language "',langsorder,'".
      ?entity rdfs:label ?entityLabel.
      ?entity schema:description ?entityDescription.
      ?instanc rdfs:label ?instancLabel.}')
  }
  #
  # First: known the number of entities
  #
  if (searchOR) {
    iof <- strsplit(instanceof, '|', fixed = T)[[1]]
    query <- paste0('SELECT (COUNT(DISTINCT *) AS ?count) WHERE {\n',
                    paste0('{[] wdt:P31 wd:', iof, '}', collapse='\n UNION\n'),"\n}\n")
    queryF <- paste0('SELECT DISTINCT ?entity WHERE {\n',
                     paste0('{?entity wdt:P31 wd:', iof, '}', collapse='\n UNION\n'),"\n}\n")
  }
  else if (searchAND) {
    iof <- strsplit(instanceof, '&', fixed = T)[[1]]
    values <- values <- paste0("wd:", paste0(iof, collapse = ', wd:'))
    query <- paste0('SELECT (COUNT(DISTINCT *) AS ?count) WHERE {[] wdt:P31 ', values, '}\n')
    queryF <- paste0('SELECT DISTINCT ?entity WHERE {?entity wdt:P31 ', values, '}\n')
  }
  else {
    query <- paste0('SELECT (COUNT(DISTINCT *) AS ?count) WHERE {[] wdt:P31 wd:', instanceof, '}\n')
    queryF <- paste0('SELECT ?entity WHERE {?entity wdt:P31 wd:', instanceof, '}\n')
  }

  d <- w_query(query, method='GET', format="csv")
  if(is.null(d)){
    return(NULL)
  }
  nq <- d$count[1]
  #
  if (debug=='count') {
    cat(paste0("INFO: The number of entities is ", nq, ".\n"), file=stderr())
    return(nq)
  }
  #
  nlim <- as.integer(nq/nlimit)
  if (nlim>0 && debug!=FALSE)
    cat(paste0("INFO: The number of entities (", nq, ") exceeds chunksize (", nlimit, ").\n"), file=stderr())
  output <- NULL
  for (k in 0:nlim){
    offset <- k*nlimit
    if ((offset+1) > nq)
      break
    if (nlim>0 && debug!=FALSE)
      cat(paste0("  INFO: Requesting elements from ", offset+1, " to ",
                 min(offset+nlimit, nq),"\n"), file=stderr())
    #
    query <- paste0("SELECT DISTINCT ?entity ",ss1,"
(GROUP_CONCAT(DISTINCT ?instanc; separator='|') as ?instanceof)",
                    ss2,"
WITH {
", queryF,
"  ORDER BY ?entity
  LIMIT ",sprintf("%d",nlimit)," OFFSET ",sprintf("%d",offset),"
  } AS %results
WHERE {
  INCLUDE %results.",
                    sq,"
  OPTIONAL {?entity wdt:P31 ?instanc.}
} GROUP BY ?entity ", ss1 ,"\n")
    #
    if (debug == "query")
      cat(query, file=stderr())
    #
    r <- w_query(query, format='csv', method='POST')
    if(is.null(r)){
      next
    }
    r$entity  <- gsub('http://www.wikidata.org/entity/', '', r$entity)
    r$instanceof  <- gsub('http://www.wikidata.org/entity/', '', r$instanceof)
    rownames(r) <- r$entity
    if (k==0)
      output <- r
    else
      output <- rbind(output, r)
  }
  return(output)
}

#' Search Wikidata entities by string (usually labels)
#'
#' Search Wikidata entities in label and altLabel ("Also known as") or in any
#' part of the entity using different approaches.
#'
#' @param string String (label or altLabel) to search. Note that single
#' quotation mark must be escaped (string="O\\'Donell"), otherwise an error will
#' be raised.
#' @param mode The mode to perform search. Default 'inlabel' mode.
#'  - 'exact' for an exact search in label or altLabel using case sensitive
#'    search and differentiate diacritics. Languages in the parameter `lang` are
#'    used, so this parameter is mandatory using this mode.
#'  - 'startswith' for entities which label or altLabel starts with the string,
#'    similar to a wildcard search "string*". The string is searched in label in
#'    the languages of `lang` parameter, but in any language in altLabel, so
#'    parameter `lang` is also mandatory in this mode. Diacritics and case are
#'    ignored in this mode.
#'  - 'cirrus' search words in any order in any part of the entity (which must
#'    be a string), not only in label or altLabel. Diacritics and case are
#'    ignored. It is a full text search using the ElasticSearch engine.
#'    Phrase search can be used if launched with double quotation marks, for
#'    example, string='"Antonio Saura"'. Also fuzzy search is possible, for
#'    example, string="algermon~1" or string="algernon~2". Also REGEX search can
#'    be used (but it is a very limited functionality) using this format:
#'    string="insource:/regex/i" (i: is for ignore case, optional).
#'    In this mode, parameter `langs` is ignored.
#'  - 'inlabel' is an special case of 'cirrus' search for matching whole words
#'    (in any order) in any position in label or altLabel. With this mode no
#'    fuzzy search can be used, but some languages can be set in the `lang`
#'    parameter.
#'    Modes 'inlabel' and 'cirrus' use the CirrusSearch of the Wikidata API.
#'    Please, for more examples, see https://www.mediawiki.org/wiki/Help:CirrusSearch
#'    and https://www.mediawiki.org/wiki/Help:Extension:WikibaseCirrusSearch
#' @param langs Languages in which the information will be searched, using "|"
#' as separator. In 'exact' or 'startswith' modes this parameter is mandatory,
#' at least one language is required. In 'inlabel'mode, if the parameter `langs`
#' is set, then the search is restricted to languages in this parameter,
#' otherwise any language. In 'cirrus' mode this parameter is ignored.
#' @param langsorder Order of languages in which the information will be
#' returned, using "|" as separator. If `langsorder`='', no labels or
#' descriptions will be returned, otherwise, they are returned in the order of
#' languages in this parameter, if any.
#' @param instanceof Wikidata entity of which the entities searched for are an
#' example or member of it (class). For example, if instanceof='Q5' the
#' search are filtered to Wikidata entities of class Q5 (human). Some
#' entity classes are allowed, separated with '|'.
#' @param Pproperty Wikidata properties to search, separated with '|', mandatory.
#' For example, is Pproperty="P21", the results contain information of the sex
#' of entities. If Pproperty="P21|P569" also searches for birthdate. If
#' Pproperty='P21|P569|P214' also searches for VIAF identifier.
#' @param debug For debugging purposes (default FALSE). If debug='query' the
#' query launched is shown.
#' @param debug For debugging purposes (default FALSE). If debug='query' the
#' query launched is shown. If debug='count' the function only returns the
#' number of entities with that occupation.
#' @return A data-frame with 'entity', 'entityLabel', 'entityDescription',
#' (including 'instance', 'instanceLabel', 'altLabel' if mode="startswith")
#' and additionally the properties of Pproperty.
#' @examples
#' \dontrun{
#' df <- w_SearchByLabel(string='Iranzo', mode="exact", langs='es|en')
#' df <- w_SearchByLabel(string='Iranzo', mode="exact", langs='es|en',
#'                       langsorder='es|en', instanceof = 'Q5|Q101352')
#' ## Search entities which label or altLabel starts with "string"
#' df <- w_SearchByLabel(string='Iranzo', mode='startswith', lang='en', langsorder='es|en')
#' ## Search in any position in Label or AltLabel (diacritics and case are ignored)
#' df <- w_SearchByLabel(string='Iranzo', mode='inlabel', langsorder='es|en')
#' ## Search in Chinese (Simplified) (language code: zh) in any part of entity:
#' df <- w_SearchByLabel(string='\u4F0A\u5170\u4f50', mode='cirrus', langsorder='es|zh|en')
#' }
#' @author Angel Zazo, Department of Computer Science and Automatics, University of Salamanca
#' @export
w_SearchByLabel <- function(string, mode='inlabel', langs="", langsorder='',
                            instanceof="", Pproperty="", debug=FALSE) {
  mode <- trimws(mode)
  string <- trimws(string)
  # string <- gsub("'", "\\'", trimws(string), fixed = T)
  #
  if (string=='' || !(mode %in% c('exact', 'startswith', 'inlabel', 'cirrus')))
    stop(paste0("ERROR. Parameter 'string' or 'mode' are invalid."))
  if (mode %in% c('exact', 'startswith') && langs=='')
    stop(paste0("ERROR. Parameter 'langs' is mandatory in mode '", mode, "'"))
  if (mode=='cirrus' && langs!='')
    cat(paste0("ERROR. In mode '",mode,"' the parameter 'langs' is ignored."), file=stderr())
  #
  p_show <- p_query <- p_lang <- ''
  if (Pproperty!='') {
    p_show  <- character()
    p_query <- character()
    p_lang <- character()
    for (p in strsplit(Pproperty, '|', fixed = T)[[1]]) {
      p_show <- append(p_show, paste0("(GROUP_CONCAT(DISTINCT ?", p ,"p;separator='|') as ?",p,")"))
      if (langsorder!='')
        p_show <- append(p_show, paste0("(GROUP_CONCAT(DISTINCT STR(?", p ,"label);separator='|') as ?",p,"Label)"))
      p_query <- append(p_query, paste0('  OPTIONAL {?entity wdt:', p, ' ?', p, 'p.}\n'))
      p_lang  <- append(p_lang, paste0('    ?', p,'p rdfs:label ?', p, 'label.\n'))
    }
    p_show  <- paste(p_show, collapse = "\n")
    p_query <- paste0(p_query, collapse = "")
    p_lang  <- paste0(p_lang, collapse = "")
  }

  if (langsorder == '') {
    ss1 <- ''
    ss2 <- p_show
    sq  <- p_query
  }
  else {
    ss1 <- "?entityLabel ?entityDescription"
    ss2 <- paste0("(GROUP_CONCAT(DISTINCT ?instancLabel; separator='|') as ?instanceofLabel)\n",
                  p_show)
    langsorder <- gsub('|', ',', langsorder, fixed=T)
    sq <- paste0(p_query, '  SERVICE wikibase:label {bd:serviceParam wikibase:language "',langsorder,'".
    ?entity rdfs:label ?entityLabel.
    ?entity schema:description ?entityDescription.
    ?instanc rdfs:label ?instancLabel.\n',
                 p_lang,'  }')
  }
  #
  # Start of the query
  query <- paste0("SELECT DISTINCT ?entity ", ss1,
                  "\n(GROUP_CONCAT(DISTINCT ?instanc; separator='|') as ?instanceof)\n",
                  ss2,"\n")
  #
  if (mode=="exact")  # UNION sentences with each language
  {
    l <- strsplit(langs, '|', fixed = T)[[1]]
    units <- paste0("{?entity rdfs:label '", string, "'@", l, '}')
    unionlabel <- paste0(units, collapse = "\nUNION\n")
    unitsalt <- paste0("{?entity skos:altLabel '", string, "'@", l, '}')
    unionaltLabel <- paste0(unitsalt, collapse = "\nUNION\n")
    #
    query <- paste0(query, "WHERE {\n", unionlabel,"\n UNION\n", unionaltLabel)
  }
  else if (mode=="startswith")
  {
    mwapi = character()
    for (l in strsplit(langs, '|', fixed = T)[[1]]) {
      mwapi <- append(mwapi,
                      paste0('SERVICE wikibase:mwapi {
  bd:serviceParam wikibase:api "EntitySearch";
                  wikibase:endpoint "www.wikidata.org";
                  mwapi:language "', l, '";
                  mwapi:search "', string, '" .
  ?entity wikibase:apiOutputItem mwapi:item.
  }'))}
    mwapi <- paste0(mwapi, collapse = "\n    UNION\n    ")
    #
    query <- paste0(query, "WITH {\n    SELECT DISTINCT ?entity\n     WHERE {\n",
                    mwapi, "    }\n  } AS %results\n  WHERE {\n    INCLUDE %results\n")
  }
  else if (mode %in% c('cirrus', 'inlabel')) {
    if (mode=='inlabel')
      string <- paste0('inlabel:', string)
    if (langs != '' && mode=='inlabel')
      string <- paste0(string, '@', sub('|', ',', langs, fixed=T))
    #
    query <- paste0(query, "WHERE {\n  SERVICE wikibase:mwapi {
    bd:serviceParam wikibase:api \"Search\";
                    wikibase:endpoint \"www.wikidata.org\";
                    mwapi:srsearch '", string, "'.
    ?entity wikibase:apiOutputItem mwapi:title.
  }")
  }
  else {
    stop("ERROR: wrong mode specified.")
  }
  #
  # The final part of the query:
  query <- paste0(query, "\n  OPTIONAL {?entity wdt:P31 ?instanc.}\n",
                  sq, "\n} GROUP BY ?entity ", ss1, "\n")
  #
  if (debug == "query")
    cat(query, file=stderr())
  #
  r <- w_query(query, format = "csv", method="GET")
  if(is.null(r)){
    return(NULL)
  }
  if (nrow(r) == 0)
    return(r)
  #
  for (e in c('entity',  'instanceof'))
    r[[e]] <- gsub('http://www.wikidata.org/entity/', '', r[[e]])
  if (Pproperty!='')
    for (e in strsplit(Pproperty, '|', fixed = T)[[1]])
      r[[e]] <- gsub('http://www.wikidata.org/entity/', '', r[[e]])
  rownames(r) <- r$entity
  #
  # Filter instanceof
  if (instanceof!='')
    r <- r[grepl(paste0('\\b(?:', instanceof, ")\\b"), r$instanceof), ]
  #
  return(r)
}


#' Get information about a Wikimedia entity (human or film)
#'
#' Get labels, descriptions and some properties of the Wikidata entities in
#' entity_list, for person or films. If person, the information returned is
#' about labels, descriptions, birth and death dates and places, occupations,
#' works, education sites, awards, identifiers in some databases, Wikipedia page
#' titles (which can be limited to the languages in the `wikilangs` parameter,
#' etc. If films, information is about title, directors, screenwriter,
#' castmember, producers, etc.
#'
#' @param entity_list The Wikidata entities to search for properties (person or
#' films.
#' @param mode In "default" mode, the list of entities is expected to correspond
#' to person, obtaining information related to person. If the mode is "film",
#' information related to films will be requested. If the mode is "tiny" less
#' properties are requested.
#' @param langsorder Order of languages in which the information will be
#' returned, separated with '|'. If no information is given in the first
#' language, next is used. For label and description, English is used for
#' language failback, if they are not in English, then information is returned
#' in any else language. The language for label and description are also
#' returned. If langsorder=='', then no other information than labels or
#' descriptions are returned in any language, only Wikidata entities, else,
#' use the order in this parameter to retrieve information.
#' @param wikilangs List of languages to limit the search of Wikipedia pages,
#' using "|" as separator. Wikipedias pages are returned in same order as
#' languages in this parameter. If wikilangs='' the function returns Wikipedia
#' pages in any language, not sorted.
#' @param nlimit If the number of entities exceeds this number, chunked queries
#' are done. This is the number of entities requested in each chunk. Please,
#' reduce the default value if error is raised.
#' @param debug For debugging (info or query)
#' @return A data-frame with the properties of the entity. Also index is set to
#' entity_list.
#' @importFrom collections dict
#' @examples
#' \dontrun{
#' df <- w_EntityInfo(entity_list='Q134644', langsorder='es|en')
#' df <- w_EntityInfo(entity_list='Q134644', langsorder='es|en', wikilangs='es|en|fr')
#' df <- w_EntityInfo(c('Q270510', 'Q1675466', 'Q24871'), mode='film',
#'                    langsorder='es|en', wikilangs='es|en|fr')
#' # Search string 'abba' inlabel
#' w <- w_SearchByLabel('abba', mode='inlabel', langsorder = '', instanceof = 'Q5')
#' df <- w_EntityInfo(w$entity, langsorder='en', wikilangs='en|es|fr', debug='info')
#' # Search 3D films
#' w <- w_SearchByInstanceof(instanceof='Q229390', langsorder = 'en|es', debug = 'info')
#' df <- w_EntityInfo(w$entity, mode="film", langsorder='en', wikilangs='en', debug='info')
#' }
#' @author Angel Zazo, Department of Computer Science and Automatics, University of Salamanca
#' @export
w_EntityInfo <- function(entity_list, mode='default', langsorder='',
                         wikilangs="",  nlimit=MW_LIMIT, debug=FALSE) {
  # Check entity_list
  entity_list <- checkEntities(entity_list)
  #
  if (mode=='film') {
    # For these claims, more than one occurrences separated with '|' are token, except
    # for fields in fieldsonlyone, in which only the most referenced is taken
    fields <- c('P31', 'P577', 'P3383', 'P18', 'P10', 'P1476', 'P2047', 'P144',
                'P135', 'P136', 'P495', 'P364', 'P57', 'P58', 'P161', 'P725',
                'P1431', 'P344', 'P1040', 'P2554', 'P86', 'P162', 'P272',
                'P462', 'P180', 'P921', 'P166', 'P444', 'P214', 'P480', 'P345')
    names(fields) <- c('instanceof', 'pubdate', 'poster', 'pic', 'video', 'title',
                       'duration', 'basedon', 'movement', 'genre', 'country',
                       'originallanguage', 'director', 'screenwriter', 'castmember',
                       'voiceactor', 'executiveproducer', 'photographdirector',
                       'filmeditor', 'productiondesigner', 'composer', 'producer',
                       'productioncompany', 'color', 'depicts', 'mainsubject',
                       'award', 'reviewscore', 'VIAF', 'FilmAffinity', 'IMDb')
    # For this claims, only the most referred is taken (note, however that the
    # preferred has priority).
    fieldsonlyone <- c('P577', 'P1476', 'P2047')
    # Note that wikipedias is added at last as new column of the dataframe
    columns <- c('entity', 'status', 'labellang', 'label', 'descriptionlang',
                 'description', 'instanceofQ', 'instanceof', 'pubdate', 'pubyear',
                 'poster', 'pic', 'video', 'duration', 'title', 'basedonQ',
                 'basedon', 'movementQ', 'movement', 'genreQ', 'genre',
                 'countryQ', 'country', 'originallanguageQ', 'originallanguage',
                 'directorQ', 'director', 'screenwriterQ', 'screenwriter',
                 'castmemberQ', 'castmember', 'voiceactorQ', 'voiceactor',
                 'executiveproducerQ', 'executiveproducer',
                 'photographdirectorQ', 'photographdirector', 'filmeditorQ', 'filmeditor',
                 'productiondesignerQ', 'productiondesigner', 'composerQ', 'composer',
                 'producerQ', 'producer', 'productioncompanyQ', 'productioncompany',
                 'colorQ', 'color', 'depictsQ', 'depicts', 'mainsubjectQ', 'mainsubject',
                 'awardQ', 'award', 'reviewscore', 'VIAF', 'FilmAffinity', 'IMDb',
                 'wikipedias')
  }
  else {
    # For these claims, more than one occurrences separated with '|' are token, except
    # for fields in fieldsonlyone, in which only the most referenced is taken
    fields <- c('P31', 'P18', 'P21', 'P69', 'P106', 'P101', 'P135', 'P136',
                'P737', 'P800', 'P463', 'P166', 'P214', 'P950', 'P4439',
                'P19', 'P20', 'P569', 'P570')
    names(fields) <- c('instanceof', 'pic', 'sex', 'educatedat', 'occupation',
                       'fieldofwork', 'movement', 'genre', 'influencedby', 'notablework',
                       'memberof', 'award', 'viafid', 'bneid', 'mncarsid', 'bplace',
                       'dplace', 'bdate', 'ddate')
    # For this claims, only the most referred is taken (note, however that the
    # preferred has priority).
    fieldsonlyone <- c('P19', 'P20', 'P569', 'P570')
    # Note that Wikipedias is added at last as new column of the dataframe
    columns <- c('entity', 'status', 'labellang', 'label', 'descriptionlang',
                 'description', 'instanceofQ', 'instanceof', 'sexQ', 'sex',
                 'bdate', 'byear', 'bplaceQ', 'bplace', 'bplaceLat', 'bplaceLon', 'bcountryQ', 'bcountry',
                 'ddate', 'dyear', 'dplaceQ', 'dplace', 'dplaceLat', 'dplaceLon', 'dcountryQ', 'dcountry',
                 'occupationQ', 'occupation', 'notableworkQ', 'notablework', 'educatedatQ', 'educatedat',
                 'fieldofworkQ', 'fieldofwork', 'movementQ', 'movement', 'genreQ', 'genre',
                 'influencedbyQ', 'influencedby', 'memberofQ', 'memberof', 'awardQ', 'award',
                 'viafid', 'bneid', 'mncarsid', 'pic', 'wikipedias')
  }
  #
  llangs <- strsplit(langsorder, '|', fixed = T)[[1]]
  # Failback English language
  if (!('en' %in% llangs))
    llangs <- append(llangs, 'en')
  langsorder <- paste0(llangs, collapse = '|')
  #
  wlangs <- character()   # wiki_langs
  if (wikilangs!='')
    wlangs <- strsplit(wikilangs, '|', fixed = T)[[1]]
  #
  d <- list()
  qidsoflabels <- character()  # store entities for searching labels
  qidsofplaces <- character()  # store entities for searching places
  #
  nq <- length(entity_list)
  # Check limits to make chucked queries
  nlim <- as.integer(nq/nlimit)
  if (nlim>0 && debug!=FALSE)
    cat(paste0("INFO: The number of entities (",nq ,") exceeds chunksize (", nlimit, ").\n"), file=stderr())
  for (k in 0:nlim)  {
    offset <- k*nlimit
    if ((offset+1) > nq)
      break
    t_list <- entity_list[(offset+1):min(nq,offset+nlimit)]
    if (nlim>0 && debug!=FALSE)
      cat(paste0("  INFO: Requesting elements from ", offset+1, " to ", offset+length(t_list),"\n"), file=stderr())
    #
    query <- list(format = 'json',
                  action = 'wbgetentities',
                  props  = 'labels|descriptions|claims|sitelinks',
                  ids    = paste0(t_list, collapse = '|'))
    #
    # Note that if wikilangs='', not only wikipedia, also other projects are returned.
    if (wikilangs!="")
      query["sitefilter"] = paste0(paste0(wlangs, 'wiki'), collapse='|')
    #
    j <- reqMediaWiki(query=query, project="www.wikidata.org", debug=debug)
    #
    if (is.null(j) || is.null(j$success) || j$success != 1)
      stop(paste0("Error in w_EntityInfo: reqMediaWiki returns an improper JSON."))
    #
    entities <- j$entities
    for (qid in names(entities)) {
      data <- entities[[qid]]
      #
      # if (debug!=FALSE)
      #   cat(paste0("  INFO: Requesting element ", qid,"\n"), file=stderr())
      #
      qiddict <- collections::dict(rep(NA, length(columns)), columns)   # set NA values, not NULL
      qiddict$set('entity', qid)
      qiddict$set('status', 'OK')
      # Check if entity is missing
      if (!is.null(data$missing)) {
        qiddict$set('status', 'missing')
        if (debug!=FALSE)
          cat(paste0("INFO: ",qid, " is missing\n"),file=stderr())
        next
      }
      # Check if entity is a redirection to other entity
      if (!is.null(data$redirects)) {
        qid_r <- data$id
        qiddict$set('status', qid_r)
        if (debug!=FALSE)
          cat(paste0("INFO: ",qid, " redirects to ", qid_r,
                     ", so all information returned is about ", qid_r, "\n"),file=stderr())
        if (debug!=FALSE && qid_r %in% entity_list)
          cat(paste0(" INFO: also note that ",qid_r,
                     " also is in the original entity list\n"), file=stderr())
      }
      #
      # Get the label and the description of the entity. Retrieves the first
      # in language order, or any one else, if exist, otherwise NA
      for (ld in c('label', 'description')) {
        item <- paste0(ld, 's')
        if (is.null(data[[item]]) || length(data[[item]])==0) {
          qiddict$set(paste0(ld, "lang"), NA)
          qiddict$set(ld, NA)
          next
        }
        lds <- data[[item]]
        existslang <- FALSE
        for (lang in llangs) {
          if (!is.null(lds[[lang]]) && is.null(lds[[lang]][['for-language']])) {
            existslang <- TRUE
            qiddict$set(paste0(ld,'lang'), lds[[lang]]$language)
            qiddict$set(ld, lds[[lang]]$value)
            break
          }
        }
        # If no label/description in any lang of langsorder, take the first one
        if (!existslang) {
          qiddict$set(paste0(ld,'lang'), lds[[1]]$language)
          qiddict$set(ld, lds[[1]]$value)
        }
      } # close: # Get the label and the description of the entity.

      # Processing claims
      claims <- data$claims
      #
      for (fname in names(fields)) {
        f <- fields[[fname]]
        values <- character()
        nrefs  <- integer()
        is_entity <- FALSE    # If the claim stores a Qxxx, add it to the corresponding "xxxQ" field
        for (item in claims[[f]]) {
          if (is.null(item$mainsnak$datavalue))  # Some information exists about claim, but unknown value is in the claim
            next
          valuetype <- item$mainsnak$datavalue$type
          value     <- item$mainsnak$datavalue$value
          # count references
          if (!is.null(item$references))
            nrefs <- append(nrefs, length(item$references) )
          else
            nrefs <- append(nrefs, 0)
          #
          if (valuetype == 'string')
            v <- value
          else if (valuetype == 'wikibase-entityid') {
            is_entity <- TRUE
            v <- value$id
            # Store qids to search labels later (not for places, because labels
            # are returned when retrieve places)
            if (!(f %in% c('P19', 'P20')))
              qidsoflabels <- append(qidsoflabels, v)
          }
          else if (valuetype == 'time')
            v <- value$time
          else if (valuetype == 'monolingualtext')
            v <- paste0(value$text, ':', value$language)
          else if (valuetype == 'quantity') {
            unit <- value$unit
            unit <- sub('http://www.wikidata.org/entity/(Q.*)', "\\1", unit)
            qidsoflabels <- append(qidsoflabels, unit)
            v <- paste0(value$amount, ' : ', unit)
          }
          else
            cat(paste0("WARNING w_EntityInfo: valuetype ", valuetype, " not implemented"), file=stderr())
          #
          # Check reviewers for P444 (review score)
          if (f=='P444' && !is.null(item$qualifiers) && !is.null(item$qualifiers$P447)) {
            reviewerQ <- item$qualifiers$P447[[1]]$datavalue$value$id
            qidsoflabels <- append(qidsoflabels, reviewerQ)
            v <- paste0(v, " [", reviewerQ, "]")
          }
          # Rank=preferred takes preference
          if (!is.null(item$rank) && item$rank=='preferred') {
            values <- c(v)
            nrefs  <- c(0)
            break
          }
          else
            values <- append(values, v)
        }

        if (length(values)!=0) { # The claim almost has one not erroneous value
          if (!(f %in% fieldsonlyone))
            qiddict$set(fname, paste0(unique(values), collapse = '|'))
          else {
            # Get the most referred value
            index_max <- which.max(nrefs)
            v <- values[index_max]
            qiddict$set(fname, v)
          }
          # Store qids to search places later
          if (f %in% c('P19', 'P20'))
            qidsofplaces <- append(qidsofplaces, v)
          # Extract year from bdate/ddate/pubdate:
          if (f %in% c('P569', 'P570', 'P577') && (f %in% fields)) {
            ff <- sub("date", "year", fname)
            qiddict$set(ff, sub("^.(.{4}).+$","\\1", v))
          }
          # if is_entity add qids to the corresponding "Q" field
          if (is_entity)
            qiddict$set(paste0(fname,'Q'), qiddict$get(fname))
        }
      }

      # Wikipedias
      sitelinks <- data$sitelinks
      if (length(sitelinks)>0) {
        wvalues <- character()
        if (length(wlangs)==0) {
          for (wlwiki in names(sitelinks)) {
            if (endsWith(wlwiki, 'wiki')) {
              wl <- sub("^(.+).{4}$", "\\1", wlwiki)
              title <- gsub(" ", "_", sitelinks[[wlwiki]]$title)
              title <- URLencode(title)
              wvalues <- append(wvalues, paste0('https://',wl,'.wikipedia.org/wiki/',title))
            }
          }
        }
        else {
          for (wl in wlangs) {
            wlwiki <- paste0(wl, 'wiki')
            if (wlwiki %in% names(sitelinks)) {
              title <- gsub(" ", "_", sitelinks[[wlwiki]]$title)
              title <- URLencode(title)
              wvalues <- append(wvalues, paste0('https://',wl,'.wikipedia.org/wiki/',title))
            }
          }
        }
        if (length(wvalues) > 0)
          qiddict$set('wikipedias', paste0(wvalues, collapse = '|'))
      }
      # Add URL parts to the pic to download, if any:
      for (fname in c('poster', 'pic', 'video')) {
        if (qiddict$has(fname) && !is.na(qiddict$get(fname))) {
          pic <- qiddict$get(fname)
          pics <- strsplit(pic, '|', fixed = T)[[1]]
          pics <- URLencode(sub(" ","_", pics))
          pics <- paste('https://commons.wikimedia.org/wiki/Special:FilePath/',pics, sep='')
          qiddict$set(fname, paste0(pics, collapse = '|'))
        }
      }
      #
      # Adding qiddict to d
      d <- append(d, qiddict)
      #
    } ## close: for (qid in names(entities))
    #
  } ## close: for (k in 0:nlim)
  #
  names(d) <- entity_list
  #
  # Only for mode='default': Search places for labels, coords (Latitude and Longitude) and countries
  placesdict <- collections::dict()
  if (length(qidsofplaces) > 0) {
    qidsofplaces <- unique(qidsofplaces)
    if (debug!=FALSE)
      cat(paste0("INFO: Searching labels, latitude and longitude coordinates, and countries for places.\n"), file=stderr())
    places <- w_Geoloc(qidsofplaces, langsorder=langsorder, debug=debug)
    # Set colnames in right form
    colnames(places) <- c('placeQ', 'place', 'placeLat', 'placeLon', 'countryQ', 'country')
    # create a dict of dicts for fast allocating
    for (qplace in rownames(places)) {
      placesdict$set(qplace, collections::dict(items=places[qplace, ], keys=colnames(places)))
    }
    # Add labels, geo-coordinates and country to entities
    for (qid in names(d)) {
      # data <- d[[qid]]
      for (bd in c('b','d')){
        fname <- paste0(bd,'placeQ')  # bplaceQ / dplaceQ
        if(!is.na(d[[qid]]$get(fname)) && placesdict$has(d[[qid]]$get(fname))){
          pdict <- placesdict$get(d[[qid]]$get(fname))    # get the Qxxx of the bplaceQ or dplaceQ
          d[[qid]]$update(collections::dict(items=pdict$values(),
                                            keys=paste0(bd, pdict$keys())))
        }
      }
    }
  }
  # Search labels for the rest of Qxxx entities
  labelsdict <- collections::dict()
  if (length(qidsoflabels) > 0) {
    qidsoflabels <- unique(qidsoflabels)
    if (debug!=FALSE)
      cat(paste0("INFO: Searching labels for Wikidata entities.\n"), file=stderr())
    labels <- w_LabelDesc(qidsoflabels, what='L', langsorder=langsorder, debug=debug)
    if(!is.null(labels)){
      # create a dict of labels for fast allocating
      labelsdict <- collections::dict(items=labels$label, keys=rownames(labels))
      # Add labels to entities
      for (qid in names(d)) {
        data <- d[[qid]]
        for (fname in data$keys()) {
          if (is.na(data$get(fname)))
            next
          if (endsWith(fname,'Q') && !(fname %in% c('bplaceQ', 'dplaceQ', 'bcountryQ', 'dcountryQ'))) {
            # Get the correct field (without "Q")
            fnn <- sub("^(.+)Q$","\\1", fname)
            # Replace all occurrences of Qxxx in those fields
            qx <- strsplit(data$get(fnn), '|', fixed = T)[[1]]
            labelsx <- lapply(qx, function(x) {labelsdict$get(x)})
            d[[qid]]$set(fnn, paste0(labelsx, collapse='|'))
          }
          if (fname == 'duration' && !is.na(data$get('duration'))) {     # data['duration'] = '+125 (Q7727)'
            duration <- data$get('duration')
            if (grepl('Q\\d+', duration))
              durationq <- regmatches(duration, regexec('Q\\d+', duration))[[1]]
            data$set('duration', sub(': Q\\d+', labelsdict$get(durationq), duration))
          }
          if (fname == 'reviewscore' && !is.na(data$get('reviewscore'))) {  # data['reviewscore'] = '99/00 [Q1234]'
            rsl <- character()
            for (rs in strsplit(data$get('reviewscore'), '|', fixed = T)[[1]]) {
              if (grepl('Q\\d+', rs)) {
                rsq <- regmatches(rs, regexec('Q\\d+', rs))[[1]]
                rsl <- append(rsl, sub('Q\\d+', labelsdict$get(rsq), rs))
              }
            }
            data$set('reviewscore', paste0(rsl, collapse = '|'))
          }
        }
      }
    }
  }
  # Finally, convert the dict of dicts to a dataframe
  df <- as.data.frame(do.call(rbind,lapply(d, function(x) unlist(x$as_list()))))

  if(mode=="tiny"){
    tinycolumns <- c('entity', 'labellang', 'label', 'descriptionlang', 'description', 'sex',
        'bdate', 'byear', 'bplace', 'bplaceLat', 'bplaceLon', 'bcountry',
        'ddate', 'dyear', 'dplace', 'dplaceLat', 'dplaceLon', 'dcountry',
        'occupationQ', 'occupation', 'viafid', 'bneid', 'mncarsid', 'pic', 'wikipedias')
    df <- df[,intersect(tinycolumns,colnames(df))]
  }

  return(df)
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
#' limit of 50 titles in each query. If number of titles is greater than this
#' limit a error is raised.
#' @param project The Wikimedia project to search. Default en.wikipedia.org.
#' @param method The method used in the httr request. Default 'GET'.
#' Note in "https://www.mediawiki.org/wiki/API:Etiquette#Request_limit":
#' "Whenever you're reading data from the web service API, you should try to use
#' GET requests if possible, not POST, as the latter are not cacheable."
#' @param attempts On ratelimit errors, the number of times the request is
#' retried using a 60 seconds interval between retries. Default 2. If 0 no
#' retries are done.
#' @param debug For debugging purposes (default FALSE). If debug='info'
#' information about chunked queries is shown. If debug='query' also the query
#' launched is shown.
#' @return The response in JSON format, raise exception on errors.
#' @author Angel Zazo, Department of Computer Science and Automatics, University of Salamanca
#' @importFrom httr GET POST content add_headers stop_for_status
#' @importFrom jsonlite fromJSON
#' @importFrom utils URLdecode
reqMediaWiki <- function(query, project='en.wikipedia.org', method='GET',
                         attempts=2, debug=FALSE) {
  # Check for titles in query
  if ('titles' %in% names(query)) {
    titles <- strsplit(query$titles, '|', fixed = T)[[1]]
    n <- length(titles)
    if (n > MW_LIMIT)
      stop(paste0("ERROR: number of titles (", n,") exceeds Wikipedia API limit (",MW_LIMIT,")\n"))
  }
  # Check for ids in query
  if ('ids' %in% names(query)) {
    ids <- strsplit(query$ids, '|', fixed = T)[[1]]
    n <- length(ids)
    if (n > MW_LIMIT)
      stop(paste0("ERROR: number of ids (", n,") exceeds Wikipedia API limit (",MW_LIMIT,")\n"))
  }
  #
  url = paste0('https://', project, "/w/api.php")
  if(!curl::has_internet() || httr::http_error(url,query = query)){
    message("No internet connection or data source broken.")
    return(NULL)
  }
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
      if (debug == "query")
        cat(paste0(URLdecode(r$request$url), "\n"), file=stderr())
      #
      # Converts http errors to R errors or warnings
      httr::stop_for_status(r)
      content <- httr::content(r, as = "text", encoding = "UTF-8")
      j <- jsonlite::fromJSON(content, simplifyVector = FALSE)
      #
      # See https://www.mediawiki.org/wiki/API:Etiquette#Request_limit
      if (!is.null(j$error)) {
        if (j$error$code == 'ratelimited') {
          if (nt > attempts)
            stop(paste0(as.character(nt)," ratelimited attempts achieved, aborting.\n"))
          else {
            t <- 60*nt
            cat(paste0("INFO: ", nt," ratelimited attempt(s) error. Sleeping", as.character(t), "seconds.\n"),
                file = stderr())
            Sys.sleep(t)
          }
        }
        else {
          stop(paste0("ERROR: reqMediaWiki(): ", j$error$code, ": ", j$error$info))
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

#' Return the normalized and redirect title from the response
#'
#' Return the normalized and the redirect title (also normalized), if any, from
#' the query part of the JSON response of a MediaWiki search. The response of
#' the MediaWiki API query (https://www.mediawiki.org/wiki/API:Query) includes
#' original page titles and possibily normalized and redirected titles, if the
#' API needs to obtain them. For a original title, this function returns them,
#' if any.
#' @param title The title likely to be found in q.
#' @param q The query part of the JSON response (j\['query'\]) from a Mediawiki
#' search. Note that this part contains some titles, so it is necessary to
#' search the original "title" in that part.
#' @return A vector with the normalized or redirected page title (target, also
#' normalized) found for the title.
normalizedTitle <- function(title, q) {
  # if ( !(jsonlite::validate(jsonlite::toJSON(q))) ) ## Very slow!!!
  #    stop("ERROR: j is not a valid JSON structure.")
  #
  anorm <- title
  # Is normalized (and possibly encoded)? # See https://phabricator.wikimedia.org/T29849#2594624
  # It is supposed that order is: first, NFC normalization, then, uppercase normalization.
  if (!is.null(q$normalized)) {
    for (nn in q$normalized) {
      if (nn$fromencoded && URLencode(anorm) == nn$from)  # NFC normalization
        anorm <- nn$to
      if (nn$from == anorm)  { # Uppercase normalization
        anorm <- nn$to
        break
      }
    }
  }
  # Is a redirect? The normalized titles is used.
  if (!is.null(q$redirects)) {
    for (nn in q$redirects)
      if (nn$from == anorm) {
        anorm <- nn$to
        break
      }
  }
  return(anorm)
}

#' checkTitles(titles)
#' Check if titles are valid. Return TRUE is all titles are valid, else FALSE.
#' See https://en.wikipedia.org/wiki/Wikipedia:Page_name#Technical_restrictions_and_limitations
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
  output <- NULL
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
    if (is.null(j) || is.null(j$query)){
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
    if (is.null(j) || is.null(j$query)){
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
  output <- NULL
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
    if (is.null(j) || is.null(j$query)){
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
          if (!is.null(page$original) && !is.null(page$original$source))
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
  output <- NULL
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
    if (is.null(j) || is.null(j$query)){
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
  url <- URLencode(url)
  if(curl::has_internet() && !httr::http_error(url)){
    httr::GET(
      url = url,
      httr::user_agent(user_agent),
      httr::add_headers(Accept = "application/json")
    )
  }else{
    message("No internet connection or data source broken.")
  }
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
    if(!is.null(r)){
      httr::stop_for_status(r)
      content <- httr::content(r, as = "text", encoding = "UTF-8")
      j <- jsonlite::fromJSON(content, simplifyVector = FALSE)
      for (v in j$items)
        a[v$timestamp] <- v$views
    }
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
  if(is.null(r)){
    return(NULL)
  }
  httr::stop_for_status(r)
  content <- httr::content(r, as = "text", encoding = "UTF-8")
  d <- jsonlite::fromJSON(content, simplifyVector = FALSE)
  #
  if (infotype == "links" && redirects) {
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
# https://developer.api.oclc.org/viaf-api#/Authority%20Cluster
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
#' @seealso https://developer.api.oclc.org/viaf-api#/Authority%20Cluster
#' @examples
#' v_AutoSuggest('Iranzo')
#' v_AutoSuggest('Esparza, María')
#' # Four rows, only two viafid:
#' v_AutoSuggest('Escobar, Modesto')
#' @export
v_AutoSuggest <- function(author) {
  url <- "https://www.viaf.org/viaf/AutoSuggest"
  query <- list(query = author)
  if(!curl::has_internet() || httr::http_error(url,query=query)){
    message("No internet connection or data source broken.")
    return(NULL)
  }
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
#' See https://developer.api.oclc.org/viaf-api#/Authority%20Cluster
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
#' @param schema The recordSchema of the query: if 'brief' (defaults) the
#' records returned are more simple. If 'JSON', then de complete cluster records
#' are returned.
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
v_Search <- function(CQL_Query, mode=c('default', 'anyField', 'allmainHeadingEl',
                                       'allNames', 'allPersonalNames', 'allTitle'),
                     schema=c('JSON', 'brief')) {
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

  schema <- schema[1]
  if(schema=='brief')
    recordSchema <- 'http://viaf.org/BriefVIAFCluster'
  else
    recordSchema <- 'info:srw/schema/1/JSON'

  searchFn <- function(cql_query){
    maxrecords <- 250
    url <- "https://www.viaf.org/viaf/search"
    query <- list(httpAccept = 'application/json',
                  maximumRecords = maxrecords,
                  recordSchema = recordSchema,
                  startRecord = 1,
                  query = cql_query)
    if(!curl::has_internet() || httr::http_error(url,query=query)){
      message("No internet connection or data source broken.")
      return(NULL)
    }
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
#' https://developer.api.oclc.org/viaf-api#/Authority%20Cluster
#' @return The VIAF record cluster in the format indicated in record_format.
#' @export
v_GetRecord <- function(viafid, record_format='viaf.json') {
  url <- paste0("https://viaf.org/viaf/", viafid, '/', record_format)
  if(!curl::has_internet() || httr::http_error(url)){
    message("No internet connection or data source broken.")
    return(NULL)
  }
  r <- httr::GET(
    url = url,
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
  opts <- c('titles', 'gender', 'dates', 'occupations', 'sources', 'sourceId', 'coauthors', 'wikipedias', 'allinfo')
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
  if (is.null(viaf$titles) || is.null(viaf$titles$work))
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
    df2 <- data.frame(occ)
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

# Gets coauthors from a VIAF record
v_coauthors <- function(viaf) {
  if (is.null(viaf$coauthors))
    return(NULL)
  #
  dd <- viaf$coauthors$data
  if (!is.null(dd$text))
    dd <- list(dd)
  # df1 is the returned data-frame.
  df1 = data.frame()
  coa <- list()
  for (l in dd) {
    coa <- list()
    coa$coauthor <- l$text
    coa$count    <- l[['@count']]
    df2 <- data.frame(coa)

    ## Unify colname of first column
    if (nrow(df1) == 0)
      df1 <- df2
    else
      # https://stackoverflow.com/questions/3402371/combine-two-data-frames-by-rows-rbind-when-they-have-different-sets-of-columns
      df1 <- rbind(
        data.frame(c(df1, sapply(setdiff(names(df2), names(df1)), function(x) NA))),
        data.frame(c(df2, sapply(setdiff(names(df1), names(df2)), function(x) NA)))
      )
    #
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
  if (is.null(viaf$xLinks) || is.null(viaf$xLinks$xLink))
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

# Gets all in one
v_allinfo <- function(viaf) {
  l <- list()
  l$vid <- viaf$viafID
  l$gender <- v_gender(viaf)
  l$dates  <- v_dates(viaf)
  l$sources <- v_sources(viaf)
  l$titles <- v_titles(viaf)
  l$occupations <- v_occupations(viaf)
  l$coauthors <- v_coauthors(viaf)
  l$wikipedias <- v_wikipedias(viaf)
  return(l)
}
