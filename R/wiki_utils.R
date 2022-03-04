#### wiki_utils.R
#### Angel F. Zazo <angelzazo@usal.es>
#### 2021-11-09

# General headers
my_user_agent <- paste('netCoincidenceAnalysis BOT <angelzazo@usal.es>.', R.version.string)
my_headers <- c(accept = 'application/json',
                'user-agent' = my_user_agent)

#' limit_requester
#' Limit the rate at which a function will execute
#' @param f The original function
#' @param n Number of allowed events within a period
#' @param period Length (in seconds) of measurement period
#' @author Angel F. Zazo, Departament of Computer Science and Automatics, University of Salamanca
#' @seealso ratelimitr
#' @importFrom ratelimitr limit_rate rate
#' @export
limit_requester <- function(f, n, period) {
  return(ratelimitr::limit_rate(f, ratelimitr::rate(n = n, period = period)))
}

#' Wikimedia_query
#' Use httr package to retrieve responses in JSON format about an article from Wikimedia API.
#' @param query A list with de (key, values) pairs with the search.
#' @param project The Wikimedia project to search.
#' @param headers A vector with aditional query headers for the request.
#' @param attempts On errors, the maximun number of times the query is launched if repetition_on_error is not zero (default 2)
#' @return The response in JSON format or NULL on errors.
#' @author Angel F. Zazo, Departament of Computer Science and Automatics, University of Salamanca
#' @importFrom httr GET content add_headers stop_for_status
#' @importFrom jsonlite fromJSON
#' @export
Wikimedia_query <- function(query, project='en.wikipedia.org', headers = my_headers, attempts = 2) {
  url = paste('https://', project, "/w/api.php", sep = '')
  nt <- 1
  tryCatch( {
    while(TRUE) {
      nt <- nt + 1         
      r <- httr::GET(url, query=query, httr::add_headers(.headers = headers))
      # Converts http errors to R errors or warnings
      httr::stop_for_status(r)
      content <- httr::content(r, as = "text", encoding = "UTF-8")
      j <- jsonlite::fromJSON(content, simplifyVector = FALSE)
      #
      # See https://www.mediawiki.org/wiki/API:Etiquette#Request_limit
      if ( !is.null(j$error) && j$error$code == 'ratelimited') {
        if (nt > attempts)
          stop(paste(as.character(nt)," ratelimited achieved, aborting query", sep=''))
        else {
          t = 60*nt
          print(paste("ratelimited error. Sleeping", as.character(t), "seconds"))
          Sys.sleep(t)
        }
      }
      else {
        return(j)
        }
      }
    },  error = function(e){
      print(e)
      return(NULL)
    }
  )
}


#' req_WDQS
#' Retrieve responses in JSON format from Wikidata Query Service (WDQS)
#' @param sparql_query A string with the query in SPARQL language.
#' @return A JSON response. Please check httr::stop_for_status(response)
#' @author Angel F. Zazo, Departament of Computer Science and Automatics, University of Salamanca
#' @importFrom httr GET user_agent add_headers
#' @note For short queries GET method is better, POST for long ones. Only GET queries as cached.
#' @export
req_WDQS <- function(sparql_query) {
  httr::GET(  # 
    url = 'https://query.wikidata.org/sparql',
    query = list(query = sparql_query),
    httr::user_agent(my_user_agent),
    httr::add_headers(Accept = "application/sparql-results+json")
  )
}


#' Wikidata_sparql_query
#' Retrieve responses in JSON format from Wikidata Query Service (WDQS)
#' #' req_WDQS_rated
#' The ratelimitr version of req_WDQS_rate.
#' See https://www.mediawiki.org/wiki/Wikidata_Query_Service/User_Manual#SPARQL_endpoint
#' @param sparql_query A string with the query in SPARQL language.
#' @return A JSON response or NULL on errors.
#' @author Angel F. Zazo, Departament of Computer Science and Automatics, University of Salamanca
#' @importFrom httr stop_for_status content 
#' @importFrom jsonlite fromJSON
#' @export
Wikidata_sparql_query <- function(sparql_query) {
  req_WDQS_rated <- limit_requester(req_WDQS, n=30, period=60)
  tryCatch(
    {
      r <- req_WDQS_rated(sparql_query)
      httr::stop_for_status(r)
      content <- httr::content(r, as = "text", encoding = "UTF-8")
      temp <- jsonlite::fromJSON(content, simplifyVector = FALSE)
      return(temp)
    }, error = function(e){
      print(e)
      return(NULL)
    }
  )
} 

#' GetWikidataitem
#' Use Wikimedia_query to obtain the Wikidata entity of a article from a Wikimedia project.
#' Automatically resolvs redirects.
#' @param article Article to search
#' @param project Wikimedia project, defaults "en.wikipedia.org"
#' @return A vector with the firts element to 1 if exists the Wikidata item and if not a
#'         disambiguation page, the second element de normalized forma of article, and the third the wikidata item. If errors, the firts element is set to 0 and the third is the explication of error.
#' @author Angel F. Zazo, Departament of Computer Science and Automatics, University of Salamanca
#' @examples 
#' GetWikidataitem('Max Planck', project='es.wikipedia.org')
#' 
#' GetWikidataitem('Max')
#' 
#' GetWikidataitem('Cervante')
#' @export
GetWikidataitem <- function(article = '', project = 'en.wikipedia.org') {
  if ((article == '') | (project == '')) {
    return(c(0, '', "No article or project present"))
  }
  query = list(format        = 'json',
               formatversion = '2',
               action        = 'query',
               redirects     = '1',
               prop          = 'pageprops',
               ppprop        = 'wikibase_item|disambiguation',
               titles        = article)
  j <- Wikimedia_query(query, project)
  if (is.null(j))
    return(c(0, article, "Error response from Wikimedia_query"))
  #
  if (is.null(j$query)) 
    return(c(0, article, "No query response"))
  #
  # Si tiene redirects, lo tomo (ya lo da normalizado, no se pasa a "normalized")
  if (!is.null(j$query$redirects))
    article <- j$query$redirects[[1]]$to
  # Si no tiene redirects, tomo el normalized, de existir.
  # Nota: puede haber varios: ver https://phabricator.wikimedia.org/T29849#2594624
  else if (!is.null(j$query$normalized))
    for (nn in j$query$normalized)
      article <- nn$to
  #
  # Con formatversion=2 se obtiene en j$query$pages un vector. Si solo se ha
  # solicitado un title, este será el único elemento, es decir el 1
  page <- j$query$pages[[1]]
  #
  # 
  if (!is.null(page$missing))  # No se encuentra el título buscado
    return (c(0, article, "missing"))
  #
  if (is.null(page$pageprops))  # No hay pageprops (por tanto, tampoco wikidta_item)
    return(c(0, article, "no_pageprops"))
  #
  if (!is.null(page$pageprops$disambiguation))
    return(c(0,article, "disambiguation"))

  if (is.null(page$pageprops$wikibase_item)) # No tiene wikibase_item
    return(c(0, article, "no_wikibase_item"))
  #
  # Finalmente se devuelve wikibase_item
  return(c(1, article, page$pageprops$wikibase_item))
}

# -----------------------
#' Wikimedia_get_redirects
#' Obtains redirection pages (from namespace 0) to the article page in the Wikimedia project
#' @param article Article target
#' @param project Wikimedia project, defaults "en.wikipedio.org"
#' @return A list with the firts element the target of all redirections, or NULL on error.
#' @author Angel F. Zazo, Departament of Computer Science and Automatics, University of Salamanca
#' @export
Wikimedia_get_redirects <- function(article, project = "en.wikipedia.org") {
  if ((article == '') | (project == '')) {
    return(NULL)
  }
  query = list(format        = 'json',
               formatversion = '2',
               action        = 'query',
               redirects     = '1',
               prop          = 'redirects',
               rdnamespace   = '0',              # Only from namespace = 0
               rdlimit       = 'max',            # Change to 5 for testing
               rdprop        = 'title',
               titles        = article)
  titles <- character()  # An empty character vector
  repeat {
    # print(cbind(query))  # checking
    j <- Wikimedia_query(query, project = project)
    #
    if (is.null(j) | is.null(j$query))    # Error response from Wikimedia_query/No query response
      return(NULL)
    #
    if (is.null(query$continue)) {   # First, there isn't 'continue' response
      #  
      # Si tiene redirects, lo tomo (ya lo da normalizado, no se pasa a "normalized")
      if (!is.null(j$query$redirects))
        titles <- append(titles, j$query$redirects[[1]]$to)  
      # Si no tiene redirects, tomo el normalized, de existir.
      # Nota: puede haber varios: ver https://phabricator.wikimedia.org/T29849#2594624
      else if (!is.null(j$query$normalized)) {
        for (nn in j$query$normalized)
          article <- nn$to
        titles <- append(titles, article)
      }
      else
        titles <- append(titles, article)
    }      
    #
    # Con formatversion=2 se obtiene en j$query$pages un vector. Si solo se ha
    # solicitado un title, este será el único elemento, es decir el 1
    page <- j$query$pages[[1]]
    # 
    if (!is.null(page$missing))  # No se encuentra el título buscado (missing)
      return(NULL)
    #
    titles <- append(titles, sapply(page$redirects,function(x){ return(x[["title"]]) }))
    #
    if (!is.null(j$continue)) {
      query$continue   <- j$continue$continue
      query$rdcontinue <- j$continue$rdcontinue
    }
    else
      return(titles)
  }
}

#' Wikimedia_person_exists
#' Use Wikimedia_query and Wikidata_sparql_query to check if a article of a person
#' exists in the Wikimedia project. If exists, also return the Wikipedia pages of
#' that person in the languages indicated in param lang
#' @param article Article to search
#' @param project Wikimedia project, defaults "en.wikipedia.org"
#' @param langs Wikipedia languages to search if the person has a page, use "|" to split languages
#' @return If the article of the person exists, a vector with four elements: the firts one set to 1, the second de article label normalized, the third de Wikidata id, and fourth a data frame with URL to Wikipedias (lang, label, URL)
#'         If the article of the person does not exist, the firts element is set to 0 and the third is the explication of error.
#' @author Angel F. Zazo, Departament of Computer Science and Automatics, University of Salamanca
#' @export
Wikimedia_person_exists <- function(article, project="en.wikipedia.org",
                                    langs="en|es|fr|de|it|pt|ca") {
  ret <- GetWikidataitem(article, project)
  if (ret[1] == 0)
    return(ret)
  #
  art <- ret[2]  # Normalized article label
  qid <- ret[3]
  langs <- paste0("'",
                  paste0(strsplit(langs, "|", fixed=T)[[1]], collapse="', '"),
                  "'")
  query = paste0('SELECT (STR(?article) as ?url) ?lang ?name
  WHERE {
    wd:',qid,' wdt:P31 ?instance .
    FILTER (?instance = wd:Q5)
    ?article schema:about wd:',qid,';
             schema:inLanguage ?lang;
             schema:name ?name;
             schema:isPartOf [ wikibase:wikiGroup "wikipedia" ] .
    FILTER(?lang in (', langs, ')) .
    }')
  j <- Wikidata_sparql_query(query)
  if (is.null(j))
    return(c(0, art, qid, 'Error response from Wikidata_sparql_query'))
  #
  bindings <- j$results$bindings
  if (length(bindings) == 0)
    return(c(0, art, qid, 'Not human'))
  #
  columns <- unlist(j$head$vars)
  df <- as.data.frame(sapply(columns,function(y){
    return(sapply(bindings,function(x){
      return(x[[y]][["value"]])
    }))
  }))
  return(list(qid = c(1, art, qid),
              wiki = df))
}

#' Wikidata_occupationCount
#' Search Wikidata Query Service (WDQS) to know the number of Wikidata entities with P106 property (occupation) set to Qoc.
#' @param Qoc The Wikidata entity of the occupation
#' @return The number of entities with that occupation (integer)
#' @author Angel F. Zazo, Departament of Computer Science and Automatics, University of Salamanca
#' @examples
#' Wikidata_occupationCount('Q2526255') # Film director
#' @export
Wikidata_occupationCount <- function(Qoc='') {
  if (Qoc == '')
    return(NULL)
  #
  query = paste0('SELECT (COUNT(*) AS ?count) WHERE {?human wdt:P106 wd:',Qoc," .}")
  j <- Wikidata_sparql_query(query)
  return(as.integer(j$results$bindings[[1]]$count$value))
}


#' req_wikimedia_metrics
#' Retrieve responses in JSON format from Wikimedia metrics API
#' @param url The URL with the query
#' @return A JSON response. Please check httr::stop_for_status(response)
#' @author Angel F. Zazo, Departament of Computer Science and Automatics, University of Salamanca
#' @importFrom httr GET user_agent add_headers
#' @note Used in Wikimedia_page_views
#' @export
req_wikimedia_metrics <- function(url) {
  httr::GET(
    url = url,
    httr::user_agent(my_user_agent),
    httr::add_headers(Accept = "application/json")
  )
}




#' Wikimedia_page_views
#' Return the number of views one article has in a Wikimedia project in a date interval (see granularity). Optionally include redirections to the article page.
#' req_wikimeida_metrics_rated
#' The limitratedr version of req_wikimedia_metrics.
#  Limit is 100 req/s, we limit 50 req/s.
#' @param article The article to search
#' @param project The Wikimedia project, defaults en.wikipedia.org
#' @param start,end First and last day to include (format YYYYMMDD or YYYYMMDDHH)
#' @param access  Filter by access method: all-access (default), desktop, mobile-app, mobile-web
#' @param agent   Filter by agent type: all-agents, user (default), spider, automated
#' @param granularity  Time unit for the response data: daily, monthly (default)
#' @param include_redirects Boolean to include redirection to the article page (defaults: False)
#' @return The number of visits
#' @author Angel F. Zazo, Departament of Computer Science and Automatics, University of Salamanca
#' @importFrom httr stop_for_status content
#' @importFrom jsonlite fromJSON
#' @export
Wikimedia_page_views <- function(article, project = "en.wikipedia.org",
                                  start, end, access = "all-access",
                                  agent = "user", granularity = "monthly",
                                  include_redirects = FALSE) {
  req_wikimedia_metrics_rated <- limit_requester(req_wikimedia_metrics, n=50, period=1) 
  article <- gsub(" ", "_", article)
  url <- "https://wikimedia.org/api/rest_v1/metrics/pageviews/per-article"
  url <- paste(url, project, access, agent, article, granularity, start, end, sep="/", collapse = "")
  a <- list()
  if (include_redirects == TRUE) {
    for (art in Wikimedia_get_redirects(article, project)) {
      b <- Wikimedia_page_views(art, project, start, end, access, agent,
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


# ------
#' Wikidata_wikipedias
#' For an occupation, obtains all Wikidata entity of the people with that ocupation, also the
#' number of wikipedias witch article they have, and the URL of those wikipedias (sep = tab)
#' Run queries splitting with offset and chunk.
#' @param Qoc The Wikidata entity of the occupation
#' @param chunk The chunk to split intermediate results with the aim of reduce the limit 60 seconds processig time.
#' @return A list with Wikidata entity, the number of wikipedias and the URL
#' @author Angel F. Zazo, Departament of Computer Science and Automatics, University of Salamanca
#' @importFrom utils tail
#' @export
Wikidata_Wikipedias <- function(Qoc, chunk=10000) {
  if (Qoc == '') return(NULL)
  nq <- Wikidata_occupationCount(Qoc)
  print(paste("Number of entities",as.character(nq), sep=" "))
  d = list()
  for (k in 0:as.integer(nq/chunk + 1)) {
    offset <- chunk * k
    print(paste("offset", as.character(offset), "chunk", as.character(k), sep=" "))
    query = paste0(
'SELECT ?qid (COUNT(?page) AS ?count) (GROUP_CONCAT(?page;separator="\t") as ?pages)
 WITH {
    SELECT ?qid
    WHERE {?qid wdt:P106 wd:' , Qoc, ' .}
    ORDER BY ?qid
    LIMIT ', chunk, ' OFFSET ', offset ,'
    } AS %results
 WHERE {
   INCLUDE %results.
   OPTIONAL {?page schema:about ?qid;
            schema:isPartOf [ wikibase:wikiGroup "wikipedia" ] .}
 } GROUP BY ?qid')
    j <- Wikidata_sparql_query(query)
    bindings <- j$results$bindings
    for (b in bindings) {
      q <- tail(strsplit(b$qid$value, '/')[[1]], 1)
      c <- b$count$value
      w <- b$pages$value
      w <- paste(sort(strsplit(w,"\t", fixed=T)[[1]]), collapse = "\t")
      d[q] <- list(q = c(q,c,w))
    }
  }
  return(d)
}

#' wmflabs_get_allinfo
#' Obtains information about an article in the Wikimedia project in JSON format, or NULL on error.
#' @param article The article to search
#' @param project The Wikimedia project, defaults en.wikipedia.org
#' @param links_in_count_redirects If infotype==links, if redirects are included or not
#' @return The number of visits
#' @author Angel F. Zazo, Departament of Computer Science and Automatics, University of Salamanca
#' @note Is important that the article be don't a redirection: with "prose" infotype the function gets information of the target article, but with "articleinfo" and "links" the information is about the redirection.
#' @importFrom httr stop_for_status content
#' @importFrom jsonlite fromJSON
#' @export
wmflabs_get_allinfo <- function(article, project = "en.wikipedia.org",
                                                    links_in_count_redirects = FALSE) {
  req_wikimedia_metrics_rated <- limit_requester(req_wikimedia_metrics, n=50, period=1)
  wmflabs_get_info <- function(article, infotype = "articleinfo", project = "en.wikipedia.org",
                               links_in_count_redirects = FALSE) {
    d <- list()
    if (infotype == "links" & links_in_count_redirects) {
      arts <- Wikimedia_get_redirects(article, project)
      for(art in arts) {
        b <- wmflabs_get_info(article = art, infotype = "links", project = project,
                              links_in_count_redirects = FALSE)
        if ("links_in_count" %in% names(d))
          d["links_in_count"] <-  d["links_in_count"][[1]] + b["links_in_count"][[1]]
        else
          d <- b
      }
    }
    else {
      url <- 'https://xtools.wmflabs.org/api/page/'
      url <- paste(url, infotype, project, article, sep="/", collapse = "")
      r <- req_wikimedia_metrics_rated(url)
      httr::stop_for_status(r)
      content <- httr::content(r, as = "text", encoding = "UTF-8")
      d <- jsonlite::fromJSON(content, simplifyVector = FALSE)
    }
    return(d)
  }
  # first: articleinfo
  r <- wmflabs_get_info(article, infotype = 'articleinfo', project = project)
  # Error in response
  if (!is.null(r$error)) {
    print("Error in response from wmflabs_get_info:")
    print(r$error)
    return(NULL)
  }
  # Second: prose
  b <- wmflabs_get_info(article, infotype = "prose", project = project)
  for (n in names(b))
    r[n] <- b[n]
  # Third: links
  c <- wmflabs_get_info(article, infotype = "links", project = project,
                        links_in_count_redirects = links_in_count_redirects)
  for (n in names(c))
    r[n] <- c[n]
    
  return(r)
}
