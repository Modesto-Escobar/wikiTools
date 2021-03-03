#### Modesto Escobar
# Sat Feb 27 23:56:26 2021 ------------------------------

#' Create the Wikipedia URL of a name or entry.
#' @param name A vector consisting of one or more Wikipedia's entry (i.e., topic or person).
#' @param language The language of the Wikipedia page version. This should consist of an ISO language code (default = "en").
#' @return A character vector of names' URLs.
#' @details This function adds the Wikipedia URL to a entry or name, i.e., "Max Weber" converts into \url{"https://es.wikipedia.org/wiki/Max_Weber"}. It also manages different the languages of Wikipedia thru the abbreviated two-letter language parameter, i.e., "en" = "english".
#' @author Modesto Escobar, Department of Sociology and Communication, University of Salamanca. See <https://sociocav.usal.es/blog/modesto-escobar/>
#' @examples
#' ## When extracting a single item;
#' nametoWikiURL("Computer", language = "en")
#'
#' ## When extracting two objetcs;
#' A <- c("Computer", "Operating system")
#' nametoWikiURL(A)
#'
#' ## Same when three or more items;
#' B <- c("Socrates", "Plato" , "Aristotle")
#' nametoWikiURL(B)
#' @export
nametoWikiURL <- function (name, language="en") {
  paste0("https://", language, ".wikipedia.org/wiki/", gsub(" ","_",name))
}

#' Create the Wikipedia link of a name or entry.
#' @param name A vector consisting of one or more Wikipedia's entry (i.e., topic or person).
#' @param language The language of the Wikipedia page version. This should consist of an ISO language code (default = "en").
#' @return A character vector of names' links.
#' @details This function adds the Wikipedia's html link to a entry or name, i.e., "Max Weber" converts into "<a href='https://es.wikipedia.org/wiki/Max_Weber', target='_blank'>Max Weber</a>". It also manages different the languages of Wikipedia thru the abbreviated two-letter language parameter, i.e., "en" = "english".
#' @author Modesto Escobar, Department of Sociology and Communication, University of Salamanca. See <https://sociocav.usal.es/blog/modesto-escobar/>
#' @examples
#' ## When extracting a single item;
#' nametoWikiText("Computer", language = "en")
#' 
#' ## When extracting two objetcs;
#' A <- c("Computer", "Operating system")
#' nametoWikiText(A)

## Same when three or more items;
#' B <- c("Socrates", "Plato","Aristotle" )
#' nametoWikiText(B)
#' @export
nametoWikiText <- function(name, language="en"){
  paste0("<a href=\'https://", language, ".wikipedia.org/wiki/", gsub(" ","_",name), "', target=\'_blank\'>", name, "</a>")
}

#' Convert names into a Wikipedia's iframe
#' @param name A vector consisting of one or more Wikipedia's entry (i.e., topic or person).
#' @param language The language of the Wikipedia page version. This should consist of an ISO language code (default = "en").
#' @details This function adds the Wikipedia's iframe to a entry or name, i.e., "Max Weber" converts into "<iframe src=\"https://es.m.wikipedia.org/wiki/Max_Weber\" width=\"100...". It also manages different the languages of Wikipedia through the abbreviated two-letter language parameter, i.e., "en" = "english".
#' @return A character vector of Wikipedia's iframes.
#' @author Modesto Escobar, Department of Sociology and Communication, University of Salamanca. See <https://sociocav.usal.es/blog/modesto-escobar/>
#' @examples
#' ## When extracting a single item;
#' nametoWiki("Computer", language = "en")
#' 
#' ## When extracting two objetcs;
#' A <- c("Computer", "Operating system")
#' nametoWiki(A)
#' 
#' ## Same when three or more items;
#' B <- c("Socrates", "Plato", "Aristotle")
#' nametoWiki(B)
#' @export
nametoWiki <- function(name, language="en") {
  paste0('<iframe src="https://',language,'.m.wikipedia.org/wiki/',gsub(" ","_",name),'" width="100%" height="100%" frameborder="0" marginwidth="0", margingheight="0"></iframe>')
}


urltoText <- function(url, text=NULL) {
  if (is.null(text)) text <- url
  paste0("<a href=\'",url, "\', target= \'_blank\'>", text, "</a>")
}


urltoInfo <- function(url){
  paste0('<iframe src="',url, '" width="100%" height="100%" frameborder="0" marginwidth="0", margingheight="0"></iframe>')
}



preName <- function(X) sub("(^.*),\\s*(.*$)","\\2 \\1", X)


errorWiki <- function(X, language=c("es", "en", "fr"), directory="./", maxtime=0) {
  errores <- NULL
  for (I in X){
    person <- gsub(" ", "_", I)
    url <-paste("https://",language,".wikipedia.org/wiki/",person,sep="")
    file <- paste0(directory, person,".html")
    oldw <- getOption("warn")
    options(warn = -1)
    E <- tryCatch(download.file(url,destfile=file, quiet=TRUE),error = function(e) person)
    if (E!=0) errores <- c(errores, E)
    options(warn = oldw)
    Sys.sleep(runif(1, min=0, max=maxtime))
  }
  return(errores)
}



searchWiki <- function(name, language=c("en", "es", "fr", "it", "de", "pt", "ca"), all=FALSE, maxtime=0) {
  errores <- data.frame(es=logical(), en=logical(), fr=logical(), it=logical(), 
                        de=logical(), pt=logical(), ca=logical())[,language, drop=FALSE]
  for (I in name){
    errores[I,language] <- rep(FALSE, length(language))
    for (L in language){
      person <- gsub(" ", "_", I)
      url <-URLencode(paste("https://",L,".wikipedia.org/wiki/",person,sep=""))
      if (valid_url(url)) {
        errores[I,L] <- TRUE
        if (!all) break
      }
      Sys.sleep(runif(1, min=0, max=maxtime))
    }
  }
  return(errores)
}

valid_url <- function(url_in,t=2){
  con <- url(url_in)
  check <- suppressWarnings(try(open.connection(con,open="rt",timeout=t),silent=T)[1])
  suppressWarnings(try(close.connection(con),silent=T))
  ifelse(is.null(check),TRUE,FALSE)
}


readfile <- function(file, encoding="UTF-8") {
  note <- readChar(file, 1e+9)
  iconv(note, from=encoding)
}

cc <- function(wordlist) {
  wordlist <- gsub("[ ]*,[ ]*",",",wordlist)
  strsplit(wordlist,',')[[1]]
}


#' Create a data.frame with Q's and descriptions of a vector of names.
#' @param names A vector consisting of one or more Wikidata's entry (i.e., topic or person).
#' @param number Take the number occurrence in case there are several equal names in Wikidata.
#' @param language The language of the Wikipedia page version. This should consist of an ISO language code (default = "en").
#' @return A data frame with name, Q, label and description of the names.
#' @examples
#' ## Obtaining information in English Wikidata
#' names <- c("Douglas Adams", "Picasso")
#' information <- getWikiQ(names)
#'
#' ## Obtaining information in Spanish Wikidata
#' informacion <- getWikiQ(names)
#' @export
#' @importFrom WikidataR find_item
getWikiQ <- function(names, number=1, language="en"){
  get <-function(name, number=1, language="en"){
    i <- find_item(name, language=language)
    if(length(i)>=number) {
      X <- c(name=name, Q=i[[number]]$id, 
             label=ifelse(is.null(i[[number]]$label),NaN, i[[number]]$label),
             description=ifelse(is.null(i[[number]]$description),NaN,i[[number]]$description))
    }
    else X <- c(name=name, Q=NaN, label=NaN, description=NaN)
    return(X)
  }
  D <- as.data.frame(t(sapply(names, get, number, language)))
  return(D)
}

getWikidata <- function(namesVector) {
preCode <- 'SELECT ?label ?sexLabel ?birthdate ?birthplaceLabel ?deathdate ?deathplaceLabel ?citizenshipLabel
(GROUP_CONCAT(DISTINCT ?pic;separator="|")     as ?pics)
(GROUP_CONCAT(DISTINCT ?ocLabel;separator="|") as ?occupation)
(GROUP_CONCAT(DISTINCT ?moLabel;separator="|") as ?movement)
(GROUP_CONCAT(DISTINCT ?geLabel;separator="|") as ?genres)
(GROUP_CONCAT(DISTINCT ?inLabel;separator="|") as ?influencedby)
(GROUP_CONCAT(DISTINCT ?in;separator="|")      as ?influencedbyQ)  # AS Qxxxx
(GROUP_CONCAT(DISTINCT ?noLabel;separator="|") as ?notablework)
(GROUP_CONCAT(DISTINCT ?no;separator="|")      as ?notableworkQ)   # As Qxxxx
WHERE {
  BIND(wd:'
  
  postCode <- ' AS ?entity)
    ?entity rdfs:label ?label
    SERVICE wikibase:label {bd:serviceParam wikibase:language "en"}
    FILTER(LANG(?label) = "en")
    OPTIONAL {?entity wdt:P21  ?sex}
    OPTIONAL {?entity wdt:P569 ?birthdate}
    OPTIONAL {?entity wdt:P19  ?birthplace}
    OPTIONAL {?entity wdt:P570 ?deathdate}
    OPTIONAL {?entity wdt:P20  ?deathplace}
    OPTIONAL {?entity wdt:P27  ?citizenship}
    OPTIONAl {?entity wdt:P18  ?pic.} 
    OPTIONAL {?entity wdt:P106 ?oc.
              ?oc rdfs:label ?ocLabel.
              FILTER((LANG(?ocLabel)) = "en")}
    OPTIONAL {?entity wdt:P135 ?mo.
              ?mo rdfs:label ?moLabel.
              FILTER((LANG(?moLabel)) = "en")}
    OPTIONAL {?entity wdt:P136 ?ge.
              ?ge rdfs:label ?geLabel.
              FILTER((LANG(?moLabel)) = "en")}
    OPTIONAL {?entity wdt:P737 ?in.
              ?in rdfs:label ?inLabel.
              FILTER((LANG(?inLabel)) = "en")}
    OPTIONAL {?entity wdt:P800 ?no.
              ?no rdfs:label ?noLabel.
              FILTER((LANG(?noLabel)) = "en")}
}
GROUP BY ?label ?sexLabel ?birthdate ?birthplaceLabel ?deathdate ?deathplaceLabel ?citizenshipLabel
'

getWiki <-function(nombre, pre=preCode, post=postCode){
  i <- find_item(nombre)
  if(length(i)>0) {
    i <- i[[1]]$id
    x <- paste0(pre, i, post)
    X <- suppressMessages(query_wikidata(x)[1,]) 
  }
  else X <- data.frame(label=nombre, sexLabel=NA, birthdate=NA, birthplaceLabel=NA, 
                         deathdate=NA,deathplaceLabel=NA,citizenshipLabel=NA,
                         pics=NA, occupation=NA,movement=NA,genres=NA, 
                         influenceddby=NA, influencebyQ=NA,notablework=NA, notableworkQ=NA,
                       stringsAsFactors = FALSE)
  return(X)
}


transM <- function(X) {
  dimensions <- dim(X)
  x <- unlist(X)
  m <- as.data.frame(matrix(x, nrow=dimensions[2], ncol=dimensions[1], byrow=TRUE), stringsAsFactors=FALSE)
  colnames(m) <- rownames(X)
  return(m)
}

X <- sapply(namesVector,getWiki)
return(transM(X)) 
}

getFiles <- function(lista, path="./", ext=NULL) {
  errores <- NULL
  path <- ifelse(substr(path,nchar(path),nchar(path))!="/",paste0(path,"/"),path)
  lista <- as.data.frame(lista)
  for (case in 1:nrow(lista)) {
    name <- lista[case,1]; url <- lista[case,2]
    if(is.null(ext)) ext <- filext(url) 
    file=paste0(path,sub("/","-",name),".",ext)
    if(!is.na(url) & !file.exists(file)) {
       oldw <- getOption("warn")
       options(warn = -1)
       E <- tryCatch(download.file(url, destfile=file, quiet=TRUE, mode="wb"),error = function(e) name)
       if (E!=0) errores <- c(errores, E)
       options(warn = oldw)
    } 
  }
    return(errores)
}


filext <- function (fn) {
  splitted    <- strsplit(x=fn, split='/')[[1]]   
  fn          <- splitted [length(splitted)]
  ext         <- ''
  splitted    <- strsplit(x=fn, split='\\.')[[1]]
  l           <-length (splitted)
  if (l > 1 && sum(splitted[1:(l-1)] != ''))  ext <-splitted [l] 
  ext
}



