# w_Exhibit
# Modesto Escobar
# Sun Apr 13 17:22:56 2025 ------------------------------
# w_Exhibit ----
#' Extract the first paragraph of a Wikipedia article with a maximum of characters.
#' @param entities A vector or data.frame of entities, whose entries have to be extracted.
#' @param mode type of data to be extracted, default=people.
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
#' @param links Vector of IDs for linking to its catalog. V.gr. c("Wikidata", "Wikipedia", "BNE", "RAH)
#' @param info Add the first paragraph of Wikipedia in the template.
#' @param imgpath Name of the directory where there are image files.
#' @param nlimit If the number of entities exceeds this number, chunked queries
#' are done. This is the number of entities requested in each chunk. Please,
#' reduce the default value if error is raised.
#' @param debug For debugging (info or query).
#' @param ... Same arguments as in netCoin::exhibit().
#' @examples
#' \dontrun{
#' ## Obtaining information in English Wikidata
#' names <- c("William Shakespeare", "Pedro Almodovar")
#' info <- getWikiInf(names)
#' w_Exhibit(info$Q)
#' }
#' @return An object of gallery_rd3 class. 
#' @author Modesto Escobar, Department of Sociology and Communication, University of Salamanca. See <https://sociocav.usal.es/blog/modesto-escobar/>
#' @export
w_Exhibit <- function(entities, mode="default", langsorder ="en", wikilangs = langsorder,
                      links=c("wikidata", "wiki", "BNE", "RAH"), info=FALSE,
                      imgpath=NULL, nlimit = MW_LIMIT, debug=FALSE, ...) {
  # Control
  if(is.data.frame(entities) && "Q" %in% names(entities)) Qs <- entities[["Q"]] else
    if(is.data.frame(entities) && grepl("Q\\d*", entities[[1]][1])) Qs <- entities[[1]] else
      if(is.data.frame(entities)) return("Frame entities has to have a Qs' field") else
        if(is.vector(entities) && grepl("Q\\d", entities[1])) Qs <- entities else
          stop("Argument entities must contain a vector of Qs")
  
  # Functions
  toupper1 <-function(text) {
    return(ifelse(is.na(text),  "", paste0(toupper(substr(text, 1, 1)),substr(text, 2, nchar(text)))))
  }
  
  toupper2 <- function(text) {
    sapply(text, function(x) {
      if (is.na(x)) return(NA_character_)
      
      partes <- strsplit(x, "\\|")[[1]]
      partes_cap <- sapply(partes, function(p) {
        if (nchar(p) == 0) return("")
        paste0(toupper(substr(p, 1, 1)), substr(p, 2, nchar(p)))
      })
      paste(partes_cap, collapse = "|")
    }, USE.NAMES = FALSE)
  }
  
  
  nolinks <- function(frame, links=intersect(names(frame), links)) {
    frame[["nolinks"]]=""
    for(x in links) {
      frame[["nolinks"]] <- ifelse(is.na(frame[[x]]), paste0(frame[["nolinks"]], "|Sin ", x), frame[["nolinks"]])
    }
    return(sub("^\\|", "", frame[["nolinks"]]))
  }
  
  # Preparation
  L <- w_EntityInfo(Qs, langsorder=langsorder, wikilangs=wikilangs)
  L$pic <- gsub("\\|.*","", L$pic)
  lang <- regmatches(langsorder, regexpr("(?<=^|\\|)(es|en)(?=\\||$)", langsorder, perl = TRUE))
  if(nchar(lang)==0) lang="en"
  
  ### Image loading
  if(!is.null(imgpath)) {
    getFiles(L[!is.na(L$pic), c("entity", "pic")], path=imgpath, ext="jpg")
  }
  
  L$description <- toupper1(L$description)
  L$occupation   <- toupper2(L$occupation)

  
  P <- selectLang(L, language=lang)
  P$wiki <- sub("\\|.*","", P$wikis)
  P$wikidata <- P[[1]]
  
## Data transformations ----
  if(lang=="es") {
    Name <- "Nombre"
    Field <- "Entidad"
    fields=c("Entidad", "Nombre", "Descripci\u00f3n", "Carencias", "G\u00e9nero",
                          "A\u00f1o nacimiento", "Lugar nacimiento", "Pa\u00eds nacimiento",
                          "A\u00f1o defunci\u00f3n", "Lugar defunci\u00f3n", "Pa\u00eds defunci\u00f3n",
                          "Ocupaci\u00f3n", "G\u00e9neros", "BNE", "RAH", "img", "wikis")
    P$Carencias   <- nolinks(P, links)
    P$img       <- ifelse(is.na(P$img), paste0(imgpath, "/Q0.png"), paste0(imgpath,"/", P[[Field]],".jpg"))
  }
  if(lang=="en") {
    Name <- "Name"
    Field <- "Entity"
    fields=c("Entity", "Name", "Description", "Missing", "Sex",
             "Birth year", "Birth place", "Birth country",
             "Death year", "Death place", "Death country",
             "Occupation", "Genre", "BNE", "RAH", "img", "wikis")
    P$Missing   <- nolinks(P, links)
    P$img       <- ifelse(is.na(P$img), paste0(imgpath, "/Q0.png"), paste0(imgpath,"/", P[[Field]],".jpg"))
  }
  P$wikidata <- ifelse(is.na(P[[Field]]), NA, paste0())
    P <- P[, fields]
 
  D <- netCoin::pop_up(P, title = Name, entity=Field, wikilangs=wikilangs, links=links, info=info)
  
  if(lang=="es") fields <- c("Nombre", "Entidad", "Descripci\u00f3n", "Carencias", "G\u00e9nero",
                             "A\u00f1o nacimiento", "Lugar nacimiento", "Pa\u00eds nacimiento",
                             "A\u00f1o defunci\u00f3n", "Lugar defunci\u00f3n", "Pa\u00eds defunci\u00f3n",
                             "Ocupaci\u00f3n", "G\u00e9neros", "img", "pop_up") else {
                 fields <- c("Name", "Entity", "Description", "Missing", "Sex",
                             "Birth year", "Birth place", "Birth country",
                             "Death year", "Death place", "Death country",
                             "Occupation", "Genre", "img", "pop_up")
                             }
  
  if(is.data.frame(entities)) D <- cbind(D[, fields], entities[,setdiff(names(entities), names(D))]) else D <- D[,fields]
  
  E <- netCoin::exhibit(D, name=Name, ntext="pop_up", image="img", language = lang, ...)
  
  return(E)
}