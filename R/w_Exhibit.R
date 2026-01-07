# w_Exhibit
# Modesto Escobar
# Sat Dec 6 17:22:56 2025 ------------------------------
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
#' @param links Vector of IDs for linking to its catalog. V.gr. c("Wikidata", "Wikipedia", "BNE", "RAH"). 
#' These four are those by default. Others possibles are c("BVMC", "VIAF", "LOC","ISNI", "ULAN", "OPENL", 
#' "MNCARS", "MNP", "SUDOC", "BRITISH", "BRITANNICA", "OXFORD", "WORLDCAT", "WEBCAT", "WEBGALL", "WIKIART")
#' @param map A map should be created? This parameter can be "none", "only", "first" or "last".
#' @param info Add the first paragraph of Wikipedia in the template.
#' @param imgpath Name of the directory where there are image files.
#' @param nlimit If the number of entities exceeds this number, chunked queries
#' are done. This is the number of entities requested in each chunk. Please,
#' reduce the default value if error is raised.
#' @param debug For debugging (info or query).
#' @param ... Arguments for netCoin::exhibit(), evolMap::create_map() and evolMap::add_markers().
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
                      links=c("wikidata", "wiki", "BNE", "RAH", "ISNI"), 
                      map=c("none", "first", "last", "only"), info=FALSE, 
                      imgpath=NULL, nlimit = MW_LIMIT, debug=FALSE, ...) {
  # Control
  if(is.data.frame(entities) && "Q" %in% names(entities)) Qs <- entities[["Q"]] else
    if(is.data.frame(entities) && grepl("Q\\d*", entities[[1]][1])) Qs <- entities[[1]] else
      if(is.data.frame(entities)) stop("Frame entities has to have a Qs' field") else
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

  nolinks <- function(frame, links=intersect(names(frame), links), lang="en") {
    text <- "No"
    if(lang=="es"){
      text <- "Sin"
    }
    frame[["nolinks"]]=""
    for(x in links) {
      frame[["nolinks"]] <- ifelse(is.na(frame[[x]]), paste0(frame[["nolinks"]], "|",text," ", x), frame[["nolinks"]])
    }
    return(sub("^\\|", "", frame[["nolinks"]]))
  }
  
  yeslinks <- function(frame, links=intersect(names(frame), links)) {
    frame[["yeslinks"]]=""
    for(x in links) {
      frame[["yeslinks"]] <- ifelse(is.na(frame[[x]]), frame[["yeslinks"]], paste0(frame[["yeslinks"]], "|", x))
    }
    return(sub("^\\|", "", frame[["yeslinks"]]))
  }
  
  checkWikis <- function(cadena_actual, cadena_referencia) {
    cadena_actual <- gsub("https://([a-z-]+)\\.wikipedia\\.org/wiki/[^|]+", "\\1", cadena_actual)
    v_ref <- unlist(strsplit(cadena_referencia, "|", fixed = TRUE))
    procesar_fila <- function(x) {
      v_act <- unlist(strsplit(x, "|", fixed = TRUE))
      res <- ifelse(v_ref %in% v_act, v_ref, paste0("~", v_ref))
      return(paste(res, collapse = "|"))
    }
    sapply(cadena_actual, procesar_fila, USE.NAMES = FALSE)
  }

  # Preparation
  L <- w_EntityInfo(Qs, mode=mode, langsorder=langsorder, wikilangs=wikilangs, nlimit=nlimit, debug=debug)
  coords <- data.frame(bLong=L$bplaceLon, bLati=L$bplaceLat, dLong=L$dplaceLon, dLati=L$dplaceLat)
  L$pic <- gsub("\\|.*","", L$pic)
  lang <- regmatches(langsorder, regexpr("(?<=^|\\|)(es|en)(?=\\||$)", langsorder, perl = TRUE))
  if(nchar(lang)==0) lang="en"

  ### Image loading
  if(!is.null(imgpath)) {
    getFiles(L[!is.na(L$pic), c("entity", "pic")], path=imgpath, ext="jpg")
  }

  L$description <- toupper1(L$description)
  L$occupation   <- toupper2(L$occupation)
# Inserta ----
  L$wiki <- sub("\\|.*","", L$wikipedias)
  fieldW=""
  if (wikilangs!=langsorder) {
    L$Wikipedias <- checkWikis(L$wikipedias, wikilangs)
    fieldW="Wikipedias"
  }
  L$wikidata <- L[[1]]

  ## Data transformations ----
  linksid1 <- c("BNE","BVMC", "RAH","VIAF","LOC","ISNI", "ULAN", "OPENL", "MNCARS", "MNP", "SUDOC", 
                "BRITISH", "BRITANNICA", "OXFORD", "WORLDCAT", "WEBGALL", "WIKIART")
  linksid2 <- c("bneid","bvmcid", "histhispid","viafid","locid","isnid", "ulanid", "openlid", "mncarsid", "mnpid", "sudocid",
                "bmid", "britannid", "oxfid", "worldcatid", "webgallid", "wikiartid")
  for(i in seq_along(linksid1)){
    names(L)[names(L)==linksid2[i]] <- linksid1[i]
  }
  L$Links     <- yeslinks(L, links)
  L$Missing   <- nolinks(L, links, lang)
  L$pic       <- ifelse(is.na(L$pic), paste0(imgpath, "/Q0.png"), paste0(imgpath,"/", L[["entity"]],".jpg"))
  L$wikidata <- ifelse(is.na(L[['entity']]), NA, paste0())
  
  fields <- c("label", "description", "Links", "Missing", "sex",
             "byear", "bplace", "bcountry",
             "dyear", "dplace", "dcountry",
             "occupation", "genre",
             "BNE", "BVMC", "RAH", "VIAF", "LOC", "SUDOC", "ISNI", "ULAN", "OPENL",
             "MNCARS", "MNP", "BRITISH", "BRITANNICA", "OXFORD", "WORLDCAT", "WEBGALL", "WIKIART",
             "pic", "Wikipedias")
  L <- L[, intersect(names(L),c(fields, "entity"))]
  links <- union(links, c("wiki", "wikidata"))
  
  D <- pop_up(L, title = "label", entity="entity", wikilangs=wikilangs, links=links, info=info)

  fields <- c(fields[1:12], fieldW, "pic", "pop_up")
  
  Dnames <- names(D)
  D <- D[,fields]
  if(is.data.frame(entities)){
    D <- cbind(D, entities[,setdiff(names(entities), Dnames)])
  }

  P <- selectLang(D, language=lang)
  Name <- "Name"
  Map <- "Map"
  Gallery <- "Gallery"
  if(lang=="es") {
    Name <- "Nombre"
    Map <- "Mapa"
    Gallery <- "Galer\u00EDa"
    names(P)[names(P)=="Missing"] <- "Carencias"
    names(P)[names(P)=="Links"] <- "Enlaces"
  }
# Exit ----

  args <- list(...)
  exhibitArgsNames <- intersect(names(formals(netCoin::exhibit)),names(args))

  exhibitArgs <- args[exhibitArgsNames]
  exhibitArgs[['nodes']] <- P
  exhibitArgs[['name']] <- Name
  exhibitArgs[['ntext']] <- "pop_up"
  exhibitArgs[['image']] <- "img"
  exhibitArgs[['language']] <- lang
  exhibitArgs[['zoom']] <- NULL
  E <- do.call(netCoin::exhibit,exhibitArgs)
  E$coords <- coords
  if(map[1]!="none") {
    mapArgsNames <- intersect(names(formals(evolMap::create_map)),names(args))
    mapArgs <- args[mapArgsNames]
    mapArgs[['mode']] <- 2
    mapArgs[['language']] <- lang
    M <- do.call(evolMap::create_map,mapArgs)

    markersArgsNames <- intersect(names(formals(evolMap::add_markers)),names(args))
    markersArgs <- args[markersArgsNames]
    markersArgs[['map']] <- M
    markersArgs[['data']] <- cbind(E$nodes, E$coords[,1:2])
    markersArgs[['latitude']] <- "bLati"
    markersArgs[['longitude']] <- "bLong"
    markersArgs[['name']] <- Name
    markersArgs[['info']] <- "pop_up"
    markersArgs[['image']] <- "img"
    M <- do.call(evolMap::add_markers,markersArgs)
    if(map[1]=="only") {
      E<-M
    }else{
      multiArgs <- list()
      multiArgs[[Gallery]] <- E
      multiArgs[[Map]] <- M
      if(map[1]=="first"){
        multiArgs <- multiArgs[c(2,1)]
      }
      E <- do.call(netCoin::multigraphCreate,multiArgs)
    }
  }
  return(E)
}

# pop_up
#' Create a drop-down vignette for nodes from different items (for galleries).
#' @param data Data frame which contains the data.
#' @param title Column name which contains the first title of the vignette.
#' @param title2 Column name which contains the secondary title of the vignette.
#' @param info Extract the first paragraph of a Wikipedia article.
#' @param entity Column name which contains a vector of Wikidata entities.
#' @param links Column names which contains the URLs for the vignette.
#' 'wikidata' and 'wiki' by default, if this columns are missing,
#' they will be generated through 'entity' argument.
#' @param wikilangs List of languages to limit the search, using "|" as
#' separator. Wikipedias page titles are returned in same order as languages in
#' this parameter. If wikilangs='' the function returns Wikipedia page titles
#' in any language, not sorted.
#' @examples
#' \dontrun{
#' library(netCoin)
#' data("sociologists")
#' sociologists$entity <- sub(".png","",sociologists$picture)
#' sociologists <- pop_up(sociologists, title="name",
#'   title2="birth_country", entity="entity")
#' plot(exhibit(sociologists, label="name", ntext="pop_up"))
#' }
#' @return a character vector of html formatted vignettes attached to 'data' in a column named 'pop_up'. 
#' @author Modesto Escobar, Department of Sociology and Communication, University of Salamanca. See <https://sociocav.usal.es/blog/modesto-escobar/>
#' @export
pop_up <- function(data, title="name", title2=NULL, info=TRUE, entity="entity", links=c("wikidata", "wiki"), 
                   wikilangs="en") {
  data <- as.data.frame(data)
  sites <- data.frame(
    url=c("wikipedia.org","wikidata.org","brumario.usal.es", "museodelprado.es", "museoreinasofia.es", "viaf.org", "bne.es", "historia-hispanica.rah.es", "id.loc.gov", "isni.org", "vocab.getty.edu",
          "cervantesvirtual.com", "openlibrary.org", "idref.fr", "britishmuseum.org", "britannica.com", "oxfordreference.com",
          "oclc.org", "wga.hu", "wikiart.org"),
    name=c("Wikipedia","Wikidata","USAL", "MNP", "MNCARS","VIAF", "BNE", "RAH", "LOC", "ISNI", "ULAN",
           "BVMC", "OPENL", "SUDOC", "BRITISH", "BRITANNICA", "OXFORD", "WORLDCAT", "WEBGALL", "WIKIART"),
    icon=c("https://www.wikipedia.org/static/favicon/wikipedia.ico",
           "https://www.wikidata.org/static/favicon/wikidata.ico",
           "https://sociocav.usal.es/me/pics/LogoBUSAL.png",
           "https://sociocav.usal.es/me/pics/MP.png",
           "https://sociocav.usal.es/me/pics/ReinaSofia.png",
           "https://sociocav.usal.es/me/pics/VIAF.png", 
           "https://sociocav.usal.es/me/pics/BNE.png",
           "https://sociocav.usal.es/me/pics/RAH.png",           
           "https://sociocav.usal.es/me/pics/LOC.png",           
           "https://sociocav.usal.es/me/pics/ISNI.png",
           "https://sociocav.usal.es/me/pics/ULAN.png",
           "https://sociocav.usal.es/me/pics/BVMC.png",
           "https://sociocav.usal.es/me/pics/OpenL.png",
           "https://sociocav.usal.es/me/pics/SUDOC.png",
           "https://sociocav.usal.es/me/pics/BM.png",
           "https://sociocav.usal.es/me/pics/Britannica.png",
           "https://sociocav.usal.es/me/pics/Oxford.png",
           "https://sociocav.usal.es/me/pics/WorldCat.png",
           "https://sociocav.usal.es/me/pics/WebGallery.png",
           "https://sociocav.usal.es/me/pics/WikiArt.png"),
    target=c("mainframe","mainframe","mainframe","mainframe", "mainframe","_blank","mainframe","mainframe","_blank","_blank", "_blank",
             "_blank","mainframe","mainframe", "_blank","mainframe", "_blank", "mainframe", "_blank", "mainframe")
  )

  formatterurls <- c("",
  "https://m.wikidata.org/wiki/$1",
  "",
  "https://www.museodelprado.es/coleccion/artista/wd/$1",
  "https://www.museoreinasofia.es/coleccion/autor/$1",
  "https://viaf.org/viaf/$1",
  "https://datos.bne.es/resource/$1",
  "https://historia-hispanica.rah.es/$1",
  "https://id.loc.gov/authorities/$1",
  "https://isni.org/isni/$1",
  "https://vocab.getty.edu/page/ulan/$1",
  "https://data.cervantesvirtual.com/person/$1",
  "https://entities.oclc.org/worldcat/entity/$1",
  "https://www.idref.fr/$1",
  "https://www.britishmuseum.org/collection/term/BIOG$1",
  "https://www.britannica.com/$1",
  "https://www.oxfordreference.com/display/10.1093/oi/authority.$1",
  "https://entities.oclc.org/worldcat/entity/$1",
  "https://www.wga.hu/frames-e.html?/html/$1",
  "https://www.wikiart.org/es/$1"
  )
  names(formatterurls) <- sites[,"name"]

  langs <- unlist(strsplit(wikilangs, "\\|"))
  for(e in links) {
    if(e=="wikidata"){
      if(!is.element(e, names(data))) {
        data$wikidata <- ifelse(substr(data[[entity]], 1, 1)!="Q", NA, paste0("https://m.wikidata.org/wiki/", data[[entity]]))
      }
    }else if(e=="wiki"){
      if(!is.element(e, names(data))){
        wikis <- w_Wikipedias(data[[entity]], wikilangs=wikilangs)[,c(1,6)]
        wikis$wiki <- sub("\\.wikipedia",".m.wikipedia", sub("\\|.*","", wikis$pages))
        if (inherits(info,"character")) {
          # wikis <- merge(wikis, data[,c("entity", info)], by="entity")
        } else {
          if (info) {
            names <- ifelse(is.na(wikis$wiki) | wikis$wiki=="", " ", sub(".*/","", wikis$wiki))
            wikis$info <- sub("character\\(0\\)", "", as.character(extractWiki(names,language=langs)))
          } else{
            wikis$info <- ""
          }
        }
        data <- merge(data, wikis, by.x=entity, by.y="entity", all.x=TRUE, sort=FALSE)
        data$wiki <- ifelse(is.na(data$wiki) | data$wiki=="", NA, data$wiki)
        data$pages <- wikis <- names <- NULL
      }
    }else{
      if(e %in% sites[,"name"] && formatterurls[e]!=""){
        data[[e]] <- sapply(data[[e]],function(x){
          x <- sub("\\|.*", "", x)
          if(is.na(x) || x==""){
            return(NA)
          }else{
            return(sub("$1",x,formatterurls[e],fixed=TRUE))
          }
        })
      }
    }
  }

  linksname <- "LINKS"
  if(langs[1]=="es"){
    linksname <- "ENLACES"
  }else if(langs[1]=="ca"){
    linksname <- "ENLLA\uC7OS"
  }

  linksList <- netCoin::renderLinks(data, links, NULL, "mainframe", sites=sites)
  data$links <- ifelse(is.na(data$wiki) & is.na(data$wikidata), data[['info']],
                       paste0(data[['info']], '</p><h3 style="margin-top:8px">',linksname,':</h3>', linksList))
  data$pop_up <- netCoin::get_template2(data, title=title, title2=title2, text="links")
  data[, union(c("links", "linksList", "info", "names", "wiki", "wikidata"), links)] <- NULL
  return(data)
}
