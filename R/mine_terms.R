#' Extracts reasonable terms for topic modeling
#' @description Given documents, extracts possible ngram terms to use for topic modeling.
#' @param x a character vector of documents
#' @param known_phrases a character vector of stopwords to retain or phrases known to be relevant
#' @param ngrams logical: if TRUE, only returns phrases of two words or more
#' @param new_stopwords a character vector of additional, subject-specific terms to remove
#' @export
mine_terms <- function(x, known_phrases=NULL, ngrams=TRUE, new_stopwords=c()){
  publisher_terms <- c("study suggests", "results suggest", "multiple sites", "study area",
                       "results indicate", "significant differences", "negative effect", "study aimed",
                       "results demonstrate", "significantly lower", "findings suggest", "data suggest", "results indicated",
                       "positively correlated", "positively related", "significantly reduced", "significantly improved",
                       "study highlights", "significant difference", "previous studies", "plos one", "significantly  higher",
                       "results show",  
                       "rights reserved",
                       "john wiley", "bmc genomics", "annual review", "annual reviews inc",
                       "copyright holder", "copyright applies", "abstract may", "content may",
                       "original published version", "s express written permission",
                       "full abstract", "individual use", "email articles", 
                       "springer nature", "public library", "elsevier b", "elsevier science", "mdpi publishing", 
                       "biomed central", "international journal", "new march",
                       "royal society", "national academy", "linnean society",
                       "biological conservation", "plos neglected tropical diseases", 
                       "academic press inc", "oxford university press", "pergamon press -",
                       "abstracts", "download", "users may print", "listserv without", "present study")
  new_stopwords <- append(new_stopwords, publisher_terms)
  sw <- unique(append(stopwords::stopwords(), new_stopwords))
  
  # keep any stopwords supplied or in phrases
  # for example, if 'of' is actually important
  if(any(known_phrases)){
    retain <- sw %in% unlist(strsplit(known_phrases, " "))
    sw <- sw[!retain]
  }

  # remove most punctuation and numbers, preserve hyphens
  x <- gsub("[^-[:^punct:]]", "|", tolower(x), perl = T)
  x <- gsub("[[:digit:]]", "|", x)

  # split everything up
  z <- unlist(strsplit(x, " "))
  z[which(z %in% sw)] <- "|"
  z <- unlist(strsplit(paste(z, collapse="_"), "\\|"))
  z <- trimws(gsub("_", " ", z))
  z[which(z %in% sw)] <- "|"
  
  # remove junk
  if(any(z=="")){
    z <- z[-which(z=="")]
  }
  
  # only keep phrases?
  if(ngrams){
    z <- z[grep(" ", z)]
  }
  return(z)
}


