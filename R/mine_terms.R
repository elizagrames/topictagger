#' Extracts reasonable terms for topic modeling
#' @description Given documents, extracts possible ngram terms to use for topic modeling.
#' @param x a character vector of documents
#' @param retain_stopwords logical: if TRUE, retains stopwords used in known terms
#' @param known_phrases a character vector of stopwords to retain or phrases known to be relevant
#' @param ngrams logical: if TRUE, only returns phrases of two words or more
#' @example inst/examples/mine_terms_ex.R
#' @export
mine_terms <- function(x, retain_stopwords=FALSE, known_phrases=NULL, ngrams=TRUE, new_stopwords=c()){
  sw <- append(litsearchr::custom_stopwords, new_stopwords)
  
  # keep any stopwords supplied or in phrases
  # for example, if 'of' is actually important
  if(retain_stopwords){
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


