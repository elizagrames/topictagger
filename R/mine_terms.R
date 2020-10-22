#' Extracts reasonable terms for topic modeling
#' @description Given documents, extracts possible ngram terms to use for topic modeling.
#' @param x a character vector of documents
#' @param retain_stopwords logical: if TRUE, retains stopwords used in known terms
#' @param known_phrases a character vector of stopwords to retain or phrases known to be relevant
#' @examples inst/examples/mine_terms.R
#' @export
mine_terms <- function(x, retain_stopwords=FALSE, known_phrases=NULL){
  sw <- litsearchr::custom_stopwords
  if(retain_stopwords){
    retain <- sw %in% unlist(strsplit(known_phrases, " "))
    sw <- sw[!retain]
  }
  output <- lapply(x, function(x){
    tmp <- litsearchr::extract_terms(x, method = "fakerake", min_freq = 1, min_n = 2, 
    ngrams = TRUE, 
    stopwords = sw, min_char = 1)
  tmp <- gsub(" ", "_", tmp)
  tmp <- paste(tmp, collapse = " ")
  } )
  return(output)
}

