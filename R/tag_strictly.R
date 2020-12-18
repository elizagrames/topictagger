#' Applies an ontology to tag document metadata 
#' @description Given a hierarchical ontology or 
#' dictionary and a set of documents, applies the 
#' ontoloy to tag metadata hierarchically for each document.
#' @param doc character: a vector of document texts to tag
#' @param scheme a dictionary object or list containing an ontology or set of terms 
#' @param enable_stemming logical: if TRUE, interpret lemmatized stems of words as synonymous (e.g. "burning" and "burned" are equivalent to "burn")
#' @param allow_multiple logical: if TRUE, returns all matched metadata, else returns most frequent
#' @example inst/examples/tag_strictly_ex.R
#' @export
tag_strictly <- function(doc, scheme, allow_multiple=TRUE){
  # check the class of docs
  if(class(doc) != "character"){
    stop("doc must be an object of class character")
  }
  
  # check the class of scheme, reformat lists as needed
  if(!class(scheme) %in% c("dictionary", "dictionary2", "list")){
    stop("scheme must be an object of class dictionary or a list of terms")
  }

  if(class(scheme)=="list"){
    scheme <- quanteda::dictionary(scheme)
  }
  
  ## note to self: do a simple dfm first, then use dfm_lookup
  ## just don't forget about levels of the dictionary
  ## then we can also easily collapse levels
  
  dfm <- as.matrix(quanteda::dfm(doc, dictionary=scheme))
  if(!allow_multiple){
    tags <- colnames(dfm)[as.numeric(apply(dfm, 1, function(x){
      if(x[which.max(x)]>1){
        which.max(x)
      }else{NA}
    }))]
  }else{
      tags <- dfm
  }
  return(tags)
  
}


#' Checks length of a word and stems if long enough to be unambiguous
#' @description Given a word, checks the number of characters of the stem and truncates if the stem is at least four letters long.
#' @param word a character vector of length 1 with a word or phrase to check for stemming
#' @return a character vector of length 1 with the input truncated to stem
#' @examples should_stem("keep cats indoors")
#' @export
should_stem <- function(word){
  splitup <- strsplit(word, " ")[[1]]
  for(i in 1:length(splitup)){
    wordcut <- SnowballC::wordStem(splitup[i], language="en")
    stem_length <- nchar(wordcut)
    
    if(i==1){
      if(stem_length > 3){
        words <- paste(wordcut, "* ", sep="")
      }
      if(stem_length <= 3){
        words <- paste(splitup[i], " ", sep="")
      }
    }
    if(i > 1){
      if(stem_length > 3){
        words <- paste(words, wordcut, "* ", sep="")
      }
      if(stem_length <= 3){
        words <- paste(words, splitup[i], " ", sep="")
      }
    }
  }
  
  words <- trimws(words)
  
  return(words)
}
