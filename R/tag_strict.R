#' Applies an ontology to tag document metadata 
#' @description Given a hierarchical ontology or 
#' dictionary and a set of documents, applies the 
#' ontoloy to tag metadata hierarchically for each document.
#' @param doc character: a vector of document texts to tag
#' @param scheme a dictionary object or list containing an ontology or set of terms 
#' @param enable_stemming logical: if TRUE, interpret lemmatized stems of words as synonymous (e.g. "burning" and "burned" are equivalent to "burn")
#' @param tag_type string: how should tags be classified? Options are "multiple" to return all tags, "first" for the first tag encountered, or "best_guess" for the most frequent tag used
tag_strict <- function(doc, scheme, enable_stemming=TRUE, tag_type="multiple"){
  # check the class of docs
  if(class(doc) != "character"){
    stop("doc must be an object of class character")
  }
  
  # check the class of scheme, reformat lists as needed
  if(!class(scheme) %in% c("dictionary", "dictionary2", "list")){
    stop("scheme must be an object of class dictionary or a list of terms")
  }
  
  if(enable_stemming){
    
  }
  
  
  if(class(scheme)=="list"){
    scheme <- quanteda::dictionary(scheme)
  }
  
  dfm <- quanteda::dfm(doc, dictionary=scheme)
  
}