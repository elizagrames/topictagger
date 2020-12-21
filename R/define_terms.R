#' Define terms within a dictionary
#' @description Extracts phrases associated with a dictionary entry to define it while removing exclusion terms that are too common or irrelevant to be included in definitions
#' @param entry a character vector of entries in a dictionary
#' @param exclusion_terms a character vector of terms to not retain as possible parts of a term definition
#' @return a named list of the same length as entry containing character vectors defining each entry
#' @export
define_terms <- function(entry, exclusion_terms=NULL){
  z <- lapply(entry, function(t){
    tmp <- topictagger::mine_terms(t)
    
if(!is.null(exclusion_terms)){
  tmp <- tmp[!(tmp %in% exclusion_terms)]
  
}
    if(length(tmp)==0){
      tmp <- t
    }
    return(tmp)
  }
  )
  names(z) <- entry
  return(z)
}
