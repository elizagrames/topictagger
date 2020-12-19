define_terms <- function(term, exclusion_terms){
  z <- lapply(term, function(t){
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
  names(z) <- term
  return(z)
}