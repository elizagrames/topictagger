#' Creates hierarchical dictionary entries
#' @description Given a list of parent and children entries, creates hierarchical named lists of lists that can be used as a dictionary object
#' @param parents a character vector of parent entries
#' @param children a character vector of length equal to parents containing child entries
#' @param descendants a named list of descendants of child entries (optional)
#' @param return_dictionary logical; if TRUE, returns a dictionary object; if FALSE, returns a list of lists
#' @return a list of lists or hierarchical dictionary object of parents, children, and descendants
#' @example inst/examples/create_dictionary_ex.R
#' @export
create_dictionary <- function(parents, children, descendants=NULL, return_dictionary=FALSE){
  adults <- unique(parents[!is.na(parents)])
  output <- list()
  
  for(i in 1:length(adults)){
    
    offspring <- children[which(parents %in% adults[i])]
    offspring <- offspring[!is.na(offspring)]
    
    if(length(offspring)<1){
      output[i] <- define(adults[i])
    }else{
      for(j in 1:length(offspring)){
        output[[i]] <- descendants[which(names(descendants)%in%offspring)]
      }
    }
    
  }
  names(output) <- adults

  if(return_dictionary){
    output <- quanteda::dictionary(output)
  }
  return(output)
}