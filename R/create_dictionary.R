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
  output <- list()
  classes <- unique(parents)
  if(any(is.na(classes))){
    classes <- classes[!is.na(classes)]
  }
  
  for(i in 1:length(classes)){
    pairs <- children[which(parents==classes[i])]
    if(any(is.na(pairs))){
      pairs <- pairs[!is.na(pairs)]
    }
    
    if(!is.null(descendants)){
      entry <- descendants[names(descendants)%in%pairs]
    }else{
      entry <- pairs
    }
    
    output[[i]] <- append(classes[i], entry)
    
  }
  names(output) <- classes
  if(return_dictionary){
    output <- quanteda::dictionary(output)
  }
  return(output)
}