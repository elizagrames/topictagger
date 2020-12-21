#' Creates hierarchical dictionary entries
#' @description Given a list of parent and children entries, creates hierarchical named lists of lists that can be used as a dictionary object
#' @param parents a character vector of parent entries
#' @param children a character vector of length equal to parents containing child entries
#' @param descendants a named list of descendants of child entries (optional)
#' @return a list of lists or hierarchical dictionary object of parents, children, and descendants
#' @export
make_entry <- function(parents, children, descendants=NULL){
  adults <- unique(parents[!is.na(parents)])
  output <- list()
  
  for(i in 1:length(adults)){
    
    offspring <- unique(children[which(parents %in% adults[i])])
    offspring <- offspring[!is.na(offspring)]
    
    if(length(offspring)<1){
      output[i] <- define_terms(adults[i])
    }else{
      tmp <- list()
      for(j in 1:length(offspring)){
        if(!is.null(descendants[which(names(descendants)%in%offspring[j])])){
          tmp[[j]] <- descendants[which(names(descendants)%in%offspring[j])]
        }else{
          tmp[j] <- define_terms(offspring[j])
        }
      }
     # tmp[[j+1]] <- define_terms(adults[i])
      
      names(tmp) <- offspring
      
      output[[i]] <- append(unlist(define_terms(adults[i])), tmp)
      }
    }
  names(output) <- adults
  return(output) 
  }

#' Creates hierarchical dictionary entries
#' @description Given an object containing parent and child entries in columns from left to right, creates hierarchical named lists of lists that can be used as a dictionary object
#' @param x a data.frame where columns represent parent and child entries in a hierarchy from left to right
#' @param return_dictionary logical; if TRUE, returns a dictionary object; if FALSE, returns a list of lists
#' @return a list of lists or hierarchical dictionary object of parents, children, and descendants
#' @export
create_dictionary <- function(x, return_dictionary=TRUE){
  
  parent.cols <- seq(c(ncol(x)-1), 1, -1)
  child.cols <- seq(ncol(x), 2, -1)
  
  for(i in 1:length(parent.cols)){
    
    if(i==1){level <- make_entry(parents = x[,parent.cols[i]], 
                                 children = x[,child.cols[i]])
    }
    
    if(i >1){
      level <- make_entry(x[,parent.cols[i]], x[,child.cols[i]], level)
      
    }
  }
  output <- level
  if(return_dictionary){
    output <- quanteda::dictionary(output)
  }
  return(output)
}
