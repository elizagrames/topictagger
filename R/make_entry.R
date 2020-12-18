make_entry <- function(parents, children, descendants=NULL, return_dictionary=FALSE){
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
    pairs <- append(classes[i], pairs)
    if(length(pairs)>0){
      pair_list <- lapply(pairs, topictagger::mine_terms)
      pair_list[pairs %in% names(descendants)] <- NA
      
      for(j in 1:length(pairs)){
        if(pairs[j] %in% names(descendants)){
          pair_list[j] <- descendants[which(names(descendants)==pairs[j])]
        }
      }
      names(pair_list) <- pairs
      
      entry <- pair_list
    }else{
      entry <- classes[i]
    }

    output[[i]] <-  entry
    
    }
    
    
  names(output) <- classes
  
  
  if(return_dictionary){
    output <- quanteda::dictionary(output)
  }
  return(output)
}
