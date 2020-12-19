make_entry <- function(parents, children=NULL, descendants=NULL){
  
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
  return(output)
}
