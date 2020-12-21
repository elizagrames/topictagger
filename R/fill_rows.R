#' Fill in parents for nested entries
#' @description Given a human-readable nested list, matches children and descendants to parent entries and fills in gaps.
#' @param x a data.frame where columns represent, from left to right, nested levels of a hierarchical list
#' @return a data.frame of the same dimensions as x with parent entries filled in
#' @export 
fill_rows <- function(x){
  if(any(x=="")){
    x[x==""] <- NA
  }
    for(j in ncol(x):2){
    for(i in nrow(x):1){
      if(!is.na(x[i,j])){
        if(is.na(x[i,(j-1)])){
          parents <- x[1:i,(j-1)]
          parent <- tail(parents[!is.na(parents)],1)
          x[i,(j-1)] <- parent
        }
      }
      
    }
    }
  return(x)

}
