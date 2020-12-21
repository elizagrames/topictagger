#' Extract tags at specified levels of an ontology
#' @description Given the results of tagging documents with a hierarchical ontology, exports the tags from user-specified levels of the original ontology.
#' @param x a character vector containing hierarchical tags separated by a period
#' @param n.levels numeric: the indices of levels that should be extracted
#' @return a list of length equal to the length of n.levels where the length of each entry is the length of x
#' @example inst/examples/extract_levels.r
#' @export
extract_levels <- function(x, n.levels = NULL) {
  if (is.null(n.levels)) {
    n.levels <-
      as.numeric(names(which.max(table(
        unlist(lapply(actions, function(a) {
          length(strsplit(a, "\\.")[[1]])
        }))
      ))))
    n.levels <- seq(1, n.levels, 1)
  }
  
  extracted_levels <- list()
  for (i in n.levels) {
    for(m in 1:length(n.levels)){
      extracted_levels[[m]] <- unlist(lapply(x, function(b) {
        strsplit(b, "\\.")[[1]][i]
      }))
      
    }
  }
  return(extracted_levels)
}
