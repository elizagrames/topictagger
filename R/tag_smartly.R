tag_smart <- function(){
  
}

tag_learn <- function(doc, topics){
  keywords <- lapply(doc, function(x){
    gsub(" ", "_", litsearchr::extract_terms(x, method="fakerake", min_freq = 1, min_n=2))
  })
  
  # how to do the seeds? should i get keywords and subset those?
  # like anything with 'forest' is a starting point?
  # or all terms in a document and then do seeded lda?
  
  lapply(keywords, function(x){
    grep("forest", x)
  })
  
}