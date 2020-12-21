#' Group documents by unknown topic
#' @description Assign topics to documents when there is no scheme or prior knowledge for classifying documents other than number of topics.
#' @param docs a character vector of documents
#' @param k numeric: how many groups should be created
#' @param ngrams logical: if TRUE, groups documents using ngrams (i.e. multi-word phrases) instead of single terms
#' @param n_terms numeric: how many terms should be returned for each topic
#' @return a vector of topic numbers and associated terms for users to post-hoc classify
#' @export
tag_freely <- function(docs, k=3, ngrams=TRUE, n_terms=10){
    tokens <- lapply(docs, mine_terms, ngrams=ngrams)
    
    tokens <- lapply(tokens, function(x){
      tmp <- gsub(" ", "_", x)
      paste(tmp, collapse = " ")}
    )
    
  # now convert the code from entogem tm here
    keep_names <- which(tokens!="")
    z2 <- tm::SimpleCorpus(tm::VectorSource(unlist(tokens[!tokens==""])))
    tdm <- tm::DocumentTermMatrix(z2)
    
    m <- topicmodels::LDA(tdm, k = k, method="Gibbs")
    tps <- topicmodels::topics(m)
    
    all_topics <- c()
    all_topics[keep_names] <- tps
    
    topic_terms <- topicmodels::terms(m, k=n_terms)
    
    return(list(all_topics, topic_terms))
}

