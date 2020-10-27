#' Tag topics based on known documents
#' @description Given a set of documents tagged with topics and a set of new documents, tags new documents by learning characteristics of topics from existing tagged documents.
#' @param new_documents a character vector of documents to tag
#' @param tagged_documents a character vector of documents 
#' @param tags a character vector of tags of the same length as tagged_documents
#' @param cutoff numeric: what cutoff should be used for probability of being included in a topic? If NULL, finds the optimal cutoff for model accuracy in training data.
#' @param prop_training numeric: what proportion of tagged_documents should be used for training? Remaining proportion will be used for model testing.
#' @return a character vector of tags of length equal to new_documents
#' @example inst/examples/tag_smartly_ex.R
#' @export
tag_smartly <- function(new_documents,
                        tagged_documents,
                        tags,
                        cutoff = NULL,
                        prop_training = .7) {
  tags <- factor(tags)
  words <- lapply(append(tagged_documents, new_documents), mine_terms)
  names(words) <- seq(1, length(words), 1)
  
  if (length(levels(tags)) > 2) {
    modfam <- "multinomial"
  } else{
    modfam <- "binomial"
  }
  
  # choo choo
  train_data <-
    sort(sample(1:length(tagged_documents), floor(length(tagged_documents) * prop_training)))
  test_data <-
    seq(1, length(tagged_documents), 1)[!(seq(1, length(tagged_documents), 1) %in% train_data)]
  new_data <-
    seq(length(tagged_documents)+1, length(words), 1)
  
  sparse_mat <- prep_matrix(words)
  
  mod <-
    fit_model(
      sparse_mat = sparse_mat,
      indices = train_data,
      tags = tags,
      modfam = modfam
    )
  # get the optimal cutoff value by performance on training data
  
  performance_train <-
    check_model(
      mod,
      indices = train_data,
      tags = tags,
      modfam = modfam,
      xdat = sparse_mat,
      cutoff = cutoff
    ) 
  
  # now check actual performance with the test data
  
  performance_test <-
    check_model(
      mod,
      indices = test_data,
      tags = tags,
      modfam = modfam,
      xdat = sparse_mat,
      cutoff = performance_train[1]
    ) 
  
  predictions <-
    stats::predict(mod,
            newx = sparse_mat[new_data, ],
            s = 'lambda.min',
            type = "response")
  
  new_tags <-
    get_tags(predictions,
             cutoff = performance_train[1],
             modfam = modfam,
             tags = tags)

  return(new_tags)
}

#' Gets model classification error rates
#' @description Given known data and model predictions, calculates model classification accuracy
#' @param input a character vector of known tags for testing model
#' @param output a character vector of tags from model predictions
#' @return a numeric score of model accuracy
#' @export
error_rates <- function(input, output) {
  crossed <- table(input, output)
  accuracy <- sum(diag(crossed)) / sum(colSums(crossed))
  
  # note to self, add an option here for binary true/false positive/negative error rates
  # can use for the alternative to kappa too
  
  return(accuracy)
}

#' Get tags from model predictions
#' @param predictions a vector or matrix of model predictions
#' @param cutoff numeric: a cutoff in probability for accepting predictions as tags
#' @param modfam string: either "binomial" or "multinomial"
#' @param tags factor: tags predicted by the model
#' @example inst/examples/tag_smartly_ex.R 
get_tags <- function(predictions, cutoff, modfam, tags) {
  if (modfam == "multinomial") {
    new_tags <- apply(predictions, 1, function(x) {
      if (x[which.max(x)] > cutoff) {
        levels(tags)[which.max(x)]
      } else{
        NA
      }
    }) ## note to self, could probably do this with dim of predictions and eliminate modfam?
  } else if (modfam == "binomial") {
    new_tags <- c()
    new_tags[predictions >= cutoff] <- levels(tags)[1]
    new_tags[predictions < cutoff] <- levels(tags)[2]
  }
  return(new_tags)
}

#' Prep sparse matrix for use in tag_smartly
#' @description Given a list of bags of words, creates a document-feature matrix
#' @param words a list of character vectors of words
#' @return a sparse document-feature matrix
prep_matrix <- function(words) {
  tokens <- lapply(words, function(x) {
    tmp <- gsub(" ", "_", x)
    paste(tmp, collapse = " ")
  })
  
  # make a sparse document-term matrix
  dtm <-
    tm::DocumentTermMatrix(tm::SimpleCorpus(tm::VectorSource(unlist(tokens))))
  sparse_mat <- Matrix::sparseMatrix(i = dtm$i, j = dtm$j)
  rm(dtm)
  dimnames(sparse_mat)[[2]] <-
    unique(unlist(words)) # avoid having numbers as terms
  
  return(sparse_mat)
}

#' Fit a GLM to build the topic model
#' @description Fits a GLM topic model to a document-feature matrix based on specified subset of documents
#' @param sparse_mat a sparse document-feature matrix of class ngCMatrix
#' @param indices a numeric vector of documents belonging to a subset used to fit the model
#' @param tags a character vector of document classifications or tags
#' @param modfam a string of length 1; options are "binomial" or "multinomial"
#' @return a glmnet model
fit_model <- function(sparse_mat, indices, tags, modfam) {
  classifications <- tags[indices]
  
  
  # can probably get this from length(levels(tags[indices])), right?
  if (modfam == "binomial") {
    classifications <- classifications == levels(classifications)[1]
  }
  doMC::registerDoMC(cores = 8)
  mod <-
    glmnet::cv.glmnet(
      sparse_mat[indices, ],
      classifications,
      family = modfam,
      type.multinomial = "grouped",
      parallel = TRUE,
      keep = TRUE
    )
  
  return(mod)
}

#' Check model performance
#' @description Given model inputs, checks model performance based on optimal or specified cutoff in classification probability 
#' @param mod a glmnet model
#' @param indices a numeric vector of documents belonging to a subset used to fit the model
#' @param tags a character vector of document classifications or tags for documents at indices
#' @param modfam a string of length 1; options are "binomial" or "multinomial"
#' @param xdat a sparse document-feature matrix of class ngCMatrix
#' @param cutoff What probability should be used as a cutoff for topic classification? Options are proportions between 0-1, or NULL to find an optimal cutoff based on model accuracy.
#' @return a vector containing the cutoff used and model accuracy at that cutoff
check_model <- function(mod, indices, tags, modfam, xdat, cutoff) {
  predictions <-
    stats::predict(mod,
            s = 'lambda.min',
            newx = xdat[indices, ],
            type = "response")
  if (is.null(cutoff)) {
    possible_cutoffs <- seq(0, 1, .01)
    errata <- c()
    for (i in seq_along(possible_cutoffs)) {
      tmp <-
        get_tags(
          predictions,
          cutoff = possible_cutoffs[i],
          modfam = modfam,
          tags = tags[indices]
        )
      errata[i] <- error_rates(as.character(tags[indices]), tmp)
    }
    cutoff <- possible_cutoffs[which.max(errata)]
  }
  new_tags <-
    get_tags(predictions,
             cutoff = cutoff,
             modfam = modfam,
             tags = tags[indices])
  mod_performance <-
    error_rates(as.character(tags[indices]), new_tags)
  mod_performance <- cbind(cutoff, mod_performance)
  return(mod_performance)
}
