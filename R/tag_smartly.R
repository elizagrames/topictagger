tag_smartly <- function(x,
                        tags = NULL,
                        cutoff = NULL,
                        prop_training = .7) {
  tags <- factor(tags)
  words <- lapply(x, mine_terms)
  names(words) <- seq(1, length(words), 1)
  
  if (length(levels(tags)) > 2) {
    modfam <- "multinomial"
  } else{
    modfam <- "binomial"
  }
  
  # choo choo
  train_data <-
    sort(sample(1:length(words), floor(length(words) * prop_training)))
  test_data <-
    as.numeric(names(words)[!(names(words) %in% train_data)])
  
  sparse_mat <- prep_matrix(words)
  
  mod <-
    fit_model(
      sparse_mat = sparse_mat,
      indices = train_data,
      tags = tags,
      modfam = modfam
    )
  performance <-
    test_model(
      mod,
      indices = train_data,
      tags = tags,
      modfam = modfam,
      xdat = sparse_mat,
      cutoff = cutoff
    )
  
  predictions <-
    predict(mod,
            newx = sparse_mat[test_data, ],
            s = 'lambda.min',
            type = "response")
  new_tags <-
    get_tags(predictions,
             cutoff = performance[1],
             modfam = modfam,
             tags = tags)
  
  return(new_tags)
}


error_rates <- function(input, output) {
  crossed <- table(input, output)
  accuracy <- sum(diag(crossed)) / sum(colSums(crossed))
  
  return(accuracy)
}

get_tags <- function(predictions, cutoff, modfam, tags) {
  if (modfam == "multinomial") {
    new_tags <- apply(predictions, 1, function(x) {
      if (x[which.max(x)] > cutoff) {
        levels(tags)[which.max(x)]
      } else{
        NA
      }
    })
  } else if (modfam == "binomial") {
    new_tags <- c()
    new_tags[predictions >= cutoff] <- levels(tags)[1]
    new_tags[predictions < cutoff] <- levels(tags)[2]
  }
  return(new_tags)
}

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

fit_model <- function(sparse_mat, indices, tags, modfam) {
  classifications <- tags[indices]
  
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

test_model <- function(mod, indices, tags, modfam, xdat, cutoff) {
  predictions <-
    predict(mod,
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
    error_rates(as.character(tags[train_data]), new_tags)
  mod_performance <- cbind(cutoff, mod_performance)
  return(mod_performance)
}
