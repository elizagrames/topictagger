scheme <- list(woodpecker=c("woodpecker", "picoides", "sapsucker"), burn=c("burn", "fire"))
x <- df$abstract[1:20]
x <- paste(entogem$Title, entogem$Abstract)

tags <- entogem$Include


tag_smartly <- function(x, tags=NULL, n_iter=3, cutoff=NULL, prop_training=.7){
  tags <- factor(tags)
  words <- lapply(x, mine_terms)
  names(words) <- seq(1, length(words), 1)

  if(length(levels(tags))>2){
    modfam <- "multinomial"
  }else{
    modfam <- "binomial"
  }
  
  # choo choo
  train_data <- sort(sample(1:length(words), floor(length(words)*prop_training)))
  test_data <- as.numeric(names(words)[!(names(words) %in% train_data)])
  
  prep_matrix <- function(words){
    tokens <- lapply(words, function(x){
      tmp <- gsub(" ", "_", x)
      paste(tmp, collapse = " ")}
    )
    
    # make a sparse document-term matrix
    dtm <- tm::DocumentTermMatrix(tm::SimpleCorpus(tm::VectorSource(unlist(tokens))))
    sparse_mat <- Matrix::sparseMatrix(i = dtm$i, j = dtm$j)
    rm(dtm)
    dimnames(sparse_mat)[[2]] <- unique(unlist(words)) # avoid having numbers as terms
       
    return(sparse_mat)
  }
  sparse_mat <- prep_matrix(words)
  
  fit_model <- function(sparse_mat, indices, tags, modfam){
    classifications <- tags[indices]
    
    if(modfam=="binomial"){
      classifications <- classifications==levels(classifications)[1]
    }
    doMC::registerDoMC(cores=8)
    mod <- glmnet::cv.glmnet(sparse_mat[indices,], classifications, family=modfam, 
                             type.multinomial="grouped", parallel=TRUE, keep=TRUE)
    
    return(mod)
}
  zz <- fit_model(sparse_mat = prep_matrix(words), indices=train_data, tags = tags, modfam = modfam)
  
  # okay, so the mat needs to be full
  predict(zz, newx = sparse_mat[test_data,], s='lambda.min', type="response")
  
test_model <- function(mod, indices, tags, modfam, xdat, cutoff){
  predictions <- predict(mod, s='lambda.min', newx=xdat, type="response")
  if(is.null(cutoff)){
    possible_cutoffs <- seq(0,1,.01)
    for(i in seq_along(possible_cutoffs)){
      tmp <- get_tags(predictions, cutoff=possible_cutoffs[i], modfam=modfam, tags=tags[indices])
      tmp2 <- error_rates(as.character(tags[train_data]), tmp)
      fp[i] <- tmp2[1]
      fn[i] <- tmp2[2]
    }
    
    cutoff <- possible_cutoffs[which.min(abs(unlist(fp) - unlist(fn)))]
  }
  new_tags <- get_tags(predictions, cutoff=cutoff, modfam=modfam, tags=tags[indices])
  mod_performance <- error_rates(as.character(tags[train_data]), new_tags)
  
  return(mod_performance)
}

test_mat <- prep_matrix(words=words, indices=test_data, tags=tags, modfam=modfam)

predictions <- predict(mod, s='lambda.min', newx=test_mat, type="response")

  

  
  
  return(new_tags)
  
  
}


error_rates <- function(input, output){
  opt1 <- levels(factor(input))[1]
  opt2 <- levels(factor(input))[2]
  
  fp <- length(which(input==opt2 & output==opt1))
  fn <- length(which(input==opt1 & output==opt2))
  tp <- length(which(input==opt1 & output==opt1))
  tn <- length(which(input==opt2 & output==opt2))
  
  return(data.frame(false_positive = fp/(fp+tp),
                    false_negative = fn/(fn+tn)))
}

get_tags <- function(predictions, cutoff, modfam, tags){
  if(modfam=="multinomial"){
    new_tags <- apply(predictions, 1, function(x){
      if(x[which.max(x)]>cutoff){
        levels(tags)[which.max(x)]
      }else{
        NA
      }
    })
  }else if(modfam=="binomial"){
    new_tags <- c()
    new_tags[predictions >= cutoff] <- levels(tags)[1]
    new_tags[predictions < cutoff] <- levels(tags)[2]
  }
  return(new_tags)
}

