#### User Query Predicton ####

library(e1071)
library(rpart)
library(class)
source("preprocessor.R")


query.predict <- function(query, model) {
  # Preprocess query terms
  query.df <- preprocess(query = query)
  
  # Merge query with vocabulary to match dimensions
  data.vocab <- data[0, -c(1, ncol(data))]
  data.vocab[1,] <- 0
  query.df <- merge(data.vocab, query.df[names(query.df) %in% names(data.vocab)], 
                    all = TRUE, all.x = FALSE)
  query.df[, is.na(query.df)] <- 0
  
  # Change term columns to binary factor
  query.df[,1:ncol(query.df)] <- lapply(query.df[,1:ncol(query.df)], factor, levels = c("0","1"))
  
  # Order column names alphabetically
  query.df <- query.df[, order(names(query.df))]
  
  # Predict category according to model
  if (model == "dt") {
    pred <- as.character(predict(dt.model, query.df, type="class"))
  } else if (model == "nb") {
    pred <- as.character(predict(nb.model, query.df))
  } else if (model == "knn") {
    k <- as.numeric(readline("Enter number of nearest neighbours (k): "))
    pred <- as.character(knn(data[, -c(1, ncol(data))], query.df, data$category, k = k))
  } else if (model == "all") {
    # Create data frame of all model predictions
    dt <- as.character(predict(dt.model, query.df, type="class"))
    nb <- as.character(predict(nb.model, query.df))
    knn <- as.character(knn(data[, -c(1, ncol(data))], query.df, data$category, k = 3))
    query.pred.df <- data.frame(row.names = c("Decision Tree", "Naive Bayes", "k-NN"), 
                                Category = c(dt, nb, knn), stringsAsFactors = FALSE)
  }
  
  # Calculate percent of matched vocabulary terms in query
  vocab.term <- colnames(query.df)[colnames(query.df) %in% colnames(data) & query.df[1,] == 1]
  query.term <- strsplit(query.in, " ")
  if (model != "all") {
    match <- (length(vocab.term) / length(query.term[[1]]))
  } else {
    query.pred.df$Match <- (length(vocab.term) / length(query.term[[1]]))
  }
    
  # Calculate evaluation metrics
  if (model == "dt") {
    accu <- dt.accu
    precis <- dt.cm[pred, pred] / sum(dt.cm[,pred])
    recall <- dt.cm[pred, pred] / sum(dt.cm[pred,])  
  } else if (model == "nb") {
    accu <- nb.accu
    precis <- nb.cm[pred, pred] / sum(nb.cm[,pred])
    recall <- nb.cm[pred, pred] / sum(nb.cm[pred,]) 
  } else if (model == "knn") {
    accu <- knn.accu
    precis <- knn.cm[pred, pred] / sum(knn.cm[,pred])
    recall <- knn.cm[pred, pred] / sum(knn.cm[pred,])  
  } else if (model == "all") {
    dt.precis <- dt.cm[dt, dt] / sum(dt.cm[,dt])
    dt.recall <- dt.cm[dt, dt] / sum(dt.cm[dt,])  
    nb.precis <- nb.cm[nb, nb] / sum(nb.cm[,nb])
    nb.recall <- nb.cm[nb, nb] / sum(nb.cm[nb,]) 
    knn.precis <- knn.cm[knn, knn] / sum(knn.cm[,knn])
    knn.recall <- knn.cm[knn, knn] / sum(knn.cm[knn,]) 
    
    query.pred.df <- cbind(query.pred.df, Accuracy = c(dt.accu, nb.accu, knn.accu))
    query.pred.df <- cbind(query.pred.df, Precision = c(dt.precis, nb.precis, knn.precis))
    query.pred.df <- cbind(query.pred.df, Recall = c(dt.recall, nb.recall, knn.recall))
  }
  
  # Calculate average score
  if (model != "all") {
    score <- mean(c(match, accu, precis, recall))
  } else {
    query.pred.df$Score <- rowMeans(query.pred.df[,2:5])
    
    # Sort by highest average score
    query.pred.df <- query.pred.df[order(query.pred.df$Score, decreasing = TRUE),]
  }
 
  # Print results
  if (model != "all") {   
    cat("Recommended category:", "\t", pred, "\n")
    cat("Terms in vocabulary:", "\t", vocab.term, "\n")
    cat("Term match:", "\t", "\t", match, "\n")
    cat("Accuracy of model:", "\t", accu, "\n")
    cat("Precision of class:", "\t", precis, "\n")
    cat("Recall of class:", "\t", recall, "\n")
    cat("Average score:", "\t", "\t", score, "\n")
  } else {
    cat("Recommended categories:", "\n")
    print(query.pred.df)
    cat("\n")
    cat("Terms in vocabulary:", "\t", vocab.term, "\n")
  }
}


# Request query
query.in <- readline(prompt = "Enter product name: ")
model <- readline(prompt = "Enter predictive model (nb/dt/knn/all): ")

# Predict category from query
query.predict(query.in, model)