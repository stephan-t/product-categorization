#### User Query Predicton ####

library(e1071)
library(rpart)
library(class)
source("preprocessor.R")


query.predict <- function(query) {
  
  #### Preprocessing ####
  
  # Preprocess query terms
  query.df <- preprocess(query = query)
  
  # Merge query with vocabulary to match dimensions
  dt.vocab <- dt.data[0, -c(1, ncol(dt.data))]
  nb.vocab <- nb.data[0, -c(1, ncol(nb.data))]
  knn.vocab <- knn.data[0, -c(1, ncol(knn.data))]
  dt.vocab[1,] <- 0
  nb.vocab[1,] <- 0
  knn.vocab[1,] <- 0
  dt.query.df <- merge(dt.vocab, query.df[names(query.df) %in% names(dt.vocab)], 
                    all = TRUE, all.x = FALSE)
  nb.query.df <- merge(nb.vocab, query.df[names(query.df) %in% names(nb.vocab)], 
                    all = TRUE, all.x = FALSE)
  knn.query.df <- merge(knn.vocab, query.df[names(query.df) %in% names(knn.vocab)], 
                    all = TRUE, all.x = FALSE)
  dt.query.df[, is.na(dt.query.df)] <- 0
  nb.query.df[, is.na(nb.query.df)] <- 0
  knn.query.df[, is.na(knn.query.df)] <- 0
  
  # Change term columns to binary factor
  dt.query.df[,1:ncol(dt.query.df)] <- lapply(dt.query.df[,1:ncol(dt.query.df)], 
                                              factor, levels = c("0","1"))
  nb.query.df[,1:ncol(nb.query.df)] <- lapply(nb.query.df[,1:ncol(nb.query.df)], 
                                              factor, levels = c("0","1"))
  knn.query.df[,1:ncol(knn.query.df)] <- lapply(knn.query.df[,1:ncol(knn.query.df)], 
                                                factor, levels = c("0","1"))
  
  # Order column names alphabetically
  dt.query.df <- dt.query.df[, order(names(dt.query.df))]
  nb.query.df <- nb.query.df[, order(names(nb.query.df))]
  knn.query.df <- knn.query.df[, order(names(knn.query.df))]
  
  
  #### Classification ####
      
  # Predict class labels
  dt.query.pred <- as.character(predict(dt.model, dt.query.df, type="class"))
  nb.query.pred <- as.character(predict(nb.model, nb.query.df))
  knn.query.pred <- as.character(knn(knn.data[, -c(1, ncol(knn.data))], knn.query.df, 
                                     knn.data$category, k = 3))
  
  # Create data frame of model predictions
  query.pred.df <- data.frame(row.names = c("Decision Tree", "Naive Bayes", "k-NN"), 
                              Category = c(dt.query.pred, nb.query.pred, knn.query.pred), 
                              stringsAsFactors = FALSE)
  
  
  #### Evaluation ####
    
  # Calculate evaluation metrics
  dt.precis <- dt.cm[dt.query.pred, dt.query.pred] / sum(dt.cm[,dt.query.pred])
  dt.recall <- dt.cm[dt.query.pred, dt.query.pred] / sum(dt.cm[dt.query.pred,])  
  nb.precis <- nb.cm[nb.query.pred, nb.query.pred] / sum(nb.cm[,nb.query.pred])
  nb.recall <- nb.cm[nb.query.pred, nb.query.pred] / sum(nb.cm[nb.query.pred,]) 
  knn.precis <- knn.cm[knn.query.pred, knn.query.pred] / sum(knn.cm[,knn.query.pred])
  knn.recall <- knn.cm[knn.query.pred, knn.query.pred] / sum(knn.cm[knn.query.pred,]) 
  
  query.pred.df <- cbind(query.pred.df, Accuracy = c(dt.accu, nb.accu, knn.accu))
  query.pred.df <- cbind(query.pred.df, Precision = c(dt.precis, nb.precis, knn.precis))
  query.pred.df <- cbind(query.pred.df, Recall = c(dt.recall, nb.recall, knn.recall))
  
  # Calculate percent of matched vocabulary terms in query
  dt.vocab.term <- colnames(dt.query.df)[colnames(dt.query.df) %in% colnames(dt.data) 
                                         & dt.query.df[1,] == 1]
  nb.vocab.term <- colnames(nb.query.df)[colnames(nb.query.df) %in% colnames(nb.data) 
                                         & nb.query.df[1,] == 1]
  knn.vocab.term <- colnames(knn.query.df)[colnames(knn.query.df) %in% colnames(knn.data) 
                                           & knn.query.df[1,] == 1]
  query.term <- strsplit(query.in, " ")
  query.pred.df$Match <- c(length(dt.vocab.term) / length(query.term[[1]]),
                           length(nb.vocab.term) / length(query.term[[1]]),
                           length(knn.vocab.term) / length(query.term[[1]]))
  
  # Create data frame of model vocabularies
  query.vocab.df <- data.frame(row.names = c("Decision Tree", "Naive Bayes", "k-NN"),
                               Size = c(length(dt.data)-2, length(nb.data)-2, length(knn.data)-2),
                               Match = c(paste0("{", paste(dt.vocab.term, collapse = ", "), "}"), 
                                         paste0("{", paste(nb.vocab.term, collapse = ", "), "}"),
                                         paste0("{", paste(knn.vocab.term, collapse = ", "), "}")), 
                               stringsAsFactors = FALSE)
  
  # Calculate average score
  query.pred.df$Score <- rowMeans(query.pred.df[,2:5])
  query.vocab.df$Score <- rowMeans(query.pred.df[,2:5])
  
  # Sort by highest average score
  query.pred.df <- query.pred.df[order(query.pred.df$Score, decreasing = TRUE),]
  query.vocab.df <- query.vocab.df[order(query.vocab.df$Score, decreasing = TRUE),]
 
  # Print results
  cat("\nRecommended Categories:\n")
  print.data.frame(query.pred.df, digits = 3)
  cat("\nClassifier Vocabularies:\n")
  print.data.frame(query.vocab.df[, -3])
}


# Request query
query.in <- readline(prompt = "Enter product name: ")

# Predict categories from query
query.predict(query.in)