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
    result <- predict(dt.model, query.df, type="class")
  } else if (model == "nb") {
    result <- predict(nb.model, query.df)
  } else if (model == "knn") {
    k <- as.numeric(readline("Enter number of nearest neighbours (k): "))
    result <- knn(data[, -c(1, ncol(data))], query.df, data$category, k = k)
  }
  result <- as.character(result)
  
  # Calculate percent of matched vocabulary terms in query
  vocab.term <- colnames(query.df)[colnames(query.df) %in% colnames(data) & query.df[1,] == 1]
  query.term <- strsplit(query.in, " ")
  match <- (length(vocab.term) / length(query.term[[1]]))
  
  # Calculate evaluation metrics
  if (model == "dt") {
    accu <- dt.accu
    cm <- dt.cm
  } else if (model == "nb") {
    accu <- nb.accu
    cm <- nb.cm
  } else if (model == "knn") {
    accu <- knn.accu
    cm <- knn.cm
  }
  precis <- cm[result, result] / sum(cm[,result])
  recall <- cm[result, result] / sum(cm[result,])  
  
  # Calculate average score
  score <- (match + accu + precis + recall) / 4
  
  # Print results
  cat("Recommended category:", "\t", result)
  cat("\n")
  cat("Terms in vocabulary:", "\t", vocab.term)
  cat("\n")
  cat("Term match:", "\t", "\t", match)
  cat("\n")
  cat("Accuracy of model:", "\t", accu)
  cat("\n")
  cat("Precision of class:", "\t", precis)
  cat("\n")
  cat("Recall of class:", "\t", recall)
  cat("\n")
  cat("Average score:", "\t", "\t", score)
}


# Request query
query.in <- readline(prompt = "Enter product name: ")
model <- readline(prompt = "Enter predictive model (nb/dt/knn): ")

# Predict category from query
query.predict(query.in, model)