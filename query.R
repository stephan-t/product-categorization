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
  
  # Find matched query terms in vocabulary
  vocab.term <- colnames(query.df)[colnames(query.df) %in% colnames(data) & query.df[1,] == 1]
  query.term <- strsplit(query.in, " ")
  
  # Print results
  cat("Recommended category:", as.character(result))
  cat("\n")
  cat("Terms in vocabulary:", vocab.term)
  cat("\n")
  cat("Term match:", (length(vocab.term) / length(query.term[[1]])))
}


# Request query
query.in <- readline(prompt = "Enter product name: ")
model <- readline(prompt = "Enter predictive model (nb/dt/knn): ")

# Predict category from query
query.predict(query.in, model)