# Request query
query <- readline(prompt = "Enter a product name: ")

# Split query into seperate elements
query <- strsplit(query, " ")

# Stem query elements
query.v <- character()
for (i in query) {
  i <- wordStem(i)  
  query.v <- c(query.v, i)
}

# Convert query to document term matrix
query.df <- data.frame(rep(1, len = length(query.v)), row.names = query.v)
query.df <- as.data.frame(t(query.df))
query.df[, 1:length(query.df)] <- lapply(query.df[, 1:length(query.df)], factor, levels = c("0", "1"))

# Return predicted category
result <- predict(nb.model, query.df)
print(paste("Recommended category: ", result))