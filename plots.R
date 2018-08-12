#### Plots ####

# install.packages("ggplot2")
library(ggplot2)

# Plot confusion matrix
plot.cm <- function(cm, title = NULL){
  p <-
    ggplot(data = as.data.frame(cm), aes(x = Predicted, y = Actual)) +
    geom_tile(aes(fill = log(Freq)), colour = "white") +
    scale_fill_gradient(low = "white", high = "steelblue", na.value="white") +
    geom_text(aes(x = Predicted, y = Actual, label = Freq), size = 3) +
    theme(legend.position = "none") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  return(p)
}
