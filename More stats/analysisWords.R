#-----------------------------------------------
#       Words analysis
#-----------------------------------------------

#data <- as.data.frame(read.csv("alphaClean.csv"))

library(stringr)

# Pick up every words

words.list <- str_split(data$title, boundary("word"))
tableWords <- sort(table(unlist(words.list)), decreasing = TRUE)


# Removing all words containing less than 2 letters

# Removing conjonction, preposition, numbers

