#----------------------------------------------------------------------
#             Text mining : Create topics
#-----------------------------------------------------------------------

#http://tidytextmining.com/topicmodeling.html
#http://data-analytics.net/cep/Schedule_files/Textmining%20%20Clustering,%20Topic%20Modeling,%20and%20Classification.htm

install.packages("topicmodels")
install.packages("tidytext")

library(topicmodels)
library(RTextTools)
library(tidytext)
library(ggplot2)
library(dplyr)

library(tm)
library(plyr)
library(class)
library(caret)
library(stringr)
library(tidyr)

#data <- as.data.frame(read.csv("alphaClean.csv"))

# Select all "Drugs & Chemicals" ads
matching_vector <- c( str_detect(data$category, "/Drugs & Chemicals/Prescription/"))
new.data <- data[matching_vector,]

#new.data <- data[data$category =="/Services/Other" ]

new.data$ad <- as.character(new.data$ad)

new.data$ad <- removeWords(new.data$ad, c("product description"))


# Removing conjonction, preposition, ??numbers??
commonWords <- read.csv("./UEL-project/Text Mining/5000CommonWords.csv")
commonWords <- as.vector(commonWords$Word)
commonWords <- str_trim(commonWords)

new.data$ad <- removeWords(new.data$ad, commonWords[0:2000])
new.data$ad <- removeWords(new.data$ad, commonWords[2001:5000])

source <- VectorSource(new.data$ad)
corpus <- Corpus(source)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removeWords, stopwords('english'))

# CREATE THE DOCUMENT-TERM MATRIX
#doc_matrix <- create_matrix(data$ad, language="english", removeNumbers=TRUE,
#                            stemWords=TRUE, removeStopwords=TRUE,removeSparseTerms=.998)

doc_matrix <- DocumentTermMatrix(corpus)
rowTotals <- apply(doc_matrix , 1, sum) #Find the sum of words in each Document
doc_matrix  <- doc_matrix[rowTotals> 0, ] #remove all docs without words


#
'
mat4 <- weightTfIdf(doc_matrix)
mat4 <- as.matrix(mat4)

norm_eucl <- function(m)
  m/apply(m,1,function(x) sum(x^2)^.5)
mat_norm <- norm_eucl(mat4)

set.seed(5)
k <- 3
kmeansResult <- kmeans(mat_norm, k)
'
#Topic Modeling


k <- 5
lda <- LDA(doc_matrix, k)


#Plot words of k topics
#ap_lda <- LDA(doc_matrix, k = 2, control = list(seed = 1234))


ap_topics <- tidy(lda, matrix = "beta")
#ap_topics


ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

terms(lda)
