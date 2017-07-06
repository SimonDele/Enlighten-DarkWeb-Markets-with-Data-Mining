#----------------------------------------------------------
#           Guess if this dealer is selling this drugs
#----------------------------------------------------------

#data <- as.data.frame(read.csv("data.csv"))

library(stringr)
library(arules)
library(arulesViz)

#Select all ads of "Drugs & Chemicals"
matching_vector <- c( str_detect(data$category, "Drugs & Chemicals"))
data_drugs <- data[matching_vector, ]

#data_drugs$category <- gsub("/Drugs & Chemicals/", "", data_drugs$category)

# Handling of this categories
# Regular expression for spliting the categories
regex <- "/(.*)/(.*)/(.*)"
cat_exp <- str_match(data_drugs$category, regex)
data_drugs$category <- cat_exp[,3]

#List all the sellers
sellers <-summary(data_drugs$seller)
sellers <- sellers[ sellers != "Null"]


#List all categories concerning drugs
list_category <- table(data_drugs[,"category"])
list_cat_drugs <- list_category [ list_category != 0 ]


#First step : initialise a data.frame with the information of the first seller

#Select all categories of the seller  
matching_vector <- c( str_detect(data$seller, names(sellers)[1]))
cat_seller <-summary(data.frame(data[matching_vector, "category"]))


#Loop which creates a boolean vector which tells if the seller sells stuffs in each category 
bool_cat <-c()
bool_vec <-c()
for( i in 1: length(list_cat_drugs)){
  bool_vec <- str_detect(cat_seller, names(list_cat_drugs)[i])
  
  bool <- FALSE 
  for(j in 1:length(bool_vec)){
    bool <- bool || bool_vec[j]
  }
  bool_cat[i] <- bool
  
}

cat_seller.data <- t(data.frame(bool_cat))
colnames(cat_seller.data) <- names(list_cat_drugs)

#Step 2 : Do the same for the other sellers

for(k in 2 : length(sellers)){
  #Select all categories of the seller  
  matching_vector <- c( str_detect(data$seller, names(sellers)[k]))
  cat_seller <-summary(data.frame(data[matching_vector, "category"]))
  
  
  #Loop which creates a boolean vector which tells if the seller sells stuffs in each category 
  bool_cat <-c()
  bool_vec <-c()
  for( i in 1: length(list_cat_drugs)){
    bool_vec <- str_detect(cat_seller, names(list_cat_drugs)[i])
    
    bool <- FALSE 
    for(j in 1:length(bool_vec)){
      bool <- bool || bool_vec[j]
    }
    bool_cat[i] <- bool
    
  }
  
  cat_seller.data <- rbind(cat_seller.data,bool_cat)
  
}



rownames(cat_seller.data)<- names(sellers)

#Replace TRUE by 1 and FALSE by 0 for the decision tree

#cat_seller.data.num <-c()
#for(i in 1:length(colnames(cat_seller.data))){
#  cat_seller.data[,i]<-gsub(TRUE, 1, cat_seller.data[,i])
#  cat_seller.data[,i] <- gsub(FALSE, 0, cat_seller.data[,i])
#  cat_seller.data.num <- as.numeric(cat_seller.data[,i])
#  cat_seller.data[,i] <- cat_seller.data.num
#}



#Association Rules with rhs containing "Stimulants/Cocaine" only
rules <- apriori(cat_seller.data,
                 parameter = list(minlen=3, supp=0.02, conf=0.8),
                 appearance = list(rhs=c("Ecstasy"),default="lhs"),
                 control = list(verbose=F))

rules.sorted <- sort(rules, by="lift")
inspect(rules.sorted)

#Plot graph of rules
plot(rules, method="graph", control=list(type="items"))



# Descision tree

#library(rattle)
#library(rpart.plot)
#library(rpart)

#train <- data.frame(cat_seller.data[1:50,])

#tree_g <- rpart(X.Drugs...Chemicals.Psychedelics.LSD ~., data=train , method = "class")
#fancyRpartPlot(tree_g)