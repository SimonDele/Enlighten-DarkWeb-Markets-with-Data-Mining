
country_Import_Export <- function(country,num) {
  
  #####################################################################
  #                            Library
  #####################################################################
  
  library (stringr)
  
  #####################################################################
  #                         Initialization
  #####################################################################
  
  #data <- as.data.frame(read.csv("data.csv"))[2:21]
  
  # Importation / Exportation :
  if (num == 0) { 
    way <- "origin"
    txt <- "- Exportation"
  } else if (num == 1) {
    way <- "destination"
    txt <- "- Importation"
  }
  
  #####################################################################
  #                           Analysis
  #####################################################################
  
  # Country as destination
  matching_vector <- str_detect(data[,way], country)
  
  # list of the categories (among the line that have "Country" as origin)
  # -> Products (categories) exporting by the country 
  country_cat <- data[matching_vector,"category"] 
  
  # Handling of this categories
  # Regular expression for spliting the categories
  regex <- "/(.*)/(.*)/(.*)"
  cat <- str_match(country_cat, regex)
  
  # Counting this categories 
  tab <- table(cat[,3])   #cat[,3] : 2nd category 
  tab <- sort(tab, decreasing = TRUE)  # Sorting (biggest in first) 
  tab <- tab[1:10] # Taking only the most important
  
  # Calculation in percentage
  piepercent<- round(100*tab/sum(tab), 1)
  # round(a,1) : one digit after the comma
  
  #####################################################################
  #                           Pie Chart     
  #####################################################################
  
  # 1- Labels :
  lab <- c()
  
  for(i in 1:length(piepercent)) {
    lab[i] <- paste(piepercent[[i]], "%", sep=" ")
  }
  
  # 2- Title :
  title <- paste(country, txt, sep=" ")
  
  # 3- Colors :
  c <- rainbow(length(piepercent))
  
  # 4- Plot :
  pie(piepercent,labels = lab, main = title ,col=c)
  
  # 5- Legend :
  legend(1.2,0.9, names(piepercent), cex = 0.8, fill = c)
  
}


#####################################################################
#                           Function  
#####################################################################

# 1st argument : Country
# 2nd : 
#   0 -> Exportation
#   1 -> Importation

country_Import_Export("United Kingdom",0)

