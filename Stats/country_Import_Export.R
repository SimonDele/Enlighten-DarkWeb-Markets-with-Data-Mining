
  
  # 4- Plot :
  #pie(v[1:10,2],labels=v[,1], main = title ,col=c)

#data <- as.data.frame(read.csv("data.csv"))
  
  #-----------------------------------------------
  #   Importation / Exportation of a country
  #-----------------------------------------------
  
    #-------------------
    #  Initialization
    #-------------------
    
    country <- "France"
    
    #---------------------------
    #    Analysis - Exportation
    #---------------------------
    
    # Country as destination
    matching_vector_exp <- str_detect(data[,"origin"], country)
    
    # list of the categories (among the line that have "Country" as origin)
    # -> Products (categories) exporting by the country 
    country_cat_exp <- data[matching_vector_exp,"category"] 
    
    # Handling of this categories
    # Regular expression for spliting the categories
    regex <- "/(.*)/(.*)/(.*)"
    cat_exp <- str_match(country_cat_exp, regex)
    
    # Counting this categories 
    tab_exp <- table(cat_exp[,3])   #cat[,3] : 2nd category 
    tab_exp <- sort(tab_exp, decreasing = TRUE)  # Sorting (biggest in first) 
    tab_exp <- tab_exp[1:10] # Taking only the most important
    
    #---------------------------
    #    Analysis - Importation
    #---------------------------
    
    # Country as destination
    matching_vector_imp <- str_detect(data[,"destination"], country)
    
    # list of the categories (among the line that have "Country" as origin)
    # -> Products (categories) exporting by the country 
    country_cat_imp <- data[matching_vector_imp,"category"] 
    
    # Handling of this categories
    # Regular expression for spliting the categories
    regex <- "/(.*)/(.*)/(.*)"
    cat_imp <- str_match(country_cat_imp, regex)
    
    # Counting this categories 
    tab_imp <- table(cat_imp[,3])   #cat[,3] : 2nd category 
    tab_imp <- sort(tab_imp, decreasing = TRUE)  # Sorting (biggest in first) 
    tab_imp <- tab_imp[1:10] # Taking only the most important
    
    #-------------------------
    #    Analysis - Fusion
    #-------------------------
    
    tab_exp <- data.frame(tab_exp)
    tab_imp <- data.frame(tab_imp)
    
    tab <- merge(tab_exp,tab_imp,by.x="Var1",by.y="Var1",all = TRUE)
    
    for(i in 1:length(tab[,2])){
      if(is.na(tab[i,2])) {tab[i,2] <-0}
    }
    
    for(i in 1:length(tab[,3])){
      if(is.na(tab[i,3])) {tab[i,3] <-0}
    }
    
    #---------------------------
    #    Pie Chart - Exporation     
    #---------------------------
    
    
    
    par(mfrow = c(1,2))
    
    # 1- Labels :
    
    # Calculation in percentage
    piepercent_exp <- round(100*tab[,2]/sum(tab[,2]), 1)
    # round(a,1) : one digit after the comma
    
    lab_exp <- c()
    
    for(i in 1:length(piepercent_exp)) {
      if(piepercent_exp[[i]] == 0) {lab_exp[i] <- ""}
      else {lab_exp[i] <- paste(piepercent_exp[[i]], "%", sep=" ")}
    }
    
    # 2- Title :
    title <- "Exportation"
    
    # 2- Colors :
    c <- rainbow(length(tab[,1]))
    
    # 3- Plot :
    pie(tab[,2],labels=lab_exp, main = title ,col=c)
    
    #---------------------------
    #    Pie Chart - Importation     
    #---------------------------
    
    
    # 1- Labels :
    
    # Calculation in percentage
    piepercent_imp <- round(100*tab[,3]/sum(tab[,3]), 1)
    # round(a,1) : one digit after the comma
    
    lab_imp <- c()
    
    for(i in 1:length(piepercent_imp)) {
      if(piepercent_imp[[i]] == 0) {lab_imp[i] <- ""}
      else {lab_imp[i] <- paste(piepercent_imp[[i]], "%", sep=" ")}
    }
    
    # 2- Title :
    title <- "Importation"
    

    # 3- Plot :
    pie(tab[,3],labels=lab_imp, main = title ,col=c)
    
    
    legend(x=-4.3,y=-1,tab[,1], cex = 0.8, fill = c,ncol=4,border=NA, xpd=NA)
    title(main="France", outer=TRUE)