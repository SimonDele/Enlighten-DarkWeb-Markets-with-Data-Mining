
# Making readable the data in a computer way :

computerReadable <- function(database) {

oz_conversion <- 28.3495
    
#####################################################################
#                     Handling : Dose and unit
#####################################################################

    # 1- Extraction of characters matching with the dose and unit in the title

# Vector with all the unit that are allowed (add unit if needed)
unit_allowed <- c("mg","kg", "ug","lb","oz","ounce","g\\s","gr","gram") 

# Construct a regular expression matching with digits + units allowed
regex_unit <- str_c("([0-9]+\\.?[0-9]*)((?:(\\s|)((",unit_allowed[1],")")
for(i in 2:length(unit_allowed)){
  regex_unit <- str_c(regex_unit,"|(",unit_allowed[i],")")
}
regex_unit <- str_c(regex_unit,")))")
# regex_unit = regular expression for dose and unit

# Extraction from the title :
dose_unit <- str_extract(database$title, regex_unit)

    # 2- Spliting the value and the unit

# Construct a regular expression 
regex_extrac_unit <- str_c("(.*?)(",unit_allowed[1])
for(i in 1:length(unit_allowed)){
  regex_extrac_unit <- str_c(regex_extrac_unit,"|",unit_allowed[i])
}
regex_extrac_unit <- str_c(regex_extrac_unit,")")

# Spliting thanks to the regular expresion (regex_extrac_unit)
dose_unit <- str_match(dose_unit, regex_extrac_unit)

# amelioration of the string (removing blank)
dose_unit <- trimws(dose_unit)

    # 3- Conversion of units in SI (in order to use a library)

# Vector of conversion : first element of the vector is unit in SI, other elements are non standard unit 
# Add your vector if needed
g <- c("g","gr","gram")
oz <- c("oz","ounce")

for(i in 2 : length(g)){
  dose_unit[,3] <- gsub(pattern=g[i], replacement=g[1],dose_unit[,3])
}
for(i in 2 : length(oz)){
  dose_unit[,3] <- gsub(pattern=oz[i], replacement=oz[1],dose_unit[,3])
}
#add loop for your vector if needed

    # 4- Insertion in the data frame

database$dose <- as.numeric(dose_unit[,2])    # Numerical conversion
database$unit <- dose_unit[,3]

    # 5- Conversion to SI units : 1g and 1l
    
for(i in 1:length(database$unit)) {
    if(!(is.na(database[i,"unit"]))) {
      if ((str_detect(database[i,"unit"],"g") | (str_detect(database[i,"unit"],"lb")))) {
        value <- set_units(database[i,"dose"],with(ud_units,database[i,"unit"]))
        database[i,"dose"] <- as.units(value, with(ud_units, g))
        database[i,"unit"] <- "g"
      }
      else if (str_detect(database[i,"unit"],"l"))  {
        value <- set_units(database[i,"dose"],with(ud_units,database[i,"unit"]))
        database[i,"dose"] <- as.units(value, with(ud_units, l))
        database[i,"unit"] <- "l"
      }
       else if (str_detect(database[i,"unit"],"oz")) {
         database[i,"dose"] <- database[i,"dose"] * oz_conversion
         database[i,"unit"] <- "g"
      }
    }
} 

#####################################################################
#                     Handling : Quantity
#####################################################################

    # 1- Extraction of characters matching with the quantity in the title

# (ex : 20 packs, 20x, x20, 20 tabs)
# add key words here if needed
key_words_quantity <- c("x","pack", "tab", "pill", "pcs", "piece")

# Particular treatment for "x" because it can be 20x or x20"
regex_extract_quantity <- str_c("(",key_words_quantity[1],"(\\s|)(\\d+,?\\d+)|(\\d+,?\\d+)(?:([-\\s]|)(",key_words_quantity[1])

for(i in 2 : length(key_words_quantity)){
  regex_extract_quantity <- str_c(regex_extract_quantity,"|",key_words_quantity[i])
}
regex_extract_quantity <- str_c(regex_extract_quantity,")))")

# Extraction from the title + insertion in the data frame :
database$quantity  <- str_extract(database$title,regex_extract_quantity)

# Keeping only digits
database$quantity  <- str_extract(database$quantity , "(\\d+,?\\d+)")

    # 2- Conversion in numerical element

# English numbers to Standard numbers (problem with the comma)
database$quantity <- gsub(pattern=",", replacement="", database$quantity) 

# Conversion :
database$quantity <- as.numeric(database$quantity)


#####################################################################
#                       Handling : Price
#####################################################################

    # 1- column price as numeric :
    
# Keeping only digits (without "USD")
database$price <- str_extract(database$price, "(\\d+,?\\.?\\d+)")

# English numbers to Standard numbers (problem with the comma)
database$price <- gsub(pattern=",", replacement="", database$price)

# Conversion :
database$price <- as.numeric(database$price)

    # 2- Price per unit :

# Creation of a new vector with the price per unit
price_per_unit <- c()

for(i in 1:length(database$quantity)) {
  if(is.na(database[i,"quantity"])) {price_per_unit[i] <- database[i,"price"]}
  else {price_per_unit[i] <- database[i,"price"]/database[i,"quantity"]}
}

#Insertion in the data frame
database$priceUnit <- price_per_unit

    
      
  # 3- Price per unit per dose :
  
# Creation of a new vector with the price per unit per dose
price_unit_dose <- c()

for(i in 1:length(database$dose)) {
    if(is.na(database[i,"dose"])) {price_unit_dose[i] <- database[i,"priceUnit"]}
    else {price_unit_dose[i] <- database[i,"priceUnit"]/database[i,"dose"]}
}

#Insertion in the data frame
database$priceUnitDose <- price_unit_dose
  

return(database) 
} 
