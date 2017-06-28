#####################################################################
#                       Units library exemple
#####################################################################
#> a <- set_units(1, km)
# b <- set_units(1, m)
# a+b
# 1.001 km
# as.units(a, with(ud_units, m))
# 1000 m

#####################################################################
#                    How to use it in our case : 
#####################################################################
if(!(is.na(data[i,"dose"]))) {value <- set_units(data[i,"dose"],with(ud_units,data[i,"unit"]))} 
value_conv <- as.units(value, with(ud_units, X))  #X = unit that you want
