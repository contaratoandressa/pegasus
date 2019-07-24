# Analises PNAD (EM CONSTRUÇÃO!)

rm(list = ls())
require(install.load)
install_load("PNADcIBGE","survey")

# selection variables
vars <- c()

# filter database
year <- 2018 # for example
data = get_pnadc(year = year, design = FALSE, interview = 1)

# tables
# var1 and var2 is a pnad's names cod, see dic 
total_gender <- svytotal(~ interaction(var1, var2), data, na.rm = T)