# Conecction database sqlite in R

rm(list = ls())

# packages
require(install.load)
install_load("RSQLite", "devtools")
# or devtools::install_github("rstats-db/RSQLite")

# directory
setwd("dir")

# connection
sqlite <- dbDriver("SQLite")
conn <- dbConnect(sqlite,"name.db")

# selection variables
p1 = dbGetQuery( conn,'select * from name' )
