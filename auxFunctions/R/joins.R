# Funcao para full join para varios anos em duas bases de dados numa mesma pasta

rm(list = ls())
require(install.load)
install_load("dplyr","readxl")

merge_podram <- function(dir = "dir", name1 = "db1", name2 = "db2", var = "id", year = "year_column"){
  
  db1 <- read_excel(paste0(dir, name1)) ; db2 <- read.csv(paste0(dir, name2))
  dados <- data.frame()
  min_year <- min(db2[,year]) ; max_year <- max(db2[,year])
  
  for(i in min_year:max_year){ 
    aux <- db1[which(db1[year] == i),]
    aux <- full_join(db2, aux, by = var)
    dados <- rbind(dados, aux)
  }
  dados <- na.omit(dados)
  write.csv2(dados, paste0(dir, "data.csv"))
  return(dados)
}
