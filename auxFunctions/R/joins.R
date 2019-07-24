# Funcao para full join para varios anos em duas bases de dados numa mesma pasta

rm(list = ls())
require(install.load)
install_load("dplyr","readxl")

merge_podram <- function(dir = "dir", name1 = "db1", name2 = "db2", var = "id", year = "year_column"){
  
  db1 <- read_excel(paste0(dir, name2)) ; db2 <- read_excel(paste0(dir, name1))
  dados <- data.frame()
  min_year <- min(bd1[,year]) ; max_year <- max(bd1[,year])
  
  for(i in min_year:max_year){ 
    aux <- filter(db2, year_column == i)
    aux <- full_join(db1, aux, by = var)
    dados <- rbind(dados, aux)
  }
  
  write.csv2(dados, paste0(dir, "data.csv"))
  return(dados)
}
