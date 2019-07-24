# function to read as bases and return as densities of variables

rm(list = ls())

require(install.load)
install_load("read_xl","dplyr")

dendidade <- function(dir, nome, colunas, pg, ini, fim){
  
  data <- data.frame(read_excel(paste0(dir, nome), sheet = pg), stringsAsFactors = F)
  data <- data[,colunas]
  
  par(mfrow=c(ini,fim))
  
  for(i in 1:ncol(data)){
    if(length(y <- as.numeric(na.omit(data[,i]))) == 0){
      print(paste0(names(data)[i], ': coluna 0'))
    }else{
      y <- as.numeric(na.omit(data[,i]))
      # by lines
      # names(data[i])
      # y <- as.numeric(data[31,5:ncol(data)])
      hist(y, prob=TRUE, breaks=20, main = "", ylab = "Distribuição dos Dados", xlab = "", lwd = 1, cex = 9)
      curve(dnorm(x, mean(y, na.rm = T), sd(y, na.rm = T)), add=TRUE, col="red", lwd=2)
    }
  }
}

# example
dendidade(dir = "dir",nome = "database.xlsx", colunas = 5:60, pg = 3, ini = 5, fim = 4)
