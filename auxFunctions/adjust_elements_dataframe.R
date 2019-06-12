#####################################################################
# function that reads a dataframe and adjusts values in factor format
#####################################################################

rm(list = ls())

# example

dataset <- data.frame(x1 = as.factor(c('teste','Teste0','0TESTE')), x2 = c(1,2,3))

# function

#' @param data dataframe
#' @param col specific column
#' @param name name for change
#' @return what the function returns

adjust_dataframe <- function(data = dataset, col = 'x1', name = 'teste'){
  data[col] <- as.vector(apply(data[col],2,as.character))
  data[,col] <- toupper(chartr("ÁÉÍÓÚÀÈÌÒÙÃÕÂÊÔÇáéíóúàèìòùãõâêôç","AEIOUAEIOUAOAEOCaeiouaeiouaoaeoc",data[,col]))
  data[col][grep("[^:digit:]", data[col])] <- name 
  return(data)
}

adjust_dataframe()

#####################################################################
# end
#####################################################################
