########################################################################
# Calculating the sample size per sample stratified by proportional pond
########################################################################

rm(list = ls()) # clean memory

# example

dados <- data.frame(nome = c("us", "un", "rs", "rn"), Wh = c(54878,34848,15585,7488)) 

# size of the sample to estimate

#' @param data dataframe
#' @param e error
#' @param h cluster
#' @param ph probability of the cluster

tam1 <- function(data = dados, e = 0.01, h = 4, ph = 0.5){
  aux <- NULL
  for(i in 1:nrow(data)){
    aux[i] <- round((((data[i,2]/sum(data[,2]))*ph*(1-ph))/(e^2)) + 1)
  }
  return(aux)
}

tam1()

# sample size to estimate a mean

#' @param data dataframe
#' @param e error
#' @param sigmah variance of the cluster

tam2 <- function(data = dados, sigmah = rep(0.5,nrow(dados)), e = 0.01){
  aux <- NULL
  for(i in 1:nrow(data)){
    aux[i] <- round((((data[i,2]/sum(data[,2]))*sigmah[i]^2)/(e^2)) + 1)
  }
  return(aux)
}

tam2()

# estimate the samples from the total sample to be obtained

#' @param n length of the sample
#' @param values values for sample

tam3 <- function(n = 3000, values = c(54878,34848,15585,7488)){
  N = sum(values)
  f = n / N
  amostras <- NULL
  for (i in 1:length(values)) {
    amostras[i] <- round(f*values[i])
  }
  return(amostras)
}

tam_amostra3()

######################################################################
# end
######################################################################