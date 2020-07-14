# packages

install.load::install_load("readxl", "knitr", "tidyr", "dplyr", "ggplot2", "plotly", "e1071","plyr")

# dataset
data <- read_excel("estudo_de_caso_-_cientista_de_dados.xlsx")

# analise descritiva

dim(data)
str(data)
summary(data[,c("Tickets vendidos Pela Ingresso.com", "Tickets vendidos pelo Cinema")])

print(paste0("Período inicial de tempo dos dados de Datas das Vendas e Estreia respectivamente: ", apply(data[,c("Datas das Vendas", "Estreia")], 2, min)))
print(paste0("Período final de tempo dos dados de Datas das Vendas e Estreia respectivamente: ", apply(data[,c("Datas das Vendas", "Estreia")], 2, max)))

print("Valores NA na base de dados: ")
apply(data,2,function(x){table(is.na(x))})

freq = function(x){
  #return(prop.table(table(x)))
  return(length(unique(x)))
}
print("Quantidade de classes das variáveis Filme") 
apply(data[,c("Filme","Genero do Filme","Dias de Vendas após a Estréia")], 2, freq)

x = aggregate(data$Filme, by = list(data$`Dias de Vendas após a Estréia`), FUN = length)
x = x[order(x$x, decreasing = T),]
barplot(x$x, col = "blue", main = "Distibuição dos dados por Dias de Vendas após a Estréia", xlim = c(0,140), names.arg = x$Group.1, cex.names = 1)

print("Top 5 dias com mais compra de ingressos: ")
x[0:5,]
print("Top 5 dias com menos compra de ingressos: ")
x[(nrow(x)-4):(nrow(x)),]

boxplot(data[,c("Tickets vendidos Pela Ingresso.com", "Tickets vendidos pelo Cinema")])


print("Variável: Filme")
print(paste0("Quantidade de Valores faltantes: ", table(is.na(data$Filme))[1] - dim(data)[1]))
#qt_um = function(x){
#  x = unique(x)
#  for(i in 1:length(x)){

#    if(nchar(x[i]) <= 1){
#      print(x[i])
#    }  
#  }
#}
# apply(data[,"Filme"], 2, print)
print(paste0("Filmes com nomes menores ou iguais a um caracter: ", 0))
