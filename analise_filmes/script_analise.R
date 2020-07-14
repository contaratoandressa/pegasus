# packages
install.load::install_load("readxl", "knitr", "tidyr", "dplyr", "ggplot2", "plotly", "e1071","plyr")

# dataset
data <- read_excel("estudo_de_caso_-_cientista_de_dados.xlsx")

# analise descritiva
dim(data)
str(data)
summary(data[,c("Tickets vendidos Pela Ingresso.com", "Tickets vendidos pelo Cinema")])

print("Valores NA na base de dados: ")
apply(data,2,function(x){table(is.na(x))})

print("Período inicial de tempo dos dados de Datas das Vendas e Estreia respectivamente: ")
apply(data[,c("Datas das Vendas", "Estreia")], 2, function(x){min(x, na.rm = T)})
print("Período final de tempo dos dados de Datas das Vendas e Estreia respectivamente: ")
apply(data[,c("Datas das Vendas", "Estreia")], 2, function(x){max(x, na.rm = T)})

freq = function(x){
  #return(prop.table(table(x)))
  return(length(unique(x)))
}
print("Quantidade de classes das variáveis: ") 
apply(data[,c("Filme","Genero do Filme","Dias de Vendas após a Estréia")], 2, freq)

print("Moda de: ")
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
apply(data[,c("Filme","Genero do Filme","Dias de Vendas após a Estréia")], 2, getmode)

print("Filmes com grande sucesso de bilheteria na ingresso.com e nos cinemas, respctivamente: ")
unique(filter(data, data$`Tickets vendidos Pela Ingresso.com` == max(data$`Tickets vendidos Pela Ingresso.com`))$Filme)
unique(filter(data, data$`Tickets vendidos pelo Cinema` == max(data$`Tickets vendidos pelo Cinema`))$Filme)

print("Filmes com grande sucesso de bilheteria na ingresso.com e nos cinemas, respctivamente: ")
unique(filter(data, data$`Tickets vendidos Pela Ingresso.com` == min(data$`Tickets vendidos Pela Ingresso.com`))$Filme)
unique(filter(data, data$`Tickets vendidos pelo Cinema` == min(data$`Tickets vendidos pelo Cinema`))$Filme)

x = aggregate(data$`Dias de Vendas após a Estréia`, by = list(data$Filme), FUN = length)
x = x[order(x$x, decreasing = T),]
barplot(x$x, col = "blue", main = "Distibuição dos dados por Dias de Vendas após a Estréia", horiz = F, names.arg = x$Group.1, cex.names = 1)
print("Top filmes com mais compra de ingressos: ")
x[0:5,]
print("Top 5 filmes com menos compra de ingressos: ")
x[(nrow(x)-4):(nrow(x)),]

x = aggregate(data$`Dias de Vendas após a Estréia`, by = list(data$`Genero do Filme`), FUN = length)
x = x[order(x$x, decreasing = T),]
barplot(x$x, col = "blue", main = "Distibuição dos dados por Dias de Vendas após a Estréia", horiz = F, names.arg = x$Group.1, cex.names = 1)
print("Top 5 gêneros de filme com mais compra de ingressos: ")
x[0:5,]
print("Top 5 gêneros de filme com menos compra de ingressos: ")
x[(nrow(x)-4):(nrow(x)),]

x = aggregate(data$Filme, by = list(data$`Dias de Vendas após a Estréia`), FUN = length)
x = x[order(x$x, decreasing = T),]
barplot(x$x, col = "blue", main = "Distibuição dos dados por Dias de Vendas após a Estréia", xlim = c(0,140), names.arg = x$Group.1, cex.names = 1)
print("Top 5 dias com mais compra de ingressos: ")
x[0:5,]
print("Top 5 dias com menos compra de ingressos: ")
x[(nrow(x)-4):(nrow(x)),]

filme_ano = function(data){
  anos = unique(data$ano)
  for(i in 1:length(anos)){
    x = aggregate(data$Filme, by = list(data$`Dias de Vendas após a Estréia`), FUN = length)
    x = x[order(x$x, decreasing = T),]
    data$ano = strftime(data$Estreia, "%Y")
    print(paste0("Top 5 dias com mais compra de ingressos no ano de : ", anos[i]))
    print(x[0:5,])
    print(paste0("Top 5 dias com menos compra de ingressos no ano de : ", anos[i]))
    print(x[(nrow(x)-4):(nrow(x)),])
  }
}

filme_ano(data)

boxplot(data[,c("Tickets vendidos Pela Ingresso.com", "Tickets vendidos pelo Cinema")])

qt_um = function(x){
  x = unique(x)
  for(i in 1:length(x)){

    if(nchar(x[i]) <= 1){
      print(x[i])
    }  
  }
}
print(paste0("Filmes com nomes menores ou iguais a um caracter: ", apply(data[,"Filme"], 2, qt_um)))

# etl

# Estreia: truncar datas em 2017 como minimo

# Genero do Filme: remover N/A, ?, padronizar e analisar palavras no plural e singular

# Inspecionar valores 0 em Tickets vendidos Pela Ingresso.com

# Inspecionar valores 0 em Tickets vendidos pelo Cinema

# Inspecionar valores 0 em Tickets vendidos Pela Ingresso.com e Tickets vendidos pelo Cinema

# Datas de Venda Após Estréia: recriar datas de venda após estreia



knitr::kable(dplyr::filter(data, data$`Genero do Filme` %in% c("?", "N/A")))
print("Quantidade de linhas com valores faltantes em gênero: ")
dim(dplyr::filter(data, data$`Genero do Filme` %in% c("?", "N/A")))
print("Filmes com gêneros faltantes: ")
unique(dplyr::filter(data, data$`Genero do Filme` %in% c("?", "N/A"))$Filme)
data = dplyr::filter(data, !data$`Genero do Filme` %in% c("?", "N/A"))
print("Dataset sem valores faltantes em Gênero do Filme")
dim(data)
print(paste0("Retirada de: ", round(1-91050/91200, 4)*100 , "% da base"))
data$`Genero do Filme` = stringi::stri_trans_general(data$`Genero do Filme`, "Latin-ASCII")
data$`Genero do Filme` = toupper(data$`Genero do Filme`)
