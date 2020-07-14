rm(list=ls())

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

data$ano = strftime(data$Estreia, "%Y", tz = "UTC")
data$ano_venda = strftime(data$`Datas das Vendas`, "%Y", tz = "UTC")

filme_ano = function(data){
  anos = unique(data$ano)
  for(i in 1:length(anos)){
    x = aggregate(data$Filme, by = list(data$`Dias de Vendas após a Estréia`), FUN = length)
    x = x[order(x$x, decreasing = T),]
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

# Estreia: truncar datas em 2017 como minimo e valores NA

print(paste0("Removendo ", dim(data[which(is.na(data$ano) == T),])[1], " do dataset."))
data = data[which(is.na(data$ano) == F),]
print("Removido NA, transformando em numeric: ")
data$ano = as.numeric(data$ano)
print("Inspecionando datas antes de 2017: ")
unique(filter(data, ano < 2017)$Filme)
unique(filter(data, ano < 2017)$`Genero do Filme`)
unique(filter(data, ano < 2017)$ano_venda)
print(paste0("Truncando datas de 2017 até 2020, com remoção de ", dim(filter(data, ano < 2017))[1], " linhas do dataset."))
data = filter(data, ano < 2017)

# Genero do Filme: remover N/A, ?, padronizar e analisar palavras no plural e singular
print("Consequentemente os valores N/A e ? foram removidos da variavel Genero do Filme: ")
unique(data$`Genero do Filme`)
print("Quantidade de linhas com valores faltantes em gênero: ")
dim(dplyr::filter(data, data$`Genero do Filme` %in% c("?", "N/A")))
print("Padronização")
data$`Genero do Filme` = stringi::stri_trans_general(data$`Genero do Filme`, "Latin-ASCII")
data$`Genero do Filme` = toupper(data$`Genero do Filme`)
print(paste0("De 74 gêneros únicos, passou para: ", length(unique(data$`Genero do Filme`))))

# Datas de Venda Após Estréia: recriar datas de venda após estreia
print("Variável: Datas de Vendas Após a Estréia com NA: ")
dplyr::filter(data, data$`Dias de Vendas após a Estréia` == "Falha ao retonar data estreia")
data$dia_estreia_novo = as.Date(strftime(data$`Datas das Vendas`, "%Y/%m/%d")) - as.Date(strftime(data$Estreia, "%Y/%m/%d"))

# Inspecionar valores 0 em Tickets vendidos Pela Ingresso.com
print(paste0("Valor de bilheteria da ingressos.com dos filmes listados acima: ", "Suposição de falhas técnicas."))
filter(data, data$`Tickets vendidos Pela Ingresso.com` == 0)

# Inspecionar valores 0 em Tickets vendidos pelo Cinema
print(paste0("Valor de bilheteria dos cinemas dos filmes listados acima: ", "Suposição de falhas técnicas."))
filter(data, data$`Tickets vendidos pelo Cinema` == 0)

# Inspecionar valores 0 em Tickets vendidos Pela Ingresso.com e Tickets vendidos pelo Cinema
filter(data, data$`Tickets vendidos pelo Cinema` == 0 & data$`Tickets vendidos Pela Ingresso.com` == 0)




