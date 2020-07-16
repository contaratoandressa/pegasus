rm(list=ls())

# packages
install.load::install_load("readxl", "knitr", "tidyr", "dplyr", "ggplot2", "plotly", 
                           "e1071","plyr", "magick","corrplot", "Hmisc","PerformanceAnalytics", "lmtest","sandwich",
                           "PortfolioAnalytics", "quantmod", "nnet", "caret", "zoo","writexl","tseries", "moments",
                           "forecast", "polynom", "rugarch", "xts","SkewHyperbolic") #rgl

source("HGbib_atz2.R")
source("ajud2.R")

# dataset
data <- read_excel("estudo_de_caso_-_cientista_de_dados.xlsx")

# analise descritiva
dim(data)
str(data)
summary(data[,c("Tickets vendidos Pela Ingresso.com", "Tickets vendidos pelo Cinema")])

print("Filmes com grande sucesso de bilheteria na ingresso.com e nos cinemas, respctivamente: ")
unique(filter(data, data$`Tickets vendidos Pela Ingresso.com` == max(data$`Tickets vendidos Pela Ingresso.com`))$Filme)
unique(filter(data, data$`Tickets vendidos pelo Cinema` == max(data$`Tickets vendidos pelo Cinema`))$Filme)

print("Filmes com grande sucesso de bilheteria na ingresso.com e nos cinemas, respctivamente: ")
unique(filter(data, data$`Tickets vendidos Pela Ingresso.com` == min(data$`Tickets vendidos Pela Ingresso.com`))$Filme)
unique(filter(data, data$`Tickets vendidos pelo Cinema` == min(data$`Tickets vendidos pelo Cinema`))$Filme)

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

x = aggregate(data$`Datas das Vendas`, by = list(data$Filme), FUN = length)
x = x[order(x$x, decreasing = T),]
names(x) = c("Filme", "Quantidades de dias")
barplot(x$x, col = "blue", main = "Distibuição dos dados por Dias de Vendas após a Estréia", horiz = F, names.arg = x$Group.1, cex.names = 1)
print("Top filmes com mais compra de ingressos: ")
x[0:5,]
print("Top 5 filmes com menos compra de ingressos: ")
x[(nrow(x)-4):(nrow(x)),]

x = aggregate(data$`Datas das Vendas`, by = list(data$`Genero do Filme`), FUN = length)
x = x[order(x$x, decreasing = T),]
names(x) = c("Genero", "Quantidades de dias")
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
data$ano_venda = as.numeric(data$ano_venda)
print("Inspecionando datas antes de 2017: ")
unique(filter(data, ano < 2017)$Filme)
unique(filter(data, ano < 2017)$`Genero do Filme`)
unique(filter(data, ano < 2017)$ano)
print(paste0("Truncando datas de 2017 até 2020, com remoção de ", dim(filter(data, ano < 2017))[1], " linhas do dataset."))
data = filter(data, ano >= 2017)

# Genero do Filme: remover N/A, ?, padronizar e analisar palavras no plural e singular
print("Consequentemente os valores N/A e ? foram removidos da variavel Genero do Filme: ")
unique(data$`Genero do Filme`)
print("Quantidade de linhas com valores faltantes em gênero: ")
dim(dplyr::filter(data, data$`Genero do Filme` %in% c("?", "N/A")))
data = dplyr::filter(data, !data$`Genero do Filme` %in% c("?", "N/A"))
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
print("Suposição: pode ter ocorrido falha técnica neste dia, mas não será removido, mas sim inspecionado futuramente para maiores insights")

print("estatisticas resumo da base tratada: ")
dim(data)
str(data)
summary(data[,c(6,7)])

# criação da variável: americano = 1 se o filme é estrangeiro, 0 c.c.
americano = read.csv("americano.csv")
names(americano)[1] = names(data)[1] 
data = left_join(data, americano, by = "Filme")
data[which(is.na(data$Americano) == T), 11] = 0

filmes_americanos <- image_read('filmes_americanos.png')
print(filmes_americanos)

# modelo
# data$new_data = strftime(data$Estreia, "%Y/%m")
data$new_data = strftime(data$Estreia, "%Y")
# data1 = aggregate(data$`Tickets vendidos Pela Ingresso.com`, by = list(data = data$new_data, genero = data$`Genero do Filme`, americano = data$Americano), FUN = sum)
# data2 = aggregate(data$`Tickets vendidos pelo Cinema`, by = list(data = data$new_data, genero = data$`Genero do Filme`, americano = data$Americano), FUN = sum)
data1 = aggregate(data$`Tickets vendidos Pela Ingresso.com`, by = list(data = data$new_data, genero = data$`Genero do Filme`), FUN = sum)
data2 = aggregate(data$`Tickets vendidos pelo Cinema`, by = list(data = data$new_data, genero = data$`Genero do Filme`), FUN = sum)

data1$ID = paste0(data1$data," ", data1$genero)
data2$ID = paste0(data2$data," ", data2$genero)
data = left_join(data1, data2, by = "ID")
# data = data[,c(1,2,3,4,9)]
data = data[,c(1,2,3,7)]
# names(data) = c("data","genero","americano","ingresso","cinema")
names(data) = c("data","genero","ingresso","cinema")
data$data = as.numeric(data$data)

google_trends = read.csv("google_trends2.csv")
google_trends$data = as.numeric(substr(google_trends$ano_mes, 1, 4))
google_trends = aggregate(google_trends$volume, by = list(data = google_trends$data), FUN = sum)
data = left_join(data, google_trends, by = "data")

ipea = read_excel("ipeadata[14-07-2020-09-52].xls")
names(ipea) = c("data", "evn")
ipea$data = as.numeric(ipea$data)
data = left_join(data, ipea, by = "data")
# data$ano = substr(data$data, 1, 4)

# data$evn = rep(0, nrow(data))
# anos = sort(as.numeric(unique(data$ano)))[-length(anos)]
# for (i in 1:length(anos)) {
#   data[which(data$ano == anos[i]), "evn"] = rep(dplyr::filter(ipea, data == anos[i])$evn, nrow(dplyr::filter(data, ano == anos[i])))
# }

# names(data) = c("data","genero","americano","ingresso","cinema","google_trends","ano_estreia","evn")
names(data) = c("data","genero","ingresso","cinema","google_trends","evn")
data$google_trends = ifelse(is.na(data$google_trends),0,data$google_trends)
data$evn = ifelse(is.na(data$evn),0,data$evn)
data$evn = ifelse(data$evn == 0,mean(data$evn),data$evn)
data$prop_cinema = round(data$cinema/(data$cinema+data$ingresso),3)

print("Resumo dos dados para aplicar o modelo: ")
names(data)
dim(data)
str(data)
summary(data[,c(3,4,5,6)])
apply(data[,c(1,2)], 2, getmode)

res = cor(data[,c("ingresso", "cinema", "google_trends", "evn")])
corrplot(res, type = "upper", order = "hclust", tl.col = "black", tl.srt = 50)
res2 <- rcorr(as.matrix(data[,c("ingresso", "cinema", "google_trends", "evn")]))
corrplot(res2$r, type="upper", order="hclust", p.mat = res2$P, sig.level = 0.01, insig = "blank")
corrplot(res2$r, type="upper", order="hclust", p.mat = res2$P, sig.level = 0.01, insig = "blank")
chart.Correlation(data[,c("ingresso", "cinema", "google_trends", "evn")], histogram=TRUE, pch=19)

write.csv2(data,"/home/andressa/dev/workspace/pegasus/analise_filmes/data_model.csv")

# nao deu certo com americano, removi logo
# mod1 = lm(ingresso ~ genero + americano + evn + google_trends + prop_cinema, data = data)
# summary(mod1)
# AIC(mod1)

set.seed(123)  # setting seed to reproduce results of random sampling
train_sample <- sample(1:nrow(data), 0.7*nrow(data))  #  training and testing: 70/30 split
train <- data[train_sample, ]  # training data
test  <- data[-train_sample, ]   # test data

# R2: 0.9387, AIC: 2715.47
mod1 = lm(ingresso ~ evn + google_trends + prop_cinema + cinema + genero, data = train)
summary(mod1)
AIC(mod1)

# R2: 0.6023 AIC: 2913.605
mod1 = lm(ingresso ~ evn + google_trends + prop_cinema + genero, data = train)
summary(mod1)
AIC(mod1)

# Diagnostico residuos

print("Teste de Normalidade dos resíduos")
shapiro.test(residuals(mod1))

print("Resíduos vs values ajustados")
plot(mod1, pch=16, col="blue", lty=1, lwd=2, which=1)

print("Teste de normalidade dos resíduos")
plot(mod1, pch=16, col="blue", lty=1, lwd=2, which=2)

print("Pontos outliers")
plot(mod1, pch=16, col="blue", lty=1, lwd=2, which=3)

print("Distância de COKK-pontos influentes")
plot(mod1, pch=16, col="blue", lty=1, lwd=2, which=5)
plot(mod1, pch=16, col="blue", lty=1, lwd=2, which=4)


# no diagnostico dos residus mostrou heterocedasticidade
# R2: 0.8918 AIC: 470.58
mod1 = lm(log(ingresso) ~ evn + google_trends + prop_cinema + genero, data = train)
summary(mod1)
AIC(mod1)

# R2: 0.8609 AIC:416.0187
train = dplyr::filter(train, !genero %in% c("CLASSICO","COMEDIA E TERROR","COMEDIA ROMANTICA","CURTA","LIVRE","ROCK","THRILLER"))
mod1 = lm(log(ingresso) ~ evn + google_trends + prop_cinema +genero, data = train)
summary(mod1)
AIC(mod1)

# Diagnostico residuos

print("Teste de Normalidade dos resíduos")
shapiro.test(residuals(mod1))

print("Resíduos vs values ajustados")
plot(mod1, pch=16, col="blue", lty=1, lwd=2, which=1)

print("Teste de normalidade dos resíduos")
plot(mod1, pch=16, col="blue", lty=1, lwd=2, which=2)

print("Pontos outliers")
plot(mod1, pch=16, col="blue", lty=1, lwd=2, which=3)

print("Distância de COKK-pontos influentes")
plot(mod1, pch=16, col="blue", lty=1, lwd=2, which=5)
plot(mod1, pch=16, col="blue", lty=1, lwd=2, which=4)

# forecast

# 1. Add predictions 
pred.int <- predict(mod1, interval = "prediction")
mydata <- cbind(data, pred.int)
# 2. Regression line + confidence intervals
p <- ggplot(mydata, aes(ingresso, google_trends)) +
  geom_point() +
  stat_smooth(method = lm)
# 3. Add prediction intervals
p + geom_line(aes(y = lwr), color = "red", linetype = "dashed")+
  geom_line(aes(y = upr), color = "red", linetype = "dashed")

predict(mod1, interval = "prediction")
predict(mod1)

predict <- exp(predict(mod1)[0:nrow(test)])  # predicted values


# TS
garch_data <- function(data, percent_train, var){
  
  # Daily Seasonality (frequency set to 1 for daily data)
  msft <- ts(na.omit(data[,var]),frequency=1)
  # Delimit training range
  msft.train <- window(msft,end=c(round(length(msft)*percent_train),1)) # pegando 70% dos dados para treino
  # Delimit testing range
  msft.test <- window(msft,start=c((round(length(msft)*percent_train)+1),1))
  
  modelauto <- auto.arima((msft.train), seasonal=FALSE)
  m = Arima(msft.train,model=modelauto) # modelo escolhido do arima
  z = msft.train # serie que estamos trabalhando
  dlz = diff(msft.train) # primeira diferenca
  n = length(msft.train) # tamanho do vetor de treino
  res = residuals(m) # residuos do modelo
  previsao = msft.test
  # forecast(modelauto,100)
  
  spec <- ugarchspec( # construindo modelo ghskew
    mean.model = list(armaOrder =c(0,0), include.mean = F, arfima = FALSE),
    variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
    distribution.model="ghst" # ghyp
  )
  
  fit <- ugarchfit(spec, dlz) # modelando com garch com a distribuição ghskew
  res <- sres1(fit)
  
  mode <- ugarchroll(spec, diff(c(z,previsao)), forecast.length = 316, refit.every = 315)
  
  media <- mode@forecast[[2]][[1]][[1]]
  desviopadrao <- mode@forecast[[2]][[1]][[2]]
  
  m <- length(previsao)
  k <- length(media) # poderia ser de sigma tb
  
  #parame <- GHfit(res, plot=F)
  
  parame <- GHfitm(res)
  forc = ugarchforecast(fit, n.ahead=18)
  
  mu.predict <- fitted(forc) # extract predicted X_t (= conditional mean mu_t; note: E[Z] = 0)
  sig.predict <- sigma(forc) # extract predicted sigma_t
  VaR.predict <- as.numeric(quantile(forc, probs = 0.01))
  nu. <- fit@fit$coef[["shape"]]
  
  stopifnot(all.equal(mu.predict, forc@forecast$seriesFor, check.attributes = FALSE),
            all.equal(sig.predict, forc@forecast$sigmaFor, check.attributes = FALSE))
  
  VaR.predict. <- abs(as.numeric(mu.predict + sig.predict * sqrt((nu.-2)/nu.) * qt(0.01, df = nu.))) 
  
  return(round(sum(VaR.predict.[7:18])))
}

google_trends = read.csv("google_trends2.csv")
ipea = read_excel("ipeadata[14-07-2020-09-52].xls")
names(ipea) = c("data","evn")
ipea$data = as.numeric(ipea$data)

garch_data(data = google_trends, percent_train = 0.7, var = "volume")
garch_data(data = ipea, percent_train = 0.7, var = "evn")


write.csv2(cbind(test,predict),"/home/andressa/dev/workspace/pegasus/analise_filmes/teste.csv")
write.csv2(train,"/home/andressa/dev/workspace/pegasus/analise_filmes/treino.csv")
write.csv2(data.frame(exp(predict(mod1))),"/home/andressa/dev/workspace/pegasus/analise_filmes/predicao.csv")
