require(SkewHyperbolic)
#Valentin: testes de secao 2.3.6 em particular de (87)
te2<-function(beta,nu)
{
a<--2*(nu-2)
b<-(-1-sqrt(1 + (8*(beta**2))/(nu-4)))
sqrt(a/b)
}
te<-function(beta,nu)
{
sqrt(((nu-2)*(nu-4)/(4*(beta**2)))*(-1+sqrt(1 + (8*(beta**2))/(nu-4))))
}
te1 <-function(beta,nu)
{
1/sqrt(2*beta/((nu-2)^2*(nu-4))+1/(nu-2))
}
vv <- function(beta,nu,delta) {
2*beta^2*delta^4/((nu-2)^2*(nu-4)) + delta^2/(nu-2)
}



############################################
# DENSIDADE DA GH COM MEDIA = 0 DESVIO = 1 #
############################################

dGH = function(x,beta = 1, nu = 1, log = FALSE, tolerance = .Machine$double.eps^0.5) 
{
    delta0 = sqrt(((nu-2)*(nu-4)/(4*(beta**2)))*(-1+sqrt(1 + (8*(beta**2))/(nu-4))))
    mu0 = -((delta0**2)*beta)/(nu-2)
    mu <- mu0
    delta <- delta0
    if (abs(beta) > tolerance) {
        ldskewhyp <- ((1 - nu)/2) * log(2) + nu * log(delta) + 
            ((nu + 1)/2) * log(abs(beta)) + log(besselK(x = sqrt(beta^2 * 
            (delta^2 + (x - mu)^2)), nu = (nu + 1)/2, expon.scaled = TRUE)) - 
            sqrt(beta^2 * (delta^2 + (x - mu)^2)) + beta * (x - 
            mu) - lgamma(nu/2) - log(sqrt(pi)) - ((nu + 1)/2) *  # aqui usa pi/2
            log(sqrt(delta^2 + (x - mu)^2)) # aqui ele usa a raiz quadrada 
    }
    else {
        ldskewhyp <- lgamma((nu + 1)/2) - log(sqrt(pi)) - log(delta) -# pi/2 
            lgamma(nu/2) - ((nu + 1)/2) * log(1 + ((x - mu)^2)/delta^2)
    }
    if (log == TRUE) 
        return(ldskewhyp)
    else return(exp(ldskewhyp))
}
###############################################################
# DENSIDADE DA GH COM MEDIA = 0 DESVIO = 1 (versao modificada)#
###############################################################

dGHm = function(x,beta = 1, nu = 1, log = FALSE, tolerance = 10^-4) 
{
    a<--2*(nu-2)
    b<-(-1-sqrt(1 + (8*(beta**2))/(nu-4)))
    delta0 =sqrt(a/b)
    mu0 = -((delta0**2)*beta)/(nu-2)
    mu <- mu0
    delta <- delta0
        dGHm_f<-function(x){
            ((1 - nu)/2) * log(2) + nu * log(delta) + 
            ((nu + 1)/2) * log(abs(beta)) + log(besselK(x = sqrt(beta^2 * 
            (delta^2 + (x - mu)^2)), nu = (nu + 1)/2, expon.scaled = TRUE)) - 
            sqrt(beta^2 * (delta^2 + (x - mu)^2)) +beta * (x - 
            mu) - lgamma(nu/2) - log(sqrt(pi)) - ((nu + 1)/2) *  # aqui usa pi/2
            log(sqrt(delta^2 + (x - mu)^2)) # aqui ele usa a raiz quadrada 
        }
        dGHm_g <-function(x){
            lgamma((nu + 1)/2) - log(sqrt(pi)) - log(delta) -# pi/2 
            lgamma(nu/2) - ((nu + 1)/2) * log(1 + ((x - mu)^2)/delta^2)
        }
    #mask <- is.nan(ldskewhyp)
    mask <- abs(beta)*(delta^2 + (x - mu)^2) > tolerance
    res<-c()
    res[mask] <- dGHm_f(x[mask])
    res[!mask] <- dGHm_g(x[!mask]) 
    if (log == TRUE) 
        return(res)
    else return(exp(res))
}

############################################
# GERADOR   DA GH COM MEDIA = 0 DESVIO = 1 #
############################################

rGH = function (n,beta = 1, nu = 20, log = FALSE) 
{
    delta0 = sqrt(((nu-2)*(nu-4)/(4*(beta**2)))*(-1+sqrt(1 + (8*(beta**2))/(nu-4))))
    mu0 = -((delta0**2)*beta)/(nu-2)
    mu <- mu0
    delta <- delta0
    if (log == TRUE) 
        stop("This function is not yet implemented")
    y <- 1/rgamma(n, shape = nu/2, scale = 2/delta^2)
    sigma <- sqrt(y)
    z <- rnorm(n)
    rskewhyp <- mu + beta * sigma^2 + sigma * z
    return(rskewhyp)
}

###########################################################
# FUN??O DE DISTRIBUI??O DA GH COM MEDIA = 0 DESVIO = 1   #
###########################################################

pGH = function (q,beta = 1, nu = 10, log = FALSE, lower.tail = TRUE, small = 10^(-6), 
    tiny = 10^(-10), subdivisions = 100, accuracy = FALSE, ...) 
{

    delta0 = sqrt(((nu-2)*(nu-4)/(4*(beta**2)))*(-1+sqrt(1 + (8*(beta**2))/(nu-4))))
    mu0 = -((delta0**2)*beta)/(nu-2)
    mu <- mu0
    delta <- delta0
    param=c(mu,delta,beta,nu)
    if (log == TRUE) 
        stop("This function is not yet implemented")
    bks <- skewhypBreaks(param = c(mu,delta,beta,nu), small = small, tiny = tiny, 
        ...)
    xTiny <- bks$xTiny
    xSmall <- bks$xSmall
    lowBreak <- bks$lowBreak
    highBreak <- bks$highBreak
    xLarge <- bks$xLarge
    xHuge <- bks$xHuge
    modeDist <- bks$modeDist
    qSort <- sort(q)
    qTiny <- which(qSort < xTiny)
    qSmall <- which(qSort < xSmall)
    qLow <- which(qSort < lowBreak)
    qLessEqMode <- which(qSort <= modeDist)
    qGreatMode <- which(qSort > modeDist)
    qHigh <- which(qSort > highBreak)
    qLarge <- which(qSort > xLarge)
    qHuge <- which(qSort > xHuge)
    if (length(qLow) > 0) 
        qLessEqMode <- qLessEqMode[qLessEqMode > max(qLow)]
    if (length(qHigh) > 0) 
        qGreatMode <- qGreatMode[qGreatMode < min(qHigh)]
    if (length(qSmall) > 0) 
        qLow <- qLow[qLow > max(qSmall)]
    if (length(qLarge) > 0) 
        qHigh <- qHigh[qHigh < min(qLarge)]
    if (length(qTiny) > 0) 
        qSmall <- qSmall[qSmall > max(qTiny)]
    if (length(qHuge) > 0) 
        qLarge <- qLarge[qLarge < min(qHuge)]
    intFun <- rep(NA, length(q))
    if (length(qTiny) > 0) 
        intFun[qTiny] <- 0
    if (length(qHuge) > 0) 
        intFun[qHuge] <- 1
    intErr <- rep(NA, length(q))
    if (length(qTiny) > 0) 
        intErr[qTiny] <- tiny
    if (length(qHuge) > 0) 
        intErr[qHuge] <- tiny
    dskewhypInt <- function(q) dGH(q, beta=beta,nu=nu, log = FALSE)
    resSmall <- safeIntegrate(dskewhypInt, xTiny, xSmall, subdivisions, 
        ...)
    resLarge <- safeIntegrate(dskewhypInt, xLarge, xHuge, subdivisions, 
        ...)
    intSmall <- resSmall$value
    intLarge <- resLarge$value
    errSmall <- tiny + resSmall$abs.error
    errLarge <- tiny + resLarge$abs.error
    resLow <- safeIntegrate(dskewhypInt, xSmall, lowBreak, subdivisions, 
        ...)
    resHigh <- safeIntegrate(dskewhypInt, highBreak, xLarge, 
        subdivisions, ...)
    intLow <- intSmall + resLow$value
    intHigh <- intLarge + resHigh$value
    errLow <- errSmall + resLow$abs.error
    errHigh <- errLarge + resHigh$abs.error
    for (i in qSmall) {
        intRes <- safeIntegrate(dskewhypInt, xTiny, qSort[i], 
            subdivisions, ...)
        intFun[i] <- intRes$value
        intErr[i] <- intRes$abs.error + tiny
    }
    for (i in qLarge) {
        intRes <- safeIntegrate(dskewhypInt, qSort[i], xHuge, 
            subdivisions, ...)
        intFun[i] <- 1 - intRes$value
        intErr[i] <- intRes$abs.error + tiny
    }
    for (i in qLow) {
        intRes <- safeIntegrate(dskewhypInt, xSmall, qSort[i], 
            subdivisions, ...)
        intFun[i] <- intRes$value + intSmall
        intErr[i] <- intRes$abs.error + errSmall
    }
    for (i in qHigh) {
        intRes <- safeIntegrate(dskewhypInt, qSort[i], xLarge, 
            subdivisions, ...)
        intFun[i] <- 1 - intRes$value - intLarge
        intErr[i] <- intRes$abs.error + errLarge
    }
    for (i in qLessEqMode) {
        intRes <- safeIntegrate(dskewhypInt, lowBreak, qSort[i], 
            subdivisions, ...)
        intFun[i] <- intRes$value + intLow
        intErr[i] <- intRes$abs.error + errLow
    }
    for (i in qGreatMode) {
        intRes <- safeIntegrate(dskewhypInt, qSort[i], highBreak, 
            subdivisions, ...)
        intFun[i] <- 1 - intRes$value - intHigh
        intErr[i] <- intRes$abs.error + errLarge
    }
    if (lower.tail) {
        ifelse((accuracy), return(list(value = intFun[rank(q)], 
            error = intErr[rank(q)])), return(intFun[rank(q)]))
    }
    else {
        ifelse((accuracy), return(list(value = 1 - intFun[rank(q)], 
            error = intErr[rank(q)])), return(1 - intFun[rank(q)]))
    }
}

######################################################################
# FUN??O PARA AJUSTAR A GH COM M?DIA 0 E DESVIO 1 (versao modificada)#
######################################################################
GHfitm = function (x) {
    f<-function(beta,nu) {
        sum(dGHm(x,beta,nu,log=T))/length(x)
    }
    ff<-function(v) -f(v[1],v[2])
    method = "L-BFGS-B"
    paramStart=c(5,50)
    ind <- 1:4
    opOut <- optim(paramStart, fn=ff, lower=c(-Inf,4+10^-4), upper=c(Inf,Inf), method=method) 
    par <- as.numeric(opOut[[ind[1]]])[1:2]
    beta = par[1]
    nu = par[2]
    a<--2*(nu-2)
    b<-(-1-sqrt(1 + (8*(beta**2))/(nu-4)))
    delta0 =sqrt(a/b)
    mu0 = -((delta0**2)*beta)/(nu-2)
    mu <- mu0
    delta <- delta0

    param = c(mu,delta,beta,nu)
    names(param) <- c("mu", "delta", "beta", "nu")
    maxLik <- -(as.numeric(opOut[[ind[2]]]))
    conv <- as.numeric(opOut[[ind[4]]])
    iter <- as.numeric(opOut[[ind[3]]])[1]
    fitResults <- list(param = param, maxLik = maxLik, hessian = NULL, 
        method = method, conv = conv, iter = iter, x = x, xName = NULL, 
        paramStart = paramStart, svName = NULL, startValues = NULL, 
        breaks = NULL, midpoints = NULL, empDens = NULL)
    class(fitResults) <- "skewhypFit"
    return(fitResults)
}
###################################################
# FUN??O PARA AJUSTAR A GH COM M?DIA 0 E DESVIO 1 #
###################################################

GHfit = function (x, freq = NULL, breaks = NULL, startValues = "LA", 
    paramStart = NULL, method = "Nelder-Mead", hessian = TRUE, 
    plots = TRUE, printOut = TRUE, controlBFGS = list(maxit = 200), 
    controlNM = list(maxit = 1000), maxitNLM = 1500, ...) 
{
    xName <- paste(deparse(substitute(x), 500), collapse = "\n")
    if (!is.null(freq)) {
        if (length(freq) != length(x)) {
            stop("vectors x and freq are not of the same length")
        }
        x <- rep(x, freq)
    }
    if (startValues == "US") {
        startInfo <- skewhypFitStart(x, breaks = breaks, startValues = startValues, 
            paramStart = paramStart)
    }
    if (startValues == "LA") {
        startInfo <- skewhypFitStart(x, breaks = breaks, startValues = startValues)
    }
    paramStart <- startInfo$paramStart
    paramStart[c(1,2)] = c(0,0)
    svName <- startInfo$svName
    breaks <- startInfo$breaks
    empDens <- startInfo$empDens
    midpoints <- startInfo$midpoints
   llfunc <- function(param) {
         beta=param[1];nu=exp(param[2])
        -sum(dGH(x, beta=beta,nu=nu, log = TRUE), na.rm = TRUE)
    }
#    llfunc <- function(param) {
#         param[2] <- exp(param[2])
#        -sum(dGH(x, param = param, log = TRUE), na.rm = TRUE)
#   }
    output <- numeric(7)
    ind <- 1:4
    if (method == "BFGS") {
        opOut <- optim(paramStart[c(3,4)], llfunc, NULL, method = "BFGS", 
            hessian = hessian, control = controlBFGS, ...)
    }
    if (method == "Nelder-Mead") {
        opOut <- optim(paramStart[c(3,4)], llfunc, NULL, method = "Nelder-Mead", 
            hessian = hessian, control = controlNM, ...)
    }
    if (method == "nlm") {
        ind <- c(2, 1, 5, 4)
        opOut <- nlm(llfunc, paramStart[c(3,4)], hessian = hessian, iterlim = maxitNLM, 
            ...)
    }
    par <- as.numeric(opOut[[ind[1]]])[1:2]
    beta = par[1]
    nu = exp(par[2])
    delta0 = sqrt(((nu-2)*(nu-4)/(4*(beta**2)))*(-1+sqrt(1 + (8*(beta**2))/(nu-4))))
    mu0 = -((delta0**2)*beta)/(nu-2)
    mu <- mu0
    delta <- delta0
    param = c(mu,delta,beta,nu)
    names(param) <- c("mu", "delta", "beta", "nu")
    maxLik <- -(as.numeric(opOut[[ind[2]]]))
    conv <- as.numeric(opOut[[ind[4]]])
    iter <- as.numeric(opOut[[ind[3]]])[1]
    paramStart <- c(paramStart[1], exp(paramStart[2]), paramStart[3], 
        exp(paramStart[4]))
    fitResults <- list(param = param, maxLik = maxLik, hessian = if (hessian) opOut$hessian else NULL, 
        method = method, conv = conv, iter = iter, x = x, xName = xName, 
        paramStart = paramStart, svName = svName, startValues = startValues, 
        breaks = breaks, midpoints = midpoints, empDens = empDens)
    class(fitResults) <- "skewhypFit"
    if (printOut == TRUE) {
        print.skewhypFit(fitResults, ...)
    }
    if (plots == TRUE) {
        plot.skewhypFit(fitResults, ...)
    }
    return(fitResults)
}


###################################
# FUN??O PARA CALCULAR MU E DELTA #
###################################

ver = function(beta = 100,nu=200){
    delta0 = sqrt(((nu-2)*(nu-4)/(4*(beta**2)))*(-1+sqrt(1 + (8*(beta**2))/(nu-4))))
    mu0 = -((delta0**2)*beta)/(nu-2)
    mu <- mu0
    delta <- delta0
    par = c(mu,delta,beta,nu)
    return(par)
}

# 
# 
# # VERIFICANDO 
# 
# x = rGH(300,beta=1,nu=5)
# 
# spec <- ugarchspec(
#   mean.model = list(armaOrder =c(0,0), include.mean = F, arfima = FALSE),
#   variance.model = list(model = "sGARCH", garchOrder = c(0,0)),
#   distribution.model="ghst"
# )
# fit_ghst <- ugarchfit(spec, x, solver="lbfgs")
# fit_ghst <- ugarchfit(spec, x)
# signi_r(fit_ghst)
# 
# spec <- ugarchspec(
#   mean.model = list(armaOrder =c(1,0), include.mean = F, arfima = FALSE),
#   variance.model = list(model = "sGARCH", garchOrder = c(0,0)))
# 
# 
# p=1 
# n <- 400 # Total de observa??es simuladas
# yar <- numeric(n) # Vetor do AR
# rb <- rnorm(n) # Ru?do branco
# c <- 0 # Constante do AR
# nx <- n*0.25 # retirando 25% de obs 
# phi <- c(0.5) # Vetor de par?metros do AR
# # as primeiras observa??es s?o mt ruins por isso a tiramos
# # para n?o corronper as analises 
# 
# for(i in (p+1):n) {
#   psum <- 0
#   for(j in 1:p){
#     psum <- psum + phi[j]*yar[i-j]
#   }
#   yar[i] <- psum + c + rb[i]
# }
# ar<- yar[-1:-nx]
# acf(ar);pacf(ar)
# 
# fit_ghst <- ugarchfit(spec, ar, solver="lbfgs")
# fit_ghst <- ugarchfit(spec, ar)
# signi_r(fit_ghst)
# 
# 
# # para ghskew t
# 
# spec <- arfimaspec(
#   mean.model = list(armaOrder =c(1,0), include.mean = F, arfima = FALSE),
#   distribution.model="ghst")
# 
# x = rGH(1000,beta=9,nu=20)
# p=1 
# n <- 1000 # Total de observa??es simuladas
# yar <- numeric(n) # Vetor do AR
# rb <- x # Ru?do branco
# c <- 0 # Constante do AR
# nx <- n*0.25 # retirando 25% de obs 
# phi <- c(0.5) # Vetor de par?metros do AR
# # as primeiras observa??es s?o mt ruins por isso a tiramos
# # para n?o corronper as analises 
# 
# for(i in (p+1):n) {
#   psum <- 0
#   for(j in 1:p){
#     psum <- psum + phi[j]*yar[i-j]
#   }
#   yar[i] <- psum + c + rb[i]
# }
# ar<- yar[-1:-nx]
# acf(ar);pacf(ar)
# 
# fit_ghst <- arfimafit(spec, ar, solver="lbfgs")
# fit_ghst <- arfimafit(spec, ar)
# signi_r(fit_ghst)
# 
# 
# # Parte ARCH(1)
# 
# x = rGH(1000,beta=9,nu=20)
# rb <- x # Ru?do branco
# r=1 
# c = 0.3 # constante
# alpha = 0.4 # vetor de constantes
# n <- 1000 # Total de observa??es simuladas
# xar <- numeric(n) # Vetor do ARCH
# h <- numeric(n)
# nx <- n*0.25 # retirando 25% de obs 
# # as primeiras observa??es s?o mt ruins por isso a tiramos
# # para n?o corronper as analises 
# 
# for(i in (r+1):n) {
#   psum <- 0
#   for(j in 1:r){
#     psum <- psum + alpha[j]*(xar[i-j])^2
#     h[i] <- c + psum
#   }
#   xar[i] <- sqrt(h[i])*rb[i]
# }
# arch1<- xar[-1:-nx]
# acf(arch1);pacf(arch1)
# 
# spec <- ugarchspec(
#   mean.model = list(armaOrder =c(0,0), include.mean = F, arfima = FALSE),
#   variance.model = list(model = "sGARCH", garchOrder = c(1,0)),
#   distribution.model="ghst")
# 
# fit_ghst <- 0
# fit_ghst <- ugarchfit(spec, arch1, solver="lbfgs")
# fit_ghst <- ugarchfit(spec, ar)
# signi_r(fit_ghst)
# 
# # TESTANDO SE M?DIA = 0 E DESVIO =1
# 
# mean(x); sd(x)
# 
# #AJUSTANDO
# 
# aju = GHfit(x,startValues="LA",method = "Nelder-Mead",plots=TRUE)
# 
# # COLOCANDO OS VERDADEIRO VALORES DOS PAR?METROS
# # E REFAZENDO OS GR?FICOS
# 
# aju$param=ver(beta = 20,nu=120)
# plot.skewhypFit(aju)
# 
# 
# SkewHyperbolic::rskewhyp
# 
# # simular se darmos qualquer parametros 
# # sem  a restricao de media zero e var 1
# 
# n <- 100000
# 
# mu <- 5 ; delta <- 2 ; nu <- 10; beta <- 3
# x <- rskewhyp(n = n, mu = mu, delta = delta, beta = beta, nu = nu)
# a*mean(x);(a**2)*var(x)
# 
# b <- 0 ; a <- 4 
# mu2 <- a*mu + b ; delta2 <- delta*a ; nu2 <- nu ;  beta2 <- beta/a
# y <- rskewhyp(n = n, mu = mu2, delta = delta2, beta = beta2, nu = nu2)
# mean(y);var(y)
