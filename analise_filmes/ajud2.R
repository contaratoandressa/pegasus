require(boot)
require(forecast)
require(polynom)
require(tseries)
Ljung_Box <- function(fit, df) {
    u <- c()
    for (i in c(12, 24, 36, 48)) {
        LB <- Box.test(resid(fit), i,
                       type = "Ljung-Box", fitdf=df)
        u <- c(u, i, LB$statistic, LB$parameter, LB$p.value)
    }
    u <- matrix(u, ncol=4, byrow=T)
    colnames(u)<- c("Lag", "$\\chi^2$", "GdL", "P-valor")
    rownames(u)<-c()
    u
}
signi <- function(fit) {
    DesPad <- rep(NA, length(coef(fit)))
    u <- cbind(
               coef(fit),
               {DesPad[fit$mask] <- sqrt(diag(vcov(fit))); DesPad},
               coef(fit)/DesPad,
               2 * pnorm(-abs(coef(fit)/DesPad))
               )
    colnames(u)<- c("Coef", "DesPad", "T", "P")
    rownames(u)<-names(coef(fit))
    u
}
plotap <- function(fit) {
    layout(matrix(c(1:2),nrow=1))
    acf(resid(fit))
    pacf(resid(fit))
}
Ljung_Box2 <- function(fit, df) {
    u <- c()
    for (i in c(10, 20, 30, 40)) {
        LB <- Box.test((resid(fit))^2, i,
                       type = "Ljung-Box", fitdf=df)
        u <- c(u, i, LB$statistic, LB$parameter, LB$p.value)
    }
    u <- matrix(u, ncol=4, byrow=T)
    colnames(u)<- c("Lag", "$\\chi^2$", "GdL", "P-valor")
    rownames(u)<-c()
    u
}

require(rugarch)
sres <- function(fit) {
    fit@fit$residuals/fit@fit$sigma
}
Ljung_Box_r <- function(fit, df) {
    u <- c()
    for (i in c(10, 20, 30, 40)) {
        LB <- Box.test(sres(fit), i,
                       type = "Ljung-Box", fitdf=df)
        u <- c(u, i, LB$statistic, LB$parameter, LB$p.value)
    }
    u <- matrix(u, ncol=4, byrow=T)
    colnames(u)<- c("Lag", "$\\chi^2$", "GdL", "P-valor")
    rownames(u)<-c()
    u
}
Ljung_Box2_r <- function(fit, df) {
    u <- c()
    for (i in c(10, 20, 30, 40)) {
        LB <- Box.test((sres(fit))^2, i,
                       type = "Ljung-Box", fitdf=df)
        u <- c(u, i, LB$statistic, LB$parameter, LB$p.value)
    }
    u <- matrix(u, ncol=4, byrow=T)
    colnames(u)<- c("Lag", "$\\chi^2$", "GdL", "P-valor")
    rownames(u)<-c()
    u
}
signi_r <- function(fit) {
    print(round(fit@fit$matcoef,6), digits = 5)
}

doBdsTest <- function(data, R = 2500, parallel = "no", ncores = NULL){#[[[
  
  # parte principal
  m <- 5
  sta <- function(data, i) {
    bds.test(data[i], m)$statistic
  }       
  u <- boot(data, sta, R, parallel = parallel, ncpus = ncores)
  f <- function(index) {
    length(which(abs(u$t[,index]) > abs(u$t0[index])))/R
  }       
  l <- bds.test(data, m)
  r.names <- rownames(l$p.value)
  c.names <- colnames(l$p.value)    
  PVAL <- matrix(sapply(1:(4*(m-1)),f), ncol=4)
  rownames(PVAL) <- r.names
  colnames(PVAL) <- c.names 
  structure(list(statistic = l$statistic,
                 p.value = PVAL,
                 method = l$metdod,
                 data.name = l$data.name,
                 parameter = l$parameter),
            class = "bdstest")
}#]]]           
passedBds <- function(bds, p) {
  all(as.vector(bds$p.value) >= p)
}

################ EU ###############

sres1 <- function(fit) {
  fit@fit$residuals
}

# Teste de Box Pierce 
# verifica??o de autocorrela??o no modelo

# H0: h? autocorrel?a? no modelo
# H1: n?o h? autocorrela??o no modelo

Ljung_Box_r_arfima <- function(fit, df) {
  u <- c()
  for (i in c(10, 20, 30, 40)) {
    LB <- Box.test(sres1(fit), i,
                   type = "Ljung-Box", fitdf=df)
    u <- c(u, i, LB$statistic, LB$parameter, LB$p.value)
  }
  u <- matrix(u, ncol=4, byrow=T)
  colnames(u)<- c("Lag", "$\\chi^2$", "GdL", "P-valor")
  rownames(u)<-c()
  u
}
