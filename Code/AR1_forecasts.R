library(xtable)
coeffs <- t(matrix(c(0.085, 2.215, -0.027, 1.489, 0.402, 0.978, 0.069, 0.162),ncol=2))
horizons <- c(1,3,6,9,12)
slopes <- matrix(rep(NA,5*4),ncol=4)
intercepts <- slopes
fcs <- slopes
for (i in 1:5)
{
  slopes[i,] <- coeffs[2,]^horizons[i]
  tmp <- matrix(rep(0,4),ncol=4)
  for (j in 0:(horizons[i]-1))
  {
    tmp <- tmp + coeffs[2,]^j
  }
  intercepts[i,] <- coeffs[1,]*tmp
}

for (i in 1:5)
{
  for (j in 1:4)
  {
    fcs[i,j] <- paste("$E(X_{T+",horizons[i],"}|X_T) = ",round(intercepts[i,j],3)," + ",round(slopes[i,j],3),"X_T$",sep="")
  }
}

EXs <- coeffs[1,]/(1-coeffs[2,])
EXs <- paste("$E(X_{T+h}) =",round(EXs,3),"$",sep="")
fcs <- rbind(fcs,EXs)
rownames(fcs) <- c("h = 1","h = 3","h = 6","h = 9","h = 12","h = $\\infty$")
colnames(fcs) <- c("CPI", "PPI", "$\\Delta$ IP", "EP")
fcs.price <- fcs[,c(1,2)]
fcs.real <- fcs[,c(3,4)]
Latex.table.price <- xtable(fcs.price)
print(Latex.table.price,file="../../../Paper/Manuscript/AR1forecasts.price.tex",sanitize.text.function=function(x){x})
Latex.table.real <- xtable(fcs.real)
print(Latex.table.real,file="../../../Paper/Manuscript/AR1forecasts.real.tex",sanitize.text.function=function(x){x})
