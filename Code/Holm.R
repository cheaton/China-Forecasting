library(matrixcalc)
p.c <- read.csv(file="../Output/pvalue_tables/CPI_MSE_levels_conditional.pvalues.csv",header=TRUE,row.names=1)[,c(1,3,6,9,12)]
p.u <- read.csv(file="../Output/pvalue_tables/CPI_MSE_levels_unconditional.pvalues.csv",header=TRUE,row.names=1)[,c(1,3,6,9,12)]

pvalues.c <- vec(as.matrix(p.c))
pvalues.u <- vec(as.matrix(p.u))

p.holm.c <- matrix(p.adjust(pvalues.c,method="holm"),ncol=length(p.c))
rownames(p.holm.c) <- rownames(p.c)
colnames(p.holm.c) <- c("h=1","h=3","h=6","h=9","h=12")

p.holm.u <- matrix(p.adjust(pvalues.u,method="holm"),ncol=length(p.u))
rownames(p.holm.u) <- rownames(p.u)
colnames(p.holm.u) <- c("h=1","h=3","h=6","h=9","h=12")

cat("CPI conditional rejections at FWER=0.05\n\n")
print(p.holm.c<0.05)
cat("\n\nCPI conditional rejections at FWER=0.01\n\n")
print(p.holm.c<0.05)

cat("CPI unconditional rejections at FWER=0.05\n\n")
print(p.holm.u<0.05)
cat("\n\nCPI unconditional rejections at FWER=0.01\n\n")
print(p.holm.u<0.01)




p.c <- read.csv(file="../Output/pvalue_tables/IP_MSE_differences_conditional.pvalues.csv",header=TRUE,row.names=1)[,c(1,3,6,9,12)]
p.u <- read.csv(file="../Output/pvalue_tables/IP_MSE_differences_unconditional.pvalues.csv",header=TRUE,row.names=1)[,c(1,3,6,9,12)]

pvalues.c <- vec(as.matrix(p.c))
pvalues.u <- vec(as.matrix(p.u))

p.holm.c <- matrix(p.adjust(pvalues.c,method="holm"),ncol=length(p.c))
rownames(p.holm.c) <- rownames(p.c)
colnames(p.holm.c) <- c("h=1","h=3","h=6","h=9","h=12")

p.holm.u <- matrix(p.adjust(pvalues.u,method="holm"),ncol=length(p.u))
rownames(p.holm.u) <- rownames(p.u)
colnames(p.holm.u) <- c("h=1","h=3","h=6","h=9","h=12")

cat("IP conditional rejections at FWER=0.05\n\n")
print(p.holm.c<0.05)
cat("\n\nIP conditional rejections at FWER=0.01\n\n")
print(p.holm.c<0.05)

cat("IP unconditional rejections at FWER=0.05\n\n")
print(p.holm.u<0.05)
cat("\n\nIP unconditional rejections at FWER=0.01\n\n")
print(p.holm.u<0.01)






p.c <- read.csv(file="../Output/pvalue_tables/EP_MSE_levels_conditional.pvalues.csv",header=TRUE,row.names=1)[,c(1,3,6,9,12)]
p.u <- read.csv(file="../Output/pvalue_tables/EP_MSE_levels_unconditional.pvalues.csv",header=TRUE,row.names=1)[,c(1,3,6,9,12)]

pvalues.c <- vec(as.matrix(p.c))
pvalues.u <- vec(as.matrix(p.u))

p.holm.c <- matrix(p.adjust(pvalues.c,method="holm"),ncol=length(p.c))
rownames(p.holm.c) <- rownames(p.c)
colnames(p.holm.c) <- c("h=1","h=3","h=6","h=9","h=12")

p.holm.u <- matrix(p.adjust(pvalues.u,method="holm"),ncol=length(p.u))
rownames(p.holm.u) <- rownames(p.u)
colnames(p.holm.u) <- c("h=1","h=3","h=6","h=9","h=12")

cat("EP conditional rejections at FWER=0.05\n\n")
print(p.holm.c<0.05)
cat("\n\nEP conditional rejections at FWER=0.01\n\n")
print(p.holm.c<0.05)

cat("EP unconditional rejections at FWER=0.05\n\n")
print(p.holm.u<0.05)
cat("\n\nEP unconditional rejections at FWER=0.01\n\n")
print(p.holm.u<0.01)
