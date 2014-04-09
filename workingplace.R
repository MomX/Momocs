rm(list = ls())
setwd("/Users/vincent/Momocs/")
require(MASS) ; require(rgl) ; require(ape) ; require(orthopolynom)
source("R/global.R")
source("R/Out.R")
source("R/Morpho.R")
load("data/bot.rda")
load("data/trilo.rda")
load("data/mosquito.rda")
load("data/hearts.rda")
load("data/Dattes.rda")
load("~/Dropbox/MG-out.rda")

bot <- coo.sample(bot, 64)
b <- bot[5]
bup <- coo.up(coo.align.xax(b))
botE <- eFourier(bot, 6, norm=TRUE)
botP <- pca(botE)
ms <- meanshapes(botE, 1)

# coo.plot(bup)
# bup.p <- polynomials(bup, n=5, ortho=FALSE)
# bup.pi <- polynomials.i(bup.p, bup[, 1])
# lines(bup.pi, col="red", type="b", pch=20)


function(M,n = dim(M)[1]){
  p<-dim(M)[1]
  if (n != p) {n<-n+1}
  M1<-M/coo.perim.cum(M)[p]
  t1<-1-coo.perim.cum(M1)
  J<-matrix(NA, p, p)
  for (i in 1:p){
    for (j in 1:p){J[i, j]<-(factorial(p-1)/(factorial(j-1)*factorial(p-j)))*(((1-t1[i])^(j-1))*t1[i]^(p-j))}}
  B<-ginv(t(J[,1:n])%*%J[,1:n])%*%(t(J[,1:n]))%*%M
  M<-J[,1:n]%*%B
  list(J=J, B=ginv(t(J[,1:n])%*%J[,1:n])%*%(t(J[,1:n]))%*%M)}


cumchord<-function(M)
{cumsum(sqrt(apply((M-rbind(M[1,],M[-(dim(M)[1]),]))^2,1,sum)))}
####f5.2
bezier<-function(M, n = dim(M)[1])
{
  coo <- M
  p<-dim(M)[1]
 if (n != p) {n<-n+1}
 M1<-coo/coo.perim.cum(coo)[p]
 t1<-1-coo.perim.cum(M1)
 J<-matrix(NA, p, p)
 for (i in 1:p){
   for (j in 1:p){J[i, j]<-(factorial(p-1)/(factorial(j-1)*factorial(p-j)))*(((1-t1[i])^(j-1))*t1[i]^(p-j))}}
 B<-ginv(t(J[,1:n])%*%J[,1:n])%*%(t(J[,1:n]))%*%M
 M<-J[,1:n]%*%B
 list(J=J, B=ginv(t(J[,1:n])%*%J[,1:n])%*%(t(J[,1:n]))%*%M)}
####f5.3
beziercurve<-function(B,p)
{X<-Y<-numeric(p)
 n<-dim(B)[1]-1
 t1<-seq(0,1,length=p)
 coef<-choose(n, k=0:n)
 b1<-0:n; b2<-n:0
 for (j in 1:p)
 {vectx<-vecty<-NA
  for (i in 1:(n+1))
  {vectx[i]<-B[i,1]*coef[i]*t1[j]^b1[i]*(1-t1[j])^b2[i]
   vecty[i]<-B[i,2]*coef[i]*t1[j]^b1[i]*(1-t1[j])^b2[i]}
  X[j]<-sum(vectx)
  Y[j]<-sum(vecty)}
 cbind(X, Y)}

b <- coo.scale(coo.center(coo.sample(bot[5], 12)))
coo.plot(b)
b.b <- bezier(b)
b.i <- beziercurve(b.b$B, 120)
lines(b.i, col="red")
