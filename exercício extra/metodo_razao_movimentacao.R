fst <- read.csv2('./rm_w_mt/dados/fustes_med1_med2_spp.csv',fileEncoding = 'latin1')

#dmin=min(fst$dap1[fst$dap1>0])
dmin <- 5
#dmax<-max(c(fst$dap1,fst$dap2))
dmax <- 45
#nclasses<-nclass.Sturges(fst$dap2)
nclasses <- 8

n_proj <- 2 #Prognose a partir da segunda medição para n_proj * intervalo de tempo entre medições

# Preparação dos dados
fst <- data.frame(dap1 = fst$dap1, dap2 = fst$dap2)
fst <- fst[(fst$dap1==0 | fst$dap1>=dmin) & (fst$dap2==0 | fst$dap2>=dmin),]

breaks <- seq(dmin,dmax, len=nclasses+1)
fst$cld1 <-cut(fst$dap1, breaks=breaks, include.lowest=T, right=F, labels=F)

tfreq <- function(x, ndigits=2,...){
  hx <- hist(sort(x),...);
  if(!is.null(ndigits)) hx$mids <- round(hx$mids,ndigits)
  return(data.frame(vc = hx$mids, fo = hx$counts))
}
fo_recrut<- tfreq(fst$dap2[fst$dap1==0], breaks=breaks, include.lowest=T,right=F, plot=F)
fo_mortas<- tfreq(fst$dap1[fst$dap2==0], breaks=breaks, include.lowest=T,right=F, plot=F)
fo_med2 <- tfreq(fst$dap2[fst$dap2>0],breaks=breaks,include.lowest=T,right=F,plot=F)

sfst <- fst[fst$cld1 %in% c(1:8) & fst$dap2 > 0,]

fg <- function(fst){
  nfreq <- list()
  for (i in unique(fst$cld1)) nfreq[i] <- nrow(fst[fst$cld1 == i,])
  nfreq <- Reduce("c", nfreq)
  
  sfst$subt <- fst$dap2 - fst$dap1
  somatorio <- vector()
  for (i in 1:8) somatorio[i] <- sum(sfst[sfst$cld1 == i,]$subt)
  im <- (somatorio / nfreq) / 5
  
  return(matrix(im))
}

im <- fg(fst = sfst)
z <- matrix(nrow = 8, ncol = 8)

f <- function(i, j)  {
  z[j,i] <- im[i,]
  z[i,i] <- 1 - z[j,i]
  z
}
f1 <- f(1,2); f2 <- f(2,3); f3 <- f(3,4); f4 <- f(4,5);
f5 <- f(5,6); f6 <- f(6,7); f7 <- f(7,8); 
f8 <- matrix(c(rep(0,7),1)) 

z <- cbind(f1[,1], f2[,2], f3[,3], f4[,4], f5[,5], f6[,6], f7[,7], f8)
z[c(which(is.na(z)))] <- 0

fator_extrapolacao <-  10000/1200

dimnames(z) <- list(fo_med2$vc,fo_med2$vc)
vc <- fo_med2$vc
fo_med2

p <- function (x, y) ((z[x,x] * fo_med2$fo[x]) + fo_med2$fo[y] * z[x,y] - fo_mortas$fo[x]) * fator_extrapolacao
fest <- c(((z[1,1] * fo_med2$fo[1]) + fo_recrut$fo[1] - fo_mortas$fo[1]) * fator_extrapolacao, 
          p(2,1), p(3,2), p(4,3), p(5,4), p(6,5), p(7,6), 0)


classe = cut(fo_med2$vc, breaks = breaks, include.lowest = T, right = F)
fo_med2 = fo_med2$fo * fator_extrapolacao
fo_recrutamento = fo_recrut$fo * fator_extrapolacao
fo_mortas = fo_mortas$fo * fator_extrapolacao
fo_esperada = fest

prog <- data.frame(classe, fo_med2, fo_recrutamento, fo_mortas, fo_esperada)
