#exercício de exemplo aula critérios de remoção - conceito de floresta balanceada
dados <- read.csv2("Pasta1.csv") #tabela 3, pg 5 do material disponibilizado
dados <- dados[,-c(2,4,5,7,8)]
names(dados) <- c("vc","fo","g")

#modelo linear com a variável transformada
modelo <- "log(fo)~I(vc)"
aj <- lm(modelo,dados)
coef (aj)

#estimado modelo linear
dados$fest <- round(exp(7.5492097+-0.1825294*dados$vc),2)     #est lin

q <- with(dados, fest[1] / fest[2])
g <- sum(dados$g)
grem <- g*.5
b1 <- as.numeric(coef(aj)[2])
b0 <- log(40000 * grem / (pi * sum(dados$vc^2 * exp(b1 * dados$vc))))

dados$fres <- round(exp(b0+b1*dados$vc),2)
dados$frem <- with(dados, fo-fres)

#retirado por espécie
especie1 <- read.csv2("tbl_especies_exemplo_ajustado.csv")
N <- sum(especie1$total_das_classes)          
especie1$DR <- especie1$total_das_classes/N * 100

especie <- especie1[especie1$DR > 1,]
especie1 <- especie1[especie1$DR <= 1,]
esp <- especie$especies

especie <- rbind(especie,especie1)

nedi <- as.numeric(mapply(sum, especie[, c(4:13)]))
nedrdi <- as.numeric(mapply(sum, especie[c(20:28), c(4:13)]))
neidi <- especie[, c(4:13)]

rem <- list()
for (i in 1:length(1:7)){
  rem[[i]] <- (neidi[i]/(nedi[[i]] - nedrdi[[i]])) * dados$frem[i]
}
rem <- data.frame(rem)