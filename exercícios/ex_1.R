# Você foi contratado(a) para realizar um plano de manejo sustentado numa área de 2800ha com vegetação de cerrado
# stricto sensu. Realizado o levantamento de campo, os dados mensurados nas parcelas foram estruturados conforme
# mostrado na tabela 1. (100%)
# 
# Tabela 1: Dados agrupados por classe diamétrica e extrapolados por hectare
#   vc  fo       g       fo.g
#   7   543,51  0,00385  2,0925
#   11  304,36  0,00950  2,8914
#   15  141,31  0,01767  2,4970
#   19  65,22   0,02835  1,8490
#   23  36,96   0,04155  1,5356
#   27  16,31   0,05726  0,9336
# Total 1107,66          11,7991
# onde: vc=valor central da classe diamétrica [cm], fo=frequência observada/ha, g = Área seccional 
# [m2/arv.].

# a) Quantas árvores podem ser removidas por classe diamétrica se a área basal remanescente for 58%, 
# o diâmetro máximo não for modificado e o novo quociente de De Liocourt for 95% do quociente original?

df <- data.frame(vc = c(7, 11, 15, 19, 23, 27),
                 fo = c(543.1, 304.36, 141.31, 65.22, 36.96, 16.31),
                 g = c(0.00385, 0.00950, 0.01767, 0.02835, 0.04155, 0.05726),
                 fog = c(2.0925, 2.8914, 2.4970, 1.8490, 1.5356, 0.9336))

# modelo de meyer linearizado
linear_model <- lm("log(fo) ~ I(vc)", data = df)

# dados de frequência estimada
df$fe <- round(exp(as.numeric(coef(linear_model)[1]) + as.numeric(coef(linear_model)[2]) * df$vc), 2)

# coeficiente de De liocourt
q <- round(with(df, fe[1] / fe[2]), 5)

# novo coeficiente 95% do quociente original
qa <- 0.95*q

# novo beta1
b1 <- with(df, log(qa)/(vc[1] - vc[2]))

# novo beta 0, área basal remanescente de 58%
g <- sum(df$fog)
grem <- g * 0.58
b0 <- log(40000 * grem/ (pi*sum((df$vc^2 * exp(b1 * df$vc)))))

# remanescente
df$fres <- round(exp(b0 + b1 * df$vc), 2)
# removida:
df$frem <- with(df, fo - fres)


# b) Quantas árvores por espécie serão removidas por classe diamétrica dadas as 
# frequências observadas apresentadas na tabela 2? 
# Lembre-se que espécies com DR ≤ 1% são proibidas de corte.

df2 <- read.csv2("tbl2_exercicios.csv")

names(df2) <- c("vc", "especie_1", "especie_2", 
                "especie_3", "especie_4", "especie_5")

df2$total_das_classes <- rowSums(df2[, c(2:6)])

# densidade relativa
df2 <- setNames(data.frame(t(df2[, -1])), nm = df2[,1])
df2$tot_especie <- rowSums(df2)
df2$DR <- df2$tot_especie / df2$tot_especie[6] * 100

# removido por classe
nedi <- as.numeric(mapply(sum, df2[c(1:5), c(1:6)]))
nedrdi <- as.numeric(mapply(sum, df2[df2$DR <= 1, c(1:6)]))
neidi <- df2[c(1:5), c(1:6)]

rem <- list()
for (i in 1:length(1:6)){
  rem[[i]] <- (neidi[i] / (nedi[[i]] - nedrdi[[i]])) * df$frem[i]
}
rem <- data.frame(rem)
rem <- t(rem)

dr_excluido <- rownames(df2[df2$DR <= 1, c(1:6)])
rem[, which(colnames(rem) == dr_excluido)] <- 0

rem <- round(rem, 2)
# c) Durante a elaboração do plano, várias simulações podem ser realizadas alterando-se 
# o quociente de De Liocourt, a área basal remanescente e o diâmetro máximo. 
# Você aceitaria o plano que você gerou no item a? Por quê?

# Eu aceitaria pois ele é um plano adequado para ser feito, isso se deve ao 
# ao fato de que nenhuma das classes ficaram com um estrato remanescente negativo.


# d) Em seu plano de manejo, utilizando o conceito de floresta balanceada, 
# o que você deve fazer para remover preferencialmente as maiores árvores? Justifique.

# Eu devo utilizar um q maior do que o calculado inicialmente, pois isso implica na 
# remoção de um maior número de plantas de maior dimensão para a remoção de uma mesma
# área basal.

