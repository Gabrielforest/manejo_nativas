# gerar 1 vetor com 5 valores entre 0 a 1 
# onde o vetor soma igual a 1
vetor <- Surrogate::RandVec(a = 0, b = 1, # intervalo dos números gerados 
                            s = 1,        # resultado da soma 
                            n = 5,        # quantidade de números
                            m = 1)        # colunas

sum(vetor$RandVecOutput[,1])

