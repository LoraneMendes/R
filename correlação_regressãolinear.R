library("pastecs")
library("dplyr")
library("Hmisc")
library("tidyverse")
options(scipen=100)
options(2)
#dados
abdominal <- c(0.87,0.2,0.1,0.23)
altura <- c(2,0.5,0.1,1)
torax <- c(0.53,0.32,0.49,0.51)
pescoco <- c(0.42,0.1,0.2,0.9)
#analise descritiva das variáveis
stat.desc(abdominal)
stat.desc(altura)
stat.desc(torax)
stat.desc(pescoco)
summary(floresta_encantada)


#correlação de Pearson entre abdominal e altura
corr1 = cor(abdominal, altura)
corr1

#correlação de Pearson entre torax e pescoço
corr2 = cor(torax, pescoco)
corr2

cor.test(abdominal, altura,method = "spearman")
cor.test(torax, pescoco,method = "spearman")
cor.test(abdominal, pescoco,method = "spearman")

library(tidyverse)
regressao_linear_multipla  <- lm(altura ~ abdominal + pescoço)
summary(regressao_linear_multipla)

ar(mfrow=c(2,2)) # init 4 charts in 1 panel
plot(regressao_linear_multipla)

