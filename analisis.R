# fontes:

# https://en.wikipedia.org/wiki/2006_Brazilian_general_election

# https://en.wikipedia.org/wiki/2016_United_States_presidential_election

# https://en.wikipedia.org/wiki/1984_United_States_presidential_election

# https://www.cesop.unicamp.br/por/banco_de_dados/v/1223

# https://www.pewresearch.org/fact-tank/2020/08/18/men-and-women-in-the-u-s-continue-to-differ-in-voter-turnout-rate-party-identification/
  
# https://www.cesop.unicamp.br/por/banco_de_dados/v/1446

# https://infograficos.gazetadopovo.com.br/economia/pib-do-brasil/

# https://data.worldbank.org/indicator/NY.GDP.MKTP.KD.ZG?locations=US




#ESQ - democr, pt, autodeclaração
#DIR - repu, Alckmin e Bolsonaro
# pegar dados do brasil de 1985 (voto masculino)
library(haven)
X00507 <- read_sav("00507.SAV")
library(tidyverse)
str(X00507$V4) # 1 homem , 2 mulher

X00507 <- X00507%>%
  filter(V4== 1)

table(X00507$V158)
prop.table(table(X00507$V158))
# esq -> 
0.012244898 + 0.020408163 + 0.089795918 + 0.028571429 + 0.208163265
#[1] 0.3591837

#dir ->
1- 0.3591837
#[1] 0.6408163

rm(X00507)


#pegar dados brasil 1994
X00328 <- read_sav("00328.SAV")

X00328 <- X00328%>%
  filter(sexo== 1)

str(X00328$p15)
prop.table(table(X00328$p15))
#45% dir
#27,32% esq

45+27
#72 é 100
(45*100)/72
(27*100)/72

#
library(readxl)
dados <- read_excel("dados.xlsx")
str(dados)
# T = tempo (4 linhas) = t1bra = 1985, t1eua = 1984, t2bra = 1993, t2eua = 1994, t3bra,eua = 2004,2006; t4braeua = 2018,2016.
# ideologia - direita - esquerda (alguns casos resultado eleitoral, outros pesquisa amostragem = 1985,1993,1994)
# quando resultado eleitoral - DEMOCRATAS(EUA)/PT x PSDB/COLLOR/BOLSONARO/Republicanos(EUA)
# men = % de votação/apoio homen (não tem apoio/voto feminino na amostro)
# country = brasil ou UnitedStates
#ganhou = se ganhou a eleição mais próxima

# bivariada
cor.test(dados$men, dados$t)#nao
t.test(men ~ ideologia, data = dados)#opa
t.test(men ~ country, data= dados)#nao
t.test(men ~ganhou, data= dados)#opa

# modelo

# votos do homem = ideolgoia + tempo + country

modelo1 <- lm(men ~ ideologia, data = dados)
modelo2 <- lm(men ~ ideologia + t + country, data = dados)
modelo3 <- lm(men ~ ideologia + t + country + ganhou, data = dados)
summary(modelo1)
summary(modelo2)
summary(modelo3)
library(huxtable)

huxreg(modelo1, modelo2, modelo3, stars = c(`'` = 0.3,`''` = 0.2,`*` = 0.1, `**` = 0.05,
                                         `***` = 0.01), statistics = c("N. obs." = "nobs", "R2" = "r.squared",
                                                                       "AIC" = "AIC"))
library(coefplot)
coefplot(modelo3, intercept = F)
library(olsrr)
ols_vif_tol(modelo3)#ok
ols_eigen_cindex(modelo3)#ok

# diagnostico 
plot(modelo3)

# peso de cada variável
library(lm.beta)
modelo3beta <- lm.beta(modelo3)
library(sjPlot)
tab_model(modelo3beta, show.ci = F, auto.label = T, show.se = T,
          collapse.se = T, wrap.labels = 60, p.style = "stars")


# conclusão (tendo em base 8 eleições de dois países, cheguei a conclusão de que o resultado conta mais para o voto masculino do que a outros fatores)

# porém, isso daqui é um exercício.
# o que chamei de esquerda e direita não é consenso
# utilizei métricas diferentes para anos diferentes
# qual seria o efeito de mais casos de mais países?
# quem sabe na parte 2...
