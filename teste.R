#################################################################################
####### SCRIPT DESENVOLVIDO POR CONTA PRÓPRIA ###################################
############################################## VISANDO TREINAR A LINGUAGEM R ####
#### TODA E QUALQUER INCONSISTÊNCIA É DE MINHA INTEIRA RESPONSABILIDADE #########
#################################################################################
########################################################## MICRODADOS OD_2017 ###

setwd("C:/R/OD")
getwd()

library(foreign)

a <- read.dbf("OD_2017.dbf")
summary(a)
View(a)
unique(a$IDADE)
class(a$IDADE)

library(tidyverse)

b <- a %>%
  filter(IDADE>18)

unique(b$IDADE)
View(b)

unique(b$RENDA_FA)
class(b$RENDA_FA) ## "numeric"
summary(b$RENDA_FA) ## Mean = 5670 || Median = 3988

unique(b$PE_BICI) ### 1 - Pequena distância; 2 - Condução cara;
## 3 - Ponto/Estação distante; 4 - Condução demora para passar;
## 5 - Viagem demorada; 6 - Condução lotada; 
## 7 - Atividade física; 8 - Outros motivos.
class(b$PE_BICI)
 
plot(x = b$PE_BICI,
     y = b$RENDA_FA,
     xlab = "Motivos de andar a pé",
     ylab = "Renda familiar")

modelo_geral <- lm(formula = PE_BICI ~ RENDA_FA, b, na.action = na.exclude)
summary(modelo_geral)

### Condução cara
c <- b %>%
  filter(PE_BICI == "2")
View(c)
unique(c$PE_BICI)

modelo2 <- lm(formula = PE_BICI ~ RENDA_FA, c, na.action = na.exclude)
summary(modelo2)

### Pequena distância
d <- b %>%
  filter(PE_BICI == "1")
unique(d$PE_BICI)

modelo1 <- lm(formula = PE_BICI ~ RENDA_FA, d, na.action = na.exclude)
summary(modelo1)

### Atividade fisica
e <- b %>%
  filter(PE_BICI == "7")
unique(e$PE_BICI)

modelo7 <- lm(formula = PE_BICI ~ RENDA_FA, e, na.action = na.exclude)
summary(modelo7)

library(performance)
tabela_modelos <- compare_performance(modelo1, modelo2, modelo7)
tabela_modelos
