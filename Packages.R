# Instalacao dos pacotes ====
install.packages("readxl")
install.packages("GPArotation")
install.packages("tidyverse")
install.packages("Rtools")
install.packages("haven")
install.packages("readr")
install.packages("mirt")
install.packages("stringr")
install.packages("latticeExtra")
install.packages('pillar')
install.packages("igraphdata")
install.packages("igraph")
install.packages("knitr")
install.packages("psych")



# Ativacao dos pacotes ====

library(GPArotation) # Pacote para rotacao analise fatorial.
library(tidyverse)
library(psych)
library(knitr)
library(readxl)
library(lavaan)
library(semPlot)
library(haven)
library(foreign)
library(pillar)
library(mirt)
library(igraph)
library(igraphdata)
library(readxl)

install.packages(c("igraph","readr","tidyr","RColorBrewer"))
library(lattice)
names(Grande_banco_riasec)
count(Grande_banco_riasec,cursos_selecionados)
count(Grande_banco_riasec,Curso)
citation("lavaan")
citation()

install.packages("plyr")
library(plyr)

library(Rcmdr)


options(scipen = 999) #opcao para eliminacao da notacao de expoentes

options(max.print=4000) #opcao para mostrar mais que 1000 resultados no console.

Grande_banco_riasec %>% count(Sexo) %>% mutate(porc = n/sum(n)*100) %>% kable()#clacular porcentagem

t$Grupo <- as.numeric(as.character(t$Grupo))

# ==== Funcoes uteis

length(x) # retorna numero de variáveis
length(riasec$Escolaridade) # desta forma, retorna o número de observações dentro da variável de interesse.
lengths(riasec, use.names = FALSE) %>% kable() #retorna o número de observações em cada variável

riasec$TCLE[riasec$TCLE == 'Concordo em participar da pesquisa'] <- '1'

dados <- dados1
dados$sexo[dados$sexo == "homens"] <- 0  
dados$sexo[dados$sexo == "mulheres"] <- 1

dados$sexo <- as.numeric(as.character(dados$sexo)) #transformar character em numeric

dados$pais[dados$pais=="Brasil"] <- 1
dados$pais[dados$pais=="Espanha"] <- 0

dados$pais <- as.numeric(as.character(dados$pais)) #transformar character em numeric

dados <- dados %>% select(-81)
names(dados)
dados <- dados %>% mutate(icm = peso_atual / (altura ^ 2))
class(dados$altura)
dados %>% glimpse("icm")

write_csv(dados,"dados.csv",na = "NA",append = FALSE)

dados_brasil <- dados %>% filter(pais==1)
write_csv(dados_brasil,"dados_brasil.csv",na = "NA",append = FALSE)

dados_espanha <- dados %>% filter(pais==0)
write_csv(dados_espanha,"dados_espanha.csv",na = "NA",append = FALSE)

gabi_teste2 %>% mutate(porc_AD = Advogados/sum(Advogados)*100)

# Funcao para alterar a decodificacao para a Lingua Portuguesa do Brasil ==== 

Sys.setlocale("LC_ALL","pt_BR.UTF-8")


