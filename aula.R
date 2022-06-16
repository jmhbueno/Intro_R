# Aula 20/08/2021 - Introdução ao R ====

# Instalação de pacotes
install.packages("tidyverse")
install.packages("expss")
install.packages("knitr")

# Ativação de pacotes
library(tidyverse) # ctrl + enter
library(expss)
library(knitr)

soma <- c(3 # este é o número 3
          + 4)  

3+4
5-2
4*2
9/3
sqrt(9)
2^3
1:10
10:1

3 == 3
3 == 5
5>=9
5<=9
5<9
3
3<= 2:5
3 + 4 >= 14/2
3 + 4 > 14/2
3+4==14/2
!3==5 # três não é igual a 5?

1 < 2 | 2 >3
TRUE
FALSE

NA # not available

# listas: coleção de objetos 
# vetores: coleção de objetos da mesma natureza
# dataframes: é uma coleção de vetores

minha_lista <- list("s01","masculino",23,"ensino médio")

meu_vetor <- c("masculino","feminino","feminino","masculino","feminino")
sexo <- meu_vetor
Id <- c("s01","s02","s03","s04","s05")
Idade <- c(18,23,19,20,30)

meu_dataframe <- data.frame(Id,sexo,Idade)

mean(meu_dataframe$Idade)

# Exercício - aula 1 =====
# Listas de Exercícios

# 1) Montar um data frame
# C:\Users\jmhbu\OneDrive\Documentos\R\R_basico\Intro_R\montar_dataframe.docx

# Passo 1: montar vetores de todas as variáveis.

id <- c("s1","s2","s3","s4","s5")
idade <- c(13, 23, 35, 20, 19)
sexo <- c("F","F","M","M","F")
escola <- c("F","M","S","S","S")
renda <- c("2","2","3","2","3")
estado_civil <- c("solt","solt","UEst","solt","solt")
estado <- c("SP","SC","SC", NA, "SP")
trabalho <- c("Não","Sim","Sim","Não","Não")
v1 <- c("4","2","4","4","1")
v2 <- c("3","3","4","4","1")
v3 <- c("1","1","4","1","1")
v4 <- c("3","4","4","4","3")
v5 <- c("3","3","4","2","1")

as.numeric(df$idade)
as.character()
as.factor()

# Passo 2: montar o data frame com os vetores criados
df <- data.frame(id, idade, sexo, escola, renda, estado_civil, 
                 estado, trabalho, v1, v2, v3, v4, v5)


# outra solução seria já criar os vetores dentro do dataframe
montar_dataframe <- data.frame(id = c("s1","s2","s3","s4","s5"),
                               idade = c(13, 23, 35, 20, 19),
                               sexo = c("F","F","M","M","F"),
                               escola = c("F","M","S","S","S"),
                               renda = c(2,2,3,2,3),
                               estado_civil = c("solt","solt","UEst","solt","solt"),
                               estado = c("SP","SC","SC", NA, "SP"),
                               trabalho = c("Não","Sim","Sim","Não","Não"),
                               v1 = c(4,2,4,4,1),
                               v2 = c(3,3,4,4,1),
                               v3 = c(1,1,4,1,1),
                               v4 = c(3,4,4,4,3),
                               v5 = c(3,3,4,2,1))

count(montar_dataframe,estado)

# aula 2 - 27/08/2021

# criar vetor sexo, em que 1 é masculino e 2 é feminino

sexo <- c(1,2,1,1,2)
is.vector(sexo)

# pode-se criar um vetor semelhante, mas com letras
# quando uma informação textual for inserida num objeto, ela tem que ir entre aspas.

sexo_cod <- c('m','f','m','m','f')

idade <- c(25,32,78,12,NA) # vetor com info sobre idade

df <- data.frame(sexo_cod,idade)

# acrescentando a variável escolaridade

escolaridade <- c('superior','medio','fundamental','fundamental','medio')

cbind(df,escolaridade) # cbind() é uma função da base que anexa colunas.

df <- cbind(df,escolaridade)

# acrescentando a variável id

id <- 1:5

df <- cbind(df,id)

# Se quiser inserir um "s" antes do número de cada sujeito, 
# podemos usar a função paste(), que concatena duas partes 
# da informação que constar em uma coluna. 
# Essa função diz: concatene um s com valores de 1 a 5, 
# separados por nada.

id <- paste("s",1:5,sep = "")

df$id <- paste("s",1:5,sep = "")

suj6 <- list("m",34,"superior","s6")

df <- rbind(df,suj6)

# inserir a variável renda
# seq(from,to,by,lenth)

df$renda <- seq(from=1000,by=500,length = 6)

rep(500,6)
df$bonus <- rep(500,6) 



df$renda_total <- df$renda + df$bonus

# inserir uma variável Estado Civil

df$"Estado Civil" <- c("casado/a","solteiro/a","viúvo/a","solteiro/a","casado/a","casado/a")
df

# FUNÇÕES PARA VISUALIZAÇÃO DO BANCO DE DADOS======================

df
names(df)
str(df)
glimpse(df) # tidyverse - dplyr
head(df)
ncol(df)
nrow(df)
view(df)
unique(df$escolaridade)
unique(df$sexo_cod)
unique(df$"Estado Civil") # estado_civil

# Alterar o nome da variável "sexo_cod"

rename(df, sexo = sexo_cod)
names(df)

df <- rename(df, sexo = sexo_cod)

# Selecionar linhas e colunas: dataframe[linhas,colunas]

df[1:3, ]
df[ ,1:3]


df[1:3,1:3]

# Se que quisermos selecionar as linhas 1, 3 e 6, como fazemos?

df[c(1,3,6),c(1,3)]

df[ ,c("id","idade","renda_total")]

# Selecionar somente as linhas dos participantes do sexo masculino

df[df$sexo == "m", ]

# Selecionar somente as linhas dos participantes do sexo masculino com renda_total maior ou igual a 2500 reais.
df[df$sexo == "m" & df$renda_total >= 2500, ]

# selecionar pessoas do sexo feminino OU (|) viúvos

df[c(df$sexo == "f" | df$`Estado Civil`== "viúvo/a"), ]

# Alterar o conteúdo dentro de uma variável

df$sexo[df$sexo == "f"] <- 0
df$sexo[df$sexo == "m"] <- 1

df$sexo <- as.numeric(df$sexo)
df$sexo <- as.factor(df$sexo) # transforma a variável em nominal

df$escolaridade <- as.factor(df$escolaridade)

# atribuir levels aos valores 0 e 1 da variável sexo

levels(df$sexo) <- c("feminino","masculino")
levels(df$escolaridade) <- c("fundamental","médio","superior")

# Alterar o nome da variável "Estado Civil"
df <- rename(df,estado_civil = "Estado Civil")

# Alterar a ordem das variáveis no banco de dados
df[ ,c("id","sexo","idade","escolaridade","estado_civil",
       "renda","bonus","renda_total")]

df <- df[ ,c("id","sexo","idade","escolaridade","estado_civil",
       "renda","bonus","renda_total")]

# importar uma planilha do excel
# read_xlsx("Variables.xlsx") # assim, o R importa mas não salva como objeto

install.packages("readxl")
library(readxl)

read_xlsx("Variables.xlsx")


variables <- read_xlsx("Variables.xlsx")
df <- cbind(df,variables)

# forma alternativa
df <- cbind(df,read_xlsx("variables.xlsx"))

saveRDS(df,"df.rds") # formato reconhecido pelo R
write_csv2(df, "df.csv")
writexl::write_xlsx(df,"df.xlsx")

read.csv2("df.csv")

str(df)


saveRDS(df,"df.rds")
readRDS("df.rds")


# Exercícios de manipulação de dados com o BIG5 ===============================

# copie os arquivos big_five.csv e codebook.txt para o mesmo diretório em que
# irá trabalhar.
# Importar o banco de dados "big_five.csv"
# Esse é um banco de dados estrangeiro, está codificado no sistema europeu.
# Portanto, use a função read.csv() para importá-lo.

setwd("C:/Users/jmhbu/OneDrive/Documentos/R/R_basico/Intro_R")
read.csv("big_five.csv",sep = "\t")

# Tidyverse (dplyr) - Aula 17/09/2021 - =========================================

library(tidyverse)
# Tidyverse é um família de pacotes
# dplyr - manipulação de dados
# ggplot - visualização de dados (gráficos)
# readr - ler arquivos de texto

# FUNÇÃO PIPE (shift+ctrl+m) - "então"

big_five[1:10,1:10]

big_five <- big_five %>% slice(1:10) %>% select(1:10)

# Verbos dyplr

## select, filter, slice, arrange, mutate, group_by

# select é utilizada para selecionar COLUNAS

df %>% select(id,sexo)
df %>% select(1,3,5)
names(df) %>% as.data.frame()

df[ ,c("id","sexo","escolaridade","estado_civil","v1","v2","v3","v4","v5")]

df %>% select(1,2,4,5,9:13)
names(df) %>% as.data.frame()

df %>% select(id, sexo, escolaridade, estado_civil,v1,v2,v3,v4,v5)

df %>% select(-4)

df %>% select(9:13,1:8)
df %>% select(-c(9,13))
df %>% select(-c(v1,v5))

df %>% select(sexo,escolaridade, everything())

# filter - filtra linhas de acordo com as categorias dentro de uma variável

df[df$sexo == "masculino",]

df %>% filter(sexo == "masculino",escolaridade == "fundamental",estado_civil=="solteiro/a")

df %>% filter(!escolaridade == "fundamental")

df %>% filter(!escolaridade == "fundamental",renda>=3000)

# slice  - serva para selecionar casos (linhas inteiras)

df %>% slice(1:3)
df %>% slice(1,4,6)

df %>% slice_sample(n=3)

df %>% slice_sample(prop = .34)

# arrange -  ordena os dados por um critério

df %>% slice_sample(prop = .50) %>% arrange(desc(idade))

df %>% arrange(idade)

df_novo <- df %>% arrange(id)

# mutate - criar novas variáveis, ou alterar variáveis existentes

df <- df %>% mutate(escore = v1+v2+v3+v4+v5)
df %>% mutate(media = (v1+v2+v3+v4+v5)/5)

# group_by agrupa os resultados de uma análise, por categorias de uma variável

df %>% group_by(sexo) %>% count(escolaridade)
df %>% group_by(sexo) %>% summarise(média = mean(idade, na.rm = TRUE))
