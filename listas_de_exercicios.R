# Listas de Exercícios

# 1) Montar um data frame
# C:\Users\jmhbu\OneDrive\Documentos\R\R_basico\Intro_R\montar_dataframe.docx

# Passo 1: montar vetores de todas as variáveis.

id <- c("s1","s2","s3","s4","s5")
idade <- c("13", "23", "35", "20", "19")
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


