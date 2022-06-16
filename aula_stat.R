Dataset <- read.csv("dataset_mapfre.csv", encoding = "UTF-8", stringsAsFactors = TRUE)

glimpse(Dataset)

# Estatística descritiva - Frequências

Dataset %>% 
  group_by(sex) %>% 
  count(country) %>% 
  mutate(porc = n/sum(n)*100) %>% 
  pander()

# usando a função tabyl() do pacote janitor

Dataset %>% 
  tabyl(country) %>% 
  pander()

Dataset %>% 
  tabyl(country) %>% 
  adorn_totals() %>% 
  pander()

Dataset %>% 
  filter(!is.na(sex)) %>% 
  tabyl(country,sex) %>% 
  adorn_totals() %>% 
  pander()

Dataset %>% 
  filter(!is.na(sex)) %>% 
  tabyl(country,sex) %>% 
  adorn_totals(c("row","col")) %>% 
  adorn_percentages("row") %>% 
  adorn_pct_formatting(rounding = "half up", digits = 0) %>% 
  adorn_ns() %>% 
  pander()

# Medidas de tendência central e de dispersão

# média
mean(Dataset$age, na.rm = TRUE) # a função na.rm() remove os NA's da variável

# desvio padrão
sd(Dataset$age, na.rm = TRUE)

# limites de um desvio em relação à média
mean(Dataset$age, na.rm = TRUE) + sd(Dataset$age, na.rm = TRUE)
mean(Dataset$age, na.rm = TRUE) - sd(Dataset$age, na.rm = TRUE)

# mediana
median(Dataset$age, na.rm = TRUE)

# mínimo
min(Dataset$age, na.rm = TRUE)

# máxima
max(Dataset$age, na.rm = TRUE)

# amplitude
range(Dataset$age, na.rm = TRUE)

# simetria
moments::skewness(Dataset$age, na.rm = TRUE)

# curtose
moments::kurtosis(Dataset$age, na.rm = TRUE)

hist(Dataset$age, breaks = 50)

data.frame(Estatísticas =       # data.frame() é a função para criar uma tabela ou planilha
             c("Média",         # eu quero três colunas: estatísticas, depressão e ansiedade
               "Desvio Padrão", # então, precisamos definir o que vai aparecer em cada coluna   
               "Mínimo",
               "Máximo",
               "Assimetria",
               "Curtose"),
           Depressão = 
             c(mean(Dataset$bdi_sum, na.rm = TRUE),
               sd(Dataset$bdi_sum, na.rm = TRUE),
               min(Dataset$bdi_sum, na.rm = TRUE),
               max(Dataset$bdi_sum, na.rm = TRUE),
               moments::skewness(Dataset$bdi_sum, na.rm = TRUE),
               moments::kurtosis(Dataset$bdi_sum, na.rm = TRUE)),
           Ansiedade = 
             c(mean(Dataset$bai_sum, na.rm = TRUE),
               sd(Dataset$bai_sum, na.rm = TRUE),
               min(Dataset$bai_sum, na.rm = TRUE),
               max(Dataset$bai_sum, na.rm = TRUE),
               moments::skewness(Dataset$bai_sum, na.rm = TRUE),
               moments::kurtosis(Dataset$bai_sum, na.rm = TRUE))) %>% 
  pander()

# Funções que sumariam dados

# describe() do pacote psych
install.packages("psych")
library(psych)

describe(Dataset$age, na.rm = TRUE) %>% pander()

# summary() da base do R
Dataset %>% select(age) %>% summary()

# tableby() do pacote arsenal
install.packages("arsenal")
library(arsenal)

tableby(country ~ bdi_sum + bai_sum,
        test = FALSE,
        data = Dataset) %>% 
  summary(text = TRUE)

names(Dataset)

# descritivas de depressão e ansiedade em função do sexo
tableby(sex ~ bdi_sum + bai_sum,
        test = FALSE,
        data = Dataset) %>% 
  summary(text = TRUE)

# descritiva de idade em função do sexo
tableby(sex ~ age + bdi_sum + bai_sum,
        test = FALSE,
        data = Dataset) %>% 
  summary(text = TRUE)


# só as descritivas para a amostra completa
tableby(~ age + bdi_sum + bai_sum,
        test = FALSE,
        data = Dataset) %>% 
  summary(text = TRUE)

# descrever as médias em função do sexo, estratificado por país

tableby(sex ~ age + bdi_sum + bai_sum,
        test = FALSE,
        data=Dataset, 
        strata = country) %>% 
  summary(text = TRUE)

# mesma tabela anterior mas invertendo as variáveis categóricas
tableby(country ~ age + bdi_sum + bai_sum,
        test = FALSE,
        data = Dataset, 
        strata = sex) %>% 
  summary(text = TRUE)

# interação entre as variáveis categóricas

tableby(interaction(sex,country) ~ bdi_sum + bai_sum,
        test = FALSE,
        data = Dataset) %>% 
  summary(text = TRUE)

# Análises de associação entre variáveis

# qui-quadrado

## gerar uma tabela de contingência

tabcont_sex_country <- table(Dataset$sex,Dataset$country)

options(scipen = 999)
chisq.test(tabcont_sex_country)

options(scipen = 0)

# calcula da correção do valor de p
0.05/(nrow(tabcont_sex_country)*ncol(tabcont_sex_country))
0.05/(2*3)

# tamanho do efeito (v de Cramer)
install.packages("rstatix")
library(rstatix)

cramer_v(tabcont_sex_country)

data.frame("Graus de Liberdade" = c(1,2,3),
           Pequeno = c(0.1,0.07,0.06),
           Médio = c(0.3,0.21,0.17),
           Grande = c(0.5,0.35,0.29)) %>% pander()

# representação gráfica

ggplot(Dataset, aes(x = country, fill = sex)) +
  geom_bar(position = "fill") +
  coord_flip() +
  labs(title = "Proporção Sexo x País",
       x = "País",
       y = "Proporção",
       fill = "sex")
  

# Correlação

# correlação de Pearson (distribuição normal - estatística paramétrica)
# Correlação de Spearman (distribuição não-normal - estatística não-paramétrica)
# Correlação de Kendall (distribuição não-normal - estatística não-paramétrica)

## Primeiro passo - Verificação de normalidade (Shapiro-Wilk test)

names(Dataset)

options(scipen = 999)
options(scipen = 0)

shapiro.test(Dataset$bdi_sum)
hist(Dataset$bdi_sum)

shapiro.test(Dataset$bai_sum)
hist(Dataset$bai_sum)

cor.test(Dataset$bdi_sum,Dataset$bai_sum, method = "pearson") %>% pander()
cor.test(Dataset$bdi_sum,Dataset$bai_sum, method = "spearman") %>% pander
cor.test(Dataset$bdi_sum,Dataset$bai_sum, method = "kendall") %>% pander()

names(big_five)
round(cor(big_five[ ,59:63], method = "pearson"),digits = 2) %>% pander()

nrow(big_five)

matrix <- corr.test(big_five[1:30,59:63], method = "pearson")

matrix$r
matrix$p
matrix$n

matrix$stars %>% knitr::kable()

corr.test(big_five$extr,big_five$amab, method = "pearson")

corr.test(big_five[,59:63], big_five$idade, method = "pearson") %>% pander()

corr.test(Dataset$bdi_sum,Dataset$bai_sum, method = "pearson")

ggplot(Dataset, aes(x=bdi_sum,y=bai_sum)) +
  geom_jitter() +
  geom_smooth(method = "lm") +
  labs(title = " Depressão x Ansiedade",
       x = "Depressão",
       y = "Ansiedade")

pairs.panels(big_five[ ,59:63], method = "pearson")

corrplot::corrplot(matrix$r,
         method = "shade",
         type = "lower",
         order = "hclust",
         adCoef.col = TRUE)
