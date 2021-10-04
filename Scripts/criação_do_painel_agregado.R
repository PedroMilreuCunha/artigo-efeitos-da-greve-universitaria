# ---- Pedro Milreu Cunha - Mestrando em Economia Aplicada - PPGE/UFPB ---- #

#### BIBLIOTECAS ####

library(dplyr)
library(readxl)
library(stargazer)

#### LEITURA DOS DADOS ####

final_federal_estadual <- readRDS("Dados trabalhados/final_federal_estadual.rds")
final_federal_estadual$CO_IES <- as.numeric(final_federal_estadual$CO_IES)

#### ÚNICOS POR ANO ####

unicos_2009 <- subset(final_federal_estadual, NU_ANO == 2009)
unicos_2009 <- unique(unicos_2009$CO_IES)

unicos_2010 <- subset(final_federal_estadual, NU_ANO == 2010)
unicos_2010 <- unique(unicos_2010$CO_IES)

unicos_2011 <- subset(final_federal_estadual, NU_ANO == 2011)
unicos_2011 <- unique(unicos_2011$CO_IES)

unicos_2012 <- subset(final_federal_estadual, NU_ANO == 2012)
unicos_2012 <- unique(unicos_2012$CO_IES)

unicos_2013 <- subset(final_federal_estadual, NU_ANO == 2013)
unicos_2013 <- unique(unicos_2013$CO_IES)

unicos_2014 <- subset(final_federal_estadual, NU_ANO == 2014)
unicos_2014 <- unique(unicos_2014$CO_IES)

unicos_ao_longo <- Reduce(intersect, list(unicos_2009,
                                          unicos_2010,
                                          unicos_2011,
                                          unicos_2012,
                                          unicos_2013,
                                          unicos_2014))

#### SUBSET DOS ANOS DAS AMOSTRAS E IES ÚNICAS ####

tamanho <- as.numeric(length(unicos_ao_longo))
unicos <- as.numeric(unicos_ao_longo)

painel_federal_estadual <- data.frame(CO_IES = rep(NA, tamanho),
                                      Ano = c(rep(2009, tamanho), rep(2010, tamanho),
                                              rep(2011, tamanho), rep(2012, tamanho),
                                              rep(2013, tamanho), rep(2014, tamanho)))

painel_federal_estadual$CO_IES <- rep(unicos,6)


#### CRIAÇÃO DAS VARIÁVEIS MÉDIAS POR IES POR ANO ####

#### Inicializando as variáveis ####

idade_media <- numeric()
idade2_media <- numeric()
nota_media <- numeric()
prop_homens <- numeric()
prop_casados <- numeric()
prop_brancos <- numeric()
prop_renda_3SM <- numeric()
prop_medio_ou_mais <- numeric()
Tratado <- numeric()

#### 2009 ####

i <- 1
j <- 2009

## Obtendo os valores ##

## Idade média dos alunos

while (i != tamanho+1)
{
  temp <- subset(final_federal_estadual,
                 CO_IES == unicos[i] & NU_ANO == j)
  idade_media <- c(idade_media, mean(temp$NU_IDADE))
  i = i+1
}

i <- 1

## Nota média

  while (i != tamanho+1)
  {
    temp <- subset(final_federal_estadual,
                   CO_IES == unicos[i] & NU_ANO == j)
    nota_media <- c(nota_media, mean(temp$NT_GER))
    i = i+1
  }

i <- 1

## Proporção de alunos homens

  while (i != tamanho+1)
  {
    temp <- subset(final_federal_estadual,
                   CO_IES == unicos[i] & NU_ANO == j)
    prop_homens <- c(prop_homens, mean(temp$Sexo))
    i = i+1
  }

i <- 1

## Proporção de alunos casados

  while (i != tamanho+1)
  {
    temp <- subset(final_federal_estadual,
                   CO_IES == unicos[i] & NU_ANO == j)
    prop_casados <- c(prop_casados, mean(temp$Casado))
    i = i+1
  }

i <- 1

## Proporção de alunos brancos

  while (i != tamanho+1)
  {
    temp <- subset(final_federal_estadual,
                   CO_IES == unicos[i] & NU_ANO == j)
    prop_brancos <- c(prop_brancos, mean(temp$Branco))
    i = i+1
  }

i <- 1

## Proporção de alunos com renda familiar de até 3SM

  while (i != tamanho+1)
  {
    temp <- subset(final_federal_estadual,
                   CO_IES == unicos[i] & NU_ANO == j)
    prop_renda_3SM <- c(prop_renda_3SM, mean(temp$Renda_3SM))
    i = i+1
  }

i <- 1

## Proporção de alunos com mãe com ensino médio ou mais

  while (i != tamanho+1)
  {
    temp <- subset(final_federal_estadual,
                   CO_IES == unicos[i] & NU_ANO == j)
    prop_medio_ou_mais <- c(prop_medio_ou_mais, mean(temp$Medio_ou_mais))
    i = i+1
  }

i <- 1

#### 2010 ####


i <- 1
j <- 2010

## Obtendo os valores ##

## Idade média dos alunos

while (i != tamanho+1)
{
  temp <- subset(final_federal_estadual,
                 CO_IES == unicos[i] & NU_ANO == j)
  idade_media <- c(idade_media, mean(temp$NU_IDADE))
  i = i+1
}

i <- 1

## Nota média

while (i != tamanho+1)
{
  temp <- subset(final_federal_estadual,
                 CO_IES == unicos[i] & NU_ANO == j)
  nota_media <- c(nota_media, mean(temp$NT_GER))
  i = i+1
}

i <- 1

## Proporção de alunos homens

while (i != tamanho+1)
{
  temp <- subset(final_federal_estadual,
                 CO_IES == unicos[i] & NU_ANO == j)
  prop_homens <- c(prop_homens, mean(temp$Sexo))
  i = i+1
}

i <- 1

## Proporção de alunos casados

while (i != tamanho+1)
{
  temp <- subset(final_federal_estadual,
                 CO_IES == unicos[i] & NU_ANO == j)
  prop_casados <- c(prop_casados, mean(temp$Casado))
  i = i+1
}

i <- 1

## Proporção de alunos brancos

while (i != tamanho+1)
{
  temp <- subset(final_federal_estadual,
                 CO_IES == unicos[i] & NU_ANO == j)
  prop_brancos <- c(prop_brancos, mean(temp$Branco))
  i = i+1
}

i <- 1

## Proporção de alunos com renda familiar de até 3SM

while (i != tamanho+1)
{
  temp <- subset(final_federal_estadual,
                 CO_IES == unicos[i] & NU_ANO == j)
  prop_renda_3SM <- c(prop_renda_3SM, mean(temp$Renda_3SM))
  i = i+1
}

i <- 1

## Proporção de alunos com mãe com ensino médio ou mais

while (i != tamanho+1)
{
  temp <- subset(final_federal_estadual,
                 CO_IES == unicos[i] & NU_ANO == j)
  prop_medio_ou_mais <- c(prop_medio_ou_mais, mean(temp$Medio_ou_mais))
  i = i+1
}

i <- 1

#### 2011 ####

i <- 1
j <- 2011

## Obtendo os valores ##

## Idade média dos alunos

while (i != tamanho+1)
{
  temp <- subset(final_federal_estadual,
                 CO_IES == unicos[i] & NU_ANO == j)
  idade_media <- c(idade_media, mean(temp$NU_IDADE))
  i = i+1
}

i <- 1

## Nota média

while (i != tamanho+1)
{
  temp <- subset(final_federal_estadual,
                 CO_IES == unicos[i] & NU_ANO == j)
  nota_media <- c(nota_media, mean(temp$NT_GER))
  i = i+1
}

i <- 1

## Proporção de alunos homens

while (i != tamanho+1)
{
  temp <- subset(final_federal_estadual,
                 CO_IES == unicos[i] & NU_ANO == j)
  prop_homens <- c(prop_homens, mean(temp$Sexo))
  i = i+1
}

i <- 1

## Proporção de alunos casados

while (i != tamanho+1)
{
  temp <- subset(final_federal_estadual,
                 CO_IES == unicos[i] & NU_ANO == j)
  prop_casados <- c(prop_casados, mean(temp$Casado))
  i = i+1
}

i <- 1

## Proporção de alunos brancos

while (i != tamanho+1)
{
  temp <- subset(final_federal_estadual,
                 CO_IES == unicos[i] & NU_ANO == j)
  prop_brancos <- c(prop_brancos, mean(temp$Branco))
  i = i+1
}

i <- 1

## Proporção de alunos com renda familiar de até 3SM

while (i != tamanho+1)
{
  temp <- subset(final_federal_estadual,
                 CO_IES == unicos[i] & NU_ANO == j)
  prop_renda_3SM <- c(prop_renda_3SM, mean(temp$Renda_3SM))
  i = i+1
}

i <- 1

## Proporção de alunos com mãe com ensino médio ou mais

while (i != tamanho+1)
{
  temp <- subset(final_federal_estadual,
                 CO_IES == unicos[i] & NU_ANO == j)
  prop_medio_ou_mais <- c(prop_medio_ou_mais, mean(temp$Medio_ou_mais))
  i = i+1
}

i <- 1

#### 2012 ####

i <- 1
j <- 2012

## Obtendo os valores ##

## Idade média dos alunos

while (i != tamanho+1)
{
  temp <- subset(final_federal_estadual,
                 CO_IES == unicos[i] & NU_ANO == j)
  idade_media <- c(idade_media, mean(temp$NU_IDADE))
  i = i+1
}

i <- 1

## Nota média

while (i != tamanho+1)
{
  temp <- subset(final_federal_estadual,
                 CO_IES == unicos[i] & NU_ANO == j)
  nota_media <- c(nota_media, mean(temp$NT_GER))
  i = i+1
}

i <- 1

## Proporção de alunos homens

while (i != tamanho+1)
{
  temp <- subset(final_federal_estadual,
                 CO_IES == unicos[i] & NU_ANO == j)
  prop_homens <- c(prop_homens, mean(temp$Sexo))
  i = i+1
}

i <- 1

## Proporção de alunos casados

while (i != tamanho+1)
{
  temp <- subset(final_federal_estadual,
                 CO_IES == unicos[i] & NU_ANO == j)
  prop_casados <- c(prop_casados, mean(temp$Casado))
  i = i+1
}

i <- 1

## Proporção de alunos brancos

while (i != tamanho+1)
{
  temp <- subset(final_federal_estadual,
                 CO_IES == unicos[i] & NU_ANO == j)
  prop_brancos <- c(prop_brancos, mean(temp$Branco))
  i = i+1
}

i <- 1

## Proporção de alunos com renda familiar de até 3SM

while (i != tamanho+1)
{
  temp <- subset(final_federal_estadual,
                 CO_IES == unicos[i] & NU_ANO == j)
  prop_renda_3SM <- c(prop_renda_3SM, mean(temp$Renda_3SM))
  i = i+1
}

i <- 1

## Proporção de alunos com mãe com ensino médio ou mais

while (i != tamanho+1)
{
  temp <- subset(final_federal_estadual,
                 CO_IES == unicos[i] & NU_ANO == j)
  prop_medio_ou_mais <- c(prop_medio_ou_mais, mean(temp$Medio_ou_mais))
  i = i+1
}

i <- 1

#### 2013 ####

i <- 1
j <- 2013

## Obtendo os valores ##

## Idade média dos alunos

while (i != tamanho+1)
{
  temp <- subset(final_federal_estadual,
                 CO_IES == unicos[i] & NU_ANO == j)
  idade_media <- c(idade_media, mean(temp$NU_IDADE))
  i = i+1
}

i <- 1


## Nota média

while (i != tamanho+1)
{
  temp <- subset(final_federal_estadual,
                 CO_IES == unicos[i] & NU_ANO == j)
  nota_media <- c(nota_media, mean(temp$NT_GER))
  i = i+1
}

i <- 1

## Proporção de alunos homens

while (i != tamanho+1)
{
  temp <- subset(final_federal_estadual,
                 CO_IES == unicos[i] & NU_ANO == j)
  prop_homens <- c(prop_homens, mean(temp$Sexo))
  i = i+1
}

i <- 1

## Proporção de alunos casados

while (i != tamanho+1)
{
  temp <- subset(final_federal_estadual,
                 CO_IES == unicos[i] & NU_ANO == j)
  prop_casados <- c(prop_casados, mean(temp$Casado))
  i = i+1
}

i <- 1

## Proporção de alunos brancos

while (i != tamanho+1)
{
  temp <- subset(final_federal_estadual,
                 CO_IES == unicos[i] & NU_ANO == j)
  prop_brancos <- c(prop_brancos, mean(temp$Branco))
  i = i+1
}

i <- 1

## Proporção de alunos com renda familiar de até 3SM

while (i != tamanho+1)
{
  temp <- subset(final_federal_estadual,
                 CO_IES == unicos[i] & NU_ANO == j)
  prop_renda_3SM <- c(prop_renda_3SM, mean(temp$Renda_3SM))
  i = i+1
}

i <- 1

## Proporção de alunos com mãe com ensino médio ou mais

while (i != tamanho+1)
{
  temp <- subset(final_federal_estadual,
                 CO_IES == unicos[i] & NU_ANO == j)
  prop_medio_ou_mais <- c(prop_medio_ou_mais, mean(temp$Medio_ou_mais))
  i = i+1
}

i <- 1

#### 2014 ####

i <- 1
j <- 2014

## Obtendo os valores ##

## Idade média dos alunos

while (i != tamanho+1)
{
  temp <- subset(final_federal_estadual,
                 CO_IES == unicos[i] & NU_ANO == j)
  idade_media <- c(idade_media, mean(temp$NU_IDADE))
  i = i+1
}

i <- 1

## Criação da variável Idade²

idade2_media <- idade_media**2

## Nota média

while (i != tamanho+1)
{
  temp <- subset(final_federal_estadual,
                 CO_IES == unicos[i] & NU_ANO == j)
  nota_media <- c(nota_media, mean(temp$NT_GER))
  i = i+1
}

i <- 1

## Proporção de alunos homens

while (i != tamanho+1)
{
  temp <- subset(final_federal_estadual,
                 CO_IES == unicos[i] & NU_ANO == j)
  prop_homens <- c(prop_homens, mean(temp$Sexo))
  i = i+1
}

i <- 1

## Proporção de alunos casados

while (i != tamanho+1)
{
  temp <- subset(final_federal_estadual,
                 CO_IES == unicos[i] & NU_ANO == j)
  prop_casados <- c(prop_casados, mean(temp$Casado))
  i = i+1
}

i <- 1

## Proporção de alunos brancos

while (i != tamanho+1)
{
  temp <- subset(final_federal_estadual,
                 CO_IES == unicos[i] & NU_ANO == j)
  prop_brancos <- c(prop_brancos, mean(temp$Branco))
  i = i+1
}

i <- 1

## Proporção de alunos com renda familiar de até 3SM

while (i != tamanho+1)
{
  temp <- subset(final_federal_estadual,
                 CO_IES == unicos[i] & NU_ANO == j)
  prop_renda_3SM <- c(prop_renda_3SM, mean(temp$Renda_3SM))
  i = i+1
}

i <- 1

## Proporção de alunos com mãe com ensino médio ou mais

while (i != tamanho+1)
{
  temp <- subset(final_federal_estadual,
                 CO_IES == unicos[i] & NU_ANO == j)
  prop_medio_ou_mais <- c(prop_medio_ou_mais, mean(temp$Medio_ou_mais))
  i = i+1
}

i <- 1

#### UNIÃO INICIAL DOS DADOS ####

painel_federal_estadual <- cbind(painel_federal_estadual,
                                 idade_media, idade2_media,
                                 nota_media, prop_brancos,
                                 prop_casados, prop_homens,
                                 prop_medio_ou_mais,
                                 prop_renda_3SM)

#### JUNTANDO OS NOMES DAS IES COM BASE EM SEUS RESPECTIVOS CÓDIGOS ####

Codigos_IES <- read_excel("Dados/Codigos_IES.xlsx",)
Codigos_IES <- Codigos_IES[,1:2]

painel_federal_estadual <- left_join(painel_federal_estadual, Codigos_IES,
                                     by = c("CO_IES" = "CO_IES"))

#### JUNTANDO A LISTA DE IES QUE ADERIRAM À GREVE ####

Aderiram <- read_excel("Dados/Federais_que_aderiram.xlsx")

Aderiram$Nome <- toupper(Aderiram$Nome)

painel_federal_estadual <- left_join(painel_federal_estadual, Aderiram,
                                     by = c("Nome" = "Nome"))

#### CRIANDO A DUMMY DE TRATAMENTO ####

painel_federal_estadual$Tratamento <- ifelse(is.na(painel_federal_estadual$Aderiram),0, 1)

painel_federal_estadual <- painel_federal_estadual[,-12]

#### CRIANDO A DUMMY DE TEMPO ####

painel_federal_estadual$Tempo <- ifelse(painel_federal_estadual$Ano >= 2012, 1, 0)


#### ESTATÍSTICAS DESCRITIVAS ####

stargazer(painel_federal_estadual,
          type = "text",
          title = "Estatísticas descritivas",
          omit = c("CO_IES", "Ano", "Tempo"),
          decimal.mark = ",", digits = 3,
          align = TRUE,
          float = TRUE,
          font.size = "small",
          median = TRUE, iqr = FALSE,
          initial.zero = TRUE)

#### EXPORTANDO O PAINEL ####

saveRDS(painel_federal_estadual, "Dados trabalhados/painel_federal_estadual.rds")
