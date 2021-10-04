# ---- Pedro Milreu Cunha - Mestrando em Economia Aplicada - PPGE/UFPB ---- #

#### Bibliotecas ####

library(dplyr)
library(readxl)
library(stargazer)

#### FUNÇÕES ####

`%notin%` <- Negate(`%in%`)

Criar_medias_area <- function(unicos, anos, index, painel) {

idade_media <- numeric()
idade2_media <- numeric()
nota_media <- numeric()
prop_homens <- numeric()
prop_casados <- numeric()
prop_brancos <- numeric()
prop_renda_3SM <- numeric()
prop_medio_ou_mais <- numeric()
Tratado <- numeric()
    
    for (j in anos) {
    
      if (j == 2009) {
  
    for (i in seq(from = 1, length.out = 69, by = 1))
    {
      temp <- subset(final_federal_estadual,
                     CO_IES == unicos[i] & NU_ANO == j & CO_GRUPO == index)
      
      idade_media[i] <- mean(temp$NU_IDADE)
      nota_media[i] <- mean(temp$NT_GER)
      prop_homens[i] <- mean(temp$Sexo)
      prop_casados[i] <- mean(temp$Casado)
      prop_brancos[i] <- mean(temp$Branco)
      prop_renda_3SM[i] <- mean(temp$Renda_3SM)
      prop_medio_ou_mais[i] <- mean(temp$Medio_ou_mais)
    }
      }
  
      if (j == 2010) {
        
        for (i in seq(from = 1, length.out = 87, by = 1))
        {
          temp <- subset(final_federal_estadual,
                         CO_IES == unicos[i] & NU_ANO == j & CO_GRUPO == index)
          
          idade_media[i] <- mean(temp$NU_IDADE)
          nota_media[i] <- mean(temp$NT_GER)
          prop_homens[i] <- mean(temp$Sexo)
          prop_casados[i] <- mean(temp$Casado)
          prop_brancos[i] <- mean(temp$Branco)
          prop_renda_3SM[i] <- mean(temp$Renda_3SM)
          prop_medio_ou_mais[i] <- mean(temp$Medio_ou_mais)
        }
      }
      
      if (j == 2011) {
        
        for (i in seq(from = 1, length.out = 74, by = 1))
        {
          temp <- subset(final_federal_estadual,
                         CO_IES == unicos[i] & NU_ANO == j & CO_GRUPO == index)
          
          idade_media[i] <- mean(temp$NU_IDADE)
          nota_media[i] <- mean(temp$NT_GER)
          prop_homens[i] <- mean(temp$Sexo)
          prop_casados[i] <- mean(temp$Casado)
          prop_brancos[i] <- mean(temp$Branco)
          prop_renda_3SM[i] <- mean(temp$Renda_3SM)
          prop_medio_ou_mais[i] <- mean(temp$Medio_ou_mais)
        }
      }
      
      if (j == 2012) {
        
        for (i in seq(from = 70, length.out = 69, by = 1))
        {
          temp <- subset(final_federal_estadual,
                         CO_IES == unicos[i-69] & NU_ANO == j & CO_GRUPO == index)
          
          idade_media[i] <- mean(temp$NU_IDADE)
          nota_media[i] <- mean(temp$NT_GER)
          prop_homens[i] <- mean(temp$Sexo)
          prop_casados[i] <- mean(temp$Casado)
          prop_brancos[i] <- mean(temp$Branco)
          prop_renda_3SM[i] <- mean(temp$Renda_3SM)
          prop_medio_ou_mais[i] <- mean(temp$Medio_ou_mais)
        }
      }
      
      if (j == 2013) {
        
        for (i in seq(from = 88, length.out = 87, by = 1))
        {
          temp <- subset(final_federal_estadual,
                         CO_IES == unicos[i-87] & NU_ANO == j & CO_GRUPO == index)
          
          idade_media[i] <- mean(temp$NU_IDADE)
          nota_media[i] <- mean(temp$NT_GER)
          prop_homens[i] <- mean(temp$Sexo)
          prop_casados[i] <- mean(temp$Casado)
          prop_brancos[i] <- mean(temp$Branco)
          prop_renda_3SM[i] <- mean(temp$Renda_3SM)
          prop_medio_ou_mais[i] <- mean(temp$Medio_ou_mais)
        }
      }
      
      if (j == 2014) {
        
        for (i in seq(from = 75, length.out = 74, by = 1))
        {
          temp <- subset(final_federal_estadual,
                         CO_IES == unicos[i-74] & NU_ANO == j & CO_GRUPO == index)
          
          idade_media[i] <- mean(temp$NU_IDADE)
          nota_media[i] <- mean(temp$NT_GER)
          prop_homens[i] <- mean(temp$Sexo)
          prop_casados[i] <- mean(temp$Casado)
          prop_brancos[i] <- mean(temp$Branco)
          prop_renda_3SM[i] <- mean(temp$Renda_3SM)
          prop_medio_ou_mais[i] <- mean(temp$Medio_ou_mais)
        }
      }
}
    
    ## União inicial dos dados ##
    
    painel <- cbind(painel,
                    data.frame(idade_media = idade_media,
                               idade2_media = idade_media**2,
                               nota_media = nota_media,
                               prop_brancos = prop_brancos,
                               prop_casados = prop_casados,
                               prop_homens = prop_homens,
                               prop_medio_ou_mais = prop_medio_ou_mais,
                               prop_renda_3SM = prop_renda_3SM))

}

#### LEITURA DOS DADOS ####

final_federal_estadual <- readRDS("Dados trabalhados/final_federal_estadual.rds")
final_federal_estadual$CO_IES <- as.numeric(final_federal_estadual$CO_IES)
Codigos_IES <- read_excel("Dados/Codigos_IES.xlsx",)
Aderiram <- read_excel("Dados/Federais_que_aderiram.xlsx")
Aderiram$Nome <- toupper(Aderiram$Nome)

#### CRIAÇÃO DAS VARIÁVEIS MÉDIAS POR IES POR ANO  ####

#### 0) Únicos por ano - Áreas: Direito (2) e Ciências Econômicas (13) (2009-2012)####
  ##                            Medicina (12) e Agronomia (17) (2010-2013)
  ##                            História (Licenciatura) (2402) e Engenharia Química (6008) (2011-2014)

cod_areas <- c(2, 12, 13, 17, 2402, 6008)

unicos_2009 <- subset(final_federal_estadual, NU_ANO == 2009 & CO_GRUPO %in% cod_areas)
unicos_2009 <- unique(unicos_2009$CO_IES)

unicos_2010 <- subset(final_federal_estadual, NU_ANO == 2010 & CO_GRUPO %in% cod_areas)
unicos_2010 <- unique(unicos_2010$CO_IES)

unicos_2011 <- subset(final_federal_estadual, NU_ANO == 2011 & CO_GRUPO %in% cod_areas)
unicos_2011 <- unique(unicos_2011$CO_IES)

unicos_2012 <- subset(final_federal_estadual, NU_ANO == 2012 & CO_GRUPO %in% cod_areas)
unicos_2012 <- unique(unicos_2012$CO_IES)

unicos_2013 <- subset(final_federal_estadual, NU_ANO == 2013 & CO_GRUPO %in% cod_areas)
unicos_2013 <- unique(unicos_2013$CO_IES)

unicos_2014 <- subset(final_federal_estadual, NU_ANO == 2014 & CO_GRUPO %in% cod_areas)
unicos_2014 <- unique(unicos_2014$CO_IES)


unicos_2009_2012 <- Reduce(intersect, list(unicos_2009,
                                           unicos_2012))
unicos_2010_2013 <- Reduce(intersect, list(unicos_2010,
                                           unicos_2013))
unicos_2011_2014 <- Reduce(intersect, list(unicos_2011,
                                           unicos_2014))

#### 1) Subset dos anos das amostras e IES únicas ####

tamanho_2009_2012 <- as.numeric(length(unicos_2009_2012))
unicos_2009_2012 <- as.numeric(unicos_2009_2012)
anos_ciclo_1 <- c(2009, 2012)

tamanho_2010_2013 <- as.numeric(length(unicos_2010_2013))
unicos_2010_2013 <- as.numeric(unicos_2010_2013)
anos_ciclo_2 <- c(2010, 2013)

tamanho_2011_2014 <- as.numeric(length(unicos_2011_2014))
unicos_2011_2014 <- as.numeric(unicos_2011_2014)
anos_ciclo_3 <- c(2011, 2014)

painel_federal_estadual_2009_2012 <- data.frame(CO_IES = rep(NA, 2*tamanho_2009_2012),
                                                Ano = c(rep(2009, tamanho_2009_2012),
                                                        rep(2012, tamanho_2009_2012)))
painel_federal_estadual_2009_2012$CO_IES <- rep(unicos_2009_2012,2)

painel_federal_estadual_2010_2013 <- data.frame(CO_IES = rep(NA, 2*tamanho_2010_2013),
                                                Ano = c(rep(2010, tamanho_2010_2013),
                                                        rep(2013, tamanho_2010_2013)))
painel_federal_estadual_2010_2013$CO_IES <- rep(unicos_2010_2013,2)

painel_federal_estadual_2011_2014 <- data.frame(CO_IES = rep(NA, 2*tamanho_2011_2014),
                                                Ano = c(rep(2011, tamanho_2011_2014),
                                                        rep(2014, tamanho_2011_2014)))
painel_federal_estadual_2011_2014$CO_IES <- rep(unicos_2011_2014,2)

#### 2) Calculando os valores e criando os paineis ####

painel_direito <- painel_federal_estadual_2009_2012
painel_direito <- painel_direito %>% left_join(Codigos_IES, by = c("CO_IES" = "CO_IES"))
painel_direito <- painel_direito %>% left_join(Aderiram, by = c("Nome" = "Nome"))

painel_ciencias_economicas <- painel_federal_estadual_2009_2012
painel_ciencias_economicas <- painel_ciencias_economicas %>% left_join(Codigos_IES, by = c("CO_IES" = "CO_IES"))
painel_ciencias_economicas <- painel_ciencias_economicas %>% left_join(Aderiram, by = c("Nome" = "Nome"))

painel_medicina <- painel_federal_estadual_2010_2013
painel_medicina <- painel_medicina %>% left_join(Codigos_IES, by = c("CO_IES" = "CO_IES"))
painel_medicina <- painel_medicina %>% left_join(Aderiram, by = c("Nome" = "Nome"))

painel_agronomia <- painel_federal_estadual_2010_2013
painel_agronomia <- painel_agronomia %>% left_join(Codigos_IES, by = c("CO_IES" = "CO_IES"))
painel_agronomia <- painel_agronomia %>% left_join(Aderiram, by = c("Nome" = "Nome"))

painel_historia <- painel_federal_estadual_2011_2014
painel_historia <- painel_historia %>% left_join(Codigos_IES, by = c("CO_IES" = "CO_IES"))
painel_historia <- painel_historia %>% left_join(Aderiram, by = c("Nome" = "Nome"))

painel_engenharia_quimica <- painel_federal_estadual_2011_2014
painel_engenharia_quimica <- painel_engenharia_quimica %>% left_join(Codigos_IES, by = c("CO_IES" = "CO_IES"))
painel_engenharia_quimica <- painel_engenharia_quimica %>% left_join(Aderiram, by = c("Nome" = "Nome"))


painel_direito <- Criar_medias_area(unicos_2009_2012, anos_ciclo_1, 2, painel_direito)
painel_direito <- mutate(painel_direito, Tratamento = ifelse(is.na(painel_direito[,4]), 1, 0))
painel_direito <- painel_direito[, -4]
painel_direito <- na.omit(painel_direito)


painel_ciencias_economicas <- Criar_medias_area(unicos_2009_2012, anos_ciclo_1, 13, painel_ciencias_economicas)
painel_ciencias_economicas <- mutate(painel_ciencias_economicas, Tratamento = ifelse(is.na(painel_ciencias_economicas[,4]), 1, 0))
painel_ciencias_economicas <- painel_ciencias_economicas[, -4]
painel_ciencias_economicas <- na.omit(painel_ciencias_economicas)
painel_ciencias_economicas <- subset(painel_ciencias_economicas,
                                     Nome %notin% c("FUNDACAO UNIVERSIDADE FEDERAL DA GRANDE DOURADOS",
                                                    "UNIVERSIDADE FEDERAL DE OURO PRETO"))

painel_medicina <- Criar_medias_area(unicos_2010_2013, anos_ciclo_2, 12, painel_medicina)
painel_medicina <- mutate(painel_medicina, Tratamento = ifelse(is.na(painel_medicina[,4]), 1, 0))
painel_medicina <- painel_medicina[, -4]
painel_medicina <- na.omit(painel_medicina)
painel_medicina <- subset(painel_medicina,
                          Nome %notin% c("UNIVERSIDADE ESTADUAL DE PONTA GROSSA",
                                         "FUNDACAO UNIVERSIDADE FEDERAL DO VALE DO SAO FRANCISCO"))

painel_agronomia <- Criar_medias_area(unicos_2010_2013, anos_ciclo_2, 17, painel_agronomia)
painel_agronomia <- mutate(painel_agronomia, Tratamento = ifelse(is.na(painel_agronomia[,4]), 1, 0))
painel_agronomia <- painel_agronomia[, -4]
painel_agronomia <- na.omit(painel_agronomia)

painel_historia <- Criar_medias_area(unicos_2011_2014, anos_ciclo_3, 2402, painel_historia)
painel_historia <- mutate(painel_historia, Tratamento = ifelse(is.na(painel_historia[,4]), 1, 0))
painel_historia <- painel_historia[, -4]
painel_historia <- na.omit(painel_historia)
painel_historia <- subset(painel_historia,
                                  Nome != "FUNDACAO UNIVERSIDADE FEDERAL DO PAMPA UNIPAMPA")

painel_engenharia_quimica <- Criar_medias_area(unicos_2011_2014, anos_ciclo_3, 6008, painel_engenharia_quimica)
painel_engenharia_quimica <- mutate(painel_engenharia_quimica, Tratamento = ifelse(is.na(painel_engenharia_quimica[,4]), 1, 0))
painel_engenharia_quimica <- painel_engenharia_quimica[, -4]
painel_engenharia_quimica <- na.omit(painel_engenharia_quimica)
painel_engenharia_quimica <- subset(painel_engenharia_quimica,
                                  Nome %notin% c("UNIVERSIDADE ESTADUAL PAULISTA JULIO DE MESQUITA FILHO",
                                                 "UNIVERSIDADE FEDERAL DA PARAIBA",
                                                 "UNIVERSIDADE FEDERAL DE GOIAS",
                                                 "UNIVERSIDADE FEDERAL DE SAO JOAO DEL REI",
                                                 "UNIVERSIDADE FEDERAL DO AMAZONAS",
                                                 "UNIVERSIDADE FEDERAL DO ESPIRITO SANTO"))

# Criação da variável TEMPO

painel_direito$Tempo <- ifelse(painel_direito$Ano %in% c(2012,2013,2014), 1, 0)
painel_ciencias_economicas$Tempo <- ifelse(painel_ciencias_economicas$Ano %in% c(2012,2013,2014), 1, 0)
painel_medicina$Tempo <- ifelse(painel_medicina$Ano %in% c(2012,2013,2014), 1, 0)
painel_agronomia$Tempo <- ifelse(painel_agronomia$Ano %in% c(2012,2013,2014), 1, 0)
painel_historia$Tempo <- ifelse(painel_historia$Ano %in% c(2012,2013,2014), 1, 0)
painel_engenharia_quimica$Tempo <- ifelse(painel_engenharia_quimica$Ano %in% c(2012,2013,2014), 1, 0)

painel_direito$nota_media <- log(painel_direito$nota_media)
painel_ciencias_economicas$nota_media <- log(painel_ciencias_economicas$nota_media)
painel_medicina$nota_media <- log(painel_medicina$nota_media)
painel_agronomia$nota_media <- log(painel_agronomia$nota_media)
painel_historia$nota_media <- log(painel_historia$nota_media)
painel_engenharia_quimica$nota_media <- log(painel_engenharia_quimica$nota_media)

names_original <- colnames(painel_direito)

colnames(painel_direito) <- c("CO_IES", "Ano", "Nome", "Mean age",
                              "Mean age²", "log(Mean score)", "Prop. whites",
                              "Prop. married", "Prop. men", "Prop. mothers with highschool education or higher",
                              "Prop. students with family income of 3M.W. or less", 
                              "Treatment", "Tempo")

colnames(painel_ciencias_economicas) <- c("CO_IES", "Ano", "Nome", "Mean age",
                                          "Mean age²", "log(Mean score)", "Prop. whites",
                                          "Prop. married", "Prop. men", "Prop. mothers with highschool education or higher",
                                          "Prop. students with family income of 3M.W. or less", 
                                          "Treatment", "Tempo")

colnames(painel_medicina) <- c("CO_IES", "Ano", "Nome", "Mean age",
                               "Mean age²", "log(Mean score)", "Prop. whites",
                               "Prop. married", "Prop. men", "Prop. mothers with highschool education or higher",
                               "Prop. students with family income of 3M.W. or less", 
                               "Treatment", "Tempo")

colnames(painel_agronomia) <- c("CO_IES", "Ano", "Nome", "Mean age",
                                "Mean age²", "log(Mean score)", "Prop. whites",
                                "Prop. married", "Prop. men", "Prop. mothers with highschool education or higher",
                                "Prop. students with family income of 3M.W. or less", 
                                "Treatment", "Tempo")

colnames(painel_historia) <- c("CO_IES", "Ano", "Nome", "Mean age",
                               "Mean age²", "log(Mean score)", "Prop. whites",
                               "Prop. married", "Prop. men", "Prop. mothers with highschool education or higher",
                               "Prop. students with family income of 3M.W. or less", 
                               "Treatment", "Tempo")

colnames(painel_engenharia_quimica) <- c("CO_IES", "Ano", "Nome", "Mean age",
                                         "Mean age²", "log(Mean score)", "Prop. whites",
                                         "Prop. married", "Prop. men", "Prop. mothers with highschool education or higher",
                                         "Prop. students with family income of 3M.W. or less", 
                                         "Treatment", "Tempo")
#### ESTATÍSTICAS DESCRITIVAS ####


stargazer(painel_direito[,-c(1:3, 13)],
          type = "text", title = "Descriptive statistics - Law",
          decimal.mark = ".", digits = 3, header = FALSE,
          median = TRUE, iqr = FALSE,
          initial.zero = TRUE)

stargazer(painel_ciencias_economicas[,-c(1:3, 13)],
          type = "text", header = FALSE, title = "Descriptive statistics - Economics",
          decimal.mark = ".", digits = 3,
          median = TRUE, iqr = FALSE,
          initial.zero = TRUE)

stargazer(painel_medicina[,-c(1:3, 13)],
          type = "text", title = "Descriptive statistics - Medicine", header = FALSE,
          decimal.mark = ".", digits = 3,
          median = TRUE, iqr = FALSE,
          initial.zero = TRUE)

stargazer(painel_agronomia[,-c(1:3, 13)],
          type = "text", title = "Descriptive statistics - Agronomy", header = FALSE,
          decimal.mark = ".", digits = 3,
          median = TRUE, iqr = FALSE,
          initial.zero = TRUE)

stargazer(painel_historia[,-c(1:3, 13)],
          type = "text", title = "Descriptive statistics - History", header = FALSE,
          decimal.mark = ".", digits = 3,
          median = TRUE, iqr = FALSE,
          initial.zero = TRUE)

stargazer(painel_engenharia_quimica[,-c(1:3, 13)],
          type = "text", title = "Descriptive statistics - Chemical Engineering", header = FALSE,
          decimal.mark = ".", digits = 3,
          median = TRUE, iqr = FALSE,
          initial.zero = TRUE)

colnames(painel_direito) <- names_original
colnames(painel_ciencias_economicas) <- names_original
colnames(painel_medicina) <- names_original
colnames(painel_agronomia) <- names_original
colnames(painel_historia) <- names_original
colnames(painel_engenharia_quimica) <- names_original

#### EXPORTANDO OS PAINEIS ####

saveRDS(painel_direito, "Dados trabalhados/painel_direito.rds")
saveRDS(painel_ciencias_economicas, "Dados trabalhados/painel_ciencias_economicas.rds")
saveRDS(painel_medicina, "Dados trabalhados/painel_medicina.rds")
saveRDS(painel_agronomia, "Dados trabalhados/painel_agronomia.rds")
saveRDS(painel_historia, "Dados trabalhados/painel_historia.rds")
saveRDS(painel_engenharia_quimica, "Dados trabalhados/painel_engenharia_quimica.rds")

