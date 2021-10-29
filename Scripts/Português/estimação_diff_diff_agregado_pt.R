# ---- Pedro Milreu Cunha - Mestrando em Economia Aplicada - PPGE/UFPB ---- #

#### BIBLIOTECAS ####

library(dplyr)
library(plm)
library(cobalt)
library(WeightIt)
library(ebal)
library(stargazer)
library(ggplot2)
library(lmtest)
library(MatchIt)
library(readxl)
library(remotes)
library(cowplot)

# Configurando o tema dos gráficos ----

theme_new <- function(base_size = 10) {
  theme_bw(base_size = base_size) %+replace%
    theme(
      plot.title = element_text(size = rel(1), face = "bold", margin = margin(5,0,5,0), hjust = 0),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.title = element_text(size = rel(1), face = "bold"),
      axis.text = element_text(size = rel(1)),
      axis.line = element_line(color = "black", arrow = arrow(length = unit(0.3, "lines"), type = "closed")),
      legend.title = element_text(size = rel(1), face = "bold"),
      legend.text = element_text(size = rel(1)),
      legend.key = element_rect(fill = "transparent", colour = NA),
      legend.key.size = unit(1.5, "lines"),
      legend.background = element_rect(fill = "transparent", colour = NA),
      strip.background = element_rect(fill = "transparent", color = NA),
      strip.text = element_text(size = rel(1), color = "black", margin = margin(5,0,5,0))
    )
}
theme_set(theme_new())

#### LEITURA DOS DADOS ####

painel_federal_estadual <- readRDS("Dados trabalhados/painel_federal_estadual.rds")
painel_federal_estadual$Tratamento <- as.factor(painel_federal_estadual$Tratamento)

#### GRÁFICO DE DENSIDADE DAS NOTAS MÉDIAS ####

grafico_densidade_notas <- ggplot(painel_federal_estadual, aes(x = nota_media, fill = Tratamento))+
  geom_density(alpha = 0.4) +
  labs(x = "Nota média", y = "Densidade", fill = "Grupo")+
  scale_fill_manual(labels=c("Controle", "Tratamento"),
                    values = c("grey", "black")) +
  theme(panel.grid = element_blank())
#ggsave(filename = "Figuras/Português/grafico_densidade_notas.png")

painel_federal_estadual$log_nota <- log(painel_federal_estadual$nota_media)

grafico_densidade_log_notas <- ggplot(painel_federal_estadual, aes(x = log_nota, fill = Tratamento))+
  geom_density(alpha = 0.4) +
  labs(x = "ln(Nota média)", y = "Densidade", fill = "Grupo")+
  scale_fill_manual(labels=c("Controle", "Tratamento"),
                    values = c("grey", "black")) +
  theme(panel.grid = element_blank())

#ggsave(filename = "Figuras/Português/grafico_densidade_log_notas.png")

#### NOMES DAS VARIÁVEIS ####

names <- data.frame(old = c("idade_media", "idade2_media", "prop_brancos",
                            "prop_casados", "prop_homens", "prop_medio_ou_mais",
                            "prop_renda_3SM"),
                    new = c("Idade média", "Idade média²",
                            "Prop. de brancos", "Prop. de casados",
                            "Prop. de homens", "Prop. de mães com ensino médio ou mais",
                            "Prop. de estudantes com renda familiar de 3S.M. ou menos"))

names_placebo <- data.frame(old = c("prop_casados", "prop_brancos",
                                    "ln_nota", "prop_homens", "prop_medio_ou_mais",
                                    "prop_renda_3SM"),
                            new = c("Prop. de casadps",
                                    "Prop. de brancos", "ln(Nota média)",
                                    "Prop. de homens", "Prop. de mães com ensino médio ou mais",
                                    "Prop. de estudantes com renda familiar de 3S.M. ou menos"))


#### HIPÓTESE DAS TENDÊNCIAS PARALELAS ####

tendencias <- ggplot(painel_federal_estadual, aes(Ano, log_nota, color = Tratamento))+
  stat_summary(geom = 'line', size = 1) +
  labs(x = "Ano", y = "ln(Nota média)", color = "Grupo")+
  geom_vline(xintercept = 2012, linetype = "dashed", col = "black", size = 0.75) +
  scale_colour_manual(labels=c("Controle", "Tratamento"),
                      values = c("grey", "black"))+
  theme(panel.grid = element_blank())

#ggsave(file = "Figuras/Português/tendencias.eps")

#### ESTIMAÇÕES - **CICLO 2009-2012** ####

painel_federal_estadual_2009_2012 <- subset(painel_federal_estadual,
                                            Ano %in% c(2009, 2012))

federal_estadual_2009 <- subset(painel_federal_estadual_2009_2012, Ano == 2009)

#### 1.1) Hipótese do suporte comum ####

ps <- glm(data = painel_federal_estadual_2009_2012,
          Tratamento ~ idade_media + idade2_media + prop_brancos +
            prop_casados + prop_homens + prop_medio_ou_mais + 
            prop_renda_3SM, family = binomial(link = "logit"))

painel_federal_estadual_2009_2012$pscore <- predict(ps, type = "response")

grafico_suporte_comum_2009_2012 <- ggplot(painel_federal_estadual_2009_2012,
                                          aes(x = pscore, fill = Tratamento))+
                                  geom_density(alpha = 0.50) +
                                  labs(x = "Propensity score",
                                       y = "Densidade", 
                                       fill = "Grupo") +
                                  scale_fill_manual(labels=c("Controle", "Tratamento"),
                                                    values = c("grey","black")) +
                                  theme(panel.grid = element_blank())

#ggsave(file = "Figuras/Português/suporte_comum_2009_2012.png")

#### 1.2) Suporte comum por quantil #### 

painel_federal_estadual_2009_2012$quantil = with(painel_federal_estadual_2009_2012,
                                                 cut(pscore,
                                                     quantile(pscore,
                                                              prob = seq(0, 0.984, length = 5),
                                                              type = 5)))

media_por_quantil_2009_2012 <- subset(painel_federal_estadual_2009_2012, !is.na(quantil)) %>%
  group_by(quantil, Tratamento) %>%
  summarise_at(vars(pscore), funs(mean(., na.rm=TRUE)))

grafico_suporte_comum_2009_2012_quantil <- ggplot(subset(painel_federal_estadual_2009_2012, !is.na(quantil)),
                                                  aes(x = pscore, fill = Tratamento)) +
                                           geom_density(alpha = 0.4) +
                                           labs(x = "Propensity score",
                                                y = "Densidade", 
                                                fill = "Grupo")+
                                           scale_fill_manual(labels = c("Controle", "Tratamento"),
                                                             values = c("grey","black")) +
                                           facet_grid(quantil ~ .) +
                                           theme(panel.grid = element_blank())


#ggsave(file = "Figuras/Português/suporte_comum_2009_2012_quantis.png")

#### 2) Modelo diff-diff simples ####

didreg = lm(log_nota ~ Tratamento + Tempo + Tempo*Tratamento + idade_media +
              idade2_media + prop_brancos + prop_casados + prop_homens + 
              prop_medio_ou_mais + prop_renda_3SM,
            data = painel_federal_estadual_2009_2012)

summary(didreg)

#### 3) Modelo diff-and-diff com efeitos fixos ####

didreg_fe <- plm(log_nota ~ Tempo + Tempo*Tratamento + idade_media +
                   idade2_media + prop_brancos + prop_casados + prop_homens + 
                   prop_medio_ou_mais + prop_renda_3SM,
                 model = "within", index = c("CO_IES", "Ano"),
                 data = painel_federal_estadual_2009_2012)

summary(didreg_fe)

#### 4) Modelo diff-and-diff com efeitos fixos e PSM ####

#### 4.1) Criação dos pesos utilizando o modelo logit multinomial #----

W1 = weightit(Tratamento ~ idade_media + idade2_media  + 
                prop_brancos + prop_casados + prop_homens + 
                prop_medio_ou_mais + prop_renda_3SM,
              data = federal_estadual_2009,
              method = "ps",
              estimand = "ATT")

#### 4.1.1) Checagem do balanceamento #====

b1 = bal.tab(W1, un = TRUE,
             m.threshold=0.1,
             v.threshold=3)
print(b1)

g1_2009_2012 <- love.plot(W1, stats = c("m"), drop.distance = TRUE,
                          thresholds = c(m=0.1),
                          wrap = 10,
                          var.order = "unadjusted",
                          sample.names = c("Desbalanceado", "Balanceado - PS"),
                          title = NULL,
                          subtitle = "Amostra", line= T,
                          var.names = names,
                          colors = c("grey", "black"),
                          shapes = c("triangle filled", "circle filled"))

g1_2009_2012 <- g1_2009_2012 + 
                scale_color_manual(name = "Amostra", values = c("grey", "black")) + 
                scale_shape_manual(name = "Amostra", values = c("triangle filled", "circle filled")) + 
                guides(group = FALSE, size = FALSE, stroke = FALSE) + 
                xlab("Diferenças médias \n padronizadas")

#### 4.2) Criação dos pesos utilizando o modelo de entropia #----

W2 = weightit(Tratamento ~ idade_media + idade2_media + 
                prop_brancos + prop_casados + prop_homens + 
                prop_medio_ou_mais + prop_renda_3SM,
              data = federal_estadual_2009,
              method = "ebal",
              estimand = "ATT")

W2_placebo <- weightit(Tratamento ~ prop_casados + 
                         prop_brancos + log_nota + prop_homens + 
                         prop_medio_ou_mais + prop_renda_3SM,
                       data = federal_estadual_2009,
                       method = "ebal",
                       estimand = "ATT")


#### 4.2.1) Checagem do balanceamento #====

b2 = bal.tab(W2, un = TRUE,
             m.threshold=0.1,
             v.threshold=3)
print(b2)

g2_2009_2012 <- love.plot(W2, stats = c("m"), drop.distance = TRUE,
                          thresholds = c(m=0.1, v = 0.05),
                          wrap = 10,
                          var.order = "unadjusted",
                          sample.names = c("Desbalanceado", "Balanceado - Entropia"),
                          title = NULL,
                          subtitle = "Amostra", line= T,
                          var.names = names,
                          colors = c("grey", "black"),
                          shapes = c("triangle filled", "circle filled"))

g2_2009_2012 <- g2_2009_2012 + 
                scale_color_manual(name = "Amostra", values = c("grey", "black")) + 
                scale_shape_manual(name = "Amostra", values = c("triangle filled", "circle filled")) + 
                guides(group = FALSE, size = FALSE, stroke = FALSE) + 
                xlab("Diferenças médias \n padronizadas")

b2_placebo = bal.tab(W2_placebo, un = TRUE,
                     m.threshold=0.1,
                     v.threshold=3)
print(b2_placebo)

g3_2009_2012 <- love.plot(W2_placebo, stats = c("m"), drop.distance = TRUE,
                          thresholds = c(m=0.1, v = 0.05),
                          wrap = 10,
                          var.order = "unadjusted",
                          sample.names = c("Desbalanceado", "Balanceado - Entropia"),
                          title = NULL,
                          subtitle = "Amostra", line= T,
                          var.names = names_placebo,
                          colors = c("grey", "black"),
                          shapes = c("triangle filled", "circle filled"))

g3_2009_2012 <- g3_2009_2012 + 
                scale_color_manual(name = "Amostra", values = c("grey", "black")) + 
                scale_shape_manual(name = "Amostra", values = c("triangle filled", "circle filled")) + 
                guides(group = FALSE, size = FALSE, stroke = FALSE) + 
                xlab("Diferenças médias \n padronizadas")


#### 5) Unindo os pesos ao data frame ####

W1_matrix <- data.frame(CO_IES = federal_estadual_2009$CO_IES,
                        W1 = W1$weights)

painel_federal_estadual_2009_2012_logit <- left_join(painel_federal_estadual_2009_2012,
                                                     W1_matrix, by = "CO_IES")

W2_matrix <- data.frame(CO_IES = federal_estadual_2009$CO_IES,
                        W2 = W2$weights)

painel_federal_estadual_2009_2012_entropia <- left_join(painel_federal_estadual_2009_2012,
                                                        W2_matrix, by = "CO_IES")

W2_placebo_matrix <- data.frame(CO_IES = federal_estadual_2009$CO_IES,
                                W2_placebo = W2_placebo$weights)

painel_federal_estadual_2009_2012_entropia_placebo <- left_join(painel_federal_estadual_2009_2012,
                                                                W2_placebo_matrix, by = "CO_IES")


#### 6) Regressão - Pesos calculados utilizando logit multinomial #####

didreg_fe_logit <- plm(log_nota ~ Tratamento + Tempo + Tempo*Tratamento + idade_media +
                         idade2_media + prop_brancos + prop_casados + prop_homens + 
                         prop_medio_ou_mais + prop_renda_3SM,
                       model = "within", index = c("CO_IES", "Ano"), weights = W1,
                       data = painel_federal_estadual_2009_2012_logit)

summary(didreg_fe_logit)

#### 7) Regressão - Pesos calculados utilizando entropia ####

didreg_fe_entropia <- plm(log_nota ~ Tratamento + Tempo + Tempo*Tratamento + idade_media +
                            idade2_media + prop_brancos + prop_casados + prop_homens + 
                            prop_medio_ou_mais + prop_renda_3SM,
                          model = "within", index = c("CO_IES", "Ano"), weights = W2,
                          data = painel_federal_estadual_2009_2012_entropia)

summary(didreg_fe_entropia)

#### 8) Placebo - Tratamento sobre idade média -  Pesos calculados utilizando entropia ####

didreg_fe_entropia_placebo <- plm(idade_media ~ Tratamento + Tempo + Tempo*Tratamento + prop_casados +
                                    prop_brancos + nota_media + prop_homens + 
                                    prop_medio_ou_mais + prop_renda_3SM,
                                  model = "within", index = c("CO_IES", "Ano"), weights = W2_placebo,
                                  data = painel_federal_estadual_2009_2012_entropia_placebo)

summary(didreg_fe_entropia_placebo)

#### 9) Visualizando os resultados ####

stargazer(didreg, didreg_fe, didreg_fe_logit, didreg_fe_entropia,
          type = "text",
          title = "Efeito médio de tratamento sobre os tratados - Ciclo 2009-2012",
          decimal.mark = ",", digits = 5,
          column.labels = c("Diff-diff",
                            "Diff-diff + FE",
                            "Diff-diff + FE + PSM (logit)",
                            "Diff-diff + FE + PSM (entropia)"),
          model.numbers = FALSE,
          flip = TRUE, align = TRUE,
          float = TRUE, float.env = "sidewaystable",
          initial.zero = TRUE, intercept.bottom = TRUE,
          dep.var.labels = "ln(Nota média)",
          multicolumn = TRUE,model.names = FALSE)

stargazer(didreg_fe_entropia_placebo,
          type = "text", out = "Resultados/ciclo_2009_2012_placebo.txt",
          title = "Placebo - Tendências paralelas - Ciclo 2009-2012",
          decimal.mark = ",", digits = 3,
          model.numbers = TRUE,
          align = TRUE,
          float = TRUE, float.env = "table",
          initial.zero = TRUE, intercept.bottom = TRUE,
          multicolumn = TRUE, model.names = FALSE,
          font.size = "small")

#### ESTIMAÇÕES - **CICLO 2010-2013** ####

painel_federal_estadual_2010_2013 <- subset(painel_federal_estadual,
                                            Ano %in% c(2010, 2013))

federal_estadual_2010 <- subset(painel_federal_estadual_2010_2013, Ano == 2010)

#### 1.1) Hipótese do suporte comum ####

ps <- glm(data = painel_federal_estadual_2010_2013,
          Tratamento ~ idade_media + idade2_media + prop_brancos +
            prop_casados + prop_homens + prop_medio_ou_mais + 
            prop_renda_3SM, family = binomial(link = "logit"))

painel_federal_estadual_2010_2013$pscore <- predict(ps, type = "response")

grafico_suporte_comum_2010_2013 <- ggplot(painel_federal_estadual_2010_2013,
                                          aes(x = pscore, fill = Tratamento))+
                                   geom_density(alpha = 0.4) +
                                   labs(x = "Propensity score",
                                        y = "Densidade", 
                                        fill = "Grupo") +
                                   scale_fill_manual(labels=c("Controle", "Tratamento"),
                                                     values = c("grey", "black")) +
                                   theme(panel.grid = element_blank())

#ggsave(file = "Figuras/Português/suporte_comum_2010_2013.png")

#### 1.2) Suporte comum por quantil #### 

painel_federal_estadual_2010_2013$quantil = with(painel_federal_estadual_2010_2013,
                                                 cut(pscore,
                                                     quantile(pscore,
                                                              prob = seq(0, 0.984, length = 5),
                                                              type = 5)))

media_por_quantil_2010_2013 <- subset(painel_federal_estadual_2010_2013, !is.na(quantil)) %>%
  group_by(quantil, Tratamento) %>%
  summarise_at(vars(pscore), funs(mean(., na.rm=TRUE)))

grafico_suporte_comum_2010_2013_quantil <- ggplot(subset(painel_federal_estadual_2010_2013, !is.na(quantil)),
                                                  aes(x = pscore, fill = Tratamento))+
                                           geom_density(alpha = 0.4) +
                                           labs(x = "Propensity score",
                                                y = "Densidade", 
                                                fill = "Grupo")+
                                           scale_fill_manual(labels=c("Controle", "Tratamento"),
                                                             values = c("grey", "black")) +
                                           facet_grid(quantil ~ .) +
                                           theme(panel.grid = element_blank())


#ggsave(file = "Figuras/Português/suporte_comum_2010_2013_quantis.png")

#### 2) Modelo diff-diff simples ####

didreg_2 = lm(log_nota ~ Tratamento + Tempo + Tempo*Tratamento + idade_media +
                idade2_media + prop_brancos + prop_casados + prop_homens + 
                prop_medio_ou_mais + prop_renda_3SM,
              data = painel_federal_estadual_2010_2013)

summary(didreg_2)

#### 3) Modelo diff-and-diff com efeitos fixos ####

didreg_fe_2 <- plm(log_nota ~ Tempo + Tempo*Tratamento + idade_media +
                     idade2_media + prop_brancos + prop_casados + prop_homens + 
                     prop_medio_ou_mais + prop_renda_3SM,
                   model = "within", index = c("CO_IES", "Ano"),
                   data = painel_federal_estadual_2010_2013)

summary(didreg_fe_2)

#### 4) Modelo diff-and-diff com efeitos fixos e PSM ####

#### 4.1) Criação dos pesos utilizando o modelo logit multinomial #----

W1 = weightit(Tratamento ~ idade_media + idade2_media  + 
                prop_brancos + prop_casados + prop_homens + 
                prop_medio_ou_mais + prop_renda_3SM,
              data = federal_estadual_2010,
              method = "ps",
              estimand = "ATT")

#### 4.1.1) Checagem do balanceamento #====

b1 = bal.tab(W1, un = TRUE,
             m.threshold=0.1,
             v.threshold=3)
print(b1)

g1_2010_2013 <- love.plot(W1, stats = c("m"), drop.distance = TRUE,
                          thresholds = c(m=0.1, v = 0.05),
                          wrap = 10,
                          var.order = "unadjusted",
                          sample.names = c("Desbalanceado", "Balanceado - PS"),
                          title = NULL,
                          subtitle = "Amostra", line= T,
                          var.names = names,
                          colors = c("grey", "black"),
                          shapes = c("triangle filled", "circle filled"))

g1_2010_2013 <- g1_2010_2013 + 
                scale_color_manual(name = "Amostra", values = c("grey", "black")) + 
                scale_shape_manual(name = "Amostra", values = c("triangle filled", "circle filled")) + 
                guides(group = FALSE, size = FALSE, stroke = FALSE) + 
                xlab("Diferenças médias \n padronizadas")

#### 4.2) Criação dos pesos utilizando o modelo de entropia #----

W2 = weightit(Tratamento ~ idade_media + idade2_media + 
                prop_brancos + prop_casados + prop_homens + 
                prop_medio_ou_mais + prop_renda_3SM,
              data = federal_estadual_2010,
              method = "ebal",
              estimand = "ATT")

W2_placebo <- weightit(Tratamento ~ prop_casados + 
                         prop_brancos + log_nota + prop_homens + 
                         prop_medio_ou_mais + prop_renda_3SM,
                       data = federal_estadual_2010,
                       method = "ebal",
                       estimand = "ATT")


#### 4.2.1) Checagem do balanceamento #====

b2 = bal.tab(W2, un = TRUE,
             m.threshold=0.1,
             v.threshold=3)
print(b2)

g2_2010_2013 <- love.plot(W2, stats = c("m"), drop.distance = TRUE,
                          thresholds = c(m=0.1, v = 0.05),
                          wrap = 10,
                          var.order = "unadjusted",
                          sample.names = c("Desbalanceado", "Balanceado - Entropia"),
                          title = NULL,
                          subtitle = "Amostra", line= T,
                          var.names = names,
                          colors = c("grey", "black"),
                          shapes = c("triangle filled", "circle filled"))

g2_2010_2013 <- g2_2010_2013 + 
                scale_color_manual(name = "Amostra", values = c("grey", "black")) + 
                scale_shape_manual(name = "Amostra", values = c("triangle filled", "circle filled")) + 
                guides(group = FALSE, size = FALSE, stroke = FALSE) + 
                xlab("Diferenças médias \n padronizadas")

b2_placebo = bal.tab(W2_placebo, un = TRUE,
                     m.threshold=0.1,
                     v.threshold=3)
print(b2_placebo)

g3_2010_2013 <- love.plot(W2_placebo, stats = c("m"), drop.distance = TRUE,
                          thresholds = c(m=0.1, v = 0.05),
                          wrap = 10,
                          var.order = "unadjusted",
                          sample.names = c("Desbalanceado", "Balanceado - Entropia"),
                          title = NULL,
                          subtitle = "Amostra", line= T,
                          var.names = names_placebo,
                          colors = c("grey", "black"),
                          shapes = c("triangle filled", "circle filled"))

g3_2010_2013 <- g3_2010_2013 + 
                scale_color_manual(name = "Amostra", values = c("grey", "black")) + 
                scale_shape_manual(name = "Amostra", values = c("triangle filled", "circle filled")) + 
                guides(group = FALSE, size = FALSE, stroke = FALSE) + 
                xlab("Diferenças médias \n padronizadas")

#### 5) Unindo os pesos ao data frame ####

W1_matrix <- data.frame(CO_IES = federal_estadual_2010$CO_IES,
                        W1 = W1$weights)

painel_federal_estadual_2010_2013_logit <- left_join(painel_federal_estadual_2010_2013,
                                                     W1_matrix, by = "CO_IES")

W2_matrix <- data.frame(CO_IES = federal_estadual_2010$CO_IES,
                        W2 = W2$weights)

painel_federal_estadual_2010_2013_entropia <- left_join(painel_federal_estadual_2010_2013,
                                                        W2_matrix, by = "CO_IES")

W2_placebo_matrix <- data.frame(CO_IES = federal_estadual_2010$CO_IES,
                                W2_placebo = W2_placebo$weights)

painel_federal_estadual_2010_2013_entropia_placebo <- left_join(painel_federal_estadual_2010_2013,
                                                                W2_placebo_matrix, by = "CO_IES")


#### 6) Regressão - Pesos calculados utilizando logit multinomial #####

didreg_fe_logit_2 <- plm(log_nota ~ Tratamento + Tempo + Tempo*Tratamento + idade_media +
                           idade2_media + prop_brancos + prop_casados + prop_homens + 
                           prop_medio_ou_mais + prop_renda_3SM,
                         model = "within", index = c("CO_IES", "Ano"), weights = W1,
                         data = painel_federal_estadual_2010_2013_logit)

summary(didreg_fe_logit_2)

#### 7) Regressão - Pesos calculados utilizando entropia ####

didreg_fe_entropia_2 <- plm(log_nota ~ Tratamento + Tempo + Tempo*Tratamento + idade_media +
                              idade2_media + prop_brancos + prop_casados + prop_homens + 
                              prop_medio_ou_mais + prop_renda_3SM,
                            model = "within", index = c("CO_IES", "Ano"), weights = W2,
                            data = painel_federal_estadual_2010_2013_entropia)

summary(didreg_fe_entropia_2)

#### 8) Placebo - Tratamento sobre idade média -  Pesos calculados utilizando entropia ####

didreg_fe_entropia_placebo_2 <- plm(idade_media ~ Tratamento + Tempo + Tempo*Tratamento + prop_casados +
                                      prop_brancos + nota_media + prop_homens + 
                                      prop_medio_ou_mais + prop_renda_3SM,
                                    model = "within", index = c("CO_IES", "Ano"), weights = W2_placebo,
                                    data = painel_federal_estadual_2010_2013_entropia_placebo)

summary(didreg_fe_entropia_placebo_2)

#### 9) Visualizando os resultados ####

stargazer(didreg_2, didreg_fe_2, didreg_fe_logit_2, didreg_fe_entropia_2,
          type = "text",
          title = "Efeito médio de tratamento sobre os tratados - Ciclo 2010-2013",
          decimal.mark = ",", digits = 3,
          column.labels = c("Diff-diff",
                            "Diff-diff + FE",
                            "Diff-diff + FE + PSM (logit)",
                            "Diff-diff + FE + PSM (entropia)"),
          model.numbers = FALSE,
          flip = TRUE, align = TRUE,
          float = TRUE, float.env = "sidewaystable",
          initial.zero = TRUE, intercept.bottom = TRUE,
          dep.var.labels = "ln(Nota média)",
          multicolumn = TRUE,model.names = FALSE)

stargazer(didreg_fe_entropia_placebo_2,
          type = "text", out = "Resultados/ciclo_2010_2013_placebo.txt",
          title = "Placebo - Tendências paralelas - Ciclo 2010-2013",
          decimal.mark = ",", digits = 3,
          model.numbers = TRUE,
          align = TRUE,
          float = TRUE, float.env = "table",
          initial.zero = TRUE, intercept.bottom = TRUE,
          multicolumn = TRUE, model.names = FALSE,
          font.size = "small")


#### ESTIMAÇÕES - **CICLO 2011-2014** ####

painel_federal_estadual_2011_2014 <- subset(painel_federal_estadual,
                                            Ano %in% c(2011, 2014))

federal_estadual_2011 <- subset(painel_federal_estadual_2011_2014, Ano == 2011)

#### 1.1) Hipótese do suporte comum ####

ps <- glm(data = painel_federal_estadual_2011_2014,
          Tratamento ~ idade_media + idade2_media + prop_brancos +
            prop_casados + prop_homens + prop_medio_ou_mais + 
            prop_renda_3SM, family = binomial(link = "logit"))

painel_federal_estadual_2011_2014$pscore <- predict(ps, type = "response")

grafico_suporte_comum_2011_2014 <- ggplot(painel_federal_estadual_2011_2014,
                                          aes(x = pscore, fill = Tratamento))+
                                   geom_density(alpha = 0.4) +
                                   labs(x = "Propensity score",
                                        y = "Densidade", 
                                        fill = "Grupo") +
                                   scale_fill_manual(labels=c("Controle", "Tratamento"),
                                                     values = c("grey", "black")) +
                                   theme(panel.grid = element_blank())

#ggsave(file = "Figuras/Português/suporte_comum_2011_2014.png")

#### 1.2) Suporte comum por quantil #### 

painel_federal_estadual_2011_2014$quantil = with(painel_federal_estadual_2011_2014,
                                                 cut(pscore,
                                                     quantile(pscore,
                                                              prob = seq(0, 0.92848, length = 5),
                                                              type = 5)))

media_por_quantil_2011_2014 <- subset(painel_federal_estadual_2011_2014, !is.na(quantil)) %>%
  group_by(quantil, Tratamento) %>%
  summarise_at(vars(pscore), funs(mean(., na.rm=TRUE)))

grafico_suporte_comum_2011_2014_quantil <- ggplot(subset(painel_federal_estadual_2011_2014, !is.na(quantil)),
                                                  aes(x = pscore, fill = Tratamento))+
                                           geom_density(alpha = 0.4) +
                                           labs(x = "Propensity score",
                                                y = "Densidade", 
                                                fill = "Grupo")+
                                           scale_fill_manual(labels=c("Controle", "Tratamento"),
                                                             values = c("grey", "black")) +
                                           facet_grid(quantil ~ .) +
                                           theme(panel.grid = element_blank())


#ggsave(file = "Figuras/Português/suporte_comum_2011_2014_quantis.png")

#### 2) Modelo diff-diff simples ####

didreg_2 = lm(log_nota ~ Tratamento + Tempo + Tempo*Tratamento + idade_media +
                idade2_media + prop_brancos + prop_casados + prop_homens + 
                prop_medio_ou_mais + prop_renda_3SM,
              data = painel_federal_estadual_2011_2014)

summary(didreg_2)

#### 3) Modelo diff-and-diff com efeitos fixos ####

didreg_fe_2 <- plm(log_nota ~ Tempo + Tempo*Tratamento + idade_media +
                     idade2_media + prop_brancos + prop_casados + prop_homens + 
                     prop_medio_ou_mais + prop_renda_3SM,
                   model = "within", index = c("CO_IES", "Ano"),
                   data = painel_federal_estadual_2011_2014)

summary(didreg_fe_2)

#### 4) Modelo diff-and-diff com efeitos fixos e PSM ####

#### 4.1) Criação dos pesos utilizando o modelo logit multinomial #----

W1 = weightit(Tratamento ~ idade_media + idade2_media  + 
                prop_brancos + prop_casados + prop_homens + 
                prop_medio_ou_mais + prop_renda_3SM,
              data = federal_estadual_2011,
              method = "ps",
              estimand = "ATT")

#### 4.1.1) Checagem do balanceamento #====

b1 = bal.tab(W1, un = TRUE,
             m.threshold=0.1,
             v.threshold=3)
print(b1)

g1_2011_2014 <- love.plot(W1, stats = c("m"), drop.distance = TRUE,
                          thresholds = c(m=0.1, v = 0.05),
                          wrap = 10,
                          var.order = "unadjusted",
                          sample.names = c("Desbalanceado", "Balanceado - PS"),
                          title = NULL,
                          subtitle = "Amostra", line= T,
                          var.names = names,
                          colors = c("grey", "black"),
                          shapes = c("triangle filled", "circle filled"))

g1_2011_2014 <- g1_2011_2014 + 
                scale_color_manual(name = "Amostra", values = c("grey", "black")) + 
                scale_shape_manual(name = "Amostra", values = c("triangle filled", "circle filled")) + 
                guides(group = FALSE, size = FALSE, stroke = FALSE) + 
                xlab("Diferenças médias \n padronizadas")

#### 4.2) Criação dos pesos utilizando o modelo de entropia #----

W2 = weightit(Tratamento ~ idade_media + idade2_media + 
                prop_brancos + prop_casados + prop_homens + 
                prop_medio_ou_mais + prop_renda_3SM,
              data = federal_estadual_2011,
              method = "ebal",
              estimand = "ATT")

W2_placebo <- weightit(Tratamento ~ prop_casados + 
                         prop_brancos + log_nota + prop_homens + 
                         prop_medio_ou_mais + prop_renda_3SM,
                       data = federal_estadual_2011,
                       method = "ebal",
                       estimand = "ATT")


#### 4.2.1) Checagem do balanceamento #====

b2 = bal.tab(W2, un = TRUE,
             m.threshold=0.1,
             v.threshold=3)
print(b2)

g2_2011_2014 <- love.plot(W2, stats = c("m"), drop.distance = TRUE,
                          thresholds = c(m=0.1, v = 0.05),
                          wrap = 10,
                          var.order = "unadjusted",
                          sample.names = c("Desbalanceado", "Balanceado - Entropia"),
                          title = NULL,
                          subtitle = "Amostra", line= T,
                          var.names = names,
                          colors = c("grey", "black"),
                          shapes = c("triangle filled", "circle filled"))

g2_2011_2014 <- g2_2011_2014 + 
                scale_color_manual(name = "Amostra", values = c("grey", "black")) + 
                scale_shape_manual(name = "Amostra", values = c("triangle filled", "circle filled")) + 
                guides(group = FALSE, size = FALSE, stroke = FALSE) + 
                xlab("Diferenças médias \n padronizadas")

b2_placebo = bal.tab(W2_placebo, un = TRUE,
                     m.threshold=0.1,
                     v.threshold=3)
print(b2_placebo)

g3_2011_2014 <- love.plot(W2_placebo, stats = c("m"), drop.distance = TRUE,
                          thresholds = c(m=0.1, v = 0.05),
                          wrap = 10,
                          var.order = "unadjusted",
                          sample.names = c("Desbalanceado", "Balanceado - Entropia"),
                          title = NULL,
                          subtitle = "Amostra", line= T,
                          var.names = names_placebo,
                          colors = c("grey", "black"),
                          shapes = c("triangle filled", "circle filled"))

g3_2011_2014 <- g3_2011_2014 + 
                scale_color_manual(name = "Amostra", values = c("grey", "black")) + 
                scale_shape_manual(name = "Amostra", values = c("triangle filled", "circle filled")) + 
                guides(group = FALSE, size = FALSE, stroke = FALSE) + 
                xlab("Diferenças médias \n padronizadas")


#### 5) Unindo os pesos ao data frame ####

W1_matrix <- data.frame(CO_IES = federal_estadual_2011$CO_IES,
                        W1 = W1$weights)

painel_federal_estadual_2011_2014_logit <- left_join(painel_federal_estadual_2011_2014,
                                                     W1_matrix, by = "CO_IES")

W2_matrix <- data.frame(CO_IES = federal_estadual_2011$CO_IES,
                        W2 = W2$weights)

painel_federal_estadual_2011_2014_entropia <- left_join(painel_federal_estadual_2011_2014,
                                                        W2_matrix, by = "CO_IES")

W2_placebo_matrix <- data.frame(CO_IES = federal_estadual_2011$CO_IES,
                                W2_placebo = W2_placebo$weights)

painel_federal_estadual_2011_2014_entropia_placebo <- left_join(painel_federal_estadual_2011_2014,
                                                                W2_placebo_matrix, by = "CO_IES")


#### 6) Regressão - Pesos calculados utilizando logit multinomial #####

didreg_fe_logit_2 <- plm(log_nota ~ Tratamento + Tempo + Tempo*Tratamento + idade_media +
                           idade2_media + prop_brancos + prop_casados + prop_homens + 
                           prop_medio_ou_mais + prop_renda_3SM,
                         model = "within", index = c("CO_IES", "Ano"), weights = W1,
                         data = painel_federal_estadual_2011_2014_logit)

summary(didreg_fe_logit_2)

#### 7) Regressão - Pesos calculados utilizando entropia ####

didreg_fe_entropia_2 <- plm(log_nota ~ Tratamento + Tempo + Tempo*Tratamento + idade_media +
                              idade2_media + prop_brancos + prop_casados + prop_homens + 
                              prop_medio_ou_mais + prop_renda_3SM,
                            model = "within", index = c("CO_IES", "Ano"), weights = W2,
                            data = painel_federal_estadual_2011_2014_entropia)

summary(didreg_fe_entropia_2)

#### 8) Placebo - Tratamento sobre idade média -  Pesos calculados utilizando entropia ####

didreg_fe_entropia_placebo_2 <- plm(idade_media ~ Tratamento + Tempo + Tempo*Tratamento + prop_casados +
                                      prop_brancos + nota_media + prop_homens + 
                                      prop_medio_ou_mais + prop_renda_3SM,
                                    model = "within", index = c("CO_IES", "Ano"), weights = W2_placebo,
                                    data = painel_federal_estadual_2011_2014_entropia_placebo)

summary(didreg_fe_entropia_placebo_2)

#### 9) Visualizando os resultados ####

stargazer(didreg_2, didreg_fe_2, didreg_fe_logit_2, didreg_fe_entropia_2,
          type = "text",
          title = "Efeito médio de tratamento sobre os tratados - Ciclo 2011-2014",
          decimal.mark = ",", digits = 3,
          column.labels = c("Diff-diff",
                            "Diff-diff + FE",
                            "Diff-diff + FE + PSM (logit)",
                            "Diff-diff + FE + PSM (entropia)"),
          model.numbers = FALSE,
          flip = TRUE, align = TRUE,
          float = TRUE, float.env = "sidewaystable",
          initial.zero = TRUE, intercept.bottom = TRUE,
          dep.var.labels = "ln(Nota média)",
          multicolumn = TRUE,model.names = FALSE)

stargazer(didreg_fe_entropia_placebo_2, 
          type = "text",
          title = "Placebo - Tendências paralelas - Ciclo 2011-2014",
          decimal.mark = ",", digits = 3,
          model.numbers = TRUE,
          align = TRUE,
          float = TRUE, float.env = "table",
          initial.zero = TRUE, intercept.bottom = TRUE,
          multicolumn = TRUE, model.names = FALSE,
          font.size = "small")


#### LISTA DE IES, NOME E PARTICIPAÇÃO NA GREVE ####

Aderiram <- read_excel("Dados/Federais_que_aderiram.xlsx")

Aderiram$Nome <- toupper(Aderiram$Nome)

lista_IES <- data.frame(CO_IES = unique(painel_federal_estadual$CO_IES),
                        Nome = unique(painel_federal_estadual$Nome))
lista_IES <- left_join(lista_IES, Aderiram, by = c("Nome" = "Nome"))
lista_IES$Aderiram <- ifelse(is.na(lista_IES$Aderiram), "Não", "Sim")

#stargazer(lista_IES[,c(2,3)], type = "latex", 
#          title = "Lista de instituições de ensino superior com dummy de aderência à greve",
#          decimal.mark = ",", digits = 3,
#          out = "LaTeX/Federal x Estadual/lista_ies.tex", out.header = TRUE, rownames = FALSE,
#          align = TRUE,
#          float = TRUE, float.env = "table",
#          initial.zero = TRUE, summary = FALSE)

#### GRÁFICOS AGREGADOS ####

## 1) SUPORTE COMUM ####

g_suporte_comum <- plot_grid(grafico_suporte_comum_2009_2012, grafico_suporte_comum_2010_2013, grafico_suporte_comum_2011_2014,
                             nrow = 3, labels = c("a) Ciclo 2009-2012",
                                                  "b) Ciclo 2010-2013",
                                                  "c) Ciclo 2011-2014"),
                             label_size = 10, label_fontface = "plain", scale = 1)

#ggsave(filename = "Figuras/Português/suporte_comum_agregado.png")


## 2) BALANCEAMENTO DAS COVARIADAS ####

g_balanceamento <- plot_grid(g2_2009_2012, g2_2010_2013, g2_2011_2014, nrow = 3,
                             labels = c("a) Ciclo 2009-2012",
                                        "b) Ciclo 2010-2013",
                                        "c) Ciclo 2011-2014"),
                             label_size = 10, label_fontface = "plain", scale = 1)

#ggsave(filename = "Figuras/Português/balanceamento_agregado.eps", height = 22, width = 15)


g_balanceamento_placebos <- plot_grid(g3_2009_2012, g3_2010_2013, g3_2011_2014, nrow = 3,
                                      labels = c("a) Ciclo 2009-2012",
                                                 "b) Ciclo 2010-2013",
                                                 "c) Ciclo 2011-2014"),
                                      label_size = 10, label_fontface = "plain", scale = 1)

#ggsave(filename = "Figuras/Português/balanceamento_placebo_agregado.eps")
