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
library(cowplot)

# Configurando o tema dos gráficos ----

theme_new <- function(base_size = 8) {
  theme_bw(base_size = base_size) %+replace%
    theme(
      plot.title = element_text(size = rel(1), face = "bold", margin = margin(5,0,5,0), hjust = 0),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.title = element_text(size = rel(1), face = "bold"),
      axis.text = element_text(size = rel(1), face = "bold"),
      axis.line = element_line(color = "black", arrow = arrow(length = unit(0.3, "lines"), type = "closed")),
      legend.title = element_text(size = rel(1), face = "bold"),
      legend.text = element_text(size = rel(1), face = "bold"),
      legend.key = element_rect(fill = "transparent", colour = NA),
      legend.key.size = unit(1.5, "lines"),
      legend.background = element_rect(fill = "transparent", colour = NA),
      strip.background = element_rect(fill = "#17252D", color = "#17252D"),
      strip.text = element_text(size = rel(1), face = "bold", color = "white", margin = margin(5,0,5,0))
    )
}
theme_set(theme_new())


#### LEITURA DOS DADOS ####

painel_direito <- readRDS("Dados trabalhados/painel_direito.rds")
painel_direito$Tratamento <- as.factor(painel_direito$Tratamento)

painel_ciencias_economicas <- readRDS("Dados trabalhados/painel_ciencias_economicas.rds")
painel_ciencias_economicas$Tratamento <- as.factor(painel_ciencias_economicas$Tratamento)

painel_medicina <- readRDS("Dados trabalhados/painel_medicina.rds")
painel_medicina$Tratamento <- as.factor(painel_medicina$Tratamento)

painel_agronomia <- readRDS("Dados trabalhados/painel_agronomia.rds")
painel_agronomia$Tratamento <- as.factor(painel_agronomia$Tratamento)

painel_historia <- readRDS("Dados trabalhados/painel_historia.rds")
painel_historia$Tratamento <- as.factor(painel_historia$Tratamento)

painel_engenharia_quimica <- readRDS("Dados trabalhados/painel_engenharia_quimica.rds")
painel_engenharia_quimica$Tratamento <- as.factor(painel_engenharia_quimica$Tratamento)

#### GRÁFICOS DE DENSIDADE DAS NOTAS MÉDIAS ####

## 1) Direito ####

grafico_densidade_notas_direito <- ggplot(painel_direito, aes(x = nota_media, fill = Tratamento))+
  geom_density(alpha = 0.25) +
  labs(x = "Mean score", y = "Densidade", fill = "Grupo")+
  scale_fill_manual(labels=c("Controle", "Tratamento"),
                    values = c("red", "navy")) +
  theme(panel.grid = element_blank())
#ggsave(filename = "Figuras/Português/Português/grafico_densidade_notas_direito.eps")

painel_direito$log_nota <- painel_direito$nota_media


grafico_densidade_log_notas_direito <- ggplot(painel_direito, aes(x = log_nota, fill = Tratamento))+
  geom_density(alpha = 0.25) +
  labs(x = "ln(Nota média)", y = "Densidade", fill = "Grupo")+
  scale_fill_manual(labels=c("Controle", "Tratamento"),                                              values = c("red", "navy")) +
  theme(panel.grid = element_blank())
#ggsave(filename = "Figuras/Português/Português/grafico_densidade_log_notas_direito.eps")

## 2) Ciências Econômicas ####

grafico_densidade_notas_economia <- ggplot(painel_ciencias_economicas, aes(x = nota_media, fill = Tratamento))+
  geom_density(alpha = 0.25) +
  labs(x = "Mean score", y = "Densidade", fill = "Grupo")+
  scale_fill_manual(labels=c("Controle", "Tratamento"),                                              values = c("red", "navy")) +
  theme(panel.grid = element_blank())
#ggsave(filename = "Figuras/Português/Português/grafico_densidade_notas_economia.eps")


painel_ciencias_economicas$log_nota <- painel_ciencias_economicas$nota_media

grafico_densidade_log_notas_ciencias_economicas <- ggplot(painel_ciencias_economicas, aes(x = log_nota, fill = Tratamento))+
  geom_density(alpha = 0.25) +
  labs(x = "ln(Nota média)", y = "Densidade", fill = "Grupo")+
  scale_fill_manual(labels=c("Controle", "Tratamento"),                                              values = c("red", "navy")) +
  theme(panel.grid = element_blank())
#ggsave(filename = "Figuras/Português/Português/grafico_densidade_log_notas_economia.eps")

## 3) Medicina #### 

grafico_densidade_notas_medicina <- ggplot(painel_medicina, aes(x = nota_media, fill = Tratamento))+
  geom_density(alpha = 0.25) +
  labs(x = "Mean score", y = "Densidade", fill = "Grupo")+
  scale_fill_manual(labels=c("Controle", "Tratamento"),                                              values = c("red", "navy")) +
  theme(panel.grid = element_blank())
#ggsave(filename = "Figuras/Português/grafico_densidade_notas_medicina.eps")

painel_medicina$log_nota <- painel_medicina$nota_media

grafico_densidade_log_notas_medicina <- ggplot(painel_medicina, aes(x = log_nota, fill = Tratamento))+
  geom_density(alpha = 0.25) +
  labs(x = "ln(Nota média)", y = "Densidade", fill = "Grupo")+
  scale_fill_manual(labels=c("Controle", "Tratamento"),                                              values = c("red", "navy")) +
  theme(panel.grid = element_blank())
#ggsave(filename = "Figuras/Português/grafico_densidade_log_notas_medicina.eps")

## 4) Agronomia ####

grafico_densidade_notas_agronomia <- ggplot(painel_agronomia, aes(x = nota_media, fill = Tratamento))+
  geom_density(alpha = 0.25) +
  labs(x = "Mean score", y = "Densidade", fill = "Grupo")+
  scale_fill_manual(labels=c("Controle", "Tratamento"),                                              values = c("red", "navy")) +
  theme(panel.grid = element_blank())
#ggsave(filename = "Figuras/Português/grafico_densidade_notas_agronomia.eps")

painel_agronomia$log_nota <- painel_agronomia$nota_media

grafico_densidade_log_notas_agronomia <- ggplot(painel_agronomia, aes(x = log_nota, fill = Tratamento))+
  geom_density(alpha = 0.25) +
  labs(x = "ln(Nota média)", y = "Densidade", fill = "Grupo")+
  scale_fill_manual(labels=c("Controle", "Tratamento"),                                              values = c("red", "navy")) +
  theme(panel.grid = element_blank())
#ggsave(filename = "Figuras/Português/grafico_densidade_log_notas_agronomia.eps")

## 5) História ####

grafico_densidade_notas_historia <- ggplot(painel_historia, aes(x = nota_media, fill = Tratamento))+
  geom_density(alpha = 0.25) +
  labs(x = "Mean score", y = "Densidade", fill = "Grupo")+
  scale_fill_manual(labels=c("Controle", "Tratamento"),                                              values = c("red", "navy")) +
  theme(panel.grid = element_blank())
#ggsave(filename = "Figuras/Português/grafico_densidade_notas_historia.eps")

painel_historia$log_nota <- painel_historia$nota_media

grafico_densidade_log_notas_historia <- ggplot(painel_historia, aes(x = log_nota, fill = Tratamento))+
  geom_density(alpha = 0.25) +
  labs(x = "ln(Nota média)", y = "Densidade", fill = "Grupo")+
  scale_fill_manual(labels=c("Controle", "Tratamento"),                                              values = c("red", "navy")) +
  theme(panel.grid = element_blank())
#ggsave(filename = "Figuras/Português/grafico_densidade_log_notas_historia.eps")

## 6) Engenharia Química ####

grafico_densidade_notas_engenharia_quimica <- ggplot(painel_engenharia_quimica, aes(x = nota_media, fill = Tratamento))+
  geom_density(alpha = 0.25) +
  labs(x = "Mean score", y = "Densidade", fill = "Grupo")+
  scale_fill_manual(labels=c("Controle", "Tratamento"),                                              values = c("red", "navy")) +
  theme(panel.grid = element_blank())
#ggsave(filename = "Figuras/Português/grafico_densidade_notas_engenharia_quimica.eps")

painel_engenharia_quimica$log_nota <- painel_engenharia_quimica$nota_media

grafico_densidade_log_notas_engenharia_quimica <- ggplot(painel_engenharia_quimica, aes(x = log_nota, fill = Tratamento))+
  geom_density(alpha = 0.25) +
  labs(x = "ln(Nota média)", y = "Densidade", fill = "Grupo")+
  scale_fill_manual(labels=c("Controle", "Tratamento"),                                              values = c("red", "navy")) +
  theme(panel.grid = element_blank())

#ggsave(filename = "Figuras/Português/grafico_densidade_log_notas_engenharia_quimica.eps")


#### NOMES DAS VARIÁVEIS ####

names <- data.frame(old = c("idade_media", "idade2_media", "prop_brancos",
                            "prop_casados", "prop_homens", "prop_medio_ou_mais",
                            "prop_renda_3SM"),
                    new = c("Idade média", "Idade média²",
                            "Prop. de brancos", "Prop. de casados",
                            "Prop. de homens", "Prop. de mães com \n ensino médio ou mais",
                            "Prop. de estudantes com renda \n familiar de 3S.M. ou menos"))

names_placebo <- data.frame(old = c("prop_casados", "prop_brancos",
                                    "ln_nota", "prop_homens", "prop_medio_ou_mais",
                                    "prop_renda_3SM"),
                            new = c("Prop. de casadps",
                                    "Prop. de brancos", "ln(Nota média)",
                                    "Prop. de homens", "Prop. de mães com \n ensino médio ou mais",
                                    "Prop. de estudantes com renda \n familiar de 3S.M. ou menos"))

#### HIPÓTESE DAS TENDÊNCIAS PARALELAS ####

## 1) Direito ####

grafico_tendencias_direito <- ggplot(painel_direito, aes(Ano, log_nota, color = Tratamento))+
                              stat_summary(geom = 'line') +
                              labs(x = "Ano", y = "ln(Nota média)", color = "Grupo")+
                              geom_vline(xintercept = 2012, linetype = "dashed", col = "black", size = 0.5) +
                              scale_colour_manual(labels=c("Controle", "Tratamento"),
                                                  values=c("red", "navy"))+
                              theme(panel.grid = element_blank())

#ggsave(file = "Figuras/Português/tendencias_direito.eps")

## 2) Ciências Econômicas ####

grafico_tendencias_economia <- ggplot(painel_ciencias_economicas, aes(Ano, log_nota, color = Tratamento))+
                               stat_summary(geom = 'line') +
                               labs(x = "Ano", y = "ln(Nota média)", color = "Grupo")+
                               geom_vline(xintercept = 2012, linetype = "dashed", col = "black", size = 0.5) +
                               scale_colour_manual(labels=c("Controle", "Tratamento"),
                                                   values = c("red", "navy")) +
                               theme(panel.grid = element_blank())

#ggsave(file = "Figuras/Português/tendencias_economia.eps")

## 3) Medicina ####

grafico_tendencias_medicina <- ggplot(painel_medicina, aes(Ano, log_nota, color = Tratamento))+
                               stat_summary(geom = 'line') +
                               labs(x = "Ano", y = "ln(Nota média)", color = "Grupo")+
                               geom_vline(xintercept = 2012, linetype = "dashed", col = "black", size = 0.5) +
                               scale_colour_manual(labels=c("Controle", "Tratamento"),
                                                   values = c("red", "navy")) +
                               theme(panel.grid = element_blank())

#ggsave(file = "Figuras/Português/tendencias_medicina.eps")

## 4) Agronomia ####

grafico_tendencias_agronomia <- ggplot(painel_agronomia, aes(Ano, log_nota, color = Tratamento))+
                                stat_summary(geom = 'line') +
                                labs(x = "Ano", y = "ln(Nota média)", color = "Grupo")+
                                geom_vline(xintercept = 2012, linetype = "dashed", col = "black", size = 0.5) +
                                scale_colour_manual(labels=c("Controle", "Tratamento"),
                                                    values = c("red", "navy")) +
                                theme(panel.grid = element_blank())

#ggsave(file = "Figuras/Português/tendencias_agronomia.eps")

## 5) História ####

grafico_tendencias_historia <- ggplot(painel_historia, aes(Ano, log_nota, color = Tratamento))+
                               stat_summary(geom = 'line') +
                               labs(x = "Ano", y = "ln(Nota média)", color = "Grupo")+
                               geom_vline(xintercept = 2012, linetype = "dashed", col = "black", size = 0.5) +
                               scale_colour_manual(labels=c("Controle", "Tratamento"),
                                                   values = c("red", "navy")) +
                               theme(panel.grid = element_blank())

#ggsave(file = "Figuras/Português/tendencias_historia.eps")


## 6) Engenharia Química ####

grafico_tendencias_engenharia_quimica <- ggplot(painel_engenharia_quimica, aes(Ano, log_nota, color = Tratamento))+
                                         stat_summary(geom = 'line') +
                                         labs(x = "Ano", y = "ln(Nota média)", color = "Grupo")+
                                         geom_vline(xintercept = 2012, linetype = "dashed", col = "black", size = 0.5) +
                                         scale_colour_manual(labels=c("Controle", "Tratamento"),
                                                             values = c("red", "navy")) +
                                         theme(panel.grid = element_blank())

#ggsave(file = "Figuras/Português/tendencias_engenharia_quimica.eps")

#### ESTIMAÇÕES - **CICLO 2009-2012** ####

## 1) Direito ####

direito_2009 <- subset(painel_direito, Ano == 2009)

#### 1.1) Hipótese do suporte comum ####

ps <- glm(data = painel_direito,
          Tratamento ~ idade_media + idade2_media + prop_brancos +
            prop_casados + prop_homens + prop_medio_ou_mais + 
            prop_renda_3SM,
          family = binomial(link = "logit"))

painel_direito$pscore <- predict(ps, type = "response")

grafico_suporte_comum_direito <- ggplot(painel_direito, aes(x = pscore, fill = Tratamento))+
                                 geom_density(alpha = 0.25) +
                                 labs(x = "Propensity score",
                                      y = "Densidade", 
                                      fill = "Grupo")+
                                 scale_fill_manual(labels=c("Controle", "Tratamento"),
                                                   values = c("red", "navy")) +
                                 theme(panel.grid = element_blank())

#ggsave(file = "Figuras/Português/suporte_comum_direito.eps")

#### 1.2) Suporte comum por quantil ####

painel_direito$quantil = with(painel_direito,
                              cut(pscore,
                                  quantile(pscore,
                                           prob = seq(0, 0.9470, length = 5),
                                           type = 5)))

media_por_quantil_direito <- subset(painel_direito, !is.na(quantil)) %>%
  group_by(quantil, Tratamento) %>%
  summarise_at(vars(pscore), funs(mean(., na.rm=TRUE)))

grafico_suporte_comum_direito_quantil <- ggplot(subset(painel_direito, !is.na(quantil)),
                                                aes(x = pscore, fill = Tratamento))+
                                         geom_density(alpha = 0.25) +
                                         labs(x = "Propensity score",
                                              y = "Densidade", 
                                              fill = "Grupo")+
                                         scale_fill_manual(labels=c("Controle", "Tratamento"),
                                                           values = c("red", "navy")) +
                                         facet_grid(quantil ~ .) +
                                         theme(panel.grid = element_blank())

#ggsave(file = "Figuras/Português/suporte_comum_direito_quantis.eps")

#### 1.3) Modelo diff-diff simples ####

didreg_direito = lm(log_nota ~ Tratamento + Tempo + Tempo*Tratamento + idade_media +
                      idade2_media + prop_brancos + prop_casados + prop_homens + 
                      prop_medio_ou_mais + prop_renda_3SM,
                    data = painel_direito)

summary(didreg_direito)

#### 1.4) Modelo diff-and-diff com efeitos fixos ####

didreg_fe_direito <- plm(log_nota ~ Tempo + Tempo*Tratamento + idade_media +
                           idade2_media + prop_brancos + prop_casados + prop_homens + 
                           prop_medio_ou_mais + prop_renda_3SM,
                         model = "within", index = c("CO_IES", "Ano"),
                         data = painel_direito)

summary(didreg_fe_direito)

#### 1.5) Modelo diff-and-diff com efeitos fixos e PSM ####
#### 1.5.1) Criação dos pesos utilizando o modelo logit multinomial #----

W1 = weightit(Tratamento ~ idade_media + idade2_media  + 
                prop_brancos + prop_casados + prop_homens + 
                prop_medio_ou_mais + prop_renda_3SM,
              data = direito_2009,
              method = "ps",
              estimand = "ATT")

#### 1.5.2) Checagem do balanceamento #====

b1 = bal.tab(W1, un = TRUE,
             m.threshold=0.1,
             v.threshold=3)
print(b1)

g1_direito <- love.plot(W1, stats = c("m"), drop.distance = TRUE,
                        thresholds = c(m=0.1),
                        wrap = 10,
                        var.order = "unadjusted",
                        sample.names = c("Desbalanceado", "Balanceado - PS"),
                        title = NULL,
                        line= T,
                        var.names = names,
                        colors = c("red", "navy"),
                        shapes = c("triangle filled", "circle filled"))

g1_direito <- g1_direito + 
              scale_color_manual(name = "Amostra", values = c("red", "navy")) + 
              scale_shape_manual(name = "Amostra", values = c("triangle filled", "circle filled")) + 
              guides(group = FALSE, size = FALSE, stroke = FALSE) + 
              xlab("Diferenças médias \n padronizadas")



#### 1.5.3) Criação dos pesos utilizando o modelo de entropia #----

W2 = weightit(Tratamento ~ idade_media + idade2_media + 
                prop_brancos + prop_casados + prop_homens + 
                prop_medio_ou_mais + prop_renda_3SM,
              data = direito_2009,
              method = "ebal",
              estimand = "ATT")

W2_placebo <- weightit(Tratamento ~ prop_casados + prop_brancos + log_nota + 
                         prop_homens + prop_medio_ou_mais + prop_renda_3SM,
                       data = direito_2009,
                       method = "ebal",
                       estimand = "ATT")

#### 1.5.4) Checagem do balanceamento #====

b2 = bal.tab(W2, un = TRUE,
             m.threshold=0.1,
             v.threshold=3)
print(b2)

g2_direito <- love.plot(W2, stats = c("m"), drop.distance = TRUE,
                        thresholds = c(m=0.1),
                        wrap = 10,
                        var.order = "unadjusted",
                        sample.names = c("Desbalanceado", "Balanceado - Entropia"),
                        title = NULL,
                        subtitle = "Amostra", line= T,
                        var.names = names,
                        colors = c("red", "navy"),
                        shapes = c("triangle filled", "circle filled"))

g2_direito <- g2_direito + 
              scale_color_manual(name = "Amostra", values = c("red", "navy")) + 
              scale_shape_manual(name = "Amostra", values = c("triangle filled", "circle filled")) + 
              guides(group = FALSE, size = FALSE, stroke = FALSE) + 
              xlab("Diferenças médias \n padronizadas")


b2_placebo = bal.tab(W2_placebo, un = TRUE,
                     m.threshold=0.1,
                     v.threshold=3)
print(b2_placebo)

g3_direito <- love.plot(W2_placebo, stats = c("m"), drop.distance = TRUE,
                        thresholds = c(m=0.1),
                        wrap = 10,
                        var.order = "unadjusted",
                        sample.names = c("Desbalanceado", "Balanceado - Entropia"),
                        title = NULL,
                        subtitle = "Amostra", line= T,
                        var.names = names_placebo,
                        colors = c("red", "navy"),
                        shapes = c("triangle filled", "circle filled"))

g3_direito <- g3_direito + 
              scale_color_manual(name = "Amostra", values = c("red", "navy")) + 
              scale_shape_manual(name = "Amostra", values = c("triangle filled", "circle filled")) + 
              guides(group = FALSE, size = FALSE, stroke = FALSE) + 
              xlab("Diferenças médias \n padronizadas")


#### 1.5.6) Unindo os pesos ao data frame ####

W1_matrix_direito <- data.frame(CO_IES = direito_2009$CO_IES,
                                W1 = W1$weights)

painel_direito_logit <- left_join(painel_direito,
                                  W1_matrix_direito, by = "CO_IES")

W2_matrix_direito <- data.frame(CO_IES = direito_2009$CO_IES,
                                W2 = W2$weights)

painel_direito_entropia <- left_join(painel_direito,
                                     W2_matrix_direito, by = "CO_IES")

W2_placebo_matrix_direito <- data.frame(CO_IES = direito_2009$CO_IES,
                                        W2_placebo = W2_placebo$weights)

painel_direito_entropia_placebo <- left_join(painel_direito,
                                             W2_placebo_matrix_direito, by = "CO_IES")

#### 1.5.7) Regressão - Pesos calculados utilizando logit multinomial ####

didreg_fe_logit_direito <- plm(log_nota ~ Tratamento + Tempo + Tempo*Tratamento + idade_media +
                                 idade2_media + prop_brancos + prop_casados + prop_homens + 
                                 prop_medio_ou_mais + prop_renda_3SM,
                               model = "within", index = c("CO_IES", "Ano"), weights = W1,
                               data = painel_direito_logit)

summary(didreg_fe_logit_direito)

#### 1.5.8) Regressão - Pesos calculados utilizando entropia ####

didreg_fe_entropia_direito <- plm(log_nota ~ Tratamento + Tempo + Tempo*Tratamento + idade_media +
                                    idade2_media + prop_brancos + prop_casados + prop_homens + 
                                    prop_medio_ou_mais + prop_renda_3SM,
                                  model = "within", index = c("CO_IES", "Ano"), weights = W2,
                                  data = painel_direito_entropia)

summary(didreg_fe_entropia_direito)

#### 1.6) Placebo - Tratamento sobre idade média -  Pesos calculados utilizando entropia ####

didreg_fe_entropia_placebo_direito <- plm(idade_media ~ Tratamento + Tempo + Tempo*Tratamento + prop_casados +
                                            prop_brancos + nota_media + prop_homens + 
                                            prop_medio_ou_mais + prop_renda_3SM,
                                          model = "within", index = c("CO_IES", "Ano"), weights = W2_placebo,
                                          data = painel_direito_entropia_placebo)

summary(didreg_fe_entropia_placebo_direito)

#### 1.7) Visualizando os resultados ####

stargazer(didreg_direito, didreg_fe_direito, didreg_fe_logit_direito, didreg_fe_entropia_direito,
          type = "text",
          title = "Efeito médio de Tratamento sobre os tratados - Direito - Ciclo 2009-2012",
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

stargazer(didreg_fe_entropia_placebo_direito,
          type = "text", out = "Resultados/direito_placebo.txt",
          title = "Placebo - Tendências paralelas - Direito - Ciclo 2009-2012",
          decimal.mark = ",", digits = 3,
          model.numbers = TRUE,
          align = TRUE,
          float = TRUE, float.env = "table",
          initial.zero = TRUE, intercept.bottom = TRUE,
          multicolumn = TRUE, model.names = FALSE,
          font.size = "small")

## 2) Ciências Econômicas ####

ciencias_economicas_2009 <- subset(painel_ciencias_economicas, Ano == 2009)

#### 2.1) Hipótese do suporte comum ####

ps <- glm(data = painel_ciencias_economicas,
          Tratamento ~ idade_media + idade2_media + prop_brancos +
            prop_casados + prop_homens + prop_medio_ou_mais + 
            prop_renda_3SM,
          family = binomial(link = "logit"))

painel_ciencias_economicas$pscore <- predict(ps, type = "response")

grafico_suporte_comum_ciencias_economicas <- ggplot(painel_ciencias_economicas, aes(x = pscore, fill = Tratamento)) +
                                             geom_density(alpha = 0.25) +
                                             labs(x = "Propensity score",
                                                  y = "Densidade", 
                                                  fill = "Grupo")+
                                             scale_fill_manual(labels=c("Controle", "Tratamento"),
                                                               values = c("red", "navy")) +
                                             theme(panel.grid = element_blank())

#ggsave(file = "Figuras/Português/suporte_comum_ciencias_economicas.eps")

#### 2.2) Suporte comum por quantil ####

painel_ciencias_economicas$quantil = with(painel_ciencias_economicas,
                                          cut(pscore,
                                              quantile(pscore,
                                                       prob = seq(0, 0.91227, length = 5),
                                                       type = 5)))

media_por_quantil_ciencias_economicas <- subset(painel_ciencias_economicas, !is.na(quantil)) %>%
  group_by(quantil, Tratamento) %>%
  summarise_at(vars(pscore), funs(mean(., na.rm=TRUE)))

grafico_suporte_comum_ciencias_economicas_quantil <- ggplot(subset(painel_ciencias_economicas, !is.na(quantil)),
                                                            aes(x = pscore, fill = Tratamento)) +
                                                     geom_density(alpha = 0.25) +
                                                     labs(x = "Propensity score",
                                                          y = "Densidade", 
                                                          fill = "Grupo")+
                                                     scale_fill_manual(labels=c("Controle", "Tratamento"),
                                                                       values = c("red", "navy")) +                                                          
                                                     facet_grid(quantil ~ .) +
                                                     theme(panel.grid = element_blank())

#ggsave(file = "Figuras/Português/suporte_comum_ciencias_economicas_quantis.eps")

#### 2.3) Modelo diff-diff simples ####

didreg_ciencias_economicas = lm(log_nota ~ Tratamento + Tempo + Tempo*Tratamento + idade_media +
                                  idade2_media + prop_brancos + prop_casados + prop_homens + 
                                  prop_medio_ou_mais + prop_renda_3SM,
                                data = painel_ciencias_economicas)

summary(didreg_ciencias_economicas)

#### 2.4) Modelo diff-and-diff com efeitos fixos ####

didreg_fe_ciencias_economicas <- plm(log_nota ~ Tempo + Tempo*Tratamento + idade_media +
                                       idade2_media + prop_brancos + prop_casados + prop_homens + 
                                       prop_medio_ou_mais + prop_renda_3SM,
                                     model = "within", index = c("CO_IES", "Ano"),
                                     data = painel_ciencias_economicas)

summary(didreg_fe_ciencias_economicas)

#### 2.5) Modelo diff-and-diff com efeitos fixos e PSM ####

#### 2.5.1) Criação dos pesos utilizando o modelo logit multinomial #----

W1 = weightit(Tratamento ~ idade_media + idade2_media  + 
                prop_brancos + prop_casados + prop_homens + 
                prop_medio_ou_mais + prop_renda_3SM,
              data = ciencias_economicas_2009,
              method = "ps",
              estimand = "ATT")

#### 2.5.2) Checagem do balanceamento #====

b1 = bal.tab(W1, un = TRUE,
             m.threshold=0.1,
             v.threshold=3)
print(b1)

g1_economia <- love.plot(W1, stats = c("m"), drop.distance = TRUE,
                         thresholds = c(m=0.1),
                         wrap = 10,
                         var.order = "unadjusted",
                         sample.names = c("Desbalanceado", "Balanceado - PS"),
                         title = NULL,
                         subtitle = "Amostra", line= T,
                         var.names = names,
                         colors = c("red", "navy"),
                         shapes = c("triangle filled", "circle filled"))

g1_economia <- g1_economia + 
               scale_color_manual(name = "Amostra", values = c("red", "navy")) + 
               scale_shape_manual(name = "Amostra", values = c("triangle filled", "circle filled")) + 
               guides(group = FALSE, size = FALSE, stroke = FALSE) + 
               xlab("Diferenças médias \n padronizadas")


#### 2.5.3) Criação dos pesos utilizando o modelo de entropia #----

W2 = weightit(Tratamento ~ idade_media + idade2_media + 
                prop_brancos + prop_casados + prop_homens + 
                prop_medio_ou_mais + prop_renda_3SM,
              data = ciencias_economicas_2009,
              method = "ebal",
              estimand = "ATT")

W2_placebo <- weightit(Tratamento ~ prop_casados + prop_brancos + log_nota + 
                         prop_homens + prop_medio_ou_mais + prop_renda_3SM,
                       data = ciencias_economicas_2009,
                       method = "ebal",
                       estimand = "ATT")

#### 2.5.4) Checagem do balanceamento #====

b2 = bal.tab(W2, un = TRUE,
             m.threshold=0.1,
             v.threshold=3)
print(b2)

g2_economia <- love.plot(W2, stats = c("m"), drop.distance = TRUE,
                         thresholds = c(m=0.1),
                         wrap = 10,
                         var.order = "unadjusted",
                         sample.names = c("Desbalanceado", "Balanceado - Entropia"),
                         title = NULL,
                         subtitle = "Amostra", line= T,
                         var.names = names,
                         colors = c("red", "navy"),
                         shapes = c("triangle filled", "circle filled"))

g2_economia <- g2_economia + 
               scale_color_manual(name = "Amostra", values = c("red", "navy")) + 
               scale_shape_manual(name = "Amostra", values = c("triangle filled", "circle filled")) + 
               guides(group = FALSE, size = FALSE, stroke = FALSE) + 
               xlab("Diferenças médias \n padronizadas")

b2_placebo = bal.tab(W2_placebo, un = TRUE,
                     m.threshold=0.1,
                     v.threshold=3)
print(b2_placebo)

g3_economia <- love.plot(W2_placebo, stats = c("m"), drop.distance = TRUE,
                         thresholds = c(m=0.1),
                         wrap = 10,
                         var.order = "unadjusted",
                         sample.names = c("Desbalanceado", "Balanceado - Entropia"),
                         title = NULL,
                         subtitle = "Amostra", line= T,
                         var.names = names_placebo,
                         colors = c("red", "navy"),
                         shapes = c("triangle filled", "circle filled"))

g3_economia <- g3_economia + 
               scale_color_manual(name = "Amostra", values = c("red", "navy")) + 
               scale_shape_manual(name = "Amostra", values = c("triangle filled", "circle filled")) + 
               guides(group = FALSE, size = FALSE, stroke = FALSE) + 
               xlab("Diferenças médias \n padronizadas")

#### 2.5.6) Unindo os pesos ao data frame ####

W1_matrix_ciencias_economicas <- data.frame(CO_IES = ciencias_economicas_2009$CO_IES,
                                            W1 = W1$weights)

painel_ciencias_economicas_logit <- left_join(painel_ciencias_economicas,
                                              W1_matrix_ciencias_economicas, by = "CO_IES")

W2_matrix_ciencias_economicas <- data.frame(CO_IES = ciencias_economicas_2009$CO_IES,
                                            W2 = W2$weights)

painel_ciencias_economicas_entropia <- left_join(painel_ciencias_economicas,
                                                 W2_matrix_ciencias_economicas, by = "CO_IES")

W2_placebo_matrix_ciencias_economicas <- data.frame(CO_IES = ciencias_economicas_2009$CO_IES,
                                                    W2_placebo = W2_placebo$weights)

painel_ciencias_economicas_entropia_placebo <- left_join(painel_ciencias_economicas,
                                                         W2_placebo_matrix_ciencias_economicas, by = "CO_IES")

#### 2.5.7) Regressão - Pesos calculados utilizando logit multinomial ####

didreg_fe_logit_ciencias_economicas <- plm(log_nota ~ Tratamento + Tempo + Tempo*Tratamento + idade_media +
                                             idade2_media + prop_brancos + prop_casados + prop_homens + 
                                             prop_medio_ou_mais + prop_renda_3SM,
                                           model = "within", index = c("CO_IES", "Ano"), weights = W1,
                                           data = painel_ciencias_economicas_logit)

summary(didreg_fe_logit_ciencias_economicas)

#### 2.5.8) Regressão - Pesos calculados utilizando entropia ####

didreg_fe_entropia_ciencias_economicas <- plm(log_nota ~ Tratamento + Tempo + Tempo*Tratamento + idade_media +
                                                idade2_media + prop_brancos + prop_casados + prop_homens + 
                                                prop_medio_ou_mais + prop_renda_3SM,
                                              model = "within", index = c("CO_IES", "Ano"), weights = W2,
                                              data = painel_ciencias_economicas_entropia)


summary(didreg_fe_entropia_ciencias_economicas)

#### 2.6) Placebo - Tratamento sobre idade média -  Pesos calculados utilizando entropia ####

didreg_fe_entropia_placebo_ciencias_economicas <- plm(idade_media ~ Tratamento + Tempo + Tempo*Tratamento + prop_casados +
                                                        prop_brancos + log_nota + prop_homens + 
                                                        prop_medio_ou_mais + prop_renda_3SM,
                                                      model = "within", index = c("CO_IES", "Ano"), weights = W2_placebo,
                                                      data = painel_ciencias_economicas_entropia_placebo)

summary(didreg_fe_entropia_placebo_ciencias_economicas)

#### 2.7) Visualizando os resultados ####

stargazer(didreg_ciencias_economicas, didreg_fe_ciencias_economicas, didreg_fe_logit_ciencias_economicas, didreg_fe_entropia_ciencias_economicas,
          type = "text", out = "Resultados/economia.txt",
          title = "Efeito médio de Tratamento sobre os tratados - Ciências Econômicas - Ciclo 2009-2012",
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

stargazer(didreg_fe_entropia_placebo_ciencias_economicas,
          type = "text", out = "Resultados/economia_placebo.txt",
          title = "Placebo - Tendências paralelas - Ciências Econômicas - Ciclo 2009-2012",
          decimal.mark = ",", digits = 3,
          model.numbers = TRUE,
          align = TRUE,
          float = TRUE, float.env = "table",
          initial.zero = TRUE, intercept.bottom = TRUE,
          multicolumn = TRUE, model.names = FALSE,
          font.size = "small")

#### ESTIMAÇÕES - **CICLO 2010-2013** ####

## 1) Medicina ####

medicina_2010 <- subset(painel_medicina, Ano == 2010)

#### 1.1) Hipótese do suporte comum ####

ps <- glm(data = painel_medicina,
          Tratamento ~ idade_media + idade2_media + prop_brancos +
            prop_casados + prop_homens + prop_medio_ou_mais + 
            prop_renda_3SM,
          family = binomial(link = "logit"))

painel_medicina$pscore <- predict(ps, type = "response")

grafico_suporte_comum_medicina <- ggplot(painel_medicina, aes(x = pscore, fill = Tratamento))+
                                  geom_density(alpha = 0.25) +
                                  labs(x = "Propensity score",
                                       y = "Densidade", 
                                       fill = "Grupo")+
                                  scale_fill_manual(labels=c("Controle", "Tratamento"),
                                                    values = c("red", "navy")) +
                                  theme(panel.grid = element_blank())

#ggsave(file = "Figuras/Português/suporte_comum_medicina.eps")

#### 1.2) Suporte comum por quantil ####

painel_medicina$quantil = with(painel_medicina,
                               cut(pscore,
                                   quantile(pscore,
                                            prob = seq(0, 0.86307, length = 5),
                                            type = 5)))

media_por_quantil_medicina <- subset(painel_medicina, !is.na(quantil)) %>%
  group_by(quantil, Tratamento) %>%
  summarise_at(vars(pscore), funs(mean(., na.rm=TRUE)))

grafico_suporte_comum_medicina_quantil <- ggplot(subset(painel_medicina, !is.na(quantil)),
                                                 aes(x = pscore, fill = Tratamento))+
                                          geom_density(alpha = 0.25) +
                                          labs(x = "Propensity score",
                                               y = "Densidade", 
                                               fill = "Grupo")+
                                          scale_fill_manual(labels=c("Controle", "Tratamento"),
                                                            values = c("red", "navy")) +
                                          facet_grid(quantil ~ .) +
                                          theme(panel.grid = element_blank())

#ggsave(file = "Figuras/Português/suporte_comum_medicina_quantis.eps")

#### 1.3) Modelo diff-diff simples ####

didreg_medicina = lm(log_nota ~ Tratamento + Tempo + Tempo*Tratamento + idade_media +
                       idade2_media + prop_brancos + prop_casados + prop_homens + 
                       prop_medio_ou_mais + prop_renda_3SM,
                     data = painel_medicina)

summary(didreg_medicina)

#### 1.4) Modelo diff-and-diff com efeitos fixos ####

didreg_fe_medicina <- plm(log_nota ~ Tempo + Tempo*Tratamento + idade_media +
                            idade2_media + prop_brancos + prop_casados + prop_homens + 
                            prop_medio_ou_mais + prop_renda_3SM,
                          model = "within", index = c("CO_IES", "Ano"),
                          data = painel_medicina)

summary(didreg_fe_medicina)

#### 1.5) Modelo diff-and-diff com efeitos fixos e PSM ####
#### 1.5.1) Criação dos pesos utilizando o modelo logit multinomial #----

W1 = weightit(Tratamento ~ idade_media + idade2_media  + 
                prop_brancos + prop_casados + prop_homens + 
                prop_medio_ou_mais + prop_renda_3SM,
              data = medicina_2010,
              method = "ps",
              estimand = "ATT")

#### 1.5.2) Checagem do balanceamento #====

b1 = bal.tab(W1, un = TRUE,
             m.threshold=0.1,
             v.threshold=3)
print(b1)

g1_medicina <- love.plot(W1, stats = c("m"), drop.distance = TRUE,
                         thresholds = c(m=0.1),
                         wrap = 10,
                         var.order = "unadjusted",
                         sample.names = c("Desbalanceado", "Balanceado - PS"),
                         title = NULL,
                         subtitle = "Amostra", line= T,
                         var.names = names,
                         colors = c("red", "navy"),
                         shapes = c("triangle filled", "circle filled"))

g1_medicina <- g1_medicina + 
               scale_color_manual(name = "Amostra", values = c("red", "navy")) + 
               scale_shape_manual(name = "Amostra", values = c("triangle filled", "circle filled")) + 
               guides(group = FALSE, size = FALSE, stroke = FALSE) + 
               xlab("Diferenças médias \n padronizadas")

#### 1.5.3) Criação dos pesos utilizando o modelo de entropia #----

W2 = weightit(Tratamento ~ idade_media + idade2_media + 
                prop_brancos + prop_casados + prop_homens + 
                prop_medio_ou_mais + prop_renda_3SM,
              data = medicina_2010,
              method = "ebal",
              estimand = "ATT")

W2_placebo <- weightit(Tratamento ~ prop_casados + prop_brancos + log_nota + 
                         prop_homens + prop_medio_ou_mais + prop_renda_3SM,
                       data = medicina_2010,
                       method = "ebal",
                       estimand = "ATT")

#### 1.5.4) Checagem do balanceamento #====

b2 = bal.tab(W2, un = TRUE,
             m.threshold=0.1,
             v.threshold=3)
print(b2)

g2_medicina <- love.plot(W2, stats = c("m"), drop.distance = TRUE,
                         thresholds = c(m=0.1),
                         wrap = 10,
                         var.order = "unadjusted",
                         sample.names = c("Desbalanceado", "Balanceado - Entropia"),
                         title = NULL,
                         subtitle = "Amostra", line= T,
                         var.names = names,
                         colors = c("red", "navy"),
                         shapes = c("triangle filled", "circle filled"))

g2_medicina <- g2_medicina + 
               scale_color_manual(name = "Amostra", values = c("red", "navy")) + 
               scale_shape_manual(name = "Amostra", values = c("triangle filled", "circle filled")) + 
               guides(group = FALSE, size = FALSE, stroke = FALSE) + 
               xlab("Diferenças médias \n padronizadas")


b2_placebo = bal.tab(W2_placebo, un = TRUE,
                     m.threshold=0.1,
                     v.threshold=3)
print(b2_placebo)

g3_medicina <- love.plot(W2_placebo, stats = c("m"), drop.distance = TRUE,
                         thresholds = c(m=0.1),
                         wrap = 10,
                         var.order = "unadjusted",
                         sample.names = c("Desbalanceado", "Balanceado - Entropia"),
                         title = NULL,
                         subtitle = "Amostra", line= T,
                         var.names = names_placebo,
                         colors = c("red", "navy"),
                         shapes = c("triangle filled", "circle filled"))

g3_medicina <- g3_medicina + 
               scale_color_manual(name = "Amostra", values = c("red", "navy")) + 
               scale_shape_manual(name = "Amostra", values = c("triangle filled", "circle filled")) + 
               guides(group = FALSE, size = FALSE, stroke = FALSE) + 
               xlab("Diferenças médias \n padronizadas")


#### 1.5.6) Unindo os pesos ao data frame ####

W1_matrix_medicina <- data.frame(CO_IES = medicina_2010$CO_IES,
                                 W1 = W1$weights)

painel_medicina_logit <- left_join(painel_medicina,
                                   W1_matrix_medicina, by = "CO_IES")

W2_matrix_medicina <- data.frame(CO_IES = medicina_2010$CO_IES,
                                 W2 = W2$weights)

painel_medicina_entropia <- left_join(painel_medicina,
                                      W2_matrix_medicina, by = "CO_IES")

W2_placebo_matrix_medicina <- data.frame(CO_IES = medicina_2010$CO_IES,
                                         W2_placebo = W2_placebo$weights)

painel_medicina_entropia_placebo <- left_join(painel_medicina,
                                              W2_placebo_matrix_medicina, by = "CO_IES")

#### 1.5.7) Regressão - Pesos calculados utilizando logit multinomial ####

didreg_fe_logit_medicina <- plm(log_nota ~ Tratamento + Tempo + Tempo*Tratamento + idade_media +
                                  idade2_media + prop_brancos + prop_casados + prop_homens + 
                                  prop_medio_ou_mais + prop_renda_3SM,
                                model = "within", index = c("CO_IES", "Ano"), weights = W1,
                                data = painel_medicina_logit)

summary(didreg_fe_logit_medicina)

#### 1.5.8) Regressão - Pesos calculados utilizando entropia ####

didreg_fe_entropia_medicina <- plm(log_nota ~ Tratamento + Tempo + Tempo*Tratamento + idade_media +
                                     idade2_media + prop_brancos + prop_casados + prop_homens + 
                                     prop_medio_ou_mais + prop_renda_3SM,
                                   model = "within", index = c("CO_IES", "Ano"), weights = W2,
                                   data = painel_medicina_entropia)

summary(didreg_fe_entropia_medicina)

#### 1.6) Placebo - Tratamento sobre idade média -  Pesos calculados utilizando entropia ####

didreg_fe_entropia_placebo_medicina <- plm(idade_media ~ Tratamento + Tempo + Tempo*Tratamento + prop_casados +
                                             prop_brancos + nota_media + prop_homens + 
                                             prop_medio_ou_mais + prop_renda_3SM,
                                           model = "within", index = c("CO_IES", "Ano"), weights = W2_placebo,
                                           data = painel_medicina_entropia_placebo)

summary(didreg_fe_entropia_placebo_medicina)

#### 1.7) Visualizando os resultados ####

stargazer(didreg_medicina, didreg_fe_medicina, didreg_fe_logit_medicina, didreg_fe_entropia_medicina,
          type = "text", out = "Resultados/medicina.txt",
          title = "Efeito médio de Tratamento sobre os tratados - Medicina - Ciclo 2010-2013",
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

stargazer(didreg_fe_entropia_placebo_medicina,
          type = "text", out = "Resultados/medicina_placebo.txt",
          title = "Placebo - Tendências paralelas - Medicina - Ciclo 2010-2013",
          decimal.mark = ",", digits = 3,
          model.numbers = TRUE,
          align = TRUE,
          float = TRUE, float.env = "table",
          initial.zero = TRUE, intercept.bottom = TRUE,
          multicolumn = TRUE, model.names = FALSE,
          font.size = "small")

## 2) Agronomia ####

agronomia_2010 <- subset(painel_agronomia, Ano == 2010)

#### 2.1) Hipótese do suporte comum ####

ps <- glm(data = painel_agronomia,
          Tratamento ~ idade_media + idade2_media + prop_brancos +
            prop_casados + prop_homens + prop_medio_ou_mais + 
            prop_renda_3SM,
          family = binomial(link = "logit"))

painel_agronomia$pscore <- predict(ps, type = "response")

grafico_suporte_comum_agronomia <- ggplot(painel_agronomia, aes(x = pscore, fill = Tratamento))+
                                   geom_density(alpha = 0.25) +
                                   labs(x = "Propensity score",
                                        y = "Densidade", 
                                        fill = "Grupo")+
                                   scale_fill_manual(labels=c("Controle","Tratamento"),
                                                     values=c("red", "navy")) +
                                   theme(panel.grid = element_blank())

#ggsave(file = "Figuras/Português/suporte_comum_agronomia.eps")

#### 2.2) Suporte comum por quantil ####

painel_agronomia$quantil = with(painel_agronomia,
                                cut(pscore,
                                    quantile(pscore,
                                             prob = seq(0, 0.9203, length = 5),
                                             type = 5)))

media_por_quantil_agronomia <- subset(painel_agronomia, !is.na(quantil)) %>%
  group_by(quantil, Tratamento) %>%
  summarise_at(vars(pscore), funs(mean(., na.rm=TRUE)))

grafico_suporte_comum_agronomia_quantil <- ggplot(subset(painel_agronomia, !is.na(quantil)),
                                                  aes(x = pscore, fill = Tratamento))+
                                           geom_density(alpha = 0.25) +
                                           labs(x = "Propensity score",
                                                y = "Densidade", 
                                                fill = "Grupo")+
                                           scale_fill_manual(labels=c("Controle","Tratamento"),
                                                             values=c("red", "navy")) +
                                           facet_grid(quantil ~ .) +
                                           theme(panel.grid = element_blank())

#ggsave(file = "Figuras/Português/suporte_comum_agronomia_quantis.eps")

#### 2.3) Modelo diff-diff simples ####

didreg_agronomia = lm(log_nota ~ Tratamento + Tempo + Tempo*Tratamento + idade_media +
                        idade2_media + prop_brancos + prop_casados + prop_homens + 
                        prop_medio_ou_mais + prop_renda_3SM,
                      data = painel_agronomia)

summary(didreg_agronomia)

#### 2.4) Modelo diff-and-diff com efeitos fixos ####

didreg_fe_agronomia <- plm(log_nota ~ Tempo + Tempo*Tratamento + idade_media +
                             idade2_media + prop_brancos + prop_casados + prop_homens + 
                             prop_medio_ou_mais + prop_renda_3SM,
                           model = "within", index = c("CO_IES", "Ano"),
                           data = painel_agronomia)

summary(didreg_fe_agronomia)

#### 2.5) Modelo diff-and-diff com efeitos fixos e PSM ####
#### 2.5.1) Criação dos pesos utilizando o modelo logit multinomial #----

W1 = weightit(Tratamento ~ idade_media + idade2_media  + 
                prop_brancos + prop_casados + prop_homens + 
                prop_medio_ou_mais + prop_renda_3SM,
              data = agronomia_2010,
              method = "ps",
              estimand = "ATT")

#### 2.5.2) Checagem do balanceamento #====

b1 = bal.tab(W1, un = TRUE,
             m.threshold=0.1,
             v.threshold=3)
print(b1)

g1_agronomia <- love.plot(W1, stats = c("m"), drop.distance = TRUE,
                          thresholds = c(m=0.1),
                          wrap = 10,
                          var.order = "unadjusted",
                          sample.names = c("Desbalanceado", "Balanceado - PS"),
                          title = NULL,
                          subtitle = "Amostra", line= T,
                          var.names = names,
                          colors = c("red", "navy"),
                          shapes = c("triangle filled", "circle filled"))

g1_agronomia <- g1_agronomia + 
                scale_color_manual(name = "Amostra", values = c("red", "navy")) + 
                scale_shape_manual(name = "Amostra", values = c("triangle filled", "circle filled")) + 
                guides(group = FALSE, size = FALSE, stroke = FALSE) + 
                xlab("Diferenças médias \n padronizadas")


#### 2.5.3) Criação dos pesos utilizando o modelo de entropia #----

W2 = weightit(Tratamento ~ idade_media + idade2_media + 
                prop_brancos + prop_casados + prop_homens + 
                prop_medio_ou_mais + prop_renda_3SM,
              data = agronomia_2010,
              method = "ebal",
              estimand = "ATT")

W2_placebo <- weightit(Tratamento ~ prop_casados + prop_brancos + log_nota + 
                         prop_homens + prop_medio_ou_mais + prop_renda_3SM,
                       data = agronomia_2010,
                       method = "ebal",
                       estimand = "ATT")

#### 2.5.4) Checagem do balanceamento #====

b2 = bal.tab(W2, un = TRUE,
             m.threshold=0.1,
             v.threshold=3)
print(b2)

g2_agronomia <- love.plot(W2, stats = c("m"), drop.distance = TRUE,
                          thresholds = c(m=0.1),
                          wrap = 10,
                          var.order = "unadjusted",
                          sample.names = c("Desbalanceado", "Balanceado - Entropia"),
                          title = NULL,
                          subtitle = "Amostra", line= T,
                          var.names = names,
                          colors = c("red", "navy"),
                          shapes = c("triangle filled", "circle filled"))

g2_agronomia <- g2_agronomia + 
                scale_color_manual(name = "Amostra", values = c("red", "navy")) + 
                scale_shape_manual(name = "Amostra", values = c("triangle filled", "circle filled")) + 
                guides(group = FALSE, size = FALSE, stroke = FALSE) + 
                xlab("Diferenças médias \n padronizadas")


b2_placebo = bal.tab(W2_placebo, un = TRUE,
                     m.threshold=0.1,
                     v.threshold=3)
print(b2_placebo)

g3_agronomia <- love.plot(W2_placebo, stats = c("m"), drop.distance = TRUE,
                          thresholds = c(m=0.1),
                          wrap = 10,
                          var.order = "unadjusted",
                          sample.names = c("Desbalanceado", "Balanceado - Entropia"),
                          title = NULL,
                          subtitle = "Amostra", line= T,
                          var.names = names_placebo,
                          colors = c("red", "navy"),
                          shapes = c("triangle filled", "circle filled"))

g3_agronomia <- g3_agronomia + 
                scale_color_manual(name = "Amostra", values = c("red", "navy")) + 
                scale_shape_manual(name = "Amostra", values = c("triangle filled", "circle filled")) + 
                guides(group = FALSE, size = FALSE, stroke = FALSE) + 
                xlab("Diferenças médias \n padronizadas")


#### 2.5.6) Unindo os pesos ao data frame ####

W1_matrix_agronomia <- data.frame(CO_IES = agronomia_2010$CO_IES,
                                  W1 = W1$weights)

painel_agronomia_logit <- left_join(painel_agronomia,
                                    W1_matrix_agronomia, by = "CO_IES")

W2_matrix_agronomia <- data.frame(CO_IES = agronomia_2010$CO_IES,
                                  W2 = W2$weights)

painel_agronomia_entropia <- left_join(painel_agronomia,
                                       W2_matrix_agronomia, by = "CO_IES")

W2_placebo_matrix_agronomia <- data.frame(CO_IES = agronomia_2010$CO_IES,
                                          W2_placebo = W2_placebo$weights)

painel_agronomia_entropia_placebo <- left_join(painel_agronomia,
                                               W2_placebo_matrix_agronomia, by = "CO_IES")

#### 2.5.7) Regressão - Pesos calculados utilizando logit multinomial ####

didreg_fe_logit_agronomia <- plm(log_nota ~ Tratamento + Tempo + Tempo*Tratamento + idade_media +
                                   idade2_media + prop_brancos + prop_casados + prop_homens + 
                                   prop_medio_ou_mais + prop_renda_3SM,
                                 model = "within", index = c("CO_IES", "Ano"), weights = W1,
                                 data = painel_agronomia_logit)

summary(didreg_fe_logit_agronomia)

#### 2.5.8) Regressão - Pesos calculados utilizando entropia ####

didreg_fe_entropia_agronomia <- plm(log_nota ~ Tratamento + Tempo + Tempo*Tratamento + idade_media +
                                      idade2_media + prop_brancos + prop_casados + prop_homens + 
                                      prop_medio_ou_mais + prop_renda_3SM,
                                    model = "within", index = c("CO_IES", "Ano"), weights = W2,
                                    data = painel_agronomia_entropia)

summary(didreg_fe_entropia_agronomia)

#### 2.6) Placebo - Tratamento sobre idade média -  Pesos calculados utilizando entropia ####

didreg_fe_entropia_placebo_agronomia <- plm(idade_media ~ Tratamento + Tempo + Tempo*Tratamento + prop_casados +
                                              prop_brancos + nota_media + prop_homens + 
                                              prop_medio_ou_mais + prop_renda_3SM,
                                            model = "within", index = c("CO_IES", "Ano"), weights = W2_placebo,
                                            data = painel_agronomia_entropia_placebo)

summary(didreg_fe_entropia_placebo_agronomia)

#### 2.7) Visualizando os resultados ####

stargazer(didreg_agronomia, didreg_fe_agronomia, didreg_fe_logit_agronomia, didreg_fe_entropia_agronomia,
          type = "text", out = "Resultados/agronomia.txt",
          title = "Efeito médio de Tratamento sobre os tratados - Agronomia - Ciclo 2010-2013",
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

stargazer(didreg_fe_entropia_placebo_agronomia,
          type = "text", out = "Resultados/agronomia_placebo.txt",
          title = "Placebo - Tendências paralelas - Agronomia - Ciclo 2010-2013",
          decimal.mark = ",", digits = 3,
          model.numbers = TRUE,
          align = TRUE,
          float = TRUE, float.env = "table",
          initial.zero = TRUE, intercept.bottom = TRUE,
          multicolumn = TRUE, model.names = FALSE,
          font.size = "small")

#### ESTIMAÇÕES - **CICLO 2011-2014** ####
## 1) Engenharia Química ####

engenharia_quimica_2011 <- subset(painel_engenharia_quimica, Ano == 2011)

#### 1.1) Hipótese do suporte comum ####

ps <- glm(data = painel_engenharia_quimica,
          Tratamento ~ idade_media + idade2_media + prop_brancos +
            prop_casados + prop_homens + prop_medio_ou_mais + 
            prop_renda_3SM,
          family = binomial(link = "logit"))

painel_engenharia_quimica$pscore <- predict(ps, type = "response")

grafico_suporte_comum_engenharia_quimica <- ggplot(painel_engenharia_quimica, aes(x = pscore, fill = Tratamento))+
                                            geom_density(alpha = 0.25) +
                                            labs(x = "Propensity score",
                                                 y = "Densidade", 
                                                 fill = "Grupo")+
                                            scale_fill_manual(labels=c("Controle","Tratamento"),
                                                              values=c("red", "navy")) +
                                            theme(panel.grid = element_blank())

#ggsave(file = "Figuras/Português/suporte_comum_engenharia_quimica.eps")

#### 1.2) Suporte comum por quantil ####

painel_engenharia_quimica$quantil = with(painel_engenharia_quimica,
                                         cut(pscore,
                                             quantile(pscore,
                                                      prob = seq(0, 0.81788, length = 5),
                                                      type = 5)))

media_por_quantil_engenharia_quimica <- subset(painel_engenharia_quimica, !is.na(quantil)) %>%
  group_by(quantil, Tratamento) %>%
  summarise_at(vars(pscore), funs(mean(., na.rm=TRUE)))

grafico_suporte_comum_engenharia_quimica_quantil <- ggplot(subset(painel_engenharia_quimica, !is.na(quantil)),
                                                           aes(x = pscore, fill = Tratamento))+
                                                    geom_density(alpha = 0.25) +
                                                    labs(x = "Propensity score",
                                                         y = "Densidade", 
                                                         fill = "Grupo")+
                                                    scale_fill_manual(labels=c("Controle","Tratamento"),
                                                                      values=c("red", "navy")) +
                                                    facet_grid(quantil ~ .) +
                                                    theme(panel.grid = element_blank())

#ggsave(file = "Figuras/Português/suporte_comum_engenharia_quimica_quantis.eps")

#### 1.3) Modelo diff-diff simples ####

didreg_engenharia_quimica = lm(log_nota ~ Tratamento + Tempo + Tempo*Tratamento + idade_media +
                                 idade2_media + prop_brancos + prop_casados + prop_homens + 
                                 prop_medio_ou_mais + prop_renda_3SM,
                               data = painel_engenharia_quimica)

summary(didreg_engenharia_quimica)

#### 1.4) Modelo diff-and-diff com efeitos fixos ####

didreg_fe_engenharia_quimica <- plm(log_nota ~ Tempo + Tempo*Tratamento + idade_media +
                                      idade2_media + prop_brancos + prop_casados + prop_homens + 
                                      prop_medio_ou_mais + prop_renda_3SM,
                                    model = "within", index = c("CO_IES", "Ano"),
                                    data = painel_engenharia_quimica)

summary(didreg_fe_engenharia_quimica)

#### 1.5) Modelo diff-and-diff com efeitos fixos e PSM ####
#### 1.5.1) Criação dos pesos utilizando o modelo logit multinomial #----

W1 = weightit(Tratamento ~ idade_media + idade2_media  + 
                prop_brancos + prop_casados + prop_homens + 
                prop_medio_ou_mais + prop_renda_3SM,
              data = engenharia_quimica_2011,
              method = "ps",
              estimand = "ATT")

#### 1.5.2) Checagem do balanceamento #====

b1 = bal.tab(W1, un = TRUE,
             m.threshold=0.1,
             v.threshold=3)
print(b1)

g1_engenharia_quimica <- love.plot(W1, stats = c("m"), drop.distance = TRUE,
                                   thresholds = c(m=0.1),
                                   wrap = 10,
                                   var.order = "unadjusted",
                                   sample.names = c("Desbalanceado", "Balanceado - PS"),
                                   title = NULL,
                                   subtitle = "Amostra", line= T,
                                   var.names = names,
                                   colors = c("red", "navy"),
                                   shapes = c("triangle filled", "circle filled"))

g1_engenharia_quimica <- g1_engenharia_quimica + 
                         scale_color_manual(name = "Amostra", values = c("red", "navy")) + 
                         scale_shape_manual(name = "Amostra", values = c("triangle filled", "circle filled")) + 
                         guides(group = FALSE, size = FALSE, stroke = FALSE) + 
                         xlab("Diferenças médias \n padronizadas")


#### 1.5.3) Criação dos pesos utilizando o modelo de entropia #----

W2 = weightit(Tratamento ~ idade_media + idade2_media + 
                prop_brancos + prop_casados + prop_homens + 
                prop_medio_ou_mais + prop_renda_3SM,
              data = engenharia_quimica_2011,
              method = "ebal",
              estimand = "ATT")

W2_placebo <- weightit(Tratamento ~ prop_casados + prop_brancos + log_nota + 
                         prop_homens + prop_medio_ou_mais + prop_renda_3SM,
                       data = engenharia_quimica_2011,
                       method = "ebal",
                       estimand = "ATT")

#### 1.5.4) Checagem do balanceamento #====

b2 = bal.tab(W2, un = TRUE,
             m.threshold=0.1,
             v.threshold=3)
print(b2)

g2_engenharia_quimica <- love.plot(W2, stats = c("m"), drop.distance = TRUE,
                                   thresholds = c(m=0.1),
                                   wrap = 10,
                                   var.order = "unadjusted",
                                   sample.names = c("Desbalanceado", "Balanceado - Entropia"),
                                   title = NULL,
                                   subtitle = "Amostra", line= T,
                                   var.names = names,
                                   colors = c("red", "navy"),
                                   shapes = c("triangle filled", "circle filled"))

g2_engenharia_quimica <- g2_engenharia_quimica + 
                         scale_color_manual(name = "Amostra", values = c("red", "navy")) + 
                         scale_shape_manual(name = "Amostra", values = c("triangle filled", "circle filled")) + 
                         guides(group = FALSE, size = FALSE, stroke = FALSE) + 
                         xlab("Diferenças médias \n padronizadas")


b2_placebo = bal.tab(W2_placebo, un = TRUE,
                     m.threshold=0.1,
                     v.threshold=3)
print(b2_placebo)

g3_engenharia_quimica <- love.plot(W2_placebo, stats = c("m"), drop.distance = TRUE,
                                   thresholds = c(m=0.1),
                                   wrap = 10,
                                   var.order = "unadjusted",
                                   sample.names = c("Desbalanceado", "Balanceado - Entropia"),
                                   title = NULL,
                                   subtitle = "Amostra", line= T,
                                   var.names = names_placebo,
                                   colors = c("red", "navy"),
                                   shapes = c("triangle filled", "circle filled"))

g3_engenharia_quimica <- g3_engenharia_quimica + 
                         scale_color_manual(name = "Amostra", values = c("red", "navy")) + 
                         scale_shape_manual(name = "Amostra", values = c("triangle filled", "circle filled")) + 
                         guides(group = FALSE, size = FALSE, stroke = FALSE) + 
                         xlab("Diferenças médias \n padronizadas")


#### 1.5.6) Unindo os pesos ao data frame ####

W1_matrix_engenharia_quimica <- data.frame(CO_IES = engenharia_quimica_2011$CO_IES,
                                           W1 = W1$weights)

painel_engenharia_quimica_logit <- left_join(painel_engenharia_quimica,
                                             W1_matrix_engenharia_quimica, by = "CO_IES")

W2_matrix_engenharia_quimica <- data.frame(CO_IES = engenharia_quimica_2011$CO_IES,
                                           W2 = W2$weights)

painel_engenharia_quimica_entropia <- left_join(painel_engenharia_quimica,
                                                W2_matrix_engenharia_quimica, by = "CO_IES")

W2_placebo_matrix_engenharia_quimica <- data.frame(CO_IES = engenharia_quimica_2011$CO_IES,
                                                   W2_placebo = W2_placebo$weights)

painel_engenharia_quimica_entropia_placebo <- left_join(painel_engenharia_quimica,
                                                        W2_placebo_matrix_engenharia_quimica, by = "CO_IES")

#### 1.5.7) Regressão - Pesos calculados utilizando logit multinomial ####

didreg_fe_logit_engenharia_quimica <- plm(log_nota ~ Tratamento + Tempo + Tempo*Tratamento + idade_media +
                                            idade2_media + prop_brancos + prop_casados + prop_homens + 
                                            prop_medio_ou_mais + prop_renda_3SM,
                                          model = "within", index = c("CO_IES", "Ano"), weights = W1,
                                          data = painel_engenharia_quimica_logit)

summary(didreg_fe_logit_engenharia_quimica)

#### 1.5.8) Regressão - Pesos calculados utilizando entropia ####

didreg_fe_entropia_engenharia_quimica <- plm(log_nota ~ Tratamento + Tempo + Tempo*Tratamento + idade_media +
                                               idade2_media + prop_brancos + prop_casados + prop_homens + 
                                               prop_medio_ou_mais + prop_renda_3SM,
                                             model = "within", index = c("CO_IES", "Ano"), weights = W2,
                                             data = painel_engenharia_quimica_entropia)

summary(didreg_fe_entropia_engenharia_quimica)

#### 1.6) Placebo - Tratamento sobre idade média -  Pesos calculados utilizando entropia ####

didreg_fe_entropia_placebo_engenharia_quimica <- plm(idade_media ~ Tratamento + Tempo + Tempo*Tratamento + prop_casados +
                                                       prop_brancos + nota_media + prop_homens + 
                                                       prop_medio_ou_mais + prop_renda_3SM,
                                                     model = "within", index = c("CO_IES", "Ano"), weights = W2_placebo,
                                                     data = painel_engenharia_quimica_entropia_placebo)

summary(didreg_fe_entropia_placebo_engenharia_quimica)

#### 1.7) Visualizando os resultados ####

stargazer(didreg_engenharia_quimica, didreg_fe_engenharia_quimica, didreg_fe_logit_engenharia_quimica, didreg_fe_entropia_engenharia_quimica,
          type = "text", out = "Resultados/engenharia_quimica.txt",
          title = "Efeito médio de Tratamento sobre os tratados - Engenharia Química - Ciclo 2011-2014",
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

stargazer(didreg_fe_entropia_placebo_engenharia_quimica,
          type = "text", out = "Resultados/engenharia_quimica_placebo.txt",
          title = "Placebo - Tendências paralelas - Agronomia - Ciclo 2011-2014",
          decimal.mark = ",", digits = 3,
          model.numbers = TRUE,
          align = TRUE,
          float = TRUE, float.env = "table",
          initial.zero = TRUE, intercept.bottom = TRUE,
          multicolumn = TRUE, model.names = FALSE,
          font.size = "small")

## 2) História ####

historia_2011 <- subset(painel_historia, Ano == 2011)

#### 2.1) Hipótese do suporte comum ####

ps <- glm(data = painel_historia,
          Tratamento ~ idade_media + idade2_media + prop_brancos +
            prop_casados + prop_homens + prop_medio_ou_mais + 
            prop_renda_3SM,
          family = binomial(link = "logit"))

painel_historia$pscore <- predict(ps, type = "response")

grafico_suporte_comum_historia <- ggplot(painel_historia, aes(x = pscore, fill = Tratamento))+
                                  geom_density(alpha = 0.25) +
                                  labs(x = "Propensity score",
                                       y = "Densidade", 
                                       fill = "Grupo")+
                                  scale_fill_manual(labels=c("Controle","Tratamento"),
                                                    values=c("red", "navy")) +
                                  theme(panel.grid = element_blank())

#ggsave(file = "Figuras/Português/suporte_comum_historia.eps")

#### 2.2) Suporte comum por quantil ####

painel_historia$quantil = with(painel_historia,
                               cut(pscore,
                                   quantile(pscore,
                                            prob = seq(0, 0.93642, length = 5),
                                            type = 5)))

media_por_quantil_historia <- subset(painel_historia, !is.na(quantil)) %>%
  group_by(quantil, Tratamento) %>%
  summarise_at(vars(pscore), funs(mean(., na.rm=TRUE)))

grafico_suporte_comum_historia_quantil <- ggplot(subset(painel_historia, !is.na(quantil)),
                                                 aes(x = pscore, fill = Tratamento))+
                                          geom_density(alpha = 0.25) +
                                          labs(x = "Propensity score",
                                               y = "Densidade", 
                                               fill = "Grupo")+
                                          scale_fill_manual(labels=c("Controle","Tratamento"),
                                                            values=c("red", "navy")) +
                                          facet_grid(quantil ~ .) +
                                          theme(panel.grid = element_blank())

#ggsave(file = "Figuras/Português/suporte_comum_historia_quantis.eps")

#### 2.3) Modelo diff-diff simples ####

didreg_historia = lm(log_nota ~ Tratamento + Tempo + Tempo*Tratamento + idade_media +
                       idade2_media + prop_brancos + prop_casados + prop_homens + 
                       prop_medio_ou_mais + prop_renda_3SM,
                     data = painel_historia)

summary(didreg_historia)

#### 2.4) Modelo diff-and-diff com efeitos fixos ####

didreg_fe_historia <- plm(log_nota ~ Tempo + Tempo*Tratamento + idade_media +
                            idade2_media + prop_brancos + prop_casados + prop_homens + 
                            prop_medio_ou_mais + prop_renda_3SM,
                          model = "within", index = c("CO_IES", "Ano"),
                          data = painel_historia)

summary(didreg_fe_historia)

#### 2.5) Modelo diff-and-diff com efeitos fixos e PSM ####
#### 2.5.1) Criação dos pesos utilizando o modelo logit multinomial #----

W1 = weightit(Tratamento ~ idade_media + idade2_media  + 
                prop_brancos + prop_casados + prop_homens + 
                prop_medio_ou_mais + prop_renda_3SM,
              data = historia_2011,
              method = "ps",
              estimand = "ATT")

#### 2.5.2) Checagem do balanceamento #====

b1 = bal.tab(W1, un = TRUE,
             m.threshold=0.1,
             v.threshold=3)
print(b1)

g1_historia <- love.plot(W1, stats = c("m"), drop.distance = TRUE,
                         thresholds = c(m=0.1),
                         wrap = 10,
                         var.order = "unadjusted",
                         sample.names = c("Desbalanceado", "Balanceado - PS"),
                         title = NULL,
                         subtitle = "Amostra", line= T,
                         var.names = names,
                         colors = c("red", "navy"),
                         shapes = c("triangle filled", "circle filled"))

g1_historia <- g1_historia + 
               scale_color_manual(name = "Amostra", values = c("red", "navy")) + 
               scale_shape_manual(name = "Amostra", values = c("triangle filled", "circle filled")) + 
               guides(group = FALSE, size = FALSE, stroke = FALSE) + 
               xlab("Diferenças médias \n padronizadas")

#### 2.5.3) Criação dos pesos utilizando o modelo de entropia #----

W2 = weightit(Tratamento ~ idade_media + idade2_media + 
                prop_brancos + prop_casados + prop_homens + 
                prop_medio_ou_mais + prop_renda_3SM,
              data = historia_2011,
              method = "ebal",
              estimand = "ATT")

W2_placebo <- weightit(Tratamento ~ prop_casados + prop_brancos + log_nota + 
                         prop_homens + prop_medio_ou_mais + prop_renda_3SM,
                       data = historia_2011,
                       method = "ebal",
                       estimand = "ATT")

#### 2.5.4) Checagem do balanceamento #====

b2 = bal.tab(W2, un = TRUE,
             m.threshold=0.1,
             v.threshold=3)
print(b2)

g2_historia <- love.plot(W2, stats = c("m"), drop.distance = TRUE,
                         thresholds = c(m=0.1),
                         wrap = 10,
                         var.order = "unadjusted",
                         sample.names = c("Desbalanceado", "Balanceado - Entropia"),
                         title = NULL,
                         subtitle = "Amostra", line= T,
                         var.names = names,
                         colors = c("red", "navy"),
                         shapes = c("triangle filled", "circle filled"))

g2_historia <- g2_historia + 
               scale_color_manual(name = "Amostra", values = c("red", "navy")) + 
               scale_shape_manual(name = "Amostra", values = c("triangle filled", "circle filled")) + 
               guides(group = FALSE, size = FALSE, stroke = FALSE) + 
               xlab("Diferenças médias \n padronizadas")


b2_placebo = bal.tab(W2_placebo, un = TRUE,
                     m.threshold=0.1,
                     v.threshold=3)
print(b2_placebo)

g3_historia <- love.plot(W2_placebo, stats = c("m"), drop.distance = TRUE,
                         thresholds = c(m=0.1),
                         wrap = 10,
                         var.order = "unadjusted",
                         sample.names = c("Desbalanceado", "Balanceado - Entropia"),
                         title = NULL,
                         subtitle = "Amostra", line= T,
                         var.names = names_placebo,
                         colors = c("red", "navy"),
                         shapes = c("triangle filled", "circle filled"))

g3_historia <- g3_historia + 
               scale_color_manual(name = "Amostra", values = c("red", "navy")) + 
               scale_shape_manual(name = "Amostra", values = c("triangle filled", "circle filled")) + 
               guides(group = FALSE, size = FALSE, stroke = FALSE) + 
               xlab("Diferenças médias \n padronizadas")


#### 2.5.6) Unindo os pesos ao data frame ####

W1_matrix_historia <- data.frame(CO_IES = historia_2011$CO_IES,
                                 W1 = W1$weights)

painel_historia_logit <- left_join(painel_historia,
                                   W1_matrix_historia, by = "CO_IES")

W2_matrix_historia <- data.frame(CO_IES = historia_2011$CO_IES,
                                 W2 = W2$weights)

painel_historia_entropia <- left_join(painel_historia,
                                      W2_matrix_historia, by = "CO_IES")

W2_placebo_matrix_historia <- data.frame(CO_IES = historia_2011$CO_IES,
                                         W2_placebo = W2_placebo$weights)

painel_historia_entropia_placebo <- left_join(painel_historia,
                                              W2_placebo_matrix_historia, by = "CO_IES")

#### 2.5.7) Regressão - Pesos calculados utilizando logit multinomial ####

didreg_fe_logit_historia <- plm(log_nota ~ Tratamento + Tempo + Tempo*Tratamento + idade_media +
                                  idade2_media + prop_brancos + prop_casados + prop_homens + 
                                  prop_medio_ou_mais + prop_renda_3SM,
                                model = "within", index = c("CO_IES", "Ano"), weights = W1,
                                data = painel_historia_logit)

summary(didreg_fe_logit_historia)

#### 2.5.8) Regressão - Pesos calculados utilizando entropia ####

didreg_fe_entropia_historia <- plm(log_nota ~ Tratamento + Tempo + Tempo*Tratamento + idade_media +
                                     idade2_media + prop_brancos + prop_casados + prop_homens + 
                                     prop_medio_ou_mais + prop_renda_3SM,
                                   model = "within", index = c("CO_IES", "Ano"), weights = W2,
                                   data = painel_historia_entropia)

summary(didreg_fe_entropia_historia)

#### 2.6) Placebo - Tratamento sobre idade média -  Pesos calculados utilizando entropia ####

didreg_fe_entropia_placebo_historia <- plm(idade_media ~ Tratamento + Tempo + Tempo*Tratamento + prop_casados +
                                             prop_brancos + nota_media + prop_homens + 
                                             prop_medio_ou_mais + prop_renda_3SM,
                                           model = "within", index = c("CO_IES", "Ano"), weights = W2_placebo,
                                           data = painel_historia_entropia_placebo)

summary(didreg_fe_entropia_placebo_historia)

#### 2.7) Visualizando os resultados ####

stargazer(didreg_historia, didreg_fe_historia, didreg_fe_logit_historia, didreg_fe_entropia_historia,
          type = "text", out = "Resultados/historia.txt",
          title = "Efeito médio de Tratamento sobre os tratados - História - Ciclo 2011-2014",
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

stargazer(didreg_fe_entropia_placebo_historia,
          type = "text", out = "Resultados/historia_placebo.txt",
          title = "Placebo - Tendências paralelas - História - Ciclo 2011-2014",
          decimal.mark = ",", digits = 3,
          model.numbers = TRUE,
          align = TRUE,
          float = TRUE, float.env = "table",
          initial.zero = TRUE, intercept.bottom = TRUE,
          multicolumn = TRUE, model.names = FALSE,
          font.size = "small")

#### GRÁFICOS AGREGADOS ####

## 1) SUPORTE COMUM ####

g_2009_2012 <- plot_grid(grafico_suporte_comum_ciencias_economicas, grafico_suporte_comum_direito, ncol = 2, vjust = 2,
                         labels = c("a) Economia", "b) Direito"), label_size = 10, label_fontface = "bold", scale = 0.75, label_y = 1)

g_2010_2013 <- plot_grid(grafico_suporte_comum_agronomia, grafico_suporte_comum_medicina, ncol = 2, vjust = 2,
                         labels = c("a) Agronomia", "b) Medicina"), label_size = 10, label_fontface = "bold", scale = 0.75, label_y = 1)

g_2011_2014 <- plot_grid(grafico_suporte_comum_engenharia_quimica, grafico_suporte_comum_historia, ncol = 2, vjust = 2,
                         labels = c("a) Eng. Química", "b) História"), label_size = 10, label_fontface = "bold", scale = 0.75, label_y = 1)

g_suporte_comum <- plot_grid(g_2009_2012, g_2010_2013, g_2011_2014, nrow = 3,
                             labels = c("1) Ciclo 2009-2012",
                                        "2) Ciclo 2010-2013",
                                        "3) Ciclo 2011-2014"),
                             label_size = 10, label_fontface = "bold", scale = 0.75)

#ggsave("Figuras/Português/suporte_comum_areas.png", height = 12, width = 8)

## 2) TENDÊNCIAS ####

g_tendencia_2009_2012 <- plot_grid(grafico_tendencias_economia, grafico_tendencias_direito, ncol = 2, vjust = 2,
                                   labels = c("a) Economia", "b) Direito"), label_size = 10, label_fontface = "bold", scale = 0.75,label_y = 1)
g_tendencia_2010_2013 <- plot_grid(grafico_tendencias_agronomia, grafico_tendencias_medicina, ncol = 2, vjust = 2,
                                   labels = c("a) Agronomia", "b) Medicina"), label_size = 10, label_fontface = "bold", scale = 0.75,label_y = 1)
g_tendencia_2011_2014 <- plot_grid(grafico_tendencias_engenharia_quimica, grafico_tendencias_historia, ncol = 2, vjust = 2,
                                   labels = c("a) Eng. Química", "b) História"), label_size = 10, label_fontface = "bold", scale = 0.75,label_y = 1)

g_tendencia <- plot_grid(g_tendencia_2009_2012, g_tendencia_2010_2013, g_tendencia_2011_2014, nrow = 3,
                         labels = c("1) Ciclo 2009-2012",
                                    "2) Ciclo 2010-2013",
                                    "3) Ciclo 2011-2014"),
                         label_size = 10, label_fontface = "bold", scale = 0.75)

#ggsave("Figuras/Português/tendencias_por_area.eps", height = 12, width = 8)

## 3) BALANCEAMENTO DAS COVARIADAS ####

g_balanceamento_2009_2012 <- plot_grid(g2_economia, g2_direito, nrow = 2, labels = c("a) Economia", "b) Direito"),
                                       label_size = 10, label_fontface = "bold")

#ggsave("Figuras/Português/balanceamento_2009_2012.eps", height = 22, width = 15)

g_balanceamento_2010_2013 <- plot_grid(g2_agronomia, g2_medicina, nrow = 2, labels = c("a) Agronomia", "b) Medicina"),
                                       label_size = 10, label_fontface = "bold")

#ggsave("Figuras/Português/balanceamento_2010_2013.eps", height = 22, width = 15)

g_balanceamento_2011_2014 <- plot_grid(g2_engenharia_quimica, g2_historia, nrow = 2, labels = c("a) Eng. Química", "b) História"),
                                       label_size = 10, label_fontface = "bold")

#ggsave("Figuras/Português/balanceamento_2011_2014.eps", height = 22, width = 15)

g_balanceamento_placebo_2009_2012 <- plot_grid(g3_economia, g3_direito, ncol = 2, labels = c("a) Economia", "b) Direito"),
                                               label_size = 7, label_fontface = "bold")


g_balanceamento_placebo_2010_2013 <- plot_grid(g3_agronomia, g3_medicina, ncol = 2, labels = c("a) Agronomia", "b) Medicina"),
                                               label_size = 7, label_fontface = "bold")


g_balanceamento_placebo_2011_2014 <- plot_grid(g3_engenharia_quimica, g3_historia, ncol = 2, labels = c("a) Eng. Química", "b) História"),
                                               label_size = 7, label_fontface = "bold")

