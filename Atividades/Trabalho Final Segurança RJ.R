#Limpando a memória global
rm(list= ls())
#Ativando Bibliotecas
library(tidyverse)
library(basedosdados)
#Setando o o billing do Google Clound
set_billing_id("teste-base-dos-dados-376717")
#Realizando as Querys e baixando o banco de dados
query6 <-  bdplyr("br_isp_estatisticas_seguranca.evolucao_mensal_uf")
#puxando os dados especificos do ano de 2019 em todo estado do Rio de Janeiro x cidades
seguranca_dados_ano5 <- basedosdados::read_sql(query = "SELECT * FROM `basedosdados.br_isp_estatisticas_seguranca.evolucao_mensal_uf` WHERE ANO BETWEEN 2010 AND 2021")
#Tratando dados
seguranca_dados_ano55 <- seguranca_dados_ano5 %>% 
  mutate(hom_doloso = as.numeric(hom_doloso)) %>% 
  mutate(lesao_corp_morte = as.numeric(lesao_corp_morte)) %>% 
  mutate(lesao_corp_dolosa = as.numeric(lesao_corp_dolosa)) %>% 
  mutate(outros_roubos = as.numeric(outros_roubos)) %>% 
  mutate(total_roubos = as.numeric(total_roubos)) %>% 
  mutate(total_furtos = as.numeric(total_furtos)) %>% 
  mutate(posse_drogas = as.numeric(posse_drogas)) %>%
  mutate(sequestro = as.numeric(sequestro))
#Realizando a Regressaão 
attach(seguranca_dados_ano55)
regm <- lm(hom_doloso ~ lesao_corp_morte +
               lesao_corp_dolosa + outros_roubos + total_roubos 
             +total_furtos + sequestro + posse_drogas)
summary(regm)