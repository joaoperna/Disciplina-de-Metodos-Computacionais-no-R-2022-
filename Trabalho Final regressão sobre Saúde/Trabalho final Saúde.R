#Limpando a memória global
rm(list= ls())
#Setando diretório
setwd("C:/Users/João Perna/Desktop/Caixa de Entrada/PNAD")
#Ativando Bibliotecas
library(tidyverse)
library(basedosdados)
#Setando o o billing do Google Clound
set_billing_id("teste-base-dos-dados-376717")
#Realizando as Querys e baixando o banco de dados
query <-  bdplyr("br_ibge_pib.municipio")
query1 <-  bdplyr("br_ibge_populacao.municipio")
query2 <-  bdplyr("br_ms_atencao_basica.municipio")
#Puxandos os dados referentes ao Estado do Tocantins das bases de dados de população e pib e juntando ambas, para os municípios do Estado do Tocantins
pib_per_cap <- basedosdados::read_sql(query = "SELECT pib.id_municipio,pop.ano,  pib.PIB / pop.populacao as pib_pc
FROM `basedosdados.br_ibge_pib.municipio` as pib 
INNER JOIN `basedosdados.br_ibge_populacao.municipio` as pop
ON pib.id_municipio = pop.id_municipio AND pib.ano = pop.ano
WHERE pib.id_municipio BETWEEN '1700000' AND '1800000' AND pib.ano BETWEEN 2010 AND 2020")
#Baíxando os dados referente a indicadores da atenção básica dos municípios do estado do Tocantins
atenca_basica <- basedosdados::read_sql(query = "SELECT * 
FROM `basedosdados.br_ms_atencao_basica.municipio` 
WHERE id_municipio BETWEEN '1700000' AND '1800000' AND ano BETWEEN 2010 AND 2020")
#Realizando a junção bancos de dados
final2 <- atenca_basica  %>% 
  left_join(pib_per_cap, by = c('id_municipio' = 'id_municipio', 'ano' = 'ano'))
#Tratando os dados 
final2$lpib <- log(final2$pib_pc)
final23 <- final2 %>% 
  mutate(quantidade_equipes_atencao_basica_equivalente = as.numeric(quantidade_equipes_atencao_basica_equivalente)) %>%
  mutate(quantidade_equipes_atencao_basica_parametrizada   = as.numeric(quantidade_equipes_atencao_basica_parametrizada  )) %>%
  mutate(quantidade_equipes_saude_familia = as.numeric(quantidade_equipes_saude_familia)) %>%
  mutate(quantidade_equipes_atencao_basica_total  = as.numeric(quantidade_equipes_atencao_basica_total )) %>%
  mutate(populacao_coberta_estrategia_saude_familia = as.numeric(populacao_coberta_estrategia_saude_familia)) %>%
  mutate(populacao_coberta_total_atencao_basica = as.numeric(populacao_coberta_total_atencao_basica))
#Realizando a regressão 
options(scipen = 9999)
rgm1 <- lm(lpib ~ carga_horaria_medica_atencao_basica_tradicional + carga_horaria_enfermagem_atencao_basica_tradicional +
             proporcao_cobertura_estrategia_saude_familia + populacao_coberta_total_atencao_basica+
             quantidade_equipes_atencao_basica_equivalente + quantidade_equipes_atencao_basica_parametrizada+
             quantidade_equipes_saude_familia + quantidade_equipes_atencao_basica_total +
             populacao_coberta_estrategia_saude_familia + populacao_coberta_total_atencao_basica, data = final23)
summary(rgm1)


save.image('FinalPIBsaude'.rds)
saveRDS(final23 ,"FinalPIBsaude")
