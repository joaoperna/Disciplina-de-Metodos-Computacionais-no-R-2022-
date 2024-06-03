#Limpando a memória Global
rm(list=ls())
#Setando o wd manualmente eo R_site para evitar msg
R_LIBS_SITE="C:/Program Files/R/R-4.2.0/R repositório"

#Baixando Pacotes se necessários
#Baixando Banco de Dados

library(basedosdados)
library(tidyverse)

set_billing_id("teste-base-dos-dados-376717")


read_sql(query = "SELECT * FROM `basedosdados.br_ana_atlas_esgotos.municipio`")
base <- basedosdados::read_sql(query = 'SELECT * FROM `basedosdados.br_ana_atlas_esgotos.municipio` ')

glimpse(base)
base_cobertura <- basedosdados::read_sql(query = 'SELECT id_municipio, indice_sem_atendimento_sem_coleta_sem_tratamento 
FROM `basedosdados.br_ana_atlas_esgotos.municipio`')

base_norte <- read_sql('SELECT * 
FROM `basedosdados.br_ana_atlas_esgotos.municipio` 
WHERE sigla_uf in ("AM","AP","RO","RR","AC", "PA")')

base_uf <- basedosdados::read_sql(query = 
                                    'SELECT sigla_uf, AVG(indice_sem_atendimento_sem_coleta_sem_tratamento) as sem_esgoto
FROM `basedosdados.br_ana_atlas_esgotos.municipio` 
GROUP BY sigla_uf ' )

base_uf%>%
  ggplot(aes(y = sem_esgoto, x = reorder(sigla_uf, -sem_esgoto))) + geom_col(fill = '#7cb342') +
  labs(x = "Estado", y = "Porcentagem média sem saneamento", 
       title = "População sem saneamento básico",
       subtitle = "Média da porcentagem da população municipal sem saneamento, por UF") + theme_classic()
