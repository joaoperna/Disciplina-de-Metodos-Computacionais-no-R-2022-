#Limpando a memória Global
rm(list=ls())
#Setando o wd manualmente eo R_site para evitar msg
R_LIBS_SITE="C:/Program Files/R/R-4.2.0/R repositório"

#Baixando Pacotes se necessários
#Baixando Banco de Dados


library(bit64)
library(tidyverse)
library(basedosdados)

basedosdados::set_billing_id("teste-base-dos-dados-376717")


query <- bdplyr("br_inep_ideb.brasil")
df <- bd_collect(query)


query57 <- basedosdados::read_sql(query = "SELECT * FROM `basedosdados.br_inep_ideb.escola`")
query56 <- basedosdados::read_sql(query = "SELECT ano, sigla_uf, id_escola, indicador_rendimento 
                                  FROM basedosdados.br_inep_ideb.escola")
                                  
query56 <- na.omit(query56)


read_sql(
  "SELECT * FROM basedosdados.br_inep_ideb.escola",
  page_size = 100000)
help(page_size)

(tibble(
  query = c(
    "SELECT * FROM `basedosdados.br_inep_ideb.escola`",
    "SELECT ana.id_municipio, ana.indice_sem_atendimento_sem_coleta_sem_tratamento
     FROM `basedosdados.br_ana_atlas_esgotos.municipio` as ana",
    "SELECT * FROM `basedosdados.br_ibge_pib.municipio`")) %>%
    mutate(resultados = map(query, read_sql)) ->
    queries)

(queries %>%
    pull(resultados) %>%
    reduce(left_join) ->
    painel)

painel %>%
  filter(!is.na(ideb), ano > 2004) %>%
  mutate(across(where(is.integer64), as.integer)) %>%
  group_by(ano) %>% 
  summarise(media_ideb = mean(ideb, na.rm = TRUE)) %>%
  ggplot(aes(x = ano, y = media_ideb)) +
  geom_line(size = 1.2) +
  theme_minimal() +
  labs(
    x = "Ano",
    y = "Média de Ideb nacional",
    title = "OLHA SÓ O IDEB TIRADO DO BASE DOS DADOS")

# IDEB médio dos municípios por percentual de descobertura de esgoto
painel %>%
  mutate(across(where(is.integer64), as.integer)) %>%
  filter(ano == 2013) %>%
  group_by(id_municipio, rede) %>%
  summarise(
    rede = rede,
    descobertura = indice_sem_atendimento_sem_coleta_sem_tratamento,
    ideb_medio = mean(ideb, na.rm = TRUE),
    .groups = "drop") %>%
  distinct(id_municipio, rede, .keep_all = TRUE) %>%
  ggplot(aes(x = descobertura, y = ideb_medio, color = rede)) +
  geom_point() +
  labs(
    x = "Descobertura de esgoto",
    y = "Ideb Médio do município",
    title = "ESGOTO TRATADO É BOM DEMAIS") +
  theme_minimal() +
  geom_smooth(method = "lm")

