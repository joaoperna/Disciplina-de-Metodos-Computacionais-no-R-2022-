#Limpando a memória Global
rm(list=ls())
#Setando o wd manualmente eo R_site para evitar msg
R_LIBS_SITE="C:/Program Files/R/R-4.2.0/R repositório"

#Baixando Pacotes se necessários
#Baixando Banco de Dados

#Exemplo: if(!require(dplyr)) install.packages('dplyr')
#library(dplyr)
install.packages('bigrquery')
library(bigrquery)
library(bit64)
library(tidyverse)
library(basedosdados)
library(magrittr)
library(dplyr)
library(purrr)

basedosdados::set_billing_id("teste-base-dos-dados-376717")

basedosdados::read_sql(
  "SELECT * FROM `basedosdados.br_inep_ideb.escola`")
  

basedosdados::download(
  "SELECT * FROM `basedosdados.br_inep_ideb.escola`",
  path = "data/ideb.csv")



(tibble(
  query = c(
    "SELECT * FROM `basedosdados.br_inep_ideb.escola`",
    "SELECT ana.id_municipio,
     FROM `basedosdados.br_ana_atlas_esgotos.municipio` as ana",
    "SELECT * FROM `basedosdados.br_ibge_pib.municipio`")) %>%
    mutate(resultados = map(query, basedosdados::read_sql)) ->
    queries %>%
    pull(resultados) %>%
    reduce(left_join) ->
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
    title = "OLHA SÓ O IDEB TIRADO DO BASE DOS DADOS"))

# IDEB médio dos municípios por percentual de descobertura de esgoto
painel %>%
  mutate(across(where(is.integer64), as.integer)) %>%
  filter(ano == 2013) %>%
  group_by(id_municipio, rede) %>%
  summarise(
    rede = rede,
    descobertura = indice_sem_atend,
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

---------------------------
#óbitos
query1 <- "SELECT ano, id_municipio, SUM(numero_obitos) AS obitos
FROM `basedosdados.br_ms_sim.municipio_causa`
WHERE LEFT(causa_basica,1) = 'J' # Apenas doenças respiratórias
GROUP BY ano, id_municipio"

obitos <- basedosdados::read_sql(query1)

#população
query2 <- "SELECT * FROM `basedosdados.br_ibge_populacao.municipio`"

pop <- basedosdados::read_sql(query2)

#PIB
query3 <- "SELECT id_municipio, ano, VA_industria
FROM `basedosdados.br_ibge_pib.municipio`"

pib <- basedosdados::read_sql(query3)

df <- left_join(obitos, pop, by = c("id_municipio", "ano"))
df <- left_join(df, pib, by = c("id_municipio", "ano"))

df_18 <- df %>%
  filter(ano == 2018) %>%
  mutate(obitos_pc = obitos*100000/populacao,
         industria_pc = VA_industria/populacao,
         log_industria_pc = log(industria_pc),
         pc = predict(prcomp(~log_industria_pc+obitos_pc, .))[,1])


ggplot(data = df_18, aes(x = log_industria_pc, y = obitos_pc, color = pc)) +
  geom_point(show.legend = FALSE, shape = 16, size = 2, alpha = .5) +
  theme_minimal() +
  ylab("Óbitos/100 mil habitantes por doenças respiratórias") +
  xlab("Valor adicionado pela indústria (em log)") +
  scale_color_gradient(low = "#0091ff", high = "#f0650e")

