#Meu primeiro script

#Definindo o diretorio
setwd("~/Documents/Teaching/metodoscomputacionais/rfiles")

#consultando meu diretorio
getwd()

#Notacao cientifica
options(scipen = 9999)

#Listar os arquivos do diretorio
dir()

#Listar objetos do workspace
ls()

#Limpar meu workspace/memoria
rm(list = ls())

#Limpar graficos
graphics.off()

#savar meu workspace
save.image("meuworkspace.RData")

#Ativando meu workspace
load("meuworkspace.RData")

#remover objetos
rm(x)

#Fechar a secao e savar o workspace
#q()

#Pacotes/bibliotecas
install.packages("tidyverse")
library(tidyverse)
#require(tidyverse)

#simbolos especiais
NA #missing values
NaN #Nao numerico (0/0)
FALSE #valor logico falso (F)
TRUE #valor logico verdadeiro (T)
-Inf # menos infinito (log(0))
Inf # menos infinito (2/0)

#cuidados
5%/%3 #divisao sem posicao de decimal
5%%3 #divisao com posicao de decimal

#operacoes
2 #imprimir um numero
2+3 #soma
2-3 #subtracao
log(5) #logaritmo
exp(5) #exponencial

#vetores
Height <- c(168, 177, 177, 177, 178, 172, 165, 171, 178, 170)

#Podemos usar : e - simetricamente
Height[4-6]
Height[4:6]

#vetor com numeros naturais
obs <- 1:10

#pesos
Weight <- c(88, 72, 85, 52, 71, 69, 61, 61, 51, 75)

#Tamanho do vetor

length(Weight)

#medidas de variacao
mean(Weight)
var(Weight)

#classe do objeto
class(Height)

#estatistica descritiva
summary(Weight)

#Matrizes 
M <- cbind(obs,Height,Weight)

BMI <- Weight/((Height/100)^2)

#incluir o vetor BMI
M <- cbind(M,BMI)

#classe do objeto
class(M)

#tipo do objeto
typeof(M)

#verificando com um valor logico
is.matrix(M)

#Demensao
dim(M)

#Plot
plot(Height,Weight,ylab = "Weight",xlab = "Height")

#ativando a biblioteca tibble
library(tibble)

#convertendo a matriz em uma tibble
mydat <- as_tibble(M)

#removendo objeto do workspace
rm(a)

#Estatistica descritiva
summary(mydat)

#Leitura de dados no R
library(readr)

#escrevendo um arquivo csv
write_csv(mydat,"mydat.csv")

#lendo um arquivo csv
mydat2 <- read_csv("mydat.csv")

#definindo o tipo padrao da tibble
cols(
  obs <- col_double(),
  Height <- col_double(),
  Weight <- col_double(),
  BMI <- col_double()
)

#minha primeira funcao
myfunction <- function(a,b=2){
  r=a/b
  return(r)
}

#usando minha funcao
myfunction(4)
myfunction(4,1)
myfunction(b=4,a=1)

#Olhando a biblioteca tidyverse
install.packages("tidyverse")
library(tidyverse)

#analisando os dados mpg
ggplot2::mpg

#biblioteca ggplot
ggplot(data = mpg) +
  geom_point(mapping = aes(x=displ, y = hwy))

#Instalando o ggplot2
#install.packages("ggplot2")
#library(ggplot2)

#biblioteca ggplot - especificando cores por classe
ggplot(data = mpg) +
  geom_point(mapping = aes(x=displ, y = hwy, color = class))

#biblioteca ggplot - especificando cores por classe
ggplot(data = mpg) +
  geom_point(mapping = aes(x=displ, y = hwy, size = class))

#biblioteca ggplot - especificando transparencia por classe
ggplot(data = mpg) +
  geom_point(mapping = aes(x=displ, y = hwy, alpha = class))

#biblioteca ggplot - especificando a forma por classe
ggplot(data = mpg) +
  geom_point(mapping = aes(x=displ, y = hwy, shape = class))

#biblioteca ggplot - especificando a forma por classe
ggplot(data = mpg) +
  geom_point(mapping = aes(x=displ, y = hwy, color = "green"))

#biblioteca ggplot - especificando a forma por classe
ggplot(data = mpg) +
  geom_point(mapping = aes(x=displ, y = hwy),color="red")

#biblioteca ggplot - especificando facets
ggplot(data=mpg)+
  geom_point(mapping = aes(x=displ , y = hwy)) +
  facet_wrap(~class,nrow = 2)

#biblioteca ggplot - especificando grid
ggplot(data=mpg)+
  geom_point(mapping = aes(x=displ , y = hwy)) +
  facet_grid(drv ~ cyl)

#biblioteca ggplot - especificando smooth function
ggplot(data=mpg)+
  geom_smooth(mapping = aes(x=displ,y=hwy))

#biblioteca ggplot - especificando point and smooth function por classe
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = drv)) +
  geom_smooth(mapping = aes(x = displ, y = hwy, linetype = drv, color = drv))

#Usando o geom_bar and fill
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = cut))

#Analise de dados no R

#IPEADATA
#install.packages("remotes")
install.packages("ipeadatar")
library(ipeadatar)

#paises disponiveis
ipeadatar::available_countries()

#series disponiveis
ipeadatar::available_series()

#regioes disponiveis
ipeadatar::available_territories()

#baixar a serie ABIA12_ALIM12 (Produção física - alimentos )
serie_prod_fisica <- ipeadatar::ipeadata("ABIA12_ALIM12")
exp_semana <- ipeadatar::ipeadata("SECEX366_XVTOT366")

#baixando paineis do ipeadata
pib_real <- ipeadatar::ipeadata("PAN4_PIBPMG4")

#Pib real no grafico
ggplot(data=pib_real)+
  geom_line(mapping = aes(x=date,y=value))

#desemprego no ipeadata
tx_des <- ipeadatar::ipeadata("PNADC_TXDES_UF")

#Baixando dados do sidrar
library(sidrar)

#ipca no sidra
sidrar::info_sidra(3065)
ipca15 <- sidrar::get_sidra(3065, period = "202101-202105")

#Pesquisa mensal de emprego
sidrar::info_sidra(6442)
pms <- sidrar::get_sidra(6442)
pms_ <- sidrar::get_sidra(6442, period = "202101-202105", geo = "State", geo.filter = list("State"=17), header = TRUE,format = 4 )

#Baixando dados do rbcb
#install.packages("miniUI")
library(rbcb)

#Divida governo federal
divida_gov <- rbcb::get_series(4504)

#expectativa anual pib
expec <- rbcb::get_annual_market_expectations("PIB Total", end_date = "2020-01-01")

#expectativa inflacao mensal
expec_inf <- rbcb::get_monthly_market_expectations("IGP-DI", start_date = "2020-06-01", end_date = "2020-06-01")

#Baixando dados World Bank
library(wbstats)

#paises
wb_paises <- wbstats::wb_countries()

#indicadores
wb_indicadores <- wbstats::wb_indicators(lang = "en", include_archive = F)

#Produto agregado bruto
gdp <- wb_data(indicator = "NY.GDP.MKTP.CD", start_date = 2015, end_date = 2016, return_wide = T)

#Salvando dados em outro formato csv
readr::write_csv(gdp,"gdpwb.csv")

#Salvando dados em outro formato xlsx
writexl::write_xlsx(gdp, "gdpwb.xlsx")

#Salvando dados em outro formato xlsx
saveRDS(gdp, "gdpwb.RDS")

#Lendo os dados em rds
readRDS("gdpwb.RDS")

#Base dos dados
library(tidyverse)
library(basedosdados)

#Informando a ID do projeto do google cloud
basedosdados::set_billing_id("bancodata")

#Erro de autenticacao https://stackoverflow.com/questions/72663831/how-to-resolve-insufficient-authentication-scopes-in-r-package-bigrquery
ssp <- read_sql(queryb)

#Criando um query da base de dados
querya <- "SELECT * FROM `basedosdados.br_sp_gov_ssp.ocorrencias_registradas` LIMIT 100"

queryb <- "SELECT * FROM `basedosdados.br_sp_gov_ssp.ocorrencias_registradas` WHERE ano BETWEEN 2012 AND 2019"

#Leitura e manipulação dos dados - Segurança SP 
grande_sp <- ssp %>%
  filter(regiao_ssp == "Grande São Paulo (exclui a Capital)") %>% 
  mutate(id_municipio = as.numeric(id_municipio)) %>% 
  mutate(furto_de_veiculo = as.numeric(furto_de_veiculo)) %>% 
  mutate(ano = as.character(ano))

#Roubo de veiculos por municipios Osasco, Barueri e Jandira "Grande São Paulo (exclui a Capital)"
furto_grandesp <- grande_sp %>%
  select(ano, mes, id_municipio, regiao_ssp, roubo_de_veiculo, furto_de_veiculo) %>%
  filter(id_municipio %in% c(3525003, 3505708, 3534401))

#Grafico de furtos
ggplot(furto_grandesp, aes(x=ano, y = furto_de_veiculo)) +
  geom_bar(stat = "identity") +
  ggtitle("Furtos de veiculos")

#Dados do futebol brasileiro
query <- "SELECT * FROM
`basedosdados.mundo_transfermarkt_competicoes.brasileirao_serie_a`
WHERE ano_campeonato = 2020"

#lendo o banco de dados
data <- read_sql(query)

#limpando meu banco de dados
soma <- data %>%
  select(idade_media_titular_man, time_man, rodada) %>%
  mutate(idade_media_titular_man = as.numeric(idade_media_titular_man)) %>%
  mutate(rodada = as.numeric(rodada)) %>% 
  filter(time_man == "Santos FC")
df <- soma

ggplot(df, aes(x= rodada, y= idade_media_titular_man))+
  geom_line() +
  geom_point() +
  labs(x= "Rodada", y= "Idade media como Mandante") +
  ggtitle("Santos FC - Idade media como mandante/2020") +
  scale_y_continuous(limits = c(23,28))

#Series temporais bacen e fgv/ibre
#install.packages("BETS")
library(BETS)

igpm <- BETSsearch(description = "IGP-M")

#Yahoo Finance BatchGetSymbols will soon be substituted by yfR <https://github.com/msperlin/yfR> 
#Analyzing Financial and Economic Data with R is available at https://www.msperlin.com/afedR/
#install.packages("BatchGetSymbols")
library(BatchGetSymbols)

#install.packages("yfR")
library(yfR)

#Baixando dados da pnadc
#install.packages("PNADcIBGE")
library(PNADcIBGE)

#Extraindo dados da pnadc com design
pnadc2017_design <- get_pnadc(year = 2017, quarter = 3, vars = c("VD4001","VD4002"))
class(pnadc2017)

#Extraindo dados da pnadc sem design
pnadc2017_ndesign <- get_pnadc(year = 2017, quarter = 3, vars = c("VD4001","VD4002"), labels = F, design = F)
class(pnadc2017)

#Carregando microdados da pnadc em um diretorio temporario
pnadc2017_temp <- get_pnadc(2017, quarter = 3, interview = NULL, vars = c("VD4001","VD4002"), labels = F, savedir = tempdir())

#Escolhendo a entrevista
pnadc2017_inter <- get_pnadc(year = 2016, interview = 1)
head()

#Importando dados offline
#Baixar os arquivo .txt em: https://www.ibge.gov.br/estatisticas/downloads-estatisticas.html?caminho=Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Trimestral/Microdados/2022
#Opcao trimestral em: https://www.ibge.gov.br/estatisticas/sociais/trabalho/9171-pesquisa-nacional-por-amostra-de-domicilios-continua-mensal.html?=&t=microdados
pnadc2022_off <- read_pnadc("PNADC_012022.txt","input_PNADC_trimestral.txt")
head(pnadc2022_off)

#Labels
pnadc2022_lables <- pnadc_labeller(pnadc2022_off,"dicionario_PNADC_microdados_trimestral.xls")
class(pnadc2022_lables)

#Definindo o design
pnadc2022_design <- pnadc_design(pnadc2022_lables)
class(pnadc2022_design)

#Adicionando coluna de 1's
pnadc2022_off$one <- 1

#Contar o numero de pessoas
sum(pnadc2022_off$one)

#Carregando alguns pacotes
options(survey.lonely.psu = "adjust")
options(OutDec = ",")
#install.packages("survey")
library(survey)
library(srvyr)

#Estimando os dados para uma estrutura de plano amostral complexo
pnadc2022_complex <- svydesign(ids = ~UPA, 
                               strata = ~Estrato, 
                               weights = ~V1027, 
                               data = pnadc2022_off, 
                               nest = T)
class(pnadc2022_complex)

#Calibrando a pnadc com os pesos de ponderacao para obter
#estimativas populacionais
pnadc2022_pos <- data.frame(
  posest = unique(pnadc2022_off$posest),
  freq <- unique(pnadc2022_off$V1029))

#Calibrando os pesos
pnadc2022_calib <- postStratify(pnadc2022_complex,~posest, pnadc2022_pos)

#Obtendo os fatores de calibracao na amostra
pnadc2022_fatores <- weights(pnadc2022_calib)/weights(pnadc2022_complex)

#Grafico da dispersao dos fatores
boxplot(pnadc2022_fatores, horizontal = T)

#salvando a pnadc em rds
saveRDS(pnadc2022_calib, "pnadc2022.rds")

#Estimativas da populacao
svytotal(~one,pnadc2022_calib)

#Lendo minha pnadc
pnadc2022_calib <- readRDS("pnadc2022.rds")

#Rendimento medio habitual do trabalhador
round(svymean(~VD4016, 
              subset(pnadc2022_complex, 
              V2009 >= 14),
              na.rm = T), 2)

#Rendimento medio habitual do trabalhador - efeito do design
round(svymean(~VD4016, 
              subset(pnadc2022_calib, 
                     V2009 >= 14),
              na.rm = T, deff = "replace"), 2)

#Estimando estatisticas de interesse
# Prepara variaveis para calculo de estimativas
pnadc2022_subset <-  update(pnadc2022_calib,
                 idade5 = factor( 1 + findInterval( V2009 , seq( 5 , 60 ,5))),
                 nivel_renda = factor( 1 + findInterval( VD4020 , seq( 5 , 60 , 5 ))),
                 sexo = as.numeric( V2007 == 1 ) ,
                 pia = as.numeric( V2009 >= 14 ) ,
                 analfabeto = 1*(V3001==2),
                 ocupado = ifelse( pia == 1 , as.numeric( VD4002 %in% 1 ) ,NA),
                 desocup30 = ifelse( pia == 1 , as.numeric( VD4002 %in% 2 ) ,NA),
                 pea_c = as.numeric( ocupado == 1 | desocup30 == 1 ),
                 # (rendimento habitual do trabalho principal)
                 VD4016n = ifelse( pia %in% 1 & VD4015 %in% 1 , VD4016 , NA ) ,
                 # (rendimento efetivo do todos os trabalhos) 
                 VD4020n = ifelse( pia %in% 1 & VD4015 %in% 1 , VD4020 , NA ) ,
                 #indicador de nivel superior
                 VD3001n = 1*( VD3004 == 7)
)

#Renda total
totalrenda <- svytotal(~VD4020, pnadc2022_calib, na.rm = T)

#intervalos de confiqnca
confint(totalrenda, level = .99)

#Estimativas para variaveis categoricas
totalsexo <- svytotal(~V2007, pnadc2022_calib, na.rm = T)
totalsexoraca <- svytotal(~V2007+V2010, pnadc2022_calib, na.rm = T)

#Estimativas para interacao entre variaveis categoricas
totalsexoEraca <- svytotal(~interaction(V2007,V2010), pnadc2022_calib,
                           na.rm = T)
#Estimando medias
mediarenda <- svymean(~VD4020, pnadc2022_calib, na.rm = T)

#Coeficiente de variacao
cv(mediarenda)

#Intervalo de confianca
confint(mediarenda)

#Estimando proporcoes
popsexo <- svymean(~V2007,pnadc2022_calib,na.rm = T)

#com mais de uma variavel
propsexoraca <- svymean(~V2007+V2010, 
                        pnadc2022_calib, 
                        na.rm = T)

#Cruzamento de variaveis
propsexoEraca <- svymean(~interaction(V2007,V2010), 
                         pnadc2022_calib,
                         na.rm = T)

#Taxa de desocupacao na pnadc
txdesocupados <- svyratio(~VD4002 == 2,
                          ~VD4001 == 1, 
                          pnadc2022_calib,
                          na.rm = T)
txdesocupados

#Calculos de coeficiente de variacao e intervalos de confianca
cv(txdesocupados)
confint(txdesocupados)

#Medidas de percentis 
medianarenda <- svyquantile(~VD4020, pnadc2022_calib, 
                            quantiles = .5, na.rm = T)
medianarenda

#estimando o erro padrao do coeficiente de variacao
SE(medianarenda)
cv(medianarenda)

#varios quantis
quantisrenda <- svyquantile(~VD4020, pnadc2022_calib, 
                            quantiles = c(.1,.25,.5,.75,.9),
                            na.rm = T)
quantisrenda

#Estimancao para um dominio
mediarendaM <- svymean(~VD4020, 
                       subset(pnadc2022_calib, 
                              V2007 == 2), na.rm = T)
mediarendaH <- svymean(~VD4020, 
                       subset(pnadc2022_calib, 
                              V2007 == 1), na.rm = T)
mediarendaH

#Estimacao no dominio da taxa de desocupados
txdesocupados <- svyratio(~VD4002 == 2,
                          ~VD4001 == 1, 
                          subset(pnadc2022_calib, V2009 <= 25),
                          na.rm = T)
#multiplas categorias - nivel de instrucao
rendaH30raca <- svymean(~VD3005, subset(pnadc2022_calib,
                                        V2007 == 1 &
                                          V2010 == 4 &
                                          V2009 > 30),
                        na.rm = T)

#Estimacao em varios dominios
freqsexoinstr <- svyby(~V2007, ~VD3005, pnadc2022_calib,
                       svymean, na.rm = T)

#Curzamento de variaveis categoricas erro:argument "design" is missing, with no default
txdesocupadosSexoRaca <- svyby(~VD4002 == 2,
                               ~interaction(V2007,V2010),
                               denominatior = ~VD4001 == 1,
                               pnadc2022_calib,
                               svyratio,
                               na.rm = T,
                               vartype = "cv")

#Histogramas - com densidade
svyhist(~as.numeric(VD4035), 
        pnadc2022_calib, 
        main = "Histograma", 
        xlab = "Número de Horas Trabalhadas")

#Histogramas - com frequencia
svyhist(~as.numeric(VD4035), 
        pnadc2022_calib,
        freq = T,
        main = "Histograma", 
        xlab = "Número de Horas Trabalhadas")

#Boxplot
#unica categoria
svyboxplot(VD4035~1, pnadc2022_calib,
           main = "Boxplot do Número de Horas Trabalhadas",
           all.outliers = T)

#por grupos
svyboxplot(VD4035~V2007, pnadc2022_calib,
           main = "Boxplot do Número de Horas Trabalhadas",
           all.outliers = T)

#Grafico de dispersao
svyplot(VD4020 ~VD4035,
        pnadc2022_calib, style = "bubble",
        xlab = "Horas efetivamente trabalhadas",
        ylab = "Rendimento efetivo")

svyplot(VD4020 ~VD4035,
        pnadc2022_calib, style = "transparent",
        xlab = "Horas efetivamente trabalhadas",
        ylab = "Rendimento efetivo")

#Capitulo 1 Adams 2020
#Estimando efeito causal - uma simulacao
#Criando um banco de dados
set.seed(123)

#Linhas de 1 a 100
N <- 100

#y = a + bx + v supondo que a=2 e b=3 sejam os parametros da populacao
a <- 2
b <- 3

#Amostra aleatoria dos valores de x
x <- runif(N)

#Vetor aleatorio do termo de erro v
v <- rnorm(N)

#Criando o vetor y
y <- a + b*x + v

#Comparando as medias de y e x
mean(y[x>0.95]) - mean(y[x<0.05])

#relacao entre y e x
plot(x,y)
abline(a=2,b=3) #adicionando uma reta

#Posso estimar b com base na media
b_hat <- (mean(y)-2)/mean(x)

#Matriz com meus dados
mydata <- cbind(y,x,v)

#abordagem matricial do modelo OLS
x1 = x[1:5]
X1 = cbind(1,x1)

#Estimando os valores de y usando o modelo
X1%*%c(2,3)

#comparando com os valores reais de y
y[1:5]

#Incluindo os valores de v
X1%*%c(2,3) + v[1:5]

#Comparando novamente com os valores de y
y[1:5]

#Defina a matriz coluna X
X <- cbind(1,x)

#Deina uma matriz 3x2 1:6
A <- matrix(c(1:6),nrow = 3)

#A transposta de A
t(A)

#multiplicacao pela transposta
t(A)%*%A

#Como ficaria o caso de X
t(X)%*%X

#usando a funcao solve
solve(t(X)%*%X)

#Obtendo a matriz de betas (B1 e B2)
beta_hat <- solve(t(X)%*%X)%*%t(X)%*%y

#Obtendo os mesmos valores para um N=1000
N <- 1000

#Estimando em termos dos valores não observados de v medio
solve(t(X)%*%X)%*%t(X)%*%v

#Estimando o modelo OLS algebricamente
optimize(function(b) sum((y-2-b*x)^2), c(-10,10))$minimum

#De outro modo
(mean(x*y)-2*mean(x))/mean(x*x)

#A funcao lm() - OLS
data1 <- as_tibble(cbind(y,x))
lm1 <- lm(y ~ x, data = data1)

#Olhando o objeto lm1
length(lm1)
lm1$residuals
summary(lm1)

#Incluindo valores estimados no banco de dados
data1 <- as_tibble(cbind(data1,lm1$fitted.values))

#Um comparativo entre as estimativas
t(beta_hat)
lm1$coefficients

#Simulacao de 1000 vezes cada amostra de 1000 individuos da população
set.seed(123)

#repeticoes
K <- 1000

#Matrix com 1000x2
sim_res <- matrix(NA,K,2)

#Obtendo os valores da matriz sim_res
for (k in 1:K) {
  x <- runif(N)
  v <- rnorm(N)
  y <- a + b*x + v
  sim_res[k,] <- lm(y ~ x)$coefficients
}

#Definindo nomes das colunas
colnames(sim_res) <- c("Est. of a", "Est. of b")

#Instalando o pacote xtable
#install.packages("xtable")
library(xtable)

sum_tab <- summary(sim_res)

#Produzindo matrizes para o Latex
#Obtendo uma sintese da sim_res
print(xtable(sum_tab), floating = F)

#Obtendo pseudo-amostras para estimar incerteza
set.seed(123)
K <- 1000
bs_mat <- matrix(NA,K,2)

for (k in 1:K) {
  index_k <- round(runif(N,min = 1, max = N))
  data_k <- data1[index_k,]
  bs_mat[k,] <- lm(y ~ x, data = data_k)$coefficients
}

#criando uma matriz para guardar a media e o desvio padrao
tab_res <- matrix(NA,2,2)
tab_res[,1] <- colMeans(bs_mat)
tab_res[,2] <- apply(bs_mat,2,sd)

#Armezenando dados dos quantis em z temporariamente
z <- matrix(NA,2,2)
z[1,] <- quantile(bs_mat[,1],c(0.025, 0.975))
z[2,] <- quantile(bs_mat[,2],c(0.025,0.975))

#combinar z e tab_res
tab_res <- cbind(tab_res,z)

#removendo z
rm(z)

#Nomeando as colunas
colnames(tab_res) <- c("Mean","SD", "2,5%", "97,5%")

#Nomeando as linhas
rownames(tab_res) <- c("Est. of a", "Est. of b")

#Gerando saida do Latex
print(xtable(tab_res))

#Retorno de escolaridade
#install.packages("readxl")
#Lendo o arquivo nls.xlsx com readxl
library(readxl)
x <- read_xlsx("nls.xlsx")

#Verificando a classe das variaveis
x$wage76 <- as.numeric(x$wage76)
x$lwage76 <- as.numeric(x$lwage76)

#Excluindo as linhas que contem NA
x1 <- x[is.na(x$lwage76)==0,]

#Regredindo o salario na escolaridade
#renda_i = a + beducacao_i + v_nobservavel
lm1 <- lm(lwage76 ~ ed76,data = x1)

#Obtendo um grafico simples
plot(x1$ed76, x1$lwage76, xlab = "Years of Eduction", ylab = "Log Wages (1976)")
abline(a=lm1$coefficients[1],b=lm1$coefficients[2])

#Capitulo 2 Regressao multipla
#Definindo alguns parametros importantes
set.seed(123456)
N <- 1000
a <- 2
b <- 3
c <- 4
u_x <- rnorm(N)

#supondo alpha=0
alpha <- 0
x <- x1 <- (1-alpha)*runif(N) + alpha*u_x
w <- w1 <- (1-alpha)*runif(N) + alpha*u_x
u <- rnorm(N)
y <- a + b*x + c*w + u
lm1 <- lm(y ~ x)
lm2 <- lm(y ~ x + w)

#supondo alpha=.5
alpha <- 0.5
x <- x2 <- (1-alpha)*runif(N) + alpha*u_x 
w <- w2 <- (1-alpha)*runif(N) + alpha*u_x
lm3 <- lm(y ~ x)
lm4 <- lm(y ~ x + w)

#supondo alpha=.95
x <- x3 <- (1-alpha)*runif(N) + alpha*u_x 
w <- w3 <- (1-alpha)*runif(N) + alpha*u_x
lm5 <- lm(y ~ x)
lm6 <- lm(y ~ x + w)

#Usando stargazer para produzir tabelas do Latex
#install.packages("stargazer")
library(stargazer)
stargazer(list(lm1,lm2,lm3,lm4,lm5,lm6), 
          keep.stat = c("n","rsq"),
          float = F,
          font.size = "small",
          digits = 2,
          keep = c(1:6))

#Correlacao entre x1 e w1 (x2 e w2)
cov(x1,w1) #alpha=0
cov(x2,w2) #alpha=0.5
cov(x3,w3) #alpha=.95

#De outro modo
t(x3)%*%w3

#Multicolinearidade no r
X2 <- cbind(1,x3,w3)

solve(t(X2)%*%X2)%*%t(X2)%*%u

mean(u)

#covariancia entre duas variaveis
cov(x3,w3)
cov(x3,u)

#matricialmente
(1/(N))*t(X2)%*%u

#Usando o exemplo do Card 1993
#renda_i = a + beducacao_i + cExperiencia + dExperiencia² + ... + v
#Obtendo a variavel experiencia
x1 <- x
x1$exp <- x1$age76 - x1$ed76 - 6
x1$expsq <- (x1$exp^2/100)

#regredindo educacao em varias variaveis
lm1 <- lm(lwage76 ~ ed76, data = x1)
lm2 <- lm(lwage76 ~ ed76 + exp + expsq, data = x1)
lm3 <- lm(lwage76 ~ ed76 + exp + expsq + black + reg76r, data = x1)
lm4 <- lm(lwage76 ~ ed76 + exp +expsq + black +reg76r+
            smsa76r + smsa66r + reg662 + reg663 + reg664 +
            reg665 + reg666 + reg667 + reg668 + reg669, data=x1)

summary(lm4)

#Obtendo a tabela com as saidas dos modelos
library(stargazer)
stargazer(list(lm1,lm2,lm3,lm4),
          keep.stat = c("n", "rsq"),
          float = T,
          font.size = "small",
          digits = 2)

#Simulacao para verificar efeito indireto
N <- 50
a <- 1
b <- 0
c <- 3
d <- 4
x <- round(runif(N))
u_w <- runif(N)
w <- d*x + u_w
u <- rnorm(N)
y <- a + b*x + c*w + u

#Regressoes
shortreg <- lm(y ~ x)
longreg <- lm(y ~ x + w)
summary(longreg)

#Obtendo manualmente os coeficiente de cada relacao
e_hat <- shortreg$coefficients[2] 
c_hat <- lm(y ~ w)$coef[2]
d_hat <- lm(w ~ x)$coef[2]

#A estimativa de b
e_hat - c_hat*d_hat

#Regressao multipla com dual path estimator
set.seed(123456)
b_mat <- matrix(NA,100,3)
for (i in 1:100) {
  x <- rnorm(runif(N))
  u_w <- runif(N)
  w <- d*x + u_w
  u <- rnorm(N)
  y <- a + b*x + c*w + u
  lm2_temp <- summary(lm(y ~ x + w))
  b_mat[i,1] <- lm2_temp[[4]][2]
  b_mat[i,2] <- lm2_temp[[4]] [8]
  e_hat <- lm(y ~ x)$coef[2]
  d_hat <- lm(w ~ x)$coef[2]
  c_hat <- lm(y ~ w)$coef[2]
  b_mat[i,3] <- e_hat - c_hat*d_hat
}

#renomeando colunas
colnames(b_mat) <- c("Standard Est", "T-stat os standard", "Proposed Est")
summ_tab <- summary(b_mat)

#Produzindo uma tabela
library(xtable)
print(xtable(summ_tab), floating = F)

#Histograma 
hist(b_mat[,1], freq = F, xlab = "Estimate of b", main = "")
lines(density(b_mat[,1]), type = "l", lwd =3)
abline(v=c(min(b_mat[,3]), max(b_mat[,3])), lty = 2, lwd = 3)

#Forma matricial do efeito indireto de X
X <- cbind(1,x)
W <- cbind(1,w)

beta_tilde_hat <- solve(t(X)%*%X)%*%t(X)%*%y
delta_hat <- solve(t(X)%*%X)%*%t(X)%*%W
gamma_hat <- solve(t(W)%*%W)%*%t(W)%*%y

beta_hat <- beta_tilde_hat - delta_hat%*%gamma_hat

#O caso de emprestimos bancarios
library(readxl)
x <- read_xlsx("hmda_aer.xlsx")
x$deny <- ifelse(x$s7==3,1,ifelse(x$s7==1 | x$s7==2,0,NA))
x$black <- as.numeric(x$s13==3)

#Salarios
x$lwage <- NA
x$wage <- ifelse(x$s31a == 0, NA, x$s31a)
x$lwage <- log(x$wage)

#Pefil de pagamento no sistema bancario
x$mhist <- x$s42

#Perfil pagamento do consumidor
x$chist <- x$s43

#Pefil de pagamento de emprestimos publico
x$phist <- x$s44

#Tempo empregado
x$emp <- x$s25a
x$emp <- ifelse(x$emp > 1000,NA,x$emp)

#Definindo as matrizes
Y1 <- x$deny
X1 <- cbind(1,x$black)
W1 <- cbind(1, x$lwage, x$chist, 
            x$mhist, x$phist, x$emp)

#Excluindo as linhas com NAs
index <- is.na(rowSums(cbind(Y1,X1,W1)))==0

#Redefinindo as matrizes sem NAs
X2 <- X1[index,]
W2 <- W1[index,]
Y2 <- Y1[index]

#Estimando as matrizes dos coeficientes
beta_tilde_hat <- solve(t(X2)%*%X2)%*%t(X2)%*%Y2
delta_hat <- solve(t(X2)%*%X2)%*%t(X2)%*%W2
gamma_hat <- solve(t(W2)%*%W2)%*%t(W2)%*%Y2
beta_hat <- beta_tilde_hat - delta_hat%*%gamma_hat

#Incluindo mais variaveis
x$married <- as.numeric(x$s23a == "M")

#Participacao das despesas na renda
x$dr <- ifelse(x$s45 > 999999, NA, x$s45)

#Linhas de credito
x$clines <- ifelse(x$s41 > 999999, NA, x$s41)

#Sexo
x$male <- as.numeric(x$s15 == 1)

#(S11) County
x$suff <- ifelse(x$s11 > 999999, NA, x$s11)

#(S35) Liquid assets (in thousands)
x$assets <- ifelse(x$s35 > 999999, NA, x$s35)

#(S6) Loan amount (in thousands)
x$s6 <- ifelse(x$s6 > 999999, NA, x$s6)

#(S50) Appraised value (in thousands)
x$s50 <- ifelse(x$s50 > 999999, NA, x$s50)

#(S33) Purchase price (in thousands)
x$s33 <- ifelse(x$s33 > 999999, NA, x$s33)

#(S6) Loan amount (in thousands)
x$lr <- x$s6/x$s50
x$pr <- x$s33/x$s50

#(S16) Co-applicant sex	
x$coap <- x$s16==4
x$school <- ifelse(x$school > 999999, NA, x$school)

#Times application was reviewed by underwriter
x$s57 <- ifelse(x$s57 > 999999, NA, x$s57)

#(S48) Term of loan (months)
x$s48 <- ifelse(x$s48 > 999999, NA, x$s48)

#(S39) Number of commercial credit reports
x$s39 <- ifelse(x$s39 > 999999, NA, x$s39)

#(chvalc) Change in median value of
#property in a given tract, 1980-1990
x$chval <- ifelse(x$chval > 999999, NA, x$chval)

#(S20) Number of units in property purchased
x$s20 <- ifelse(x$s20 > 999999, NA, x$s20)
x$lwage_coap <- NA

#(S31C) Total monthly income of coapplicant ($)
x[x$s31c > 0 & x$s31c < 999999,]$lwage_coap <-
  log(x[x$s31c > 0 & x$s31c < 999999,]$s31c)
x$lwage_coap2 <- ifelse(x$coap==1,x$lwage_coap,0)
x$male_coap <- x$s16==1

#Redefinindo nosso W1
W1 <- cbind(1,x$lwage,x$chist,x$mhist,x$phist,x$emp,
      x$emp^2,x$married,x$dr,x$clines,x$male,
      x$suff,x$assets,x$lr,x$pr,x$coap,x$s20,
      x$s24a,x$s27a,x$s39,x$s48,x$s53,x$s55,x$s56,
      x$s57,x$chval,x$school,x$bd,x$mi,x$old,
      x$vr,x$uria,x$netw,x$dnotown,x$dprop,
      x$lwage_coap2,x$lr^2,x$pr^2,x$clines^2,x$rtdum)

#Eliminando NAs
index <- is.na(rowSums(cbind(Y1,X1,W1)))==0
X2 <- X1[index,]
W2 <- W1[index,]
Y2 <- Y1[index]

#Estimando as matrizes dos coeficientes
beta_tilde_hat <- solve(t(X2)%*%X2)%*%t(X2)%*%Y2
delta_hat <- solve(t(X2)%*%X2)%*%t(X2)%*%W2
gamma_hat <- solve(t(W2)%*%W2)%*%t(W2)%*%Y2
beta_hat <- beta_tilde_hat - delta_hat%*%gamma_hat

#Bootstrap
set.seed(123456)
K <- 1000
bs_mat <- matrix(NA,K,2)

for (k in 1:K) {
  index_k <- round(runif(length(Y2),min = 1, max = length(Y2)))
  Y3 <- Y2[index_k]
  X3 <- X2[index_k,]
  W3 <- W2[index_k,]
  beta_tilde_hat <- solve(t(X3)%*%X3)%*%t(X3)%*%Y3
  delta_hat <- solve(t(X3)%*%X3)%*%t(X3)%*%W3
  gamma_hat <- solve(t(W3)%*%W3)%*%t(W3)%*%Y3
  bs_mat[k,] <- beta_tilde_hat - delta_hat%*%gamma_hat
}

#Inserindo valores na tabela
tab_res <- matrix(NA,2,4)
tab_res[,1] <- colMeans(bs_mat)
tab_res[,2] <- apply(bs_mat,2,sd)
tab_res[1,3:4] <- quantile(bs_mat[,1], c(0.025,0.975))
tab_res[2,3:4] <- quantile(bs_mat[,2], c(0.025,0.975))
colnames(tab_res) <- c("Estimate",'SD', "2.5%","97.5%")
tab_res

#Capitulo 3 - Variaveis instrumentais
set.seed(123456)
N <- 1000
a <- 2
b <- 3
c <- 2
e <- 3
f <- -1
d <- 4
z <- runif(N)
u_1 <- rnorm(N,mean = 0, sd=3)
u_2 <- rnorm(N, mean = 0, sd= 1)
x <- f + d*z + u_2 + c*u_1
y <- a + b*x + e*u_1
lm1 <- lm(y ~ x)

bd_hat <- lm(y ~ z)$coef[2]
d_hat <- lm(x ~ z)$coef[2]

#Verificando a razao entre eles
bd_hat/d_hat

#Matricialmente
X <- cbind(1,x)
Z <- cbind(1,z)
beta_hat_ols <- solve(t(X)%*%X)%*%t(X)%*%y
beta_hat_iv <- solve(t(Z)%*%X)%*%t(Z)%*%y

#Bootstrap IV
lm_iv <- function(y, X_in, Z_in=X_in, Reps = 100, 
                  min_in = 0.05, max_in = 0.95) {
  set.seed(123456)
  X <- cbind(1, X_in)
  Z <- cbind(1,Z_in)
  
  bs_mat <- matrix(NA,Reps, dim(X)[2])
  N <- length(y)
  for (r in 1:Reps) {
    index_bs <- round(runif(N, min = 1, max = N))
    y_bs <- y[index_bs]
    X_bs <- X[index_bs,]
    Z_bs <- Z[index_bs,]
    bs_mat[r,] <- solve(t(Z_bs)%*%X_bs)%*%t(Z_bs)%*%y_bs
  }
  
  tab_res <- matrix(NA, dim(X)[2],4)
  tab_res[,1] <- colMeans(bs_mat)
  for (j in 1:dim(X)[2]) {
    tab_res[j,2] <- sd(bs_mat[,j])
    tab_res[j,3] <- quantile(bs_mat[,j],min_in)
    tab_res[j,4] <- quantile(bs_mat[,j],max_in)
  }
  colnames(tab_res) <- c("coef", "sd", 
                         as.character(min_in), as.character(max_in))
  return(tab_res)
}

print(lm_iv(y,x), digits = 3) #OLS
print(lm_iv(y,x,z), digits = 3) #IV

library(readxl)
nls <- read_xlsx("nls.xlsx")

#Lendo variaveis como numericas
nls$lwage76 <- as.numeric(nls$lwage76)
nls1 <- nls[is.na(nls$lwage76)==0,]

#Criando variaveis
nls1$exp <- nls1$age76 - nls1$ed66 - 6
nls1$exp2 <- (nls1$exp^2)/100

#Estimativa OLS
lm4 <- lm(lwage76 ~ ed76 + exp + exp2 + black +reg76r + smsa76r + smsa66r
          + reg662 + reg663 + reg664 + reg665 + reg666 + reg667 + reg668
          + reg669, data = nls1)
summary(lm4)

#Tratando a estimativa na relacao nearc4 e lwage
lm5 <- lm(lwage76 ~ nearc4 + exp + exp2 + black +reg76r + smsa76r + smsa66r
                 + reg662 + reg663 + reg664 + reg665 + reg666 + reg667 + reg668
                 + reg669, data = nls1)
lm4$coefficients[2]
lm5$coefficients[2]

#Efeito de nearc4 em ed76
lm6 <- lm(ed76 ~ nearc4 + exp + exp2 + black +reg76r + smsa76r + smsa66r
          + reg662 + reg663 + reg664 + reg665 + reg666 + reg667 + reg668
          + reg669, data = nls1)
lm6$coefficients[2]

summary(lm5)

#O estimador IV
lm5$coefficients[2]/lm6$coefficients[2]

#Usando matrizes....
y <- nls1$lwage76
X <- cbind(nls1$ed76, nls1$exp, nls1$exp2, nls1$black, nls1$reg76r,
           nls1$smsa76r, nls1$smsa66r, nls1$reg662, nls1$reg663,
           nls1$reg664, nls1$reg665, nls1$reg666, nls1$reg667,
           nls1$reg668, nls1$reg669)
nls1$age2 <- nls1$age76^2
Z1 <- cbind(nls1$nearc4, nls1$age76, nls1$age2, nls1$black,
            nls1$reg76r,nls1$smsa76r, nls1$smsa66r, nls1$reg662,
            nls1$reg663,nls1$reg664, nls1$reg665, nls1$reg666,
            nls1$reg667, nls1$reg668, nls1$reg669)

res <- lm_iv(y,X, Reps=1000)
res <- lm_iv(y,X,Z1, Reps=1000)

rownames(res) <- c("intercept","ed76","exp", "exp2",
                   "black", "reg76r","smsa76r", "smsa66r",
                   "reg662","reg663","reg664", "reg665",
                   "reg666","reg667", "reg668", "reg669")

#Comparando medias para domicilios proximos ou não da escola
tab_cols <- c("Near College", "Not Near College")
tab_rows <- c("ed76", "exp", "black", "south66", 
              "smsa66r", "reg76r", "smsa76r")
table_dist <- matrix(NA,7,2)

#Obter os valores medios
for (i in 1:7) {
  table_dist[i,1] <-
    sum(nls1[nls1$nearc4 == 1, colnames(nls1)==tab_rows[i]])/count(nls1[nls1$nearc4 == 1, colnames(nls1)==tab_rows[i]])
  table_dist[i,2] <-
    sum(nls1[nls1$nearc4 == 0, colnames(nls1)==tab_rows[i]])/count(nls1[nls1$nearc4 == 0, colnames(nls1)==tab_rows[i]])
}

#Teste de identificacao
Z2 <- cbind(nls1$momdad14, nls1$age76, nls1$age2, nls1$black,
            nls1$reg76r, nls1$smsa76r, nls1$smsa66r, nls1$reg662,
            nls1$reg663, nls1$reg664, nls1$reg665, nls1$reg666,
            nls1$reg667, nls1$reg668, nls1$reg669)

#Bootstrap
set.seed(123456)
bs_diff <- matrix(NA,1000,1)
N <- length(y)

for (i in 1:1000) {
  index_bs <- round(runif(N,min = 1,max = N))
  y_bs <- y[index_bs]
  X_bs <- X[index_bs,]
  Z1_bs <- Z1[index_bs,]
  Z2_bs <- Z2[index_bs,]
  bs_diff[i] <- (solve(t(Z1_bs)%*%X_bs)%*%t(Z1_bs)%*%y_bs)[2,1] - 
                   (solve(t(Z2_bs)%*%X_bs)%*%t(Z2_bs)%*%y_bs)[2,1]
}

summary(bs_diff)
quantile(bs_diff,c(0.05, 0.95))

#Estimando LATE
X2 <- X[,1] > 12

#Obtendo os valores de y condicionais em z
mu_y1 <- mean(y[Z1[,1]==1])
mu_y0 <- mean(y[Z1[,1]==0])

#Probabilidades
p_11 <- mean(X2[Z1[,1]==1])
p_10 <- mean(X2[Z1[,1]==0])

#LATE
((mu_y1-mu_y0)/(p_11-p_10))/4

#Estimativa OLS
mu_y1 <- mean(y[Z2[,1]==1])
mu_y0 <- mean(y[Z2[,1]==0])
p_11 <- mean(X2[Z2[,1]==1])
p_10 <- mean(X2[Z2[,1]==0])
((mu_y1-mu_y0)/(p_11-p_10))/4

#Capitulo 4
set.seed(123456789)
N <- 200
a <- 2
b <- rnorm(N,mean = 2, sd =3)
x0 <- rep(0,N)
x1 <- rep(1,N)
u <- rnorm(N)
y <- a + b*cbind(x0,x1) + u

#Distribuicao do ATE
par(mfrow = c(2,1))
#par(mar=c(2,4,0.5,0.5))
plot(density(y[,1]), lwd = 3, xlim(range(y)),
     ylab = "density", main = "")
lines(density(y[,2]),lwd = 1)
abline(v=colMeans(y), lwd = c(3,1))
legend("topright", c("No College", "College"), lwd = c(3,1))

#Densidade do efeito do tratamento entre os individuos
plot(ecdf(y[,1]), xlim=range(y), main = "", 
     do.points = F, lwd = 3, xlab = "y")
lines(ecdf(y[,2]), lwd= 2, do.points = F)

#Diferença de medias entre os dois grupos
mean(y[,2]-y[,1]) ==  mean(y[,2]) - mean(y[,1])

#Resultados observados
X <- runif(N) < 0.3
Y <- (1-X)*y[,1] + X*y[,2]

#ATE estimado com base na amostra
mean(Y[X==1])-mean(Y[X==0])

#Criando os limites superior e inferior de Kolmogorov
FL <- function(b,y1,y0){
  f < function(x) -(mean(y1<x) -mean(y0<x-b))
  a <- optimize(f, c(min(y1,y0), max(y1,y0)))
  return(max(-a$objetive,0))
}

FL <- function(b,y1,y0){
  f < function(x) (mean(y1<x) -mean(y0<x-b))
  a <- optimize(f, c(min(y1,y0), max(y1,y0)))
  return(min(a$objetive,0))
}

K <- 50
min_diff <- min(y[,1]) - max(y[,2])
max_diff <- max(y[,1] - min(y[,2]))

delta_diff <- (max_diff - min_diff)/K

y_K <- min_diff + c(1:K)*delta_diff



save.image("meuworkspace.RData")



































































