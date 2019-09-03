#title: "Análise de Dados - UFPE/2019 - Trabalho final"
#parte 1: "Tratamento dos dados"
#author: "Marcelle Amaral"
#date: "02/09/2019"


#definindo diretório
setwd("C:/Users/celle/Documents/R/marcelle-amaral-ad-ufpe-2019/marcelle-amaral-ad-ufpe-2019.R")

getwd() # checando

# carregando funções

library(foreign)
library(tidyverse)
library(pracma)
library(tidyr)

# usando o dataset do banco de Helen V. Milner

# disponível em: https://scholar.princeton.edu/hvmilner/publications/replication-data-foreign-direct-investment-and-institutional-diversity-trade

# Data Citation: 
# Büthe, Tim and Helen V. Milner. 2014. "Replication data for: Foreign Direct Investment and Institutional Diversity in Trade Agreements", 
# V1, http://scholar.princeton.edu/hvmilner/data. 

# abrindo 

dt <- read.dta("marcelle-amaral-ad-ufpe-2019-bdoriginal.dta")

head(dt) # primeiras 6 linhas de uma matriz
names(dt) # nomes das variáveis

# Corrigindo a diferença de nomes para um mesmo país

# corrigindo Bolivia

dt$ctylabel<- ifelse(dt$ctylabel=="Bolivia (Plurinational State of)","Bolivia", dt$ctylabel)

# corrigindo Venezuela

dt$ctylabel <- ifelse(dt$ctylabel=="Venezuela (Bolivarian Republic of)","Venezuela", dt$ctylabel)

# corrigindo Costa Rica

dt$ctylabel <- ifelse(grepl("Costa Rica", dt$ctylabel) == T, "CostaRica", dt$ctylabel)

# corrigindo El Salvador

dt$ctylabel <- ifelse(grepl("El Salvador", dt$ctylabel) == T, "ElSalvador", dt$ctylabel)

# corrigindo Republica Dominica

dt$ctylabel <- ifelse(grepl("Dominican Republic", dt$ctylabel) == T, "DominicanRepublic", dt$ctylabel)


# selecionando e salvando as variáveis de interesse para os 20 países de interesse:
# AMÉRICA LATINA :

table(dt$ctylabel) # identificando nome dos países

# primeiro para a Argentina
# criando uma variável para o resto dos países
latin <- c("Bolivia","Brazil","Chile","Colombia","Ecuador","Paraguay","Peru",
           "Uruguay", "Venezuela", "CostaRica", "ElSalvador", "Guatemala", "Honduras",
           "Mexico", "Nicaragua", "Panama", "Haiti", "DominicanRepublic", "Cuba")

# selecionando as variáveis de interesse do banco original em uma variável
vars.sel <- c("country","ctylabel","date","gdp_pc_95d_old","fdi_inflow_unctad_gdp",
              "fdi_inflow_unctad_gdp_2008","gdp_gr","fdi_inflow_unctad_world_old",
              "fdi_inflow_unctad_gdp_old","lag_gattwto","lag_bits_cuml_restricted",
              "lag_ln_pop","lag_gdp_gr","lag_polconiii_2002","lag_polinstability",
              "lag_trade_pgdp","fdi_inflow_unctad_usd_2008","fdi_inflow_unctad_world_2008",
              "fdi_inflow_unctad_usd","fdi_inflow_unctad_world","pop_gr","urb_pop",
              "trade_pgdp","polconiii_2010","polconv_2010","pta_sign_old","pta_force_old",
              "polinstability","gatt_wto","bits_new",
              "bits_new_force","bits_new_gdpw","bits_new_lgdpw","bits_new_rel_gdpw",
              "bits_new_rel_lgdpw","bits_new_gdpw_force","bits_new_lgdpw_force",
              "bits_new_rel_gdpw_force","bits_new_rel_lgdpw_force","bits_cuml","bits_cuml_restricted",
              "bits_cumlr_gdpw","bits_cumlr_lgdpw","bits_cumlr_rel_gdpw","bits_cumlr_rel_lgdpw",
              "trade_pgdp_old","gatt_old","gatt_wto_old","wto_old","gattformal",
              "nohs_PTAsigned","nohs_PTAforce","pta_sign","pta_force","ln_pop",
              "ln_gdp_pc_00d","ptas_signedonly","ptas_signedonlypos","bits_signedonly",
              "lag_polity2","lag_freedom","lag_pta_sign","lag_ptas_signedonly",
              "lag_pta_force","lag_gatt","lag_bits_signedonly","lag_bits_cumlr_force",
              "lag_ln_gdp_pc_00d","polity2pos","lag_polity2pos",
              "lag_polconiii_2010","lag_wto")

# selecionando as variáveis que sofrerão detrend linear
vars.det <- c("fdi_inflow_unctad_gdp_2008","fdi_inflow_unctad_gdp_old",
              "lag_ln_pop", "lag_ln_gdp_pc_00d", "lag_gdp_gr",
              "lag_bits_cuml_restricted", "lag_polconiii_2002", 
              "lag_polconiii_2010", "lag_polinstability", "lag_gattwto",
              "fdi_inflow_unctad_gdp", "lag_pta_force")


# primeiro para a Argentina como teste

dttotal1 <- subset(dt, ctylabel == "Argentina") # selecionando o país por ctylabel
dttotal <- dttotal1 %>% select(vars.sel) # selecionando para o país as variáveis de interesse
dttotal <- dttotal[c(1:71)] # reordenando a numeração das novas colunas
dttotal <- subset(dttotal, date >= 1970 & date <= 2009) # selecionando o tempo para o modelo
for (var in vars.det) { # selecionando as variáveis que sofrerao detrend
  new.var <- paste0(var,"_det") # acrescentando a terminação a elas
  dttotal <- cbind(dttotal,detrend(dttotal[,var])) # juntando ao banco as novas variáveis com a nova terminação
  colnames(dttotal)[length(colnames(dttotal))] <- new.var
}

# Repetindo para o resto dos países

for (pais in latin) {
  pais.dataframe <- subset(dt, ctylabel == pais) 
  pais.sel <- pais.dataframe %>% select(vars.sel)  
  pais.sel <- pais.sel[c(1:71)]
  pais.sel <- subset(pais.sel, date >= 1970 & date <= 2009)  
  for (var in vars.det) {
    new.var <- paste0(var,"_det")
    pais.sel <- cbind(pais.sel,detrend(pais.sel[,var]))
    colnames(pais.sel)[length(colnames(pais.sel))] <- new.var
  }
  dttotal <- rbind(dttotal, pais.sel)
}

dttotal # visualizando

# resetando o número das linhas

row.names(dttotal) <- NULL

# reordendo os índices das colunas

dttotal <- dttotal[c(1:83)]

# numerando os países de 1 a 20

dttotal$country[dttotal$country == 213] <- 1
dttotal$country[dttotal$country == 218] <- 2
dttotal$country[dttotal$country == 223] <- 3
dttotal$country[dttotal$country == 228] <- 4
dttotal$country[dttotal$country == 233] <- 5
dttotal$country[dttotal$country == 248] <- 6
dttotal$country[dttotal$country == 288] <- 7
dttotal$country[dttotal$country == 293] <- 8
dttotal$country[dttotal$country == 298] <- 9
dttotal$country[dttotal$country == 299] <- 10
dttotal$country[dttotal$country == 238] <- 11
dttotal$country[dttotal$country == 253] <- 12
dttotal$country[dttotal$country == 258] <- 13
dttotal$country[dttotal$country == 268] <- 14
dttotal$country[dttotal$country == 273] <- 15
dttotal$country[dttotal$country == 278] <- 16
dttotal$country[dttotal$country == 283] <- 17
dttotal$country[dttotal$country == 263] <- 18
dttotal$country[dttotal$country == 243] <- 19
dttotal$country[dttotal$country == 928] <- 20

dttotal # visualizando

# salvando arquivo em RData
save(dttotal, file = "marcelle-amaral-ad-ufpe-2019-bdtratado.RData")

load("marcelle-amaral-ad-ufpe-2019-bdtratado.RData")



