#title: "An�lise de Dados - UFPE/2019 - Trabalho final"
#parte 2: "Conhecendo os dados"
#author: "Marcelle Amaral"
#date: "02/09/2019"

# set wd at file path
current_path <- rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path))
print(getwd())

# carregando o banco

load("marcelle-amaral-ad-ufpe-2019-bdtratado.RData")

# carregando fun��es

require("pastecs")
require("ggpubr")

# conhecendo os dados:

## an�lise explorat�ria

## vari�vel dependente

summary(dttotal$fdi_inflow_unctad_gdp)

stat.desc(dttotal$fdi_inflow_unctad_gdp)

hist(dttotal$fdi_inflow_unctad_gdp,
     main ="Vari�vel dependente", xlab = "IED em % do PIB", ylab = "Frequ�ncia")

## variaveis independentes quantitativas do modelo

# para pta_force

sum(dttotal$pta_force)
sum(dttotal$ptas_signedonly)
sum(dttotal$pta_force_old)
sum(dttotal$pta_sign)

summary(dttotal$pta_force)
sd(dttotal$pta_force)

print(dttotal$pta_force)

hist(dttotal$pta_force,
     main ="PTAs em vigor", xlab = "Quantidades", ylab = "Frequ�ncia")

# QQ-plot - correla��o entre a amostra da vari�vel
# e a distribui��o normal

ggqqplot(dttotal$pta_force, ylab = "PTAs em vigor")

# relacao da vari�vel dependente com independente quantitativa 

ggscatter(dttotal, x = "pta_force", y = "fdi_inflow_unctad_gdp", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "PTAs em vigor", ylab = "IED inflow")

# para bits_cuml_restricted

summary(dttotal$bits_cuml_restricted)
sd(dttotal$bits_cuml_restricted)
stat.desc(dttotal$bits_cuml_restricted)

print(dttotal$bits_cuml_restricted)

sum(dttotal$bits_cuml)
sum(dttotal$bits_cuml_restricted)

hist(dttotal$bits_cuml_restricted,
     main ="BITs cumulativos restritivos", xlab = "Quantidade", ylab = "Frequ�ncia")

# QQ-plot - correla��o entre a amostra da vari�vel
# e a distribui��o normal

ggqqplot(dttotal$bits_cuml_restricted, ylab = "BITs cumul. rest.")

# relacao da vari�vel dependente com independente quantitativa 

ggscatter(dttotal, x = "bits_cuml_restricted", y = "fdi_inflow_unctad_gdp", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "BITs cuml. rest.", ylab = "IED inflow")

# para polconiii_2010

summary(dttotal$polconiii_2010)
stat.desc(dttotal$polconiii_2010)

hist(dttotal$polconiii_2010,
     main ="Constri��es Dom�sticas Pol�ticas - POLCON", xlab = "N�vel do �ndice", ylab = "Frequ�ncia")

# QQ-plot - correla��o entre a amostra da vari�vel
# e a distribui��o normal

ggqqplot(dttotal$polconiii_2010, ylab = "Constri��es Pol�ticas")

# relacao da vari�vel dependente com independente quantitativa 

ggscatter(dttotal, x = "polconiii_2010", y = "fdi_inflow_unctad_gdp", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Constri��es Pol�ticas", ylab = "IED inflow")

# para polinstability

summary(dttotal$polinstability)
stat.desc(dttotal$polinstability)

hist(dttotal$polinstability,
     main ="Instabilidade Pol�tica", xlab = "Medi��o", ylab = "Frequ�ncia")

# QQ-plot - correla��o entre a amostra da vari�vel
# e a distribui��o normal

ggqqplot(dttotal$polinstability, ylab = "Instabilidade Pol�tica")

# relacao da vari�vel dependente com independente quantitativa 

ggscatter(dttotal, x = "polinstability", y = "fdi_inflow_unctad_gdp", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Instabilidade Pol�tica", ylab = "IED inflow")

# para ln_pop

summary(dttotal$ln_pop)
sd(dttotal$ln_pop)
stat.desc(dttotal$ln_pop)

hist(dttotal$ln_pop,
     main ="Tamanho do Mercado", xlab = "Log da popula��o", ylab = "Frequ�ncia")

# QQ-plot - correla��o entre a amostra da vari�vel
# e a distribui��o normal

ggqqplot(dttotal$ln_pop, ylab = "Tamanho do Mercado")

# relacao da vari�vel dependente com independente quantitativa 

ggscatter(dttotal, x = "ln_pop", y = "fdi_inflow_unctad_gdp", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Tamanho do Mercado", ylab = "IED inflow")

# para ln_gdp_pc_00d

summary(dttotal$ln_gdp_pc_00d)
stat.desc(dttotal$ln_gdp_pc_00d)

hist(dttotal$ln_gdp_pc_00d,
     main ="Desenvolvimento Econ�mico", xlab = "Log do PIB per capita", ylab = "Frequ�ncia")

# QQ-plot - correla��o entre a amostra da vari�vel
# e a distribui��o normal

ggqqplot(dttotal$ln_gdp_pc_00d, ylab = "Desenvolvimento Econ�mico")

# relacao da vari�vel dependente com independente quantitativa 

ggscatter(dttotal, x = "ln_gdp_pc_00d", y = "fdi_inflow_unctad_gdp", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Desenvolvimento Econ�mico", ylab = "IED inflow")

# para gdp_gr

summary(dttotal$gdp_gr)
stat.desc(dttotal$gdp_gr)

hist(dttotal$gdp_gr,
     main ="Crescimento do PIB", xlab = "Varia��o percentual do PIB", ylab = "Frequ�ncia")

# QQ-plot - correla��o entre a amostra da vari�vel
# e a distribui��o normal

ggqqplot(dttotal$gdp_gr, ylab = "Crescimento do PIB")

# relacao da vari�vel dependente com independente quantitativa 

ggscatter(dttotal, x = "gdp_gr", y = "fdi_inflow_unctad_gdp", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Crescimento do PIB", ylab = "IED inflow")

## vari�vel independente categ�rica

# para gatt_wto

table(dttotal$gatt_wto)
sum(dttotal$gatt_wto)

barplot(table(dttotal$gatt_wto), 
        main = "GATT/OMC", names.arg = c("n�o participantes", "participantes"))

# QQ-plot - correla��o entre a amostra da vari�vel
# e a distribui��o normal

ggqqplot(dttotal$gatt_wto, ylab = "GATT/OMC")

# relacao da vari�vel dependente com a independente categ�rica

boxplot(dttotal$fdi_inflow_unctad_gdp ~ dttotal$gatt_wto)

