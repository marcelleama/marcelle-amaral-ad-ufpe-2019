#title: "Análise de Dados - UFPE/2019 - Trabalho final"
#parte 3: "Gerando modelos e gráficos do artigo"
#author: "Marcelle Amaral"
#date: "02/09/2019"

# set wd at file path
current_path <- rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path))
print(getwd())

# carregando o banco

load("marcelle-amaral-ad-ufpe-2019-bdtratado.RData")

# requerindo pacotes:
require(pacman)

# conditional installing/loading
p_load(lmtest)
p_load(kinship2)
p_load(Formula)
p_load(plm)
p_load(apsrtable)
p_load(tseries)
p_load(dplyr)
p_load(car)
p_load(Metrics)
p_load(nlme)
p_load(olsrr)
p_load(dotwhisker)

# Gerando o modelo conforme o modelo 4 de Buthe/Milner

# Baseando-os em 
# Molly Roberts and Megan Westrum, 2010, "RobertsWestrumReplicationFile.R", 
# Replication for: The Politics of Foreign Direct Investment into Developing Countries, https://doi.org/10.7910/DVN/3FXS5K/UFEHTH, Harvard Dataverse, V1


# configurando função necessária

# Arellano - para garantir a homocedasticidade e ausência de autocorrelação do modelo

clx <- function(fm, time, cluster){
  library(sandwich)
  library(lmtest)
  T <- length(unique(time))
  N <- length(cluster)
  dfc <- (N)/(N - T)
  u <- apply(estfun(fm),2,
             function(x) tapply(x, cluster, sum))
  vcovCL <- dfc*sandwich(fm, meat=crossprod(u)/N)
  coeftest(fm, vcovCL)}

# Carregando os dados 

fdi <- dttotal

# Gerando modelos:

# Modelo 1 
# modelo com os dados sem detrend, ou seja, com tendência:

se3 <- lm(fdi_inflow_unctad_gdp ~ lag_pta_force + lag_gattwto + 
            lag_bits_cuml_restricted + lag_polconiii_2010 + 
            lag_polinstability + lag_ln_pop + lag_ln_gdp_pc_00d +
            lag_gdp_gr + factor(country), data = fdi)

summary(se3)

# checando o ajuste do modelo:
rmse3 <- sqrt(mean(se3$residuals^2))
rmse3

# checando os pressupostos do modelo:

# 1 - média dos resíduos igual a zero:
mean(residuals(se3))

# 2 - resíduos com distribuição normal:
hist(residuals(se3))
shapiro.test(residuals(se3))

# 3 - heterocedasticidade antes da função:
plot(se3,which = 1)
ncvTest(se3)

# 4 - multicolinearidade:
vif(se3)

# 5 - presença de outliers:
ols_plot_resid_qq(se3)
ols_plot_resid_fit(se3)


### Configuracao da variavel para estrategias "linear" e "quadratic"

fdi4 <- na.omit(fdi)

fdi4 <- fdi4 %>%
  distinct(country, date, gdp_pc_95d_old, .keep_all = TRUE) # remove duplicates

date2 <- fdi4$date^2
date3 <- fdi4$date^3

### Modelo 2 - com as variáveis detrended linearmente

se4 <- lm(fdi_inflow_unctad_gdp_det ~ lag_pta_force_det + lag_gattwto_det + 
            lag_bits_cuml_restricted_det + lag_polconiii_2010_det + 
            lag_polinstability_det + lag_ln_pop_det + lag_ln_gdp_pc_00d_det +
            lag_gdp_gr_det + factor(country), data = fdi4)
summary(se4)

# checando o ajuste do modelo:
rmse4 <- sqrt(mean(se4$residuals^2))
rmse4

# checando os pressupostos do modelo 

# 1 - média dos resíduos igual a zero:
mean(residuals(se4))

# 2 - resíduos com distribuição normal:
hist(residuals(se4))
shapiro.test(residuals(se4))

# 3 - multicolinearidade
vif(se4)

# 4 - heterocedasticidade antes da função
plot(se4,which = 1)
ncvTest(se4)

# 5 - presença de outliers:
ols_plot_resid_qq(se4)
ols_plot_resid_fit(se4)

# modelo final com a garantina de homocedasticidade pela função de Arellano 
mod4 <- clx(se4, fdi4$date, fdi4$country) 
mod4


### Configuração de variável sem detrend

vars.4 <- c("fdi_inflow_unctad_gdp", "lag_pta_force", "lag_gattwto", "lag_trade_pgdp", 
            "lag_bits_cuml_restricted", "lag_polconiii_2010", "lag_polinstability", 
            "lag_ln_pop", "lag_ln_gdp_pc_00d", "lag_gdp_gr","date", "country", 
            "fdi_inflow_unctad_gdp_det", "lag_pta_force_det", "ctylabel")

fdi4 <- na.omit(fdi[,vars.4])
date2 <- fdi4$date^2
date3 <- fdi4$date^3


# Realizando o detrend quadratico para as variáveis do modelo

lm1 <- lm(fdi4$fdi_inflow_unctad_gdp ~ fdi4$date + date2 + date3 + factor(fdi4$country))

fdi4$fdi_det2 <- lm1$residuals

lm1 <- lm(fdi4$lag_pta_force ~ fdi4$date + date2 + factor(fdi4$country))

fdi4$pta_det2 <- lm1$residuals

lm1 <- lm(fdi4$lag_gattwto ~ fdi4$date + date2 + factor(fdi4$country))

fdi4$gattwto_det2 <- lm1$residuals

lm1 <- lm(fdi4$lag_trade_pgdp ~ fdi4$date + factor(fdi4$country))

fdi4$trade_det2 <- lm1$residuals

lm1 <- lm(fdi4$lag_bits_cuml_restricted ~ fdi4$date + date2 + factor(fdi4$country))

fdi4$bits_det2 <- lm1$residuals

lm1 <- lm(fdi4$lag_polconiii_2010 ~ fdi4$date + date2 + factor(fdi4$country))

fdi4$pol_det2 <- lm1$residuals

lm1 <- lm(fdi4$lag_ln_pop ~ fdi4$date + date2 + factor(fdi4$country))

fdi4$pop_det2 <- lm1$residuals

lm1 <- lm(fdi4$lag_ln_gdp_pc_00d ~ fdi4$date + date2 + factor(fdi4$country))

fdi4$gdppc_det2 <- lm1$residuals

lm1 <- lm(fdi4$lag_gdp_gr ~ fdi4$date + date2 + factor(fdi4$country))

fdi4$gdpgr_det2 <- lm1$residuals

lm1 <- lm(fdi4$lag_trade_pgdp ~ fdi4$date + date2 + factor(fdi4$country))

fdi4$trade_det2 <- lm1$residuals

# Replicação do modelo 4 de Milner and Buthe com as
# Variáveis "Quadratically Detrended"

### Modelo 3

se5 <- lm(fdi_det2 ~ pta_det2 + gattwto_det2 + bits_det2 + pol_det2 + lag_polinstability + pop_det2 + gdppc_det2 + gdpgr_det2 + factor(country), data=fdi4)

summary (se5)

# checando o ajuste do modelo:
rmse5 <- sqrt(mean(se5$residuals^2))
rmse5

# checando os pressupostos do modelo:

# 1 - média dos resíduos igual a zero:
mean(residuals(se5))

# 2 - resíduos com distribuição normal:
hist(residuals(se5))
shapiro.test(residuals(se5))

# 3 - multicolinearidade
vif(se5)

# 4 - heterocedasticidade antes da função
plot(se5,which = 1)
ncvTest(se5)

# 5 - presença de outliers:
ols_plot_resid_qq(se5)
ols_plot_resid_fit(se5)

# gerando o modelo final para garantir homocedasticidade

mod5 <- clx(se5, fdi4$date, fdi4$country)
mod5

# intervalos de compatibilidade

confint(se3, level = 0.95)
confint(se4, level = 0.95)
confint(se5, level = 0.95)
dwplot(list(se3,se4,se5))

######################################

# Gráficos 

# Tendências de ptas e bits dos anos 1970 a 2007:

meanpta <- tapply(fdi$pta_force, fdi$date, mean)
meanbits <- tapply(fdi$bits_cuml_restricted, fdi$date, mean)
names(meanpta) <- names(meanbits) <-  seq(1970, 2007)
plot(names(meanpta), meanpta, xlab="Anos", ylab="PTAs em vigor, BITs cumulados", type="l")
lines(names(meanbits), meanbits, col="red")
text(1975, 3, "Mean PTAs", col="red")
text(1990, 4, "Mean BITs Cumulados")

# Plotando tendências para as variáveis detrended linearmente

meanbits2 <- tapply(fdi$lag_bits_cuml_restricted_det, fdi$date, mean)
meanpta2 <- tapply(fdi$lag_pta_force_det, fdi$date, mean)
names(meanbits2) <- names(meanpta2) <-  seq(1970, 2007)
plot(names(meanbits2), meanbits2, xlab="Date", ylab="BITs cumulados det, PTAs em vigor det", type="l")
lines(names(meanpta2), meanpta2, col="red")
text(1980, 3, "Mean BITs cum det", col="red")
text(1990, 4, "Mean PTAs em vigor det")

# Plotando tendência para as variáveis detrended quadraticamente

meanbits3 <- tapply(fdi4$bits_det2, fdi4$date, mean, na.rm = T)
meanpta3 <- tapply(fdi4$pta_det2, fdi4$date, mean, na.rm = T)
names(meanbits3) <- names(meanpta3) <-  seq(1970, 2007)
plot(names(meanbits3), meanbits3, xlab="Date", ylab="BITs cumulados det2, PTAs em vigor det2", type="l")
lines(names(meanpta3), meanpta3, col="red")
text(1985,0.3, "Mean BITs cumulados det2", col="red")
text(1990,-1, "Mean PTAs em vigor det2")



