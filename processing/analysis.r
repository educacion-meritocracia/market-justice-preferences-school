
# 1. Packages -------------------------

pacman::p_load(tidyverse,
               ggpubr, 
               lme4, 
               ordinal,
               sjPlot, 
               texreg,
               easystats,
               performance)

options(scipen=999)
rm(list = ls())

# 2. Data -------------------------

load("input/data/data_index.RData")
load("input/data/deservingness.RData")
load("input/data/soc_effort.RData")
load("input/data/soc_talent.RData")
load("input/data/school_effort.RData")
load("input/data/school_talent.RData")
load("input/data/data_rec.RData")


# 3. Analysis -------------------------

odds_fun <- function(modelos) {
   or <- texreg::extract(model = modelos, include.deviance = T)
   or@coef <- exp(or@coef)
   return(or)
}


# 3.1 Pensions ----

data_pension <- data_rec %>% 
    dplyr::select(-just_educ, -just_salud, -redistribucion)
data_pension <- na.omit(data_pension)
data_pension$just_pension_fact <- as_factor(data_pension$just_pension)

# Null model
m0_p <- clmm(just_pension_fact ~ 1 + (1 | mrbd), data=data_pension)

performance::icc(m0_p, by_group = T) 
## ICC School = 0.033

# Model 1: Only meritocracy variables
m1_p <- clmm(just_pension_fact ~ 1 + inteligencia_esc + esfuerzo_esc + 
    inteligencia_soc + esfuerzo_soc + merito_soc + (1 | mrbd), data=data_pension) 

# Model 2: Meritocracy variables + mean_educ + cod_depe2 + cod_grupo + simce
m2_p <- clmm(just_pension_fact ~ 1 + inteligencia_esc + esfuerzo_esc + 
    inteligencia_soc + esfuerzo_soc + merito_soc + mean_educ + cod_depe2 + 
    cod_grupo + simce + (1 | mrbd), data=data_pension) 

# Model 3: Meritocracy variables + mean_educ + cod_depe2 + cod_grupo + simce + controls
m3_p <- clmm(just_pension_fact ~ 1 + inteligencia_esc + esfuerzo_esc + 
    inteligencia_soc + esfuerzo_soc + merito_soc + educacion_rec + 
    libros_rec + acc_tec + internet_rec + mean_educ + cod_depe2 + 
    cod_grupo + simce + (1 | mrbd), data=data_pension) 

tbls <- list(m1_p, m2_p, m3_p)

modelos_odd_pension <- map(tbls, odds_fun)

screenreg(l = modelos_odd_pension, stars = c(0.05, 0.01, 0.001))

# 3.2 Health ----

data_salud <- data_rec %>% 
    dplyr::select(-just_pension, -just_educ, -redistribucion)
data_salud <- na.omit(data_salud)
data_salud$just_salud_fact <- as_factor(data_salud$just_salud)

# Null model
m0_h <- clmm(just_salud_fact ~ 1 + (1 | mrbd), data=data_salud)

performance::icc(m0_h, by_group = T) 
## ICC School = 0.048

# Model 1: Only meritocracy variables
m1_h <- clmm(just_salud_fact ~ 1 + inteligencia_esc + esfuerzo_esc + 
    inteligencia_soc + esfuerzo_soc + merito_soc + (1 | mrbd), data=data_salud) 

# Model 2: Meritocracy variables + mean_educ + cod_depe2 + cod_grupo + simce
m2_h <- clmm(just_salud_fact ~ 1 + inteligencia_esc + esfuerzo_esc + 
    inteligencia_soc + esfuerzo_soc + merito_soc + mean_educ + cod_depe2 + 
    cod_grupo + simce + (1 | mrbd), data=data_salud) 

# Model 3: Meritocracy variables + mean_educ + cod_depe2 + cod_grupo + simce + controls
m3_h <- clmm(just_salud_fact ~ 1 + inteligencia_esc + esfuerzo_esc + 
    inteligencia_soc + esfuerzo_soc + merito_soc + educacion_rec + 
    libros_rec + acc_tec + internet_rec + mean_educ + cod_depe2 + 
    cod_grupo + simce + (1 | mrbd), data=data_salud) 

tbls <- list(m1_h, m2_h, m3_h)

modelos_odd_health <- map(tbls, odds_fun)

screenreg(l = modelos_odd_health, stars = c(0.05, 0.01, 0.001))

# 3.3 Education ----

data_educ <- data_rec %>% 
    dplyr::select(-just_pension, -just_salud, -redistribucion)
data_educ <- na.omit(data_educ)
data_educ$just_educ_fact <- as_factor(data_educ$just_educ)

# Null model
m0_e <- clmm(just_educ_fact ~ 1 + (1 | mrbd), data=data_educ)

performance::icc(m0_e, by_group = T) 
## ICC School = 0.038

# Model 1: Only meritocracy variables
m1_e <- clmm(just_educ_fact ~ 1 + inteligencia_esc + esfuerzo_esc + 
    inteligencia_soc + esfuerzo_soc + merito_soc + (1 | mrbd), data=data_educ) 

# Model 2: Meritocracy variables + mean_educ + cod_depe2 + cod_grupo + simce
m2_e <- clmm(just_educ_fact ~ 1 + inteligencia_esc + esfuerzo_esc + 
    inteligencia_soc + esfuerzo_soc + merito_soc + mean_educ + cod_depe2 + 
    cod_grupo + simce + (1 | mrbd), data=data_educ) 

# Model 3: Meritocracy variables + mean_educ + cod_depe2 + cod_grupo + simce + controls
m3_e <- clmm(just_educ_fact ~ 1 + inteligencia_esc + esfuerzo_esc + 
    inteligencia_soc + esfuerzo_soc + merito_soc + educacion_rec + 
    libros_rec + acc_tec + internet_rec + mean_educ + cod_depe2 + 
    cod_grupo + simce + (1 | mrbd), data=data_educ) 

tbls <- list(m1_e, m2_e, m3_e)

modelos_odd_educ <- map(tbls, odds_fun)

screenreg(l = modelos_odd_educ, stars = c(0.05, 0.01, 0.001))

# Complete models

screenreg(l = list(modelos_odd_pension[[3]], 
                   modelos_odd_health[[3]], 
                   modelos_odd_educ[[3]]), 
    stars = c(0.05, 0.01, 0.001))


a <- texreg::extract(model = m3_p, include.deviance = T, include.loglik = T)

screenreg(a)





test01 <- anova(m0_p, m2_p, test = "Chisq")
test02 <- anova(m0_p, m3_p, test = "Chisq")

test.pvalues1 <- test01$`Pr(>Chisq)`[2]
test.pvalues2 <- test02$`Pr(>Chisq)`[2]

deviance1 <- test01$deviance[2]


anova(m0_p, m2_p, m3_p)


performance::test_likelihoodratio(m0_p, m2_p)


