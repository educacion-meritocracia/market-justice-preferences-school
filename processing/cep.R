
cep <- read_rds(file = "input/data/base_consolidada_2010_2023_v11.rds") %>% 
  as_tibble()

cep %>% 
  select(starts_with("encuesta"), educacion_64, pensiones_5, salud_10, educacion_30, 
         educacion_30_a1, educacion_30_b1, salud_83) %>% 
  group_by(encuesta_a) %>%
  summarise(across(c(educacion_64, pensiones_5, salud_10, educacion_30, 
                     educacion_30_a1, educacion_30_b1, salud_83), ~ sum(is.na(.)), .names = "NA_{col}"))

frq(cep$pensiones_5)
frq(cep$salud_10)
frq(cep$educacion_30)

cep %>% 
  group_by(encuesta_a, salud_10) %>% 
  tally() %>% 
  print(n = nrow(.)) # 2017

cep %>% 
  group_by(encuesta_a, pensiones_5) %>% 
  tally() %>% 
  print(n = nrow(.)) # 2022

cep %>% 
  group_by(encuesta_a, educacion_30) %>% 
  tally() %>% 
  print(n = nrow(.)) # 2014

salud_cep <- cep %>%
  select(starts_with("encuesta"), salud_10, pond) %>%
  filter(encuesta_a == 2017, !is.na(salud_10)) %>%
  filter(encuesta_m == max(encuesta_m, na.rm = TRUE))

pension_cep <- cep %>%
  select(starts_with("encuesta"), pensiones_5, pond) %>%
  filter(encuesta_a == 2022, !is.na(pensiones_5)) %>%
  filter(encuesta_m == max(encuesta_m, na.rm = TRUE))

educacion_cep <- cep %>%
  select(starts_with("encuesta"), educacion_30, pond) %>%
  filter(encuesta_a == 2014, !is.na(educacion_30)) %>%
  filter(encuesta_m == max(encuesta_m, na.rm = TRUE))

salud_cep$salud_10 <- sjlabelled::set_na(salud_cep$salud_10, na = c(-8,-9))
pension_cep$pensiones_5 <- sjlabelled::set_na(pension_cep$pensiones_5, na = c(-8,-9))
educacion_cep$educacion_30 <- sjlabelled::set_na(educacion_cep$educacion_30, na = c(-8,-9))

frq(salud_cep$salud_10, weights = salud_cep$pond)
frq(pension_cep$pensiones_5, weights = pension_cep$pond)
frq(educacion_cep$educacion_30, weights = educacion_cep$pond)
