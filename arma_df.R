## data ----
# repo <- "data/"
# data <- read.csv2(paste0(repo,"cotizaciones_alicuotas_10_topes.csv"), dec = ".")
# sucursales <- read.xlsx(paste0(repo,"Sucursales.xlsx"))
# sectorE <- read.xlsx(paste0(repo,"Sector - CIIU3 - ciiu6.xlsx"))
# especies <-  read.csv2(paste0(repo,"especies.csv"))
# ilt <-  read.csv2(paste0(repo,"ilt.csv"))
# incap <- read.xlsx(paste0(repo,"incapacidades.xlsx"))
# juicios <- read.xlsx(paste0(repo,"juicios.xlsx"))
# 
# data_app <- data %>% select(CUIT,
#                             rango_capitas,
#                             provincia,
#                             sector,
#                             ciiu3,
#                             ciiu6_r2,
#                             alicuotaVerdeTopeo,
#                             bins_stros_pc,
#                             alicuota_enfoque,
#                             Variable.Piscys..Cotizador.,
#                             Variable.Contrato,
#                             tamano_empresa,
#                             
#                             sucursal_id,
#                             #art_amiga,
#                             #Status,
#                             capitas,
#                             masa_salarial,
#                             art_amiga,
#                             Variable.Contrato,
#                             gr_ciiu2) %>% mutate(
#                               salario                   = replace_na(masa_salarial/capitas,0)
#                             ) %>% filter (alicuotaVerdeTopeo < 25) %>% mutate(
#                               provincia = as.character(provincia),
#                               bins_stros_pc = as.character(bins_stros_pc),
#                               sector = as.character(sector))
# 
# 
# 
# ciiuetp <- read.xlsx(paste0(repo,"CIIUs No Suscribible.xlsx"))
# 
# 
# data_app <- data_app %>% mutate (Ettp = if_else(ciiu6_r2 %in% ciiuetp$Ciiu.Rev..2 |
#                                                   ciiu6_r2 %in% ciiuetp$Ciiu.Rev..3 |
#                                                   ciiu6_r2 %in% ciiuetp$Ciiu.Rev..4, 
#                                                 "No Suscribible",
#                                                 "Suscribible"))
# 
# 
# data_app <- left_join(data_app, sucursales %>% 
#                         select(idSUC, Region)
#                       , by =c("sucursal_id" ="idSUC"))
# data_app <- left_join(data_app, sectorE %>% 
#                         select(ciiu6, ciiu2_desc)
#                       , by =c("ciiu6_r2" ="ciiu6"))
# 
# data_app_1 <- data_app %>% filter (Variable.Contrato != 0 & Variable.Piscys..Cotizador. != 0 ) %>%
#   mutate (
#     dif_piscys_vs_vblecontrato = round(Variable.Piscys..Cotizador./Variable.Contrato -1,2)
#   )
# 
# 
# # métricas
# 
# z <-  round(nrow(
#   data %>% filter(alicuotaVerdeTopeo > 25))/
#     nrow(data),5)*100
# d_save <- NULL
# e_save <- NULL
# ##---- agrego los datos de relatividades 
# 
# ## especies
# especies_ciiu <- especies %>% 
#   filter (var =="gr_ciiu2") %>% 
#   mutate(valor = as.integer(as.character(valor))) %>% 
#   select(
#     valor,
#     coef_exp
#   ) %>% 
#   rename (relatividad_especies_ciiu =coef_exp)
# data_app <- left_join(data_app,
#                       especies_ciiu,
#                       by = c( "gr_ciiu2" ="valor" ))
# 
# 
# especies_pcia <- especies %>% 
#   filter (var =="provincia") %>% 
#   mutate(valor = as.character(valor)) %>% 
#   select(
#     valor,
#     coef_exp
#   ) %>% 
#   rename (relatividad_especies_pcia =coef_exp)
# data_app <- left_join(data_app,
#                       especies_pcia,
#                       by = c("provincia" = "valor"))
# especies_tamano_empresa <- especies %>% 
#   filter (var =="tamano_empresa") %>% 
#   mutate(valor = as.character(valor)) %>% 
#   select(
#     valor,
#     coef_exp
#   ) %>% 
#   rename (relatividad_especies_tamano_empresa =coef_exp)
# data_app <- left_join(data_app,
#                       especies_tamano_empresa,
#                       by = c("tamano_empresa" = "valor"))
# 
# ## ilt
# 
# 
# ilt_ciiu <- ilt %>% 
#   filter (var =="gr_ciiu2") %>% 
#   mutate(valor = as.integer(as.character(valor))) %>% 
#   select(
#     valor,
#     coef_exp
#   ) %>% 
#   rename (relatividad_ilt_ciiu =coef_exp)
# data_app <- left_join(data_app,
#                       ilt_ciiu,
#                       by = c( "gr_ciiu2" ="valor" ))
# 
# 
# ilt_pcia <- ilt %>% 
#   filter (var =="provincia") %>% 
#   mutate(valor = as.character(valor)) %>% 
#   select(
#     valor,
#     coef_exp
#   ) %>% 
#   rename (relatividad_ilt_pcia =coef_exp)
# data_app <- left_join(data_app,
#                       ilt_pcia,
#                       by = c("provincia" = "valor"))
# 
# 
# ilt_tamano_empresa <- ilt %>% 
#   filter (var =="tamano_empresa") %>% 
#   mutate(valor = as.character(valor)) %>% 
#   select(
#     valor,
#     coef_exp
#   ) %>% 
#   rename (relatividad_ilt_tamano_empresa =coef_exp)
# data_app <- left_join(data_app,
#                       ilt_tamano_empresa,
#                       by = c("tamano_empresa" = "valor"))
# 
# 
# 
# ## incap
# 
# 
# incap_ciiu <- incap %>% 
#   filter (var =="gr_ciiu2") %>% 
#   mutate(valor = as.integer(as.character(valor))) %>% 
#   select(
#     valor,
#     coef_exp
#   ) %>% 
#   rename (relatividad_incap_ciiu =coef_exp)
# data_app <- left_join(data_app,
#                       incap_ciiu,
#                       by = c( "gr_ciiu2" ="valor" ))
# 
# 
# incap_pcia <- incap %>% 
#   filter (var =="provincia") %>% 
#   mutate(valor = as.character(valor)) %>% 
#   select(
#     valor,
#     coef_exp
#   ) %>% 
#   rename (relatividad_incap_pcia =coef_exp)
# data_app <- left_join(data_app,
#                       incap_pcia,
#                       by = c("provincia" = "valor"))
# 
# 
# incap_tamano_empresa <- incap %>% 
#   filter (var =="tamano_empresa") %>% 
#   mutate(valor = as.character(valor)) %>% 
#   select(
#     valor,
#     coef_exp
#   ) %>% 
#   rename (relatividad_incap_tamano_empresa =coef_exp)
# data_app <- left_join(data_app,
#                       incap_tamano_empresa,
#                       by = c("tamano_empresa" = "valor"))
# 
# ## juicios
# 
# 
# juicios_ciiu <- juicios %>% 
#   filter (var =="gr_ciiu2") %>% 
#   mutate(valor = as.integer(as.character(valor))) %>% 
#   select(
#     valor,
#     coef_exp
#   ) %>% 
#   rename (relatividad_juicios_ciiu =coef_exp)
# data_app <- left_join(data_app,
#                       juicios_ciiu,
#                       by = c( "gr_ciiu2" ="valor" ))
# 
# 
# juicios_pcia <- juicios %>% 
#   filter (var =="provincia") %>% 
#   mutate(valor = as.character(valor)) %>% 
#   select(
#     valor,
#     coef_exp
#   ) %>% 
#   rename (relatividad_juicios_pcia =coef_exp)
# data_app <- left_join(data_app,
#                       juicios_pcia,
#                       by = c("provincia" = "valor"))
# 
# 
# juicios_tamano_empresa <- juicios %>% 
#   filter (var =="tamano_empresa") %>% 
#   mutate(valor = as.character(valor)) %>% 
#   select(
#     valor,
#     coef_exp
#   ) %>% 
#   rename (relatividad_juicios_tamano_empresa =coef_exp)
# data_app <- left_join(data_app,
#                       juicios_tamano_empresa,
#                       by = c("tamano_empresa" = "valor"))
# 
# ## data sample
# samp <- sample(nrow(data_app),1000)
# data_app <- data_app[samp,]
# saveRDS(data_app, "c:/users/jrr/desktop/data_app.rds")
