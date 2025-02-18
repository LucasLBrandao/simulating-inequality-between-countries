library(PNADcIBGE)
library(tidyverse)
library(survey)
library(srvyr)
library(zoo)
library(data.table)

linha_de_pobreza_sm <- data.table(ano = 2023:2024, 
                                  linha_pobr = c(1320,1518)/2, 
                                  linha_ext = c(1320,1518)/4)
for(ano in 2023:2024){
        renda_bpc = "V5001A2" # Valor recebido do BPC
        tem_pbf = "V5002A"
        renda_pbf = "V5002A2" # Valor recebido do PBF
        renda_outros_benef = "V5003A2"  # Valor recebido de outros programas sociais

        entrevista <- 1

        pnadc <- get_pnadc(year = ano,
                           interview = entrevista , 
                           vars = c("UF", # Unidade da Federação
                                    "UPA", # Unidade Primária de Amostragem
                                    "V1008", # Número de seleção do domicílio
                                    "V1014", # Painel - Grupo de Amostra
                                    "V1022", # Situação do Domicílio
                                    "V2001", # Número de pessoas do domicílio
                                    "V2005", # Papel no domicílio
                                    "V2009", # Idade do morador
                                    "V2010", # Cor ou raça
                                    "V2007", # Sexo
                                    "VD4019", # Renda mensal habitual de todos os trabalhos
                                    "VD5008", # RDPC habitual de todos os trabalhos e efetivo de outras as fontes
                                    "VD5009", # Faixa de rendimento
                                    "VD2003",  # Número de componentes do domícilio
                                    "VD3004", # Nível de instrução mais elevado alcançado pela pessoa (escolaridade)
                                    renda_bpc, # Valor recebido do BPC
                                    tem_pbf, # Se recebeu valores do Bolsa família
                                    renda_pbf, # Valor recebido do Bolsa familia
                                    renda_outros_benef # Valor recebido de outros programas sociais
                           ),
                           deflator = TRUE,
                           defyear = 2024,
                           design = F,
                           labels = F)
        pnadc <- pnadc %>% as.data.table()  
        pnadc <- linha_de_pobreza_sm[pnadc[,':='(ano = as.numeric(Ano))],on = "ano"][, ':='(
                VD5008_DEF = VD5008*CO2, # Renda domiciliar per capita inflacionada
                linha_pobr_def = linha_pobr*CO3 , # Linha de pobreza inflacionada
                linha_ext_def = linha_ext*CO3, # Linha de extrema pobreza inflacionada 
                VD4019_DEF = VD4019*CO2, # Renda de todos os trabalhos inflacionada
                situacao_domicilio = case_when(as.numeric(V1022) == 1 ~ "Urbana",
                                               as.numeric(V1022) == 2 ~ "Rural",
                                               TRUE ~ as.character("")),
                VD5009 = case_when(as.numeric(VD5009) == 1 ~ "Até ¼ salário mínimo",
                                   as.numeric(VD5009) == 2 ~ "Mais de ¼ até ½ salário mínimo",
                                   TRUE ~ as.character("")),
                faixa_idade = case_when(as.numeric(V2009) <= 13 ~ "0 a 13 anos",
                                        as.numeric(V2009) > 13 & as.numeric(V2009) <= 17 ~ "14 a 17 anos",
                                        as.numeric(V2009) > 17 & as.numeric(V2009) <= 24 ~ "18 a 24 anos",
                                        as.numeric(V2009) > 24 & as.numeric(V2009) <= 59 ~ "25 a 59 anos",
                                        as.numeric(V2009) > 59 ~ "60 anos ou mais",
                                        TRUE ~ as.character("")),
                raça = case_when(as.numeric(V2010) == 1 ~ "Branca",
                                 as.numeric(V2010) %in% c(2,4) ~ "Negra",
                                 TRUE ~ as.character("")),
                sexo = ifelse(as.numeric(V2007) == 1,"Homem","Mulher"),
                escolaridade = case_when(as.numeric(VD3004) <= 2 ~ "Fundamental incompleto",
                                         as.numeric(VD3004) <= 4 ~ "Fundamental completo",
                                         as.numeric(VD3004) <= 6 ~ "Médio completo",
                                         as.numeric(VD3004) == 7 ~ "Superior",
                                         TRUE ~ as.character("")))][,':='(sexo_raca = paste(sexo,raça))] %>% as_tibble()
        
        pnadc_tratado <- pnadc %>% 
                group_by(ID_DOMICILIO) %>% 
                mutate(renda_trabalho_pc = sum(VD4019_DEF, na.rm = TRUE)/mean(VD2003, na.rm = TRUE), # Renda do trabalho per capita
                       renda_bpc_pc = sum(!!as.symbol(renda_bpc)*CO2, na.rm = TRUE)/mean(VD2003, na.rm = TRUE), # Renda do bpc per capita
                       renda_pbf_pc = sum(!!as.symbol(renda_pbf)*CO2, na.rm = TRUE)/mean(VD2003, na.rm = TRUE), # Renda do pbf per capita
                       renda_outros_benef_pc = sum(!!as.symbol(renda_outros_benef)*CO2, na.rm = TRUE)/mean(VD2003, na.rm = TRUE)) %>% # Renda outros benef per capita
                ungroup() %>% # Desagrupando por domicilio
                rowwise() %>% # cálculos no contexto de linha
                mutate(renda_benef_pc = sum(renda_bpc_pc, renda_pbf_pc, renda_outros_benef_pc, na.rm = TRUE), # renda de todos benefícios per capita
                       rdpc_s_pbf = sum(VD5008_DEF, - renda_pbf_pc,na.rm = TRUE), # renda per cpaita sem bolsa familia
                       rdpc_s_outros_benef = sum(VD5008_DEF, - renda_outros_benef_pc, na.rm = TRUE), # renda per capita sem outros beneficios
                       rdpc_s_benef = sum(VD5008_DEF, - renda_benef_pc,na.rm = TRUE ),
                       rdpc_s_pbf_e_benef = sum(VD5008_DEF, - renda_outros_benef_pc, - renda_pbf_pc, na.rm = TRUE)) %>% 
                ungroup()# renda per capita sem todos os benefícios
        pnadcA_design <- pnadc_design(pnadc_tratado)
        saveRDS(pnadcA_design, paste0("./intermediarios/renda_PNADc_brasil",ano,"visita_1.rds"))
        gc()
}

rm(list = ls())

pnadcdata  <- readRDS("intermediarios/renda_PNADc_brasil2023visita_1.rds")

pnadcdata %>% glimpse()