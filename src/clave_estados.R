pacman::p_load(tidyverse, readxl, ggrepel, sf)

# la base de origen tiene a los estados sin código, se los ponemos

bd <- read_rds(here("data", "corte_2807.rda"))

# create a dataset
conestados <- bd %>% 
  janitor::clean_names() %>% 
  mutate(code = case_when(
    entidad== "AGUASCALIENTES"  ~ 1,
    entidad== "BAJA CALIFORNIA"  ~ 2,
    entidad== "BAJA CALIFORNIA SUR"  ~ 3,
    entidad== "CAMPECHE"  ~ 4,
    entidad== "CHIAPAS"  ~ 7,
    entidad== "CHIHUAHUA"  ~ 8,
    entidad== "CIUDAD DE MEXICO"  ~ 9,
    entidad== "COAHUILA DE ZARAGOZA"  ~ 5,
    entidad== "COLIMA"  ~ 6,
    entidad== "DURANGO"  ~ 10,
    entidad== "GUANAJUATO"  ~ 11,
    entidad== "GUERRERO"  ~ 12,
    entidad== "HIDALGO"  ~ 13,
    entidad== "JALISCO"  ~ 14,
    entidad== "MEXICO"  ~ 15,
    entidad== "MICHOACAN DE OCAMPO"  ~ 16,
    entidad== "MORELOS"  ~ 17,
    entidad== "NAYARIT"  ~ 18,
    entidad== "NUEVO LEON"  ~ 19,
    entidad== "OAXACA"  ~ 20,
    entidad== "PUEBLA"  ~ 21,
    entidad== "QUERETARO"  ~ 22,
    entidad== "QUINTANA ROO"  ~ 23,
    entidad== "SAN LUIS POTOSI"  ~ 24,
    entidad== "SINALOA"  ~ 25,
    entidad== "SONORA"  ~ 26,
    entidad== "TABASCO"  ~ 27,
    entidad== "TAMAULIPAS"  ~ 28,
    entidad== "TLAXCALA"  ~ 29,
    entidad== "VERACRUZ DE IGNACIO DE LA LLAVE"  ~ 30,
    entidad== "YUCATAN"  ~ 31,    
    entidad== "ZACATECAS"  ~ 32
  ))

