pacman::p_load(tidyverse, here,ggridges,readxl, ggrepel, sf)

require(geofacet)
source(here::here("src","clave_estados.R"))

# instituciones -----------------------------------------------------------


conestados %>% 
  janitor::clean_names() %>% 
  mutate(fecha_solicitud=lubridate::ymd(fecha_solicitud)) %>% 
  group_by(institucion) %>% 
  summarise(n=n()) %>% 
  ungroup() %>% 
  mutate(totales=sum(n, na.rm=T), 
         pct=n/totales) %>% 
  ggplot(aes(x = reorder(institucion, n),
             y= n,
             fill=institucion)) +
  geom_bar(stat="identity", position="dodge") +
  geom_vline(xintercept = 0)+
  ggfittext::geom_bar_text(outside = T,contrast = T,
                           aes(label=glue::glue("{scales::comma(n, accuracy=1)}\n
                                                  ({scales::percent(pct, accuracy=0.1)})")))+
  scale_fill_manual(values=c("#005835", "#24893D","#8A1538")) +
  labs(title ="Medicamentos solicitados, por institución",
       subtitle="Desde el 2 de junio al 25 de julio de 2025", x = "",
       y = "Total de claves (medicamentos)",
       caption = stringr::str_wrap(glue::glue("Fuente: Con datos de la Plataforma de Monitoreo de Medicamentos"), 100),
       fill="") +
  scale_y_continuous(labels = scales::label_comma(accuracy = 1))+
  theme_bw()+
  theme(text = element_text(color = "grey35"),
        plot.title = element_text(size = 15, face = "bold",  color = "grey35"),
        plot.subtitle = element_text(size = 10, face = "bold", colour = "#666666"),
        plot.caption = element_text(size = 10),
        legend.position = "none",
        panel.grid = element_blank(),
        panel.border =   element_blank(),
        axis.title = element_text(size = 14, face = "bold"),
        axis.ticks.y=element_blank(),
        axis.text = element_text(size = 10, face = "bold")) +
  coord_flip()
ggsave(here("out", "barras_instituciones.png"), width = 11, height = 7, units="in")


b2<- conestados%>% 
  janitor::clean_names() %>% 
  mutate(fecha_solicitud=lubridate::ymd(fecha_solicitud)) %>% 
  group_by(institucion, estatus) %>% 
  summarise(n=n()) %>% 
  ungroup() %>% 
  group_by(institucion) %>%
  mutate(total_institucion=sum(n, na.rm=T), 
         pct=n/total_institucion) %>% 
  ungroup()

b2 %>% ggplot(aes(fill=estatus, 
                  y=pct, 
                  x=institucion)) + 
  geom_bar(position="stack", stat="identity") +
  geom_vline(xintercept = 0)+
  ggfittext::geom_fit_text(contrast = T,
                           position = "stack", 
                           aes(label = glue::glue("{scales::percent(pct, accuracy=0.1)}")), 
                           show.legend = FALSE)+
  scale_fill_manual(values=c("COMPLETA"="#ffffb2", "EN TRÁNSITO"= "#fd8d3c",
                             "INCOMPLETA"="#f03b20", "INCUMPLIDA"="#bd0026")) +
  labs(title ="Estado de entrega de medicamentos solicitados, por institución",
       subtitle="Del 2 de junio al 25 de julio de 2025", x = "",
       y = "% de claves (medicamentos)",
       caption = stringr::str_wrap(glue::glue("Fuente: Con datos de la Plataforma de Monitoreo de Medicamentos"), 100),
       fill="") +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1))+
  theme_bw()+
  theme(text = element_text(color = "grey35"),
        plot.title = element_text(size = 15, face = "bold",  color = "grey35"),
        plot.subtitle = element_text(size = 10, face = "bold", colour = "#666666"),
        plot.caption = element_text(size = 10),
        legend.position = "bottom",
        panel.grid = element_blank(),
        panel.border =   element_blank(),
        axis.title = element_text(size = 14, face = "bold"),
        axis.ticks.y=element_blank(),
        axis.text = element_text(size = 10, face = "bold"))

ggsave(here("out", "stack_instituciones.png"), width = 11, height = 7, units="in")


# estatus total -----------------------------------------------------------

conestados %>% 
  janitor::clean_names() %>% 
  mutate(fecha_solicitud=lubridate::ymd(fecha_solicitud)) %>% 
  group_by(estatus) %>% 
  summarise(n=n()) %>% 
  ungroup() %>% 
  mutate(totales=sum(n, na.rm=T), 
         pct=n/totales) %>% 
  ggplot(aes(x = reorder(estatus, n),
             y= n,
             fill=estatus)) +
  geom_bar(stat="identity", position="dodge") +
  geom_vline(xintercept = 0)+
  ggfittext::geom_bar_text(outside = T,contrast = T,
                           aes(label=glue::glue("{scales::comma(n, accuracy=1)}\n
                                                  ({scales::percent(pct, accuracy=0.1)})")))+
  scale_fill_manual(values=c("COMPLETA"="#ffffb2", "EN TRÁNSITO"= "#fd8d3c",
                             "INCOMPLETA"="#f03b20", "INCUMPLIDA"="#bd0026")) +
  labs(title ="Medicamentos solicitados, por estatus de entrega",
       subtitle="Desde el 2 de junio al 25 de julio de 2025", x = "",
       y = "Total de claves (medicamentos)",
       caption = stringr::str_wrap(glue::glue("Fuente: Con datos de la Plataforma de Monitoreo de Medicamentos"), 100),
       fill="") +
  scale_y_continuous(labels = scales::label_comma(accuracy = 1))+
  theme_bw()+
  theme(text = element_text(color = "grey35"),
        plot.title = element_text(size = 15, face = "bold",  color = "grey35"),
        plot.subtitle = element_text(size = 10, face = "bold", colour = "#666666"),
        plot.caption = element_text(size = 10),
        legend.position = "none",
        panel.grid = element_blank(),
        panel.border =   element_blank(),
        axis.title = element_text(size = 14, face = "bold"),
        axis.ticks.y=element_blank(),
        axis.text = element_text(size = 9, face = "bold")) +
  coord_flip()
ggsave(here("out", "barras_totales.png"), width = 11, height = 7, units="in")



# estados -----------------------------------------------------------------

myg <- mx_state_grid2
c_25 <- conestados %>% 
  mutate(fecha_solicitud=lubridate::ymd(fecha_solicitud)) %>% 
  group_by(code, entidad, estatus) %>% 
  summarise(n=n()) %>% 
  group_by(entidad) %>% 
  mutate(total_entidad=sum(n, na.rm=T)) %>% 
  ungroup() %>% 
  mutate(porc=n/total_entidad) %>% 
  select(code, entidad, estatus, porc) %>% 
  left_join(myg, by="code")


ggplot(c_25, aes("", porc, fill = estatus)) +
  geom_col(width = 1) +
  scale_fill_manual(values=c("COMPLETA"="#ffffb2", "EN TRÁNSITO"= "#fd8d3c",
                             "INCOMPLETA"="#f03b20", "INCUMPLIDA"="#bd0026")) +
  ggfittext::geom_fit_text(contrast = T,
                           position = "stack", 
                           aes(label = glue::glue("{scales::percent(porc, accuracy=0.1)}")), 
                           show.legend = FALSE)+
  facet_geo(~name, grid = "mx_state_grid2") +
  scale_y_continuous(expand = c(0, 0)) +
  labs(title ="Estatus de la entrega de medicamentos solicitados por estado",
       subtitle="Del 2 de junio al 25 de julio de 2025", x = "",
       y = "",
       caption = stringr::str_wrap(glue::glue("Fuente: Con datos de la Plataforma de Monitoreo de Medicamentos"), 100),
       fill="") +
  theme_bw()+
  theme(text = element_text(color = "grey35"),
        plot.title = element_text(size = 15, face = "bold",  color = "grey35"),
        plot.subtitle = element_text(size = 10, face = "bold", colour = "#666666"),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 5),
        axis.ticks.x = element_blank(),
        legend.title = element_blank(),
        strip.text.x = element_text(size = 6), 
        strip.background=element_rect(colour="black",
                                      fill="white"))


ggsave(here("out", "mapa.png"), width = 13, height = 9, units="in")



# dias --------------------------------------------------------------------


c_25 <- conestados %>% 
  mutate(fecha_solicitud=lubridate::ymd(fecha_solicitud), 
         fecha_entrega=lubridate::ymd(fecha_entrega), 
         fecha_corte=lubridate::ymd("2025-07-25"), 
         dias= case_when(
           is.na(fecha_entrega)  ~ fecha_corte-fecha_solicitud,
           !is.na(fecha_entrega)  ~ fecha_entrega-fecha_solicitud), 
         dias=as.numeric(dias), 
         entidad=gsub("VERACRUZ DE IGNACIO DE LA LLAVE", "VERACRUZ", entidad), 
         entidad=gsub("MICHOACAN DE OCAMPO", "MICHOACAN", entidad),
         entidad=gsub("COAHUILA DE ZARAGOZA", "COAHUILA", entidad)) %>% 
  # filter(estatus=="COMPLETA" | estatus=="INCOMPLETA") %>%
  group_by(entidad) %>% 
  mutate(prom=mean(dias, na.rm=T)) %>%
  # mutate(prom=weighted.mean(dias, piezas_solicitadas)) %>%
  ungroup() %>% 
  mutate(entidad = stringr::str_to_title(entidad),
         entidad = fct_reorder(entidad, prom))
# %>% 
#   #la base viaja al futuro y hay solicitudes hechas en agosto, aunque sigamos en julio
#   filter(dias>0)



# ggplot(c_25, aes(x = dias, y = entidad)) +
#   geom_density_ridges() +
#   theme_ridges() + 
#   theme(legend.position = "none")
c_25 %>% 
  ggplot( aes(y=entidad, x=dias)) +
  geom_density_ridges(fill="#fdbb84") +
  labs(title =stringr::str_wrap(glue::glue("Distribución del número de días entre el pedido y la entrega de insumos médicos, por estado"), 100),
       subtitle="Del 2 de junio al 25 de julio de 2025", x = "Total de días",
       y = "",
       caption = stringr::str_wrap(glue::glue("Fuente: Con datos de la Plataforma de Monitoreo de Medicamentos"), 100),
       fill="") +
  xlim(0, 35)+
  theme_ridges() +
  theme(text = element_text(color = "grey35"),
        plot.title = element_text(size = 10, face = "bold",  color = "grey35"),
        plot.subtitle = element_text(size = 10, face = "bold", colour = "#666666"),
        axis.text.x = element_text(face="bold",  color = "grey35"),
        axis.text.y = element_text(face="bold",  color = "grey35"))

ggsave(here("out", "distribucion_dias.png"), width = 13, height = 9, units="in")

# proveedor ---------------------------------------------------------------

prove <- conestados %>% 
  janitor::clean_names() %>% 
  mutate(fecha_solicitud=lubridate::ymd(fecha_solicitud)) %>% 
  group_by(proveedor, estatus) %>% 
  summarise(n=n()) %>% 
  ungroup() %>% 
  group_by(proveedor) %>% 
  mutate(total_provedor=sum(n, na.rm=T)) %>% 
  ungroup() %>% 
  mutate(porc_claves=round((n/total_provedor)*100, 2))

prove_porc <- conestados %>% 
  janitor::clean_names() %>% 
  mutate(fecha_solicitud=lubridate::ymd(fecha_solicitud)) %>% 
  group_by(proveedor) %>% 
  summarise(n=n()) %>% 
  ungroup() %>% 
  mutate(total=sum(n, na.rm=T),
         porc_claves=round((n/total)*100,2)) %>% 
  select(proveedor, porc_claves)

prove_inclum <- conestados %>% 
  janitor::clean_names() %>% 
  mutate(fecha_solicitud=lubridate::ymd(fecha_solicitud)) %>% 
  group_by(proveedor, estatus) %>% 
  summarise(n=n()) %>% 
  ungroup() %>% 
  filter(estatus=="INCUMPLIDA") %>% 
  mutate(total=sum(n, na.rm=T),
         porc_incum=round((n/total)*100,2)) %>% 
  select(proveedor, porc_incum)


scatplot <- left_join(prove_inclum, prove_porc)

scatplot %>% 
  ggplot( aes(x = porc_incum, y =porc_claves)) +
  geom_point(alpha = .8, color = "#006837", size=3) +
  geom_text_repel(
    data = scatplot %>%
      filter(porc_incum>4)%>%
      mutate(proveedor = stringr::str_to_lower(proveedor),
             proveedor = stringr::str_to_title(proveedor)),
    aes(x = porc_incum, y = porc_claves,label = stringr::str_wrap(proveedor, 35)),
    size = 3, nudge_y = 1, segment.size = 0.2
  )+
  geom_text_repel(
    data = scatplot %>%
      filter(porc_claves>1.95)%>%
      mutate(proveedor = stringr::str_to_lower(proveedor),
             proveedor = stringr::str_to_title(proveedor)),
    aes(x = porc_incum, y = porc_claves,label = stringr::str_wrap(proveedor, 35)),
    size = 3, nudge_y = 1, segment.size = 0.2
  )+
  # scale_x_log10() +
  # scale_y_log10() +
  geom_abline(slope=1, intercept = 0, color="red")+
  xlim(0, 9)+
  ylim(0, 9)+
  labs(title="Porcentaje de claves incumplidas por proveedor vs. Porcentaje de claves por proveedor", 
       subtitle="Del 2 de junio al 25 de julio de 2025",
     x= "% claves incumplidas por proveedor", 
     y= "% claves por proveedor")+  
  theme_bw()+
  theme(text = element_text(color = "grey35"),
        plot.title = element_text(size = 15, face = "bold",  color = "grey35"),
        plot.subtitle = element_text(size = 10, face = "bold", colour = "#666666"),
        axis.text.x = element_text(face="bold",  color = "grey35"),
        axis.text.y = element_text(face="bold",  color = "grey35"))

ggsave(here("out", "scatter.png"), width = 10, height = 7, units="in")

# tiempo, claves y proveedor ---------------------------------------------------------


prove_tiempo <- conestados %>% 
  janitor::clean_names() %>% 
  mutate(fecha_solicitud=lubridate::ymd(fecha_solicitud), 
         fecha_entrega=lubridate::ymd(fecha_entrega), 
         fecha_corte=lubridate::ymd("2025-07-25"), 
         dias= case_when(
           is.na(fecha_entrega)  ~ fecha_corte-fecha_solicitud,
           !is.na(fecha_entrega)  ~ fecha_entrega-fecha_solicitud), 
         dias=as.numeric(dias)) %>% 
  group_by(proveedor) %>% 
  summarise(total_claves=n(), 
            prom_dias=mean(dias, na.rm=T)) %>% 
  ungroup()


media_global <- mean(prove_tiempo$prom_dias)
sd_global    <- sd(prove_tiempo$prom_dias)

prove_tiempo <- prove_tiempo %>% 
mutate(
  es_outlier = prom_dias > (media_global + 2 * sd_global / sqrt(total_claves)),
  marcar_proveedor = es_outlier & total_claves >= 500 & prom_dias >= 20,
  tiempo=case_when(es_outlier==T ~ "Lento", 
                   es_outlier==F ~ "Normal"))



prove_tiempo %>% 
  filter(proveedor!="LABORATORIOS PISA SA DE CV") %>% 
  mutate(proveedor = stringr::str_to_lower(proveedor),
    proveedor = stringr::str_to_title(proveedor)) %>% 
ggplot() +
  geom_point(aes(x = total_claves, y = prom_dias, color = tiempo),
    size = 3,
             position = position_jitter(width = 0.1, height = 0)) +
  scale_color_manual(values=c("#ffffb2", "#fd8d3c"))+
  # Etiquetas solo para proveedores lentos y relevantes
  geom_point(
    data = filter(prove_tiempo, marcar_proveedor==T),
    aes(x = total_claves, y = prom_dias, color=marcar_proveedor),  size = 3,
    position = position_jitter(width = 0.1, height = 0), color="#bd0026") +
  geom_text_repel(
    data = prove_tiempo %>%
      filter(marcar_proveedor==T)%>%
      mutate(proveedor = stringr::str_to_lower(proveedor),
             proveedor = stringr::str_to_title(proveedor)),
    aes(x = total_claves, y = prom_dias,label = stringr::str_wrap(proveedor, 35)),
    size = 3, nudge_y = 1, segment.size = 0.2
  )+

  # Banda superior: media + 2·sd/√n
  stat_function(
    fun = function(x) media_global + 2 * sd_global / sqrt(x),
    linetype = "dashed"
  ) +
  # Banda inferior: media - 2·sd/√n
  stat_function(
    fun = function(x) media_global - 2 * sd_global / sqrt(x),
    linetype = "dashed"
  ) +
  # scale_x_log10() +
  labs(title="Total de claves de cada proveedor vs. Tiempo promedio de entrega", 
       subtitle="Del 2 de junio al 25 de julio de 2025",
       x= "Total de claves del proveedor", 
       y= "Tiempo promedio de entrega (dias)", 
       color= stringr::str_wrap("Tiempo de entrega promedio", 20))+  
  theme_bw()+
  theme(text = element_text(color = "grey35"),
        plot.title = element_text(size = 15, face = "bold",  color = "grey35"),
        plot.subtitle = element_text(size = 10, face = "bold", colour = "#666666"),
        axis.text.x = element_text(face="bold",  color = "grey35"),
        axis.text.y = element_text(face="bold",  color = "grey35"))

ggsave(here("out", "funnel_plot.png"), width = 14, height = 9, units="in")

outliers <- prove_tiempo %>% 
  filter(marcar_proveedor==T) %>% 
  select(proveedor) %>% 
  unlist()

gravedad <- conestados %>% 
  filter(proveedor%in%outliers & estatus!="COMPLETA") %>% 
  # acumulan el 10% de las claves
  select(descripcion,proveedor) %>% 
  unique()

write_csv(gravedad, here("out", "insumos_outliers.csv"))

# descriptivo -------------------------------------------------------------
# pregunta: ¿cuántos proveedores acumulan el 50% de las claves?
# Respuesta: 35
t <- conestados %>% 
  group_by(proveedor) %>% 
  summarise(n=n()) %>% 
  ungroup() %>% 
  filter(!is.na(proveedor)) %>% 
  mutate(totales=sum(n, na.rm = T),
    pct=round((n/totales)*100, 2)) %>% 
  arrange(-pct) %>% 
  mutate(acumulado = cumsum(pct))
  

# medicamentos ------------------------------------------------------------

medi <- conestados%>% 
  janitor::clean_names() %>% 
  mutate(fecha_solicitud=lubridate::ymd(fecha_solicitud)) %>%
  filter(estatus=="INCUMPLIDA" | estatus=="INCOMPLETA") %>%
  mutate(faltantes=case_when(
    piezas_entregadas== 0  ~ piezas_solicitadas,
    piezas_entregadas!= 0  ~ piezas_solicitadas - piezas_entregadas
  )) %>%
  group_by(descripcion) %>% 
  summarise(total_faltantes=sum(faltantes, na.rm=T)) %>% 
  ungroup() %>% 
  arrange(-total_faltantes) %>% 
  head(15) %>% 
  mutate(nombre = case_when(stringr::str_detect(descripcion, "CEPILLOS") ~ "Cepillos dentales", 
         stringr::str_detect(descripcion, "COMPLEJO B") ~ "Complejo B",
         stringr::str_detect(descripcion, "METFORMINA") ~ "Metformina",
         stringr::str_detect(descripcion, "CLORURO DE SODIO. SOLUCIÓN INYECTABLE") ~ "Cloruro de sodio inyectable",
         stringr::str_detect(descripcion, "BOTAS") ~ "Bota quirúrgica de tela",
         stringr::str_detect(descripcion, "PINAVERIO") ~ "Tabletas de pinaverio",
         stringr::str_detect(descripcion, "NORMOGOTERO") ~ "Equipo para venoclisis - normogotero",
         stringr::str_detect(descripcion, "DIETA") ~ "Dieta polimérica con fibra",
         stringr::str_detect(descripcion, "JERINGAS") ~ "Jeringas para insulina",
         stringr::str_detect(descripcion, "ACETILSALICILICO") ~ "Ácido acetilsalicílico",
         stringr::str_detect(descripcion, "ESPEJO") ~ "Espejo vaginal",
         stringr::str_detect(descripcion, "HARTMANN") ~ "Solución Hartmann",
         stringr::str_detect(descripcion, "FÓLICO") ~ "Ácido fólico",
         stringr::str_detect(descripcion, "MICROGOTERO") ~ "Equipo para venoclisis - Microgotero",
         stringr::str_detect(descripcion, "CATÉTERES") ~ "Catéteres para oxígeno"))

medi %>% 
  ggplot(aes(x = reorder(nombre, total_faltantes),
             y= total_faltantes)) +
  geom_bar(stat="identity", position="dodge", fill="#fdae61") +
  geom_vline(xintercept = 0)+
  ggfittext::geom_bar_text(outside = T,contrast = T,
                           aes(label=glue::glue("{scales::comma(total_faltantes, accuracy=1)}")))+
  labs(title ="Total de piezas incumplidas, por medicamento",
       subtitle="Desde el 2 de junio al 25 de julio de 2025", x = "",
       y = "Total de piezas",
       caption = stringr::str_wrap(glue::glue("Fuente: Con datos de la Plataforma de Monitoreo de Medicamentos"), 100),
       fill="") +
  scale_y_continuous(labels = scales::label_comma(accuracy = 1))+
  theme_bw()+
  theme(text = element_text(color = "grey35"),
        axis.text.x=element_blank(),
        plot.title = element_text(size = 15, face = "bold",  color = "grey35"),
        plot.subtitle = element_text(size = 10, face = "bold", colour = "#666666"),
        plot.caption = element_text(size = 10),
        legend.position = "none",
        panel.grid = element_blank(),
        panel.border =   element_blank(),
        axis.title = element_text(size = 14, face = "bold"),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank() 
        ) +
  coord_flip()

ggsave(here("out", "insumos_faltantes.png"), width = 11, height = 7, units="in")
