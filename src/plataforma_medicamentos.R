pacman::p_load(tidyverse, here,ggridges,readxl, ggrepel, sf)

require(geofacet)
source(here::here("src","clave_estados.R"))

# instituciones -----------------------------------------------------------


barras <- conestados %>% 
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
       subtitle="Desde el 2 de junio al 28 de julio de 2025", x = "",
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
       subtitle="Del 2 de junio al 28 de julio de 2025", x = "",
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



# estatus total -----------------------------------------------------------

barras <- conestados %>% 
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
       subtitle="Desde el 2 de junio al 28 de julio de 2025", x = "",
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
       subtitle="Del 2 de junio al 28 de julio de 2025", x = "",
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


ggsave(here("out", "mapa.png"), width = 12, height = 8, units="in")



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
       subtitle="Del 2 de junio al 18 de julio de 2025", x = "Total de días",
       y = "",
       caption = stringr::str_wrap(glue::glue("Fuente: Con datos de la Plataforma de Monitoreo de Medicamentos"), 100),
       fill="") +
  xlim(0, 35)+
  theme_ridges() +
  theme(text = element_text(color = "grey35"),
        plot.title = element_text(size = 15, face = "bold",  color = "grey35"),
        plot.subtitle = element_text(size = 10, face = "bold", colour = "#666666"),
        axis.text.x = element_text(face="bold",  color = "grey35"),
        axis.text.y = element_text(face="bold",  color = "grey35"))


# observatorio desabasto --------------------------------------------------

obs <- read_csv(("Downloads/csv(1).csv"))

enfermedad <- read_csv("Downloads/csv(2).csv")


# diferencias en dias por institucion -------------------------------------

# 
# c_25 <- conestados %>% 
#   mutate(fecha_solicitud=lubridate::ymd(fecha_solicitud), 
#          fecha_entrega=lubridate::ymd(fecha_entrega), 
#          fecha_corte=lubridate::ymd("2025-07-25"), 
#          dias= case_when(
#            is.na(fecha_entrega)  ~ fecha_corte-fecha_solicitud,
#            !is.na(fecha_entrega)  ~ fecha_entrega-fecha_solicitud), 
#          dias=as.numeric(dias)) %>% 
#   filter(dias>0)
# 
# 
# c_25 %>% 
#   ggplot( aes(x=institucion, y=dias, fill=institucion)) +
#   geom_violin(width=1.4) +
#   # geom_jitter(height = 0, width = 0.1)
#   geom_boxplot(width=0.1, color="grey", alpha=0.2) +
#   scale_fill_manual(values=c("#005835", "#24893D","#8A1538")) +
#   labs(title =stringr::str_wrap(glue::glue("Distribución de la diferencia de días entre el pedido y la entrega de medicamento, por institución"), 100),
#        subtitle="Desde el 2 de junio al 18 de julio de 2025", x = "",
#        y = "Días",
#        caption = stringr::str_wrap(glue::glue("Fuente: Con datos de la Plataforma de Monitoreo de Medicamentos"), 100),
#        fill="") +
#   scale_y_continuous(labels = scales::label_comma(accuracy = 1), limits = c(0,48)) +
#   theme_bw()+
#   theme(text = element_text(color = "grey35"),
#         plot.title = element_text(size = 15, face = "bold",  color = "grey35"),
#         plot.subtitle = element_text(size = 10, face = "bold", colour = "#666666"),
#         plot.caption = element_text(size = 10),
#         legend.position = "none",
#         panel.grid = element_blank(),
#         panel.border =   element_blank(),
#         axis.title = element_text(size = 14, face = "bold"),
#         axis.ticks.y=element_blank(),
#         axis.text = element_text(size = 10, face = "bold"))



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
  mutate(total=20260,
         porc_incum=round((n/total)*100,2)) %>% 
  select(proveedor, porc_incum)


scatplot <- left_join(prove_inclum, prove_porc)

scatplot %>% 
  ggplot( aes(x = porc_incum, y =porc_claves)) +
  geom_point(alpha = .8, color = "#006837", size=3) +
  theme_bw()+
  geom_abline(slope=1, intercept = 0, color="red")+
  xlim(0, 9)+
  ylim(0, 9)+
  labs(title="Porcentaje de claves por proveedor vs. \nporcentaje de claves incomplidas por proveedor", 
     x= "% claves incumplidas por proveedor", 
     y= "% claves por proveedor")+  
  theme_bw()+
  theme(text = element_text(color = "grey35"),
        plot.title = element_text(size = 15, face = "bold",  color = "grey35"),
        plot.subtitle = element_text(size = 10, face = "bold", colour = "#666666"),
        axis.text.x = element_text(face="bold",  color = "grey35"),
        axis.text.y = element_text(face="bold",  color = "grey35"))


# tiempo y claves ---------------------------------------------------------


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
  )



prove_tiempo %>% 
  filter(proveedor!="LABORATORIOS PISA SA DE CV") %>% 
  mutate(proveedor = stringr::str_to_lower(proveedor),
    proveedor = stringr::str_to_title(proveedor)) %>% 
ggplot() +
  geom_point(aes(x = total_claves, y = prom_dias, color = es_outlier),
    size = 3,
             position = position_jitter(width = 0.1, height = 0)) +
  scale_color_manual(values=c("#ffffb2", "#fd8d3c"))+
  # Etiquetas solo para proveedores lentos y relevantes
  geom_point(
    data = filter(prove_tiempo, marcar_proveedor==T),
    aes(x = total_claves, y = prom_dias, color=marcar_proveedor),  size = 3,
    position = position_jitter(width = 0.1, height = 0), color="#bd0026") +
  # geom_text_repel(
  #   data = prove_tiempo %>% 
  #     filter(marcar_proveedor==T)%>% 
  #     mutate(proveedor = stringr::str_to_lower(proveedor),
  #            proveedor = stringr::str_to_title(proveedor)),
  #   aes(x = total_claves, y = prom_dias,label = stringr::str_wrap(proveedor, 35)),
  #   size = 3, nudge_y = 1, segment.size = 0.2
  # )+

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
  scale_x_log10() +
  labs(title="Porcentaje de claves de cada proveedor vs. \nTiempo de entrega", 
       x= "Total de claves del proveedor (escala logarítmica)", 
       y= "Tiempo promedio de entrega (dias)")+  
  theme_bw()+
  theme(text = element_text(color = "grey35"),
        plot.title = element_text(size = 15, face = "bold",  color = "grey35"),
        plot.subtitle = element_text(size = 10, face = "bold", colour = "#666666"),
        axis.text.x = element_text(face="bold",  color = "grey35"),
        axis.text.y = element_text(face="bold",  color = "grey35"))

ggsave(here("out", "funnel_plot.png"), width = 15, height = 7, units="in")

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

t <- conestados %>% 
  group_by(proveedor) %>% 
  summarise(n=n()) %>% 
  ungroup() %>% 
  filter(!is.na(proveedor)) %>% 
  mutate(pct=round((n/225989)*100, 2)) %>% 
  arrange(-pct) %>% 
  mutate(acumulado = cumsum(pct))
  

# medicamentos ------------------------------------------------------------

medi <- read_excel("Downloads/entregas(1).xlsx") %>% 
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
  arrange(-total_faltantes)
# %>% 
#   arrange(-total_faltantes)%>% 
#     head(10)
# 
#   write_csv(medi, "Downloads/muestra_piezas_faltantes.csv")


demanda <- read_excel("Downloads/entregas(1).xlsx") %>% 
  janitor::clean_names() %>% 
  group_by(descripcion) %>% 
  summarise(total_demanda=sum(piezas_solicitadas, na.rm=T)) %>% 
  ungroup() %>% 
  arrange(-total_demanda) %>% 
  
  
  
  demanda %>% 
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
       subtitle="Desde el 2 de junio al 18 de julio de 2025", x = "",
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


# muestra <- read_excel("Downloads/entregas.xlsx") %>% 
#   janitor::clean_names() %>% 
#   head(100)
# 
# write_csv(muestra, "Downloads/muestra.csv")






