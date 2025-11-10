library(text2vec); library(tokenizers);library(dplyr)
library(janitor);library(stringi);library(stringr)
library(tm);library(tidytext);library(tidyr);library(igraph)
library(matchingR);library(stringdist);library(tm)
library(proxy);library(glue)
library(showtext)
library(ggplot2)
library(tidyverse)
library(forcats)


setwd("C:/Users/Jazmin/Documents/Ciencia de datos/Proyecto")

baseCompleteRaw <- read.csv("respuestas.csv")%>%
  clean_names()

# Limpieza de la base ----------------
names(baseCompleteRaw)

# Se quitan columnas que no se van a utilizar y también se renombran 
# las otras para que sea más fácil trabajar

baseComplete <- baseCompleteRaw%>%
  select(-para_la_primera_cita_jueves,
         -para_la_primera_cita_viernes,
         -columna_12,
         -columna_15,
         -x_tienes_alguna_alergia_o_restriccion_alimentaria,
         -si_tienes_algun_comentario_o_pregunta_escribelo_a_continuacion)%>%
  dplyr::rename(hora = marca_temporal,
                correo = direccion_de_correo_electronico,
                muybien = para_la_primera_cita_perfecto,
                indiferente = para_la_primera_cita_indiferente,
                mal = para_la_primera_cita_para_nada,
                nombre = x_como_te_llamas,
                genero = x_como_te_identificas,
                gustos = x_que_te_gusta,
                busca = x_principalmente_que_estas_buscando,
                lugares = lugares_favoritos_de_la_facultad,
                comentario = escribe_algo_que_le_quieras_decir_a_tu_match_aqui_puedes_poner_cualquier_cosa_por_ejemplo_una_presentacion_sobre_ti_por_que_decidiste_estudiar_en_ciencias_que_te_gustaria_hacer_el_14_de_febrero_si_ya_estas_yendo_a_terapia_v_etc
  )%>%
  # se modifica lo de la fecha y hora
  mutate(hora = as.POSIXlt(hora, format = "%d/%m/%Y %H:%M:%S")) %>%
  mutate(busca = gsub(" >:)","", busca),
         busca = gsub(" :)","", busca),
         busca = gsub(" <3","", busca))%>%
  mutate(grupo = case_when(
    (genero=="Hombre" & gustos == c("Ambos")) ~ "HA",
    (genero=="Hombre" & gustos == c("Hombres")) ~ "HH",
    (genero=="Hombre" & gustos == c("Mujeres")) ~ "HM",
    (genero=="Mujer" & gustos == c("Ambos")) ~ "MA",
    (genero=="Mujer" & gustos == c("Hombres")) ~ "MH",
    (genero=="Mujer" & gustos == c("Mujeres")) ~ "MM"
  ))%>%
  mutate(nombre = gsub("^\\s+|\\s+$", "", nombre))


# Hay personas que modifican varias veces tu correo
baseComplete%>%
  count(correo)%>%
  filter(n>1)


# Se quitan los duplicados y se conserva el registro más reciente
base <- baseComplete%>%
  group_by(correo) %>%
  filter(hora == max(hora)) %>%
  arrange(hora)%>%
  ungroup()%>%
  mutate(id = row_number())



# Análisis exploratorio ----------------
font_add_google(c("Poppins"))
showtext_auto()
tipografia <- "Poppins"
base


# Género de los participantes -----

generos <- base %>%
  count(genero)%>%
  rename("totalGenero" = n)

porcentaje_generos <- generos%>%
  mutate(
    pct = round((100 * totalGenero / sum(totalGenero)), 1),
    etiqueta_pct = paste0(pct, "%"))%>%
  arrange(desc(totalGenero))


porcentaje_generos

ggplot(porcentaje_generos, aes(x = "", y = totalGenero, fill = genero)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#84994F", "#F4CE14")) +
  theme_void(base_family = "Poppins") +
  theme(plot.title = element_text(size=18, family = "Poppins"),
        legend.title = element_blank(),
        legend.text = element_text(size=12, family = "Poppins"))+
  # theme_bw(base_family = "Poppins")+
  labs(title = "Género de los participantes", fill = "Categoría")+
  geom_label(aes(label = etiqueta_pct),
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) 




# Gustos por género -----

gustos_genero <- base %>%
  count(genero, gustos)%>%
  group_by(genero) %>%
  mutate(porcentaje = round(n / sum(n) * 100, 1),
         porcentaje = paste0(porcentaje, "%"))

gustos_genero



# Hobbies --------

split_hobbies <- base%>% 
  separate_rows(hobbies , sep = ", ")

hobbies_por_genero <- split_hobbies%>%
  count(genero, hobbies)%>%
  right_join(generos, by="genero")%>%
  mutate(hobbiePorGenero = round(100 * n / totalGenero, 1))%>%    
  mutate(
    hobbiePorGenero = round(100 * n / totalGenero, 1),   
    hobbies = as_factor(hobbies),
    hobbies = fct_reorder(hobbies, hobbiePorGenero, .desc = TRUE)
  )


hobbies_por_genero

ggplot(hobbies_por_genero, aes(fill=genero, 
                                     y=hobbiePorGenero, 
                                     x=hobbies)) + 
  geom_bar(position="dodge", stat="identity")+
  scale_fill_manual(values = c("#84994F", "#F4CE14")) +
  theme_minimal()+
  theme(
    text = element_text(size=18, family = "Poppins"),
    plot.title = element_text(size=18, family = "Poppins"),
    legend.text = element_text(size=12, family = "Poppins"),
    legend.title = element_blank(),
    axis.title = element_blank()
  )+
  labs(title = "Hobbies", fill = "Categoría")


# Distribución de lugares favoritos por género -----

split_lugares_muybien <- base%>% 
  separate_rows(muybien , sep = ", ")

lugares_muybien_por_genero <- split_lugares_muybien %>%
  count(genero, muybien) %>%                 
  right_join(generos, by = "genero") %>%    
  mutate(
    lugarPorGenero = round(100 * n / totalGenero, 1),   
    muybien = as_factor(muybien),
    muybien = fct_reorder(muybien, lugarPorGenero, .desc = TRUE)
  )

lugares_muybien_por_genero

ggplot(lugares_muybien_por_genero, aes(fill=genero, 
                               y=lugarPorGenero, 
                               x=muybien)) + 
  geom_bar(position="dodge", stat="identity")+
  scale_fill_manual(values = c( "#84994F", "#F4CE14")) +
  theme_minimal()+
  theme(
    text = element_text(size=18, family = "Poppins"),
    plot.title = element_text(size=18, family = "Poppins"),
    legend.text = element_text(size=12, family = "Poppins"),
    legend.title = element_blank(),
    axis.title = element_blank()
  )+
  labs(title = "Lugares preferidos para la primera cita", fill = "Categoría")
