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


# Matrices de distancias -------

# Variables que se usarán
usuarios <- unique(base$id)
n_usuarios <- length(usuarios)

compute_distance_matrix <- function(df, column, distance_function) {
  n <- nrow(df)
  dist_matrix <- matrix(0, n, n)
  
  for (i in 1:n) {
    for (j in 1:n) {
      if (i != j) {
        dist_matrix[i, j] <- distance_function(df[[column]][i], df[[column]][j])
      }
    }
  }
  return(dist_matrix)
}



jaccard_distance <- function(x, y) {
  set_x <- strsplit(x, ",")[[1]]
  set_y <- strsplit(y, ",")[[1]]
  
  intersec <- length(intersect(set_x, set_y))
  union <- length(unique(c(set_x, set_y)))
  
  return(1 - (intersec / union))  # 1 - Jaccard Similarity
}


# Primera cita
distancia_primeraCita <- function(df){
  lugares_distance <- function(x1, x2, y1, y2, z1, z2) {
    set_x1 <- strsplit(x1, ",")[[1]]  # muybien
    set_x2 <- strsplit(x2, ",")[[1]]
    set_z1 <- strsplit(z1, ",")[[1]]  # mal
    set_z2 <- strsplit(z2, ",")[[1]]
    
    intersec_muybien <- length(intersect(set_x1, set_x2))
    intersec_mal <- length(intersect(set_z1, set_z2))
    
    union_muybien <- length(unique(c(set_x1, set_x2)))
    union_mal <- length(unique(c(set_z1, set_z2)))
    
    # Aplicamos la fórmula de distancia ponderada
    score <- (3 * intersec_muybien + 1 * intersec_mal) / (3 * union_muybien + 1 * union_mal)
    
    return(1 - score)  # Convertimos a distancia
  }
  
  n <- nrow(df)
  dist_lugares <- matrix(0, n, n)
  
  for (i in 1:n) {
    for (j in 1:n) {
      if (i != j) {
        dist_lugares[i, j] <- lugares_distance(df$muybien[i], df$muybien[j], df$indiferente[i], df$indiferente[j], df$mal[i], df$mal[j])
      }
    }
  }
  return(dist_lugares)
}
dist_primeraCita <- distancia_primeraCita(base)

# Búsqueda
distancia_busqueda <- function(df){
  
  busca_distance_ordinal <- function(x, y) {
    valores <- c("Algo casual" = -0.5, "Amistad" = 0, "Una relación" = 0.5)
    
    return(abs(valores[x] - valores[y]))
  }
  
  
  dist_busca_ordinal <- compute_distance_matrix(df, "busca", busca_distance_ordinal)
  
  return(dist_busca_ordinal)
}
dist_busqueda <- distancia_busqueda(base)

# Lugares de la facultad
dist_lugaresFac <- compute_distance_matrix(base, "lugares", jaccard_distance)

# Hobbies
dist_hobbies <- compute_distance_matrix(base, "hobbies", jaccard_distance)




get_distancias <- function(pts_busca,
                           pts_hobbies,
                           pts_primeraCita,
                           pts_lugaresFac){
  
  distancias <- as.data.frame((pts_busca * dist_busqueda) +
                                (pts_hobbies * dist_hobbies) +
                                (pts_primeraCita * dist_primeraCita) +
                                (pts_lugaresFac * dist_lugaresFac) 
  )
  row.names(distancias) <- usuarios
  names(distancias) <- usuarios
  return(distancias)
}


get_preferencias <- function(ids_renglones, ids_columnas, distancias){
  
  distancias_seleccionadas <- distancias[ids_renglones, ids_columnas]
  dim(distancias_seleccionadas)
  usuarios <- names(distancias_seleccionadas)
  length(usuarios)
  length(ids_renglones)
  
  distancias_seleccionadas <- distancias_seleccionadas%>%
    mutate(id = ids_renglones)
  preferencias <- data.frame(matrix(NA, nrow = length(ids_renglones), ncol = 0))
  
  
  for (usuario in usuarios) {
    # usuario <- "1"
    
    preferencias_usuario <- distancias_seleccionadas%>%
      select(usuario, id)%>%
      rename(distancia = usuario)%>%
      arrange(distancia)%>%
      select(id)%>%pull()
    
    preferencias <- cbind(preferencias, preferencias_usuario)
    
  }
  
  names(preferencias) <- usuarios
  
  return(preferencias)
}


# Parte 1: Recomendaciones para heterosexuales ----

get_resultados_hetero <- function(base, distancias){
  
  ids_heterosexuales <- base %>%
    filter(
      (genero == "Hombre" & gustos == "Mujeres") | (genero == "Mujer" & gustos == "Hombres") |
        (genero == "Hombre" & gustos == "Ambos") | (genero == "Mujer" & gustos == "Ambos")
    )%>%
    distinct(id, .keep_all = TRUE)%>%distinct(id)%>%pull()
  
  
  ids_hombres_hetero <- base%>%
    filter(id %in% ids_heterosexuales)%>%
    filter(genero == "Hombre")%>%
    pull(id)
  
  
  ids_mujeres_hetero <- base%>%
    filter(id %in% ids_heterosexuales)%>%
    filter(genero == "Mujer")%>%
    pull(id)
  
  
  pref_h_mujeres <- get_preferencias(ids_hombres_hetero,ids_mujeres_hetero, distancias)
  pref_h_hombres <- get_preferencias(ids_mujeres_hetero,ids_hombres_hetero, distancias)
  
  
  # Número de plazas por mujer
  mujeres_slots <- rep(2,length(ids_mujeres_hetero))
  
  resultado_heterosexuales <- galeShapley.collegeAdmissions(pref_h_hombres, pref_h_mujeres, slots = mujeres_slots)
  
  emparejamientos_hombres <- as.data.frame(resultado_heterosexuales$matched.students)%>%
    mutate(hombre = ids_hombres_hetero)%>%
    left_join(as.data.frame(list(mujer = names(pref_h_mujeres)))%>%
                mutate(V1 = row_number()))%>%
    select(hombre, mujer)
  
  emparejamientos_mujeres_heteros <- as.data.frame(resultado_heterosexuales$matched.colleges)%>%
    mutate(mujer = ids_mujeres_hetero)%>%
    left_join(as.data.frame(list(hombre1 = names(pref_h_hombres)))%>%
                mutate(V1 = row_number()))%>%
    left_join(as.data.frame(list(hombre2 = names(pref_h_hombres)))%>%
                mutate(V2 = row_number()))%>%
    select(mujer, hombre1, hombre2)
  
  return(list(emparejamientos_hombres, emparejamientos_mujeres_heteros))
  
}

evaluacion_promedio_distancias <- function(modelo){
  
  
  comparar_resultados_por_id <- function(id, modelo) {
    
    emparejamientos_mujeres_heteros <- modelo[[2]]
    
    # Entre más cerca de uno es mejor ya que es cuánto nivel de porcentaje comparten
    # Por ejemplo si a la mujer le gusta Cocinar y hacer ejercicio y al hombre solo cocinar, tiene .5
    filter_base_emparejamientos_mujeres_heteros <- function(idSeleccionado){
      emparejamientos_mujeres_heteros_filtrado <- emparejamientos_mujeres_heteros%>%
        filter(mujer==idSeleccionado)
      
      base_filtrada <- base%>%
        filter(id %in% c(emparejamientos_mujeres_heteros_filtrado$mujer,
                         emparejamientos_mujeres_heteros_filtrado$hombre1,
                         emparejamientos_mujeres_heteros_filtrado$hombre2))
      
    }
    
    
    
    df = filter_base_emparejamientos_mujeres_heteros(id)
    # Filtrar el registro de referencia
    referencia <- df %>% filter(id == !!id)
    
    if (nrow(referencia) == 0) {
      stop("El ID no se encuentra en el data frame.")
    }
    
    # Columnas a evaluar
    cols <- c("muybien", "mal", "busca", "hobbies", "lugares")
    referencia_listas <- lapply(referencia[cols], function(x) unlist(strsplit(as.character(x), ",")))
    
    # Función para encontrar coincidencias y calcular la proporción
    encontrar_coincidencias <- function(fila) {
      sapply(cols, function(col) {
        valores <- unlist(strsplit(as.character(fila[[col]]), ","))
        compartidos <- valores[valores %in% referencia_listas[[col]]]
        ifelse(length(referencia_listas[[col]]) > 0, length(compartidos) / length(referencia_listas[[col]]), 0)
      })
    }
    
    # Aplicar la función a todas las filas
    coincidencias <- do.call(rbind, lapply(1:nrow(df), function(i) encontrar_coincidencias(df[i, ]))) %>%
      as.data.frame() %>%
      mutate(id = df$id) %>%  # Agregar ID
      filter(id != !!id) %>%  # Quitar la fila de referencia
      select(-id)  # Eliminar columna de ID para promediar
    
    # Calcular promedio por columna
    promedios <- colMeans(coincidencias, na.rm = TRUE) %>% round(2)
    
    # Calcular medianas por columna
    medianas <- apply(coincidencias, 2, median, na.rm = TRUE)%>% round(2)

    
    promedios <- (as.data.frame(t(promedios)))  # Transponer para visualizar mejor
    medianas <- (as.data.frame(t(medianas)))
    
    resultados <- cbind(c("promedios","medianas") , rbind(promedios, medianas))
    return(resultados)
  }
  
  emparejamientos_mujeres_heteros <- modelo[[2]]
  resultados <- data.frame(matrix(ncol = 6, nrow = 0))
  
  for(idSeleccionado in emparejamientos_mujeres_heteros$mujer){
    resultado <- comparar_resultados_por_id(idSeleccionado, modelo)%>%mutate(id=idSeleccionado)
    resultados <- rbind(resultado, resultados)
  }
  
  resultados_finales_promedios <- as.data.frame(list(muybien = mean(resultados$muybien),
                                           mal = mean(resultados$mal),
                                           busca = mean(resultados$busca),
                                           hobbies = mean(resultados$hobbies),
                                           lugares = mean(resultados$lugares)
  ))

  resultados_finales_medianas <- as.data.frame(list(muybien = median(resultados$muybien),
                                           mal = median(resultados$mal),
                                           busca = median(resultados$busca),
                                           hobbies = median(resultados$hobbies),
                                           lugares = median(resultados$lugares)
  ))
  
  resultados_finales <- cbind(c("promedios","medianas") , rbind(resultados_finales_promedios,
                                                                resultados_finales_medianas))
  names(resultados_finales)[1] <- c("estadística")
  return(resultados_finales)
}

resumen_evaluacion <- function(resultados){
  media <- mean(unlist(c(resultados[1,2:5])))
  mediana <- median(unlist(c(resultados[1,2:5])))
  resultados <- cbind(media, mediana)
  # names(resultados) <- c("media","mediana")
  return( resultados )
  
  
}


# # Entre más cerca de 1 mejor
# # Modelo 1 ----
# modelo1 <- get_resultados_hetero(base,
#                                  get_distancias(pts_busca = 0.4,
#                                                 pts_hobbies = 0.3,
#                                                 pts_primeraCita = 0.25,
#                                                 pts_lugaresFac = 0.05))
# 
# resultados_promedios_modelo1 <- evaluacion_promedio_distancias(modelo1)
# 
# 
# # Modelo 2 ----
# modelo2 <- get_resultados_hetero(base,
#                                  get_distancias(pts_busca = 0.3,
#                                                 pts_hobbies = 0.4,
#                                                 pts_primeraCita = 0.25,
#                                                 pts_lugaresFac = 0.05))
# resultados_promedios_modelo2 <- evaluacion_promedio_distancias(modelo2)
# 
# 
# # Modelo 3 ----
# modelo3 <- get_resultados_hetero(base,
#                                  get_distancias(pts_busca = 0.25,
#                                                 pts_hobbies = 0.25,
#                                                 pts_primeraCita = 0.25,
#                                                 pts_lugaresFac = 0.25))
# 
# resultados_promedios_modelo3 <- evaluacion_promedio_distancias(modelo3)
# 
# 
# 
# # Modelo 4 ----
# modelo4 <- get_resultados_hetero(base,
#                                  get_distancias(pts_busca = 0.4,
#                                                 pts_hobbies = 0.4,
#                                                 pts_primeraCita = 0.2,
#                                                 pts_lugaresFac = 0.0))
# 
# resultados_promedios_modelo4 <- evaluacion_promedio_distancias(modelo4)
# 
# 
# 
# # Modelo 5 ----
# modelo5 <- get_resultados_hetero(base,
#                                  get_distancias(pts_busca = 0.3,
#                                                 pts_hobbies = 0.3,
#                                                 pts_primeraCita = 0.2,
#                                                 pts_lugaresFac = 0.2))
# 
# resultados_promedios_modelo5 <- evaluacion_promedio_distancias(modelo5)
# 
# 
# # Modelo 6 ----
# modelo6 <- get_resultados_hetero(base,
#                                  get_distancias(pts_busca = 0.25,
#                                                 pts_hobbies = 0.25,
#                                                 pts_primeraCita = 0.15,
#                                                 pts_lugaresFac = 0.15))
# 
# resultados_promedios_modelo6 <- evaluacion_promedio_distancias(modelo6)
# 
# 
# # Modelo 7 ----
# modelo7 <- get_resultados_hetero(base,
#                                  get_distancias(pts_busca = 0.5,
#                                                 pts_hobbies = 0.3,
#                                                 pts_primeraCita = 0.2,
#                                                 pts_lugaresFac = 0.0))
# 
# resultados_promedios_modelo7 <- evaluacion_promedio_distancias(modelo7)
# 
# # Modelo 8 ----
# modelo8 <- get_resultados_hetero(base,
#                                  get_distancias(pts_busca = 0.45,
#                                                 pts_hobbies = 0.35,
#                                                 pts_primeraCita = 0.15,
#                                                 pts_lugaresFac = 0.05))
# 
# resultados_promedios_modelo8 <- evaluacion_promedio_distancias(modelo8)
# 
# 
# # Modelo 9 ----
# modelo9 <- get_resultados_hetero(base,
#                                  get_distancias(pts_busca = 0.35,
#                                                 pts_hobbies = 0.35,
#                                                 pts_primeraCita = 0.25,
#                                                 pts_lugaresFac = 0.05))
# 
# resultados_promedios_modelo9 <- evaluacion_promedio_distancias(modelo9)
# 
# 
# # Modelo 10 ----
# modelo10 <- get_resultados_hetero(base,
#                                  get_distancias(pts_busca = 0.4,
#                                                 pts_hobbies = 0.3,
#                                                 pts_primeraCita = 0.15,
#                                                 pts_lugaresFac = 0.15))
# 
# resultados_promedios_modelo10 <- evaluacion_promedio_distancias(modelo10)
# 
# 
# # Resumen de los resultados
# for (i in 1:10) {
#   objeto <- get(paste0("resultados_promedios_modelo", i))
#   resumen <- resumen_evaluacion(objeto)
#   print(paste("Resumen del modelo", i))
#   print(resumen)
# }
# Se conserva el modelo 10 por lo que ocupamos esas distancias para todos

distancias <- get_distancias(pts_busca = 0.4,
                             pts_hobbies = 0.3,
                             pts_primeraCita = 0.15,
                             pts_lugaresFac = 0.15)

modelo_heteros <- get_resultados_hetero(base, distancias)


# Se hacen las pruebas de los heteros

filter_base_emparejamientos_mujeres_heteros <- function(idSeleccionado){
  emparejamientos_mujeres_heteros_filtrado <- emparejamientos_mujeres_heteros%>%
    filter(mujer==idSeleccionado)
  
  base_filtrada <- base%>%
    filter(id %in% c(emparejamientos_mujeres_heteros_filtrado$mujer,
                     emparejamientos_mujeres_heteros_filtrado$hombre1,
                     emparejamientos_mujeres_heteros_filtrado$hombre2))
  
  return(base_filtrada)
}
emparejamientos_mujeres_heteros <- modelo_heteros[[2]]

m <- sample(emparejamientos_mujeres_heteros$mujer, 1)
matches <- filter_base_emparejamientos_mujeres_heteros(m)



# Parte 2: Recomendaciones para homosexuales  ----
# Se usa el algoritmo de GaleyShapley con la modificación de roomies

get_resultados_homo <- function(base_filtrada, distancias){
  
  # Tenemos que tener nuevos ids
  # Este paso es fundamental porque sino, no funciona el algoritmo
  base_filtrada <- base_filtrada%>%
    mutate(new_id = row_number())
  
  diccionario_ids <- base_filtrada%>%
    select(id, new_id)
  
  
  ids <- base_filtrada%>%distinct(id)%>%pull()
  
  
  distancias_seleccionadas <- as.matrix(distancias[ids, ids])

  m <- distancias_seleccionadas
  m_sin_diag <- m[col(m) != row(m)]
  dim(m_sin_diag) <- c(nrow(m) - 1, ncol(m))
  m_sin_diag


  preemparejamientos_homosexuales = matchingR::roommate(utils = m_sin_diag)
  preemparejamientos_homosexuales
  
  # Reemplazar valores en 'resultado_homosexuales' usando los id originales
  emparejamientos_homosexuales <- as.data.frame(preemparejamientos_homosexuales)%>%
    rename(new_id = V1)%>%left_join(diccionario_ids)%>%
    mutate(persona1 = diccionario_ids$id)%>%
    rename(persona2 = id)%>%
    select(persona1, persona2)
  
  
  return(emparejamientos_homosexuales)
  
}

# Modelo para las lesbianas ----
lesbianas <- base %>%
  filter((genero == "Mujer" & gustos == "Mujeres") |
           (genero == "Mujer" & gustos == "Ambos"))

emparejamientos_lesbianas <- get_resultados_homo(lesbianas, distancias)


# Modelo para los gays ----
gays <- base %>%
  filter((genero == "Hombre" & gustos == "Hombres") |
           (genero == "Hombre" & gustos == "Ambos"))

emparejamientos_gays <- get_resultados_homo(gays, distancias)
