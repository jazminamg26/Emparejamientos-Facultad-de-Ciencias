library(text2vec); library(tokenizers);library(dplyr)
library(janitor);library(stringi);library(stringr)
library(tm);library(tidytext);library(tidyr);library(igraph)
library(matchingR);library(stringdist);library(tm)
library(proxy);library(glue)

setwd("C:/Users/Jazmin/Documents/Ciencia de datos/Proyecto")

baseCompleteRaw <- read.csv("respuestas.csv")%>%
  clean_names()

names(baseCompleteRaw)

# Se quitan columnas que no se van a utilizar y también se renombran las otras para que sea más fácil trabajar
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
    (genero=="Hombre" & gustos == c("Ambos")) ~ "Ha",
    (genero=="Hombre" & gustos == c("Hombres")) ~ "Hh",
    (genero=="Hombre" & gustos == c("Mujeres")) ~ "Hm",
    (genero=="Mujer" & gustos == c("Ambos")) ~ "Ma",
    (genero=="Mujer" & gustos == c("Hombres")) ~ "Mh",
    (genero=="Mujer" & gustos == c("Mujeres")) ~ "Mm"
  ))%>%
  mutate(nombre = gsub("^\\s+|\\s+$", "", nombre))


# Hay personas que modifican varias veces tu correo
baseComplete%>%
  count(correo)%>%
  filter(n>1)


# Se quitan los duplicados y se conserva el registro más reciente
prebase <- baseComplete%>%
  group_by(correo) %>%
  filter(hora == max(hora)) %>%
  arrange(hora)%>%
  ungroup()%>%
  mutate(id = row_number())%>%
  mutate(comentario_original = comentario,
         comentario = str_to_lower(comentario))

library(dplyr)
library(tidyr)
library(stringr)

library(dplyr)
library(tidyr)
library(stringr)

library(dplyr)
library(tidyr)
library(stringr)

separa_valores_columna <- function(df, columna) {
  columna_sym <- rlang::ensym(columna)  # convierte a símbolo para tidy eval
  
  df %>%
    # Asegurarse de que la columna sea character
    mutate(!!columna_sym := as.character(!!columna_sym)) %>%
    
    # Separar los valores por coma en múltiples filas
    separate_rows(!!columna_sym, sep = ",") %>%
    
    # Quitar espacios al inicio y final
    mutate(!!columna_sym := str_trim(!!columna_sym)) %>%
    
    # Crear columna id consecutiva
    mutate(id = row_number()) %>%
    
    # Seleccionar columnas en orden: id y columna original
    select(id, !!columna_sym)
}


muybien <- separa_valores_columna(prebase, "muybien")



# Analisis de texto ----

# Función para obtener las palabras más repetidas en los textos
frecuencia_palabras <- function(df, columna_texto, palabrasInutiles) {
  # Extraer los textos
  textos <- df[[columna_texto]]

  # Tokenizar textos
  tokens <- tokenize_words(textos)

  # Filtrar stopwords en español y palabras inútiles personalizadas
  palabras_stopwords <- stopwords::stopwords("es")  # Stopwords en español
  palabras_a_filtrar <- unique(c(palabras_stopwords, palabrasInutiles))  # Unimos ambas listas

  tokens_filtrados <- lapply(tokens, function(palabras) {
    palabras[!(palabras %in% palabras_a_filtrar)]  # Quitamos palabras no deseadas
  })

  # Convertir lista de tokens en un vector de palabras
  palabras <- unlist(tokens_filtrados)

  # Contar frecuencia de palabras
  frecuencia <- table(palabras)

  # Convertir en data frame y ordenar de mayor a menor frecuencia
  palabras_df <- as.data.frame(frecuencia, stringsAsFactors = FALSE) %>%
    rename(Palabra = palabras, Frecuencia = Freq) %>%
    arrange(desc(Frecuencia))

  # Devolver las N palabras más frecuentes
  return(palabras_df)
}

reemplazos <- c(
  "gustan" = "gustar", "gustaria" = "gustar", "gustos" = "gustar",  "gusto" = "gustar",
  "encanta" = "gustar",
  "risueño" = "reir",
  "alcoholizado" = "bar",
  "gustaa" = "gustar","gustamos"="gustar", "gustas" ="gustar","gustes"="gustar",
  " gusta " = " gustar ",
  "gustitos"="gustar",
  "pq" = "porque",
  " p d " = " posdata ",
  "favorita" = "favorito",
  "febrero" = "14",  "valentin" = "14",
  "facultad" = "fac",
  "tranquila" = "tranquilo",
  "holaaaaap" = "hola",
  " q " = " ",
  " 1 " = " ", " 2 " = " ", " 3 " = " ", " dos " = " ",
  "holaa" = "hola",
  "juegos" = "jugar", "juego" = "jugar",
  "deportes" = "deporte",
  "puede " = "poder ", "puedo " = "poder ", "podemos " = "poder ", "puedes" = "poder", "pueda "="poder ",
  "actuario" = "actuaria",
  "amigos" = "amistad", "amistades" = "amistad",
  "buenas" = "buen", "buena" = "buen", "bueno" = "buen",
  "buscando" = "buscar", "busco"="buscar",
  "ciencia " = "ciencias ",
  "conocernos" = "conocer",
  "encantan" = "encantar", "encantaria" = "encantar",
  "estudiando" = "estudiar", "estudiante" = "estudiar", "estudio" = "estudiar",
  "fisica" = "fisico",
  "hace " = "hacer ", "hago " = "hacer ",  "haciendo " = "hacer ", "hecho" = "hacer",
  "lugares" = "lugar",
  "personas" = "persona",
  "pasarla " = "pasar ",
  "nuevas" = "nuevos", "nueva" = "nuevos", "nuevo " = "nuevos ",
  "matematico" = "matematicas", "mates" = "matematicas",
  "jajaja" = "jaja", "jajaj"="jaja", "jeje" = "jaja","jajjs" = "jaja","jajs"  = "jaja",
  "jajsjaja"  = "jaja", "jajsjaka" = "jaja","jajsk" = "jaja","jaksjakak" = "jaja",
  "jakskskskks" = "jaja", "ajajjaja" = "jaja", "ajasjasjas" = "jaja", "ahshd"  = "jaja", "ajaj" = "jaja",
  "favoritos" = "favorito",
  "planes" = "plan",
  "introvertida" = "introvertido",
  "holi" = "hola",
  " anos " = " año ",  " anitos " = " año ", "anos60"  = "año", "anos84"   = "año",
  "risas" = "reir",  "chistosas"  = "reir", "chistosita" = "reir","chistoson"  = "reir",
  "disfruto" = "disfrutar",
  " amo " = " amor ",
  "abierta" = "abierto",
  " alguna " = " algun ",
  "escucho" = "escuchar",
  "relaciones" = "relacion",
  "trabajando" = "trabajar", "trabajo" = "trabajar",
  "cosas" = "cosa",
  "tranquilos" = "tranquilo", " tranqui " = " tranquilo ",
  "momentos" = "momento",
  "libros" = "leer",
  "quisiera" = "querer", "quiero" = "querer","quieres" = "querer",
  "hablo" = "hablar",
  " toco " = " tocar ",
  "dicen" = "decir",
  "computologo" = "computacion",
  "bonita" = "bonito",
  "encuentro" = "encontrar",
  "conozco" = "conocer",
  "tendre" = "tener", "tendria" = "tener", "tendrias" = "tener", "tenerla" = "tener", "tenganme" = "tener",
  "teorias" = "teoria", "teorica" = "teoria",
  "terrologo" = "tierra",
  "agradaba" = "agrada", "agradaria" = "agrada", "agradas" = "agrada", "agradeceria" = "agrada", "agrademos" = "agrada",
  " ah " = " ahhh ", " ahhh " = " ahhh ",
  "acabarlo" = "acabar",
  "academicamente" = "academico", "academico" = "academico",
  "activamente" = "activo",
  "afectuosa" = "afectuoso",
  "acuerdos" = "acuerdo",
  "baja" = "bajo",
  "chicos" = "chico",
  "gatos" = "gato", "gatitos"="gato", "gatas"="gato",
  "acercamiento" = "acercar", "acerquen"= "acercar",
  "alcanzo" = "alcanzar",
  "agradaba" = "agradar", "agradas" = "agradar",
  "animarme" = "animo", "animas" = "animo", "animate" = "animo", "animico" = "animo", "animos" = "animo",
  "alimenta" = "alimentos", "alimenticios" = "alimentos",
  "amigar" = "amistad", "amiguitos" = "amistad", "amigxs" = "amistad", "amixes" = "amistad",
  "aprovechan" = "aprovechar", "aprovechar" = "aprovechar", "aprovecharlo" = "aprovechar",
  "atractivo" = "atraer", " atrae " =  " atraer ", "atraemos"  = "atraer", "atraen" = "atraer",
  "asustan" = "asusta",
  "chistes" = "reir", "divertidos" = "reir", "divertimento"  = "reir", "divertirte" = "reir",
  "diversion"  = "reir",
  "drugs" = "droga","fentanilo"="droga","thinner"="droga",
  "romantic" = "romancito", "romantica"= "romancito",
  "flora" ="flores" ,"floressssss"="flores",
  "respetuoso" = "respetar", "respetemos" = "respetar",
  "recomendaciones" = "recomendar", "recomendado" = "recomendar", "recomiendo" = "recomendar",
  "comerme" = "comer",
  "buscarle" = "buscar", "buscarse" = "buscar", "busque" = "buscar",
  "caballerosidad" = "caballero", "caballerosos" = "caballero" ,
  "aceptado" = "aceptar", "acepten" = "aceptar",
  "chidas" = "chido", "chidito" = "chido","chevere" = "chido",
  "chismoso" = "chismosa",
  "cinefilo" = "cinefila",
  "comidas"  = "comida",
  "coneja" = "conejo",
  "catolica" = "dios", "cristiano" = "dios" , "cristo" = "dios",
  "doctora"="doctor", "doctorado"="doctor", "dogtora"="doctor",
  "enamoradisimo"="enamorar", "enamorado"="enamorar","enamorarme"="enamorar",
  "entendido"="entender","entendiste"="entender", "entiendes"="entender", "entiendo" ="entender",
  "estupida"="estupido", "estupideces" ="estupido",
  "expresarlo"  = "expresar", "expresiones" = "expresar" ,
  "hablamos" = "hablar", "hablamossss" = "hablar","hablare" = "hablar","hable"  = "hablar",
  "hoal" = "hola", "holap" = "hola","holaspersona" = "hola","holass" = "hola","holo" = "hola","holooo"   = "hola",
  "importancia" ="importar","importantes"  ="importar", "importe" ="importar", "importo"="importar",
  "llevan" = "llevar" ,"llevando" = "llevar","llevaremos"= "llevar","llevarlo"= "llevar" ,
  "llevarnos"  = "llevar" ,"llevarte"= "llevar",
  "aburrido" = "aburrir", "aburriras" = "aburrir", "aburrirme"= "aburrir",
  "acaricie"  = "acariciar","acaricio"= "acariciar",
  "autentica" = "autentico",
  "ayudaaaaaa" ="ayudar","ayudado"="ayudar","ayudo" ="ayudar",
  "auqnue"  = "aunque",
  "catorce" = "14",
  "comoda"="comodidad","comodos"="comodidad",
  "compartamos"= "compartir", "comparten"= "compartir",
  "compartes"= "compartir", "compartieras"= "compartir",
  "compartimos"= "compartir",
  "conocemos"="conocer","conocera"="conocer", "conocerme"="conocer",
  "conocerse"="conocer", "conoces"="conocer", "conoci" ="conocer",
  "conozca"="conocer", "conozcan"="conocer",
  "criticas"="criticar","criticona"="criticar",
  "escucha"="escuchar","escuchandote"="escuchar",
  "escucharla"="escuchar","escuche"="escuchar",
  "estudiaba"="estudiar","estudiara"="estudiar","estudias" ="estudiar",
  "chambeador"="trabajar","chambiar"="trabajar",
  "chismear"="chisme", "chismesito"="chisme",
  "computlogo"="computacion","computo"="computacion",
  "cuidando"="cuidar", "cuide"="cuidar",
  "mascotas" = "animales",
  "deberian"="deber", "debido"  ="deber","debo"="deber",
  "decides"="decidir","decidimos"="decidir",
  "decirle" ="decir","decirme"="decir",
  "dibuja"="dibujar","dibujando"="dibujar", "dibujarr"="dibujar",
  "disciplinada"="disciplina",    "disciplinado"="disciplina",
  "tristes" = "tristeza", "triste" = "tristeza",
  "titulacion" = "titulo", "titulada"= "titulo", "titulandome"= "titulo", "titularme" = "titulo",
  "ravenclaw" = "HarryPotter", "hufflepuff" = "HarryPotter",
  "ghostee" = "ghost"
)

palabrasInutiles <- c("")
regex_palabras <- paste0("\\b(", paste(palabrasInutiles, collapse = "|"), ")\\b")

PocasPalabras <- prebase%>%
  mutate(word_count = str_count(comentario, "\\S+"))%>%
  filter(word_count < 10 |
           grepl("busco una conejita o conejito",comentario)|
           grepl("Me gusta mucho la naturaleza, andar en moto y entrenar, tengo 23",comentario_original)|
           grepl("tacos de sal",comentario)|
           grepl("……………………………",comentario))%>%
  select(id, comentario, busca, genero)

NoCiencias <- prebase%>%
  filter(grepl("no soy de ciencias",comentario) | grepl("no estudio en ciencias",comentario)| # No de ciencias
           grepl("estudio en la facultad de ingeniería",comentario) | # Ingenieros
           grepl("vet",comentario) | # Veterinarios
           grepl("odonto",comentario) | # Odonto
           grepl("qfb",comentario) | # Odonto
           grepl("estudio derecho",comentario) | # Odonto
           grepl("soy estudiante de economia ",comentario)
  )%>%
  filter(genero == "Hombre")%>%
  select(id, comentario, busca, genero)

# Limpieza de la base -----
prebaseClean <- prebase%>%
  filter(!(id %in% c(PocasPalabras$id, NoCiencias$id)))%>%
  mutate(comentario = gsub("&", " y ", comentario),
         comentario = gsub(":)", " caritafeliz ", comentario),
         comentario = gsub("\\(:", " caritafeliz ", comentario),
         comentario = gsub("c:", " caritafeliz ", comentario),
         comentario = gsub(":D", " caritafeliz ", comentario),
         comentario = gsub(":c", " caritatriste ", comentario),
         comentario = gsub(":\\(", " caritatriste ", comentario),
         comentario = gsub("):", " caritatriste ", comentario),
         comentario = gsub(":v", "xd", comentario),
         comentario = gsub(":'v", "", comentario)
         )%>%
  mutate(
         comentario = stri_trans_general(str = comentario, id = "Latin-ASCII"), #reemplaza acentos
         comentario = gsub("\\s+", " ", comentario), # quita espacios dobles
         comentario = gsub("[\r\n]", " ", comentario), # quita saltos de linea
         comentario = gsub('[[:punct:] ]+',' ',comentario), # quita puntuación
         comentario = gsub("^\\s+|\\s+$", "", comentario))%>%# quita espacios al inicio o final
  mutate(comentario_clean = tm::removeWords(comentario, stopwords("spanish")))%>%# quita las stop words
  mutate(comentario_clean = str_replace_all(comentario_clean, reemplazos))%>%
  mutate(comentario_clean = gsub("[^\x01-\x7F]", " ", comentario_clean)) # quita emojis u otros caracteres especiales)

# Para ver qué palabras se repiten más
top_palabras <- head(frecuencia_palabras(prebaseClean, "comentario_clean", c("")),100)
palabras_menos_usadas <- frecuencia_palabras(prebaseClean, "comentario_clean", c(""))%>%filter(Frecuencia < 2)

palabrasInutiles <- c(palabras_menos_usadas$Palabra, "gustar",
                      "si","estudiar","mas","conocer","hacer","persona",
                      "cosa","ir","alguien","tambien","poder","asi","bien",
                      "buen","gustarn","jaja","salir","hola","nuevos","buscar",
                      "querer","pues","solo",
                      "lugar","siempre","aunque","carrera","escucharr","hablar","ser","favorito","gente","pasar",
                      "relacion","tiempo","creo","platicar","tener","cualquier","disfrutar",
                      "compartir","considero","veces","general","aqui","decidi","espero","nunca",
                      "verdad","tipo","ademas","estudie","aprender","estaria","poner","ahi","momento","yendo",
                      "ahora","bastante","igual","raro","mismo","muchas","siento","despues","importante",
                      "tan","temas","agrada","tampoco","conmigo","simplemente","ando",
                      "intentar","interesante","libre","vemos","ando","vivo","entonces","manera",
                      "mientras","pasa", "hice","demasiado","quiera","pase", "embargo", "tmb", "diria",
                      "seria" ,"holas", "parecer","holaa"  ," p " ," m ", " d ", "etc", "cada", "realmenre", "va", "dar", "serio",
                      "dia", "atencion","tal")
regex_palabras <- paste0("\\b(", paste(palabrasInutiles, collapse = "|"), ")\\b")

# Creación de la base -----
base <- prebaseClean%>%
  mutate(comentario_clean = str_remove_all(comentario_clean, regex_palabras))%>%
  mutate(comentario_clean = gsub("\\s+", " ", comentario_clean), # quita espacios dobles
         comentario_clean = gsub("^\\s+|\\s+$", "", comentario_clean))%>%# quita espacios al inicio o final
  mutate(id = row_number())

palabras_base <- frecuencia_palabras(base, "comentario_clean", c(""))
summary(palabras_base$Frecuencia)
top_palabras <- head(palabras_base,200)
palabras_menos_usadas <- palabras_base%>%filter(Frecuencia == 1)

top_palabras
palabras_menos_usadas

# Variables que se usarán
usuarios <- unique(base$id)
n_usuarios <- length(usuarios)


# Pruebas para ver la limpieza de texto
comens <- base[sample(1:n_usuarios, 1),]%>%
  select(comentario_original, comentario_clean)
comens$comentario_original
comens$comentario_clean

# Distancias en texto ----
get_distancia_coseno <- function(base){
  # Crear una matriz de términos (TF-IDF)
  corpus <- Corpus(VectorSource(base$comentario_clean))
  dtm <- DocumentTermMatrix(corpus, control = list(weighting = weightTfIdf))

  # Calcular distancia del coseno
  distancia_coseno <- dist(as.matrix(dtm), method = "cosine")
  distancia_coseno <- as.matrix(distancia_coseno)
  return(distancia_coseno)
}

distancia_levenshtein <- stringdistmatrix(base$comentario_clean, base$comentario_clean, method = "lv")
distancia_jaccard <- stringdistmatrix(base$comentario_clean, base$comentario_clean, method = "jaccard")
distancia_coseno <- get_distancia_coseno(base)


levenshtein <- as.data.frame(distancia_levenshtein)
names(levenshtein) <- usuarios
levenshtein <- levenshtein%>%mutate(id = row_number())

jaccard <- as.data.frame(distancia_jaccard)
names(jaccard) <- usuarios
jaccard <- jaccard%>%
  mutate(id = row_number())

coseno <- as.data.frame(distancia_coseno)
names(coseno) <- usuarios
coseno <- coseno%>%
  mutate(id = row_number())


pruebaDistanciasTexto <- function(){

  u <- sample(usuarios, 1)
  print(paste("usuario:", u))

  user_levenshtein <- levenshtein%>%
    select(u, id)
  names(user_levenshtein) <- c("distancias_levenshtein" , "id")

  user_jaccard <- jaccard%>%
    select(u, id)
  names(user_jaccard) <- c("distancias_jaccard" , "id")

  user_coseno <- coseno%>%
    select(u, id)
  names(user_coseno) <- c("distancias_coseno" , "id")


  distancias_texto <- user_levenshtein%>%
    left_join(user_jaccard)%>%
    left_join(user_coseno)

  nrow(distancias_texto)
  names(distancias_texto)

  similares_jaccard <- distancias_texto%>%
    arrange(distancias_jaccard)

  similares_levenshtein<- distancias_texto%>%
    arrange(distancias_levenshtein)

  similares_coseno<- distancias_texto%>%
    arrange(distancias_coseno)

  ids_similares_jaccard <- head(similares_jaccard, 2)%>%pull(id)
  ids_similares_levenshtein <- head(similares_levenshtein, 2)%>%pull(id)
  ids_similares_coseno <- head(similares_coseno, 2)%>%pull(id)

  resultado_texto_jaccard <- base%>%
    filter(id %in% c(ids_similares_jaccard))%>%
    select(comentario_original, id)%>%
    mutate(tipo = "jaccard")

  resultado_texto_levenshtein <- base%>%
    filter(id %in% c(ids_similares_levenshtein))%>%
    select(comentario_original, id)%>%
    mutate(tipo = "levenshtein")

  resultado_texto_coseno <- base%>%
    filter(id %in% c(ids_similares_coseno))%>%
    select(comentario_original, id)%>%
    mutate(tipo = "coseno")

  resultados <- rbind(resultado_texto_jaccard,
                      resultado_texto_levenshtein,
                      resultado_texto_coseno)

  return(resultados)
}
lol <- pruebaDistanciasTexto()
# se elige la jaccard

# Obtener distancias ----
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

# Entre el texto
dist_texto <- distancia_jaccard

# Se hacen pruebas
# p <- sample(1:n_usuarios, 1)
#
# as.data.frame(list(distancia = dist_hobbies[, p],
#                    id = 1:n_usuarios))%>%
#   arrange(distancia, id)%>%
#   head(5)%>%
#   left_join(base)%>%
#   select(hobbies, id, nombre)
# Funciona bien

# Todas las distancias estan entre cero y uno

# Se le puede dar distintos pesos a cada variable y después ver cuál es la mejor ponderación
# load("casi.Rdata") # AQUI

get_distancias <- function(pts_busca,
                           pts_hobbies,
                           pts_primeraCita,
                           pts_lugaresFac,
                           pts_texto){

  distancias <- as.data.frame((pts_busca * dist_busqueda) +
                                (pts_hobbies * dist_hobbies) +
                                (pts_primeraCita * dist_primeraCita) +
                                (pts_lugaresFac * dist_lugaresFac) +
                                (pts_texto * dist_texto)
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
# Se usa el algoritmo de GaleyShapley con la modificación estudiantes - escuelas

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

# Evaluación del modelo en heteros -----

evaluacion <- function(modelo){


  comparar_resultados_por_id <- function(id, modelo) {

    emparejamientos_mujeres_heteros <- modelo[[2]]

    # Entre más cerca de uno es mejor ya que es cuánto nivel de porcentaje comparten
    # Por ejemplo si le gusta Cocinar y hacer ejercicio y al hombre solo cocinar, tiene .5
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

    return(as.data.frame(t(promedios)))  # Transponer para visualizar mejor
  }

  emparejamientos_mujeres_heteros <- modelo[[2]]
  resultados <- data.frame(matrix(ncol = 6, nrow = 0))

  for(idSeleccionado in emparejamientos_mujeres_heteros$mujer){
    resultado <- comparar_resultados_por_id(idSeleccionado, modelo)%>%mutate(id=idSeleccionado)
    resultados <- rbind(resultado, resultados)
  }

  resultados_finales <- as.data.frame(list(muybien = mean(resultados$muybien),
                                           mal = mean(resultados$mal),
                                           busca = mean(resultados$busca),
                                           hobbies = mean(resultados$hobbies),
                                           lugares = mean(resultados$lugares)
  ))

  return(resultados_finales)
}

# Modelos y evaluación -----

# Modelo 1 ----
modelo1 <- get_resultados_hetero(base,
                                 get_distancias(pts_busca = 0.3,
                                                pts_hobbies = 0.3,
                                                pts_primeraCita = 0.15,
                                                pts_lugaresFac = 0.05,
                                                pts_texto = 0.2))
resultados_modelo1 <- evaluacion(modelo1)

#
# Modelo 2 ----
modelo2 <- get_resultados_hetero(base,
                                 get_distancias(pts_busca = 0.2,
                                                pts_hobbies = 0.3,
                                                pts_primeraCita = 0.1,
                                                pts_lugaresFac = 0.1,
                                                pts_texto = 0.3))
resultados_modelo2 <- evaluacion(modelo2)

# # Modelo 3 ----
# modelo3 <- get_resultados_hetero(base,
#                                  get_distancias(pts_busca = 0.25,
#                                                 pts_hobbies = 0.35,
#                                                 pts_primeraCita = 0.2,
#                                                 pts_lugaresFac = 0.1,
#                                                 pts_texto = 0.1))
# resultados_modelo3 <- evaluacion(modelo3)
#
#
# # Modelo 4 ----
# modelo4 <- get_resultados_hetero(base,
#                                  get_distancias(pts_busca = 0.2,
#                                                 pts_hobbies = 0.4,
#                                                 pts_primeraCita = 0.1,
#                                                 pts_lugaresFac = 0.05,
#                                                 pts_texto = 0.25))
# resultados_modelo4 <- evaluacion(modelo4)
#
#
# # Modelo 5 ----
# modelo5 <- get_resultados_hetero(base,
#                                  get_distancias(pts_busca = 0.1,
#                                                 pts_hobbies = 0.6,
#                                                 pts_primeraCita = 0.1,
#                                                 pts_lugaresFac = 0.1,
#                                                 pts_texto = 0.1))
# resultados_modelo5 <- evaluacion(modelo5)
#
# # Modelo 6 ----
# modelo6 <- get_resultados_hetero(base,
#                                  get_distancias(pts_busca = 0.1,
#                                                 pts_hobbies = 0.1,
#                                                 pts_primeraCita = 0.1,
#                                                 pts_lugaresFac = 0.1,
#                                                 pts_texto = 0.6))
# resultados_modelo6 <- evaluacion(modelo6)
#
# resultados_modelo1
# resultados_modelo2
# resultados_modelo3
# resultados_modelo4
# resultados_modelo5
# resultados_modelo6

# Se conserva el modelo 2
distancias <- get_distancias(pts_busca = 0.2,
                                            pts_hobbies = 0.3,
                                            pts_primeraCita = 0.1,
                                            pts_lugaresFac = 0.1,
                                            pts_texto = 0.3)

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
matches <- filter_base_emparejamientos_mujeres_heteros(m)%>%
  select(-comentario_clean, -comentario)


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


  distancias_seleccionadas <- distancias[ids, ids]
  distancias_seleccionadas <- distancias_seleccionadas[-1,]
  dim(distancias_seleccionadas)
  preemparejamientos_homosexuales = matchingR::roommate(utils = distancias_seleccionadas)


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

# Se hacen pruebas
filter_base_emparejamientos_homosexuales <- function(emparejamientos, idSeleccionado){

  emparejamientos <- emparejamientos%>%
    filter(persona1==idSeleccionado)

  base_filtrada <- base%>%
    filter(id %in% c(emparejamientos$persona1,
                     emparejamientos$persona2))

  return(base_filtrada)
}

p <- sample(emparejamientos_gays$persona1, 1)
matches <- filter_base_emparejamientos_homosexuales(emparejamientos_gays, p)%>%
  select(-comentario_clean, -comentario)


p <- sample(emparejamientos_lesbianas$persona1, 1)
matches <- filter_base_emparejamientos_homosexuales(emparejamientos_lesbianas, p)%>%
  select(-comentario_clean, -comentario)


# Resultados ----

emparejamientos_lesbianas

emparejamientos_gays

emparejamientos_mujeres_heteros

# Se necesita una sola lista con todos los emparejamientos
emparejamientos_mujeres_heteros_unique <- rbind(
  emparejamientos_mujeres_heteros%>% select(mujer, hombre1)%>%rename(persona1=mujer, persona2=hombre1),
  emparejamientos_mujeres_heteros%>% select(mujer, hombre2)%>%rename(persona1=mujer, persona2=hombre2))%>%
  arrange(persona1)


# Se juntan todos los emparejamientos
emparejamientosComplete <- rbind(emparejamientos_mujeres_heteros_unique,
                         emparejamientos_gays,
                         emparejamientos_lesbianas)

emparejamientosComplete
emparejamientos <- emparejamientosComplete%>% drop_na()
dim(emparejamientosComplete)
dim(emparejamientos)

# Ahora es importante saber su distancia
distancias <- as.matrix(distancias)
rownames(distancias) <- colnames(distancias) <- usuarios
emparejamientos$distancia <- mapply(function(p1, p2) distancias[p1, p2],
                                    emparejamientos$persona1,
                                    emparejamientos$persona2)


summary(emparejamientos$distancia)

emparejamientos%>%
  count(persona1)%>%
  filter(n>2)

emparejamientos%>%
  count(persona2)%>%
  filter(n>1)


emparejamientos
# Ahora necesito que todos los ids esten en una sola columna
emparejamientosInverse <- emparejamientos%>%
  rename(persona = persona1)%>%
  rename(persona1 = persona2)%>%
  rename(persona2 = persona)%>%
  select(persona1, persona2, distancia)


preResultados <- rbind(emparejamientos, emparejamientosInverse)%>%
  distinct(persona1, persona2, distancia)%>%
  rename(id = persona1,
         match = persona2)

nrow(base)-length(unique(preResultados$id))


length(unique(preResultados$id))
# Sólo se quedaron 9 personas sin emparejar
# por quéee?


noEmparejados <- base%>%filter(id %in% setdiff(base$id, unique(preResultados$id)))
distanciasnoEmparejados <- distancias[,noEmparejados$id]
# Después se ve qué hacer con ellos

Resultados <- preResultados


# Comprobación de los resultados
Resultados%>%
  filter(distancia<0.25)

p <- base%>%
  filter(id %in% c(291, 180))

