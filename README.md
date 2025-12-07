# Emparejamientos Óptimos

Repositorio para generar y evaluar **emparejamientos** entre participantes a partir de sus preferencias y características.  
El proyecto está implementado en **R** y combina cálculo de matrices de distancia (Jaccard y distancias ordinales), múltiples modelos de ponderación y algoritmos de emparejamiento (Gale–Shapley para heterosexuales y `roommate` para emparejamientos homo/roomies).

---

## Contenido del repositorio

- `data/base.csv` — Archivo de entrada con los registros de participantes.  
- `src/` — Código fuente (script principal, funciones auxiliares).  
- `notebooks/` — Notebooks o análisis exploratorio (opcional).  
- `README.md` — Este archivo.  
- `requirements.R` / `packages.R` — Lista de paquetes R necesarios (opcional).

---

## Descripción rápida

El flujo principal del proyecto:

1. Cargar la base de datos (`base.csv`).  
2. Limpiar / transformar variables (dividir listas: hobbies, lugares, etc.).  
3. Calcular matrices de distancia por dimensión (búsqueda, hobbies, primera cita, lugares de facultad).  
4. Construir combinaciones ponderadas de las distancias (`get_distancias`) y generar preferencias.  
5. Ejecutar algoritmos de emparejamiento:
   - `galeShapley.collegeAdmissions` (para parejas heterosexuales con posibilidad de que una persona tenga 2 "slots").
   - `roommate` (para emparejamientos entre personas del mismo género).
6. Consolidar resultados, calcular distancias por pareja y evaluar frente a emparejamientos aleatorios.

---

## Formato esperado del `data/base.csv`

Columnas mínimas (nombres exactos usados por el script):

- `id` — identificador numérico de la persona.  
- `genero` — `"Hombre"` / `"Mujer"` (según el script).  
- `gustos` — `"Hombres"`, `"Mujeres"`, `"Ambos"`.  
- `busca` — búsqueda/objetivo (valores como `"Algo casual"`, `"Amistad"`, `"Una relación"`).  
- `hobbies` — cadenas separadas por `", "` (ej: `"Cocinar, Deporte"`).  
- `muybien` — lugares preferidos separados por `", "`.  
- `indiferente` — lugares indiferentes (opcional, usado en `distancia_primeraCita`).  
- `mal` — lugares que no le gustan (opcional).  
- `lugares` — (usado en `dist_lugaresFac`, formato cadena separada por `,`).  

> El script original lee `base.csv` con `read.csv("base.csv")`. Asegúrate que los separadores/encodings coincidan.

---

## Paquetes R requeridos

Instala estos paquetes antes de ejecutar el script:

```r
install.packages(c(
  "sets","dplyr","janitor","stringi","stringr","tm","tidyr",
  "matchingR","stringdist","proxy","glue","showtext","ggplot2","tidyverse","forcats"
))
