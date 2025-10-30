# -*- coding: utf-8 -*-
"""
Created on Wed Oct 29 20:54:54 2025

@author: Jazmin
"""

import os
import pandas as pd 
import janitor
import numpy as np

path = 'C:/Users/Jazmin/Documents/Ciencia de datos/Proyecto'
os.chdir(path)

baseCompleteRaw = pd.read_csv('respuestas.csv')


# ********************************************************************************
# PARTE 1: Limpieza de datos
# ********************************************************************************

# Se limpian los nombres de las columnas
baseCompleteRaw = baseCompleteRaw.clean_names(remove_special=False)

baseCompleteRaw.columns

baseComplete = baseCompleteRaw.copy()

# Se eliminan las columnas que no se van a utilizar 
baseComplete = baseComplete.drop(['para_la_primera_cita_[jueves]',
                                   'para_la_primera_cita_[viernes]',
                                   'columna_12',
                                   'columna_15',
                                   '¿tienes_alguna_alergia_o_restriccion_alimentaria_',
                                   'si_tienes_algun_comentario_o_pregunta_escribelo_a_continuacion_']
                                  , axis=1)

# Se renombran las columnas 
baseComplete = baseComplete.rename(columns={"marca_temporal": "fecha_hora", 
                   "direccion_de_correo_electronico": "correo",
                   "para_la_primera_cita_[perfecto]" : "muybien",
                   "para_la_primera_cita_[indiferente]" : "indiferente",
                   "para_la_primera_cita_[para_nada]" : "mal",
                   "¿como_te_llamas_" : "nombre",
                   "¿como_te_identificas_" : "genero",
                   "¿que_te_gusta_" : "gustos",
                   "¿principalmente_que_estas_buscando_" : "busca",
                   "lugares_favoritos_de_la_facultad" : "lugares",
                   "escribe_algo_que_le_quieras_decir_a_tu_match_aqui_puedes_poner_cualquier_cosa_por_ejemplo_una_presentacion_sobre_ti_por_que_decidiste_estudiar_en_ciencias_que_te_gustaria_hacer_el_14_de_febrero_si_ya_estas_yendo_a_terapia_v_etc_" : "comentario"})

baseComplete.columns

# Se modifica la fecha y hora en formato correcto
baseComplete['fecha_hora'] = pd.to_datetime(baseComplete['fecha_hora'], format="%d/%m/%Y %H:%M:%S")

# Se junta el genero y los gustos
def junta_sexo_genero(df):
    
    condiciones = [
        (df['genero'] == 'Hombre') & (df['gustos'] == 'Ambos'),
        (df['genero'] == 'Hombre') & (df['gustos'] == 'Hombres'),
        (df['genero'] == 'Hombre') & (df['gustos'] == 'Mujeres'),
        (df['genero'] == 'Mujer') & (df['gustos'] == 'Ambos'),
        (df['genero'] == 'Mujer') & (df['gustos'] == 'Hombres'),
        (df['genero'] == 'Mujer') & (df['gustos'] == 'Mujeres')
    ]


    # Valores correspondientes
    valores = ['Ha', 'Hh', 'Hm', 'Ma', 'Mh', 'Mm']

    # Crear la nueva columna
    df['grupo'] = np.select(condiciones, valores, default='')
    
    return df

baseComplete = junta_sexo_genero(baseComplete)


# Se limpia la parte de busca 
baseComplete['busca'] = baseComplete['busca'].str.replace(r' >:\)', '', regex=True)
baseComplete['busca'] = baseComplete['busca'].str.replace(r' :\)', '', regex=True)
baseComplete['busca'] = baseComplete['busca'].str.replace(r' <3', '', regex=True)

baseComplete['busca'] = baseComplete['busca'].str.replace(r'Una relación', 'relación', regex=True)
baseComplete['busca'] = baseComplete['busca'].str.replace(r'Algo casual', 'casual', regex=True)
baseComplete['busca'] = baseComplete['busca'].str.replace(r'Amistad', 'amistad', regex=True)


baseComplete['nombre'] = baseComplete['nombre'].str.replace(r'^\s+|\s+$', '', regex=True)

# quitar espacios al inicio y al final
baseComplete['nombre'] = baseComplete['nombre'].str.strip()

# reemplazar múltiples espacios internos por uno solo
baseComplete['nombre'] = baseComplete['nombre'].str.replace(r'\s+', ' ', regex=True)



# Como hay duplicados de personas que pusieron más de una vez su respuesta

Base = (
    baseComplete
    .sort_values('fecha_hora')                       # ordenar por hora
    .drop_duplicates(subset='correo', keep='last')  # quedarse con el registro más reciente por correo
    .reset_index(drop=True)                    # reiniciar índices
)

Base['id'] = Base.index + 1

# ********************************************************************************
# PARTE 2: Descriptivo
# ********************************************************************************
Base.columns


def separa_valores_columna(Base, columna):
    df = Base.copy()
    # Convertir a lista
    df[columna] = df[columna].str.split(',')
    
    # “Explotar” la lista en varias filas
    df_long = df.explode(columna).reset_index(drop=True)
    df_long[columna] = df_long[columna].str.strip()

    
    return df_long[['id', columna]]


lugares_muy_bien = separa_valores_columna(Base, 'muybien')
