# -*- coding: utf-8 -*-
"""
Created on Wed Oct 29 20:54:54 2025

@author: Jazmin
"""

import os
import pandas as pd 
import janitor

path = 'C:/Users/Jazmin/Documents/Ciencia de datos/Proyecto'
os.chdir(path)

baseCompleteRaw = pd.read_csv('respuestas.csv')

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

