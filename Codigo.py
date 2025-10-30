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




