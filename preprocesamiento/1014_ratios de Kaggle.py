# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""

import pandas as pd

import numpy as np

features_to_transform = ["ctrx_quarter", "cpayroll_trx", "mcuentas_saldo", "mcaja_ahorro", "ctarjeta_visa_transacciones", 
    "mtarjeta_visa_consumo", "mprestamos_personales",  "mrentabilidad_annual", "mactivos_margen", "cliente_edad"]

# df_train = pd.read_csv('C:/Users/Usuario/Documents/Investigación/1. Maestría DM/2021Economia/datasetsOri/paquete_premium_202009.csv')
# df_test = pd.read_csv('C:/Users/Usuario/Documents/Investigación/1. Maestría DM/2021Economia/datasetsOri/paquete_premium_202011.csv')

df_test = pd.read_csv('C:/Archivos/maestria/dmeyf/datasetsOri/paquete_premium_202011.csv')
df_train = pd.read_csv('C:/Archivos/maestria/dmeyf/datasetsOri/paquete_premium_202009.csv')

# Ratio de todas con todas.
for col in features_to_transform:
    for col2 in features_to_transform:
        if col != col2:
            col_name = f'ratio_{col}-{col2}'
            df_train.loc[:, col_name] = df_train.loc[:, col] / df_train.loc[:, col2]

            # En el caso de que alguna quede con valor infinito, pongo el valor mas grande de la columna (excluyendo el infinito)
            df_train.loc[df_train[col_name] == np.inf, col_name] = df_train[col_name].replace(np.inf, 0).max()
df_train.fillna(0, inplace=True)

# Lo mismo para el set de test
for col in features_to_transform:
    for col2 in features_to_transform:
        if col != col2:
            col_name = f'ratio_{col}-{col2}'
            df_test.loc[:, col_name] = df_test.loc[:, col] / df_test.loc[:, col2]

            # En el caso de que alguna quede con valor infinito, pongo el valor mas grande de la columna (excluyendo el infinito)
            df_test.loc[df_test[col_name] == np.inf, col_name] = df_test[col_name].replace(np.inf, 0).max()

df_test.fillna(0, inplace=True)
# df_train.to_csv('C:/Users/Usuario/Documents/Investigación/1. Maestría DM/2021Economia/datasets/paquete_premium_202009_most_important_features_ratio.csv')
# df_test.to_csv('C:/Users/Usuario/Documents/Investigación/1. Maestría DM/2021Economia/datasets/paquete_premium_202011_most_important_features_ratio.csv')
df_train.to_csv('C:/Archivos/maestria/dmeyf/datasets/paquete_premium_202009_most_important_features_ratio.csv')
df_test.to_csv('C:/Archivos/maestria/dmeyf/datasets/paquete_premium_202011_most_important_features_ratio.csv')

