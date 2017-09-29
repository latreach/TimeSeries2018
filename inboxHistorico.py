## LIBRERIAS
import facebook
import pandas as pd
import sys
import itertools
from itertools import chain
from datetime import timedelta, date
import datetime
import time
import re

### FUNCIONES
def existKey(dictionary, key):
    try:
        dictionary[key]
        return True
    except KeyError as error:
        return False
        pass

def obtenHora(x):
    y = re.sub(".*T", "", x)
    return(y)

def obtenFecha(x):
    y = re.sub("T.*", "", x)
    return(y)

###GLOBALES

token=""
format = "%Y-%m-%d"

string1 = 'me/conversations?fields=message_count,updated_time,'
string2 = 'messages{message,id,from,to,created_time}&limit=100'
afterG  = '&after='

stringBusqueda = string1 + string2


FechaFinal = '2015-01-01'
fechaTest  = '2017-09-24'

fechasTest = []
for i in range(1,31):
    i = str(i)
    if(len(i)<2):
        i = '0' + i
    else:
        i
    numero = '-'.join(['2014', '12', i])
    fechasTest.append(numero)

#### CONEXION API
fB = facebook.GraphAPI(token)

### RECOLECCION DATOS 
testLista = test['data']
conteo = 0
while(1):
    conteo = conteo + 1
    print(conteo)
    if('next' in test['paging']):
        nextToken = test['paging']['cursors']['after']
        stringBusqueda1  = stringBusqueda + afterG + nextToken
        test = fb.get_object(stringBusqueda1)
        for i in test['data']:
            fechaDestino = i['update_time']
            fechaDestino = obtenHora(fechaDestino)
            fechaDestino.encode('utf-8')
            if(fechaDestino not in fechasTest):
                testLista.append(i)
            else:
                break:
    else:
        break

### MANIPULACION MENSAJES
mensajes = []
for i in testLista:
    mensaje = i['messages']['data']
    mensajes.append(mensaje)

mensajes = list(chain.from_iterable(mensajes))

textoMsj = []


###TEST
idsConv = [j['id'] for j in testLista]
conteos = [j['message_count'] for j in testLista]

repeticion = map(lambda x,y: np.repeat(x,y), idsConv, conteos)
repeticion = list(chain.from_iterable(repeticion))
mensajes = [d['messages']['data'] for d in testLista]
#mensajes = list(chain.from_iterable(mensajes))

longitudes = []

for i in mensajes:
    longitudes.append(len(i))

repeticion = map(lambda x,y: np.repeat(x, y), idsConv, longitudes)
repeticion = list(chain.from_iterable(repeticion))

#repeticion = map(λ x,y: np.repeat(x,y), idsConv, conteos)

mensajes = list(chain.from_iterable(mensajes))

textoMsj = []
for i in mensajes:
    temporal = dict(id_FbUsuario = i['from']['id'])
    temporal.update(nombre_FbUsuario = i['from']['name'])
    temporal.update(id_mensaje = i['id'])
    temporal.update(id_FbUsuarioDestino = i['to']['data'][0]['id'])
    temporal.update(nombre_FbUsuasioDestino = i['to']['data'][0]['name'])
    temporal.update(mensaje = i['message'])
    temporal.update(fecha_Creacion = obtenFecha(i['created_time']))
    temporal.update(hora_Creacion  = obtenHora(i['created_time']))
    textoMsj.append(temporal)

textoMsj  = pd.DataFrame.from_dict(textoMsj)

idConversacion = pd.DataFrame(repeticion)
mensajesHistorico = pd.concat([idConversacion, textoMsj], axis=1)
mensajesHistorico.columns = ['idConversacion', "Fecha", "Hora",
                             "id_Usuario_Desde", "id_Usuario_Hacia", 
                             "idMensaje", "Mensaje", "Nombre_Usuario_Desde",
                            "Nombre_Usuario_Hacia"]

mensajesHistorico.to_csv("datos/facebook/mensajesHistoricoInbox.csv",
                         sep=",", header=True, index=False, 
                         encoding="utf-8")
















