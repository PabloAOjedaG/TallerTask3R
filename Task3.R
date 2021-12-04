# Tarea (Task) 3. Taller R de Estadistica y Programacion.
# Taller elaborado por:
# Pablo Andres Ojeda Gomez, con codigo 202012899;
# Diana Lucia Sanchez Ruiz, con codigo 202013592;
# Gabriel Perdomo Olmos, con codigo 202013093.
# Version de R 4.1.1

# Configuraciones iniciales
rm(list = ls())
if(!require(pacman)) install.packages("pacman") ; require(pacman)
p_load(rio,tidyverse,readxl,data.table,plyr,XML,rvest,xml2)

#--------------------------
#-------- PUNTO 1 ---------
#--------------------------




#--------------------------
#-------- PUNTO 2 ---------
#--------------------------




#--------------------------
#-------- PUNTO 3 ---------
#--------------------------

#Web scrapping

#3.1
#Leemos el archivo desde la consola
dptos = "https://es.wikipedia.org/wiki/Departamentos_de_Colombia"

#Creamos el objeto con el HTML como xml_doc
dptos_html = read_html(dptos)

#3.2
#Extraemos el titutlo de la pagina 
dptos_html %>% html_nodes(xpath = '//*[@id="firstHeading"]') %>% html_text()

#3.3

#Presentamos dos opciones para hacerlo:

#Opcion 1. Extraemos de una vez la tabla con el xpath
dptos_html %>% html_nodes(xpath = '//*[@id="mw-content-text"]/div[1]/table[3]') %>% html_table()

#Opcion 2. Usamos el paquete rvest

#Extraemos todas las tablas
tablas = dptos_html %>% html_nodes('table')

#Vemos el elemento que nos interesa que es la tabla con todos los departamentos
tablas[4] %>% html_table(header = T,fill=T)
