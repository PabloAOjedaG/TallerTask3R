# Tarea (Task) 3. Taller R de Estadistica y Programacion.
# Taller elaborado por:
# Pablo Andres Ojeda Gomez, con codigo 202012899;
# Diana Lucia Sanchez Ruiz, con codigo 202013592;
# Gabriel Perdomo Olmos, con codigo 202013093.
# Version de R 4.1.1

# Configuraciones iniciales
rm(list = ls())
if(!require(pacman)) install.packages("pacman") ; require(pacman)
p_load(rio,tidyverse,readxl,data.table,plyr,XML,rvest,xml2, broom, mfx, margins,  modelsummary,stargazer, outreg)

#--------------------------
#-------- PUNTO 1 ---------
#--------------------------

#1.1
via <- read.shapefile("data/input/VIAS")
puntos <- read.shapefile("data/input/MGN_URB_TOPONIMIA")

c_medico <- subset(puntos, puntos$dbf$dbf$CSIMBOL=="021001", select_all())
c_medico <- subset(puntos, puntos$dbf$dbf$CSIMBOL=="021002", select_all())
c_medico <- subset(puntos, puntos$dbf$dbf$CSIMBOL=="021003", select_all())

c_poblado <- readRDS(file = "data/input/c poblado (2017).rds")
as.numeric(c_poblado$cod_dane)
c_poblado <- subset(c_poblado, cod_dane>=54001 & cod_dane<55000)

depto <- readRDS(file = "data/input/dp deptos (2017).rds")
depto <- subset(depto, name_dpto="NORTE DE SANTANDER")

mapmuse <- readRDS(file = "data/input/victimas_map-muse.rds")

#1.2
skim(c_medico)
skim(c_poblado)
skim(depto)
skim(mapmuse)
skim(puntos)
skim(via)


#--------------------------
#-------- PUNTO 2 ---------
#--------------------------

#2.1
#Importamos la base de datos
map_muse <- readRDS(file="data/output/f_mapmuse.rds")

#creamos la variable dummy para hacer la regresion 
map_muse$genero2 <- ifelse(map_muse$genero == "Masculino", 1, 0)
map_muse$condicion2 <- ifelse(map_muse$condicion == "Fuerza pública", 1, 0)
map_muse$accidente_Mu <- ifelse(map_muse$tipo_accidente == "Accidente por MUSE", 1, 0)

#realizamos la regresion.
ols = lm(formula= fallecido ~ accidente_Mu + condicion2 + genero2 + dist_hospi + dist_cpoblado + dist_vias + as.factor(month), data=map_muse)
ols %>% summary()

#2.2
#graficamos los resultados coefplot 

olsest = list("OLS" = ols)

graph_ols = modelplot(olsest) + coord_flip() + 
  labs(title = "Probabilidad de fallecer")

ggsave(plot=graph_ols, file = "views/graph_ols.jpeg")


#2.3
#realizamos la regresion con la misma ecuacion pero ahora con modelos Logit y Probit
logit = glm(formula= fallecido ~ accidente_Mu + condicion2 + genero2 + dist_hospi + dist_cpoblado + dist_vias + as.factor(month), data=map_muse)
logit %>% summary()

Probit = glm(formula= fallecido ~ accidente_Mu + condicion2 + genero2 + dist_hospi + dist_cpoblado + dist_vias + as.factor(month), data=map_muse)
Probit %>% summary()

#calculamos los efectos marginales de las regresiones Lgit y Probit
logit_marg = margins(logit)
Probit_marg = margins(Probit)

#2.4

#Hacemos la tabla con los resultados de los 3 modelos
stargazer(ols, logit_marg, Probit_marg,
          type= 'text',
          dep.var.labels = c('','Probabilidad de fallecer',''), 
          df = FALSE,
          digits = 3, 
          out = paste0('data/output/ols.text'))

#2.5

# graficamos y exportamos los efectos marginales

#Vamos a hacer dos tipos de gráficos

#Primero, el grafico con todos los efectos marginales para los dos modelos
coeflist= list("Logit" = logit_marg, "Probit"=Probit_marg)
graph_problog = modelplot(coeflist) + coord_flip() + 
  labs(title = "Probabilidad de fallecer")

ggsave(plot=graph_problog, file = "views/graph_problog.jpeg")


#Segundo, los coefplot para cada modelo
coef-plot(logit_marg)

coef-plot(Probit_marg)

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
