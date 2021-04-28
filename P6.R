#============================================================================== #
# Fecha: 2021-04-07 
# Ejemplo de análisis descriptivo
# ============================================================================= #


# Paquetes                        ----------------------------------------------
if (!require("pacman")) install.packages("pacman")#instala pacman si se requiere

pacman::p_load(tidyverse, 
               readxl,writexl,googlesheets4, # importar hojas de cálculo
               haven, foreign, # importacón de dta y sav
               sjlabelled, GGally, RColorBrewer, # etiquetas
               janitor, skimr, # Limpieza
               esquisse, stringr, cov.wt) 



# Datos                   -----------------------------------------------

BD_MUJERES_ENDIREH2016_sample <- read_dta("https://github.com/aniuxa/repo-MEG/raw/main/datos/BD_MUJERES_ENDIREH2016_sample.dta")

# Etiquetas de la práctica anterior -----------------------------------

sit_conyugal.lab<-c("casada-unida",  "Unida+pareja ausente", "Separada o Divorciada", 
                    "viuda", "Soltera + novio", "Soltera con ex", "Soltera nunca novio")


BD_MUJERES_ENDIREH2016_sample$sit_conyugal<-set_labels(BD_MUJERES_ENDIREH2016_sample$sit_conyugal, 
                                                       labels=sit_conyugal.lab) # establece las etiquetas



niv.shortlab <- c("Ninguna", "Preescolar", "Primaria", "Secundaria", "Prepa", "Técnico -pri",
                  "Técnico -sec", "Técnico -prepa", "Normal", "Normal-lic", 
                  "Licenciatura", "Posgrado") #Vector de etiquetas cortas.

BD_MUJERES_ENDIREH2016_sample$niv<-set_labels(BD_MUJERES_ENDIREH2016_sample$niv,
                                              labels=niv.shortlab) # establece las etiquetas


freq.shortlab <- c("muchas veces", "pocas veces", "una vez", 
                   "no ocurrió", "no_especificado") #Vector de etiquetas cortas.


BD_MUJERES_ENDIREH2016_sample <- BD_MUJERES_ENDIREH2016_sample %>% 
  dplyr::mutate_at(vars(p13_1_1:p13_1_22), ~ set_labels(.x, labels=freq.shortlab))


si_no<-c("Sí", "No", "No especificado")

# IMPORTANTE: REVISAR SI LAS VARIABLES SON NUMÉRICAS, NO SE PUEDE ETIQUETAR UNA VARIABLE NO NUMÉRICA

BD_MUJERES_ENDIREH2016_sample <- BD_MUJERES_ENDIREH2016_sample %>%  
  dplyr::mutate_at(vars(p12_1_1_1:p12_1_1_29), ~ as.numeric(.x) ) %>%  # Vuelve numérico
  dplyr::mutate_at(vars(p12_1_1_1:p12_1_1_29), ~ set_labels(.x, labels=si_no)) 

class(BD_MUJERES_ENDIREH2016_sample$p12_1_1_30)
table(BD_MUJERES_ENDIREH2016_sample$p12_1_1_30)



BD_MUJERES_ENDIREH2016_sample <- BD_MUJERES_ENDIREH2016_sample %>% 
  dplyr::mutate_at(vars(starts_with("p13_1_")), ~ set_labels(.x, labels=freq.shortlab))

# Índice de clases anteriores ----
## Vamos a construir indices con los valores los primeros 22 items de la sección 13 ---


index <-  BD_MUJERES_ENDIREH2016_sample %>% 
  filter(sit_conyugal<7) %>% # Ojo!!!!!!! Siempre checar a quién le toca la sección
  select(id_muj, p13_1_1:p13_1_22) %>% # Vamos a hacer una base chiquita con el id y los items relevantes
  dplyr::mutate_at(vars(p13_1_1:p13_1_22), ~na_if(.x, 9)) %>%  # manda los 9 a missing
  dplyr::mutate_at(vars(p13_1_1:p13_1_22), ~ (4 - .x))  # cambia la lógica de la numeración a 4 se le resta el valor. Cambia sentido

# Hoy hacemos el índice
index<- index %>% 
  mutate(index= rowMeans(across(p13_1_1:p13_1_22), na.rm = T)) %>% 
  mutate(index=index/3) # nos quedan una metrica del 0 al 3 

hist(index$index)

index<-index %>% 
  select(id_muj,index)

# Plan con maña

BD_MUJERES_ENDIREH2016_sample<-BD_MUJERES_ENDIREH2016_sample %>% 
  merge(index, by="id_muj", all.x = T)


# Código base para un análisis descriptivo ----


## Univariado ----

### Variable cuanti ----
#### Gráficos

BD_MUJERES_ENDIREH2016_sample %>% 
  ggplot(aes(x=income_xx)) +
  geom_histogram() 

BD_MUJERES_ENDIREH2016_sample %>% 
  ggplot(aes(x=income_xx, size=fac_muj)) + # ponemos el peso del factor
  geom_histogram(bins=15) 

BD_MUJERES_ENDIREH2016_sample %>% 
  ggplot(aes(x=income_xx, size=fac_muj)) +
  geom_density(color="blue") + # podemos poner colores en la geometría
  theme_minimal()


#### Medidas númericas

summary(BD_MUJERES_ENDIREH2016_sample$income_xx) # Ojo tenemos NA

BD_MUJERES_ENDIREH2016_sample %>% 
  filter(edad>14) %>% # la ventaja de este formato es que podemos hacer filtro
  summarise(media=weighted.mean(income_xx, na.rm=T)) # con pesos, checa que cambia
  

boxplot(BD_MUJERES_ENDIREH2016_sample$income_xx)

cinco<-boxplot(BD_MUJERES_ENDIREH2016_sample$income_xx)
cinco$stats # resumen de cinco números


### Variable cuali ----

#### Gráfico

BD_MUJERES_ENDIREH2016_sample %>% 
  ggplot(aes(x=as_label(sit_conyugal))) +
  geom_bar() 

BD_MUJERES_ENDIREH2016_sample %>% 
  ggplot(aes(x=as_label(sit_conyugal), size=fac_muj)) +
  geom_bar(fill="blue") + coord_flip() +
  theme_minimal() +
  labs(title= "Situación conyugal", y="frecuencias", x="") # ojo queda con las coordenadas originales


#### Tabulados con tabyl

BD_MUJERES_ENDIREH2016_sample %>% 
  mutate(sit_conyugal=as_label(sit_conyugal)) %>% 
  tabyl(sit_conyugal) %>% 
  adorn_totals()

#### Tabulado con count para poner pesos

BD_MUJERES_ENDIREH2016_sample %>% 
  mutate(sit_conyugal=as_label(sit_conyugal)) %>% 
  count(sit_conyugal, wt=fac_viv) %>% 
  adorn_totals() %>% 
  adorn_percentages("col") %>% 
  adorn_pct_formatting()



## Bivariado ----

### Cuanti vs c uanti -----


##### Gráfico

BD_MUJERES_ENDIREH2016_sample %>% 
  ggplot(aes(y=index, x=income_xx))+
  geom_point()


BD_MUJERES_ENDIREH2016_sample %>% 
  ggplot(aes(y=index, x=income_xx, color=as_label(sit_conyugal)))+
  geom_point()

BD_MUJERES_ENDIREH2016_sample %>% 
  ggplot(aes(y=index, x=income_xx))+
  geom_point() + facet_wrap(~as_label(sit_conyugal))

##### Correlación

cor(BD_MUJERES_ENDIREH2016_sample$index,
    BD_MUJERES_ENDIREH2016_sample$income_xx) #ojo con los missings

cor(BD_MUJERES_ENDIREH2016_sample$index,
    BD_MUJERES_ENDIREH2016_sample$income_xx, 
    use= "pairwise" )

cor(BD_MUJERES_ENDIREH2016_sample$index,
    BD_MUJERES_ENDIREH2016_sample$income_xx, 
    use= "pairwise", method="pearson" ) # por default


### Cuali vs cuanti ----


BD_MUJERES_ENDIREH2016_sample %>% 
  ggplot(aes(x=income_xx, 
             size=fac_muj, 
             fill=as_label(sit_conyugal))) +# relleno depende de una variable + 
  geom_density() + 
  theme_minimal()


BD_MUJERES_ENDIREH2016_sample %>% 
  ggplot(aes(x=log(income_xx), # escala logaritmica
             size=fac_muj, 
             fill=as_label(sit_conyugal))) + # relleno depende de una variable
  geom_density(alpha=0.5) + 
  theme_minimal()


BD_MUJERES_ENDIREH2016_sample %>% 
  ggplot(aes(x=log(income_xx), # escala logaritmica
             size=fac_muj, 
             fill=as_label(sit_conyugal))) + # relleno depende de una variable
  geom_density(alpha=0.5) + 
  theme_minimal() + 
  labs(title="Distribución del ingreso y  situación conyugal",
      x="Logaritmo del ingreso",
      y="Densidad - Kernel", 
      fill="situación conyugal") 
  

## Boxplot

BD_MUJERES_ENDIREH2016_sample %>% 
  ggplot(aes(x=income_xx,
             size=fac_muj, 
             fill=as_label(sit_conyugal))) + # relleno depende de una variable
  geom_boxplot() + 
  theme_minimal() 

# Violín

BD_MUJERES_ENDIREH2016_sample %>% 
  ggplot(aes(x=income_xx, 
             size=fac_muj, 
             y=as_label(sit_conyugal),
             fill=as_label(sit_conyugal))) + 
  geom_violin() + 
  theme_minimal() 

#Parece redundante

BD_MUJERES_ENDIREH2016_sample %>% 
  ggplot(aes(x=income_xx, 
             size=fac_muj, 
             y=as_label(sit_conyugal),
             fill=as_label(sit_conyugal))) + 
  geom_violin() + 
  theme_minimal() + 
  guides(fill=FALSE) #ojo con es

# Más de apagar y encender leyendas 
#https://www.datanovia.com/en/blog/ggplot-legend-title-position-and-labels/


## Medidas númericas 

BD_MUJERES_ENDIREH2016_sample %>% 
  group_by(as_label(sit_conyugal)) %>% # paso esencial  
  summarise(media=weighted.mean(income_xx, na.rm=T),
           sd=sd(income_xx, na.rm=T)) 

### Cuali vs cuali  ----

BD_MUJERES_ENDIREH2016_sample %>% 
  dplyr::mutate_at(vars(sit_conyugal, niv), as_label) %>% 
  ggplot(aes(niv, fill=sit_conyugal)) +
  geom_bar() 

BD_MUJERES_ENDIREH2016_sample %>% 
  dplyr::mutate_at(vars(sit_conyugal, niv), as_label) %>% 
  ggplot(aes(niv, fill=sit_conyugal), size=fac_muj) +
  geom_bar() 

#Ojo la barra depende de lo que pongamos en las x

BD_MUJERES_ENDIREH2016_sample %>% 
  dplyr::mutate_at(vars(sit_conyugal, niv), as_label) %>% 
  ggplot(aes(niv, fill=sit_conyugal), size=fac_muj) +
  geom_bar(position="fill") 

# Tabulados con count

BD_MUJERES_ENDIREH2016_sample %>% 
  dplyr::mutate_at(vars(sit_conyugal, niv), ~ as_label(.)) %>% 
  count(sit_conyugal, niv,  wt = fac_muj) %>% 
  pivot_wider(names_from = sit_conyugal, 
              values_from = n)


# De preferencia mandar estas tablas a un objeto cuando soy muy grandes
# luego puedes mandarlas a excel


BD_MUJERES_ENDIREH2016_sample %>% 
  dplyr::mutate_at(vars(sit_conyugal, niv), ~ as_label(.)) %>% 
  count(sit_conyugal, niv,  wt = fac_muj) %>% 
  pivot_wider(names_from = sit_conyugal, 
              values_from = n) %>% 
  adorn_totals() %>% # Agrega total
  adorn_percentages("col")  %>% 
  adorn_pct_formatting()

tab1<-BD_MUJERES_ENDIREH2016_sample %>% 
  dplyr::mutate_at(vars(sit_conyugal, niv), ~ as_label(.)) %>% 
  count(sit_conyugal, niv,  wt = fac_muj) %>% 
  pivot_wider(names_from = sit_conyugal, 
              values_from = n) %>% 
  adorn_totals() %>% # Agrega total
  adorn_percentages("col")  %>% 
  adorn_pct_formatting()

write_xlsx(tab1, path = "tabulado1.xlsx")