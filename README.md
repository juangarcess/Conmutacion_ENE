
## Antecedentes

En este documento se analiza número de personas ocupadas que residen en la Región del Biobío, que realizan su actividad laboral principal en otras regiones. Se trabajará con la base de datos de la Encuesta Nacional de Empleo respecto al trimestre móvil abril-junio 2023, y posteriormente con referencia al año 2022.

Cabe señalar que **los resultados no corresponden a una publicación oficial, sino que es un análisis ilustrativo**.

En primer lugar, se lee la base de datos disponible al público, correspondiente al trimestre móvil AMJ 2023. 


```{r setup, include=FALSE}

knitr::opts_chunk$set(echo = TRUE)
```
 



```{r lee BD, echo=TRUE, message=FALSE, warning=FALSE, paged.print=FALSE}
library(dplyr)
library(survey)
library (haven)
library(car)
library(openxlsx)
library(readxl)


BASE_ENE<-read_sav("https://www.ine.gob.cl/docs/default-source/ocupacion-y-desocupacion/bbdd/2023/spss/ene-2023-05-amj.sav")

```


El libro de códigos está disponible [AQUÍ](https://www.ine.gob.cl/docs/default-source/ocupacion-y-desocupacion/bbdd/libro-de-codigos/codigos-ene-2020.pdf?sfvrsn=54753851_60).Luego de revisarlo, se tienen las siguientes variables clave:


**region**, se refiere a la región donde reside habitualmente la persona encuestada.

**activ**, es una variable que indica la condición de actividad de la persona durante la semana de referencia.
  1 Ocupados/as
  2 Desocupados/as
  3 Fuera de la fuerza de trabajo 
  
**b14_rev4cl_caenes**, es la rama de actividad económica de empresa donde trabaja según CAENES (adaptación de CIIU Rev. 4.cl).

**b18_region**, indica el código de la región donde se ubica la empresa, negocio, institución actividad por cuenta propia donde realizó su trabajo.

Se carga una función que permite evaluar calidad de las estimaciones de acuerdo con estándar publicado [AQUÍ](https://www.ine.gob.cl/docs/default-source/institucionalidad/buenas-pr%C3%A1cticas/clasificaciones-y-estandares/est%C3%A1ndar-evaluaci%C3%B3n-de-calidad-de-estimaciones-publicaci%C3%B3n-27022020.pdf).

```{r Función evaluar calidad, echo=TRUE, message=FALSE, warning=FALSE, paged.print=FALSE}


options(OutDec = ",")

### 
##Funcion que evalua calidad en funcion de observaciones, g de l, y se o cv dependiendo de si es tasa o nivel
## 
## requiere tener previamente calculados los parametros de entrada, los cuales son
# nobs=numero de observaciones
# error_medido= coef de variacion (expresado entre 0 y 1) o error estandar (en el caso de proporciones, expresado entre 0 y 1)
# gdel=grados de libertad
# Estos parametros tienen valores por defecto
# nivel=1,  por lo tanto, por defecto evalua el caso de estimacion de nivel (totales), 
#           poner cualquier otro valor para evaluar proporciones camino de proporciones.
# p_estimada=0 se usa para meter el valor del parametro cuando es tasa o proporcion
#

calidad_puntual<-function(nobs,error_medido, gdel,nivel=1, p_estimada=0 )
{
  resultado_calidad<-"por evaluar"### esta variable va a ser la que entregará el resultado
  
  if(nobs>=60)
  {
    if(gdel>=9)
    {
      
      if(nivel==1)
      {### aqui entremos al caso de estimacion de niveles
        
        if(error_medido<=0.15)
        {
          resultado_calidad<-"fiable"
        }
        else if(error_medido<=0.30)
        {
          resultado_calidad<-"poco fiable"
        }else{
          resultado_calidad<-"no fiable"
        }
        
      }else{###estimacion de proporciones
        
           if(p_estimada<0.5)
           {
             cota2<-((p_estimada)^(2/3))/9
             
             if(error_medido<=cota2)
             {
               resultado_calidad<-"fiable"
             }else{
               resultado_calidad<-"poco fiable"
             }
             
             
           }else{
             
             cota1<-((1-p_estimada)^(2/3))/9
             if(error_medido<=cota1)
             {
               resultado_calidad<-"fiable"
               
             }else{
               
               resultado_calidad<-"poco fiable"
               
             }
             
           }
        
      }
      
    }else{
      resultado_calidad<-"no fiable"
    }
    
    
  }else{
    resultado_calidad<-"no fiable"
  }  
  
}

###fin funcion

```


Se crea variable binaria para identificar a las personas ocupadas.
```{r echo=TRUE, message=FALSE, warning=FALSE, paged.print=FALSE}

BASE_ENE<-mutate(BASE_ENE, binario_ocupado=case_when( activ==1 ~1, TRUE~0))

```




Se crea diseño complejo, de forma de obtener la calidad de estimaciones

```{r Codigo inicial, include=FALSE}

  class(BASE_ENE$fact_cal)

  diseno_enet =svydesign(id=~conglomerado, # Etiquetas UPM
                         strata=~estrato, #Estratos
                         check.strata=TRUE, # Comprueba que los clusters est?n anidados en los estratos
                         weights=~fact_cal, # Ponderador
                         data=BASE_ENE)
  
  options(survey.lonely.psu="remove") 
```
  
 

A continuación, se obtiene el total de ocupados de la región del Biobío por Región donde trabajan.  
  
  
```{r echo=TRUE, message=FALSE, warning=FALSE, paged.print=TRUE}

 #ocupados_totales
estimacion<-svyby(~binario_ocupado
                  ,by=~b18_region
                  ,data=BASE_ENE
                  ,drop.empty.groups=FALSE
                  , na.rm.all=FALSE
                  , subset(diseno_enet,region==8 & binario_ocupado==1),svytotal ,vartype=c("se","cv"))

recuento_no_ponderado<-svyby(~binario_ocupado
                             ,by=~b18_region
                             ,data=BASE_ENE,
                             drop.empty.groups=FALSE
                             , na.rm.all=FALSE
                             , subset(diseno_enet,region==8 & binario_ocupado==1),unwtd.count  ,vartype=c("se","cv"))
  
estimacion<-cbind(estimacion, nobs=recuento_no_ponderado$counts) 

estimacion$b18_region_num<-as.numeric(estimacion$b18_region)

estimacion$gdel<-NA
 estimacion$calidad<-NA

for(q in 1:nrow(estimacion))
{
  estimacion$gdel[q]<-degf(subset(diseno_enet,region==8 & binario_ocupado==1 & b18_region==estimacion$b18_region[q])) 

}


for(q in 1:nrow(estimacion))
{
  estimacion$calidad[q]<-calidad_puntual(estimacion$nobs[q], estimacion$cv[q], estimacion$gdel[q] )
}


estimacion

```

Como se ve, al desagregar por Región, las estimaciones resultan ser no fiables.

Ahora se verá si agregando las regiones, se pueden obtener estimaciones fiables. se crea una variable `region_agregada` con tres categorias, NORTE, CENTRO y SUR

```{r echo=TRUE, message=FALSE, warning=FALSE, paged.print=TRUE}

BASE_ENE<-mutate(BASE_ENE, region_macro=case_when(b18_region%in% c(15,1,2,3,4) ~1  ###NORTE Arica a Coquimbo
                                                  ,b18_region %in% c(5, 13, 6, 7) ~2  ###CENTRO Valparaiso a Maule
                                                  , b18_region %in% c(16,8,9,14,10,11,12) ~3###SUR Ñuble a Magallanes
                                                    ,TRUE ~9999  ###NO RESPONDE NO SABE
                                                  ))
 
 #a<-group_by(BASE_ENE, region_macro, b18_region)
 #a<-summarise(a, total=n())


   diseno_enet =svydesign(id=~conglomerado, # Etiquetas UPM
                         strata=~estrato, #Estratos
                         check.strata=TRUE, # Comprueba que los clusters est?n anidados en los estratos
                         weights=~fact_cal, # Ponderador
                         data=BASE_ENE)
  
  options(survey.lonely.psu="remove") 


estimacion2<-svyby(~binario_ocupado
                  ,by=~region_macro
                  ,data=BASE_ENE
                  ,drop.empty.groups=FALSE
                  , na.rm.all=FALSE
                  , subset(diseno_enet,region==8 & binario_ocupado==1),svytotal ,vartype=c("se","cv"))

recuento_no_ponderado2<-svyby(~binario_ocupado
                             ,by=~region_macro
                             ,data=BASE_ENE,
                             drop.empty.groups=FALSE
                             , na.rm.all=FALSE
                             , subset(diseno_enet,region==8 & binario_ocupado==1),unwtd.count  ,vartype=c("se","cv"))
  
estimacion2<-cbind(estimacion2, nobs=recuento_no_ponderado2$counts) 

 
estimacion2$gdel<-NA
 estimacion2$calidad<-NA

for(q in 1:nrow(estimacion2))
{
  estimacion2$gdel[q]<-degf(subset(diseno_enet,region==8 & binario_ocupado==1 & region_macro==estimacion2$region_macro[q])) 
  
 
}


for(q in 1:nrow(estimacion2))
{
  estimacion2$calidad[q]<-calidad_puntual(estimacion2$nobs[q], estimacion2$cv[q], estimacion2$gdel[q] )
}


estimacion2

```
  
  
  Es así como para el trimestre abril-junio de 2023, se estimó un total de `r format(round(estimacion2$binario_ocupado[1], 0), nsmall=0) ` personas ocupadas que residen en la Región del Biobío, pero que realizan su actividad en regiones del norte del país (Arica y Parinacota hasta Coquimbo). Dicha estimación sería `r estimacion2$calidad[1] `.
