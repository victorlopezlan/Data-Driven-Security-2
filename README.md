**MÁSTER CYBERSECURITY MANAGEMENT**

**PRÁCTICA 2 – ASIGNATURA DATA DRIVEN SECURITY**

**Autores: Miriam, Oriol, Ulises, Víctor**

Este repositorio contiene un proyecto desarrollado en R con RStudio para realizar tareas de crawling y scraping sobre una página web (MediaWiki) y analizar los enlaces extraídos. El trabajo se presenta en un documento R Markdown (.Rmd) que puede renderizarse a HTML con resultados, tablas y visualizaciones, y en un script .R.


**Estructura del proyecto**

├── Practica2.R

├── Practica2.Rmd

├── Practica2.html

└── README.md



**Descripción de los archivos**

Practica2.R

Script de R que contiene todo el código necesario para ejecutar el análisis solicitado en la práctica de forma directa desde la consola o RStudio.

Permite descargar, limpiar y normalizar los datos, generar resultados y generar gráficos con los resultados del análisis.

Se puede ejecutar seleccionando todo el código del archivo y Control + Shift + Intro.

Practica2.Rmd

Documento R Markdown que combina código, texto y resultados para generar el informe de la práctica.

- Descarga de la web
- Conversión y parseo a XML/HTML
- Extracción del título con XPath
- Extracción de enlaces (<a>), texto y href
- Construcción de tabla de enlaces y conteos de repetición
- Normalización de URLs
- Verificación del estado HTTP
- Visualizaciones: histograma, barras y gráfico de tarta

Para generar el HTML: Control + Shift + K

Practica2.html

Informe final generado a partir del archivo Pratica2.Rmd.

Puede abrirse directamente en cualquier navegador web y se sobreescribe cada vez que se renderiza Practica2.Rmd.



**Requisitos**

R

RStudio

Paquetes utilizados: httr, XML, ggplot2, gridExtra, dplyr, rlang, DT, stringr, scales, knitr



**Uso del proyecto**

Clonar el repositorio: <https://github.com/victorlopezlan/Data-Driven-Security-2.git>

Abrir el proyecto en RStudio y ejecutar:

Practica2.R , seleccionar todo el código y Control + Shift + Intro

Practica2.Rmd para generar el informe en HTML, Control + Shift + K
