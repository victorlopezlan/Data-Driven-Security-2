############################################
# Practica 2
# Autores: Miriam, Oriol, Ulises, Víctor
# Fecha: 2026-01-12
############################################

# --------------------------------------------------#
#   Configuración global (equivalente a setup)      #
# --------------------------------------------------#
options(warn = -1)

# --------------------------------------------------#
#           Carga de librerías                      #
# --------------------------------------------------#
library(httr)
library(XML)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(rlang)
library(stringr)
library(DT)

# --------------------------------------------------#
#     Ejercicio 1.1 - Descarga de la web            #
# --------------------------------------------------#
dominio_web <- "https://www.mediawiki.org"
url_web <- "https://www.mediawiki.org/wiki/MediaWiki"

get_response <- httr::GET(url_web)

# Convertimos la respuesta en XML
web_xml <- XML::htmlParse(get_response)

# --------------------------------------------------#
#     Ejercicio 1.2 - Obtener el título             #
# --------------------------------------------------#
print("Ejercicio 1.2")
title <- XML::xpathSApply(web_xml, "//title", xmlValue)
print(title)

# --------------------------------------------------#
#   Ejercicio 1.3 - Extracción de enlaces           #
# --------------------------------------------------#
print("Ejercicio 1.3")
# Texto de los enlaces
links_text <- XML::xpathSApply(web_xml, "//a", xmlValue)

# URLs de los enlaces
links_url <- XML::xpathSApply(web_xml, "//a", xmlGetAttr, "href")

# Tratamiento de valores NULL
links_text[sapply(links_text, is.null)] <- NA
links_url[sapply(links_url, is.null)] <- NA

vector_links_text_nulls <- unlist(links_text)
vector_links_url_nulls <- unlist(links_url)

print(head(vector_links_text_nulls))
print(head(vector_links_url_nulls))

# --------------------------------------------------#
# Ejercicio 1.4 - Tabla de enlaces y frecuencia     #
# --------------------------------------------------#
print("Ejercicio 1.4")
tabla_valores <- data.frame(
  Enlace = vector_links_url_nulls,
  Texto = vector_links_text_nulls,
  stringsAsFactors = FALSE
)

# Tabla con número de apariciones
tabla_vistos <- tabla_valores %>%
  group_by(Enlace, Texto) %>%
  summarise(Visto = n(), .groups = "drop")

print(tabla_vistos)

# --------------------------------------------------#
# Ejercicio 1.5 - Comprobación del estado HTTP      #
# --------------------------------------------------#
print("Ejercicio 1.5")

# Normalización de enlaces
estado_enlaces <- stringr::str_replace(tabla_vistos$Enlace, "^#", paste0(url_web, "#"))
estado_enlaces <- stringr::str_replace(estado_enlaces, "^//", "https://")
estado_enlaces <- stringr::str_replace(estado_enlaces, "^/", paste0(dominio_web, "/"))

tabla_vistos$Enlace_Normalizado <- estado_enlaces
tabla_vistos <- tabla_vistos[, c("Enlace", "Enlace_Normalizado", "Texto", "Visto")]

# Función segura para HEAD
safe_head <- function(url) {
  Sys.sleep(0.5)
  tryCatch(
    httr::status_code(httr::HEAD(url)),
    error = function(e) NA
  )
}

tabla_vistos$status <- sapply(tabla_vistos$Enlace_Normalizado, safe_head)

df_estado <- tabla_vistos[, c("Enlace", "Enlace_Normalizado", "Visto", "status")]
print(df_estado)

# --------------------------------------------------#
#         Ejercicio 2.1 - Frecuencia de enlaces     #
# --------------------------------------------------#
print("Ejercicio 2.1")
# Clasificación de URLs absolutas y relativas
tabla_vistos$Tipo_URL <- ifelse(
  grepl("^http", tabla_vistos$Enlace),
  "Absoluta",
  "Relativa"
)

# Gráfico de barras (frecuencia de aparición)
ggplot(tabla_vistos, aes(x = Visto, fill = Tipo_URL)) +
  geom_bar(position = "dodge", color = "black") +
  labs(
    title = "Frecuencia de aparición de enlaces",
    x = "Número de veces que aparece un enlace",
    y = "Número de enlaces",
    fill = "Tipo de URL"
  ) +
  theme_minimal(base_size = 14)
