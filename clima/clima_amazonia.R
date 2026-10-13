# Clima de setembro de 2026 por município do Amazonas
# Fonte: INPE / Programa Queimadas (dados observados do Risco de Fogo)

library(tidyverse)
library(sf)
library(terra)   # install.packages("terra")

pasta <- "~/infoamazoniaelnino/clima"
dir.create(file.path(pasta, "data"), recursive = TRUE, showWarnings = FALSE)

base <- "https://dataserver-coids.inpe.br/queimadas/queimadas/riscofogo_meteorologia/observado"
dias <- format(seq(as.Date("2026-09-01"), as.Date("2026-09-30"), by = "day"), "%Y%m%d")

# Baixa os 30 dias de setembro e resume o mês (média ou máximo)
setembro <- function(pasta_inpe, nome, resumo) {
  arquivos <- file.path(pasta, "data", sprintf("INPE_FireRiskModel_2.2_%s_%s.nc", nome, dias))
  for (a in arquivos[!file.exists(arquivos)]) {
    try(download.file(file.path(base, pasta_inpe, basename(a)), a, mode = "wb", quiet = TRUE))
  }
  arquivos <- arquivos[file.exists(arquivos) & file.size(arquivos) > 0]
  message(nome, ": ", length(arquivos), " de 30 dias")   # confira se deu 30
  r <- rast(arquivos)
  r <- if (resumo == "max") max(r, na.rm = TRUE) else mean(r, na.rm = TRUE)
  if (crs(r) == "") crs(r) <- "EPSG:4326"
  r
}

chuva          <- setembro("precipitacao", "Precipitation", "media")
temperatura    <- setembro("temperatura", "Temperature", "media")
dias_sem_chuva <- setembro("numero_dias_sem_chuva", "NDWR", "max")   # maior sequência seca do mês

# Municípios do Amazonas (IBGE)
municipios <- st_read("~/infoamazoniaelnino/fire/data/AM_Municipios_2025/AM_Municipios_2025.shp",
                      quiet = TRUE) |>
  st_transform(4326)
mun <- vect(municipios)

media_mun <- \(r) extract(r, mun, fun = mean, na.rm = TRUE, touches = TRUE)[, 2]

tabela <- municipios |>
  st_drop_geometry() |>
  transmute(
    codigo_ibge          = CD_MUN,
    municipio            = NM_MUN,
    chuva_mm_dia         = media_mun(chuva),
    temperatura_max_c    = media_mun(temperatura),
    dias_sem_chuva_max   = media_mun(dias_sem_chuva)
  )

# Temperatura às vezes vem em Kelvin
if (mean(tabela$temperatura_max_c, na.rm = TRUE) > 200) {
  tabela$temperatura_max_c <- tabela$temperatura_max_c - 273.15
}

tabela |>
  mutate(across(where(is.double), \(x) round(x, 1))) |>
  write_csv(file.path(pasta, "clima_setembro_2026_municipios.csv"))