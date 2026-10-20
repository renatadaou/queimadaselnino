# =============================================================================
# Clima de setembro de 2026 por município do Amazonas
# Fonte: INPE / Programa Queimadas (dados observados do Risco de Fogo)
# Saída: (1) dias sem chuva em setembro e (2) temperatura máxima média
# =============================================================================

library(tidyverse)
library(sf)
library(terra)   # install.packages("terra")

pasta <- "~/queimadaselnino/clima"
dir.create(file.path(pasta, "data"), recursive = TRUE, showWarnings = FALSE)

base <- "https://dataserver-coids.inpe.br/queimadas/queimadas/riscofogo_meteorologia/observado"
dias <- format(seq(as.Date("2026-09-01"), as.Date("2026-09-30"), by = "day"), "%Y%m%d")

# Quanto de chuva (mm no dia) ainda conta como "dia sem chuva".
# 1 mm é a convenção mais comum em climatologia. Diga qual usou na metodologia.
limite_chuva_mm <- 1

# -----------------------------------------------------------------------------
# 1. Download: pilha com 30 camadas (uma por dia). Para se faltar algum dia.
# -----------------------------------------------------------------------------

baixa_setembro <- function(pasta_inpe, nome) {
  arquivos <- file.path(pasta, "data",
                        sprintf("INPE_FireRiskModel_2.2_%s_%s.nc", nome, dias))
  for (a in arquivos[!file.exists(arquivos)]) {
    ok <- try(download.file(file.path(base, pasta_inpe, basename(a)), a,
                            mode = "wb", quiet = TRUE), silent = TRUE)
    # não deixa arquivo vazio ou pela metade para trás
    if (inherits(ok, "try-error") && file.exists(a)) file.remove(a)
  }
  faltando <- dias[!file.exists(arquivos) | file.size(arquivos) == 0]
  if (length(faltando) > 0) {
    stop(nome, ": faltam os dias ", paste(faltando, collapse = ", "))
  }
  r <- rast(arquivos)
  if (nlyr(r) != 30) stop(nome, ": esperava 30 camadas, veio ", nlyr(r))
  if (crs(r) == "") crs(r) <- "EPSG:4326"
  r
}

chuva_diaria <- baixa_setembro("precipitacao", "Precipitation")
temp_diaria  <- baixa_setembro("temperatura",  "Temperature")

# CONFERÊNCIA: o que é exatamente a variável de temperatura?
# Precisa ser a MÁXIMA do dia para a coluna se chamar "temperatura máxima média".
longnames(temp_diaria)[1]
units(temp_diaria)[1]
global(temp_diaria[[1]], range, na.rm = TRUE)   # ~295–315 = Kelvin; ~22–42 = °C

# -----------------------------------------------------------------------------
# 2. Os dois indicadores, pixel a pixel
# -----------------------------------------------------------------------------

# (1) Dias sem chuva: quantos dos 30 dias de setembro tiveram chuva < limite
#     (NA se o pixel não tem dado em algum dia: melhor ver o buraco do que esconder)
dias_sem_chuva <- sum(chuva_diaria < limite_chuva_mm)

# (2) Temperatura máxima média: média das 30 máximas diárias
temp_max_media <- mean(temp_diaria, na.rm = TRUE)
if (global(temp_max_media, mean, na.rm = TRUE)[1, 1] > 200) {
  temp_max_media <- temp_max_media - 273.15   # Kelvin -> °C
}

# -----------------------------------------------------------------------------
# 3. Média por município (ponderada pela fração de cada pixel dentro dele)
# -----------------------------------------------------------------------------

municipios <- st_read("~/queimadaselnino/fire/data/AM_Municipios_2025/AM_Municipios_2025.shp",
                      quiet = TRUE) |>
  st_transform(4326)
mun <- vect(municipios)

media_mun <- function(r) {
  v <- extract(r, mun, exact = TRUE) |>
    as_tibble() |>
    rename(valor = 2) |>
    group_by(ID) |>
    summarise(valor = weighted.mean(valor, fraction, na.rm = TRUE)) |>
    complete(ID = seq_len(nrow(municipios))) |>   # município sem pixel vira NA, não some
    arrange(ID) |>
    pull(valor)
  stopifnot(length(v) == nrow(municipios))
  v
}

tabela <- municipios |>
  st_drop_geometry() |>
  transmute(
    codigo_ibge          = CD_MUN,
    municipio            = NM_MUN,
    dias_sem_chuva_set   = media_mun(dias_sem_chuva),
    temp_max_media_c     = media_mun(temp_max_media)
  )

# Conferência: 62 municípios, nada vazio, valores plausíveis
nrow(tabela)
summary(tabela)

tabela |>
  mutate(across(where(is.double), \(x) round(x, 1))) |>
  write_csv(file.path(pasta, "clima_setembro_2026_municipios.csv"))

