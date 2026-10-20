# =============================================================================
# Dias sem chuva no estado do Amazonas em 2025 (total do ano e por mês)
# Fonte: INPE / Programa Queimadas (precipitação observada do Risco de Fogo)
# Leitura do resultado: "em média, um ponto do Amazonas teve X dias sem chuva"
# =============================================================================

library(tidyverse)
library(sf)
library(terra)

ano <- 2025

# Tudo de 2025 fica numa pasta própria, separado dos dados de 2026
pasta       <- file.path("~/queimadaselnino/clima", ano)
pasta_dados <- file.path(pasta, "data")
dir.create(pasta_dados, recursive = TRUE, showWarnings = FALSE)

base <- "https://dataserver-coids.inpe.br/queimadas/queimadas/riscofogo_meteorologia/observado"
datas <- seq(as.Date(paste0(ano, "-01-01")), as.Date(paste0(ano, "-12-31")), by = "day")
dias  <- format(datas, "%Y%m%d")

limite_chuva_mm <- 1   # chuva < 1 mm no dia = dia sem chuva

# -----------------------------------------------------------------------------
# 1. Download dos 365 dias (para se faltar algum)
# -----------------------------------------------------------------------------

arquivos <- file.path(pasta_dados,
                      sprintf("INPE_FireRiskModel_2.2_Precipitation_%s.nc", dias))
for (a in arquivos[!file.exists(arquivos)]) {
  ok <- try(download.file(file.path(base, "precipitacao", basename(a)), a,
                          mode = "wb", quiet = TRUE), silent = TRUE)
  if (inherits(ok, "try-error") && file.exists(a)) file.remove(a)
}
faltando <- dias[!file.exists(arquivos) | file.size(arquivos) == 0]
if (length(faltando) > 0) stop("Faltam os dias: ", paste(faltando, collapse = ", "))

chuva <- rast(arquivos)
if (nlyr(chuva) != length(dias)) stop("Esperava ", length(dias), " camadas, veio ", nlyr(chuva))
if (crs(chuva) == "") crs(chuva) <- "EPSG:4326"

# -----------------------------------------------------------------------------
# 2. Polígono do estado (une os 62 municípios) e recorte
# -----------------------------------------------------------------------------

am <- st_read("~/queimadaselnino/fire/data/AM_Municipios_2025/AM_Municipios_2025.shp",
              quiet = TRUE) |>
  st_transform(4326) |>
  st_union() |>
  vect()

chuva <- crop(chuva, am)
seco  <- chuva < limite_chuva_mm          # 1 = dia sem chuva, 0 = choveu

# -----------------------------------------------------------------------------
# 3. Contagem por pixel: ano inteiro e cada mês
# -----------------------------------------------------------------------------

seco_ano <- sum(seco)                                   # NA se faltou dado no pixel
seco_mes <- tapp(seco, index = month(datas), fun = sum) # 12 camadas, uma por mês
names(seco_mes) <- c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun",
                     "Jul", "Ago", "Set", "Out", "Nov", "Dez")

# -----------------------------------------------------------------------------
# 4. Média estadual ponderada pela fração de cada pixel dentro do estado
#    + quanto da área tem dado (deve dar 1)
# -----------------------------------------------------------------------------

media_estado <- function(r) {
  v <- extract(r, am, exact = TRUE) |> as_tibble()
  w <- v$fraction
  v |>
    select(-ID, -fraction) |>
    summarise(across(everything(), list(
      dias          = \(x) weighted.mean(x, w, na.rm = TRUE),
      frac_com_dado = \(x) sum(w[!is.na(x)]) / sum(w)
    )))
}

# Ano inteiro
media_estado(seco_ano)

# Por mês
por_mes <- media_estado(seco_mes) |>
  pivot_longer(everything(),
               names_to = c("mes", ".value"),
               names_pattern = "(.*)_(dias|frac_com_dado)") |>
  mutate(dias_no_mes = days_in_month(as.Date(paste0(ano, "-", 1:12, "-01"))),
         dias = round(dias, 1))
por_mes

write_csv(por_mes, file.path(pasta, paste0("dias_sem_chuva_AM_", ano, "_por_mes.csv")))

# Bônus: % da área do estado que passou o MÊS INTEIRO sem chuva
area_mes_todo_seco <- (seco_mes == as.vector(por_mes$dias_no_mes)) * 100
media_estado(area_mes_todo_seco)
