# =============================================================================
# Queimadas no bioma Amazônia, 2021–2026: limpeza para Datawrapper
# Fonte: INPE / Programa Queimadas, satélite de referência (AQUA_M-T)
# =============================================================================

library(tidyverse)
library(lubridate)
library(sf)

pasta <- "~/queimadaselnino/fire"
saida <- file.path(pasta, "limpos")
dir.create(saida, showWarnings = FALSE)
dados <- file.path(pasta, "data")

# -----------------------------------------------------------------------------
# 1. Leitura e padronização
# -----------------------------------------------------------------------------

# Anuais 2021–2025: Brasil inteiro, todos os biomas -> filtra Amazônia
arquivos_anuais <- list.files(dados, pattern = "focos_br_ref_\\d{4}\\.csv$",
                              full.names = TRUE)

anuais <- map_dfr(arquivos_anuais,
                  ~ read_csv(.x, col_types = cols(.default = "c"))) |>
  filter(bioma == "Amazônia") |>
  transmute(
    data_hora = ymd_hms(data_pas),
    estado,
    municipio,
    lat = as.numeric(lat),
    lon = as.numeric(lon)
  )

# 2026: formato do BDQueimadas (nomes de coluna diferentes)
# 2026: um ou mais arquivos do BDQueimadas (podem se sobrepor nas datas)
arquivos_2026 <- list.files(dados, pattern = "bdqueimadas_2026.*\\.csv$",
                            full.names = TRUE)

focos_2026 <- map_dfr(arquivos_2026,
                      ~ read_csv(.x, col_types = cols(.default = "c"))) |>
  filter(Satelite == "AQUA_M-T", Bioma == "Amazônia") |>
  transmute(
    data_hora = ymd_hms(DataHora),
    estado    = Estado,
    municipio = Municipio,
    lat       = as.numeric(Latitude),
    lon       = as.numeric(Longitude)
  ) |>
  distinct(data_hora, lat, lon, .keep_all = TRUE)  # remove os focos repetidos (1–2/10)

focos <- bind_rows(anuais, focos_2026) |>
  mutate(ano = year(data_hora), mes = month(data_hora))

# Conferência: um total por ano, sem anos faltando
count(focos, ano)
ultima_data <- as_date(max(focos_2026$data_hora))
ultima_data

# Corte comum a todos os anos (01/01 até o mesmo dia/mês da última data de 2026)
corte <- format(ultima_data, "%m%d")

# Último mês COMPLETO de 2026 (se a última data não é fim de mês, o mês fica de fora)
ultimo_mes_completo <- if (day(ultima_data + 1) == 1) month(ultima_data) else month(ultima_data) - 1

# -----------------------------------------------------------------------------
# 2. CSV 1: focos por mês (linhas = meses, colunas = anos)
# -----------------------------------------------------------------------------

meses <- c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun",
           "Jul", "Ago", "Set", "Out", "Nov", "Dez")

focos |>
  count(ano, mes, name = "focos") |>
  complete(ano, mes = 1:12, fill = list(focos = 0)) |>
  mutate(focos = if_else(ano == 2026 & mes > ultimo_mes_completo, NA, focos)) |>
  arrange(mes) |>
  mutate(mes = meses[mes]) |>
  pivot_wider(names_from = ano, values_from = focos) |>
  write_csv(file.path(saida, "focos_mensais.csv"), na = "")

# Números para o texto (só console): 01/01 até a mesma data em todos os anos
focos |>
  filter(format(data_hora, "%m%d") <= corte) |>
  count(ano, name = "focos_ate_a_data")

# CSV 1b: focos por mês, incluindo outubro de 2026 (parcial, até ultima_data)
focos |>
  count(ano, mes, name = "focos") |>
  complete(ano, mes = 1:12, fill = list(focos = 0)) |>
  mutate(focos = if_else(ano == 2026 & mes > month(ultima_data), NA, focos)) |>
  arrange(mes) |>
  mutate(mes = meses[mes]) |>
  pivot_wider(names_from = ano, values_from = focos) |>
  write_csv(file.path(saida, "focos_mensais_com_out.csv"), na = "")

# -----------------------------------------------------------------------------
# 3. CSVs 2 e 3: focos por distância de Manaus
# -----------------------------------------------------------------------------

manaus_lat <- -3.1189
manaus_lon <- -60.0217

# Agosto, setembro e outubro, cortados na mesma data em todos os anos
# (outubro de 2026 está incompleto: o corte compara os mesmos dias em todos os anos)
focos_dist <- focos |>
  filter(mes %in% 8:10, format(data_hora, "%m%d") <= corte) |>
  mutate(dist_km = 6371 * 2 * asin(sqrt(
    sin((lat - manaus_lat) * pi / 360)^2 +
      cos(manaus_lat * pi / 180) * cos(lat * pi / 180) *
      sin((lon - manaus_lon) * pi / 360)^2))) |>
  mutate(faixa = cut(dist_km, c(0, 50, 100, 300, 500, 800, Inf), include.lowest = TRUE,
                     labels = c("Até 50 km", "50–100 km", "100–300 km", "300–500 km",
                                "500–800 km", "Mais de 800 km")))

# Tabela: linhas = anos, colunas = faixas
tabela_faixas <- function(df) {
  df |>
    count(ano, faixa) |>
    complete(ano = 2021:2026, faixa, fill = list(n = 0)) |>
    pivot_wider(names_from = faixa, values_from = n)
}

# CSV 2: agosto + setembro
focos_dist |>
  filter(mes %in% 8:9) |>
  tabela_faixas() |>
  write_csv(file.path(saida, "focos_distancia_manaus_ago_set.csv"))

# CSV 3: só setembro
focos_dist |>
  filter(mes == 9) |>
  tabela_faixas() |>
  write_csv(file.path(saida, "focos_distancia_manaus_set.csv"))

# CSVs 4 e 5: % dos focos em cada faixa de distância (cada linha soma 100%)
tabela_pct <- function(df) {
  df |>
    count(ano, faixa) |>
    complete(ano = 2021:2026, faixa, fill = list(n = 0)) |>
    group_by(ano) |>
    mutate(focos_total = sum(n),
           pct = round(100 * n / focos_total, 1)) |>
    ungroup() |>
    select(-n) |>
    pivot_wider(names_from = faixa, values_from = pct) |>
    relocate(focos_total, .after = last_col())
}

# CSV 4: agosto + setembro
focos_dist |>
  filter(mes %in% 8:9) |>
  tabela_pct() |>
  write_csv(file.path(saida, "pct_distancia_manaus_ago_set.csv"))

# CSV 5: só setembro
focos_dist |>
  filter(mes == 9) |>
  tabela_pct() |>
  write_csv(file.path(saida, "pct_distancia_manaus_set.csv"))

# CSV 6: número de focos em setembro até 300 km de Manaus
focos_dist |>
  filter(mes == 9, dist_km <= 300) |>
  count(ano, name = "focos_ate_300km") |>
  complete(ano = 2021:2026, fill = list(focos_ate_300km = 0)) |>
  write_csv(file.path(saida, "focos_set_ate_300km.csv"))

# CSV 7: % dos focos de setembro perto de Manaus (até 100 e até 300 km)
focos_dist |>
  filter(mes == 9) |>
  group_by(ano) |>
  summarise(
    focos_total   = n(),
    ate_100km     = sum(dist_km <= 100),
    ate_300km     = sum(dist_km <= 300),
    pct_ate_100km = round(100 * ate_100km / focos_total, 1),
    pct_ate_300km = round(100 * ate_300km / focos_total, 1)
  ) |>
  write_csv(file.path(saida, "pct_focos_perto_manaus_set.csv"))

# -----------------------------------------------------------------------------
# CSVs 8 a 11: OUTUBRO (mesmas tabelas de setembro)
# Em 2026 outubro está incompleto; todos os anos vão só até o mesmo dia (corte)
# -----------------------------------------------------------------------------

focos_out <- focos_dist |> filter(mes == 10)

# CSV 8: nº de focos por faixa de distância, outubro
focos_out |>
  tabela_faixas() |>
  write_csv(file.path(saida, "focos_distancia_manaus_out.csv"))

# CSV 9: % por faixa de distância, outubro
focos_out |>
  tabela_pct() |>
  write_csv(file.path(saida, "pct_distancia_manaus_out.csv"))

# CSV 10: nº de focos em outubro até 300 km de Manaus
focos_out |>
  filter(dist_km <= 300) |>
  count(ano, name = "focos_ate_300km") |>
  complete(ano = 2021:2026, fill = list(focos_ate_300km = 0)) |>
  write_csv(file.path(saida, "focos_out_ate_300km.csv"))

# CSV 11: % dos focos de outubro perto de Manaus (até 100 e até 300 km)
focos_out |>
  group_by(ano) |>
  summarise(
    focos_total   = n(),
    ate_100km     = sum(dist_km <= 100),
    ate_300km     = sum(dist_km <= 300),
    pct_ate_100km = round(100 * ate_100km / focos_total, 1),
    pct_ate_300km = round(100 * ate_300km / focos_total, 1)
  ) |>
  write_csv(file.path(saida, "pct_focos_perto_manaus_out.csv"))

# Até que dia de outubro os CSVs 8 a 11 vão (mesmo dia em todos os anos)
ultima_data

# CSV 12: focos de 1º/ago a 14/out, somados, por ano (mesmo corte em todos os anos)
focos |>
  filter(format(data_hora, "%m%d") >= "0801",
         format(data_hora, "%m%d") <= "1014") |>
  count(ano, name = "focos_ago_a_14out") |>
  complete(ano = 2021:2026, fill = list(focos_ago_a_14out = 0)) |>
  write_csv(file.path(saida, "focos_ago_a_14out.csv"))

# CSV 13: focos de 1º/ago a 14/out a até 300 km de Manaus, por ano
focos_dist |>
  filter(format(data_hora, "%m%d") <= "1014") |>
  group_by(ano) |>
  summarise(
    focos_total   = n(),
    focos_ate_300km = sum(dist_km <= 300),
    pct_ate_300km = round(100 * focos_ate_300km / focos_total, 1)
  ) |>
  write_csv(file.path(saida, "focos_ago_a_14out_ate_300km.csv"))