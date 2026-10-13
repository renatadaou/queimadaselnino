# =============================================================================
# Queimadas no bioma Amazônia, 2021–2026: limpeza para Datawrapper
# Fonte: INPE / Programa Queimadas, satélite de referência (AQUA_M-T)
# =============================================================================

library(tidyverse)
library(lubridate)
library(sf)
library(ggridges)   # install.packages("ggridges") se não tiver

pasta <- "~/infoamazoniaelnino/fire"
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
arquivo_2026 <- list.files(dados, pattern = "bdqueimadas_2026.*\\.csv$",
                           full.names = TRUE)

focos_2026 <- read_csv(arquivo_2026, col_types = cols(.default = "c")) |>
  filter(Satelite == "AQUA_M-T", Bioma == "Amazônia") |>
  transmute(
    data_hora = ymd_hms(DataHora),
    estado    = Estado,
    municipio = Municipio,
    lat       = as.numeric(Latitude),
    lon       = as.numeric(Longitude)
  )

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

# -----------------------------------------------------------------------------
# 4. Mapa: grade de mapinhos, anos (linhas) x meses ago–nov (colunas)
# -----------------------------------------------------------------------------

crs_m <- 5880   # SIRGAS 2000 / Brazil Polyconic: em metros, hexágonos e anéis sem distorção

bioma <- st_read(file.path(dados, "Biomas_250mil", "lm_bioma_250.shp"), quiet = TRUE) |>
  filter(Bioma == "Amazônia") |>
  st_transform(crs_m)

manaus <- st_sfc(st_point(c(manaus_lon, manaus_lat)), crs = 4326) |>
  st_transform(crs_m)

# Anéis de distância (mesmas faixas do CSV)
aneis <- do.call(rbind, lapply(c(100, 300, 500), \(km)
                               st_sf(km = km, geometry = st_boundary(st_buffer(manaus, km * 1000)))))

# Grade hexagonal de ~25 km cobrindo o bioma
hex <- st_make_grid(bioma, cellsize = 25000, square = FALSE) |>
  st_sf() |>
  mutate(hex_id = row_number())
hex <- hex[lengths(st_intersects(hex, st_union(bioma))) > 0, ]

# Focos de ago a nov, sem meses incompletos de 2026
pontos <- focos |>
  filter(mes %in% 8:11, !(ano == 2026 & mes > ultimo_mes_completo)) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  st_transform(crs_m)

contagem <- st_join(pontos, hex) |>
  st_drop_geometry() |>
  count(ano, mes, hex_id, name = "focos")

hex_focos <- hex |>
  inner_join(contagem, by = "hex_id") |>
  mutate(ano = factor(ano, levels = 2021:2026),
         mes = factor(meses[mes], levels = meses[8:11]))

# Para o QGIS (mapa de destaque de 2026): filtre por ano e mes
st_write(hex_focos, file.path(saida, "hex_focos_ago_nov.gpkg"), delete_dsn = TRUE, quiet = TRUE)

# Escala FIXA, igual nos 24 mapas (ajuste os cortes depois de ver os dados)
ggplot() +
  geom_sf(data = bioma, fill = "grey93", color = NA) +
  geom_sf(data = hex_focos, aes(fill = focos), color = NA) +
  geom_sf(data = aneis, fill = NA, color = "grey25", linewidth = 0.2, linetype = "dashed") +
  geom_sf(data = manaus, size = 0.6) +
  scale_fill_stepsn(colours = c("#FCDE5A", "#F2A623", "#E85D24", "#A32D2D", "#501313"),
                    breaks = c(5, 15, 30, 60, 120), name = "Focos por hexágono") +
  facet_grid(ano ~ mes, switch = "y") +
  theme_void() +
  theme(legend.position = "bottom", strip.text = element_text(size = 9))

ggsave(file.path(saida, "mapa_grade_focos_ago_nov.pdf"), width = 8, height = 11)

# -----------------------------------------------------------------------------
# 5. Mapa de pontos: focos de setembro até 300 km de Manaus, um mapa por ano
# -----------------------------------------------------------------------------

circulo_300 <- st_buffer(manaus, 300000)
anel_100    <- st_boundary(st_buffer(manaus, 100000))
xy_manaus   <- st_coordinates(manaus)

# Mesmo filtro do CSV 4 (distância calculada igual), então os totais batem
pontos_set <- focos_dist |>
  filter(mes == 9, dist_km <= 300) |>
  mutate(ano = factor(ano, levels = 2021:2026)) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  st_transform(crs_m)

totais <- pontos_set |>
  st_drop_geometry() |>
  count(ano, .drop = FALSE) |>
  mutate(rotulo = paste(format(n, big.mark = "."), "focos"),
         x = xy_manaus[1], y = xy_manaus[2] - 345000)

# Para o QGIS (se quiser pôr rios e municípios do polimapa por baixo)
st_write(pontos_set, file.path(saida, "pontos_focos_set_300km.gpkg"), delete_dsn = TRUE, quiet = TRUE)

ggplot() +
  geom_sf(data = circulo_300, fill = "grey96", color = "grey40", linewidth = 0.3) +
  geom_sf(data = anel_100, color = "grey55", linewidth = 0.2, linetype = "dashed") +
  geom_sf(data = pontos_set, color = "#D85A30", size = 0.35, alpha = 0.7) +
  geom_sf(data = manaus, size = 1.6) +
  geom_text(data = totais, aes(x = x, y = y, label = rotulo), size = 3.4, fontface = "bold") +
  coord_sf(datum = NA, clip = "off") +
  facet_wrap(~ ano, ncol = 3, drop = FALSE) +
  theme_void() +
  theme(strip.text = element_text(size = 12, face = "bold", margin = margin(b = 4)),
        panel.spacing = unit(1.2, "lines"))

ggsave(file.path(saida, "mapa_pontos_set_300km.pdf"), width = 9, height = 7)

# -----------------------------------------------------------------------------
# 6. Curvas de distância (ridgeline): onde o fogo de agosto + setembro se concentrou
# -----------------------------------------------------------------------------

focos_dist |>
  filter(mes %in% 8:9) |>
  mutate(ano = factor(ano, levels = 2026:2021),
         destaque = ano == "2026") |>
  ggplot(aes(x = dist_km, y = ano, fill = destaque)) +
  geom_density_ridges(scale = 1.6, color = "grey30", linewidth = 0.3, alpha = 0.8) +
  geom_vline(xintercept = c(100, 300), linetype = "dashed", color = "grey50", linewidth = 0.3) +
  scale_fill_manual(values = c(`TRUE` = "#D85A30", `FALSE` = "grey75"), guide = "none") +
  scale_x_continuous(breaks = c(0, 100, 300, 500, 800, 1200, 1600, 2000),
                     expand = c(0, 0), labels = \(x) paste(x, "km")) +
  coord_cartesian(xlim = c(0, 2000)) +
  labs(x = "Distância de Manaus", y = NULL) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank())

ggsave(file.path(saida, "curvas_distancia_manaus_ago_set.pdf"), width = 8, height = 6)
# -----------------------------------------------------------------------------
# 7. GeoPackage para o QGIS: focos de setembro + Manaus + anéis + bioma
#    (abra com o script mapa_setembro_qgis.py)
# -----------------------------------------------------------------------------

mapa_gpkg <- file.path(saida, "mapa_setembro.gpkg")
if (file.exists(mapa_gpkg)) file.remove(mapa_gpkg)

focos_dist |>
  filter(mes == 9) |>
  transmute(ano, municipio, estado, dist_km = round(dist_km, 1),
            faixa = as.character(faixa), lat, lon) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE) |>
  st_transform(crs_m) |>
  st_write(mapa_gpkg, layer = "focos_setembro", quiet = TRUE)

st_write(st_sf(nome = "Manaus", geometry = manaus),
         mapa_gpkg, layer = "manaus", quiet = TRUE)

st_write(st_sf(km = c(100, 300),
               geometry = c(st_buffer(manaus, 100000), st_buffer(manaus, 300000))),
         mapa_gpkg, layer = "aneis", quiet = TRUE)

st_write(select(bioma, Bioma), mapa_gpkg, layer = "bioma", quiet = TRUE)
