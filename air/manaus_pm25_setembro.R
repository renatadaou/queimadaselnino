# =============================================================================
# Piores dias e horas de fumaça em Manaus — setembro de 2026
# Dados: PurpleAir, médias horárias de 15 sensores externos
#
# O que o script faz:
#   1. Lê os CSVs baixados da PurpleAir e confere sensor, intervalo e bordas
#   2. Descarta horas em que os dois canais do sensor (A e B) discordam
#   3. Aplica a correção da EPA para PurpleAir (inclui ajuste para fumaça)
#   4. Calcula a média diária por sensor no horário de Manaus (UTC-4)
#   5. Junta os sensores num valor diário para a cidade e ranqueia os dias
#   6. Lista as piores horas
#
# Saídas (na pasta "air"):
#   - qualidade_por_sensor.csv  -> horas, descartes e dias válidos por sensor
#   - diario_por_sensor.csv     -> média diária de cada sensor
#   - ranking_manaus.csv        -> dias de setembro, do pior ao melhor
#   - picos_horarios.csv        -> horas válidas, da pior à melhor (corrigido)
# =============================================================================

library(dplyr)
library(readr)
library(lubridate)
library(purrr)

# ---- Configuração ----------------------------------------------------------

pasta_csv   <- "/Users/renatacarlosdaou/queimadaselnino/air/data"
pasta_saida <- "/Users/renatacarlosdaou/queimadaselnino/air"
limite_oms   <- 15    # diretriz da OMS para PM2.5 em 24h (µg/m³)
min_horas    <- 18    # horas válidas mínimas para o dia contar (75%)
limite_falha <- 1000  # canal acima disso é tratado como defeito (Plantower satura perto daqui)
limite_calib <- 500   # bruto acima disso: correção com poucos dados, cite como "passou de X"
fuso         <- "America/Manaus"
horas_no_mes <- 30 * 24

nomes_sensores <- tribble(
  ~sensor_index, ~nome,
  31509,  "UEA-EST",
  315615, "MIT_NAMA_UFAM",
  98395,  "FAS-Ar",
  161259, "UEA_EDUCAIR_2",
  161261, "UEA_EducAIR_1",
  161279, "UEA_EducAIR_5",
  161291, "UEA_EducAIR_6",
  165047, "UEA_EducAIR_14",
  165067, "UEA_EducAIR_11",
  165131, "UEA_EducAIR_17",
  177605, "UEA_EducAIR_26",
  181801, "UEA_EducAIR_31",
  181825, "UEA_EducAIR_32",
  181835, "UEA_EducAIR_33",
  205957, "SEMA_MANAUS"
) %>%
  mutate(sensor_index = as.integer(sensor_index))

# ---- 1. Ler os CSVs e conferir -----------------------------------------------

converter_hora <- function(x) {
  if (inherits(x, "POSIXct")) return(with_tz(x, "UTC"))
  if (is.numeric(x))          return(as_datetime(x, tz = "UTC"))
  ymd_hms(x, tz = "UTC")
}

ler_csv <- function(arquivo) {
  d <- read_csv(arquivo, show_col_types = FALSE)
  
  # Sem coluna sensor_index: tenta o nome do arquivo (5 ou 6 dígitos, para não
  # confundir com um ano). Logo abaixo tudo é conferido contra a lista.
  if (!"sensor_index" %in% names(d)) {
    numero <- regmatches(basename(arquivo),
                         regexpr("(?<![0-9])[0-9]{5,6}(?![0-9])", basename(arquivo), perl = TRUE))
    d$sensor_index <- if (length(numero) == 1) as.integer(numero) else NA_integer_
  }
  
  d %>%
    mutate(
      sensor_index = as.integer(sensor_index),
      hora_utc     = converter_hora(time_stamp),
      arquivo      = basename(arquivo)
    )
}

arquivos <- list.files(pasta_csv, pattern = "\\.csv$", full.names = TRUE)
stopifnot("Nenhum CSV encontrado na pasta" = length(arquivos) > 0)

lidos <- map_dfr(arquivos, ler_csv)

# (a) Todo arquivo precisa ter um sensor da lista. Isso vem ANTES do distinct():
#     dois arquivos com sensor NA seriam tratados como o mesmo sensor.
fora_da_lista <- lidos %>%
  filter(is.na(sensor_index) | !sensor_index %in% nomes_sensores$sensor_index) %>%
  distinct(arquivo, sensor_index)
if (nrow(fora_da_lista) > 0) {
  print(fora_da_lista)
  stop("Arquivo com sensor_index ausente ou fora da lista de sensores")
}
if (anyNA(lidos$hora_utc)) stop("Há horários que não foram lidos (time_stamp em formato inesperado)")

sem_repeticao <- lidos %>%
  distinct(sensor_index, hora_utc, .keep_all = TRUE)   # remove horas repetidas entre downloads

# (b) Todos os arquivos precisam ser médias de 60 minutos
intervalos <- sem_repeticao %>%
  group_by(sensor_index) %>%
  summarise(intervalo_min = median(diff(sort(as.numeric(hora_utc)))) / 60, .groups = "drop")
if (any(abs(intervalos$intervalo_min - 60) > 1)) {
  print(intervalos, n = Inf)
  stop("Algum sensor não está em médias horárias: baixe de novo com média de 60 minutos")
}

bruto <- sem_repeticao %>%
  mutate(
    hora_local = with_tz(hora_utc, fuso),
    data       = as_date(hora_local)
  ) %>%
  filter(year(data) == 2026, month(data) == 9)

# (c) Bordas do mês: 1/9 e 30/9 no horário de Manaus precisam de 24 horas.
#     Se faltar, pode ser download pedido em UTC (baixe até 01/10 04:00 UTC)
#     ou só o sensor fora do ar.
bordas <- bruto %>%
  filter(data %in% as_date(c("2026-09-01", "2026-09-30"))) %>%
  count(sensor_index, data, name = "horas") %>%
  filter(horas < 24)
if (nrow(bordas) > 0) {
  message("Atenção: sensores com menos de 24 horas em 1/9 ou 30/9:")
  print(bordas)
}

# ---- 2. Controle de qualidade (canais A e B) ---------------------------------
# Passo 1: canal acima de limite_falha está com defeito e é ignorado.
# Passo 2: com os dois canais bons, usa a média, mas só se concordarem
#          (critério da EPA: descarta quando diferem em mais de 5 µg/m³ E 70%).
# Passo 3: com só um canal bom, usa esse canal e marca "canal_unico".

horario <- bruto %>%
  rename(a = `pm2.5_cf_1_a`, b = `pm2.5_cf_1_b`, umidade = humidity) %>%
  mutate(
    a_ok        = !is.na(a) & a >= 0 & a < limite_falha,
    b_ok        = !is.na(b) & b >= 0 & b < limite_falha,
    dif_abs     = abs(a - b),
    dif_rel     = if_else((a + b) > 0, dif_abs / ((a + b) / 2), 0),
    concordam   = a_ok & b_ok & !(dif_abs > 5 & dif_rel > 0.7),
    canal_unico = xor(a_ok, b_ok),
    pa_cf1      = case_when(
      concordam         ~ (a + b) / 2,
      a_ok & !b_ok      ~ a,
      b_ok & !a_ok      ~ b,
      TRUE              ~ NA_real_
    ),
    valida      = !is.na(pa_cf1) & !is.na(umidade)
  )

# ---- 3. Correção da EPA ------------------------------------------------------
# Correção nacional da EPA (Barkjohn et al.), versão estendida para fumaça,
# a mesma do mapa Fire and Smoke da AirNow.
# Atenção: foi calibrada nos EUA. Para Manaus, trate como estimativa.
#
# Como a correção mexe nos números:
#   - abaixo de ~340 µg/m³ (bruto), o corrigido é MENOR que o bruto
#     (ex.: bruto 40 -> corrigido ~25). Citar o bruto infla o valor.
#   - acima de ~340, o corrigido passa a ser maior que o bruto.
#   - acima de ~500 há pouca calibração: cite "passou de X".

corrigir_epa <- function(pa, rh) {
  w1 <- pa / 20 - 3 / 2
  w2 <- pa / 50 - 21 / 5
  pm <- case_when(
    pa < 30  ~ 0.524 * pa - 0.0862 * rh + 5.75,
    pa < 50  ~ (0.786 * w1 + 0.524 * (1 - w1)) * pa - 0.0862 * rh + 5.75,
    pa < 210 ~ 0.786 * pa - 0.0862 * rh + 5.75,
    pa < 260 ~ (0.69 * w2 + 0.786 * (1 - w2)) * pa - 0.0862 * rh * (1 - w2) +
      2.966 * w2 + 5.75 * (1 - w2) + 8.84e-4 * pa^2 * w2,
    TRUE     ~ 2.966 + 0.69 * pa + 8.84e-4 * pa^2
  )
  pmax(pm, 0)
}

horario_valido <- horario %>%
  filter(valida) %>%
  mutate(pm25 = corrigir_epa(pa_cf1, umidade))

# ---- 4. Média diária por sensor ----------------------------------------------

diario_sensor <- horario_valido %>%
  group_by(sensor_index, data) %>%
  summarise(
    horas_validas     = n(),
    horas_canal_unico = sum(canal_unico),
    pm25_media        = mean(pm25),
    pm25_pico_hora    = max(pm25),
    .groups = "drop"
  ) %>%
  filter(horas_validas >= min_horas) %>%
  left_join(nomes_sensores, by = "sensor_index") %>%
  relocate(nome, .after = sensor_index)

# ---- 5. Valor diário da cidade e ranking -------------------------------------
# MEDIANA entre sensores: um sensor com problema não puxa o resultado.
# O máximo mostra o sensor mais afetado naquele dia.

ranking <- diario_sensor %>%
  group_by(data) %>%
  summarise(
    sensores_validos    = n(),
    pm25_mediana        = median(pm25_media),
    pm25_max_sensor     = max(pm25_media),
    sensor_mais_afetado = nome[which.max(pm25_media)],
    .groups = "drop"
  ) %>%
  mutate(
    vezes_limite_oms = pm25_mediana / limite_oms,
    acima_oms        = pm25_mediana > limite_oms
  ) %>%
  arrange(desc(pm25_mediana)) %>%
  mutate(posicao = row_number(), .before = 1)

cat("\nDias com valor válido:", nrow(ranking), "de 30\n")
cat("Dias acima do limite da OMS:", sum(ranking$acima_oms), "\n\n")
cat("10 piores dias (mediana entre sensores, µg/m³):\n")
print(ranking %>% slice_head(n = 10), width = Inf)

poucos <- ranking %>% filter(sensores_validos < 5)
if (nrow(poucos) > 0) {
  cat("\nAtenção: dias com menos de 5 sensores válidos:\n")
  print(poucos %>% select(data, sensores_validos, pm25_mediana))
}

# ---- 6. Piores horas -----------------------------------------------------------
# Médias de uma hora inteira (não picos de minutos), só de horas válidas,
# ordenadas pelo valor CORRIGIDO, que é o que deve ir para a matéria.
# fora_da_calibracao = TRUE: cite como "passou de X" em vez do número exato.

picos <- horario_valido %>%
  left_join(nomes_sensores, by = "sensor_index") %>%
  arrange(desc(pm25)) %>%
  transmute(
    nome, sensor_index,
    hora_manaus        = format(hora_local, "%Y-%m-%d %H:%M"),  # horário local
    canal_a = a, canal_b = b, canal_unico, umidade,
    pm25_bruto         = round(pa_cf1, 1),
    pm25_corrigido     = round(pm25, 1),
    fora_da_calibracao = pa_cf1 > limite_calib
  )

cat("\n20 piores horas (média horária por sensor, µg/m³, corrigido):\n")
print(picos %>% slice_head(n = 20), width = Inf)

# ---- 7. Qualidade por sensor -------------------------------------------------
# Inclui os 15 sensores, mesmo os que não trouxeram nenhuma hora em setembro.

qualidade <- horario %>%
  group_by(sensor_index) %>%
  summarise(
    horas_no_arquivo  = n(),
    horas_validas     = sum(valida),
    horas_canal_unico = sum(valida & canal_unico),
    horas_descartadas = sum(!valida),
    canal_a_com_falha = sum(!a_ok),
    canal_b_com_falha = sum(!b_ok),
    canais_discordam  = sum(a_ok & b_ok & !concordam),
    umidade_ausente   = sum(is.na(umidade)),
    .groups = "drop"
  ) %>%
  left_join(diario_sensor %>% count(sensor_index, name = "dias_validos"),
            by = "sensor_index") %>%
  right_join(nomes_sensores, by = "sensor_index") %>%
  mutate(
    across(-c(sensor_index, nome), \(x) coalesce(x, 0L)),
    horas_faltando = horas_no_mes - horas_no_arquivo
  ) %>%
  relocate(nome, .after = sensor_index) %>%
  arrange(desc(horas_validas))

print(qualidade, n = Inf, width = Inf)

# ---- 8. Exportar -------------------------------------------------------------

write_csv(qualidade,     file.path(pasta_saida, "qualidade_por_sensor.csv"))
write_csv(diario_sensor, file.path(pasta_saida, "diario_por_sensor.csv"))
write_csv(ranking,       file.path(pasta_saida, "ranking_manaus.csv"))
write_csv(picos,         file.path(pasta_saida, "picos_horarios.csv"))