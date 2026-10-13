# =============================================================================
# Piores dias de fumaça em Manaus — setembro de 2026
# Dados: PurpleAir, médias horárias de 15 sensores externos
#
# O que o script faz:
#   1. Lê todos os CSVs baixados da API do PurpleAir (endpoint history/csv)
#   2. Descarta horas em que os dois canais do sensor (A e B) discordam
#   3. Aplica a correção da EPA para PurpleAir (inclui ajuste para fumaça)
#   4. Calcula a média diária por sensor no horário de Manaus (UTC-4)
#   5. Junta os sensores num valor diário para a cidade e ranqueia os dias
#
# Saídas (na pasta "air", um nível acima de "data"):
#   - qualidade_por_sensor.csv  -> quantas horas cada sensor teve e quantas
#                                  foram descartadas
#   - diario_por_sensor.csv     -> média diária de cada sensor
#   - ranking_manaus.csv        -> todos os dias de setembro, do pior ao melhor
#   - picos_horarios.csv        -> todas as horas válidas, da pior à melhor,
#                                  com valor bruto e corrigido
# =============================================================================

library(dplyr)
library(readr)
library(lubridate)
library(purrr)

# ---- Configuração ----------------------------------------------------------

pasta_csv   <- "/Users/renatacarlosdaou/infoamazoniaelnino/air/data"  # CSVs baixados
pasta_saida <- "/Users/renatacarlosdaou/infoamazoniaelnino/air"       # onde salvar os resultados
limite_oms <- 15                 # diretriz da OMS para PM2.5 em 24h (µg/m³)
min_horas  <- 18                 # horas válidas mínimas para o dia contar (75%)
fuso       <- "America/Manaus"

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
)

# ---- 1. Ler os CSVs ----------------------------------------------------------

converter_hora <- function(x) {
  if (inherits(x, "POSIXct")) return(with_tz(x, "UTC"))
  if (is.numeric(x))          return(as_datetime(x, tz = "UTC"))
  ymd_hms(x, tz = "UTC")
}

ler_csv <- function(arquivo) {
  d <- read_csv(arquivo, show_col_types = FALSE)
  
  # Se o CSV não trouxer a coluna sensor_index, tenta tirar do nome do arquivo
  if (!"sensor_index" %in% names(d)) {
    numero <- regmatches(basename(arquivo), regexpr("[0-9]{4,}", basename(arquivo)))
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

bruto <- map_dfr(arquivos, ler_csv) %>%
  distinct(sensor_index, hora_utc, .keep_all = TRUE) %>%   # remove horas repetidas
  mutate(
    hora_local = with_tz(hora_utc, fuso),
    data       = as_date(hora_local)
  ) %>%
  filter(year(data) == 2026, month(data) == 9)

if (any(is.na(bruto$sensor_index))) {
  warning("Há arquivos sem sensor_index identificável: ",
          paste(unique(bruto$arquivo[is.na(bruto$sensor_index)]), collapse = ", "))
}

# ---- 2. Controle de qualidade (canais A e B) ---------------------------------
# Passo 1: um canal com valor perto de 5000 está com defeito (é o valor de
#          saturação/falha do sensor, não fumaça). Ele é ignorado.
# Passo 2: se os dois canais estão bons, usa a média deles, mas só se
#          concordarem (critério da EPA: descarta quando diferem em mais de
#          5 µg/m³ E em mais de 70%).
# Passo 3: se só um canal está bom, usa esse canal sozinho e marca a hora
#          como "canal_unico", para você saber quanto do resultado depende disso.

limite_falha <- 1000  # acima disso, o canal é tratado como defeito

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

qualidade <- horario %>%
  group_by(sensor_index) %>%
  summarise(
    horas_no_arquivo  = n(),
    horas_validas     = sum(valida),
    horas_canal_unico = sum(valida & canal_unico),
    horas_descartadas = sum(!valida),
    canal_a_com_falha = sum(!a_ok),
    canal_b_com_falha = sum(!b_ok),
    .groups = "drop"
  ) %>%
  left_join(nomes_sensores, by = "sensor_index") %>%
  relocate(nome, .after = sensor_index)

print(qualidade, n = Inf)

# ---- 3. Correção da EPA ------------------------------------------------------
# Correção nacional da EPA (Barkjohn et al.), na versão estendida para fumaça
# de incêndios, a mesma usada no mapa Fire and Smoke da AirNow.
# pa = pm2.5_cf_1 (média dos canais, ou o canal bom); rh = umidade do sensor.
# Atenção: foi calibrada nos EUA. Para Manaus, trate como estimativa.

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
# Usa a MEDIANA entre sensores: um sensor com problema não puxa o resultado.
# O máximo mostra o bairro mais afetado naquele dia.

ranking <- diario_sensor %>%
  group_by(data) %>%
  summarise(
    sensores_validos   = n(),
    pm25_mediana       = median(pm25_media),
    pm25_max_sensor    = max(pm25_media),
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

# Dias com poucos sensores merecem cautela na matéria
poucos <- ranking %>% filter(sensores_validos < 5)
if (nrow(poucos) > 0) {
  cat("\nAtenção: dias com menos de 5 sensores válidos:\n")
  print(poucos %>% select(data, sensores_validos, pm25_mediana))
}

# ---- 5b. Piores horas ----------------------------------------------------------
# Médias de uma hora inteira (não picos de minutos), só de horas válidas.
# pm25_bruto = valor do sensor sem correção; pm25_corrigido = com correção EPA.
# Acima de ~260 a correção AUMENTA o valor e tem menos calibração: na matéria,
# é mais seguro citar o bruto ou dizer "passou de X".

picos <- horario_valido %>%
  left_join(nomes_sensores, by = "sensor_index") %>%
  arrange(desc(pa_cf1)) %>%
  transmute(
    nome, sensor_index,
    hora_manaus = format(hora_local, "%Y-%m-%d %H:%M"),  # texto no horário local
    
    canal_a = a, canal_b = b, canal_unico,
    pm25_bruto     = round(pa_cf1, 1),
    pm25_corrigido = round(pm25, 1)
  )

cat("\n20 piores horas (média horária por sensor, µg/m³):\n")
print(picos %>% slice_head(n = 20), width = Inf)

# ---- 6. Exportar -------------------------------------------------------------

write_csv(qualidade,     file.path(pasta_saida, "qualidade_por_sensor.csv"))
write_csv(diario_sensor, file.path(pasta_saida, "diario_por_sensor.csv"))
write_csv(ranking,       file.path(pasta_saida, "ranking_manaus.csv"))
write_csv(picos,         file.path(pasta_saida, "picos_horarios.csv"))