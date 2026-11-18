
# Queimadas e fumaça em Manaus - InfoAmazonia

[Matéria completa](https://infoamazonia.org/2026/10/19/fogo-se-aproxima-de-manaus-em-2026-e-mma-suspeita-de-queimadas-intencionais/)

Os gráficos da matéria foram feitos no Datawrapper, com os CSVs gerados pelos scripts abaixo.

## Parte 1 - Focos de calor e distância até Manaus

- Use `queimadas_amazonia.R` e os arquivos da pasta `fire/data`
- Os dados são do INPE / Programa Queimadas. Usei só o satélite de referência (AQUA_M-T) e filtrei para o bioma Amazônia
- Para 2021 a 2025 usei os arquivos anuais (`focos_br_ref_AAAA.csv`). Para 2026 usei os arquivos baixados do BDQueimadas, que têm nomes de colunas diferentes, então o código padroniza tudo antes de juntar
- Os arquivos de 2026 podem se sobrepor nas datas, então o código remove os focos repetidos (mesma data, hora, latitude e longitude)
- Como 2026 ainda não estava completo, as comparações entre anos vão de 1º de janeiro até o mesmo dia e mês da última data disponível em 2026
- `focos_mensais.csv` mostra os focos por mês e por ano. Os meses incompletos de 2026 ficam em branco. `focos_mensais_com_out.csv` é a mesma tabela, mas com outubro de 2026 parcial
- Para a distância, o código calcula a distância em linha reta (fórmula de haversine) entre cada foco e um ponto no centro de Manaus (-3.1189, -60.0217)
- Os focos são divididos em faixas: até 50 km, 50–100 km, 100–300 km, 300–500 km, 500–800 km e mais de 800 km
- A análise de distância usa só agosto, setembro e outubro, com o mesmo corte de data em todos os anos
- Saídas, todas na pasta `fire/limpos`:
  - `focos_distancia_manaus_ago_set.csv`, `_set.csv` e `_out.csv`: número de focos em cada faixa de distância
  - `pct_distancia_manaus_ago_set.csv`, `_set.csv` e `_out.csv`: % dos focos em cada faixa (cada linha soma 100%)
  - `focos_set_ate_300km.csv` e `focos_out_ate_300km.csv`: focos a até 300 km de Manaus
  - `pct_focos_perto_manaus_set.csv` e `_out.csv`: % dos focos a até 100 km e a até 300 km
  - `focos_ago_a_14out.csv`: total de focos de 1º de agosto a 14 de outubro, por ano
  - `focos_ago_a_14out_ate_300km.csv`: o mesmo período, só a até 300 km de Manaus

## Parte 2 - Clima de setembro

- Use `clima_amazonia.R`. O código baixa os dados sozinho, direto do [servidor do INPE](https://dataserver-coids.inpe.br/queimadas/queimadas/riscofogo_meteorologia/observado)
- São os dados observados de chuva e temperatura que o INPE usa no modelo de Risco de Fogo (versão 2.2), um arquivo NetCDF por dia, de 1º a 30 de setembro de 2026
- Se faltar algum dia, o código para e avisa quais dias estão faltando
- O código calcula dois indicadores em cada pixel:
  - Dias sem chuva: quantos dos 30 dias tiveram menos de 1 mm de chuva
  - Temperatura máxima média: média das 30 máximas diárias, em °C
- Depois faz a média por município, usando a malha municipal do IBGE de 2025 (`AM_Municipios_2025`, na pasta `fire/data`). Cada pixel tem peso de acordo com a parte dele que fica dentro do município
- Saída: `clima_setembro_2026_municipios.csv`, com os 62 municípios do Amazonas

## Parte 3 - Qualidade do ar em Manaus

- Use `manaus_pm25_setembro.R` e os CSVs da PurpleAir na pasta `air/data`
- São médias horárias de PM2.5 de 15 sensores externos em Manaus, em setembro de 2026: UEA-EST, MIT_NAMA_UFAM, FAS-Ar, SEMA_MANAUS e os sensores UEA_EducAIR 1, 2, 5, 6, 11, 14, 17, 26, 31, 32 e 33
- Os horários vêm em UTC e o código converte para o horário de Manaus (UTC-4). Para o dia 30 de setembro ficar completo, os dados precisam ir até 1º de outubro às 04:00 UTC
- Os sensores da PurpleAir têm dois canais (A e B). Para cada hora:
  - Um canal acima de 1.000 µg/m³ é tratado como defeito e ignorado
  - Se os dois canais funcionam, o código usa a média dos dois, mas descarta a hora se eles discordam em mais de 5 µg/m³ e em mais de 70% (critério da EPA)
  - Se só um canal funciona, o código usa esse canal e marca a hora como `canal_unico`
  - Horas sem umidade também são descartadas, porque a correção precisa dela
- Depois aplico a correção da EPA para PurpleAir (Barkjohn et al.), na versão para fumaça, a mesma do mapa Fire and Smoke da AirNow. Essa correção foi feita com dados dos EUA, então para Manaus os números são estimativas
- Abaixo de ~340 µg/m³, o valor corrigido é menor que o bruto. Por isso o número que deve ser citado é sempre o corrigido
- Acima de 500 µg/m³ (bruto) a correção tem poucos dados. Essas horas ficam marcadas como `fora_da_calibracao` e devem ser citadas como "passou de X"
- Um dia só conta para um sensor se tiver pelo menos 18 horas válidas (75%)
- O valor de Manaus em cada dia é a mediana entre os sensores, para que um sensor com problema não puxe o resultado. Esse valor é comparado com o limite da OMS para 24 horas (15 µg/m³)
- Saídas, na pasta `air`:
  - `ranking_manaus.csv`: dias de setembro, do pior ao melhor
  - `picos_horarios.csv`: piores horas, por sensor
  - `diario_por_sensor.csv`: média diária de cada sensor
  - `qualidade_por_sensor.csv`: quantas horas cada sensor teve, quantas foram descartadas e por quê
