# Clean permits ----
#
# Standardizes the municipal permit records. The rules follow
# `original/1_alvaras_trusted_pde.R`; the regression tests check that the
# output reproduces the version 3 cleaned snapshot.

import::from(
  dplyr,
  across,
  all_of,
  arrange,
  case_when,
  desc,
  distinct,
  everything,
  filter,
  first,
  if_else,
  mutate,
  na_if,
  rename,
  row_number,
  select,
  starts_with,
  where
)
import::from(lubridate, as_date, month, year)
import::from(
  stringr,
  str_detect,
  str_extract,
  str_remove,
  str_remove_all,
  str_replace_all,
  str_split_i,
  str_squish,
  str_starts
)

# Month names are fixed so the output does not depend on the system locale.
meses <- c(
  "JANEIRO",
  "FEVEREIRO",
  "MARÇO",
  "ABRIL",
  "MAIO",
  "JUNHO",
  "JULHO",
  "AGOSTO",
  "SETEMBRO",
  "OUTUBRO",
  "NOVEMBRO",
  "DEZEMBRO"
)

# Street-type prefixes removed when comparing addresses. `str_remove()` drops
# the first occurrence anywhere in the string, as in the original script.
tipos_logradouro <- paste0(
  "R |AV |ES |EST |VIA |AL |PC |PG |LV |TV |PV |LG |VD |RV |PQ |VP |RUA |",
  "Av. |Rua |PRAÇA |rua |AVENIDA |Avenida |Alameda |av |R. |AV. |Av "
)

numero_pattern <- "[-+]?[0-9]*\\.?[0-9]+"

clean_columns <- c(
  "id",
  "data_aprovacao",
  "mes",
  "ano",
  "alvara",
  "descricao",
  "descricao_tipo",
  "unidade_pmsp",
  "processo",
  "data_autuacao",
  "categoria_de_uso",
  "area_do_terreno",
  "area_da_construcao",
  "n_blocos",
  "n_pavimentos_por_bloco",
  "n_unidades_por_bloco",
  "n_pavimentos",
  "n_unidades",
  "unid_his",
  "unid_hmp",
  "unid_r2h_r2v",
  "sql_incra_11",
  "sql_incra",
  "endereco_raw",
  "endereco_unico",
  "endereco",
  "distrito",
  "subprefeitura",
  "zona_de_uso_registro",
  "coord_x",
  "coord_y",
  "georref_nivel",
  "georref_modo",
  "ind_loteamento",
  "ind_aprovacao",
  "ind_execucao",
  "ind_conclusao",
  "ind_correcao",
  "ind_edificacao_nova",
  "ind_r2v",
  "ind_r2h",
  "ind_his",
  "ind_hmp",
  "ind_ezeis",
  "ind_sql_incra_null",
  "ind_incra"
)

clean_alvaras <- function(alvaras_raw) {
  permits <- standardize_permits(alvaras_raw)
  permits <- parse_permit_values(permits)
  permits <- classify_permits(permits)
  permits <- harmonize_addresses(permits)
  permits <- select(permits, all_of(clean_columns))

  # A repeated permit number does not always mean a repeated permit. Records
  # are duplicates only when all of these fields match.
  permits <- distinct(
    permits,
    unidade_pmsp,
    subprefeitura,
    alvara,
    data_aprovacao,
    data_autuacao,
    descricao,
    endereco_raw,
    sql_incra,
    .keep_all = TRUE
  )
  return(permits)
}

# Standardize columns ----

standardize_permits <- function(alvaras_raw) {
  permits <- rename(
    alvaras_raw,
    alvara = n_documento,
    descricao = assunto,
    unidade_pmsp = unid_aprov,
    sql_incra = sql,
    zona_de_uso_registro = zona_uso,
    categoria_de_uso = uso_subcat,
    area_do_terreno = area_terreno,
    area_da_construcao = ac_total,
    n_blocos = blocos,
    n_pavimentos_por_bloco = pavimentos,
    n_unidades = unid_resid,
    data_aprovacao = data_emissao
  )
  permits <- mutate(permits, across(where(is.character), str_squish))
  permits <- filter(permits, !is.na(alvara))
  permits <- mutate(permits, id = row_number(), .before = everything())
  return(permits)
}

# Parse dates and numbers ----

parse_permit_date <- function(x) {
  # Only complete dates (10 characters) are kept.
  complete_dates <- if_else(nchar(x) == 10, x, NA_character_)
  dates <- as_date(str_replace_all(complete_dates, "/", "-"))
  return(dates)
}

parse_permit_values <- function(permits) {
  permits <- mutate(
    permits,
    across(starts_with("data_"), parse_permit_date),
    across(starts_with(c("n_", "area_", "unid_", "coord_")), as.numeric),
    n_unidades_por_bloco = n_unidades / n_blocos,
    n_pavimentos = n_blocos * n_pavimentos_por_bloco,
    # Zero areas and counts are unreported values.
    across(starts_with(c("area_", "n_")), \(x) if_else(x == 0, NA, x)),
    ano = year(data_aprovacao),
    mes = factor(meses[month(data_aprovacao)], levels = meses)
  )
  return(permits)
}

# Classify permits ----

classify_permits <- function(permits) {
  permits <- mutate(
    permits,
    ind_loteamento = str_detect(
      descricao,
      paste0(
        "DESMEMBRAMENTO|LOTEAMENTO|DESDOBRO|REMEMBRAMENTO|TERMO DE VERIF|",
        "Desmembramento|Loteamento"
      )
    ),
    ind_aprovacao = str_detect(descricao, "APROVACAO|Aprovação"),
    ind_execucao = str_detect(descricao, "EXECUCAO|Execução"),
    ind_conclusao = str_detect(descricao, "CONCLUSAO|Conclusão"),
    ind_correcao = str_detect(
      descricao,
      "APOSTILAMENTO|PROJETO MODIFICATIVO|Apostilamento|Projeto Modificativo"
    ),
    ind_edificacao_nova = str_detect(
      descricao,
      "EDIFICACAO NOVA|EDIFICACAONOVA|EDI-FICACAO NOVA|Edificação Nova"
    ),
    descricao_tipo = case_when(
      ind_loteamento ~ "PLANO INTEGRADO",
      ind_aprovacao & ind_execucao ~ "APROVACAO E EXECUCAO",
      ind_aprovacao ~ "APROVACAO",
      ind_execucao ~ "EXECUCAO",
      ind_conclusao ~ "CONCLUSAO",
      .default = "OUTRO"
    ),
    ind_r2v = str_detect(categoria_de_uso, "R2V"),
    ind_r2h = str_detect(categoria_de_uso, "R2H"),
    ind_his = str_detect(categoria_de_uso, "HIS|H.I.S"),
    ind_hmp = str_detect(categoria_de_uso, "HMP|H.M.P"),
    ind_ezeis = str_detect(categoria_de_uso, "EZEIS"),
    # Permits from Aprova Digital may list several SQLs; keep the first.
    sql_incra_11 = case_when(
      nchar(sql_incra) == 0 ~ NA_character_,
      nchar(sql_incra) > 11 ~ substr(sql_incra, 1, 11),
      .default = sql_incra
    ),
    ind_sql_incra_null = is.na(sql_incra_11),
    ind_incra = !ind_sql_incra_null
  )
  return(permits)
}

# Harmonize addresses ----

extract_logradouro <- function(endereco, endereco_unico) {
  logradouro <- case_when(
    str_detect(endereco, "[0-9]\\'") ~ endereco_unico,
    str_detect(endereco, ",") ~ str_split_i(endereco, ",", 1),
    .default = str_split_i(endereco, "(?<=[a-zA-Z])\\s*(?=[0-9])", 1)
  )
  return(logradouro)
}

extract_numero <- function(endereco, endereco_unico) {
  # Highway kilometres, e.g. "ROD ANHANGUERA KM 32,5".
  numero_km <- str_extract(endereco, "KM.*") |>
    str_replace_all(",", ".") |>
    str_extract(numero_pattern)
  # Numbers after an apostrophe, e.g. "R X '123".
  numero_apostrofo <- str_extract(endereco_unico, "\\'.*") |>
    str_extract(numero_pattern)
  # Numbers between commas, e.g. "R X, 1.584, ...".
  numero_virgulas <- str_extract(endereco, ",\\s*\\d{1,5}(\\.\\d{3})?") |>
    str_remove_all("[,.]") |>
    str_extract("\\d+")
  numero_padrao <- str_extract(endereco, numero_pattern)

  numero <- case_when(
    str_detect(endereco, "\\sKM\\s") ~ as.character(as.numeric(numero_km)),
    str_detect(endereco, "[0-9]\\'") ~
      as.character(as.numeric(numero_apostrofo)),
    str_detect(endereco, ",\\s*\\d{1,5}(\\.\\d{3})?,") ~ numero_virgulas,
    .default = as.character(as.numeric(numero_padrao))
  )
  numero <- na_if(numero, "99999")
  return(numero)
}

harmonize_addresses <- function(permits) {
  addresses <- mutate(
    permits,
    endereco_raw = endereco,
    # Permits with several SQLs list several addresses; keep the first.
    endereco_unico = if_else(
      nchar(sql_incra) > 11,
      sub(";.*", "", endereco),
      endereco
    ),
    endereco_sem_sn = str_remove_all(endereco_unico, ",SN|S/N"),
    logradouro = extract_logradouro(endereco_sem_sn, endereco_unico),
    numero = extract_numero(endereco_sem_sn, endereco_unico),
    endereco_padrao = case_when(
      str_detect(endereco_sem_sn, "\\sKM\\s") ~
        paste0(logradouro, " KM ", numero),
      str_detect(endereco_sem_sn, "[0-9]\\'") ~ endereco_sem_sn,
      !is.na(numero) & !str_starts(numero, "-") ~
        paste0(logradouro, " ", numero),
      .default = logradouro
    ),
    endereco_padrao = str_replace_all(endereco_padrao, ",", " "),
    endereco_chave = str_remove(endereco_padrao, tipos_logradouro)
  )

  # The same street may appear with and without its type ("R", "AV"). Within
  # a subprefeitura, every spelling takes the most recent permit's address.
  addresses <- arrange(addresses, desc(data_aprovacao))
  addresses <- mutate(
    addresses,
    endereco = first(endereco_padrao),
    .by = c(endereco_chave, subprefeitura)
  )
  addresses <- arrange(addresses, id)
  return(addresses)
}
