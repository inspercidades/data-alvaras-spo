# Clean permits ----

import::from(
  dplyr,
  across,
  arrange,
  case_when,
  count,
  desc,
  distinct,
  everything,
  filter,
  first,
  group_by,
  if_else,
  left_join,
  mutate,
  na_if,
  rename,
  row_number,
  select,
  starts_with,
  ungroup
)
import::from(lubridate, NA_Date_, as_date, month, year, ymd)
import::from(purrr, map_chr, reduce)
import::from(
  stringr,
  str_detect,
  str_extract,
  str_remove,
  str_remove_all,
  str_replace_all,
  str_split,
  str_squish,
  str_to_upper,
  str_trim
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

clean_alvaras <- function(alvaras_raw) {
  # Standardize source columns ----

  # Cria colunas ano e mes
  alvaras_raw <- alvaras_raw |>
    mutate(
      ano = ano_emissao,
      mes = ymd(paste(year(data_emissao), month(data_emissao), 1, sep = "-"))
    )

  # Renomeia colunas
  alvaras_raw <- alvaras_raw |>
    rename(
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
      data_aprovacao = data_emissao,
      ano_aprovacao = ano_emissao
    )

  # Cria coluna de unidades por bloco e de número total de pavimentos
  alvaras_raw <- alvaras_raw |>
    mutate(
      n_unidades_por_bloco = n_unidades / n_blocos,
      n_pavimentos = n_blocos * n_pavimentos_por_bloco
    )

  #
  alvaras_trimed <- alvaras_raw |>
    mutate(across(everything(), ~ str_squish(str_trim(.x)))) |>
    filter(!is.na(alvara)) |>
    mutate(id = row_number()) |>
    select(id, everything())

  # Chave relacional excluindo variáveis alvo do tratamento
  alvaras_key <- alvaras_trimed |>
    select(
      -c(
        # datas
        "data_aprovacao",
        "data_autuacao",
        # numericas
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
        "coord_x",
        "coord_y",
        # categoricas
        "ano",
        "mes",
        "descricao",
        "categoria_de_uso",
        "sql_incra",
        "endereco"
      )
    )

  # 3. Ajusta datas ---------------------------------------------------------

  # Cria tabela de atributos atualizados com chave relacional
  atts_datas <- alvaras_trimed |>
    select(id, starts_with("data_")) |>
    mutate(across(
      starts_with("data_"),
      ~ case_when(
        nchar(.x) == 0 ~ NA_Date_, # Se não há caracteres, atribui NA
        nchar(.x) == 10 ~ as_date(str_replace_all(.x, "/", "-")), # Se tem 10 caracteres, converte para data
        TRUE ~ NA_Date_ # Qualquer outro caso, atribui NA
      )
    ))

  # 4. Ajusta variáveis numéricas -------------------------------------------

  # Cria tabela de atributos atualizados com chave relacional e ajustes
  atts_numericos <- alvaras_trimed |>
    select(
      id,
      starts_with("n_"),
      starts_with("area_"),
      starts_with("unid_"),
      starts_with("coord_")
    ) |>
    mutate(across(everything(), as.numeric)) |>
    # Ajusta subnotificação de NA's
    mutate(across(starts_with(c("area_", "n_")), ~ ifelse(.x == 0, NA, .x)))

  #
  # 5. Ajusta variáveis categóricas ----------------------------------------------

  # A - Mês
  categorical_source <- alvaras_trimed |>
    select(-starts_with("data_"))
  categorical_source <- left_join(categorical_source, atts_datas, by = "id")

  att_mes <- categorical_source |>
    mutate(
      mes_raw = mes,
      mes = factor(meses[month(data_aprovacao)], levels = meses)
    ) |>
    select(id, mes_raw, mes)
  # B - Ano
  att_ano <- categorical_source |>
    mutate(ano_raw = ano, ano = year(data_aprovacao)) |>
    select(id, ano_raw, ano)

  # C - Descrição
  # Definição de alvará relevante
  att_descricao <- alvaras_trimed |>
    mutate(
      id,
      descricao,
      ind_loteamento = if_else(
        str_detect(
          descricao,
          "DESMEMBRAMENTO|LOTEAMENTO|DESDOBRO|REMEMBRAMENTO|TERMO DE VERIF|Desmembramento|Loteamento"
        ),
        TRUE,
        FALSE
      ),
      ind_aprovacao = if_else(
        str_detect(descricao, "APROVACAO|Aprovação"),
        TRUE,
        FALSE
      ),
      ind_execucao = if_else(
        str_detect(descricao, "EXECUCAO|Execução"),
        TRUE,
        FALSE
      ),
      ind_conclusao = if_else(
        str_detect(descricao, "CONCLUSAO|Conclusão"),
        TRUE,
        FALSE
      ),
      ind_correcao = if_else(
        str_detect(
          descricao,
          "APOSTILAMENTO|PROJETO MODIFICATIVO|Apostilamento|Projeto Modificativo"
        ),
        TRUE,
        FALSE
      ),
      ind_edificacao_nova = if_else(
        str_detect(
          descricao,
          "EDIFICACAO NOVA|EDIFICACAONOVA|EDI-FICACAO NOVA|Edificação Nova"
        ),
        TRUE,
        FALSE
      ),
      descricao_tipo = case_when(
        ind_loteamento == TRUE ~ "PLANO INTEGRADO",
        ind_aprovacao == TRUE & ind_execucao == TRUE ~ "APROVACAO E EXECUCAO",
        ind_aprovacao == TRUE ~ "APROVACAO",
        ind_execucao == TRUE ~ "EXECUCAO",
        ind_conclusao == TRUE ~ "CONCLUSAO",
        TRUE ~ "OUTRO"
      )
      # Alvarás relevantes para M&A de licenciamentos imobiliários residenciais:
      # Aprovação, Execução ou Aprovação e Execução de Edificação Nova
      # Loteamento e Conclusão (quando lote está atribuido à edificaçao nova)
      # Dados Bloco-Pavimentos-Unidades: Aprovação, Aprovaçao e Execução
      # Casos excepcionais podem ter dados em Execução
    ) |>
    select(id, descricao, starts_with("ind_"), descricao_tipo)

  # F - Categoria de uso
  att_categoria_de_uso <- alvaras_trimed |>
    mutate(
      ind_r2v = if_else(str_detect(categoria_de_uso, "R2V"), TRUE, FALSE),
      ind_r2h = if_else(str_detect(categoria_de_uso, "R2H"), TRUE, FALSE),
      ind_his = if_else(str_detect(categoria_de_uso, "HIS|H.I.S"), TRUE, FALSE),
      ind_hmp = if_else(str_detect(categoria_de_uso, "HMP|H.M.P"), TRUE, FALSE),
      ind_ezeis = if_else(str_detect(categoria_de_uso, "EZEIS"), TRUE, FALSE)
    ) |>
    select(id, categoria_de_uso, starts_with("ind_"))

  #
  att_sql_incra <- alvaras_trimed |>
    mutate(
      id,
      sql_incra, #nchar_sql_incra = nchar(sql_incra),
      sql_incra_11 = case_when(
        nchar(sql_incra) == 0 ~ NA_character_,
        nchar(sql_incra) > 11 ~ substr(sql_incra, 1, 11),
        TRUE ~ sql_incra
      ),
      ind_sql_incra_null = if_else(is.na(sql_incra_11), TRUE, FALSE),
      ind_incra = if_else(ind_sql_incra_null == FALSE, TRUE, FALSE),
    ) |>
    select(id, sql_incra, sql_incra_11, ind_sql_incra_null, ind_incra)

  # G - Endereço

  #
  att_endereco <- alvaras_trimed |>
    mutate(
      id,
      data_aprovacao = case_when(
        nchar(data_aprovacao) == 0 ~ NA_Date_, # Se não há caracteres, atribui NA
        nchar(data_aprovacao) == 10 ~ as_date(str_replace_all(
          data_aprovacao,
          "/",
          "-"
        )), # Se tem 10 caracteres, converte para data
        TRUE ~ NA_Date_
      ),
      endereco_raw = endereco,
      endereco_unico = case_when(
        nchar(sql_incra) > 11 ~ sub(";.*", "", endereco),
        TRUE ~ endereco
      ),
      endereco_ajust_1 = str_remove_all(endereco_unico, ",SN|S/N"),
      logradouro = case_when(
        str_detect(endereco_ajust_1, "[0-9]\\'") ~ endereco_unico,
        str_detect(endereco_ajust_1, ",") ~ str_split(endereco_ajust_1, ",") |>
          map_chr(~ .x[1]),
        TRUE ~ str_split(endereco_ajust_1, "(?<=[a-zA-Z])\\s*(?=[0-9])") |>
          map_chr(~ .x[1])
      ),
      numero = case_when(
        # Casos onde "KM" está presente (ex., "Rodovia Anhanguera KM 32,5")
        str_detect(endereco_ajust_1, "\\sKM\\s") ~ str_extract(
          endereco_ajust_1,
          "KM.*"
        ) |>
          str_replace_all(",", ".") |>
          str_extract(pattern = "[-+]?[0-9]*\\.?[0-9]+") |>
          str_replace_all("\\s", "") |>
          as.numeric() |>
          as.character(),

        # Casos onde um número está entre apóstofres (ex., "Rua X, '123'")
        str_detect(endereco_ajust_1, "[0-9]\\'") ~ str_extract(
          endereco_unico,
          "\\'.*"
        ) |>
          str_extract(pattern = "[-+]?[0-9]*\\.?[0-9]+") |>
          as.numeric() |>
          as.character(),

        # Casos onde o número está entre duas vírgulas em um intervalo
        str_detect(
          endereco_ajust_1,
          ",\\s*\\d{1,5}(\\.\\d{3})?,"
        ) ~ str_extract(endereco_ajust_1, ",\\s*\\d{1,5}(\\.\\d{3})?") |>
          str_replace_all(",", "") |> # Remove leading comma
          str_replace_all("\\.", "") |> # Remove dot inside numbers (e.g., "1.584" → "1584")
          str_extract("\\d+") |> # Extract only the number
          as.character(),

        # Caso base: extrai a primeira ocorrência
        TRUE ~ str_extract(
          endereco_ajust_1,
          pattern = "[-+]?[0-9]*\\.?[0-9]+"
        ) |>
          as.numeric() |>
          as.character()
      ) |>
        # Convert "99999" to NA
        na_if("99999"),
      endereco_ajust_2 = case_when(
        str_detect(endereco_ajust_1, "\\sKM\\s") ~ paste0(
          logradouro,
          " KM ",
          numero
        ),
        str_detect(endereco_ajust_1, "[0-9]\\'") ~ endereco_ajust_1,
        numero >= 0 & !is.na(numero) ~ paste0(logradouro, " ", numero),
        TRUE ~ logradouro
      ) |>
        str_replace_all(",", " "),
      subprefeitura,
      endereco_clean = str_remove(
        endereco_ajust_2,
        "R |AV |ES |EST |VIA |AL |PC |PG |LV |TV |PV |LG |VD |RV |PQ |VP |RUA |Av. |Rua |PRAÇA |rua |AVENIDA |Avenida |Alameda |av |R. |AV. |Av "
      )
    ) |>
    # Para garantir a não-duplicação de endereços por falta de tipo de logradouro
    # Importante também chave secundária de subprefeitura para evitar unificação equivocada
    # Ex: Rua/Avenida/Alameda Santo Amaro
    group_by(endereco_clean, subprefeitura) |>
    # Toma informação mais recente como referência
    arrange(desc(data_aprovacao)) |>
    mutate(
      endereco = first(endereco_ajust_2) #,
      #ind_diff_endereco = if_else(endereco_ajust_2 != endereco, TRUE, FALSE)
    ) |>
    ungroup() |>
    select(
      id,
      data_aprovacao,
      endereco_raw,
      endereco_unico,
      endereco_ajust_1,
      logradouro,
      numero,
      endereco_ajust_2,
      subprefeitura,
      endereco_clean,
      endereco
    )

  # Cria tabela de atributos atualizados com chave relacional
  att_categoricos <- list(
    att_ano,
    att_mes,
    att_descricao,
    att_categoria_de_uso,
    att_sql_incra,
    att_endereco |>
      select(-c(data_aprovacao, subprefeitura))
  ) |>
    reduce(left_join, by = "id")

  # 6. Cria conjunto de dados ----------------------------------------------------

  #
  alvaras_trusted_not_unique <- list(
    alvaras_key,
    atts_datas,
    atts_numericos,
    att_categoricos
  ) |>
    reduce(left_join, by = "id")
  alvaras_trusted_not_unique <- alvaras_trusted_not_unique |>
    select(
      id,
      data_aprovacao,
      mes,
      ano,
      alvara,
      descricao,
      descricao_tipo,
      unidade_pmsp,
      processo,
      data_autuacao,
      categoria_de_uso,
      area_do_terreno,
      area_da_construcao,
      n_blocos,
      n_pavimentos_por_bloco,
      n_unidades_por_bloco,
      n_pavimentos,
      n_unidades,
      n_unidades,
      unid_his,
      unid_hmp,
      unid_r2h_r2v,
      sql_incra_11,
      sql_incra,
      endereco_raw,
      endereco_unico,
      endereco,
      distrito,
      subprefeitura,
      zona_de_uso_registro,
      coord_x,
      coord_y,
      georref_nivel,
      georref_modo,
      starts_with("ind_")
    )

  # Número duplicado, entretanto, não indica necessariamente mesmo alvará!
  # Para correção, mantém-se tratamento dado no script Java
  # Considerada duplicata Somente se idênticos: {unidade_pmsp, subprefeitura, alvara,
  # data_aprovacao, data_autuacao, descricao, endereco, sql_incra}
  #
  alvaras_trusted <- alvaras_trusted_not_unique |>
    distinct(
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

  return(alvaras_trusted)
}
