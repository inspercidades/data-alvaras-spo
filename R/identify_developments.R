# Identify developments ----

import::from(
  dplyr,
  across,
  arrange,
  bind_cols,
  bind_rows,
  case_when,
  count,
  dense_rank,
  desc,
  filter,
  first,
  group_by,
  if_else,
  last,
  left_join,
  mutate,
  n,
  n_distinct,
  rename,
  row_number,
  select,
  slice_max,
  starts_with,
  summarise,
  summarize,
  ungroup,
  where
)
import::from(lubridate, days, interval, year)
import::from(purrr, map_chr)
import::from(sf, st_drop_geometry, st_sf)
import::from(stringr, str_detect, str_extract, str_split, str_trim)
import::from(tidyr, pivot_longer)

pair_combinations <- function(indices) {
  indices <- sort(unique(indices))
  if (length(indices) < 2) {
    return(data.frame(left = integer(), right = integer()))
  }
  pairs <- utils::combn(indices, 2)
  return(data.frame(left = pairs[1, ], right = pairs[2, ]))
}

shared_sql_pairs <- function(sql_values) {
  sql_tokens <- stringr::str_split(sql_values, ",")
  sql_tokens <- lapply(sql_tokens, stringr::str_trim)
  sql_tokens <- lapply(sql_tokens, function(tokens) {
    tokens[is.na(tokens) | tokens == ""] <- "__MISSING_SQL__"
    return(unique(tokens))
  })

  permit_index <- rep(seq_along(sql_tokens), lengths(sql_tokens))
  token_index <- split(permit_index, unlist(sql_tokens, use.names = FALSE))
  pairs <- lapply(token_index, pair_combinations)
  pairs <- dplyr::bind_rows(pairs)
  pairs <- dplyr::distinct(pairs, left, right)
  return(pairs)
}

nearby_pairs <- function(geo_permits, distance_threshold) {
  neighbors <- sf::st_is_within_distance(
    geo_permits,
    geo_permits,
    dist = distance_threshold
  )
  left <- rep(seq_along(neighbors), lengths(neighbors))
  right <- unlist(neighbors, use.names = FALSE)
  pairs <- data.frame(left = left, right = right)
  pairs <- dplyr::filter(pairs, left < right)
  pairs <- dplyr::distinct(pairs, left, right)
  return(pairs)
}

classify_match_pairs <- function(geo_permits, distance_threshold) {
  sql_pairs <- shared_sql_pairs(geo_permits$sql_incra_composto)
  sql_pairs$shared_sql <- rep(TRUE, nrow(sql_pairs))
  distance_pairs <- nearby_pairs(geo_permits, distance_threshold)
  distance_pairs$nearby <- rep(TRUE, nrow(distance_pairs))

  candidates <- dplyr::full_join(
    sql_pairs,
    distance_pairs,
    by = c("left", "right")
  )
  candidates$shared_sql[is.na(candidates$shared_sql)] <- FALSE
  candidates$nearby[is.na(candidates$nearby)] <- FALSE

  left <- candidates$left
  right <- candidates$right
  same_land_area <- !is.na(geo_permits$area_do_terreno[left]) &
    !is.na(geo_permits$area_do_terreno[right]) &
    geo_permits$area_do_terreno[left] == geo_permits$area_do_terreno[right]

  candidates$match_type <- dplyr::case_when(
    candidates$shared_sql & candidates$nearby & same_land_area ~ 1L,
    candidates$shared_sql & !candidates$nearby & same_land_area ~ 2L,
    candidates$shared_sql & candidates$nearby & !same_land_area ~ 3L,
    !candidates$shared_sql & candidates$nearby & same_land_area ~ 4L,
    candidates$shared_sql & !candidates$nearby & !same_land_area ~ 5L,
    TRUE ~ 0L
  )

  partial <- candidates$match_type %in% 2:4
  same_building_area <- !is.na(geo_permits$area_da_construcao[left]) &
    !is.na(geo_permits$area_da_construcao[right]) &
    geo_permits$area_da_construcao[left] ==
      geo_permits$area_da_construcao[right]
  same_units <- !is.na(geo_permits$n_unidades[left]) &
    !is.na(geo_permits$n_unidades[right]) &
    geo_permits$n_unidades[left] == geo_permits$n_unidades[right]
  candidates$match_type[partial & (same_building_area | same_units)] <- 1L
  candidates <- dplyr::filter(candidates, match_type > 0)
  return(candidates)
}

component_ids <- function(match_pairs, number_of_permits) {
  vertices <- data.frame(name = as.character(seq_len(number_of_permits)))
  ids <- vector("list", 5)

  for (match_type in seq_len(5)) {
    edges <- dplyr::filter(match_pairs, .data$match_type == match_type)
    edges <- dplyr::select(edges, from = left, to = right)
    edges$from <- as.character(edges$from)
    edges$to <- as.character(edges$to)
    graph <- igraph::graph_from_data_frame(
      edges,
      directed = FALSE,
      vertices = vertices
    )
    membership <- igraph::components(graph)$membership
    membership <- membership[as.character(seq_len(number_of_permits))]
    component_minimum <- tapply(seq_along(membership), membership, min)
    component_order <- order(component_minimum)
    canonical_id <- integer(length(component_order))
    canonical_id[component_order] <- seq_along(component_order)
    ids[[match_type]] <- unname(canonical_id[membership])
  }

  names(ids) <- paste0("empreendimento_id_match_", seq_len(5))
  ids <- as.data.frame(ids)
  return(ids)
}

identify_developments <- function(geo_alvaras, dist_max = 100) {
  geo_alvaras <- sf::st_sf(geo_alvaras, crs = 31983)

  # 2. Observações com mais de um SQL --------------------------------------------------------------

  # Precisamos identificar as observações com mais de um SQL que possuem os mesmos SQLs, mas que podem não estar na mesma ordem
  geo_alvaras <- geo_alvaras |>
    mutate(
      sql_incra_composto = sql_incra |>
        str_split(",") |>
        lapply(function(x) sort(str_trim(x))) |>
        sapply(paste, collapse = ",")
    )

  # 3. Identifica empreendimentos --------------------------------------------------------------

  # Precisamos criar um indicador de empreendimento. Para isso, utilizaremos 3 variáveis: SQL,
  # área do terreno e distância entre os pontos. Definimos um MATCH PERFEITO entre duas observações
  # se houver igualdade (ou inclusão) entre os SQLs, igualdade entre as áreas e com a distância entre
  # os dois pontos sendo menor ou igual a um valor pré-definido. Para os MATCHES PARCIAIS, podemos
  # ter casos distintos. São eles:
  # 1) SQLs e área do terreno: depende da distância limite, mas deve se tratar de um empreendimento com muitos lotes
  # 2) SQLs e distância: necessário comparar magnitude da área e, talvez, outras variáveis
  # 3) Área do terreno e distância: muito provavelmente é o mesmo empreendimento, com mais de um lote
  # Gostaríamos de transformar matches parciais em matches perfeitos. Para isso, criamos a seguinte
  # árvore de decisão:
  # a. Caso haja um match parcial entre duas observações, comparamos a área da construção. Se for igual,
  # definimos como match perfeito;
  # b. Se a área da construção não for igual, comparamos o número de unidades; se for igual, definimos
  # como match perfeito. Se não, continua como match parcial.
  # Além dos matches perfeitos e parciais, vamos definir os MATCHES ÚNICOS, baseados apenas no SQL.
  # Por fim, as observações SEM MATCH serão definidas como as em que não houve nenhum dos três tipos.

  amostra <- geo_alvaras
  match_pairs <- classify_match_pairs(amostra, dist_max)
  empreendimento_ids <- component_ids(match_pairs, nrow(amostra))
  amostra <- bind_cols(amostra, empreendimento_ids)

  # ---------------------------------------------
  # Gera um único ID de empreendimento

  # 1. Conta o tamanho de cada componente para cada tipo de match
  tamanhos <- bind_rows(
    amostra |>
      st_drop_geometry() |>
      mutate(tipo = 1, empreendimento_id = empreendimento_id_match_1) |>
      count(empreendimento_id, tipo, name = "tam"),
    amostra |>
      st_drop_geometry() |>
      mutate(tipo = 2, empreendimento_id = empreendimento_id_match_2) |>
      count(empreendimento_id, tipo, name = "tam"),
    amostra |>
      st_drop_geometry() |>
      mutate(tipo = 3, empreendimento_id = empreendimento_id_match_3) |>
      count(empreendimento_id, tipo, name = "tam"),
    amostra |>
      st_drop_geometry() |>
      mutate(tipo = 4, empreendimento_id = empreendimento_id_match_4) |>
      count(empreendimento_id, tipo, name = "tam"),
    amostra |>
      st_drop_geometry() |>
      mutate(tipo = 5, empreendimento_id = empreendimento_id_match_5) |>
      count(empreendimento_id, tipo, name = "tam")
  )

  # 2. Prepara os dados da amostra com IDs e tamanhos
  # Transforma em dataframe normal
  amostra_df <- amostra |> st_drop_geometry()

  # Agora faz as manipulações
  amostra_ext <- amostra_df |>
    mutate(row_id = row_number(), n_unidades = n_unidades) |>
    pivot_longer(
      cols = starts_with("empreendimento_id_match_"),
      names_to = "tipo",
      values_to = "empreendimento_id"
    ) |>
    mutate(tipo = as.numeric(str_extract(tipo, "\\d+")))
  amostra_ext <- left_join(
    amostra_ext,
    tamanhos,
    by = c("empreendimento_id", "tipo")
  )

  # 3. Para matches parciais, escolhe o empreendimento com mais unidades
  match_parcial <- amostra_ext |>
    filter(tipo %in% 2:4, tam > 1) |>
    group_by(row_id) |>
    slice_max(order_by = n_unidades, with_ties = FALSE) |>
    ungroup()

  # 4. Define o ID final e o tipo de match com a prioridade
  id_final <- amostra_ext |>
    filter(
      (tipo == 1 & tam > 1) |
        (tipo %in% 2:4 & tam > 1) |
        (tipo == 5 & tam > 1)
    ) |>
    group_by(row_id) |>
    summarise(
      tipo_final = case_when(
        any(tipo == 1) ~ 1,
        any(tipo %in% 2:4) ~ NA_real_, # Preenche depois com match_parcial
        any(tipo == 5) ~ 5,
        TRUE ~ NA_real_
      ),
      empreendimento_id_final = case_when(
        any(tipo == 1) ~ empreendimento_id[tipo == 1][1], # Pega o primeiro do match perfeito
        any(tipo %in% 2:4) ~ NA_real_, # Preenche depois com match_parcial
        any(tipo == 5) ~ empreendimento_id[tipo == 5][1], # Pega o primeiro do match único
        TRUE ~ NA_real_
      ),
      .groups = "drop"
    )

  partial_ids <- match_parcial |>
    select(
      row_id,
      empreendimento_id_parcial = empreendimento_id,
      tipo_parcial = tipo
    )
  id_final <- left_join(id_final, partial_ids, by = "row_id")
  id_final <- id_final |>
    mutate(
      tipo_final = if_else(is.na(tipo_final), tipo_parcial, tipo_final),
      empreendimento_id_final = if_else(
        is.na(empreendimento_id_final),
        empreendimento_id_parcial,
        empreendimento_id_final
      )
    ) |>
    select(row_id, tipo_final, empreendimento_id_final)

  # 5. Junta ao dataframe principal e define "sem match"
  amostra <- amostra |>
    mutate(row_id = row_number())
  amostra <- left_join(amostra, id_final, by = "row_id")
  amostra <- amostra |>
    mutate(
      id_empreendimento = case_when(
        !is.na(empreendimento_id_final) & tipo_final == 1 ~ paste0(
          "P_",
          empreendimento_id_final
        ),
        !is.na(empreendimento_id_final) & tipo_final %in% 2:4 ~ paste0(
          "M",
          tipo_final,
          "_",
          empreendimento_id_final
        ),
        !is.na(empreendimento_id_final) & tipo_final == 5 ~ paste0(
          "U_",
          empreendimento_id_final
        ),
        TRUE ~ paste0("S_", row_id) # Sem match
      ),
      tipo_match = case_when(
        tipo_final == 1 ~ "perfeito",
        tipo_final %in% 2:4 ~ paste0("parcial_", tipo_final),
        tipo_final == 5 ~ "único",
        TRUE ~ "sem match"
      )
    ) |>
    select(-row_id, -empreendimento_id_final, -tipo_final)

  amostra <- amostra |>
    mutate(
      id_empreendimento_num = dense_rank(as.integer(as.factor(
        id_empreendimento
      ))) # Cria o ID sequencial
    )

  amostra <- amostra |>
    subset(
      select = c(
        -empreendimento_id_match_1,
        -empreendimento_id_match_2,
        -empreendimento_id_match_3,
        -empreendimento_id_match_4,
        -empreendimento_id_match_5,
        -id_empreendimento
      )
    )

  # 4. Cria conjunto de dados por empreendimento ---------------------

  # Identifica e trata casos de parcelamento
  # Validado em uma série de reuniões Insper/Abrainc entre nov-dez de 22
  amostra_sem_geometria <- st_drop_geometry(amostra)

  # A - Tipifica parcelamento
  parcelamento_attributes <- amostra |>
    # Somente sql's válidos
    filter(ind_sql_incra_null == FALSE) |>
    # Contabiliza aprovações e execuções em alvará relevante para o sql/lote
    group_by(sql_incra_composto) |>
    mutate(
      n_aprovacao = length(which(
        ind_aprovacao == TRUE &
          ind_correcao == FALSE &
          ind_edificacao_nova == TRUE
      )),
      n_execucao = length(which(
        ind_execucao == TRUE &
          ind_correcao == FALSE &
          ind_edificacao_nova == TRUE
      ))
    ) |>
    ungroup() |>
    # Tipifica sql's com áreas de terreno diferentes em alvarás relevantes de tipo igual
    group_by(sql_incra_composto, descricao_tipo) |>
    mutate(
      n_areas_terreno = length(unique(area_do_terreno[
        !is.na(area_do_terreno) &
          (ind_edificacao_nova == TRUE |
            ind_loteamento == TRUE |
            ind_conclusao == TRUE)
      ]))
    ) |>
    ungroup() |>
    # Somente sql's com algum alvará relevante
    group_by(sql_incra_composto) |>
    filter(any(ind_edificacao_nova == TRUE)) |>
    #1 Sql possui alvará do tipo loteamento
    ## Descricao := {PLANO INTEGRADO}
    group_by(sql_incra_composto) |>
    mutate(
      n_loteamento = length(
        which(descricao_tipo == "PLANO INTEGRADO") &
          ind_correcao == FALSE
      ),
      ind_loteamento = if_else(n_loteamento >= 1, TRUE, FALSE),
      ind_caso_1 = if_else(
        n_loteamento >= 1 &
          n_areas_terreno > 1 &
          n_aprovacao > 1 |
          n_loteamento >= 1 &
            n_areas_terreno > 1 &
            n_execucao > 1,
        TRUE,
        FALSE
      )
    ) |>
    #2 Sql possui mais de um alvará de tipo conclusão
    group_by(sql_incra_composto) |>
    mutate(
      n_caso_2 = length(which(
        ind_conclusao == TRUE &
          ind_correcao == FALSE
      )),
      ind_caso_2 = if_else(n_caso_2 > 1 & n_areas_terreno > 1, TRUE, FALSE)
    ) |>
    #3 Sql com diferentes endereços em alvarás de tipo igual
    group_by(sql_incra_composto, descricao_tipo) |>
    mutate(
      n_caso_3 = length(unique(endereco[
        (ind_edificacao_nova == TRUE |
          ind_loteamento == TRUE |
          ind_conclusao == TRUE) &
          ind_correcao == FALSE
      ])),
      ind_caso_3 = if_else(n_caso_3 > 1 & n_areas_terreno > 1, TRUE, FALSE)
    ) |>
    ungroup() |>
    # Cria Indicador de parcelamento
    group_by(sql_incra_composto) |>
    mutate(
      ind_parcelamento = if_else(
        any(ind_edificacao_nova == TRUE) &
          (any(ind_caso_1 == TRUE) |
            any(ind_caso_2 == TRUE) |
            any(ind_caso_3 == TRUE)),
        TRUE,
        FALSE
      )
    ) |>
    ungroup()
  parcelamento_join_cols <- intersect(
    names(amostra_sem_geometria),
    names(parcelamento_attributes)
  )
  amostra_parcelamento <- left_join(
    amostra_sem_geometria,
    parcelamento_attributes,
    by = parcelamento_join_cols
  )

  # Agrupa por empreendimento
  amostra_emp <- amostra_parcelamento |>
    filter(ind_parcelamento == FALSE) |>
    group_by(id_empreendimento_num) |>
    arrange(desc(data_aprovacao)) |> # importante para inferir que posição 1 é sempre data mais atual
    summarize(
      tipo_match = first(tipo_match),
      ano_aprovacao = year(first(data_aprovacao[which(
        ind_aprovacao == TRUE & ind_edificacao_nova == TRUE
      )])),
      ano_execucao = year(first(data_aprovacao[which(
        ind_execucao == TRUE & ind_edificacao_nova == TRUE
      )])),
      n_alvaras = n(),
      n_alvaras_aprovacao = length(which(
        ind_aprovacao == TRUE &
          ind_edificacao_nova == TRUE &
          ind_correcao == FALSE
      )),
      n_alvaras_execucao = length(which(
        ind_execucao == TRUE &
          ind_edificacao_nova == TRUE &
          ind_correcao == FALSE
      )),
      data_autuacao_projeto = last(data_autuacao[which(
        ind_aprovacao == TRUE &
          ind_edificacao_nova == TRUE &
          ind_correcao == FALSE
      )]),
      data_validacao_projeto = first(data_aprovacao[which(
        ind_aprovacao == TRUE &
          ind_edificacao_nova == TRUE &
          ind_correcao == FALSE
      )]),
      data_autuacao_execucao = last(data_autuacao[which(
        ind_execucao == TRUE &
          ind_edificacao_nova == TRUE &
          ind_correcao == FALSE
      )]),
      data_validacao_execucao = first(data_aprovacao[which(
        ind_execucao == TRUE &
          ind_edificacao_nova == TRUE &
          ind_correcao == FALSE
      )]),
      diff_dias_projeto = interval(
        data_autuacao_projeto,
        data_validacao_projeto
      ) /
        days(1),
      diff_dias_execucao = interval(
        data_validacao_projeto,
        data_validacao_execucao
      ) /
        days(1),
      unidade_pmsp = map_chr(
        list(unique(unidade_pmsp)),
        ~ paste0(.x, collapse = "; ")
      ),
      categoria_de_uso_grupo = as.factor(case_when(
        any(ind_his == TRUE) |
          any(ind_hmp == TRUE) |
          any(ind_ezeis == TRUE) ~ "ERP",
        any(str_detect(
          categoria_de_uso,
          "R2V|R202|R302|R2H|R301|R302|R303|(?<!N)R1|(?<!N)R2"
        )) ~ "ERM",
        TRUE ~ "Outra"
      )),
      categoria_de_uso_lista = map_chr(
        list(unique(na.omit(categoria_de_uso))),
        ~ paste0(.x, collapse = "; ")
      ),
      area_do_terreno = first(na.omit(area_do_terreno[which(
        ind_edificacao_nova == TRUE &
          (ind_aprovacao == TRUE |
            ind_execucao == TRUE |
            ind_conclusao == TRUE)
      )])),
      area_da_construcao = first(na.omit(area_da_construcao[which(
        ind_edificacao_nova == TRUE &
          (ind_aprovacao == TRUE |
            ind_execucao == TRUE |
            ind_conclusao == TRUE)
      )])),
      n_blocos = first(na.omit(n_blocos[which(
        ind_edificacao_nova == TRUE &
          (ind_aprovacao == TRUE |
            ind_execucao == TRUE)
      )])),
      n_pavimentos = first(na.omit(n_pavimentos[which(
        ind_edificacao_nova == TRUE &
          (ind_aprovacao == TRUE |
            ind_execucao == TRUE)
      )])),
      n_unidades = first(na.omit(n_unidades[which(
        ind_edificacao_nova == TRUE &
          (ind_aprovacao == TRUE |
            ind_execucao == TRUE)
      )])),
      n_pavimentos_por_bloco = n_pavimentos / n_blocos,
      n_unidades_por_bloco = n_unidades / n_blocos,
      n_unidades_his = first(na.omit(unid_his[which(
        ind_edificacao_nova == TRUE &
          (ind_aprovacao == TRUE |
            ind_execucao == TRUE)
      )])),
      n_unidades_his_por_bloco = n_unidades_his / n_blocos,
      n_unidades_hmp = first(na.omit(unid_hmp[which(
        ind_edificacao_nova == TRUE &
          (ind_aprovacao == TRUE |
            ind_execucao == TRUE)
      )])),
      n_unidades_hmp_por_bloco = n_unidades_hmp / n_blocos,
      n_unidades_r2h_r2v = first(na.omit(unid_r2h_r2v[which(
        ind_edificacao_nova == TRUE &
          (ind_aprovacao == TRUE |
            ind_execucao == TRUE)
      )])),
      n_unidades_r2h_r2v_por_bloco = n_unidades_r2h_r2v / n_blocos,
      sql_incra = first(sql_incra),
      sql_incra_composto = first(sql_incra_composto),
      sql_incra_lista = map_chr(
        list(unique(na.omit(sql_incra))),
        ~ paste0(.x, collapse = "; ")
      ),
      n_enderecos = n_distinct(endereco),
      endereco = first(na.omit(endereco)),
      endereco_lista = map_chr(
        list(unique(na.omit(endereco_raw))),
        ~ paste0(.x, collapse = "; ")
      ),
      distrito = first(na.omit(distrito)),
      subprefeitura = first(na.omit(subprefeitura)),
      zona_de_uso_registro = map_chr(
        list(unique(na.omit(zona_de_uso_registro))),
        ~ paste0(.x, collapse = "; ")
      ),
      ind_edificacao_nova = if_else(
        any(ind_edificacao_nova == TRUE),
        TRUE,
        FALSE
      ),
      ind_aprovacao = if_else(any(ind_aprovacao == TRUE), TRUE, FALSE),
      ind_execucao = if_else(any(ind_execucao == TRUE), TRUE, FALSE),
      ind_r2v = if_else(any(ind_r2v == TRUE), TRUE, FALSE),
      ind_r2h = if_else(any(ind_r2h == TRUE), TRUE, FALSE),
      ind_his = if_else(any(ind_his == TRUE), TRUE, FALSE),
      ind_hmp = if_else(any(ind_hmp == TRUE), TRUE, FALSE),
      ind_ezeis = if_else(any(ind_ezeis == TRUE), TRUE, FALSE),
      ind_uso_misto = if_else(
        categoria_de_uso_grupo != "Outra" &
          ind_edificacao_nova == TRUE &
          any(str_detect(
            categoria_de_uso_lista,
            "NR|C1|C2|C3|S1|S2|S3|E1|E2|E3|E4"
          )),
        TRUE,
        FALSE
      ),
      ind_zeis = if_else(
        any(str_detect(zona_de_uso_registro, "ZEIS")),
        TRUE,
        FALSE
      ),
      ind_parcelamento = if_else(any(ind_parcelamento == TRUE), TRUE, FALSE),
      geometry = first(geometry)
    ) |>
    ungroup()

  # B - Trata em separado atributos numéricos em casos de parcelamento
  # Somente parcelamentos
  atts_ind_parcelamento <- amostra_parcelamento |>
    filter(ind_parcelamento == TRUE) |>
    # Ordena mais recente para mais antigo
    arrange(desc(data_aprovacao)) |>
    # Agrupa por Área de terreno e pega informação do mais recente por grupo
    ## Grupo = mesmo sql, mesma área de terreno
    group_by(sql_incra_composto, area_do_terreno) |>
    summarize(
      id_empreendimento_num = first(id_empreendimento_num),
      tipo_match = first(tipo_match),
      ano_aprovacao = year(first(data_aprovacao[which(
        ind_aprovacao == TRUE & ind_edificacao_nova == TRUE
      )])),
      ano_execucao = year(first(data_aprovacao[which(
        ind_execucao == TRUE & ind_edificacao_nova == TRUE
      )])),
      n_alvaras = n(),
      n_alvaras_aprovacao = length(which(
        ind_aprovacao == TRUE &
          ind_edificacao_nova == TRUE &
          ind_correcao == FALSE
      )),
      n_alvaras_execucao = length(which(
        ind_execucao == TRUE &
          ind_edificacao_nova == TRUE &
          ind_correcao == FALSE
      )),
      data_autuacao_projeto = last(data_autuacao[which(
        ind_aprovacao == TRUE &
          ind_edificacao_nova == TRUE &
          ind_correcao == FALSE
      )]),
      data_validacao_projeto = first(data_aprovacao[which(
        ind_aprovacao == TRUE &
          ind_edificacao_nova == TRUE &
          ind_correcao == FALSE
      )]),
      data_autuacao_execucao = last(data_autuacao[which(
        ind_execucao == TRUE &
          ind_edificacao_nova == TRUE &
          ind_correcao == FALSE
      )]),
      data_validacao_execucao = first(data_aprovacao[which(
        ind_execucao == TRUE &
          ind_edificacao_nova == TRUE &
          ind_correcao == FALSE
      )]),
      diff_dias_projeto = interval(
        data_autuacao_projeto,
        data_validacao_projeto
      ) /
        days(1),
      diff_dias_execucao = interval(
        data_validacao_projeto,
        data_validacao_execucao
      ) /
        days(1),
      unidade_pmsp = map_chr(
        list(unique(unidade_pmsp)),
        ~ paste0(.x, collapse = "; ")
      ),
      categoria_de_uso_grupo = as.factor(case_when(
        any(ind_his == TRUE) |
          any(ind_hmp == TRUE) |
          any(ind_ezeis == TRUE) ~ "ERP",
        any(str_detect(
          categoria_de_uso,
          "R2V|R202|R302|R2H|R301|R302|R303|(?<!N)R1|(?<!N)R2"
        )) ~ "ERM",
        TRUE ~ "Outra"
      )),
      categoria_de_uso_lista = map_chr(
        list(unique(na.omit(categoria_de_uso))),
        ~ paste0(.x, collapse = "; ")
      ),
      area_da_construcao = first(na.omit(area_da_construcao[which(
        ind_edificacao_nova == TRUE &
          (ind_aprovacao == TRUE |
            ind_execucao == TRUE |
            ind_conclusao == TRUE)
      )])),
      n_blocos = first(na.omit(n_blocos[which(
        ind_edificacao_nova == TRUE &
          (ind_aprovacao == TRUE |
            ind_execucao == TRUE)
      )])),
      n_pavimentos = first(na.omit(n_pavimentos[which(
        ind_edificacao_nova == TRUE &
          (ind_aprovacao == TRUE |
            ind_execucao == TRUE)
      )])),
      n_unidades = first(na.omit(n_unidades[which(
        ind_edificacao_nova == TRUE &
          (ind_aprovacao == TRUE |
            ind_execucao == TRUE)
      )])),
      n_unidades_his = first(na.omit(unid_his[which(
        ind_edificacao_nova == TRUE &
          (ind_aprovacao == TRUE |
            ind_execucao == TRUE)
      )])),
      n_unidades_hmp = first(na.omit(unid_hmp[which(
        ind_edificacao_nova == TRUE &
          (ind_aprovacao == TRUE |
            ind_execucao == TRUE)
      )])),
      n_unidades_r2h_r2v = first(na.omit(unid_r2h_r2v[which(
        ind_edificacao_nova == TRUE &
          (ind_aprovacao == TRUE |
            ind_execucao == TRUE)
      )])),
      sql_incra = first(sql_incra),
      sql_incra_composto = first(sql_incra_composto),
      sql_incra_lista = map_chr(
        list(unique(na.omit(sql_incra))),
        ~ paste0(.x, collapse = "; ")
      ),
      n_enderecos = n_distinct(endereco),
      endereco = first(na.omit(endereco)),
      endereco_lista = map_chr(
        list(unique(na.omit(endereco_raw))),
        ~ paste0(.x, collapse = "; ")
      ),
      distrito = first(na.omit(distrito)),
      subprefeitura = first(na.omit(subprefeitura)),
      zona_de_uso_registro = map_chr(
        list(unique(na.omit(zona_de_uso_registro))),
        ~ paste0(.x, collapse = "; ")
      ),
      ind_edificacao_nova = if_else(
        any(ind_edificacao_nova == TRUE),
        TRUE,
        FALSE
      ),
      ind_aprovacao = if_else(any(ind_aprovacao == TRUE), TRUE, FALSE),
      ind_execucao = if_else(any(ind_execucao == TRUE), TRUE, FALSE),
      ind_r2v = if_else(any(ind_r2v == TRUE), TRUE, FALSE),
      ind_r2h = if_else(any(ind_r2h == TRUE), TRUE, FALSE),
      ind_his = if_else(any(ind_his == TRUE), TRUE, FALSE),
      ind_hmp = if_else(any(ind_hmp == TRUE), TRUE, FALSE),
      ind_ezeis = if_else(any(ind_ezeis == TRUE), TRUE, FALSE),
      ind_uso_misto = if_else(
        categoria_de_uso_grupo != "Outra" &
          ind_edificacao_nova == TRUE &
          any(str_detect(
            categoria_de_uso_lista,
            "NR|C1|C2|C3|S1|S2|S3|E1|E2|E3|E4"
          )),
        TRUE,
        FALSE
      ),
      ind_zeis = if_else(
        any(str_detect(zona_de_uso_registro, "ZEIS")),
        TRUE,
        FALSE
      ),
      ind_parcelamento = if_else(any(ind_parcelamento == TRUE), TRUE, FALSE),
      geometry = first(geometry)
    ) |>
    # Somas os valores dos agrupamentos
    group_by(sql_incra_composto) |>
    summarize(
      id_empreendimento_num = first(id_empreendimento_num),
      tipo_match = first(tipo_match),
      ano_aprovacao = first(ano_aprovacao),
      ano_execucao = first(ano_execucao),
      n_alvaras = sum(n_alvaras),
      n_alvaras_aprovacao = sum(n_alvaras_aprovacao),
      n_alvaras_execucao = sum(n_alvaras_execucao),
      data_autuacao_projeto = first(data_autuacao_projeto),
      data_validacao_projeto = first(data_validacao_projeto),
      data_autuacao_execucao = last(data_autuacao_execucao),
      data_validacao_execucao = first(data_validacao_execucao),
      diff_dias_projeto = interval(
        data_autuacao_projeto,
        data_validacao_projeto
      ) /
        days(1),
      diff_dias_execucao = interval(
        data_validacao_projeto,
        data_validacao_execucao
      ) /
        days(1),
      unidade_pmsp = map_chr(
        list(unique(unidade_pmsp)),
        ~ paste0(.x, collapse = "; ")
      ),
      categoria_de_uso_grupo = if_else(
        any(categoria_de_uso_grupo == "ERP"),
        "ERP",
        "ERM"
      ),
      categoria_de_uso_lista = map_chr(
        list(unique(na.omit(categoria_de_uso_lista))),
        ~ paste0(.x, collapse = "; ")
      ),
      area_do_terreno = sum(area_do_terreno),
      area_da_construcao = sum(area_da_construcao),
      n_blocos = sum(n_blocos),
      n_pavimentos = sum(n_pavimentos),
      n_unidades = sum(n_unidades),
      n_pavimentos_por_bloco = n_pavimentos / n_blocos,
      n_unidades_por_bloco = n_unidades / n_blocos,
      n_unidades_his = sum(n_unidades_his),
      n_unidades_his_por_bloco = n_unidades_his / n_blocos,
      n_unidades_hmp = sum(n_unidades_hmp),
      n_unidades_hmp_por_bloco = n_unidades_hmp / n_blocos,
      n_unidades_r2h_r2v = sum(n_unidades_r2h_r2v),
      n_unidades_r2h_r2v_por_bloco = n_unidades_r2h_r2v / n_blocos,
      sql_incra = first(sql_incra),
      sql_incra_composto = first(sql_incra_composto),
      sql_incra_lista = map_chr(
        list(unique(na.omit(sql_incra))),
        ~ paste0(.x, collapse = "; ")
      ),
      n_enderecos = sum(n_enderecos),
      endereco = first(na.omit(endereco)),
      endereco_lista = map_chr(
        list(unique(na.omit(endereco))),
        ~ paste0(.x, collapse = "; ")
      ),
      distrito = first(na.omit(distrito)),
      subprefeitura = first(na.omit(subprefeitura)),
      zona_de_uso_registro = map_chr(
        list(unique(na.omit(zona_de_uso_registro))),
        ~ paste0(.x, collapse = "; ")
      ),
      ind_edificacao_nova = if_else(
        any(ind_edificacao_nova == TRUE),
        TRUE,
        FALSE
      ),
      ind_aprovacao = if_else(any(ind_aprovacao == TRUE), TRUE, FALSE),
      ind_execucao = if_else(any(ind_execucao == TRUE), TRUE, FALSE),
      ind_r2h = if_else(any(ind_r2h == TRUE), TRUE, FALSE),
      ind_r2v = if_else(any(ind_r2v == TRUE), TRUE, FALSE),
      ind_his = if_else(any(ind_his == TRUE), TRUE, FALSE),
      ind_hmp = if_else(any(ind_hmp == TRUE), TRUE, FALSE),
      ind_ezeis = if_else(any(ind_ezeis == TRUE), TRUE, FALSE),
      ind_uso_misto = if_else(
        categoria_de_uso_grupo != "Outra" &
          ind_edificacao_nova == TRUE &
          any(str_detect(
            categoria_de_uso_lista,
            "NR|C1|C2|C3|S1|S2|S3|E1|E2|E3|E4"
          )),
        TRUE,
        FALSE
      ),
      ind_zeis = if_else(
        any(str_detect(zona_de_uso_registro, "ZEIS")),
        TRUE,
        FALSE
      ),
      ind_parcelamento = if_else(any(ind_parcelamento == TRUE), TRUE, FALSE),
      geometry = first(geometry)
    ) |>
    ungroup() |>
    # Ajusta subnotificação de NA
    mutate(across(where(is.numeric), ~ ifelse(.x == 0, NA, .x)))

  # Combina dois dataframes
  amostra_por_emp <-
    bind_rows(amostra_emp, atts_ind_parcelamento)

  # Exporta alvarás agrupados
  amostra_por_emp <- amostra_por_emp |>
    filter(ind_aprovacao == TRUE & ind_execucao == TRUE)

  amostra_por_emp <- st_sf(
    amostra_por_emp,
    amostra_por_emp$geometry,
    crs = 31983
  )
  amostra_por_emp <- subset(
    amostra_por_emp,
    select = -c(geometry, sql_incra_composto, sql_incra_lista)
  )
  amostra_por_emp <- amostra_por_emp |>
    rename(geometry = amostra_por_emp.geometry) |>
    filter(!(n_unidades <= 5) | is.na(n_unidades))

  return(amostra_por_emp)
}
