# Identify developments ----
#
# Groups permits that belong to the same development and summarises each
# development. The rules follow `original/3_alvaras_empreendimentos_pde.R`;
# the regression tests check that the output reproduces version 3. Rules
# marked "Questão para os autores" are kept as in version 3 and listed in
# `validation/README.md`.

import::from(
  dplyr,
  across,
  arrange,
  bind_rows,
  case_when,
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
  select,
  summarise,
  where
)
import::from(lubridate, year)
import::from(purrr, map_chr)
import::from(stringr, str_detect, str_split, str_trim)

uso_residencial <- "R2V|R202|R302|R2H|R301|R302|R303|(?<!N)R1|(?<!N)R2"
uso_nao_residencial <- "NR|C1|C2|C3|S1|S2|S3|E1|E2|E3|E4"

identify_developments <- function(geo_alvaras, dist_max = 100) {
  permits <- sf::st_sf(geo_alvaras, crs = 31983)
  permits <- mutate(permits, sql_incra_composto = combine_sql(sql_incra))

  match_pairs <- classify_match_pairs(permits, dist_max)
  permits <- assign_developments(permits, match_pairs)

  # Summaries carry a row index and recover the point at the end.
  points <- sf::st_geometry(permits)
  permits <- sf::st_drop_geometry(permits)
  permits <- mutate(permits, ponto = seq_len(n()))

  permits <- flag_parcelamento(permits)
  developments <- summarise_developments(permits)

  developments <- filter(developments, ind_aprovacao & ind_execucao)
  developments <- filter(developments, is.na(n_unidades) | n_unidades > 5)
  developments <- mutate(developments, geometry = points[ponto])
  developments <- select(
    developments,
    -c(ponto, sql_incra_composto, sql_incra_lista)
  )
  developments <- sf::st_sf(developments, sf_column_name = "geometry")
  return(developments)
}

# Match permits ----

combine_sql <- function(sql_incra) {
  # Permits may list the same SQLs in a different order.
  sql_tokens <- str_split(sql_incra, ",")
  sql_incra_composto <- map_chr(
    sql_tokens,
    \(tokens) paste(sort(str_trim(tokens), method = "radix"), collapse = ",")
  )
  return(sql_incra_composto)
}

pair_combinations <- function(indices) {
  indices <- sort(unique(indices))
  if (length(indices) < 2) {
    return(data.frame(left = integer(), right = integer()))
  }
  pairs <- utils::combn(indices, 2)
  return(data.frame(left = pairs[1, ], right = pairs[2, ]))
}

shared_sql_pairs <- function(sql_values) {
  sql_tokens <- str_split(sql_values, ",")
  sql_tokens <- lapply(sql_tokens, str_trim)
  # Questão para os autores: permits without an SQL all share this token, so
  # the matcher treats them as sharing one lot.
  sql_tokens <- lapply(sql_tokens, function(tokens) {
    tokens[is.na(tokens) | tokens == ""] <- "__MISSING_SQL__"
    return(unique(tokens))
  })

  permit_index <- rep(seq_along(sql_tokens), lengths(sql_tokens))
  token_index <- split(permit_index, unlist(sql_tokens, use.names = FALSE))
  pairs <- lapply(token_index, pair_combinations)
  pairs <- bind_rows(pairs)
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
  pairs <- filter(pairs, left < right)
  pairs <- dplyr::distinct(pairs, left, right)
  return(pairs)
}

# Two permits match when they share an SQL, share a land area, and lie
# within `distance_threshold` metres. Match types:
#   1: perfect (all three)
#   2: SQL and land area
#   3: SQL and distance
#   4: land area and distance
#   5: SQL only
# Partial matches (2-4) become perfect when the building area or the number of
# units is also equal.
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
  same_value <- function(x) {
    return(!is.na(x[left]) & !is.na(x[right]) & x[left] == x[right])
  }
  same_land_area <- same_value(geo_permits$area_do_terreno)

  candidates$match_type <- case_when(
    candidates$shared_sql & candidates$nearby & same_land_area ~ 1L,
    candidates$shared_sql & !candidates$nearby & same_land_area ~ 2L,
    candidates$shared_sql & candidates$nearby & !same_land_area ~ 3L,
    !candidates$shared_sql & candidates$nearby & same_land_area ~ 4L,
    candidates$shared_sql & !candidates$nearby & !same_land_area ~ 5L,
    .default = 0L
  )

  partial <- candidates$match_type %in% 2:4
  promoted <- same_value(geo_permits$area_da_construcao) |
    same_value(geo_permits$n_unidades)
  candidates$match_type[partial & promoted] <- 1L
  candidates <- filter(candidates, match_type > 0)
  return(candidates)
}

component_ids <- function(match_pairs, number_of_permits) {
  ids <- vector("list", 5)

  # igraph numbers components in order of their lowest vertex, which matches
  # the labels of the original dense adjacency matrices.
  for (type in seq_len(5)) {
    edges <- match_pairs[match_pairs$match_type == type, c("left", "right")]
    graph <- igraph::make_empty_graph(number_of_permits, directed = FALSE)
    graph <- igraph::add_edges(graph, as.vector(t(as.matrix(edges))))
    ids[[type]] <- igraph::components(graph)$membership
  }

  names(ids) <- paste0("empreendimento_id_match_", seq_len(5))
  ids <- as.data.frame(ids)
  return(ids)
}

# Each permit joins one development. A perfect-match group takes priority,
# then a partial-match group (lowest type first), then an SQL-only group.
# Permits in no group form a development of their own.
assign_developments <- function(permits, match_pairs) {
  components <- as.matrix(component_ids(match_pairs, nrow(permits)))
  in_group <- apply(components, 2, \(id) tabulate(id)[id] > 1)
  row <- seq_len(nrow(permits))

  # Questão para os autores: the original comment says a permit in several
  # partial-match groups joins the one with more units. The code compares the
  # permit's own units across its groups, which always tie, so the permit
  # joins its lowest partial type.
  tipo_final <- case_when(
    in_group[, 1] ~ 1L,
    in_group[, 2] ~ 2L,
    in_group[, 3] ~ 3L,
    in_group[, 4] ~ 4L,
    in_group[, 5] ~ 5L
  )
  grupo <- components[cbind(row, tipo_final)]
  id_empreendimento <- case_when(
    tipo_final == 1 ~ paste0("P_", grupo),
    tipo_final %in% 2:4 ~ paste0("M", tipo_final, "_", grupo),
    tipo_final == 5 ~ paste0("U_", grupo),
    .default = paste0("S_", row)
  )

  permits <- mutate(
    permits,
    id_empreendimento_num = match(
      id_empreendimento,
      sort(unique(id_empreendimento), method = "radix")
    ),
    tipo_match = case_when(
      tipo_final == 1 ~ "perfeito",
      tipo_final %in% 2:4 ~ paste0("parcial_", tipo_final),
      tipo_final == 5 ~ "único",
      .default = "sem match"
    )
  )
  return(permits)
}

# Flag parcelamentos ----
#
# A lot (SQL) is a parcelamento when it was split into sub-lots with their own
# permits. Rules validated in Insper/Abrainc meetings, Nov-Dec 2022.

flag_parcelamento <- function(permits) {
  lots <- filter(permits, !ind_sql_incra_null)
  lots <- mutate(
    lots,
    n_aprovacao = sum(
      ind_aprovacao & !ind_correcao & ind_edificacao_nova,
      na.rm = TRUE
    ),
    n_execucao = sum(
      ind_execucao & !ind_correcao & ind_edificacao_nova,
      na.rm = TRUE
    ),
    .by = sql_incra_composto
  )
  lots <- mutate(
    lots,
    n_areas_terreno = n_distinct(area_do_terreno[
      !is.na(area_do_terreno) &
        (ind_edificacao_nova | ind_loteamento | ind_conclusao)
    ]),
    .by = c(sql_incra_composto, descricao_tipo)
  )
  lots <- filter(lots, any(ind_edificacao_nova), .by = sql_incra_composto)

  # Questão para os autores: the original counts plano integrado permits with
  # `length(which(...) & ind_correcao == FALSE)`, which ignores the correction
  # filter. It has no effect on version 3.
  lots <- mutate(
    lots,
    tem_loteamento = any(descricao_tipo == "PLANO INTEGRADO"),
    # Case 1: plano integrado with several land areas and approvals.
    ind_caso_1 = tem_loteamento &
      n_areas_terreno > 1 &
      (n_aprovacao > 1 | n_execucao > 1),
    # Case 2: several completion permits and several land areas.
    ind_caso_2 = sum(ind_conclusao & !ind_correcao, na.rm = TRUE) > 1 &
      n_areas_terreno > 1,
    .by = sql_incra_composto
  )
  # Case 3: several addresses among permits of the same type.
  lots <- mutate(
    lots,
    ind_caso_3 = n_distinct(endereco[
      (ind_edificacao_nova | tem_loteamento | ind_conclusao) &
        !ind_correcao
    ]) >
      1 &
      n_areas_terreno > 1,
    .by = c(sql_incra_composto, descricao_tipo)
  )
  lots <- mutate(
    lots,
    ind_parcelamento = any(ind_edificacao_nova) &
      (any(ind_caso_1) | any(ind_caso_2) | any(ind_caso_3)),
    .by = sql_incra_composto
  )

  # Questão para os autores: the original overwrote `ind_loteamento` with the
  # lot-level flag and then joined on every shared column. Permits whose flag
  # changed found no match and were left out, as were permits without an SQL.
  lots <- filter(lots, ind_loteamento == tem_loteamento)
  flags <- select(lots, id, ind_parcelamento)
  permits <- left_join(permits, flags, by = "id")

  excluded <- sum(is.na(permits$ind_parcelamento))
  cli::cli_inform(
    "{excluded} permit{?s} left out of the developments by the version 3 parcelamento rule."
  )
  return(permits)
}

# Summarise developments ----

first_valid <- function(x, keep = TRUE) {
  valid <- x[which(keep & !is.na(x))]
  return(first(valid))
}

collapse_unique <- function(x) {
  return(paste(unique(stats::na.omit(x)), collapse = "; "))
}

summarise_permits <- function(grouped_permits, ...) {
  developments <- summarise(
    grouped_permits,
    ...,
    tipo_match = first(tipo_match),
    ano_aprovacao = year(first(data_aprovacao[which(aprovacao_nova)])),
    ano_execucao = year(first(data_aprovacao[which(execucao_nova)])),
    n_alvaras = n(),
    n_alvaras_aprovacao = sum(projeto_valido, na.rm = TRUE),
    n_alvaras_execucao = sum(execucao_valida, na.rm = TRUE),
    data_autuacao_projeto = last(data_autuacao[which(projeto_valido)]),
    data_validacao_projeto = first(data_aprovacao[which(projeto_valido)]),
    data_autuacao_execucao = last(data_autuacao[which(execucao_valida)]),
    data_validacao_execucao = first(data_aprovacao[which(execucao_valida)]),
    diff_dias_projeto = as.numeric(
      data_validacao_projeto - data_autuacao_projeto
    ),
    diff_dias_execucao = as.numeric(
      data_validacao_execucao - data_validacao_projeto
    ),
    unidade_pmsp = paste(unique(unidade_pmsp), collapse = "; "),
    categoria_de_uso_grupo = case_when(
      any(ind_his) | any(ind_hmp) | any(ind_ezeis) ~ "ERP",
      any(str_detect(categoria_de_uso, uso_residencial)) ~ "ERM",
      .default = "Outra"
    ),
    categoria_de_uso_lista = collapse_unique(categoria_de_uso),
    area_do_terreno = first_valid(area_do_terreno, fonte_areas),
    area_da_construcao = first_valid(area_da_construcao, fonte_areas),
    n_blocos = first_valid(n_blocos, fonte_unidades),
    n_pavimentos = first_valid(n_pavimentos, fonte_unidades),
    n_unidades = first_valid(n_unidades, fonte_unidades),
    n_pavimentos_por_bloco = n_pavimentos / n_blocos,
    n_unidades_por_bloco = n_unidades / n_blocos,
    n_unidades_his = first_valid(unid_his, fonte_unidades),
    n_unidades_his_por_bloco = n_unidades_his / n_blocos,
    n_unidades_hmp = first_valid(unid_hmp, fonte_unidades),
    n_unidades_hmp_por_bloco = n_unidades_hmp / n_blocos,
    n_unidades_r2h_r2v = first_valid(unid_r2h_r2v, fonte_unidades),
    n_unidades_r2h_r2v_por_bloco = n_unidades_r2h_r2v / n_blocos,
    sql_incra = first(sql_incra),
    sql_incra_composto = first(sql_incra_composto),
    sql_incra_lista = collapse_unique(sql_incra),
    n_enderecos = n_distinct(endereco),
    endereco = first_valid(endereco),
    endereco_lista = collapse_unique(endereco_raw),
    distrito = first_valid(distrito),
    subprefeitura = first_valid(subprefeitura),
    zona_de_uso_registro = collapse_unique(zona_de_uso_registro),
    across(
      c(
        ind_edificacao_nova,
        ind_aprovacao,
        ind_execucao,
        ind_r2v,
        ind_r2h,
        ind_his,
        ind_hmp,
        ind_ezeis
      ),
      any
    ),
    ind_uso_misto = categoria_de_uso_grupo != "Outra" &
      ind_edificacao_nova &
      str_detect(categoria_de_uso_lista, uso_nao_residencial),
    ind_zeis = str_detect(zona_de_uso_registro, "ZEIS"),
    ind_parcelamento = any(ind_parcelamento),
    ponto = first(ponto),
    .groups = "drop"
  )
  return(developments)
}

# Parcelamentos are summarised per sub-lot (same SQL and land area), then
# added up per SQL.
combine_sub_lots <- function(sub_lots) {
  # Questão para os autores: sub-lots are ordered by land area here, so
  # `first()` takes dates, SQL, and address from the smallest sub-lot rather
  # than the most recent one.
  developments <- summarise(
    group_by(sub_lots, sql_incra_composto),
    id_empreendimento_num = first(id_empreendimento_num),
    tipo_match = first(tipo_match),
    ano_aprovacao = first(ano_aprovacao),
    ano_execucao = first(ano_execucao),
    across(c(n_alvaras, n_alvaras_aprovacao, n_alvaras_execucao), sum),
    data_autuacao_projeto = first(data_autuacao_projeto),
    data_validacao_projeto = first(data_validacao_projeto),
    data_autuacao_execucao = last(data_autuacao_execucao),
    data_validacao_execucao = first(data_validacao_execucao),
    diff_dias_projeto = as.numeric(
      data_validacao_projeto - data_autuacao_projeto
    ),
    diff_dias_execucao = as.numeric(
      data_validacao_execucao - data_validacao_projeto
    ),
    unidade_pmsp = paste(unique(unidade_pmsp), collapse = "; "),
    categoria_de_uso_grupo = if_else(
      any(categoria_de_uso_grupo == "ERP"),
      "ERP",
      "ERM"
    ),
    categoria_de_uso_lista = collapse_unique(categoria_de_uso_lista),
    area_do_terreno = sum(area_lote),
    across(
      c(
        area_da_construcao,
        n_blocos,
        n_pavimentos,
        n_unidades,
        n_unidades_his,
        n_unidades_hmp,
        n_unidades_r2h_r2v
      ),
      sum
    ),
    n_pavimentos_por_bloco = n_pavimentos / n_blocos,
    n_unidades_por_bloco = n_unidades / n_blocos,
    n_unidades_his_por_bloco = n_unidades_his / n_blocos,
    n_unidades_hmp_por_bloco = n_unidades_hmp / n_blocos,
    n_unidades_r2h_r2v_por_bloco = n_unidades_r2h_r2v / n_blocos,
    sql_incra = first(sql_incra),
    sql_incra_lista = collapse_unique(sql_incra),
    n_enderecos = sum(n_enderecos),
    endereco = first_valid(endereco),
    endereco_lista = collapse_unique(endereco),
    distrito = first_valid(distrito),
    subprefeitura = first_valid(subprefeitura),
    zona_de_uso_registro = collapse_unique(zona_de_uso_registro),
    across(
      c(
        ind_edificacao_nova,
        ind_aprovacao,
        ind_execucao,
        ind_r2v,
        ind_r2h,
        ind_his,
        ind_hmp,
        ind_ezeis
      ),
      any
    ),
    ind_uso_misto = categoria_de_uso_grupo != "Outra" &
      ind_edificacao_nova &
      str_detect(categoria_de_uso_lista, uso_nao_residencial),
    ind_zeis = str_detect(zona_de_uso_registro, "ZEIS"),
    ind_parcelamento = any(ind_parcelamento),
    ponto = first(ponto),
    .groups = "drop"
  )
  # Zero counts and areas are unreported values.
  developments <- mutate(
    developments,
    across(where(is.numeric), \(x) if_else(x == 0, NA, x))
  )
  return(developments)
}

summarise_developments <- function(permits) {
  permits <- mutate(
    permits,
    aprovacao_nova = ind_aprovacao & ind_edificacao_nova,
    execucao_nova = ind_execucao & ind_edificacao_nova,
    projeto_valido = aprovacao_nova & !ind_correcao,
    execucao_valida = execucao_nova & !ind_correcao,
    fonte_areas = ind_edificacao_nova &
      (ind_aprovacao | ind_execucao | ind_conclusao),
    fonte_unidades = ind_edificacao_nova & (ind_aprovacao | ind_execucao)
  )
  # Most recent permit first: `first()` takes the latest value.
  permits <- arrange(permits, desc(data_aprovacao))

  single_lots <- filter(permits, !ind_parcelamento)
  developments <- summarise_permits(group_by(
    single_lots,
    id_empreendimento_num
  ))

  parcelamentos <- filter(permits, ind_parcelamento)
  parcelamentos <- mutate(parcelamentos, area_lote = area_do_terreno)
  sub_lots <- summarise_permits(
    group_by(parcelamentos, sql_incra_composto, area_lote),
    id_empreendimento_num = first(id_empreendimento_num)
  )
  parcel_developments <- combine_sub_lots(sub_lots)

  developments <- bind_rows(developments, parcel_developments)
  return(developments)
}
