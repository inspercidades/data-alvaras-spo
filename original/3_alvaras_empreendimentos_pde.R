library(fs)
library(here)
library(sf)
library(tidyverse)
library(tidylog)
library(janitor)
library(tidygeocoder)
library(sfarrow)
library(arrow)
library(ggplot2)
library(stringr)
library(igraph)
library(units)
library(ggplot2)
library(scales)

# 1. Importa --------------------------------------------------------------

geo_alvaras <- sfarrow::st_read_parquet(here("2025_AtualizacaoDadosAlvaras", "3.DadosTratados", 
                                        "geo_alvaras_pde.parquet"))

# Define como sf com o CRS correto 
geo_alvaras <- st_sf(geo_alvaras, crs = 31983)

# 2. Observações com mais de um SQL --------------------------------------------------------------

# Precisamos identificar as observações com mais de um SQL que possuem os mesmos SQLs, mas que podem não estar na mesma ordem
geo_alvaras <- geo_alvaras %>%
  mutate(
    sql_incra_composto = sql_incra %>%
      str_split(",") %>%
      lapply(function(x) sort(str_trim(x))) %>%
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

# Amostra
amostra <- geo_alvaras

# Cria matriz para identificar se dois alvarás compartilham ao menos um SQL
sql_list <- amostra$sql_incra_composto %>%
  str_split(",") %>%
  lapply(str_trim)

n <- length(sql_list)
mat_sql <- matrix(0, nrow = n, ncol = n)

for (i in 1:n) {
  for (j in 1:n) {
    mat_sql[i, j] <- length(intersect(sql_list[[i]], sql_list[[j]])) > 0
  }
}

# Cria matriz de distâncias
mat_dist <- drop_units(as.matrix(st_distance(amostra)))

# Cria matriz da diferença de áreas
areas <- amostra$area_do_terreno
mat_area_diff <- abs(outer(areas, areas, "-"))

# Cria matriz de empreendimentos
dist_max <- 100
mat_match <- matrix(0, nrow = nrow(amostra), ncol = nrow(amostra))

# Condições principais
cond_sql  <- (mat_sql == 1)
cond_dist <- (mat_dist <= dist_max)
cond_area <- (mat_area_diff == 0)
cond_area[is.na(cond_area)] <- FALSE

# Inicializa matriz de match com 0
mat_match <- matrix(0, nrow = nrow(amostra), ncol = nrow(amostra))

# Match perfeito
mat_match[cond_sql & cond_dist & cond_area] <- 1

# Matches parciais
mat_match[cond_sql & !cond_dist & cond_area] <- 2  # SQL e área
mat_match[cond_sql & cond_dist & !cond_area] <- 3  # SQL e distância
mat_match[!cond_sql & cond_dist & cond_area] <- 4  # área e distância

# Matches únicos
mat_match[cond_sql & !cond_dist & !cond_area] <- 5  # SQL

# Diagonal sempre tipo 1 (match consigo mesmo)
diag(mat_match) <- 1

# ---------------------------------------------
# ÁRVORE DE DECISÃO para promover matches parciais

# Extrai variáveis auxiliares
area_construcao <- amostra$area_da_construcao
n_unidades <- amostra$n_unidades

# Loop sobre matches parciais (tipos 2, 3 e 4)
for (tipo in 2:4) {
  idx <- which(mat_match == tipo, arr.ind = TRUE)
  
  for (k in seq_len(nrow(idx))) {
    i <- idx[k, 1]
    j <- idx[k, 2]
    
    # Compara area_da_construcao
    if (!is.na(area_construcao[i]) && !is.na(area_construcao[j]) &&
        area_construcao[i] == area_construcao[j]) {
      mat_match[i, j] <- 1
      next
    }
    
    # Se não, compara n_unidades
    if (!is.na(n_unidades[i]) && !is.na(n_unidades[j]) &&
        n_unidades[i] == n_unidades[j]) {
      mat_match[i, j] <- 1
    }
  }
}

# Reforça simetria e diagonal
mat_match <- pmin(mat_match, t(mat_match))
diag(mat_match) <- 1

rm(cond_area, cond_dist, cond_sql, mat_area_diff, mat_sql, sql_list)

# ---------------------------------------------
# Gera os IDs de empreendimentos por tipo de match
empreendimento_ids <- list()

for (tipo in 1:5) {
  mat_tipo <- (mat_match == tipo)
  diag(mat_tipo) <- 1  # garante autoconexão
  mat_tipo <- (mat_tipo | t(mat_tipo)) * 1  # força simetria
  g <- graph_from_adjacency_matrix(mat_tipo, mode = "undirected")
  empreendimento_ids[[paste0("empreendimento_id_match_", tipo)]] <- components(g)$membership
}

# Adiciona ao dataframe
amostra <- bind_cols(amostra, as.data.frame(empreendimento_ids))

# ---------------------------------------------
# Gera um único ID de empreendimento 

# 1. Conta o tamanho de cada componente para cada tipo de match
tamanhos <- bind_rows(
  amostra %>% st_drop_geometry() %>% mutate(tipo = 1, empreendimento_id = empreendimento_id_match_1) %>% count(empreendimento_id, tipo, name = "tam"),
  amostra %>% st_drop_geometry() %>% mutate(tipo = 2, empreendimento_id = empreendimento_id_match_2) %>% count(empreendimento_id, tipo, name = "tam"),
  amostra %>% st_drop_geometry() %>% mutate(tipo = 3, empreendimento_id = empreendimento_id_match_3) %>% count(empreendimento_id, tipo, name = "tam"),
  amostra %>% st_drop_geometry() %>% mutate(tipo = 4, empreendimento_id = empreendimento_id_match_4) %>% count(empreendimento_id, tipo, name = "tam"),
  amostra %>% st_drop_geometry() %>% mutate(tipo = 5, empreendimento_id = empreendimento_id_match_5) %>% count(empreendimento_id, tipo, name = "tam")
)

# 2. Prepara os dados da amostra com IDs e tamanhos
# Transforma em dataframe normal
amostra_df <- amostra %>% st_drop_geometry()

# Agora faz as manipulações
amostra_ext <- amostra_df %>%
  mutate(row_id = row_number(), n_unidades = n_unidades) %>%
  pivot_longer(cols = starts_with("empreendimento_id_match_"), names_to = "tipo", values_to = "empreendimento_id") %>%
  mutate(tipo = as.numeric(str_extract(tipo, "\\d+"))) %>%
  left_join(tamanhos, by = c("empreendimento_id", "tipo"))

# 3. Para matches parciais, escolhe o empreendimento com mais unidades
match_parcial <- amostra_ext %>%
  filter(tipo %in% 2:4, tam > 1) %>%
  group_by(row_id) %>%
  slice_max(order_by = n_unidades, with_ties = FALSE) %>%
  ungroup()

# 4. Define o ID final e o tipo de match com a prioridade
id_final <- amostra_ext %>%
  filter((tipo == 1 & tam > 1) | 
           (tipo %in% 2:4 & tam > 1) | 
           (tipo == 5 & tam > 1)) %>%
  group_by(row_id) %>%
  summarise(
    tipo_final = case_when(
      any(tipo == 1) ~ 1,
      any(tipo %in% 2:4) ~ NA_real_,  # Preenche depois com match_parcial
      any(tipo == 5) ~ 5,
      TRUE ~ NA_real_
    ),
    empreendimento_id_final = case_when(
      any(tipo == 1) ~ empreendimento_id[tipo == 1][1],  # Pega o primeiro do match perfeito
      any(tipo %in% 2:4) ~ NA_real_,  # Preenche depois com match_parcial
      any(tipo == 5) ~ empreendimento_id[tipo == 5][1],  # Pega o primeiro do match único
      TRUE ~ NA_real_
    ),
    .groups = "drop"
  )

id_final <- id_final %>%
  left_join(
    match_parcial %>% 
      select(row_id, empreendimento_id_parcial = empreendimento_id, tipo_parcial = tipo),
    by = "row_id"
  ) %>%
  mutate(
    tipo_final = if_else(is.na(tipo_final), tipo_parcial, tipo_final),
    empreendimento_id_final = if_else(is.na(empreendimento_id_final), empreendimento_id_parcial, empreendimento_id_final)
  ) %>%
  select(row_id, tipo_final, empreendimento_id_final)

# 5. Junta ao dataframe principal e define "sem match"
amostra <- amostra %>%
  mutate(row_id = row_number()) %>%
  left_join(id_final, by = "row_id") %>%
  mutate(
    id_empreendimento = case_when(
      !is.na(empreendimento_id_final) & tipo_final == 1 ~ paste0("P_", empreendimento_id_final),
      !is.na(empreendimento_id_final) & tipo_final %in% 2:4 ~ paste0("M", tipo_final, "_", empreendimento_id_final),
      !is.na(empreendimento_id_final) & tipo_final == 5 ~ paste0("U_", empreendimento_id_final),
      TRUE ~ paste0("S_", row_id)  # Sem match
    ),
    tipo_match = case_when(
      tipo_final == 1 ~ "perfeito",
      tipo_final %in% 2:4 ~ paste0("parcial_", tipo_final),
      tipo_final == 5 ~ "único",
      TRUE ~ "sem match"
    )
  ) %>%
  select(-row_id, -empreendimento_id_final, -tipo_final)

amostra <- amostra %>%
  mutate(
    id_empreendimento_num = dense_rank(as.integer(as.factor(id_empreendimento)))  # Cria o ID sequencial
  )

amostra <- amostra %>%
  subset(select = c(-empreendimento_id_match_1, -empreendimento_id_match_2, -empreendimento_id_match_3,
                    -empreendimento_id_match_4, -empreendimento_id_match_5, -id_empreendimento))

# 4. Cria conjunto de dados por empreendimento ---------------------

# Identifica e trata casos de parcelamento
# Validado em uma série de reuniões Insper/Abrainc entre nov-dez de 22
amostra_parcelamento <- amostra %>%
  st_drop_geometry() %>%
  
  # A - Tipifica parcelamento
  left_join(
    # Unifica com atributo novo - Indicador de parcelamento
    amostra %>%
      # Somente sql's válidos
      filter(ind_sql_incra_null == FALSE) %>%
      # Contabiliza aprovações e execuções em alvará relevante para o sql/lote
      group_by(sql_incra_composto) %>%
      mutate(
        n_aprovacao = length(which(ind_aprovacao == TRUE &
                                     ind_correcao == FALSE &
                                     ind_edificacao_nova == TRUE)),
        n_execucao = length(which(ind_execucao == TRUE &
                                    ind_correcao == FALSE &
                                    ind_edificacao_nova == TRUE))
      ) %>%
      ungroup() %>%
      # Tipifica sql's com áreas de terreno diferentes em alvarás relevantes de tipo igual
      group_by(sql_incra_composto, descricao_tipo) %>%
      mutate(    
        n_areas_terreno = length(unique(area_do_terreno[
          !is.na(area_do_terreno) & (ind_edificacao_nova == TRUE |
                                       ind_loteamento == TRUE | ind_conclusao == TRUE)]))
      ) %>%
      ungroup() %>%
      # Somente sql's com algum alvará relevante
      group_by(sql_incra_composto) %>%
      filter(any(ind_edificacao_nova == TRUE)) %>%
      #1 Sql possui alvará do tipo loteamento
      ## Descricao := {PLANO INTEGRADO}
      group_by(sql_incra_composto) %>%
      mutate(
        n_loteamento = length(which(descricao_tipo == "PLANO INTEGRADO") & 
                                ind_correcao == FALSE),
        ind_loteamento = if_else(n_loteamento >= 1, TRUE, FALSE),
        ind_caso_1 = if_else(n_loteamento >= 1 &
                               n_areas_terreno > 1 &
                               n_aprovacao > 1 |
                               n_loteamento >= 1 &
                               n_areas_terreno > 1 &
                               n_execucao > 1, TRUE, FALSE)
      ) %>%
      #2 Sql possui mais de um alvará de tipo conclusão
      group_by(sql_incra_composto) %>%
      mutate(
        n_caso_2 = length(which(ind_conclusao == TRUE &
                                  ind_correcao == FALSE)),
        ind_caso_2 = if_else(n_caso_2 > 1 & n_areas_terreno > 1,
                             TRUE, FALSE)
      ) %>%
      #3 Sql com diferentes endereços em alvarás de tipo igual 
      group_by(sql_incra_composto, descricao_tipo) %>%
      mutate(
        n_caso_3 = length(unique(endereco[
          (ind_edificacao_nova == TRUE |
             ind_loteamento == TRUE | ind_conclusao == TRUE) &
            ind_correcao == FALSE])),
        ind_caso_3 = if_else(n_caso_3 > 1 & n_areas_terreno > 1, 
                             TRUE, FALSE)
      ) %>%
      ungroup() %>%
      # Cria Indicador de parcelamento
      group_by(sql_incra_composto) %>%
      mutate(ind_parcelamento = if_else(
        any(ind_edificacao_nova == TRUE) &
          (any(ind_caso_1 == TRUE) | any(ind_caso_2 == TRUE) |
             any(ind_caso_3 == TRUE)),
        TRUE, FALSE)) %>%
      ungroup()
  )
  
# Agrupa por empreendimento
amostra_emp <- amostra_parcelamento %>%
  filter(ind_parcelamento == FALSE) %>%
  group_by(id_empreendimento_num) %>%
  arrange(desc(data_aprovacao)) %>% # importante para inferir que posição 1 é sempre data mais atual
  summarize(
    tipo_match = first(tipo_match),
    ano_aprovacao = year(first(data_aprovacao[which(
      ind_aprovacao == TRUE & ind_edificacao_nova == TRUE)])),
    ano_execucao = year(first(data_aprovacao[which(
      ind_execucao == TRUE & ind_edificacao_nova == TRUE)])),
    n_alvaras = n(),
    n_alvaras_aprovacao = length(which(ind_aprovacao == TRUE &
                                         ind_edificacao_nova == TRUE &
                                         ind_correcao == FALSE)),
    n_alvaras_execucao = length(which(ind_execucao == TRUE &
                                        ind_edificacao_nova == TRUE &
                                        ind_correcao == FALSE)),
    data_autuacao_projeto = last(data_autuacao[which(
      ind_aprovacao == TRUE & ind_edificacao_nova == TRUE & ind_correcao == FALSE)]),
    data_validacao_projeto = first(data_aprovacao[which(
      ind_aprovacao == TRUE & ind_edificacao_nova == TRUE & ind_correcao == FALSE)]),
    data_autuacao_execucao = last(data_autuacao[which(
      ind_execucao == TRUE & ind_edificacao_nova == TRUE & ind_correcao == FALSE)]),
    data_validacao_execucao = first(data_aprovacao[which(
      ind_execucao == TRUE & ind_edificacao_nova == TRUE & ind_correcao == FALSE)]),
    diff_dias_projeto = interval(data_autuacao_projeto, 
                                 data_validacao_projeto) / days(1),
    diff_dias_execucao = interval(data_validacao_projeto, 
                                  data_validacao_execucao) / days(1),
    unidade_pmsp = map_chr(list(unique(unidade_pmsp)), ~paste0(.x, collapse = "; ")),
    categoria_de_uso_grupo = as.factor(case_when(
      any(ind_his == TRUE) | any(ind_hmp == TRUE) | any(ind_ezeis == TRUE) ~ "ERP",
      any(str_detect(categoria_de_uso,
                     "R2V|R202|R302|R2H|R301|R302|R303|(?<!N)R1|(?<!N)R2")) ~ "ERM",
      TRUE ~ "Outra"
    )),
    categoria_de_uso_lista = map_chr(list(unique(na.omit(categoria_de_uso))), 
                                     ~ paste0(.x, collapse = "; ")),
    area_do_terreno = first(na.omit(area_do_terreno[which(
      ind_edificacao_nova == TRUE & (ind_aprovacao == TRUE | 
                                       ind_execucao == TRUE | 
                                       ind_conclusao == TRUE))])),
    area_da_construcao = first(na.omit(area_da_construcao[which(
      ind_edificacao_nova == TRUE & (ind_aprovacao == TRUE | 
                                       ind_execucao == TRUE | 
                                       ind_conclusao == TRUE))])),
    n_blocos = first(na.omit(n_blocos[which(
      ind_edificacao_nova == TRUE & (ind_aprovacao == TRUE | 
                                       ind_execucao == TRUE))])),
    n_pavimentos = first(na.omit(n_pavimentos[which(
      ind_edificacao_nova == TRUE & (ind_aprovacao == TRUE | 
                                       ind_execucao == TRUE))])),
    n_unidades = first(na.omit(n_unidades[which(
      ind_edificacao_nova == TRUE & (ind_aprovacao == TRUE | 
                                       ind_execucao == TRUE))])),
    n_pavimentos_por_bloco = n_pavimentos/n_blocos,
    n_unidades_por_bloco = n_unidades/n_blocos,
    n_unidades_his = first(na.omit(unid_his[which(
      ind_edificacao_nova == TRUE & (ind_aprovacao == TRUE | 
                                       ind_execucao == TRUE))])),
    n_unidades_his_por_bloco = n_unidades_his/n_blocos,
    n_unidades_hmp = first(na.omit(unid_hmp[which(
      ind_edificacao_nova == TRUE & (ind_aprovacao == TRUE | 
                                       ind_execucao == TRUE))])),
    n_unidades_hmp_por_bloco = n_unidades_hmp/n_blocos,
    n_unidades_r2h_r2v = first(na.omit(unid_r2h_r2v[which(
      ind_edificacao_nova == TRUE & (ind_aprovacao == TRUE | 
                                       ind_execucao == TRUE))])),
    n_unidades_r2h_r2v_por_bloco = n_unidades_r2h_r2v/n_blocos,
    sql_incra = first(sql_incra),
    sql_incra_composto = first(sql_incra_composto),
    sql_incra_lista = map_chr(list(unique(na.omit(sql_incra))), 
                              ~ paste0(.x, collapse = "; ")),
    n_enderecos = n_distinct(endereco),
    endereco = first(na.omit(endereco)),
    endereco_lista= map_chr(list(unique(na.omit(endereco_raw))), 
                            ~ paste0(.x, collapse = "; ")),
    distrito = first(na.omit(distrito)),
    subprefeitura = first(na.omit(subprefeitura)),
    zona_de_uso_registro = map_chr(list(unique(na.omit(zona_de_uso_registro))), 
                                   ~ paste0(.x, collapse = "; ")),
    ind_edificacao_nova = if_else(any(ind_edificacao_nova == TRUE), TRUE, FALSE),
    ind_aprovacao = if_else(any(ind_aprovacao == TRUE), TRUE, FALSE),
    ind_execucao = if_else(any(ind_execucao == TRUE), TRUE, FALSE),
    ind_r2v = if_else(any(ind_r2v == TRUE), TRUE, FALSE),
    ind_r2h = if_else(any(ind_r2h == TRUE), TRUE, FALSE),
    ind_his = if_else(any(ind_his == TRUE), TRUE, FALSE),
    ind_hmp = if_else(any(ind_hmp == TRUE), TRUE, FALSE),
    ind_ezeis = if_else(any(ind_ezeis == TRUE), TRUE, FALSE),
    ind_uso_misto = if_else(categoria_de_uso_grupo != "Outra" &
                              ind_edificacao_nova == TRUE &
                              any(str_detect(categoria_de_uso_lista,
                                             "NR|C1|C2|C3|S1|S2|S3|E1|E2|E3|E4")), 
                            TRUE, FALSE),
    ind_zeis = if_else(any(str_detect(zona_de_uso_registro,
                                      "ZEIS")),
                       TRUE, FALSE),
    ind_parcelamento = if_else(any(ind_parcelamento == TRUE), TRUE, FALSE),
    geometry = first(geometry)
  ) %>%
  ungroup()

# B - Trata em separado atributos numéricos em casos de parcelamento
  # Somente parcelamentos
atts_ind_parcelamento <- amostra_parcelamento %>% 
  filter(ind_parcelamento == TRUE) %>%
  # Ordena mais recente para mais antigo
  arrange(desc(data_aprovacao)) %>%
  # Agrupa por Área de terreno e pega informação do mais recente por grupo
  ## Grupo = mesmo sql, mesma área de terreno
  group_by(sql_incra_composto, area_do_terreno) %>%
  summarize(
    id_empreendimento_num = first(id_empreendimento_num),
    tipo_match = first(tipo_match),
    ano_aprovacao = year(first(data_aprovacao[which(
      ind_aprovacao == TRUE & ind_edificacao_nova == TRUE)])),
    ano_execucao = year(first(data_aprovacao[which(
      ind_execucao == TRUE & ind_edificacao_nova == TRUE)])),
    n_alvaras = n(),
    n_alvaras_aprovacao = length(which(ind_aprovacao == TRUE &
                                         ind_edificacao_nova == TRUE &
                                         ind_correcao == FALSE)),
    n_alvaras_execucao = length(which(ind_execucao == TRUE &
                                        ind_edificacao_nova == TRUE &
                                        ind_correcao == FALSE)),
    data_autuacao_projeto = last(data_autuacao[which(
      ind_aprovacao == TRUE & ind_edificacao_nova == TRUE & ind_correcao == FALSE)]),
    data_validacao_projeto = first(data_aprovacao[which(
      ind_aprovacao == TRUE & ind_edificacao_nova == TRUE & ind_correcao == FALSE)]),
    data_autuacao_execucao = last(data_autuacao[which(
      ind_execucao == TRUE & ind_edificacao_nova == TRUE & ind_correcao == FALSE)]),
    data_validacao_execucao = first(data_aprovacao[which(
      ind_execucao == TRUE & ind_edificacao_nova == TRUE & ind_correcao == FALSE)]),
    diff_dias_projeto = interval(data_autuacao_projeto, 
                                 data_validacao_projeto) / days(1),
    diff_dias_execucao = interval(data_validacao_projeto, 
                                  data_validacao_execucao) / days(1),
    unidade_pmsp = map_chr(list(unique(unidade_pmsp)), ~paste0(.x, collapse = "; ")),
    categoria_de_uso_grupo = as.factor(case_when(
      any(ind_his == TRUE) | any(ind_hmp == TRUE) | any(ind_ezeis == TRUE) ~ "ERP",
      any(str_detect(categoria_de_uso,
                     "R2V|R202|R302|R2H|R301|R302|R303|(?<!N)R1|(?<!N)R2")) ~ "ERM",
      TRUE ~ "Outra"
    )),
    categoria_de_uso_lista = map_chr(list(unique(na.omit(categoria_de_uso))), 
                                     ~ paste0(.x, collapse = "; ")),
    area_da_construcao = first(na.omit(area_da_construcao[which(
      ind_edificacao_nova == TRUE & (ind_aprovacao == TRUE | 
                                       ind_execucao == TRUE | 
                                       ind_conclusao == TRUE))])),
    n_blocos = first(na.omit(n_blocos[which(
      ind_edificacao_nova == TRUE & (ind_aprovacao == TRUE | 
                                       ind_execucao == TRUE))])),
    n_pavimentos = first(na.omit(n_pavimentos[which(
      ind_edificacao_nova == TRUE & (ind_aprovacao == TRUE | 
                                       ind_execucao == TRUE))])),
    n_unidades = first(na.omit(n_unidades[which(
      ind_edificacao_nova == TRUE & (ind_aprovacao == TRUE | 
                                       ind_execucao == TRUE))])),
    n_unidades_his = first(na.omit(unid_his[which(
      ind_edificacao_nova == TRUE & (ind_aprovacao == TRUE | 
                                       ind_execucao == TRUE))])),
    n_unidades_hmp = first(na.omit(unid_hmp[which(
      ind_edificacao_nova == TRUE & (ind_aprovacao == TRUE | 
                                       ind_execucao == TRUE))])),
    n_unidades_r2h_r2v = first(na.omit(unid_r2h_r2v[which(
      ind_edificacao_nova == TRUE & (ind_aprovacao == TRUE | 
                                       ind_execucao == TRUE))])),
    sql_incra = first(sql_incra),
    sql_incra_composto = first(sql_incra_composto),
    sql_incra_lista = map_chr(list(unique(na.omit(sql_incra))), 
                              ~ paste0(.x, collapse = "; ")),
    n_enderecos = n_distinct(endereco),
    endereco = first(na.omit(endereco)),
    endereco_lista= map_chr(list(unique(na.omit(endereco_raw))), 
                            ~ paste0(.x, collapse = "; ")),
    distrito = first(na.omit(distrito)),
    subprefeitura = first(na.omit(subprefeitura)),
    zona_de_uso_registro = map_chr(list(unique(na.omit(zona_de_uso_registro))), 
                                   ~ paste0(.x, collapse = "; ")),
    ind_edificacao_nova = if_else(any(ind_edificacao_nova == TRUE), TRUE, FALSE),
    ind_aprovacao = if_else(any(ind_aprovacao == TRUE), TRUE, FALSE),
    ind_execucao = if_else(any(ind_execucao == TRUE), TRUE, FALSE),
    ind_r2v = if_else(any(ind_r2v == TRUE), TRUE, FALSE),
    ind_r2h = if_else(any(ind_r2h == TRUE), TRUE, FALSE),
    ind_his = if_else(any(ind_his == TRUE), TRUE, FALSE),
    ind_hmp = if_else(any(ind_hmp == TRUE), TRUE, FALSE),
    ind_ezeis = if_else(any(ind_ezeis == TRUE), TRUE, FALSE),
    ind_uso_misto = if_else(categoria_de_uso_grupo != "Outra" &
                              ind_edificacao_nova == TRUE &
                              any(str_detect(categoria_de_uso_lista,
                                             "NR|C1|C2|C3|S1|S2|S3|E1|E2|E3|E4")), 
                            TRUE, FALSE),
    ind_zeis = if_else(any(str_detect(zona_de_uso_registro,
                                      "ZEIS")),
                       TRUE, FALSE),
    ind_parcelamento = if_else(any(ind_parcelamento == TRUE), TRUE, FALSE),
    geometry = first(geometry)
  ) %>%
  # Somas os valores dos agrupamentos 
  group_by(sql_incra_composto) %>%
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
    diff_dias_projeto = interval(data_autuacao_projeto, 
                                 data_validacao_projeto) / days(1),
    diff_dias_execucao = interval(data_validacao_projeto, 
                                  data_validacao_execucao) / days(1),
    unidade_pmsp = map_chr(list(unique(unidade_pmsp)), ~paste0(.x, collapse = "; ")),
    categoria_de_uso_grupo = if_else(any(categoria_de_uso_grupo == "ERP"), "ERP", "ERM"),
    categoria_de_uso_lista = map_chr(list(unique(na.omit(categoria_de_uso_lista))), 
                                     ~ paste0(.x, collapse = "; ")),
    area_do_terreno = sum(area_do_terreno),
    area_da_construcao = sum(area_da_construcao),
    n_blocos = sum(n_blocos),
    n_pavimentos = sum(n_pavimentos),
    n_unidades = sum(n_unidades),
    n_pavimentos_por_bloco = n_pavimentos/n_blocos,
    n_unidades_por_bloco = n_unidades/n_blocos,
    n_unidades_his = sum(n_unidades_his),
    n_unidades_his_por_bloco = n_unidades_his/n_blocos,
    n_unidades_hmp = sum(n_unidades_hmp),
    n_unidades_hmp_por_bloco = n_unidades_hmp/n_blocos,
    n_unidades_r2h_r2v = sum(n_unidades_r2h_r2v),
    n_unidades_r2h_r2v_por_bloco = n_unidades_r2h_r2v/n_blocos,
    sql_incra = first(sql_incra),
    sql_incra_composto = first(sql_incra_composto),
    sql_incra_lista = map_chr(list(unique(na.omit(sql_incra))), 
                              ~ paste0(.x, collapse = "; ")),
    n_enderecos = sum(n_enderecos),
    endereco = first(na.omit(endereco)),
    endereco_lista= map_chr(list(unique(na.omit(endereco))), 
                            ~ paste0(.x, collapse = "; ")),
    distrito = first(na.omit(distrito)),
    subprefeitura = first(na.omit(subprefeitura)),
    zona_de_uso_registro = map_chr(list(unique(na.omit(zona_de_uso_registro))), 
                                   ~ paste0(.x, collapse = "; ")),
    ind_edificacao_nova = if_else(any(ind_edificacao_nova == TRUE), TRUE, FALSE),
    ind_aprovacao = if_else(any(ind_aprovacao == TRUE), TRUE, FALSE),
    ind_execucao = if_else(any(ind_execucao == TRUE), TRUE, FALSE),
    ind_r2h = if_else(any(ind_r2h == TRUE), TRUE, FALSE),
    ind_r2v = if_else(any(ind_r2v == TRUE), TRUE, FALSE),
    ind_his = if_else(any(ind_his == TRUE), TRUE, FALSE),
    ind_hmp = if_else(any(ind_hmp == TRUE), TRUE, FALSE),
    ind_ezeis = if_else(any(ind_ezeis == TRUE), TRUE, FALSE),
    ind_uso_misto = if_else(categoria_de_uso_grupo != "Outra" &
                              ind_edificacao_nova == TRUE &
                              any(str_detect(categoria_de_uso_lista,
                                             "NR|C1|C2|C3|S1|S2|S3|E1|E2|E3|E4")), 
                            TRUE, FALSE),
    ind_zeis = if_else(any(str_detect(zona_de_uso_registro,
                                      "ZEIS")),
                       TRUE, FALSE),
    ind_parcelamento = if_else(any(ind_parcelamento == TRUE), TRUE, FALSE),
    geometry = first(geometry)
  ) %>%
  ungroup() %>%
  # Ajusta subnotificação de NA
  mutate(across(where(is.numeric), ~ ifelse(.x == 0, NA, .)))

# Combina dois dataframes
amostra_por_emp <-
  bind_rows(amostra_emp, atts_ind_parcelamento)

# parcelamentos
amostra_por_emp %>%
  count(ind_parcelamento)

# SQL's sem alvara de execução não podem ter data de aprovação de execução
amostra_por_emp %>% 
  st_drop_geometry() %>%
  filter(ind_edificacao_nova == TRUE & ind_execucao == FALSE) %>%
  filter(is.na(data_validacao_execucao)) %>%
  count(ind_execucao)

# alvarás relevantes (todos)
amostra_por_emp %>% 
  st_drop_geometry() %>%
  count(ind_edificacao_nova)

# mais de um endereço válido
amostra_por_emp %>% 
  st_drop_geometry() %>%
  filter(ind_edificacao_nova == TRUE) %>%
  filter(n_enderecos > 1) %>%
  count()

# 5. Exporta -------------------------------------------------------------------

# Assegura a existência do diretório para exportação
fs::dir_create(here("2025_AtualizacaoDadosAlvaras", "4.Outputs"))

# Exporta alvarás agrupados
amostra_por_emp <- amostra_por_emp %>%
  filter(ind_aprovacao == TRUE & ind_execucao == TRUE) 

amostra_por_emp <- amostra_por_emp %>%
  st_sf(amostra_por_emp$geometry, crs = 31983) %>%
  subset(select = -c(geometry, sql_incra_composto, sql_incra_lista)) %>% 
  rename(geometry = amostra_por_emp.geometry) %>%
  filter(!(n_unidades <= 5) | is.na(n_unidades))

sfarrow::st_write_parquet(amostra_por_emp,here("2025_AtualizacaoDadosAlvaras", "4.Outputs", 
                                               "geo_alvaras_emp_pde.parquet"))


