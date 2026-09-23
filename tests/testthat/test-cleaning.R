test_that("cleaning parses permit dates in an isolated session", {
  raw_permit <- data.frame(
    n_documento = "1",
    assunto = "ALVARA DE APROVACAO E EXECUCAO DE EDIFICACAO NOVA",
    unid_aprov = "APROV",
    sql = "00100200304",
    zona_uso = "Z1",
    uso_subcat = "R2V",
    area_terreno = 100,
    ac_total = 200,
    blocos = 1,
    pavimentos = 2,
    unid_resid = 10,
    data_emissao = "2026-01-02",
    ano_emissao = 2026,
    coord_x = 330000,
    coord_y = 7390000,
    endereco = "R EXEMPLO,100",
    distrito = "CENTRO",
    subprefeitura = "SE",
    data_autuacao = "2026-01-01",
    processo = "1",
    unid_his = 0,
    unid_hmp = 0,
    unid_r2h_r2v = 10,
    georref_nivel = "lote",
    georref_modo = "municipal"
  )

  cleaned <- clean_alvaras(raw_permit)

  expect_equal(nrow(cleaned), 1)
  expect_s3_class(cleaned$data_aprovacao, "Date")
})
