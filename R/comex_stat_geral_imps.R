library(arrow)
library(tidyverse)

# Cache global para armazenar arquivos de importação baixados
.import_cache <- new.env()

#' @noRd
get_cached_import_file <- function(arquivo, force_download = FALSE) {
  base_url <- "https://pub-113e4cc603524d8783e99dec0b56b682.r2.dev"

  # Verificar se arquivo já está em cache
  if (exists(arquivo, envir = .import_cache) && !force_download) {
    cat("Usando arquivo de importação em cache:", arquivo, "\n")
    return(get(arquivo, envir = .import_cache))
  }

  # Baixar arquivo
  cat("Baixando arquivo de importação:", arquivo, "\n")
  url <- paste0(base_url, "/", arquivo)

  tryCatch({
    df <- arrow::read_parquet(url)
    cat("Arquivo baixado com sucesso. Linhas:", nrow(df), "\n")

    # Verificar se tem as colunas esperadas de importação
    expected_cols <- c("CO_ANO", "CO_MES", "CO_NCM", "CO_UNID", "CO_PAIS",
                       "SG_UF_NCM", "CO_VIA", "CO_URF", "QT_ESTAT",
                       "KG_LIQUIDO", "VL_FOB", "VL_FRETE", "VL_SEGURO")

    missing_cols <- setdiff(expected_cols, names(df))
    if (length(missing_cols) > 0) {
      warning("Colunas esperadas não encontradas: ", paste(missing_cols, collapse = ", "))
    }

    # Salvar no cache
    assign(arquivo, df, envir = .import_cache)
    cat("Arquivo de importação salvo no cache.\n")

    return(df)
  }, error = function(e) {
    stop("Erro ao baixar arquivo de importação: ", e$message)
  })
}

#' @noRd
clear_import_cache <- function(arquivo = NULL) {
  if (is.null(arquivo)) {
    # Limpar todo o cache
    rm(list = ls(envir = .import_cache), envir = .import_cache)
    cat("Cache de importação completamente limpo.\n")
  } else {
    # Limpar arquivo específico
    if (exists(arquivo, envir = .import_cache)) {
      rm(list = arquivo, envir = .import_cache)
      cat("Arquivo removido do cache de importação:", arquivo, "\n")
    } else {
      cat("Arquivo não encontrado no cache de importação:", arquivo, "\n")
    }
  }
}

#' @noRd
list_cached_import_files <- function() {
  arquivos <- ls(envir = .import_cache)
  if (length(arquivos) == 0) {
    cat("Nenhum arquivo de importação em cache.\n")
  } else {
    cat("Arquivos de importação em cache:\n")
    for (arquivo in arquivos) {
      df <- get(arquivo, envir = .import_cache)
      cat("-", arquivo, "(", nrow(df), "linhas )\n")
    }
  }
  return(arquivos)
}

#' @title Análise de Dados de Importação Comex Stat
#'
#' @description
#' Função principal para analisar dados de importação do Brasil a nível geral,
#' permitindo filtragem por ano, produtos (SH2, SH4, SH6), países, UFs, URFs,
#' vias e unidades. Os dados são baixados e cacheados localmente.
#'
#' @param anos Vetor numérico. Os anos dos dados de importação a serem processados (ex: 2024 ou c(2023, 2024)).
#' @param sh2 Vetor numérico. Códigos SH2 para filtrar.
#' @param sh4 Vetor numérico. Códigos SH4 para filtrar. Se sh6 for usado, sh4 é ignorado.
#' @param sh6 Vetor numérico. Códigos SH6 para filtrar. Se sh6 for usado, sh2 e sh4 são ignorados.
#' @param co_pais Vetor numérico. Códigos de países para filtrar.
#' @param sg_uf_ncm Vetor de caracteres. Siglas das UFs para filtrar (SG_UF_NCM).
#' @param co_urf Vetor numérico. Códigos das URFs para filtrar.
#' @param co_via Vetor numérico. Códigos das vias de transporte para filtrar.
#' @param co_unid Vetor numérico. Códigos das unidades de medida para filtrar.
#' @param agregar_por Vetor de caracteres. Colunas adicionais para agrupar os resultados
#'                    (ex: c("CO_ANO", "CO_PAIS", "CO_MES")).
#' @param metricas Vetor de caracteres. As métricas de importação a serem calculadas e somadas.
#'                 Padrão: c("VL_FOB", "KG_LIQUIDO", "QT_ESTAT", "VL_FRETE", "VL_SEGURO").
#'                 Métricas derivadas como "VL_TOTAL_CIF", "PERC_FRETE" e "PERC_SEGURO"
#'                 serão adicionadas automaticamente se as métricas base estiverem presentes.
#' @param force_download Lógico. Se TRUE, força o download dos dados, ignorando o cache.
#'                       Padrão é FALSE.
#'
#' @return Um data.frame (tibble) com os dados agregados de importação.
#'         Retorna um data.frame vazio se nenhum registro for encontrado após os filtros.
#'
#' @examples
#' \dontrun{
#' # Exemplo 1: Importações básicas por SH6 em 2024
#' # resultado1 <- comex_stat_geral_imps(
#' #   anos = 2024,
#' #   sh6 = c(901110, 901120)
#' # )
#' # print(head(resultado1))
#'
#' # Exemplo 2: Análise multi-anual de importações por país para SH2 = 90
#' # resultado2 <- comex_stat_geral_imps(
#' #   anos = c(2022, 2023, 2024),
#' #   sh2 = 90,
#' #   co_pais = c(249, 589),  # EUA e Uruguai
#' #   agregar_por = c("CO_ANO", "CO_PAIS")
#' # )
#' # print(head(resultado2))
#' }
#' @importFrom arrow read_parquet
#' @importFrom dplyr %>% filter mutate group_by across summarise bind_rows setdiff
#' @importFrom rlang .data !! sym expr
#' @importFrom purrr map reduce
#' @export
comex_stat_geral_imps <- function(
    anos = 2024,                # Vetor de anos (ex: 2024 ou c(2023, 2024))
    sh2 = NULL,                 # Códigos SH2 para filtrar
    sh4 = NULL,                 # Códigos SH4 para filtrar
    sh6 = NULL,                 # Códigos SH6 para filtrar
    co_pais = NULL,             # Países para filtrar
    sg_uf_ncm = NULL,           # UFs para filtrar (SG_UF_NCM)
    co_urf = NULL,              # URFs para filtrar
    co_via = NULL,              # Vias para filtrar
    co_unid = NULL,             # Unidades para filtrar
    agregar_por = NULL,         # Variáveis adicionais para agrupar
    metricas = c("VL_FOB", "KG_LIQUIDO", "QT_ESTAT", "VL_FRETE", "VL_SEGURO"),  # Métricas específicas de importação
    force_download = FALSE      # Forçar novo download mesmo com cache
) {

  # Gerar nomes dos arquivos para cada ano
  arquivos <- paste0("importacao/IMP_", anos, ".parquet")

  cat("Processando IMPORTAÇÕES de", length(anos), "ano(s):", paste(anos, collapse = ", "), "\n")
  cat("Arquivos necessários:", paste(arquivos, collapse = ", "), "\n\n")

  # Baixar e combinar dados de todos os anos
  dados_list <- list()

  for (i in seq_along(arquivos)) {
    arquivo <- arquivos[i]
    ano <- anos[i]

    cat("=== Processando importações do ano", ano, "===\n")
    df_ano <- get_cached_import_file(arquivo, force_download)

    # Adicionar coluna de ano se não existir (backup)
    if (!"CO_ANO" %in% names(df_ano)) {
      df_ano$CO_ANO <- ano
    }

    dados_list[[as.character(ano)]] <- df_ano
  }

  # Combinar todos os anos
  cat("\n=== Combinando dados de importação de todos os anos ===\n")
  df <- bind_rows(dados_list)
  cat("Total de linhas combinadas:", nrow(df), "\n")

  # Criar colunas SH se CO_NCM existe
  if ("CO_NCM" %in% names(df)) {
    df <- df %>%
      mutate(
        CO_NCM_str = sprintf("%08d", CO_NCM),
        sh2 = as.numeric(substr(CO_NCM_str, 1, 2)),
        sh4 = as.numeric(substr(CO_NCM_str, 1, 4)),
        sh6 = as.numeric(substr(CO_NCM_str, 1, 6))
      )
  }

  # Aplicar filtros
  cat("\n=== Aplicando filtros nas importações ===\n")

  # Filtros de produto (hierárquico)
  if (!is.null(sh6)) {
    df <- df %>% filter(sh6 %in% !!sh6)
    agregar_produto <- "sh6"
    cat("Filtrado por SH6:", paste(sh6, collapse = ", "), "\n")
  } else if (!is.null(sh4)) {
    df <- df %>% filter(sh4 %in% !!sh4)
    agregar_produto <- "sh4"
    cat("Filtrado por SH4:", paste(sh4, collapse = ", "), "\n")
  } else if (!is.null(sh2)) {
    df <- df %>% filter(sh2 %in% !!sh2)
    agregar_produto <- "sh2"
    cat("Filtrado por SH2:", paste(sh2, collapse = ", "), "\n")
  } else {
    agregar_produto <- "CO_NCM"
  }

  # Filtro adicional por ano
  anos_filtro <- intersect(anos, unique(df$CO_ANO))
  if (length(anos_filtro) < length(anos)) {
    cat("AVISO: Alguns anos solicitados não foram encontrados nos dados\n")
  }
  df <- df %>% filter(CO_ANO %in% !!anos_filtro)

  # Outros filtros
  if (!is.null(co_pais)) {
    df <- df %>% filter(CO_PAIS %in% !!co_pais)
    cat("Filtrado por países:", paste(co_pais, collapse = ", "), "\n")
  }

  if (!is.null(sg_uf_ncm)) {
    df <- df %>% filter(SG_UF_NCM %in% !!sg_uf_ncm)
    cat("Filtrado por UFs:", paste(sg_uf_ncm, collapse = ", "), "\n")
  }

  if (!is.null(co_urf)) {
    df <- df %>% filter(CO_URF %in% !!co_urf)
    cat("Filtrado por URFs:", paste(co_urf, collapse = ", "), "\n")
  }

  if (!is.null(co_via)) {
    df <- df %>% filter(CO_VIA %in% !!co_via)
    cat("Filtrado por vias:", paste(co_via, collapse = ", "), "\n")
  }

  if (!is.null(co_unid)) {
    df <- df %>% filter(CO_UNID %in% !!co_unid)
    cat("Filtrado por unidades:", paste(co_unid, collapse = ", "), "\n")
  }

  cat("Linhas após filtros:", nrow(df), "\n")

  if (nrow(df) == 0) {
    warning("Nenhum registro encontrado após aplicar os filtros!")
    return(data.frame())
  }

  # Definir variáveis de agrupamento
  group_vars <- agregar_produto

  if (!is.null(agregar_por)) {
    # Verificar se as variáveis existem no dataset
    vars_existem <- agregar_por %in% names(df)
    if (!all(vars_existem)) {
      vars_inexistentes <- agregar_por[!vars_existem]
      warning("Variáveis não encontradas no dataset: ", paste(vars_inexistentes, collapse = ", "))
      agregar_por <- agregar_por[vars_existem]
    }
    group_vars <- c(group_vars, agregar_por)
  }

  # Verificar quais métricas existem no dataset
  metricas_existem <- metricas %in% names(df)
  if (!all(metricas_existem)) {
    metricas_inexistentes <- metricas[!metricas_existem]
    warning("Métricas não encontradas no dataset: ", paste(metricas_inexistentes, collapse = ", "))
    metricas <- metricas[metricas_existem]
  }

  if (length(metricas) == 0) {
    stop("Nenhuma métrica válida encontrada no dataset!")
  }

  # Agregar dados
  cat("\n=== Agregando dados de importação ===\n")
  cat("Agregando por:", paste(group_vars, collapse = ", "), "\n")
  cat("Calculando métricas:", paste(metricas, collapse = ", "), "\n")

  # Criar expressão de summarise dinamicamente
  summarise_exprs <- map(metricas, ~expr(sum(!!sym(.x), na.rm = TRUE)))
  names(summarise_exprs) <- metricas

  df_final <- df %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(!!!summarise_exprs, .groups = "drop")

  # Calcular métricas derivadas úteis para importação
  if (all(c("VL_FOB", "VL_FRETE", "VL_SEGURO") %in% names(df_final))) {
    df_final <- df_final %>%
      mutate(
        VL_TOTAL_CIF = VL_FOB + VL_FRETE + VL_SEGURO,  # Valor total CIF
        PERC_FRETE = round((VL_FRETE / VL_FOB) * 100, 2),  # % Frete sobre FOB
        PERC_SEGURO = round((VL_SEGURO / VL_FOB) * 100, 2)  # % Seguro sobre FOB
      )
    cat("Métricas derivadas adicionadas: VL_TOTAL_CIF, PERC_FRETE, PERC_SEGURO\n")
  }

  cat("Resultado final:", nrow(df_final), "linhas\n")

  return(df_final)
}

