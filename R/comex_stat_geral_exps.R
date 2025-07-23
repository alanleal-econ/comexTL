library(arrow)
library(tidyverse)

# Cache global para armazenar arquivos baixados
.comexstat_cache <- new.env()

#' @noRd
get_cached_file <- function(arquivo, force_download = FALSE) {
  base_url <- "https://pub-113e4cc603524d8783e99dec0b56b682.r2.dev"

  # Verificar se arquivo já está em cache
  if (exists(arquivo, envir = .comexstat_cache) && !force_download) {
    cat("Usando arquivo em cache:", arquivo, "\n")
    return(get(arquivo, envir = .comexstat_cache))
  }

  # Baixar arquivo
  cat("Baixando arquivo:", arquivo, "\n")
  url <- paste0(base_url, "/", arquivo)

  tryCatch({
    df <- arrow::read_parquet(url)
    cat("Arquivo baixado com sucesso. Linhas:", nrow(df), "\n")

    # Salvar no cache
    assign(arquivo, df, envir = .comexstat_cache)
    cat("Arquivo salvo no cache.\n")

    return(df)
  }, error = function(e) {
    stop("Erro ao baixar arquivo: ", e$message)
  })
}

#' @noRd
clear_cache <- function(arquivo = NULL) {
  if (is.null(arquivo)) {
    # Limpar todo o cache
    rm(list = ls(envir = .comexstat_cache), envir = .comexstat_cache)
    cat("Cache completamente limpo.\n")
  } else {
    # Limpar arquivo específico
    if (exists(arquivo, envir = .comexstat_cache)) {
      rm(list = arquivo, envir = .comexstat_cache)
      cat("Arquivo removido do cache:", arquivo, "\n")
    } else {
      cat("Arquivo não encontrado no cache:", arquivo, "\n")
    }
  }
}

#' @noRd
list_cached_files <- function() {
  arquivos <- ls(envir = .comexstat_cache)
  if (length(arquivos) == 0) {
    cat("Nenhum arquivo em cache.\n")
  } else {
    cat("Arquivos em cache:\n")
    for (arquivo in arquivos) {
      df <- get(arquivo, envir = .comexstat_cache)
      cat("-", arquivo, "(", nrow(df), "linhas )\n")
    }
  }
  return(arquivos)
}

#' @title Análise de Dados de Exportação Comex Stat
#'
#' @description
#' Função principal para analisar dados de exportação do Brasil a nível geral,
#' permitindo filtragem por ano, produtos (SH2, SH4, SH6), países, UFs, URFs,
#' vias e unidades. Os dados são baixados e cacheados localmente.
#'
#' @param tipo Caractere. O tipo de operação comercial: "exportacao" (o padrão para esta função).
#'                    (Nota: Embora a função original possa aceitar "importacao", a função `comex_stat_geral_imps`
#'                    é mais específica para importações, sendo recomendado usar 'exportacao' aqui).
#' @param anos Vetor numérico. Os anos dos dados de exportação a serem processados (ex: 2024 ou c(2023, 2024)).
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
#' @param metricas Vetor de caracteres. As métricas de exportação a serem calculadas e somadas.
#'                 Padrão: c("VL_FOB", "KG_LIQUIDO", "QT_ESTAT").
#' @param force_download Lógico. Se TRUE, força o download dos dados, ignorando o cache.
#'                       Padrão é FALSE.
#'
#' @return Um data.frame (tibble) com os dados agregados de exportação.
#'         Retorna um data.frame vazio se nenhum registro for encontrado após os filtros.
#'
#' @examples
#' \dontrun{
#' # Exemplo 1: Exportações de Soja (SH2 = 12) do ano de 2024, agrupado por UF
#' # exp_soja_2024_uf <- comex_stat_geral_exps(
#' #   tipo = "exportacao",
#' #   anos = 2024,
#' #   sh2 = 12,
#' #   agregar_por = c("CO_ANO", "SG_UF_NCM")
#' # )
#' # print(head(exp_soja_2024_uf))
#'
#' # Exemplo 2: Análise multi-anual de exportações para SH2 = 90
#' # resultado2 <- comex_stat_geral_exps(
#' #   tipo = "exportacao",
#' #   anos = c(2022, 2023, 2024),
#' #   sh2 = 90,
#' #   agregar_por = c("CO_ANO", "CO_PAIS")
#' # )
#' # print(head(resultado2))
#' }
#' @importFrom arrow read_parquet
#' @importFrom dplyr %>% filter mutate group_by across summarise bind_rows setdiff
#' @importFrom rlang .data !! sym expr
#' @importFrom purrr map reduce
#' @export
comex_stat_geral_exps <- function(
    tipo = "exportacao",         # "exportacao" ou "importacao"
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
    metricas = c("VL_FOB", "KG_LIQUIDO", "QT_ESTAT"),  # Métricas a calcular
    force_download = FALSE      # Forçar novo download mesmo com cache
) {

  # Validar tipo
  if (!tipo %in% c("exportacao", "importacao")) {
    stop("Tipo deve ser 'exportacao' ou 'importacao'")
  }

  # Gerar nomes dos arquivos para cada ano
  arquivos <- paste0(tipo, "/", toupper(substr(tipo, 1, 3)), "_", anos, ".parquet")

  cat("Processando", length(anos), "ano(s):", paste(anos, collapse = ", "), "\n")
  cat("Arquivos necessários:", paste(arquivos, collapse = ", "), "\n\n")

  # Baixar e combinar dados de todos os anos
  dados_list <- list()

  for (i in seq_along(arquivos)) {
    arquivo <- arquivos[i]
    ano <- anos[i]

    cat("=== Processando ano", ano, "===\n")
    df_ano <- get_cached_file(arquivo, force_download)

    # Adicionar coluna de ano se não existir (backup)
    if (!"CO_ANO" %in% names(df_ano)) {
      df_ano$CO_ANO <- ano
    }

    dados_list[[as.character(ano)]] <- df_ano
  }

  # Combinar todos os anos
  cat("\n=== Combinando dados de todos os anos ===\n")
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
  cat("\n=== Aplicando filtros ===\n")

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

  # Filtro adicional por ano (se o usuário quiser filtrar anos específicos dentro dos baixados)
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
  cat("\n=== Agregando dados ===\n")
  cat("Agregando por:", paste(group_vars, collapse = ", "), "\n")
  cat("Calculando métricas:", paste(metricas, collapse = ", "), "\n")

  # Criar expressão de summarise dinamicamente
  summarise_exprs <- map(metricas, ~expr(sum(!!sym(.x), na.rm = TRUE)))
  names(summarise_exprs) <- metricas

  df_final <- df %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(!!!summarise_exprs, .groups = "drop")

  cat("Resultado final:", nrow(df_final), "linhas\n")

  return(df_final)
}
