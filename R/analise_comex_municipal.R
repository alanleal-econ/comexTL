library(arrow)
library(tidyverse)

# Cache global para armazenar arquivos municipais baixados
.municipal_cache <- new.env()

#' @noRd
get_cached_municipal_file <- function(arquivo, force_download = FALSE) {
  base_url <- "https://pub-113e4cc603524d8783e99dec0b56b682.r2.dev"

  # Verificar se arquivo já está em cache
  if (exists(arquivo, envir = .municipal_cache) && !force_download) {
    cat("Usando arquivo municipal em cache:", arquivo, "\n")
    return(get(arquivo, envir = .municipal_cache))
  }

  # Baixar arquivo
  cat("Baixando arquivo municipal:", arquivo, "\n")
  url <- paste0(base_url, "/", arquivo)

  tryCatch({
    df <- read_parquet(url)
    cat("Arquivo baixado com sucesso. Linhas:", nrow(df), "\n")

    # Verificar se tem as colunas esperadas municipais
    expected_cols <- c("CO_ANO", "CO_MES", "SH4", "CO_PAIS",
                       "SG_UF_MUN", "CO_MUN", "KG_LIQUIDO", "VL_FOB")

    missing_cols <- setdiff(expected_cols, names(df))
    if (length(missing_cols) > 0) {
      warning("Colunas esperadas não encontradas: ", paste(missing_cols, collapse = ", "))
    }

    # Salvar no cache
    assign(arquivo, df, envir = .municipal_cache)
    cat("Arquivo municipal salvo no cache.\n")

    return(df)
  }, error = function(e) {
    stop("Erro ao baixar arquivo municipal: ", e$message)
  })
}

#' @noRd
clear_municipal_cache <- function(arquivo = NULL) {
  if (is.null(arquivo)) {
    # Limpar todo o cache
    rm(list = ls(envir = .municipal_cache), envir = .municipal_cache)
    cat("Cache municipal completamente limpo.\n")
  } else {
    # Limpar arquivo específico
    if (exists(arquivo, envir = .municipal_cache)) {
      rm(list = arquivo, envir = .municipal_cache)
      cat("Arquivo removido do cache municipal:", arquivo, "\n")
    } else {
      cat("Arquivo não encontrado no cache municipal:", arquivo, "\n")
    }
  }
}

#' @noRd
list_cached_municipal_files <- function() {
  arquivos <- ls(envir = .municipal_cache)
  if (length(arquivos) == 0) {
    cat("Nenhum arquivo municipal em cache.\n")
  } else {
    cat("Arquivos municipais em cache:\n")
    for (arquivo in arquivos) {
      df <- get(arquivo, envir = .municipal_cache)
      cat("-", arquivo, "(", nrow(df), "linhas )\n")
    }
  }
  return(arquivos)
}

#' @title Análise de Dados de Comércio Exterior Municipal
#'
#' @description
#' Esta função permite analisar dados de comércio exterior (exportação ou importação)
#' a nível municipal (município da empresa), filtrando por ano, produtos (SH2, SH4),
#' países, UFs da empresa e municípios da empresa. Os dados são baixados e cacheados localmente.
#'
#' @param tipo Caractere. O tipo de operação comercial: "exportacao" ou "importacao".
#' @param anos Vetor numérico. Os anos dos dados a serem processados (ex: 2024 ou c(2023, 2024)).
#' @param sh2 Vetor numérico. Códigos SH2 para filtrar (derivado de SH4 nos dados municipais).
#' @param sh4 Vetor numérico. Códigos SH4 para filtrar.
#' @param co_pais Vetor numérico. Códigos de países para filtrar.
#' @param sg_uf_mun Vetor de caracteres. Siglas das UFs da empresa para filtrar (SG_UF_MUN).
#' @param co_mun Vetor numérico. Códigos dos municípios da empresa para filtrar.
#' @param agregar_por Vetor de caracteres. Colunas adicionais para agrupar os resultados
#'                    (ex: c("CO_ANO", "SG_UF_MUN", "CO_MES")).
#' @param metricas Vetor de caracteres. As métricas a serem calculadas e somadas.
#'                 Métricas disponíveis para dados municipais: "VL_FOB" e "KG_LIQUIDO".
#'                 Outras métricas serão ignoradas com um aviso.
#' @param force_download Lógico. Se TRUE, força o download dos dados, ignorando o cache.
#'                       Padrão é FALSE.
#'
#' @return Um data.frame (tibble) com os dados agregados de comércio exterior municipal.
#'         Inclui a coluna `N_MUNICIPIOS` se a agregação não for por `CO_MUN`.
#'         Retorna um data.frame vazio se nenhum registro for encontrado após os filtros.
#'
#' @examples
#' \dontrun{
#' # Exemplo 1: Exportações municipais básicas por SH4 em 2024, agrupado por UF
#' # resultado1 <- analise_comex_municipal(
#' #   tipo = "exportacao",
#' #   anos = 2024,
#' #   sh4 = c(9011, 9012),
#' #   agregar_por = "SG_UF_MUN"
#' # )
#' # print(head(resultado1))
#'
#' # Exemplo 2: Análise multi-anual de importações por município para SH2 = 85
#' # imp_mun_maquinas <- analise_comex_municipal(
#' #   tipo = "importacao",
#' #   anos = c(2023, 2024),
#' #   sh2 = 85,
#' #   co_mun = c(3550308), # Exemplo: São Paulo
#' #   agregar_por = c("CO_ANO", "CO_MES")
#' # )
#' # print(head(imp_mun_maquinas))
#' }
#' @importFrom arrow read_parquet
#' @importFrom dplyr %>% filter mutate group_by across summarise bind_rows n_distinct left_join setdiff
#' @importFrom rlang .data !! sym expr
#' @importFrom purrr map
#' @export
analise_comex_municipal <- function(
    tipo = "exportacao",         # "exportacao" ou "importacao"
    anos = 2024,                # Vetor de anos (ex: 2024 ou c(2023, 2024))
    sh2 = NULL,                 # Códigos SH2 para filtrar (criados a partir de SH4)
    sh4 = NULL,                 # Códigos SH4 para filtrar
    co_pais = NULL,             # Países para filtrar
    sg_uf_mun = NULL,           # UFs da empresa para filtrar (SG_UF_MUN)
    co_mun = NULL,              # Municípios da empresa para filtrar
    agregar_por = NULL,         # Variáveis adicionais para agrupar
    metricas = c("VL_FOB", "KG_LIQUIDO"),  # Métricas municipais disponíveis
    force_download = FALSE      # Forçar novo download mesmo com cache
) {

  # Validar tipo
  if (!tipo %in% c("exportacao", "importacao")) {
    stop("Tipo deve ser 'exportacao' ou 'importacao'")
  }

  # Gerar nomes dos arquivos para cada ano
  # Formato: municipio/exportacao/EXP_YYYY_MUN.parquet ou municipio/importacao/IMP_YYYY_MUN.parquet
  tipo_sigla <- if(tipo == "exportacao") "EXP" else "IMP"
  arquivos <- paste0("municipio/", tipo, "/", tipo_sigla, "_", anos, "_MUN.parquet")

  cat("Processando dados MUNICIPAIS de", toupper(tipo), "de", length(anos), "ano(s):", paste(anos, collapse = ", "), "\n")
  cat("Arquivos necessários:", paste(arquivos, collapse = ", "), "\n\n")

  # Baixar e combinar dados de todos os anos
  dados_list <- list()

  for (i in seq_along(arquivos)) {
    arquivo <- arquivos[i]
    ano <- anos[i]

    cat("=== Processando", tipo, "municipal do ano", ano, "===\n")
    df_ano <- get_cached_municipal_file(arquivo, force_download)

    # Adicionar coluna de ano se não existir (backup)
    if (!"CO_ANO" %in% names(df_ano)) {
      df_ano$CO_ANO <- ano
    }

    dados_list[[as.character(ano)]] <- df_ano
  }

  # Combinar todos os anos
  cat("\n=== Combinando dados municipais de", tipo, "de todos os anos ===\n")
  df <- bind_rows(dados_list)
  cat("Total de linhas combinadas:", nrow(df), "\n")

  # Criar coluna SH2 a partir de SH4 (nos dados municipais só temos até SH4)
  if ("SH4" %in% names(df)) {
    df <- df %>%
      mutate(
        SH4_str = sprintf("%04d", SH4),
        sh2 = as.numeric(substr(SH4_str, 1, 2))
      )
  }

  # Aplicar filtros
  cat("\n=== Aplicando filtros nos dados municipais ===\n")

  # Filtros de produto (só temos SH4 e SH2 derivado)
  if (!is.null(sh4)) {
    df <- df %>% filter(SH4 %in% !!sh4)
    agregar_produto <- "SH4"
    cat("Filtrado por SH4:", paste(sh4, collapse = ", "), "\n")
  } else if (!is.null(sh2)) {
    df <- df %>% filter(sh2 %in% !!sh2)
    agregar_produto <- "sh2"
    cat("Filtrado por SH2 (derivado de SH4):", paste(sh2, collapse = ", "), "\n")
  } else {
    agregar_produto <- "SH4"
    cat("Sem filtro de produto - mantendo todos os SH4\n")
  }

  # Filtro adicional por ano
  anos_filtro <- intersect(anos, unique(df$CO_ANO))
  if (length(anos_filtro) < length(anos)) {
    cat("AVISO: Alguns anos solicitados não foram encontrados nos dados\n")
  }
  df <- df %>% filter(CO_ANO %in% !!anos_filtro)

  # Outros filtros específicos municipais
  if (!is.null(co_pais)) {
    df <- df %>% filter(CO_PAIS %in% !!co_pais)
    cat("Filtrado por países:", paste(co_pais, collapse = ", "), "\n")
  }

  if (!is.null(sg_uf_mun)) {
    df <- df %>% filter(SG_UF_MUN %in% !!sg_uf_mun)
    cat("Filtrado por UFs das empresas:", paste(sg_uf_mun, collapse = ", "), "\n")
  }

  if (!is.null(co_mun)) {
    df <- df %>% filter(CO_MUN %in% !!co_mun)
    cat("Filtrado por municípios das empresas:", paste(co_mun, collapse = ", "), "\n")
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

  # Verificar quais métricas existem no dataset (só VL_FOB e KG_LIQUIDO disponíveis)
  metricas_municipais <- c("VL_FOB", "KG_LIQUIDO")
  metricas_validas <- intersect(metricas, metricas_municipais)

  if (length(setdiff(metricas, metricas_municipais)) > 0) {
    metricas_invalidas <- setdiff(metricas, metricas_municipais)
    warning("Métricas não disponíveis nos dados municipais: ", paste(metricas_invalidas, collapse = ", "))
    cat("Métricas disponíveis nos dados municipais: VL_FOB, KG_LIQUIDO\n")
  }

  if (length(metricas_validas) == 0) {
    stop("Nenhuma métrica válida para dados municipais!")
  }

  metricas <- metricas_validas

  # Agregar dados
  cat("\n=== Agregando dados municipais ===\n")
  cat("Agregando por:", paste(group_vars, collapse = ", "), "\n")
  cat("Calculando métricas:", paste(metricas, collapse = ", "), "\n")

  # Criar expressão de summarise dinamicamente
  summarise_exprs <- map(metricas, ~expr(sum(!!sym(.x), na.rm = TRUE)))
  names(summarise_exprs) <- metricas

  df_final <- df %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(!!!summarise_exprs, .groups = "drop")

  # Adicionar contagem de municípios únicos se agregando por algo diferente de município
  if (!"CO_MUN" %in% group_vars && "CO_MUN" %in% names(df)) {
    # Contar municípios únicos por grupo
    municipios_count <- df %>%
      group_by(across(all_of(group_vars))) %>%
      summarise(N_MUNICIPIOS = n_distinct(CO_MUN), .groups = "drop")

    df_final <- df_final %>%
      left_join(municipios_count, by = group_vars)

    cat("Adicionada contagem de municípios únicos por grupo (N_MUNICIPIOS)\n")
  }

  cat("Resultado final:", nrow(df_final), "linhas\n")

  return(df_final)
}
