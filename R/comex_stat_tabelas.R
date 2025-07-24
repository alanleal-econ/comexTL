library(readr)
library(dplyr)

# Cache global para armazenar tabelas de referência
.comex_tabelas_cache <- new.env()

# Mapeamento de códigos para nomes de arquivos
TABELAS_DISPONIVEIS <- list(
  "1" = list(nome = "NCM.csv", descricao = "Nomenclatura Comum do Mercosul"),
  "2" = list(nome = "NCM_SH.csv", descricao = "NCM com códigos SH"),
  "3" = list(nome = "PAIS.csv", descricao = "Países"),
  "4" = list(nome = "UF.csv", descricao = "Unidades Federativas"),
  "5" = list(nome = "UF_MUN.csv", descricao = "UF e Municípios")
)

#' @noRd
get_cached_table <- function(codigo_tabela, force_download = FALSE) {
  base_url <- "https://pub-113e4cc603524d8783e99dec0b56b682.r2.dev/tabelas/"

  # Validar código da tabela
  if (!codigo_tabela %in% names(TABELAS_DISPONIVEIS)) {
    cat("Códigos disponíveis:\n")
    for (cod in names(TABELAS_DISPONIVEIS)) {
      cat("-", cod, ":", TABELAS_DISPONIVEIS[[cod]]$descricao, "\n")
    }
    stop("Código de tabela inválido: ", codigo_tabela)
  }

  arquivo <- TABELAS_DISPONIVEIS[[codigo_tabela]]$nome

  # Verificar se tabela já está em cache
  if (exists(arquivo, envir = .comex_tabelas_cache) && !force_download) {
    cat("Usando tabela em cache:", arquivo, "\n")
    return(get(arquivo, envir = .comex_tabelas_cache))
  }

  # Baixar tabela do Cloudflare
  cat("Baixando tabela:", arquivo, "\n")
  url <- paste0(base_url, arquivo)

  tryCatch({
    # Tentar ler como CSV com diferentes configurações
    df <- read.csv(url,
                   encoding = "latin1",
                   sep=";") %>% as_tibble()

    cat("Tabela baixada com sucesso. Linhas:", nrow(df), "Colunas:", ncol(df), "\n")

    # Salvar no cache
    assign(arquivo, df, envir = .comex_tabelas_cache)
    cat("Tabela salva no cache.\n")

    return(df)
  }, error = function(e) {
    # Tentar com encoding diferente se der erro
    tryCatch({
      df <- read.csv(url,
                     encoding = "UTF-8",
                     sep=";") %>% as_tibble()

      cat("Tabela baixada com sucesso (encoding latin1). Linhas:", nrow(df), "Colunas:", ncol(df), "\n")

      # Salvar no cache
      assign(arquivo, df, envir = .comex_tabelas_cache)
      cat("Tabela salva no cache.\n")

      return(df)
    }, error = function(e2) {
      stop("Erro ao baixar tabela: ", e2$message)
    })
  })
}

#' @noRd
clear_tables_cache <- function(codigo_tabela = NULL) {
  if (is.null(codigo_tabela)) {
    # Limpar todo o cache
    rm(list = ls(envir = .comex_tabelas_cache), envir = .comex_tabelas_cache)
    cat("Cache de tabelas completamente limpo.\n")
  } else {
    # Limpar tabela específica
    if (codigo_tabela %in% names(TABELAS_DISPONIVEIS)) {
      arquivo <- TABELAS_DISPONIVEIS[[codigo_tabela]]$nome
      if (exists(arquivo, envir = .comex_tabelas_cache)) {
        rm(list = arquivo, envir = .comex_tabelas_cache)
        cat("Tabela removida do cache:", arquivo, "\n")
      } else {
        cat("Tabela não encontrada no cache:", arquivo, "\n")
      }
    } else {
      cat("Código de tabela inválido:", codigo_tabela, "\n")
    }
  }
}

#' @noRd
list_cached_tables <- function() {
  arquivos <- ls(envir = .comex_tabelas_cache)
  if (length(arquivos) == 0) {
    cat("Nenhuma tabela em cache.\n")
  } else {
    cat("Tabelas em cache:\n")
    for (arquivo in arquivos) {
      df <- get(arquivo, envir = .comex_tabelas_cache)

      # Encontrar o código correspondente
      codigo <- NULL
      for (cod in names(TABELAS_DISPONIVEIS)) {
        if (TABELAS_DISPONIVEIS[[cod]]$nome == arquivo) {
          codigo <- cod
          break
        }
      }

      cat("- [", codigo, "]", arquivo, "(", nrow(df), "linhas,", ncol(df), "colunas )\n")
    }
  }
  return(arquivos)
}

#' @title Carregar Tabelas de Referência do Comex Stat
#'
#' @description
#' Esta função permite baixar e carregar tabelas de referência utilizadas nos dados
#' de comércio exterior (Comex Stat), como NCM, Países, UFs e Municípios.
#' Os dados são cacheados para uso futuro, evitando downloads repetidos.
#'
#' @param codigos_tabelas Vetor de caracteres ou numérico. Um ou mais códigos das tabelas desejadas.
#'                        Códigos disponíveis:
#'                        "1": Nomenclatura Comum do Mercosul (NCM)
#'                        "2": NCM com códigos SH (NCM_SH)
#'                        "3": Países (PAIS)
#'                        "4": Unidades Federativas (UF)
#'                        "5": UF e Municípios (UF_MUN)
#' @param force_download Lógico. Se TRUE, força o novo download da(s) tabela(s),
#'                       ignorando o cache existente. Padrão é FALSE.
#'
#' @return Um data.frame (tibble) se apenas uma tabela for solicitada,
#'         ou uma lista de data.frames (tibbles) se múltiplas tabelas forem solicitadas.
#'         Os nomes na lista seguirão o padrão `tabela_[codigo]_[nome_arquivo_minusculo]`.
#'
#' @examples
#' \dontrun{
#' # Carregar a tabela de NCM
#' ncm_data <- load_reference_tables("1")
#' print(head(ncm_data))
#'
#' # Carregar tabelas de Países e UFs
#' ref_tables <- load_reference_tables(c("3", "4"))
#' print(names(ref_tables))
#' print(head(ref_tables$tabela_3_pais))
#' }
#' @importFrom readr read_csv
#' @importFrom dplyr %>% as_tibble
#' @export
load_reference_tables <- function(
    codigos_tabelas,           # Vetor de códigos das tabelas (ex: c("1", "3"))
    force_download = FALSE     # Forçar novo download mesmo com cache
) {

  # Garantir que códigos_tabelas seja um vetor de caracteres
  codigos_tabelas <- as.character(codigos_tabelas)

  # Validar códigos
  codigos_invalidos <- codigos_tabelas[!codigos_tabelas %in% names(TABELAS_DISPONIVEIS)]
  if (length(codigos_invalidos) > 0) {
    cat("Códigos inválidos:", paste(codigos_invalidos, collapse = ", "), "\n")
    cat("Códigos disponíveis:\n")
    for (cod in names(TABELAS_DISPONIVEIS)) {
      cat("-", cod, ":", TABELAS_DISPONIVEIS[[cod]]$descricao, "\n")
    }
    stop("Códigos de tabela inválidos encontrados!")
  }

  cat("Carregando", length(codigos_tabelas), "tabela(s):", paste(codigos_tabelas, collapse = ", "), "\n\n")

  # Carregar cada tabela
  tabelas <- list()

  for (codigo in codigos_tabelas) {
    cat("=== Processando tabela", codigo, "===\n")

    tabela_info <- TABELAS_DISPONIVEIS[[codigo]]
    cat("Descrição:", tabela_info$descricao, "\n")

    df <- get_cached_table(codigo, force_download)

    # Mostrar preview da tabela
    cat("Colunas disponíveis:", paste(names(df), collapse = ", "), "\n")
    if (nrow(df) > 0) {
      cat("Primeiras linhas:\n")
      print(head(df, 3))
    }
    cat("\n")

    # Adicionar à lista com nome descritivo
    nome_tabela <- paste0("tabela_", codigo, "_", gsub("\\.csv$", "", tolower(tabela_info$nome)))
    tabelas[[nome_tabela]] <- df
  }

  # Se for apenas uma tabela, retornar diretamente o data.frame
  if (length(tabelas) == 1) {
    cat("Retornando tabela única.\n")
    return(tabelas[[1]])
  } else {
    cat("Retornando lista com", length(tabelas), "tabelas.\n")
    cat("Nomes das tabelas na lista:", paste(names(tabelas), collapse = ", "), "\n")
    return(tabelas)
  }
}

#' @title Mostrar Tabelas de Referência Disponíveis
#'
#' @description
#' Exibe uma lista das tabelas de referência de Comércio Exterior que podem ser
#' carregadas usando a função `load_reference_tables()`, incluindo seus códigos
#' e descrições.
#'
#' @return Imprime no console as informações das tabelas disponíveis. Não retorna nenhum objeto.
#'
#' @examples
#' \dontrun{
#' show_available_tables()
#' }
#' @export
show_available_tables <- function() {
  cat("=== TABELAS DE REFERÊNCIA DISPONÍVEIS ===\n\n")
  for (codigo in names(TABELAS_DISPONIVEIS)) {
    info <- TABELAS_DISPONIVEIS[[codigo]]
    cat("Código:", codigo, "\n")
    cat("Arquivo:", info$nome, "\n")
    cat("Descrição:", info$descricao, "\n")
    cat("Uso: load_reference_tables(\"", codigo, "\")\n\n", sep = "")
  }
}

#' @title Buscar Termo em Tabela de Referência
#'
#' @description
#' Realiza uma busca por um termo específico dentro de uma tabela de referência
#' do Comex Stat, permitindo filtrar por uma coluna específica ou em todas
#' as colunas de texto da tabela.
#'
#' @param codigo_tabela Caractere. O código da tabela de referência onde a busca será feita.
#'                      (Ex: "1" para NCM, "3" para Países).
#' @param termo_busca Caractere. O termo a ser buscado. A busca não diferencia maiúsculas de minúsculas.
#' @param coluna Caractere opcional. O nome da coluna específica onde a busca será realizada.
#'               Se NULL, a busca será feita em todas as colunas de texto da tabela.
#'               Padrão é NULL.
#'
#' @return Um data.frame (tibble) contendo os registros que correspondem ao termo de busca.
#'         Retorna um data.frame vazio se nenhum registro for encontrado.
#'
#' @examples
#' \dontrun{
#' # Buscar "café" na tabela NCM
#' cafe_ncm <- search_in_table("1", "café")
#' print(head(cafe_ncm))
#'
#' # Buscar países que contêm "Unidos" na coluna de nome do país
#' paises_unidos <- search_in_table("3", "Unidos", coluna = "NO_PAIS")
#' print(paises_unidos)
#' }
#' @importFrom dplyr filter %>%
#' @importFrom purrr map reduce
#' @importFrom rlang !! sym expr
#' @export
search_in_table <- function(codigo_tabela, termo_busca, coluna = NULL) {

  # Carregar tabela
  df <- get_cached_table(codigo_tabela)

  if (is.null(coluna)) {
    # Buscar em todas as colunas de texto
    colunas_texto <- names(df)[sapply(df, function(x) is.character(x) || is.factor(x))]

    if (length(colunas_texto) == 0) {
      cat("Nenhuma coluna de texto encontrada na tabela.\n")
      return(data.frame())
    }

    # Criar condição de busca para múltiplas colunas
    condicoes <- map(colunas_texto, ~expr(grepl(!!termo_busca, !!sym(.x), ignore.case = TRUE)))
    condicao_final <- reduce(condicoes, ~expr(!!.x | !!.y))

    resultado <- df %>%
      filter(!!condicao_final)

  } else {
    # Buscar em coluna específica
    if (!coluna %in% names(df)) {
      stop("Coluna '", coluna, "' não encontrada. Colunas disponíveis: ", paste(names(df), collapse = ", "))
    }

    resultado <- df %>%
      filter(grepl(!!termo_busca, !!sym(coluna), ignore.case = TRUE))
  }

  cat("Encontrados", nrow(resultado), "registros para o termo '", termo_busca, "'\n")
  return(resultado)
}
