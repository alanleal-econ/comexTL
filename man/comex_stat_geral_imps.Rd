% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/comex_stat_geral_imps.R
\name{comex_stat_geral_imps}
\alias{comex_stat_geral_imps}
\title{Análise de Dados de Importação Comex Stat}
\usage{
comex_stat_geral_imps(
  anos = 2024,
  sh2 = NULL,
  sh4 = NULL,
  sh6 = NULL,
  co_pais = NULL,
  sg_uf_ncm = NULL,
  co_urf = NULL,
  co_via = NULL,
  co_unid = NULL,
  agregar_por = NULL,
  metricas = c("VL_FOB", "KG_LIQUIDO", "QT_ESTAT", "VL_FRETE", "VL_SEGURO"),
  force_download = FALSE
)
}
\arguments{
\item{anos}{Vetor numérico. Os anos dos dados de importação a serem processados (ex: 2024 ou c(2023, 2024)).}

\item{sh2}{Vetor numérico. Códigos SH2 para filtrar.}

\item{sh4}{Vetor numérico. Códigos SH4 para filtrar. Se sh6 for usado, sh4 é ignorado.}

\item{sh6}{Vetor numérico. Códigos SH6 para filtrar. Se sh6 for usado, sh2 e sh4 são ignorados.}

\item{co_pais}{Vetor numérico. Códigos de países para filtrar.}

\item{sg_uf_ncm}{Vetor de caracteres. Siglas das UFs para filtrar (SG_UF_NCM).}

\item{co_urf}{Vetor numérico. Códigos das URFs para filtrar.}

\item{co_via}{Vetor numérico. Códigos das vias de transporte para filtrar.}

\item{co_unid}{Vetor numérico. Códigos das unidades de medida para filtrar.}

\item{agregar_por}{Vetor de caracteres. Colunas adicionais para agrupar os resultados
(ex: c("CO_ANO", "CO_PAIS", "CO_MES")).}

\item{metricas}{Vetor de caracteres. As métricas de importação a serem calculadas e somadas.
Padrão: c("VL_FOB", "KG_LIQUIDO", "QT_ESTAT", "VL_FRETE", "VL_SEGURO").
Métricas derivadas como "VL_TOTAL_CIF", "PERC_FRETE" e "PERC_SEGURO"
serão adicionadas automaticamente se as métricas base estiverem presentes.}

\item{force_download}{Lógico. Se TRUE, força o download dos dados, ignorando o cache.
Padrão é FALSE.}
}
\value{
Um data.frame (tibble) com os dados agregados de importação.
Retorna um data.frame vazio se nenhum registro for encontrado após os filtros.
}
\description{
Função principal para analisar dados de importação do Brasil a nível geral,
permitindo filtragem por ano, produtos (SH2, SH4, SH6), países, UFs, URFs,
vias e unidades. Os dados são baixados e cacheados localmente.
}
\examples{
\dontrun{
# Exemplo 1: Importações básicas por SH6 em 2024
# resultado1 <- comex_stat_geral_imps(
#   anos = 2024,
#   sh6 = c(901110, 901120)
# )
# print(head(resultado1))

# Exemplo 2: Análise multi-anual de importações por país para SH2 = 90
# resultado2 <- comex_stat_geral_imps(
#   anos = c(2022, 2023, 2024),
#   sh2 = 90,
#   co_pais = c(249, 589),  # EUA e Uruguai
#   agregar_por = c("CO_ANO", "CO_PAIS")
# )
# print(head(resultado2))
}
}
