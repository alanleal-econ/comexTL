<!-- Substitua `URL_DO_SEU_LOGO` pelo link onde você hospedar o logo em tons de laranja -->

<p align="center">
  <img src="https://www2.thetalab.com.br/wp-content/uploads/2025/07/comextl_logo.png" alt="comexTL logo" width="200"/>
</p>

# comexTL

**Dados do Comércio Exterior disponíveis diretamente no R**

`comexTL` é um pacote em R desenvolvido por Alan Leal que facilita o acesso, download e análise de dados de comércio exterior do Brasil diretamente no ambiente R.

---

## Recursos Principais

* **Importação de dados**: Funções para buscar dados de exportação e importação por país, estado, município e produto.
* **Filtros avançados**: Filtragem por período, categoria de produto (SH4, SH6), país de destino/origem, e nível geográfico.
* **Limpeza e transformação**: Funções utilitárias para tratar e formatar os dados, tornando-os prontos para análise.
* **Documentação completa**: Ajuda integrada via `?nome_da_funcao` e exemplos práticos.

---

## Instalação

Você pode instalar a versão mais recente diretamente do GitHub:

```r
# Instale o pacote remotes se ainda não tiver
install.packages("remotes")

# Instale comexTL do GitHub
remotes::install_github("alanleal-econ/comexTL")
```

---

## Uso Básico

```r
library(comexTL)

# Obter dados de exportação por município para o ano de 2023
exp_mun_2023 <- get_exportacao_municipio(ano = 2023)

# Visualizar as primeiras linhas
head(exp_mun_2023)

# Filtrar por código SH6 = "100610"
exp_prod_100610 <- get_comex_por_produto(codigo_sh = "100610", nivel = 6)

# Resumo estatístico das exportações
summary(exp_prod_100610$valor_fob)
```

---

Criado por Alan Leal/Theta Lab (E-mail:leal@thetalab.com.br)
