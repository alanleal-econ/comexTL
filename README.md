# comexTL

**Dados do Comércio Exterior Brasileiro no R**

[![GitHub](https://img.shields.io/badge/GitHub-Repository-blue?logo=github)](https://github.com/alanleal-econ/comexTL)
[![R](https://img.shields.io/badge/R-4.0%2B-blue?logo=r)](https://www.r-project.org/)
[![License](https://img.shields.io/badge/License-MIT-green)](LICENSE)

**Dados Atualizados em 07/08/2026*

`comexTL` é um pacote R avançado que facilita o acesso, análise e manipulação dos dados oficiais de comércio exterior brasileiro do ComexStat (MDIC). Com uma arquitetura moderna baseada em Apache Arrow e sistema de cache inteligente, o pacote oferece interface unificada para análises desde o nível geral (NCM detalhado) até municipal (SH4).

---

## 🚀 Características Principais

### **📊 Múltiplos Níveis de Análise**
- **Dados Gerais**: Exportações e importações detalhadas por NCM (8 dígitos)
- **Dados Municipais**: Comércio exterior por município da empresa (SH4)
- **Tabelas de Referência**: NCM, países, UFs, municípios com busca integrada

### **⚡ Performance Otimizada**
- **Arquivos Parquet**: Processamento rápido de grandes volumes via Apache Arrow
- **Sistema de Cache**: Armazenamento local inteligente para consultas repetidas
- **Downloads Paralelos**: Múltiplos anos processados simultaneamente

### **🔍 Filtragem Avançada**
- **Hierarquia de Produtos**: SH2 → SH4 → SH6 → NCM (8 dígitos)
- **Dimensão Geográfica**: País, UF, município, URF
- **Dimensão Temporal**: Multi-anual com agregações mensais
- **Logística**: Vias de transporte, unidades de medida

### **📈 Métricas Especializadas**
- **Exportações**: Valor FOB, peso líquido, quantidade estatística
- **Importações**: FOB + frete + seguro, com cálculo automático de CIF
- **Derivadas**: Percentuais de frete/seguro, contagem de municípios

---

## 📦 Instalação

### Instalação via GitHub
```r
# Instalar devtools se necessário
if (!require(devtools)) {
    install.packages("devtools")
}

# Instalar comexTL
devtools::install_github("alanleal-econ/comexTL")

# Carregar o pacote
library(comexTL)
```

### Dependências
O pacote requer:
- `arrow` - Processamento de arquivos Parquet
- `dplyr` - Manipulação de dados
- `readr` - Leitura de CSV
- `purrr` - Programação funcional
- `rlang` - Metaprogramação

---

## 📋 Início Rápido

### 1. Explorar Tabelas de Referência
```r
# Ver tabelas disponíveis
show_available_tables()

# Carregar tabela de países
paises <- load_reference_tables("3")
head(paises)

# Buscar códigos NCM para soja
soja_ncm <- search_in_table("1", "soja")
print(soja_ncm)
```

### 2. Análise de Exportações
```r
# Exportações de soja (SH2=12) em 2024 por UF
exp_soja_2024 <- comex_stat_geral_exps(
    anos = 2024,
    sh2 = 12,
    agregar_por = c("SG_UF_NCM")
)
head(exp_soja_2024)

# Análise multi-anual para China e EUA
exp_china_eua <- comex_stat_geral_exps(
  anos = c(2023, 2024),
  sh6 = 120190,  # Soja específica
  co_pais = c(160, 249),  # China e EUA
  agregar_por = c("CO_ANO", "CO_PAIS")
)
```

### 3. Análise de Importações
```r
# Importações com análise de custos
imp_maquinas <- comex_stat_geral_imps(
    anos = 2024,
    sh2 = 85,  # Máquinas elétricas
    co_pais = c(160, 249),  # China e EUA
    agregar_por = c("CO_PAIS"),
    metricas = c("VL_FOB", "VL_FRETE", "VL_SEGURO")
)

# Verificar métricas derivadas automáticas
names(imp_maquinas)  # Inclui VL_TOTAL_CIF, PERC_FRETE, PERC_SEGURO
```

### 4. Análise Municipal
```r
# Exportações municipais por UF
exp_municipal <- analise_comex_municipal(
    tipo = "exportacao",
    anos = 2024,
    sh2 = 12,  # Soja
    sg_uf_mun = c("MT", "RS", "GO"),
    agregar_por = c("SG_UF_MUN")
)
# Inclui automaticamente N_MUNICIPIOS

# Top municípios exportadores
top_municipios <- analise_comex_municipal(
    tipo = "exportacao",
    anos = 2024,
    sh4 = 1201,
    agregar_por = c("CO_MUN")
) %>%
    arrange(desc(VL_FOB)) %>%
    slice_head(n = 10)
```

---

## 📊 Exemplos Avançados

### Dashboard de Balança Comercial
```r
library(dplyr)
library(ggplot2)

# Dados mensais de exportação e importação
exp_mensal <- comex_stat_geral_exps(
    anos = 2024,
    agregar_por = c("CO_MES")
)

imp_mensal <- comex_stat_geral_imps(
    anos = 2024,
    agregar_por = c("CO_MES"),
    metricas = c("VL_FOB")
)

# Calcular balança comercial
balanca <- exp_mensal %>%
    select(CO_MES, EXPORTACOES = VL_FOB) %>%
    left_join(
        imp_mensal %>% select(CO_MES, IMPORTACOES = VL_FOB),
        by = "CO_MES"
    ) %>%
    mutate(SALDO = EXPORTACOES - IMPORTACOES)

# Gráfico
ggplot(balanca, aes(x = CO_MES)) +
    geom_line(aes(y = EXPORTACOES/1e9, color = "Exportações")) +
    geom_line(aes(y = IMPORTACOES/1e9, color = "Importações")) +
    geom_line(aes(y = SALDO/1e9, color = "Saldo")) +
    labs(title = "Balança Comercial Brasileira - 2024",
         x = "Mês", y = "US$ Bilhões") +
    theme_minimal()
```

### Análise Regional com Tabelas de Referência
```r
# Carregar tabela de UFs
uf_table <- load_reference_tables("4")

# Exportações por região
exp_regional <- comex_stat_geral_exps(
    anos = 2024,
    sh2 = 12,  # Soja
    agregar_por = c("SG_UF_NCM")
) %>%
    mutate(
        REGIAO = case_when(
            SG_UF_NCM %in% c("MT", "MS", "GO", "DF") ~ "Centro-Oeste",
            SG_UF_NCM %in% c("RS", "SC", "PR") ~ "Sul",
            SG_UF_NCM %in% c("SP", "RJ", "MG", "ES") ~ "Sudeste",
            TRUE ~ "Outras"
        )
    ) %>%
    group_by(REGIAO) %>%
    summarise(VL_FOB_TOTAL = sum(VL_FOB), .groups = "drop")

print(exp_regional)
```

---

## 📚 Estrutura dos Dados

### Dados Gerais (NCM detalhado)
| Variável | Descrição | Tipo |
|----------|-----------|------|
| CO_ANO | Ano | Numérico |
| CO_MES | Mês | Numérico |
| CO_NCM | Código NCM (8 dígitos) | Numérico |
| CO_PAIS | Código do país | Numérico |
| SG_UF_NCM | UF de origem/produção | Caractere |
| VL_FOB | Valor FOB (US$) | Numérico |
| KG_LIQUIDO | Peso líquido (kg) | Numérico |

### Dados Municipais (SH4)
| Variável | Descrição | Tipo |
|----------|-----------|------|
| CO_ANO | Ano | Numérico |
| SH4 | Código SH4 | Numérico |
| CO_PAIS | Código do país | Numérico |
| SG_UF_MUN | UF da empresa | Caractere |
| CO_MUN | Código do município da empresa | Numérico |
| VL_FOB | Valor FOB (US$) | Numérico |

### Métricas Específicas de Importação
- **VL_FRETE**: Valor do frete (US$)
- **VL_SEGURO**: Valor do seguro (US$)
- **VL_TOTAL_CIF**: FOB + Frete + Seguro (calculado automaticamente)
- **PERC_FRETE**: Percentual do frete sobre FOB
- **PERC_SEGURO**: Percentual do seguro sobre FOB

---

## 🔧 Funções Principais

### Tabelas de Referência
| Função | Descrição |
|--------|-----------|
| `show_available_tables()` | Lista tabelas disponíveis |
| `load_reference_tables(codes)` | Carrega tabelas por código |
| `search_in_table(code, term)` | Busca termo em tabela |

### Análise de Dados
| Função | Descrição |
|--------|-----------|
| `comex_stat_geral_exps()` | Exportações gerais (NCM) |
| `comex_stat_geral_imps()` | Importações gerais (NCM) |
| `analise_comex_municipal()` | Dados municipais (SH4) |

### Códigos de Tabelas
- **"1"**: NCM (Nomenclatura Comum do Mercosul)
- **"2"**: NCM com códigos SH
- **"3"**: Países
- **"4"**: Unidades Federativas
- **"5"**: UF e Municípios

---

## 🎯 Casos de Uso

### 🌾 **Agronegócio**
- Análise de exportações de commodities por região
- Monitoramento de preços e volumes por destino
- Identificação de novos mercados

### 🏭 **Indústria**
- Análise de importação de insumos e máquinas
- Cálculo de custos logísticos (frete/seguro)
- Competitividade por setor

### 🏛️ **Política Pública**
- Impacto regional do comércio exterior
- Análise de balança comercial setorial
- Monitoramento de acordos comerciais

### 📊 **Pesquisa Acadêmica**
- Estudos de economia internacional
- Análise de competitividade
- Séries temporais de comércio exterior

---

## ⚡ Dicas de Performance

### ✅ **Boas Práticas**
```r
# Use filtros específicos
resultado <- comex_stat_geral_exps(
    anos = 2024,
    sh4 = c(1201, 1202),  # Específico
    co_pais = c(160)      # China apenas
)

# Aproveite o cache
dados_2024 <- comex_stat_geral_exps(anos = 2024)  # Primeira vez: download
dados_2024_novo <- comex_stat_geral_exps(anos = 2024)  # Segunda vez: cache
```

### ❌ **Evitar**
```r
# Muito amplo - pode consumir muita memória
dados_tudo <- comex_stat_geral_exps(anos = c(2020:2024))  # Evitar

# Sempre force_download = TRUE desnecessário
dados <- comex_stat_geral_exps(anos = 2024, force_download = TRUE)  # Evitar
```

---

## 🐛 Solução de Problemas

### Erro: "Nenhum registro encontrado"
```r
# Verificar códigos válidos
search_in_table("1", "produto_desejado")  # Para NCM
search_in_table("3", "pais_desejado")     # Para países
```

### Problemas de Conectividade
```r
# Testar conectividade
tryCatch({
    test_data <- load_reference_tables("3")
    cat("Conexão OK\n")
}, error = function(e) {
    cat("Erro de conexão:", e$message, "\n")
})
```

### Limpar Cache (se necessário)
```r
# Use force_download apenas se necessário
dados_fresh <- comex_stat_geral_exps(anos = 2024, force_download = TRUE)
```

---

## 📖 Documentação Completa

- **📄 Documentação Técnica e Detalhes do Pacote**: [comexTL](https://alanleal-econ.com/index.php/codes/comextl/)
- **❓ Ajuda Integrada**: Use `?nome_da_funcao` no R
- **🐛 Issues**: [GitHub Issues](https://github.com/alanleal-econ/comexTL/issues)

---

## 🤝 Contribuindo

Contribuições são bem-vindas! 

### Como Contribuir
1. **Fork** do repositório
2. **Clone** para sua máquina
3. Crie uma **branch** para sua feature
4. Faça suas modificações com **testes**
5. **Commit** com mensagens descritivas
6. **Push** para sua branch
7. Abra um **Pull Request**

### Tipos de Contribuições
- 🐛 **Bug reports** via GitHub Issues
- 💡 **Sugestões** de funcionalidades
- 📝 **Melhorias** na documentação
- 🧪 **Testes** adicionais
- 🚀 **Novas funcionalidades**

---

## 📄 Licença

Este projeto está licenciado sob a Licença MIT - veja o arquivo [LICENSE](LICENSE) para detalhes.

---

## 👨‍💻 Autor

**Alan Leal** - Economista especializado em comércio, meio ambiente e clima.

- 🐙 **GitHub**: [@alanleal-econ](https://github.com/alanleal-econ)
- 📧 **Email**: prof@alanleal-econ.com
- 🏢 **Homepage Alan Leal**: [alanleal-econ.com](https://alanleal-econ.com)

---

## 🙏 Agradecimentos

- **SECEX/MDIC** pela disponibilização dos dados abertos
- **Apache Arrow** pela tecnologia de processamento eficiente
- **Cloudflare** pela infraestrutura de distribuição

---

## 📊 Status do Projeto

![Maintenance](https://img.shields.io/badge/Maintained%3F-yes-green.svg)
![GitHub last commit](https://img.shields.io/github/last-commit/alanleal-econ/comexTL)
![GitHub issues](https://img.shields.io/github/issues/alanleal-econ/comexTL)

**Status**: 🚧 Em desenvolvimento ativo
**Versão**: Pré-release
**Compatibilidade**: R 4.0+

---

*Transforme dados de comércio exterior em insights poderosos com comexTL! 🚀*
