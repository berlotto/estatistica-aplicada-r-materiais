## ============================================================
## Biblioteca de templates ggplot2 — Projeto Estatística Aplicada em R
## Módulo 1 — entrega esperada
##
## 4 funções reutilizáveis para os gráficos mais recorrentes nos módulos
## seguintes: histograma, boxplot, scatter+regressão, série temporal.
##
## Uso: source("templates_ggplot2.R") no início de cada script/relatório.
## Todas as funções recebem NOMES DE COLUNA COMO STRING (ex.: "receita_mensal"),
## via o pronome .data[[ ]] do rlang/ggplot2 — não precisa citar a coluna sem
## aspas como no uso "manual" de aes().
## ============================================================

library(ggplot2)
library(dplyr)
library(scales)
library(RColorBrewer)

## ------------------------------------------------------------
## Tema padrão do projeto — aplicado por default em todas as funções abaixo
## ------------------------------------------------------------
tema_estudo <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", size = rel(1.15)),
      plot.subtitle = element_text(color = "gray40"),
      plot.caption = element_text(color = "gray50", size = rel(0.75)),
      panel.grid.minor = element_blank(),
      axis.title = element_text(face = "bold"),
      legend.position = "bottom"
    )
}

## ------------------------------------------------------------
## 1. Histograma com linhas de média e mediana
## ------------------------------------------------------------
#' @param df data.frame
#' @param var string — nome da coluna numérica
#' @param bins número de bins (default 30)
#' @param titulo título do gráfico (opcional)
#' @param mostrar_referencias se TRUE, desenha linha de média (vermelho) e mediana (verde tracejado)
plot_histograma <- function(df, var, bins = 30, titulo = NULL, mostrar_referencias = TRUE) {
  p <- ggplot(df, aes(x = .data[[var]])) +
    geom_histogram(bins = bins, fill = "steelblue", color = "white")

  if (mostrar_referencias) {
    media_val   <- mean(df[[var]], na.rm = TRUE)
    mediana_val <- median(df[[var]], na.rm = TRUE)
    p <- p +
      geom_vline(xintercept = media_val, color = "firebrick", linewidth = 1) +
      geom_vline(xintercept = mediana_val, color = "darkgreen", linewidth = 1, linetype = "dashed")
  }

  p +
    labs(
      title = titulo %||% paste("Distribuição de", var),
      subtitle = if (mostrar_referencias) "Média (vermelho) vs. mediana (verde tracejado)" else NULL,
      x = var, y = "Contagem"
    ) +
    tema_estudo()
}

## ------------------------------------------------------------
## 2. Boxplot comparativo entre grupos, ordenado pela mediana
## ------------------------------------------------------------
#' @param df data.frame
#' @param grupo string — coluna categórica (eixo x)
#' @param valor string — coluna numérica (eixo y)
#' @param titulo título do gráfico (opcional)
#' @param paleta paleta RColorBrewer qualitativa (default "Set2")
plot_boxplot <- function(df, grupo, valor, titulo = NULL, paleta = "Set2") {
  df$.grupo_ordenado <- reorder(df[[grupo]], df[[valor]], FUN = median, na.rm = TRUE)

  ggplot(df, aes(x = .grupo_ordenado, y = .data[[valor]], fill = .grupo_ordenado)) +
    geom_boxplot() +
    scale_fill_brewer(palette = paleta) +
    labs(
      title = titulo %||% paste(valor, "por", grupo),
      subtitle = "Grupos ordenados pela mediana",
      x = grupo, y = valor
    ) +
    tema_estudo() +
    theme(legend.position = "none")
}

## ------------------------------------------------------------
## 3. Scatter + regressão (lm ou loess)
## ------------------------------------------------------------
#' @param df data.frame
#' @param x string — coluna numérica (eixo x)
#' @param y string — coluna numérica (eixo y)
#' @param metodo "lm" (linear) ou "loess" (suavização não-paramétrica)
#' @param facetar_por string opcional — coluna categórica para facet_wrap
#' @param titulo título do gráfico (opcional)
plot_scatter_regressao <- function(df, x, y, metodo = "lm", facetar_por = NULL, titulo = NULL) {
  p <- ggplot(df, aes(x = .data[[x]], y = .data[[y]])) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = metodo, color = "firebrick", se = TRUE)

  if (!is.null(facetar_por)) {
    p <- p + facet_wrap(vars(.data[[facetar_por]]))
  }

  p +
    labs(
      title = titulo %||% paste(y, "vs.", x),
      subtitle = paste("Ajuste:", metodo),
      x = x, y = y
    ) +
    tema_estudo()
}

## ------------------------------------------------------------
## 4. Série temporal (uma ou várias séries)
## ------------------------------------------------------------
#' @param df data.frame
#' @param data_col string — coluna de data (classe Date)
#' @param valor_col string — coluna numérica a plotar
#' @param grupo_col string opcional — coluna categórica para múltiplas séries (cor)
#' @param facetar se TRUE e grupo_col for informado, usa facet_wrap em vez de cor sobreposta
#' @param titulo título do gráfico (opcional)
plot_serie_temporal <- function(df, data_col, valor_col, grupo_col = NULL,
                                 facetar = FALSE, titulo = NULL) {
  if (is.null(grupo_col)) {
    p <- ggplot(df, aes(x = .data[[data_col]], y = .data[[valor_col]])) +
      geom_line(color = "steelblue", linewidth = 0.7)
  } else if (facetar) {
    p <- ggplot(df, aes(x = .data[[data_col]], y = .data[[valor_col]])) +
      geom_line(color = "steelblue", linewidth = 0.7) +
      facet_wrap(vars(.data[[grupo_col]]), scales = "free_y")
  } else {
    p <- ggplot(df, aes(x = .data[[data_col]], y = .data[[valor_col]], color = .data[[grupo_col]])) +
      geom_line(linewidth = 0.7)
  }

  p +
    labs(
      title = titulo %||% paste("Série temporal —", valor_col),
      x = "Data", y = valor_col, color = grupo_col
    ) +
    tema_estudo()
}

## ------------------------------------------------------------
## Fallback do operador %||% (Elvis) — nativo a partir do R >= 4.4
## Se sua versão do R for anterior, esta definição evita erro.
## ------------------------------------------------------------
if (!exists("%||%")) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}

## ------------------------------------------------------------
## Exemplo de uso (requer varejo_sintetico.csv e painel_regional_varejo.csv
## no diretório de trabalho)
## ------------------------------------------------------------
# varejo <- read.csv("varejo_sintetico.csv", stringsAsFactors = TRUE)
# painel <- read.csv("painel_regional_varejo.csv", stringsAsFactors = FALSE)
# painel$mes_ano <- as.Date(painel$mes_ano)
#
# plot_histograma(varejo, "receita_mensal")
# plot_boxplot(varejo, grupo = "porte", valor = "ticket_medio")
# plot_scatter_regressao(varejo, x = "fluxo_clientes_mensal", y = "receita_mensal",
#                         facetar_por = "regiao")
# plot_serie_temporal(
#   painel %>% dplyr::filter(cidade_id %in% c("CID01", "CID10", "CID25")),
#   data_col = "mes_ano", valor_col = "indice_vendas", grupo_col = "cidade_nome", facetar = TRUE
# )
