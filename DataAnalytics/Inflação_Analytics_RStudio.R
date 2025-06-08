
# Pacotes [Sessão: CRTL+SHIFT+R] ------------------------------------------

library(sidrar)
library(dplyr) # adicionei essa instalação de pacote depois
library(lubridate) # adicionei depois
install.packages("ggplot2")
library(ggplot2) # adicionei depois para fazer o gráfico

# Coleta de Dados ---------------------------------------------------------
# API pega no site do sidra, função dada por '::'
dados_brutos <- sidrar::get_sidra(api = "/t/1737/n1/all/v/2265/p/all/d/v2265%202")

dados_brutos_desemprego <- sidrar::get_sidra(api = "/t/6381/n1/all/v/4099/p/all/d/v4099%201")

# Tratamento de Dados -----------------------------------------------------

dados_tratados <- dados_brutos |>
  dplyr::mutate(
    data = lubridate::ym(`Mês (Código)`), # formato era chr e mudar pra data
    ipca = Valor, # renomeando Valor para valor
    .keep = "none" # descarta colunas pré existentes
  ) |>
  dplyr::filter(data >= "2004-01-01") ## excluir hiperinflação no BR
|>
  dplyr::as_tibble() ## transformando em tabela


# Tratando dados de desemprego
dados_tratados_u <- dados_brutos_desemprego |>
  dplyr::mutate(
    data = lubridate::ym(`Trimestre Móvel (Código)`),
    desemprego = Valor,
    .keep = "none"
  ) |>
  dplyr::as_tibble() ## transformando em tabela

## Unindo as tabelas por JOIN

dados_cruzados <- dplyr::inner_join(
  x = dados_tratados,
  y = dados_tratados_u,
  by = "data"
)


# Análise de Dados --------------------------------------------------------


# Como a inflação de comportou no Brasil? (evolução temporal do dado analisado)

ggplot2::ggplot(dados_tratados) +
  ggplot2::aes(x = data, y = ipca) +
  ggplot2::geom_line()

# Qual o período com menores e maiores taxas de inflação no Brasil?
dados_tratados |>
  dplyr::arrange(ipca) |> # até aqui só ordenamos
  dplyr::slice(c(1, nrow(dados_tratados)))

# Qual o valor médio da inflação no Brasil e como é a distrib. dos seus valores?

# Resumo da variável ipca (correto)
summary(dados_tratados$ipca)

# Histograma corrigido
ggplot2::ggplot(dados_tratados) +
  ggplot2::aes(x = ipca) +
  ggplot2::geom_histogram(
    fill = "#276DC3",   # azul da logo do R
    color = "white"     # borda branca
  ) +
  ggplot2::theme_minimal() +
  ggplot2::labs(
    title = "Histograma do IPCA",
    x = "IPCA",
    y = "Frequência")

# Gráfico de linha 2
ggplot2::ggplot(data = dados_tratados, ggplot2::aes(x = data, y = ipca)) +
  ggplot2::geom_line(color = "#276DC3", size = 1) +   # linha azul
  ggplot2::geom_smooth(method = "loess", se = FALSE, color = "darkred") +  # tendência suave
  ggplot2::theme_minimal() +
  ggplot2::labs(
    title = "Evolução do IPCA ao longo do tempo",
    x = "Data",
    y = "IPCA (%)"
  )


# O que afeta a inflação? Com qual variável ela se relaciona?
# EXEMPLO 1
ggplot2::ggplot(dados_cruzados) +
  ggplot2::aes(x = desemprego, y = ipca) +
  ggplot2::geom_point()

# EXEMPLO 2
ggplot2::ggplot(dados_cruzados, ggplot2::aes(x = desemprego, y = ipca)) +
  ggplot2::geom_point(color = "#276DC3", size = 2, alpha = 0.7) +  # pontos azulados com leve transparência
  ggplot2::geom_smooth(method = "lm", se = TRUE, color = "darkred", linetype = "dashed") +  # linha de tendência
  ggplot2::theme_minimal() +
  ggplot2::labs(
    title = "Relação entre Desemprego e Inflação (IPCA)",
    subtitle = "Cada ponto representa uma observação temporal",
    x = "Taxa de Desemprego (%)",
    y = "IPCA (%)"
  )

# EXEMPLO 3
cor(dados_cruzados$desemprego, dados_cruzados$ipca, use = "complete.obs")


### Se a linha de tendência estiver com inclinação negativa, pode sugerir que maior desemprego → menor inflação, o que conversa com a ideia da Curva de Phillips (embora controversa em dados reais). (menor desemprego = muita gente empregada = muita gente consumindo = maior inflação).
### correl: entre -1 e 1: Próximo de 1: forte correlação positiva; Próximo de -1: forte correlação negativa; Próximo de 0: sem correlação linear

# análise de regressão
modelo <- lm(ipca ~ desemprego, data = dados_cruzados)
summary(modelo)

ggplot2::ggplot(dados_cruzados) +
  ggplot2::aes(x = desemprego, y = ipca) +
  ggplot2::geom_point() +
  ggplot2::geom_smooth(method = 'lm')
  
