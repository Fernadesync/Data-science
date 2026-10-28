# ============================================================

# ANÁLISE GRÁFICA AUTOMÁTICA DE UM ARQUIVO CSV

# Variáveis qualitativas e quantitativas

# Exemplo: DadosCiaMB.csv

# ============================================================

# ------------------------------------------------------------

# 1. PACOTES

# ------------------------------------------------------------

# Instalar, caso ainda não estejam instalados:

# install.packages(c("tidyverse", "scales", "patchwork"))

library(tidyverse)
library(scales)
library(patchwork)
library(dplyr)
library(ggplot2)
library(plotrix)
# ------------------------------------------------------------

# 2. LEITURA DOS DADOS

# ------------------------------------------------------------

# Arquivo CSV

dados <- read.csv(
  "DadosCiaMB.csv",
  header = TRUE,
  sep=";",dec=","
)

# Visualização inicial

head(dados)

# Estrutura da base

str(dados)

# Dimensões

dim(dados)

# Nomes das variáveis

names(dados)


# ============================================================

# PARTE I — VARIÁVEIS QUALITATIVAS

# ============================================================

# ------------------------------------------------------------

grafico_barras <- function(
    dados,
    variavel,
    tipo = "percentual",
    cor = "steelblue",
    mostrar_valor = TRUE,
    casas = 1,
    titulo = NULL,
    subtitulo = NULL,
    ordem = NULL
) {
  
  # ----------------------------------------------------------
  
  # 1. Criar cópia dos dados
  
  # ----------------------------------------------------------
  
  dados_grafico <- dados
  
  # ----------------------------------------------------------
  
  # 2. Definir ordem das categorias
  
  # ----------------------------------------------------------
  
  if (!is.null(ordem)) {
    
    
    dados_grafico[[variavel]] <- factor(
      dados_grafico[[variavel]],
      levels = ordem,
      ordered = TRUE
    )
    
    
  }
  
  # ----------------------------------------------------------
  
  # 3. Tabela de frequências
  
  # ----------------------------------------------------------
  
  tabela <- dados_grafico %>%
    count(.data[[variavel]]) %>%
    mutate(
      percentual = 100 * n / sum(n)
    ) %>%
    filter(n > 0)
  
  # ----------------------------------------------------------
  
  # 4. Título automático
  
  # ----------------------------------------------------------
  
  if (is.null(titulo)) {
    
    
    titulo <- paste(
      "Distribuição de",
      variavel
    )
    
    
  }
  
  # ==========================================================
  
  # 5. FREQUÊNCIA ABSOLUTA
  
  # ==========================================================
  
  if (tipo == "frequencia") {
    
    
    grafico <- ggplot(
      tabela,
      aes(
        x = .data[[variavel]],
        y = n
      )
    ) +
      
      geom_col(
        fill = cor,
        width = 0.7
      ) +
      
      labs(
        title = titulo,
        subtitle = subtitulo,
        x = variavel,
        y = "Frequência absoluta"
      ) +
      
      theme_minimal(
        base_size = 14
      )
    
    
    # Valores sobre as barras
    
    if (mostrar_valor) {
      
      grafico <- grafico +
        
        geom_text(
          aes(
            label = n
          ),
          vjust = -0.3,
          size = 5
        )
    }
    
    
    # ==========================================================
    
    # 6. PERCENTUAL
    
    # ==========================================================
    
  } else if (tipo == "percentual") {
      grafico <- ggplot(
      tabela,
      aes(
        x = .data[[variavel]],
        y = percentual
      )
    ) +
        geom_col(
        fill = cor,
        width = 0.7
      ) +
        scale_y_continuous(
        labels = function(x) {
          paste0(x, "%")
        },
        expand = expansion(
          mult = c(0, 0.08)
        )
      ) +
        labs(
        title = titulo,
        subtitle = subtitulo,
        x = variavel,
        y = "Percentual"
      ) +
        theme_minimal(
        base_size = 14
      )
      # Percentuais sobre as barras
    
    if (mostrar_valor) {
        grafico <- grafico +
        geom_text(
          aes(
            label = paste0(
              round(
                percentual,
                casas
              ),
              "%"
            )
          ),
          vjust = -0.3,
          size = 5
        )
    }
    } else {
    
    
    stop(
      "O argumento 'tipo' deve ser 'frequencia' ou 'percentual'."
    )
    }
  
  # ----------------------------------------------------------
  
  # 7. Retornar gráfico
  
  # ----------------------------------------------------------
  
  return(grafico)
}

# ============================================================

# EXEMPLO 1 — VARIÁVEL NOMINAL

# ============================================================

grafico_barras(
  dados = dados,
  variavel = "Civil",
  tipo = "percentual",
  cor = "steelblue",
  titulo = "Estado civil dos funcionários",
  subtitulo = "Distribuição percentual da amostra",
  
)

# ============================================================

# EXEMPLO 2 — VARIÁVEL ORDINAL

# ============================================================

grafico_barras(
  dados = dados,
  variavel = "Instrucao",
  tipo = "percentual",
  cor = "darkorange",
  
  titulo = "Nível de instrução dos funcionários",
  
  subtitulo = "Distribuição percentual segundo a escolaridade",
  
  ordem = c(
    "f",
    "m",
    "s"
  )
)

# ============================================================

# EXEMPLO 3 — FREQUÊNCIA ABSOLUTA

# ============================================================

grafico_barras(
  dados = dados,
  variavel = "Instrucao",
  tipo = "frequencia",
  cor = "darkgreen",
  
  titulo = "Número de funcionários por nível de instrução",
  
  subtitulo = "Frequência absoluta",
  
  ordem = c(
    "f",
    "m",
    "s"
  )
)

# ============================================================

# EXEMPLO 4 — TÍTULO E SUBTÍTULO PERSONALIZADOS

# ============================================================

grafico_barras(
  dados = dados,
  variavel = "Civil",
  
  tipo = "percentual",
  
  cor = "#2E86AB",
  
  titulo = "Perfil dos funcionários da empresa",
  
  subtitulo = "Distribuição segundo o estado civil",
  
  mostrar_valor = T,
  
  casas = 1
)

# ============================================================

# 7. GRÁFICO DE SETORES

# ============================================================

grafico_setores <- function(dados, variavel) {
  
  tabela <- dados %>%
    count(.data[[variavel]]) %>%
    mutate(
      percentual = n / sum(n) * 100,
      label = paste0(
        .data[[variavel]],
        "\n",
        round(percentual, 1),
        "%"
      )
    )
  
  ggplot(
    tabela,
    aes(
      x = "",
      y = n,
      fill = .data[[variavel]]
    )
  ) +
    
    
    geom_col(width = 1) +
    
    coord_polar(theta = "y") +
    
    geom_text(
      aes(
        label = label
      ),
      position = position_stack(vjust = 0.5),
      size = 5
    ) +
    
    labs(
      title = paste("Distribuição percentual —", variavel),
      fill = variavel
    ) +
    
    theme_void() +
    
    theme(
      plot.title = element_text(
        hjust = 0.5,
        face = "bold"
      ),
      legend.position = "right"
    )
  
  
}

# Exemplos

grafico_setores(dados, "Civil")

grafico_setores(dados, "Instrucao")

grafico_setores(dados, "procedencia")


# ============================================================

# GRÁFICO DE SETORES TRIDIMENSIONAL

# ============================================================

grafico_setores_3d <- function(
    dados,
    variavel,
    titulo = NULL,
    cores = NULL,
    raio = 1,
    altura = 0.15,
    mostrar_percentual = TRUE,
    casas = 1,
    legenda = TRUE
) {
  
  # ----------------------------------------------------------
  
  # Tabela de frequências
  
  # ----------------------------------------------------------
  
  tabela <- table(
    dados[[variavel]]
  )
  
  # ----------------------------------------------------------
  
  # Percentuais
  
  # ----------------------------------------------------------
  
  percentual <- 100 * tabela / sum(tabela)
  
  # ----------------------------------------------------------
  
  # Rótulos das categorias
  
  # ----------------------------------------------------------
  
  categorias <- names(tabela)
  
  # ----------------------------------------------------------
  
  # Criar rótulos
  
  # ----------------------------------------------------------
  
  if (mostrar_percentual) {
    
    
    rotulos <- paste0(
      categorias,
      "\n",
      round(
        percentual,
        casas
      ),
      "%"
    )
    
    
  } else {
    
    
    rotulos <- categorias
    
    
  }
  
  # ----------------------------------------------------------
  
  # Cores automáticas
  
  # ----------------------------------------------------------
  
  if (is.null(cores)) {
    
    
    cores <- hcl.colors(
      length(tabela),
      palette = "Set 2"
    )
    
    
  }
  
  # ----------------------------------------------------------
  
  # Título automático
  
  # ----------------------------------------------------------
  
  if (is.null(titulo)) {
    
    
    titulo <- paste(
      "Distribuição de",
      variavel
    )
    
    
  }
  
  # ----------------------------------------------------------
  
  # Gráfico 3D
  
  # ----------------------------------------------------------
  
  pie3D(
    tabela,
    
    
    labels = rotulos,
    
    explode = 0.05,
    
    radius = raio,
    
    height = altura,
    
    col = cores,
    
    main = titulo,
    
    labelcex = 1.0,
    
    start = pi / 2
    
    
  )
  
  # ----------------------------------------------------------
  
  # Legenda
  
  # ----------------------------------------------------------
  
  if (legenda) {
    
    
    legend(
      "topright",
      
      legend = paste0(
        categorias,
        " (",
        round(
          percentual,
          casas
        ),
        "%)"
      ),
      
      fill = cores,
      
      cex = 0.9,
      
      bty = "n"
    )
    
    
  }
}

# ============================================================

# Exemplos

# ============================================================

grafico_setores_3d(
  dados = dados,
  
  variavel = "Civil",
  
  titulo = "Estado civil dos funcionários",
  
  cores = c(
    "#3498DB",
    "#E74C3C"
  )
)



grafico_setores_3d(
  dados = dados,
  
  variavel = "Instrucao",
  
  titulo = "Nível de instrução dos funcionários",
  
  cores = c(
    "#2E86AB",
    "#F39C12",
    "#27AE60"
  )
)



grafico_setores_3d(
  dados = dados,
  
  variavel = "procedencia",
  
  titulo = "Procedência dos funcionários",
  
  cores = c(
    "#9B59B6",
    "#3498DB",
    "#E67E22"
  )
)

# ============================================================

# SEM LEGENDA

# ============================================================

grafico_setores_3d(
  dados = dados,
  
  variavel = "Civil",
  
  titulo = "Distribuição do estado civil",
  
  cores = c(
    "#3498DB",
    "#E74C3C"
  ),
  
  legenda = FALSE
)

# ============================================================

# ALTERANDO A PROFUNDIDADE DO EFEITO 3D

# ============================================================

grafico_setores_3d(
  dados = dados,
  
  variavel = "Civil",
  
  titulo = "Estado civil — efeito 3D",
  
  cores = c(
    "#3498DB",
    "#E74C3C"
  ),
  
  altura = 0.25
)

# PERCENTUAIS COM DUAS CASAS DECIMAIS

# ============================================================

grafico_setores_3d(
  dados = dados,
  
  variavel = "procedencia",
  
  titulo = "Procedência dos funcionários",
  
  cores = c(
    "#3498DB",
    "#E67E22",
    "#27AE60"
  ),
  
  casas = 2
)

#============================================================
#Retirando os rótulos do gráfico deixando apenas os percentuais
# ============================================================

grafico_setores_3d <- function(
    dados,
    variavel,
    titulo = NULL,
    cores = NULL,
    raio = 1,
    altura = 0.15,
    mostrar_percentual = TRUE,
    casas = 1,
    legenda = TRUE
) {
  
  # ----------------------------------------------------------
  
  # Tabela de frequências
  
  # ----------------------------------------------------------
  
  tabela <- table(
    dados[[variavel]]
  )
  
  # ----------------------------------------------------------
  
  # Percentuais
  
  # ----------------------------------------------------------
  
  percentual <- 100 * tabela / sum(tabela)
  
  # ----------------------------------------------------------
  
  # Rótulos das categorias
  
  # ----------------------------------------------------------
  
  categorias <- names(tabela)
  
  # ----------------------------------------------------------
  
  # Criar rótulos
  
  # ----------------------------------------------------------
  
  if (mostrar_percentual) {
    
    
    rotulos <- paste0(round(percentual, casas), "%")
    
    
    
  } else {
    
    
    rotulos <- categorias
    
    
  }
  
  # ----------------------------------------------------------
  
  # Cores automáticas
  
  # ----------------------------------------------------------
  
  if (is.null(cores)) {
    
    
    cores <- hcl.colors(
      length(tabela),
      palette = "Set 2"
    )
    
    
  }
  
  # ----------------------------------------------------------
  
  # Título automático
  
  # ----------------------------------------------------------
  
  if (is.null(titulo)) {
    
    
    titulo <- paste(
      "Distribuição de",
      variavel
    )
    
    
  }
  
  # ----------------------------------------------------------
  
  # Gráfico 3D
  
  # ----------------------------------------------------------
  
  pie3D(
    tabela,
    
    
    labels = rotulos,
    
    explode = 0.05,
    
    radius = raio,
    
    height = altura,
    
    col = cores,
    
    main = titulo,
    
    labelcex = 1.0,
    
    start = pi / 2
    
    
  )
  
  # ----------------------------------------------------------
  
  # Legenda
  
  # ----------------------------------------------------------
  
  if (legenda) {
    
    
    legend(
      "topright",
      
      legend = paste0(
        categorias,
        " (",
        round(
          percentual,
          casas
        ),
        "%)"
      ),
      
      fill = cores,
      
      cex = 0.9,
      
      bty = "n"
    )
    
    
  }
}
#=============================================================

grafico_setores_3d(
  dados = dados,
  
  variavel = "procedencia",
  
  titulo = "Procedência dos funcionários",
  
  cores = c(
    "#3498DB",
    "#E67E22",
    "#27AE60"
  ),
  
  casas = 2
)

