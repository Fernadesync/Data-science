library(tidyverse)
library(scales)
library(patchwork)
library(dplyr)
library(ggplot2)
library(plotrix)

data(mtcars)

dados <- mtcars

# Visualizar os primeiros registros

head(dados)

# Estrutura da base

str(dados)

# Resumo estatístico

summary(dados)

# ============================================================

# 3. PREPARAÇÃO DAS VARIÁVEIS QUALITATIVAS

# ============================================================

# Na base mtcars:

#

# am:

# 0 = automático

# 1 = manual

#

# vs:

# 0 = motor em V

# 1 = motor em linha


# ------------------------------------------------------------

# 3.1 Transforma am em variável qualitativa

# ------------------------------------------------------------

dados$am <- factor(
  dados$am,
  levels = c(0, 1),
  labels = c(
    "Automático",
    "Manual"
  )
)

# ------------------------------------------------------------

# 3.2 Transforma vs em variável qualitativa

# ------------------------------------------------------------

dados$vs <- factor(
  dados$vs,
  levels = c(0, 1),
  labels = c(
    "Motor em V",
    "Motor em linha"
  )
)



# ============================================================

# 4. FUNÇÃO PARA GRÁFICO DE BARRAS

# ============================================================

grafico_barras <- function(
    dados,
    variavel,
    tipo = "percentual",
    cor = "steelblue",
    mostrar_valor = TRUE,
    casas = 1,
    titulo = NULL,
    subtitulo = NULL
) {
  
  # ----------------------------------------------------------
  
  # Tabela de frequências
  
  # ----------------------------------------------------------
  
  tabela <- dados %>%
    count(.data[[variavel]]) %>%
    mutate(
      percentual = 100 * n / sum(n)
    ) %>%
    filter(n > 0)
  
  # ----------------------------------------------------------
  
  # Título automático
  
  # ----------------------------------------------------------
  
  if (is.null(titulo)) {
    
    titulo <- paste(
      "Distribuição de",
      variavel
    )
    
    
  }
  
  # ==========================================================
  
  # FREQUÊNCIA ABSOLUTA
  
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
    
    # PERCENTUAL
    
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
      "tipo deve ser 'frequencia' ou 'percentual'"
    )
    
    
  }
  
  return(grafico)
}

# ============================================================

# 5. GRÁFICO PARA am

# ============================================================

grafico_barras(
  dados,
  "am",
  tipo = "percentual",
  cor = "steelblue",
  titulo = "Tipo de transmissão",
  subtitulo = "Distribuição percentual dos automóveis"
)

# ============================================================

# 6. GRÁFICO PARA vs

# ============================================================

grafico_barras(
  dados,
  "vs",
  tipo = "percentual",
  cor = "darkorange",
  titulo = "Configuração do motor",
  subtitulo = "Motor em V versus motor em linha"
)

# ============================================================


# ============================================================

# 9. FREQUÊNCIA ABSOLUTA

# ============================================================

grafico_barras(
  dados,
  "am",
  tipo = "frequencia",
  cor = "tomato",
  titulo = "Tipo de transmissão",
  subtitulo = "Frequência absoluta"
)
