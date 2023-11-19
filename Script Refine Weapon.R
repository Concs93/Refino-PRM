# Bibliotecas necessárias
rm(list=ls())
options(scipen = 999, digits = 2)
options(big.mark = ".", decimal.mark = ",")
library(ggplot2)
library(dplyr)
library(pbapply)
library(scales) # Para a função alpha()
library(RColorBrewer)
library(knitr)
library(ggrepel)
setwd("D:\\Return to Morroc Full")


# dados_refino <- read.csv2("Refine Table.csv", sep = ";", fileEncoding="UTF-8-BOM")

# Função para simular o refino

# nivel_arma <- 1
# nivel_refino <- 9
# refine_data <- read.csv2("Refine Table.csv", sep = ";", fileEncoding="UTF-8-BOM")

##############################################
sim_refino <- function(nivel_arma, nivel_refino) {
  # Carregar os dados do arquivo CSV
  refine_data <- read.csv2("Refine Table.csv", sep = ";", fileEncoding="UTF-8-BOM")
  # refine_data <- read.csv2("Refine Table Garantido.csv", sep = ";", fileEncoding="UTF-8-BOM")

  # Filtrar os dados para o nível de arma específico
  data_filtered <- refine_data %>% filter(Type.Of.Refine == nivel_arma)

  total_cost_materials <- 0
  total_cost_refine <- 0
  materials_count <- c("Oridecon" = 0, "HD Oridecon" = 0, "Enriched Oridecon" = 0, "Jewel Oridecon" = 0,
                       "Elunium" = 0, "HD Elunium" = 0, "Enriched Elunium" = 0, "Jewel Elunium" = 0,
                       "Shadowdecon" = 0, "Jewel Shadowdecon" = 0, "Bradium" = 0, "Jewel Bradium" = 0)
  current_level <- 0

  while (current_level < nivel_refino) {
    # Obter dados para o nível atual
    refine_info <- data_filtered[current_level + 1, ]
    success_chance <- refine_info["Success.Chance"]
    refine_price <- refine_info["Refine.Price"]
    material_price <- refine_info["Material.Price"]
    required_material <- as.character(refine_info["Required.Material"])

    # Tentar refinar
      if (runif(1) < success_chance) {
        current_level <- current_level + 1
      } else {
        if (current_level >= 9){
          
        } else {
          current_level <- max(current_level - 1, 0)
        }
      }


    # Adicionar custos e materiais
    total_cost_refine <- total_cost_refine + refine_price
    total_cost_materials <- total_cost_materials + material_price
    materials_count[required_material] <- materials_count[required_material] + 1
  }

  list("Minérios Gastos" = materials_count,
       "Zeny Gasto" = list("Com minérios" = total_cost_materials,
                           "Com refino" = total_cost_refine,
                           "Total" = total_cost_materials + total_cost_refine))
}
###############################################

sim_refino_sf <- function(nivel_arma, nivel_refino) {
  # Carregar os dados do arquivo CSV
  # refine_data <- read.csv2("Refine Table.csv", sep = ";", fileEncoding="UTF-8-BOM")
  refine_data <- read.csv2("Refine Table Garantido.csv", sep = ";", fileEncoding="UTF-8-BOM")
  
  # Filtrar os dados para o nível de arma específico
  data_filtered <- refine_data %>% filter(Type.Of.Refine == nivel_arma)
  
  total_cost_materials <- 0
  total_cost_refine <- 0
  materials_count <- c("Oridecon" = 0, "HD Oridecon" = 0, "Enriched Oridecon" = 0, "Jewel Oridecon" = 0,
                       "Elunium" = 0, "HD Elunium" = 0, "Enriched Elunium" = 0, "Jewel Elunium" = 0,
                       "Shadowdecon" = 0, "Jewel Shadowdecon" = 0, "Bradium" = 0, "Jewel Bradium" = 0)
  current_level <- 0
  
  while (current_level < nivel_refino) {
    # Obter dados para o nível atual
    refine_info <- data_filtered[current_level + 1, ]
    success_chance <- refine_info["Success.Chance"]
    refine_price <- refine_info["Refine.Price"]
    material_price <- refine_info["Material.Price"]
    required_material <- as.character(refine_info["Required.Material"])
    
    # Tentar refinar
    if (runif(1) < success_chance) {
      current_level <- current_level + 1
    }
    # Adicionar custos e materiais
    total_cost_refine <- total_cost_refine + refine_price
    total_cost_materials <- total_cost_materials + material_price
    materials_count[required_material] <- materials_count[required_material] + 1
  }
  
  list("Minérios Gastos" = materials_count, 
       "Zeny Gasto" = list("Com minérios" = total_cost_materials, 
                           "Com refino" = total_cost_refine, 
                           "Total" = total_cost_materials + total_cost_refine))
}

######################################################
# sim_refino_sr <- function(nivel_arma, nivel_refino) {
#   # Carregar os dados do arquivo CSV
#   refine_data <- read.csv2("Refine Table.csv", sep = ";", fileEncoding="UTF-8-BOM")
#   
#   # Filtrar os dados para o nível de arma específico
#   data_filtered <- refine_data %>% filter(Type.Of.Refine == nivel_arma)
#   
#   total_cost_materials <- 0
#   total_cost_refine <- 0
#   materials_count <- c("Oridecon" = 0, "HD Oridecon" = 0, "Enriched Oridecon" = 0, "Jewel Oridecon" = 0,
#                        "Elunium" = 0, "HD Elunium" = 0, "Enriched Elunium" = 0, "Jewel Elunium" = 0,
#                        "Jewel Shadowdecon" = 0, "Jewel Bradium" = 0)
#   current_level <- 0
#   
#   while (current_level < nivel_refino) {
#     # Obter dados para o nível atual
#     refine_info <- data_filtered[current_level + 1, ]
#     success_chance <- refine_info["Success.Chance"]
#     refine_price <- refine_info["Refine.Price"]
#     material_price <- refine_info["Material.Price"]
#     required_material <- as.character(refine_info["Required.Material"])
#     
#     # Tentar refinar
#     if (runif(1) < success_chance) {
#       current_level <- current_level + 1
#     }
#     # Adicionar custos e materiais
#     total_cost_refine <- total_cost_refine + refine_price
#     total_cost_materials <- total_cost_materials + material_price
#     materials_count[required_material] <- materials_count[required_material] + 1
#   }
#   
#   list("Minérios Gastos" = materials_count, 
#        "Zeny Gasto" = list("Com minérios" = total_cost_materials, 
#                            "Com refino" = total_cost_refine, 
#                            "Total" = total_cost_materials + total_cost_refine))
# }
################################################################################



n_simulacoes <- 10000
nivel_arma <- c("Weapon Level 1",
                "Weapon Level 2",
                "Weapon Level 3",
                "Weapon Level 4",
                "Armor",
                "Shadow Armor",
                "Runes and Orbs")
nivel_refino <- 6

for (nivel in nivel_arma) {
  # Usando pblapply para executar as simulações com barra de progresso
  # resultados <- pblapply(1:n_simulacoes, function(x) sim_refino(nivel, nivel_refino))
  # para shadow e runas
  # resultados <- pblapply(1:n_simulacoes, function(x) sim_refino_sr(nivel, nivel_refino))
  # usando minerios sem voltar
  set.seed(123)
  resultados <- pblapply(1:n_simulacoes, function(x) sim_refino(nivel, nivel_refino))
  
  
  # Função para calcular a moda
  Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  
  # Inicializar contadores para os minérios
  total_minerios <- data.frame("Oridecon" = integer(), "HD Oridecon" = integer(), "Enriched Oridecon" = integer(), "Jewel Oridecon" = integer(),
                               "Elunium" = integer(), "HD Elunium" = integer(), "Enriched Elunium" = integer(), "Jewel Elunium" = integer(),
                               "Shadowdecon" = integer(), "Jewel Shadowdecon" = integer(), "Bradium" = integer(), "Jewel Bradium" = integer())
  
  # Coletar dados das simulações
  for (resultado in resultados) {
    total_minerios <- rbind(total_minerios, resultado[["Minérios Gastos"]])
  }
  colnames(total_minerios) <- c("Oridecon", "HD Oridecon", "Enriched Oridecon", "Jewel Oridecon",
                                "Elunium", "HD Elunium", "Enriched Elunium", "Jewel Elunium",
                                "Shadowdecon", "Jewel Shadowdecon","Bradium", "Jewel Bradium")
  
  # Calcular estatísticas para cada tipo de minério
  estatisticas_minerios <- sapply(total_minerios, function(x) list("Média" = ceiling(mean(x)), "Moda" = ceiling(Mode(x)), "Mediana" = ceiling(median(x)), "Desvio Padrão" = ceiling(sd(x))))
  colnames(estatisticas_minerios) <- c("Oridecon", "HD Oridecon", "Enriched Oridecon", "Jewel Oridecon",
                                       "Elunium", "HD Elunium", "Enriched Elunium", "Jewel Elunium",
                                       "Shadowdecon", "Jewel Shadowdecon","Bradium", "Jewel Bradium")
  
  # Inicializar contadores para zeny
  zeny_gasto <- data.frame("Com Minérios" = numeric(), "Com Refino" = numeric(), "Total" = numeric())
  
  # Coletar dados das simulações
  for (resultado in resultados) {
    zeny_atual <- resultado[["Zeny Gasto"]]
    zeny_gasto <- rbind(zeny_gasto, c(zeny_atual[["Com minérios"]], zeny_atual[["Com refino"]], zeny_atual[["Total"]]))
  }
  colnames(zeny_gasto) <- c("Preço Material", "Preço Refino", "Preço Total")
  
  # Calcular estatísticas para zeny
  estatisticas_zeny <- data.frame(
    "Média" = c(mean(zeny_gasto[["Preço Material"]]), mean(zeny_gasto[["Preço Refino"]]), mean(zeny_gasto[["Preço Total"]])),
    "Moda" = c(Mode(zeny_gasto[["Preço Material"]]), Mode(zeny_gasto[["Preço Refino"]]), Mode(zeny_gasto[["Preço Total"]])),
    "Mediana" = c(median(zeny_gasto[["Preço Material"]]), median(zeny_gasto[["Preço Refino"]]), median(zeny_gasto[["Preço Total"]])),
    "Desvio Padrão" = c(sd(zeny_gasto[["Preço Material"]]), sd(zeny_gasto[["Preço Refino"]]), sd(zeny_gasto[["Preço Total"]]))
  )
  rownames(estatisticas_zeny) <- c("Preço Material", "Preço Refino", "Preço Total")
  colnames(estatisticas_zeny) <- c("Média", "Moda", "Mediana", "Desvio Padrão")
  
  estatisticas_minerios
  estatisticas_zeny
  
  
  data_long <- data.frame(minerio = rep(c("Oridecon", "HD Oridecon", "Enriched Oridecon", "Jewel Oridecon",
                                          "Elunium", "HD Elunium", "Enriched Elunium", "Jewel Elunium",
                                          "Shadowdecon", "Jewel Shadowdecon","Bradium", "Jewel Bradium"), each = nrow(total_minerios)),
                          quantidade = c(total_minerios$Oridecon, total_minerios$`HD Oridecon`, total_minerios$`Enriched Oridecon`, total_minerios$`Jewel Oridecon`,
                                         total_minerios$Elunium, total_minerios$`HD Elunium`, total_minerios$`Enriched Elunium`, total_minerios$`Jewel Elunium`,
                                         total_minerios$`Shadowdecon`, total_minerios$`Jewel Shadowdecon`, total_minerios$`Bradium`, total_minerios$`Jewel Bradium`))
  
  data_filtered <- filter(data_long, quantidade > 0)
  
  # Calcular estatísticas para cada minério
  
  # Calcular média, moda e percentil 90 para cada tipo de minério
  estatisticas_minerios <- data_filtered %>%
    group_by(minerio) %>%
    summarise(
      media = mean(quantidade),
      moda = Mode(quantidade),
      percentil90 = quantile(quantidade, 0.9),
      percentil95 = max(quantile(quantidade, 0.95))
    )
  
  estatisticas_minerios <- as.data.frame(estatisticas_minerios)
  estatisticas_minerios <- estatisticas_minerios %>% filter(media != 0, moda != 0, percentil90 != 0)
  
  # Limitar o eixo X até o percentil 95
  xmax_limit <- max(estatisticas_minerios$percentil95)
  
  # Criar o gráfico
  # ggplot(data_filtered, aes(x = quantidade, fill = minerio)) +
  #   geom_density(alpha = 0.5) +
  #   scale_fill_brewer(palette = "Set1") +
  #   xlim(0, xmax_limit) +
  #   # Adicionar linhas verticais para média, moda e percentil 90
  #   geom_vline(data = estatisticas_minerios, aes(xintercept = media, color = minerio), linetype = "dashed", size = 1) +
  #   geom_vline(data = estatisticas_minerios, aes(xintercept = moda, color = minerio), linetype = "dotted", size = 1) +
  #   geom_vline(data = estatisticas_minerios, aes(xintercept = percentil90, color = minerio), linetype = "solid", size = 1) +
  #   # Adicionar anotações para média, moda e percentil 90
  #   geom_text(data = estatisticas_minerios, aes(x = round(media, 0), y = 0.20, label = paste("Média:", round(media, 0)), color = minerio), hjust = -0.05, vjust = 7, fontface = "bold") +
  #   geom_text(data = estatisticas_minerios, aes(x = moda, y = 0.20, label = paste("Moda:", round(moda, 0)), color = minerio), hjust = -0.05, vjust = 9, fontface = "bold") +
  #   geom_text(data = estatisticas_minerios, aes(x = percentil90, y = 0.20, label = paste("P90:", round(percentil90, 0)), color = minerio), hjust = -0.05, vjust = 11, fontface = "bold") +
  #   labs(title = "Histograma Suavizado dos Minérios Gastos com Linhas de Referência",
  #        x = "Quantidade de Minério",
  #        y = "Densidade") +
  #   theme_minimal()
  
  
  # Aplique a função escurecer_cor para cada cor da paleta
  cores_originais <- brewer.pal(4, "Set1")[1:length(unique(data_filtered$minerio))] # Ajuste o número conforme necessário para o número de cores que você está usando
  cores_escuras <- sapply(cores_originais, adjustcolor, 1.5)
  # 
  # # Mapeamento manual de cores para minérios
  cores_minerios <- setNames(rev(cores_escuras), unique(data_filtered$minerio))
  
  # ggplot(data_filtered, aes(x = quantidade, fill = minerio)) +
  #   geom_density(alpha = 0.5) +
  #   scale_fill_brewer(palette = "Set1") +
  #   # xlim(0, xmax_limit) +
  #   # Adicionar linhas verticais para média, moda e percentil 90
  #   geom_vline(data = estatisticas_minerios, aes(xintercept = media, color = minerio), linetype = "dashed", size = 1, show.legend = FALSE) +
  #   geom_vline(data = estatisticas_minerios, aes(xintercept = moda, color = minerio), linetype = "dotted", size = 1, show.legend = FALSE) +
  #   geom_vline(data = estatisticas_minerios, aes(xintercept = percentil90, color = minerio), linetype = "solid", size = 1, show.legend = FALSE) +
  #   # scale_color_manual(values = cores_minerios) + # Use as cores mapeadas para minérios
  #   # Adicionar anotações para média, moda e percentil 90
  #   geom_label(data = estatisticas_minerios, aes(x = round(media, 0), y = 0.05, label = paste("Média/Mean:", round(media, 0))), fill = "white", vjust = -13, fontface = "bold", label.size = NA, label.padding = unit(0, "lines"), show.legend = FALSE) +
  #   geom_label(data = estatisticas_minerios, aes(x = round(moda, 0), y = 0.05, label = paste("Moda/Mode:", round(moda, 0))), fill = "white", vjust = -15, fontface = "bold", label.size = NA, label.padding = unit(0, "lines"), show.legend = FALSE) +
  #   geom_label(data = estatisticas_minerios, aes(x = percentil90, y = 0.05, label = paste("P90:", round(percentil90, 0))), fill = "white", vjust = -17, fontface = "bold", label.size = NA, label.padding = unit(0, "lines"), show.legend = FALSE) +
  #   labs(title = "Histograma Suavizado da Quantidade de Minérios Usados\nSmooth Histogram of Minerals Spent",
  #        x = "Minérios\nMinerals",
  #        y = "Densidade\nDensity") +
  #   theme_minimal() +
  #   scale_x_continuous(breaks = seq(0, xmax_limit, 5), limits = c(0, xmax_limit))
  
  # ... [código para preparar os dados] ...
  
  # Crie um histograma suavizado separado para cada tipo de minério usando facet_wrap
  minerio_plot <- ggplot(data_filtered, aes(x = quantidade, fill = minerio)) +
    geom_density(alpha = 0.5) +
    scale_fill_brewer(palette = "Set1") +
    geom_vline(data = estatisticas_minerios, aes(xintercept = media, color = minerio), linetype = "dashed", size = 1, show.legend = FALSE) +
    geom_vline(data = estatisticas_minerios, aes(xintercept = moda, color = minerio), linetype = "dotted", size = 1, show.legend = FALSE) +
    geom_vline(data = estatisticas_minerios, aes(xintercept = percentil90, color = minerio), linetype = "solid", size = 1, show.legend = FALSE) +
    geom_label_repel(data = estatisticas_minerios, aes(x = media, y = 0.05, label = paste("Média/Mean:", round(media, 0))), fill = "white",
               fontface = "bold",
               label.size = NA,
               label.padding = unit(0.1, "lines"),
               show.legend = FALSE,
               box.padding = 1,   # ajuste este valor conforme necessário
               point.padding = 1) +
    geom_label_repel(data = estatisticas_minerios, aes(x = moda, y = 0.05, label = paste("Moda/Mode:", round(moda, 0))), fill = "white",
               fontface = "bold",
               label.size = NA,
               label.padding = unit(0.1, "lines"),
               show.legend = FALSE,
               box.padding = 3,   # ajuste este valor conforme necessário
               point.padding = 3) +
    geom_label_repel(data = estatisticas_minerios, aes(x = percentil90, y = 0.05, label = paste("P90:", round(percentil90, 0))), fill = "white",
               fontface = "bold",
               label.size = NA,
               label.padding = unit(0.1, "lines"),
               show.legend = FALSE,
               box.padding = 5,   # ajuste este valor conforme necessário
               point.padding = 5) +
    labs(title = "Histograma Suavizado da Quantidade de Minérios Usados",
         x = "Quantidade de Minério",
         y = "Densidade") +
    theme_minimal() +
    facet_wrap(~ minerio, scales = "free", ncol = 1)
    # Use facet_wrap para criar uma matriz de gráficos
  # scale_x_continuous(breaks = seq(0, xmax_limit, by = 5), limits = c(0, xmax_limit))
  
  ggsave(paste0("Gasto de Minérios - ", nivel, " Refino ", nivel_refino, ".PNG"), plot = minerio_plot, width = 9, height = 9, bg = 'white')
  
  # Calcular estatísticas para os gastos em zeny
  estatisticas_zeny <- summarise(zeny_gasto,
                                 media = mean(`Preço Total`),
                                 moda = Mode(`Preço Total`),
                                 percentil90 = quantile(`Preço Total`, 0.9),
                                 percentil95 = quantile(`Preço Total`, 0.95)
  )
  
  # Definir o limite do eixo X para o percentil 95
  xmax_limit_zeny <- estatisticas_zeny$percentil95
  
  zeny_plot <- ggplot(zeny_gasto, aes(x = `Preço Total`)) +
    geom_density(alpha = 0.5, fill = "blue") +  # Escolha uma cor de preenchimento
    xlim(0, xmax_limit_zeny) +
    geom_vline(data = estatisticas_zeny, aes(xintercept = media), linetype = "dashed", size = 1, show.legend = FALSE) +
    geom_vline(data = estatisticas_zeny, aes(xintercept = moda), linetype = "dotted", size = 1, show.legend = FALSE) +
    geom_vline(data = estatisticas_zeny, aes(xintercept = percentil90), linetype = "solid", size = 1, show.legend = FALSE) +
    geom_label(data = estatisticas_zeny, aes(x = media, y = 0, label = paste("Média/Mean:", scales::dollar(round(media, 0)))), fill = "white", vjust = -30, fontface = "bold", label.size = NA, label.padding = unit(0, "lines"), show.legend = FALSE) +
    geom_label(data = estatisticas_zeny, aes(x = moda, y = 0, label = paste("Moda/Mode:", scales::dollar(round(moda, 0)))), fill = "white", vjust = -32, fontface = "bold", label.size = NA, label.padding = unit(0, "lines"), show.legend = FALSE) +
    geom_label(data = estatisticas_zeny, aes(x = percentil90, y = 0, label = paste("P90:", scales::dollar(round(percentil90, 0)))), fill = "white", vjust = -34, fontface = "bold", label.size = NA, label.padding = unit(0, "lines")) +
    labs(title = "Histograma Suavizado da Quantidade de Zeny Gasta\nSmooth Histogram of Spent Zeny",
         x = "Zeny",
         y = "Densidade\nDensity") +
    theme_minimal()

  
  ggsave(paste0("Gasto de Zeny - ", nivel, " Refino ", nivel_refino, ".PNG"), plot = zeny_plot, width = 9, height = 9, bg = 'white')
}


