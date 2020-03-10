# Consumer ~ Sensory PLS1
library(readxl)
library(tidyverse)
library(plsdepot)
library(FactoMineR)
library(ggrepel)
library(ggforce)

da_cons_pls <- read_excel("FILEPATH")
pls_df <- as.data.frame(da_cons_pls) %>% column_to_rownames("Product")

gplot_pls1 <- function (predictors, response) {
  ### Make a PLS1 and plot it nicely with train tracks. 
  ### Common usage: gplot_pls1(pls_df[,14:32], pls_df[34])
  ### Where the first argument is range of variables and the last one is the response column

  library(readxl)
  library(tidyverse)
  library(plsdepot)
  library(FactoMineR)
  library(ggrepel)
  library(ggforce)
  
  # Make the PLS
  pls_1 <- plsreg1(predictors, response, crosval = F)

  # Name the response and create the tibble for plotting based on the PLS1
  y_name = colnames(response)
  names = c( rep("attribute", length(predictors)), "value")
  points <- pls_1$cor.xyt %>% as.tibble(rownames = "attributes") %>% 
    rename(x = t1, y = t2) %>% mutate (x0 = 0 , y0 = 0, type = names)
  points[nrow(points),]$attributes <- y_name
  
  # Extract the R2 values
  r2_lab <- paste0("R2 Cum = ",round (pls_1$R2[1], 2 ), ", ", round (pls_1$R2[1], 2 ) + round (pls_1$R2[2], 2 ) )
  
  # Plot nicely
  plot <- points %>% ggplot(aes(xend = x, yend = y, x=x0, y= y0, color = type, label = attributes )) + 
      geom_segment(arrow = arrow(length = unit(0.1, "inches"), type = "open"), alpha = 0.5) + 
      geom_text_repel(aes(x = x, y = y)) + 
      theme_minimal() + scale_colour_manual(values = c("black", "red")) + 
      geom_circle(aes(x0=0, y0=0, r = 1),inherit.aes = F ) +
      geom_circle(aes(x0=0, y0=0, r = 0.7),inherit.aes = F ) + 
      theme(legend.position = "none") + 
      geom_text(aes(x=-.75, y=-1, label = r2_lab), color = "black", inherit.aes = F, alpha = 0.1, cex = 3) +
      labs (x = "t1", y = "t2")
  
  return (plot) 
  }

gplot_pls1(pls_df[,14:32], pls_df[34]) %>% 
  ggsave(filename = "Adult_Cluster1_DApls1.jpg", path = "FILEPATH", width = 7)
gplot_pls1(pls_df[,14:32], pls_df[35]) %>% 
  ggsave(filename = "Adult_Cluster2_DApls1.jpg", path = "FILEPATH", width = 7)
gplot_pls1(pls_df[,14:32], pls_df[36]) %>% 
  ggsave(filename = "Child_Cluster1_DApls1.jpg", path = "FILEPATH", width = 7)
gplot_pls1(pls_df[,14:32], pls_df[37]) %>% 
  ggsave(filename = "Child_Cluster2_DApls1.jpg", path = "FILEPATH", width = 7)
gplot_pls1(pls_df[,14:32], pls_df[38]) %>% 
  ggsave(filename = "Child_Cluster3_DApls1.jpg", path = "FILEPATH", width = 7)

