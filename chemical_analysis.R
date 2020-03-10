# Chemical analysis
library(plsdepot)
library(tidyverse)
library(clipr)
library(agricolae)
library(SensoMineR)
library(readxl)

single_factor_lsd <- function (one_factor_tibble){
  
  one_factor_lsd <- function (df){
    ### This function intakes a tibble and outputs a 
    
    
    # perform aov
    perform_aov <- function (x) {
      x <- df %>% pull(x)
      first_col <- df %>% pull(1) %>% as.factor()
      return ( aov (x ~ (first_col), data = df)  )
    }
    is_sig_aov <- function (x){
      summary(aov_list[[x]])[[1]]["first_col","Pr(>F)"] < 0.05
    }
    
    aov_list <- lapply (colnames(df)[-c(1)], perform_aov  )
    sig_list <- lapply (1:length(aov_list), is_sig_aov ) %>% unlist()
    df_sig <- cbind (df[,1], df[,-1][,sig_list] )
    sig_names <- colnames(df_sig)[-1]
    
    get_lsd_groups <- function (aov_list_elem){
      lsd_model <- LSD.test(aov_list[[aov_list_elem]], trt = "first_col" ) 
      return ( lsd_model$groups[order(rownames(lsd_model$groups)),] )
    }

    
        # Make all of the LSD_groups into a list
    lsd_group_list <- lapply (1:length(aov_list),  get_lsd_groups)
    
    # Select only the significant ones
    
    final_list <- lsd_group_list[sig_list]
    names(final_list) <- sig_names
    return (final_list) 
    
  }
  
  
  convert_to_tibble_lsd <- function (z) {sig_lsd_list[[z]] %>% as.tibble(rownames = "Product") %>%
      mutate(compound = names(sig_lsd_list)[z], groups = as.character(groups)) %>% 
      rename(concentration = x)  }
  
  
  
  ### START CODE
  
  sig_lsd_list <- one_factor_lsd(one_factor_tibble)
  
  lsd_tibble <- lapply ( 1:length(sig_lsd_list), convert_to_tibble_lsd) %>%
    bind_rows
  
  return (lsd_tibble)
}

blood_full_chemical <- read_csv("FILEPATH",
                                na = "NA")


## PCA Biplot Function

# This function plots a PCA given a means table with color coated attributes and products, zoom,
# and scale functions to move the products in and out to adjust for the biplot. 

# PCA Charting
library(ggplot2)
library(ggrepel)
attach(pca.points)



# Plot a PCA biplot from a means table
function.pca = function (means.table, name, dim1, dim2, products.scale, zoom = 1) {
  
  
  da.pca = PCA(means.table, scale.unit= TRUE, graph = FALSE)
  
  # Map the attributes
  attribute.points = as.data.frame (   da.pca$var$coord[,c( dim1,dim2 )]  )
  attribute.points[,3:4] = 0
  colnames (attribute.points) <- c("Dim.1", "Dim.2", "Dim.1.0","Dim.2.0")
  
  # Map the products
  product.points = as.data.frame (   da.pca$ind$coord[,c( dim1,dim2 )]  )
  product.points[,3:4] = 0
  product.points <-  product.points * (1/products.scale)
  
  colnames (product.points) <- c("Prod.1", "Prod.2", "Prod.1.0","Prod.2.0")
  
  product.points2 <- product.points
  product.points2[,5] <- "Products"
  
  attribute.points2 <- attribute.points
  attribute.points2[,5] <- "Attributes"
  
  names ( product.points2) <- names ( attribute.points2)
  
  names.matrix <- rbind ( product.points2, attribute.points2)
  names.matrix$V5 <- as.factor (names.matrix$V5)
  
  # % Explained:
  percent.expl <- da.pca$eig[,'percentage of variance']
  
  pca.plot <- ggplot() + 
    geom_point (aes(x=Prod.1,y=Prod.2), 
                data=product.points, 
                alpha = 0.5, 
                color = I("black")) +
    
    geom_text_repel( aes (x=( Dim.1), y=(Dim.2), 
                          color = factor (V5), 
                          label = rownames(names.matrix) ), 
                     size = 4,  
                     alpha = 1, 
                     data = names.matrix) + 
    
    geom_segment(aes (xend=Dim.1,yend=Dim.2, x = 0, y = 0 ), 
                 data = attribute.points, 
                 arrow = arrow(length = unit(0.01, "npc")), 
                 alpha = 0.6 ) + 
    coord_cartesian(xlim = c(-2/zoom,2/zoom), ylim=c(-1/zoom,1/zoom)) +
    theme_minimal () +
    labs(title = name, 
         x = sprintf("PC%d (%s%%) ", dim1, round ( percent.expl[dim1],2)), 
         y= sprintf("PC%d (%s%%) ",dim2, round ( percent.expl[dim2],2)),
         color = "") + 
    theme(plot.title = element_text(hjust = 0.5), 
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank() ) + 
    geom_vline(xintercept = 0, alpha = 0.5) + geom_hline(yintercept  = 0, alpha = 0.5)
  return (pca.plot)
}


# Do the LSD test
blood_orange_full <- blood_full_chemical %>% select (-rep, -type) %>% 
  select_if(function(x){!any (is.na(x))}) %>% single_factor_lsd() 

# Spread the lsd
spread_flor <- blood_orange_full %>% select (-groups) %>%
  mutate (concentration = as.numeric(concentration)) %>%
            spread(compound, concentration)



# Make full chemical data
# full_chem <- nmr_bo %>% select (-groups) %>% spread(compound, concentration) %>% left_join(spread_flor, by = "Product")
# full_chem_df <- as.data.frame(full_chem) 
# rownames(full_chem_df) <- full_chem_df[,1]
# full_chem_df[,1] <- NULL

# Import the NMR data to make the full chemical data
Blood_Orange_NMR <- read_csv("FILEPATH")

# LSD on NMR data
nmr_bo <- Blood_Orange_NMR %>% select (X4, `2-oxoglutarate`:`quinic acid`) %>% rename(Product = X4) %>%
  mutate_at(vars(`2-oxoglutarate`:`quinic acid`), as.numeric) %>% head(-2) %>% single_factor_lsd()

nmr_means <- nmr_bo %>% select (-groups) %>% spread(compound, concentration) %>% 
  as.data.frame() %>% column_to_rownames("Product")

function.pca(nmr_means, name = "Specialty Orange Chemicals", 1, 2, 8, 1.2) %>%
  ggsave(file = "spec_orange_nmr_12.jpg", path="FILEPATH", width = 12)


# Get the chemical clusters
clusters <- scale(nmr_means) %>% t() %>% dist() %>% hclust %>% cutree(8)

# Get the group means and scale the data
chem_clusters <- nmr_means %>% scale() %>% t() %>% cbind(cluster = clusters) %>% 
  as.tibble(rownames = "compound") %>% group_by(cluster) %>% 
  summarise_at(vars(-compound, -cluster), mean)

# Make the dataframe for the PLS1
chem_clust_df <- chem_clusters %>% select (-cluster) %>% t()
colnames(chem_clust_df) <- paste0("ChemClust", 1:8)

function.pca(chem_clust_df, name = "Specialty Orange Chemical Clusters", 1, 3, 4, 1) %>%
  ggsave(file = "nmr_cluster_PCA13.jpg", path="FILEPATH", width = 12)
function.pca(chem_clust_df, name = "Specialty Orange Chemical Clusters", 1, 2, 4, 1)%>%
  ggsave(file = "nmr_cluster_PCA12.jpg", path=".FILEPATH", width = 12)


# PLS1s
# import the DA data
da_sig <- read_excel("FILEPATH")

chem_da <- chem_clust_df %>% as.tibble(rownames="Product") %>% left_join(da_sig, by = "Product") %>%
  as.data.frame() %>% column_to_rownames("Product")






full_nmr_cons <- nmr_means %>% rownames_to_column ("Product") %>%
inner_join (cons_chem_pls %>% rownames_to_column ("Product") %>% 
              select(Product, AdultCluster1:ChildCluster3)) %>% column_to_rownames("Product")
gplot_pls1(full_nmr_cons[1:36], full_nmr_cons[38])

cons_chem_pls <- cons_chem_pls %>% as.data.frame() %>% column_to_rownames("Product")
for (x in 37:ncol(full_nmr_cons)) { 
  plot_name <- paste0(colnames(full_nmr_cons)[x],"pls1.jpg")
  ggsave ( filename = plot_name, gplot_pls1(full_nmr_cons[,1:36], full_nmr_cons[x]), path = "FILEPATH",
           width = 7)
}
gplot_pls1(cons_chem_pls[,1:12], cons_chem_pls[13])
plsreg1(cons_chem_pls[,1:12], cons_chem_pls[13])



full_nmr_DA <- nmr_means %>% rownames_to_column ("Product") %>%
  inner_join (da_sig) %>% column_to_rownames("Product")



for (x in 37:ncol(full_nmr_DA)) { 
  plot_name <- paste0(colnames(full_nmr_DA)[x],"pls1.jpg")
  ggsave ( filename = plot_name, gplot_pls1(full_nmr_DA[,1:36], full_nmr_DA[x]), path = "FILEPATH",
           width = 7)
}






# Regular plot
for (x in 13:ncol(chem_da)) { 
  pls_reg <- plsreg1(chem_da[,1:12], chem_da[,x])
  plot_name <- paste0(colnames(chem_da)[x],"pls1.jpg")
  q2_label <- paste0("Q2 = ", round (pls_reg$Q2[2,5],2) )
  
  
  jpeg(plot_name, quality = 150)
  gplot_pls1(chem_da[,1:12], chem_da[x])
  plot(pls_reg, main = colnames(chem_da)[x])
  text(x = 0.6, y = -1, labels = q2_label)
  dev.off()
  
}



# # non-parameteric tests if required by reviewers
# kruskal.test(blood_full_chemical$Nomilin ~ as.factor(blood_full_chemical$sample) )
# shapiro.test
# install.packages("dunn.test")
# library(dunn.test)
# dt <- dunn.test(blood_full_chemical$Limonin, as.factor(blood_full_chemical$sample))
# dt$P.adjusted
# attach(airquality)



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
  pls_1 <- plsreg1(predictors, response, crosval = T)
  
  # Name the response and create the tibble for plotting based on the PLS1
  y_name = colnames(response)
  names = c( rep("attribute", length(predictors)), "value")
  points <- pls_1$cor.xyt %>% as.tibble(rownames = "attributes") %>% 
    rename(x = t1, y = t2) %>% mutate (x0 = 0 , y0 = 0, type = names)
  points[nrow(points),]$attributes <- y_name
  
  # Extract the R2 values
  r2_lab <- paste0("R2 Cum = ",round (pls_1$R2[1], 2 ), ", ", round (pls_1$R2[1], 2 ) + round (pls_1$R2[2], 2 ) )
  r2_lab <- paste0("Q2 = ", round (pls_1$Q2[2,5],2) )
  
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




FullYr3DA %>% group_by(ProductName) %>% summarise_all(mean) %>% View()

