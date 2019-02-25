# Descriptive Analysis Data Analysis

# Call libraries
library(tidyverse)
library(readxl)
library(FactoMineR)
library(reshape2)
library(ggrepel)

# Helper functions
set_rn <- function (df){
  df <- as.data.frame(df)
  rownames(df) <- df[,1]
  df[,1] <- NULL
  return (df)
}

# Import the Data

# DA Data of commerical samples that the consumers also tasted
CommercialDAdata <- read_csv("../DA Data/CommercialDAdata.csv")
ncol(CommercialDAdata) -3
# Full DA data including Riverside fruit
BloodOrangesFull <- read_excel("../DA Data/BloodOrangesFull.xlsx") 

# MANOVA function
MANOVA_da <- function (da){
  
  da <- as.data.frame(da)
  
  ## Set the factors
  da$NR <- as.factor (da$NR)
  da$CJ <- as.factor (da$CJ)
  da$ProductName <- as.factor (da$ProductName)
  
  ### MANOVA ###
  
  # Define the variables in their own matrix
  attributes <- as.matrix ( da [,4:ncol(da)] )
  
  # Perform the MANOVA
  manova.sum <- summary ( manova (attributes ~ (CJ + ProductName + NR)^2, data = da ), test = "Wilks" )
  
  return ( manova.sum ) 
}

# Psuedomixed model for descriptive as described by Hildegarde Heymann
psuedo_mixed_model <- function (da){
  ### Input: Dataframe with the first 3 columns as:
  ### 1. Colname: CJ | Judge Name
  ### 2. Colname: NR | Replication
  ### 3. Colname: ProductName | Name of the product they evaluated
  
  ### Output: Dataframe with not-significant columns removed
  
  # Test to make sure the columns are correct
  first_three_cols <- colnames(da)[1:3]
  required_first_three <- c("CJ", "NR", "ProductName")
  if (all (!first_three_cols == required_first_three)) return ( stop("Wrong Column Order in given data") ) 
  
  # Set as a data.frame
  da <- as.data.frame(da)

  ## Set the factors
  da$NR <- as.factor (da$NR)
  da$CJ <- as.factor (da$CJ)
  da$ProductName <- as.factor (da$ProductName)
  
  ### MANOVA ###
  
  # Define the variables in their own matrix
  attributes <- as.matrix ( da [,4:ncol(da)] )
  
  ### ANOVA ###
  
  data.list0 <- lapply ( 4:ncol(da),  function ( x) { ( aov ( da[[x]] ~ ( ProductName + CJ + NR) ^ 2, data = da ) ) } )
  names ( data.list0 ) <- colnames(da)[4:ncol(da)]
  
  # Get summaries of each of the ANOVA values
  data.list <- lapply ( data.list0, summary)
  names ( data.list ) <- colnames(da)[4:ncol(da)]
  
  ## Psuedo-mixed modeling ##
  
  #Required F value for signifiance
  # This takes the degrees of freedom for each of the interactions and compares
  # it to the df error. 
  
  df.prod <- length ( unique(da$ProductName) ) - 1
  df.pj <- (length ( unique(da$ProductName) ) - 1) * (length ( unique(da$CJ) ) - 1)
  df.pr <- (length ( unique(da$ProductName) ) - 1) * (length ( unique(da$NR) ) - 1)
  df.err <- (length ( unique(da$ProductName) ) - 1) * (length ( unique(da$NR) ) - 1) * (length ( unique(da$CJ) ) - 1)
  
  req.pxj.fvalue <- qf(0.95, df.pj, df.err)
  req.pxr.fvalue <- qf(0.95, df.pr, df.err)
  req.product.fvalue <- qf(0.95, df.prod, df.err)
  
  # Find where the product is significant
  p.fvalues <- sapply ( 1:length(data.list), function (x){  unlist ( data.list[[x]] )["F value1"] } )
  pj.fvalues <- sapply ( 1:length(data.list), function (x){  unlist ( data.list[[x]] )["F value4"] } )
  pr.fvalues <- sapply ( 1:length(data.list), function (x){  unlist ( data.list[[x]] )["F value5"] } )

  # Create an F table of the values
  f.table <- data.frame ( "Product F Value" = p.fvalues, 
                          "Product x Judge F Value" = pj.fvalues, 
                          "Product x Rep F Value" = pr.fvalues)                                      
  
  # Rename the f.table with the correct column names
  rownames ( f.table ) <- colnames(da)[4:ncol(da)]

  # Psuedomixed model to remove not significant attributes
  prod.sig <- f.table [ which ( f.table[,1] > req.product.fvalue ), ]
  pxj.sig <-prod.sig[ which ( ( prod.sig[,1] /prod.sig[,2]) > req.product.fvalue), ] 
  pxr.sig <-pxj.sig[ which ( ( pxj.sig[,1] /pxj.sig[,3])  > req.product.fvalue), ] 
  da.sig <- data.frame ( da[,1:3], da[,rownames(pxr.sig)] )

  return (da.sig)
  }

# Select the pseudomixed attributes
pm_model <- psuedo_mixed_model(BloodOrangesFull)

# Make plots for the distributions of significant variables
hist_plots <- lapply ( colnames(pm_model)[4:ncol(pm_model)], function (x){hist(pm_model[x] %>% pull, 
                                                                 xlab = "Rating", 
                                                                 main = colnames(pm_model[x]))})

# Intake a column and output a ggsave object
save_hist <- function (x){
  return ( ggsave ( filename = paste0(colnames(pm_model)[x+3], ".jpg"),
                      plot = plot(hist_plots[[x]], 
                      xlab = "Rating", main = colnames(pm_model)[4:ncol(pm_model)][x]), 
                      path ="../Manuscript/Figures/DA Hists/") ) } 

lapply (1:length(hist_plots), save_hist)

# Create the means table
means_df <- pm_model %>% group_by(ProductName) %>% summarise_all(mean) %>% select (-CJ, -NR) 

cor (means_df[2:ncol(means_df)]) %>% copy.clipboard()

# PCA function to plot the means tables
function.pca = function (means.table, name, dim1, dim2, products.scale, zoom = 1) {
  
  
  da.pca = PCA(means.table, scale.unit= FALSE, graph = FALSE)
  
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

# All fruit PCA
da_pca_full <- function.pca(means_df, "Specialty Oranges DA PCA", 1, 2, 5, 0.65)
ggsave("DA_PCA_all.jpg",da_pca_full, device = "jpg", width = 10, path = "~/Desktop/Manuscripts/Year 3/Blood Oranges/Manuscript/Figures")

# Commerical Samples only that were tasted by consumers
da_pca_comm <- function.pca(means_df[c(1,4,5,6,7,8,10,13),], "Commercial Specialty Oranges DA PCA", 1, 2, 5, 0.65)
ggsave("DA_PCA_comm.jpg",da_pca_comm, device = "jpg", width = 10, path = "~/Desktop/Manuscripts/Year 3/Blood Oranges/Manuscript/Figures")
