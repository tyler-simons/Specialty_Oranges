# Descriptive Analysis Data Analysis

# Call libraries
library(tidyverse)
library(readxl)

# Import the Data

# DA Data of commerical samples that the consumers also tasted
CommercialDAdata <- read_csv("../DA Data/CommercialDAdata.csv")

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
psuedo_mixed_model(BloodOrangesFull)

