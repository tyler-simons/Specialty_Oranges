# Chemical analysis
library(plsdepot)
library(tidyverse)


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

blood_full_chemical <- read_csv("~/Desktop/Manuscripts/Year 3/Florida Data/Cleaned_Data/blood_full_chemical.csv",
                                na = "NA")

# Do the LSD test
blood_orange_full <- blood_full_chemical %>% select (-rep, -type) %>% 
  select_if(function(x){!any (is.na(x))}) %>% single_factor_lsd() 

# Spread the lsd
spread_flor <- blood_orange_full %>% select (-groups) %>%
  mutate (concentration = as.numeric(concentration)) %>%
            spread(compound, concentration)


# Import the NMR data to make the full chemical data
Blood_Orange_NMR <- read_csv("~/Desktop/Manuscripts/Year 3/Blood Oranges/Data/Blood_Orange_NMR.csv")

# LSD on NMR data
nmr_bo <- Blood_Orange_NMR %>% select (X4, `2-Oxoglutarate`:`quinic acid 5`) %>% rename(Product = X4) %>%
  mutate_at(vars(`2-Oxoglutarate`:`quinic acid 5`), as.numeric) %>% head(-2) %>% single_factor_lsd()

# Make full chemical data
full_chem <- nmr_bo %>% select (-groups) %>% spread(compound, concentration) %>% left_join(spread_flor, by = "Product")
full_chem_df <- as.data.frame(full_chem) 
rownames(full_chem_df) <- full_chem_df[,1]
full_chem_df[,1] <- NULL

# Get the chemical clusters
clusters <- scale(full_chem_df) %>% t() %>% dist() %>% hclust %>% cutree(12)

# Get the group means and scale the data
chem_clusters <- full_chem_df %>% scale() %>% t() %>% cbind(cluster = clusters) %>% 
  as.tibble(rownames = "compound") %>% group_by(cluster) %>% 
  summarise_at(vars(-compound, -cluster), mean)

# Make the dataframe for the PLS1
chem_clust_df <- chem_clusters %>% select (-cluster) %>% t()
colnames(chem_clust_df) <- paste0("ChemClust", 1:12)

# PLS1s
# import the DA data
da_sig <- read_excel("~/Desktop/Manuscripts/Year 3/Blood Oranges/Data/da_sig.xlsx")

chem_da <- chem_clust_df %>% as.tibble(rownames="Product") %>% left_join(da_sig, by = "Product") %>%
  as.data.frame() %>% column_to_rownames("Product")

lapply ( 13:ncol(chem_da), function (x){
  pls_reg <- plsreg1(chem_da[,1:12], chem_da[,x])
  names(pls_reg) <- colnames(chem_da)[x]
  ggsave(paste0(names(pls_reg),"pls1.jpg"), plot(pls_reg, main = names(pls_reg)),
         device = "jpg", width = 7, 
         path = "~/Desktop/Manuscripts/Year 3/Blood Oranges/Manuscript/Figures/ChemDAPLS1s/")
  })

for (x in 13:ncol(chem_da)) { 
  pls_reg <- plsreg1(chem_da[,1:12], chem_da[,x])
  plot_name <- paste0(colnames(chem_da)[x],"pls1.jpg")
  jpeg(plot_name, quality = 150)
  plot(pls_reg, main = colnames(chem_da)[x])
  dev.off()

}



# Sort the chemical clusters for viewing
clusters %>% sort() %>% as.data.frame()


for (x in 13:ncol(chem_da)) { 
  pls_reg <- plsreg1(chem_da[,1:12], chem_da[,x])
  print (colnames(chem_da)[x])
  print (pls_reg$Q2)
  
}

# # non-parameteric tests if required by reviewers
# kruskal.test(blood_full_chemical$Nomilin ~ as.factor(blood_full_chemical$sample) )
# shapiro.test
# install.packages("dunn.test")
# library(dunn.test)
# dt <- dunn.test(blood_full_chemical$Limonin, as.factor(blood_full_chemical$sample))
# dt$P.adjusted
# attach(airquality)
