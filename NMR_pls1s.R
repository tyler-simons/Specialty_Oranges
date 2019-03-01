#NMR PLS1s

library(tidyverse)
library(readxl)
library(clipr)

setwd("~/Desktop/Manuscripts/Year 3/Specialty Mandarins")

da_full_mandarins <- read_xlsx(path = "../FullYr3DA.xlsx")
NMR_mandarins <- read_excel("./NMR_Mandarins_Yr3.xlsx")


head(da_full_mandarins)
head(NMR_mandarins)

da_means <- da_full_mandarins %>% group_by(ProductName) %>% summarise_all(mean) %>% select (-CJ, -ProductName2, -NR)
NMR_means <- NMR_mandarins %>% group_by(ProductName) %>% summarize_all(mean) %>% select (-c(`Blinding Code`:`DSS-d6 Lot Number`))
NMR_means %>% View()
summary(NMR_means)
full_data <- inner_join(da_means, NMR_means, "ProductName")
full_data <- as.data.frame(full_data)
rownames(full_data) <- full_data[,1]
full_data[,1] <- NULL



library(plsdepot)

pls1 <- full_data %>% select (`2-Oxoglutarate`:`quinic acid 5`, FruityF) %>% scale
pls_test <- plsreg1(pls1[,1:ncol(pls1)-1], pls1[,ncol(pls1)])
plot(pls_test)
pls_test$Q2

just_nmr <- pls1[,1:ncol(pls1)-1] 
clusts <- just_nmr %>% scale %>% t %>% dist %>% hclust(method = "complete") %>% cutree (12) 

just_nmr %>% scale %>% t %>% dist %>% hclust(method = "complete") %>% plot
just_nmr %>% scale %>% t %>% dist %>% hclust(method = "complete") %>% rect.hclust(k = 12)

just_nmr %>% t %>% mutate (clusters = as.vector(clusts))
clustered_means <- as.tibble ( cbind ( t(just_nmr %>% scale), clusts), rownames = "Compound" ) %>% group_by(clusts) %>%
  summarize_all(mean) %>% select (-Compound, -clusts) 

cm <- clustered_means %>% t()
colnames(cm) <- c(paste0("Cluster", c(1:12)))

pls_test <- plsreg1(cm, full_data %>% select (GrassyF) %>% scale)
plot(pls_test)
pls_test$Q2

write_clip(as.data.frame(clusts))
write_clip(as.data.frame(names(clusts)))

library(randomForest)
rf_data <- as.data.frame(cbind ( cm, full_data %>% select (Sweetness) %>% scale))

rf_obj <- randomForest(Sweetness ~ ., data=rf_data, ntrees = 10000, importance = TRUE)



# Unclustered RF
pls1 <- full_data %>% select (`2-Oxoglutarate`:`quinic acid 5`, Sourness) %>% scale
pls_test <- cbind(pls1[,1:ncol(pls1)-1], FruityF = pls1[,ncol(pls1)])
colnames(pls_test) <- make.names(colnames(pls_test))

randomForest(FruityF ~ ., data=pls_test, ntrees = 500, importance = TRUE)

library(FactoMineR)
PCA(just_nmr)
