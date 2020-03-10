## Remake consumer code:
library(tidyverse)



# Helper functions
# Function to multi find and replace
copy.clipboard = function (x){
  clip <- pipe("pbcopy", "w")                       
  write.table(cbind (rownames(x), x) , file=clip, sep = '\t', row.names = FALSE)                               
  close(clip)}
replace.function <- function (df, replace.what, replace.with){
  check <-
    replace(df,
            match (as.character (replace.what), df),
            replace.with)
  return (check)
}
replace.long.fun <- function (df, replace.what, replace.with) {
  for (i in 1:length(unique(df))){
    df <- gsub (replace.what[i], replace.with[i], df)
  }
  return (df)
}

library(tidyr)
library(reshape2)
library (RVAideMemoire)
library(dplyr)
library(agricolae)

# Get file
# convert to data frame
# extract factors and overall liking
# Impute data by filling in with overall rounded mean
# remove any consumers where sd is 0 
# scale, dist, hclust, cut tree
# Print a validation statistic to ensure differences
# return dataframe that is identical to the full one with the cluster column appended to the factors
# DA


imputed.function <- function (data){
  # This function splits the data up into matrices 
  # in order to take the row sums and fill with the 
  # rounded value. You can only have two factors with your data starting
  # in the third column
  
  # First rename overall opinion to overall liking:
  if ("Overall Opinion" %in% names (data)){
    names ( data )  [which ( names (data) == "Overall Opinion" )] <- "Overall Liking" 
  }
  
  
  
  
  # Reorganize to create a cons x code matrix
  
  
  imputed.matrix = function ( x, df ){ 
    df <- as.data.frame (df)
    mat = colnames(df)[x]
    one.col <- df[,c(1,2,which (colnames(df) == mat))]
    matrix <- suppressMessages( dcast(one.col, one.col[,2 ] ~ one.col[,1]) ) 
    
    # Rename the rows and remove the codes
    rownames(matrix) = matrix[,1]
    matrix2 = matrix[,-1]
    
    # Fill the missing values with the row means
    ind <- which(is.na(matrix2), arr.ind = TRUE)
    if ( length (ind) != 0) {
      matrix2[ind] <- round ( rowMeans(matrix2, na.rm = TRUE) [ind[,1]] )
    }
    matrix2["names"] <- rownames (matrix2)
    melted <- suppressMessages( melt (matrix2, value.var = .  )  )
    names(melted)[1:2] <- c("Product", "Consumer")
    names(melted)[3] <- mat
    melted <- melted[,c(2,1,3)] 
    return(melted)
  }
  full.imputed <- Reduce(merge , lapply (3:ncol(data), imputed.matrix, df = data) )
  return (full.imputed ) 
}

remove_0sd_cons  <- function (imputed_dataset){
  # This function removes consumers that have 0 
  
  # Navel2017Adults <- read_csv("FILEPATH")
  # Should be used with the imputed function
  # Example: Navel2017Adults %>% imputed.function() %>%
  # remove_0sd_cons()
  
  imputed <- imputed.function(imputed_dataset)
  if ("Overall Opinion" %in% names (imputed_dataset)){
    split_by_cons <- imputed %>% select(Consumer, Product, `Overall Opinion`) %>% split(imputed$Consumer)
  } else if (("Overall Liking" %in% names (imputed_dataset))){
    split_by_cons <- imputed %>% select(Consumer, Product, `Overall Liking`) %>% split(imputed$Consumer)
  }
  bad.cons.list <- strsplit( names ( which ( sapply ( sapply (split_by_cons, "[", 3 ), sd )  == 0 ) ), "\\." )
  removed.bad.cons <- imputed_dataset[- which (imputed_dataset$Consumer %in% sapply (bad.cons.list, "[",1 )),]
  if (nrow(removed.bad.cons) == 0) {return (imputed_dataset)}
  return (removed.bad.cons) }

cluster.consumers <- function (cleaned.data, num.clusters)   {
  # This function returns a complete datafile set with clusters
  # It starts with the cleaned.data file from above
  
  
  
  
  if ( "Overall Opinion" %in% names(cleaned.data)  ) {
    only.liking <- cleaned.data[,c(1,2,which(names(cleaned.data) == "Overall Opinion"))]
  } else if ( "Overall Liking" %in% names(cleaned.data)  ) {
    only.liking <- cleaned.data[,c(1,2,which(names(cleaned.data) == "Overall Liking"))]
  } else {
    print ("Error. Overall liking column not found")
    return()
  }
  
  casted <- dcast (only.liking, Product ~ Consumer)
  rownames ( casted ) <- casted [,1] 
  casted [,1] <- NULL
  clustered <- ( hclust ( dist ( t ( scale (casted) ) ), method = "ward.D2" ) )
  plot (clustered)
  cut <- as.data.frame ( cutree (clustered, k = num.clusters) ) 
  colnames(cut) = "Cluster"
  cleaned.data$Consumer <- as.character(cleaned.data$Consumer)
  return ( merge ( cleaned.data, cut, by.x = "Consumer", by.y = "row.names"  ) )
}

anova.validation <- function (clustered.data) {
  # This function prints the LSD values of clustered data
  my.anova <- aov (clustered.data$`Overall Liking` ~ (clustered.data$Product + clustered.data$Cluster)^2)
  my.anova$model$pxjinteraction = with(clustered.data, interaction(Cluster, Product))
  lsd.int <- LSD.test(my.anova, "pxjinteraction")
  print (summary (my.anova))
  print (lsd.int$groups )
  return ( )
}

cons.meanstable <- function (clustered.data, attribute, type, round.dec = 2, by = "Product"){
  # clustered.data = Navel_Kids_Master
  # attribute = "Overall Liking"
  # type = "both"
  # by == "Product, Cluster, or full"
  # This splits the anova to compare between products (did one product do better by group)
  # cluster = how the distribution between products, full = comparison between all consumers
  if (max (clustered.data$`Overall Liking`) == 9) {
    adults.kids = "Adults"
  } else {
    adults.kids = "Children"
  }
  prod.col <- which (names (clustered.data) == "Product")
  cluster.col <- which (names (clustered.data) == "Cluster")
  consumer.table <- cbind ( clustered.data[,c(prod.col, cluster.col)], 
                            clustered.data[,-c(1,prod.col,cluster.col)] )
  
  consumer.table <- consumer.table[,c(1,2, which (names ( consumer.table) == attribute) )]
  
  if (by == "Product"){
    cons.split <- split(consumer.table, consumer.table$Product)
    lsd.func <- function (x, type, round.dec) {
      my.aov <- aov ( cons.split[[x]]$`Overall Liking` ~ (cons.split[[x]]$Cluster) )
      my.lsd <- LSD.test(my.aov, trt = "cons.split[[x]]$Cluster")  
      if (type == "letts"){ return ( paste0( my.lsd$groups[,2])[order(rownames(my.lsd$groups))] ) }
      if (type == "nums") {return ( round (my.lsd$groups[,1], round.dec ) [order(rownames(my.lsd$groups))] ) }
      if (type == "both") { return ( paste0( round (my.lsd$groups[,1], round.dec ), my.lsd$groups[,2]) ) [order(rownames(my.lsd$groups))] }
    }
    my.df <- data.frame(matrix(0,ncol=length(unique(consumer.table$Product)), nrow = length(unique(consumer.table$Cluster))))
    for (i in 1:length(cons.split)){
      my.df[,i] <- ( lsd.func(i, type = type,round.dec = round.dec) )
    }
    colnames (my.df) <- names(cons.split)
    rownames(my.df) <-  paste0 ("Cluster ", 
                                unique (consumer.table$Cluster), 
                                " (n =", 
                                table(consumer.table$Cluster) / length(unique(consumer.table$Product)),
                                ")")
    return(my.df) 
  } else if (by == "Cluster"){
    cons.split <- split(consumer.table, consumer.table$Cluster)
    
    lsd.func <- function (x, type, round.dec) {
      my.aov <- aov ( cons.split[[x]]$`Overall Liking` ~ (cons.split[[x]]$Product) )
      my.lsd <- LSD.test(my.aov, trt = "cons.split[[x]]$Product")  
      if (type == "letts"){ return ( paste0( my.lsd$groups[,2]) [order(rownames(my.lsd$groups))] ) }
      if (type == "nums") {return ( round (my.lsd$groups[,1], round.dec ) [order(rownames(my.lsd$groups))] ) }
      if (type == "both") { return ( paste0( round (my.lsd$groups[,1], round.dec ), my.lsd$groups[,2]) [order(rownames(my.lsd$groups))] ) }
    }
    my.df <- data.frame(matrix(0,ncol= length(unique(consumer.table$Cluster)), nrow = length(unique(consumer.table$Product))))
    for (i in 1:length(cons.split)){
      my.df[,i] <- ( lsd.func(i, type = type,round.dec = round.dec) )
    }
    rownames (my.df) <- (unique(consumer.table$Product))
    colnames(my.df) <-  paste0 ("Cluster ", 
                                unique (consumer.table$Cluster), 
                                " (n =", 
                                table(consumer.table$Cluster) / length(unique(consumer.table$Product)),
                                ")")
    return(my.df) 
  } else {
    lsd.func <- function (x, type, round.dec) {
      val = as.matrix (clustered.data[attribute])
      fac = as.matrix(clustered.data[,"Product"])
      my.aov <- aov ( val ~ fac )
      my.lsd <- LSD.test(my.aov, trt = "fac")  
      if (type == "letts"){ return ( paste0( my.lsd$groups[,2])[order(rownames(my.lsd$groups))] ) }
      if (type == "nums") {return ( round (my.lsd$groups[,1], round.dec ) [order(rownames(my.lsd$groups))] ) }
      if (type == "both") { return ( paste0( round (my.lsd$groups[,1], round.dec ), my.lsd$groups[,2]) [order(rownames(my.lsd$groups))] )}
    }
    my.df = as.data.frame ( lsd.func(1, type, round.dec) )
    colnames (my.df) <- attribute
    # paste ( adults.kids, sprintf ( "(n=%d)", length(clustered.data$Consumer) / length(unique ( clustered.data$Product) ) ) )
    rownames(my.df) <-  unique ( clustered.data$Product )
    return((my.df)) 
  }
}

jar.percents <- function (attribute, clustered.data){
  jar.attribute = clustered.data[,which( names (clustered.data) == attribute)]
  jar.table <- table (clustered.data$Product, jar.attribute)
  table_sums <- cbind ("Too little" = jar.table[,1] + jar.table[,2], JAR =
                         jar.table[,3], "Too much" = jar.table[4] + jar.table[,5])
  jar.table <- as.data.frame ( t ( round ( table_sums / rowSums (table_sums), 2 ) )  )
  jar.table$attribute <- rep ( attribute, 3)
  return (jar.table)
}

cata.table <- function (data, start.cata, end.cata){
  # data = clustered.data
  # start.cata = 11
  # end.cata = 24
  cata.terms <- names(data)[start.cata:end.cata]
  make.jar.table <- function (x){
    
    jar.table <- table (clustered.data[,c("Product", cata.terms[x])])
    return ( ( jar.table / rowSums (jar.table) ) [,2] )
  }
  cata.df <- sapply (1:length(cata.terms), make.jar.table)
  colnames(cata.df) <- cata.terms
  return (t(cata.df) )
}

make.table <- function (data , attribute , x, type, adult.kid ) {
  # This function just performs ANOVA and LSD on your file with regards to cluster
  # Data is a master file, attribute is chosen, x = vector of 1 : cluster, type = nums, std, lettnums, or lett
  # for desired output
  just.one.att <- data[c("Consumer", "Product", attribute, "Cluster")]
  cons.liking.matrix.final <-
    dcast (just.one.att, Consumer + Cluster ~ Product, value.var = attribute)
  
  split.clust <-
    split (cons.liking.matrix.final,
           cons.liking.matrix.final$Cluster)
  clust.single.melt <-
    melt (split.clust[[x]][, -which (colnames (split.clust[[1]]) == "Cluster")], id.vars = "Consumer")
  
  
  # Mean
  mean.single <-
    aggregate (clust.single.melt, list (clust.single.melt$variable), mean)
  
  # Std
  std.single <-
    aggregate (
      clust.single.melt,
      list (clust.single.melt$variable),
      std <- function(x)
        sd(x) / sqrt(length(x))
    )
  
  # LSD Letters
  clust.lsd <-
    LSD.test (aov (clust.single.melt$value ~ clust.single.melt$variable),
              trt = "clust.single.melt$variable")
  
  if (type == "nums") {
    clust.single.df <- as.data.frame (mean.single$value)
  }
  
  
  
  sprintf("%.3f", round(5.2, 3))
  
  
  if (type == "std") {
    clust.single.df <- as.data.frame (std.single$value)
  }
  if (type == "lettnums") {
    clust.single.df <-
      as.data.frame (paste (sprintf("%.2f", round (
        clust.lsd$groups[order(rownames(clust.lsd$groups)), 1], 2
      ))  ,
      clust.lsd$groups[order(rownames(clust.lsd$groups)), 2]))
  }
  if (type == "lett") {
    clust.single.df <-
      as.data.frame  (clust.lsd$groups[order(rownames(clust.lsd$groups)), 2])
  }
  
  rownames(clust.single.df) <-
    rownames (clust.lsd$groups[order(rownames(clust.lsd$groups)), ])
  colnames(clust.single.df) <-
    sprintf ("%s Cluster %s (n=%s)", adult.kid , x, nrow (split.clust[[x]]))
  return (clust.single.df)
}

make.table.overall <- function (data , attribute , type, adult.kid) {
    # This function just performs ANOVA and LSD on your file with regards to cluster
    # Data is a master file, attribute is chosen, x = vector of 1 : cluster, type = nums, std, lettnums, or lett
    # for desired output
    
    just.one.att <- data[c("Consumer", "Product", attribute, "Cluster")]
    cons.liking.matrix.final <-
      dcast (just.one.att, Consumer ~ Product, value.var = attribute)
    cons.melted <-
      melt (cons.liking.matrix.final, id.vars = "Consumer")
    # Mean
    mean.single <- sapply (cons.liking.matrix.final, mean)[-1]
    
    # Std
    std.single <-
      sapply (cons.liking.matrix.final,
              std <- function(x)
                sd(x) / sqrt(length(x))) [-1]
    
    # LSD Letters
    clust.lsd <-
      LSD.test (aov (cons.melted$value ~ cons.melted$variable), trt = "cons.melted$variable")
    
    if (type == "nums") {
      clust.single.df <- as.data.frame (mean.single)
    }
    if (type == "std") {
      clust.single.df <- as.data.frame (std.single)
    }
    if (type == "lettnums") {
      clust.single.df <-
        as.data.frame (paste (sprintf("%.2f", round (
          clust.lsd$groups[order(rownames(clust.lsd$groups)), 1], 2
        ))  ,
        clust.lsd$groups[order(rownames(clust.lsd$groups)), 2]))
    }
    if (type == "lett") {
      clust.single.df <-
        as.data.frame  (clust.lsd$groups[order(rownames(clust.lsd$groups)), 2])
    }
    
    rownames(clust.single.df) <-
      rownames (clust.lsd$groups[order(rownames(clust.lsd$groups)), ])
    colnames(clust.single.df) <- sprintf(attribute)
    return (clust.single.df)
  }

plot.bar.cluster = function (data , x.axis.title = "Title",attribute, letters = T, adj.y = 1.35) {
  means <- do.call (
    "cbind",
    lapply (
      unique (data$Cluster),
      data = data,
      attribute = attribute,
      make.table,
      type = "nums",
      adult.kid = adult.kid
    )
  )
  
  sem <- do.call (
    "cbind",
    lapply (
      unique (data$Cluster),
      data = data,
      attribute = attribute,
      make.table,
      type = "std",
      adult.kid = adult.kid
    )
  )
  
  letts <- do.call (
    "cbind",
    lapply (
      unique (data$Cluster),
      data = data,
      attribute = attribute,
      make.table,
      type = "lett",
      adult.kid = adult.kid
    )
  )
  
  
  
  low = means - sem
  low$prod = rownames(low)
  low2 <- melt (low)
  high = means + sem
  high$prod = rownames(high)
  high2 <- melt (high)
  
  means$prods <- rownames(means)
  means2 <- melt (means)
  
  
  # Plot
  plot <-
    ggplot(data = means2, aes (y = value, x = variable, fill = prods)) +
    geom_col(position = position_dodge(width = NULL)) +
    labs (x = "Cluster",
          y = x.axis.title,
          fill = "Product",
          cex = 8) +
    coord_cartesian(ylim = c(adj.y, max(data[[attribute]]))) +
    geom_errorbar (
      aes(ymin = low2$value, ymax = high2$value),
      width = .2,
      # Width of the error bars
      position = position_dodge(.9)
    ) +
    scale_y_continuous(breaks = 1:9) +
    {
      if (letters)
        geom_text(aes(
          label = unlist (letts),
          y = high2$value,
          vjust = -1
        ),
        position = position_dodge(width = 0.9))
    } +
    theme_bw() + theme(text = element_text(size = 15))
  return (plot)
}

plot.bar.overall = function (data , x.axis.title = "Title",attribute, letters = T, adj.y = 1.35) {
  means <-
    make.table.overall(
      data = data,
      attribute = attribute,
      type = "nums",
      adult.kid = adult.kid
    )
  sem <-
    make.table.overall(
      data = data,
      attribute = attribute,
      type = "std",
      adult.kid = adult.kid
    )
  letts <-
    make.table.overall(
      data = data,
      attribute = attribute,
      type = "lett",
      adult.kid = adult.kid
    )
  
  low = means - sem
  low$prod = rownames(low)
  low2 <- melt (low)
  high = means + sem
  high$prod = rownames(high)
  high2 <- melt (high)
  
  means$prods <- rownames(means)
  means2 <- melt (means)
  
  # Plot
  plot <-
    ggplot(data = means2, aes (y = value, x = variable, fill = prods)) +
    geom_col(position = position_dodge(width = NULL)) +
    labs (
      x = sprintf ("%s (n=%s)", adult.kid, length(unique(data$Consumer))),
      y = x.axis.title,
      fill = "Product",
      cex = 8
    ) +
    coord_cartesian(ylim = c(adj.y, max(data[[attribute]]))) +
    geom_errorbar (
      aes(ymin = low2$value, ymax = high2$value),
      width = .2,
      # Width of the error bars
      position = position_dodge(.9)
    ) +
    scale_y_continuous(breaks = 1:9) +
    {
      if (letters)
        geom_text(aes(
          label = unlist (letts),
          y = high2$value,
          vjust = -1
        ),
        position = position_dodge(width = 0.9))
    } +
    theme_bw() + theme(text = element_text(size = 15), axis.text.x = element_blank())
  return (plot)
}

# Example with Navels

Navel_Key_Names <- read_excel("FILEPATH")
Navel2017Adults <- read_csv("FILEPATH")
Navel2017Adults$Sample <- Navel2017Adults$Sample %>% replace.long.fun(Navel_Key_Names$`Navel Key`, Navel_Key_Names$`Manuscript Names`)

clustered.data <- Navel2017Adults %>% imputed.function() %>%
  remove_0sd_cons() %>% cluster.consumers (num.clusters =  4)

# Get the means tables
clustered.data %>% cons.meanstable (attribute = "Overall Liking", type = "both", by = "Cluster") 

# Plot
clustered.data %>% plot.bar.cluster("Overall Liking", "Overall Liking")


# Specialty Oranges
adults_spec_oranges_named <- read_excel("FILEPATH")

adults_master <- adults_spec_oranges_named %>% imputed.function() %>%
  remove_0sd_cons() %>% cluster.consumers (num.clusters =  2)
# write_csv(adults_master, "FILEPATH")


# LSD Means tables
adults_master %>% cons.meanstable("Overall Liking", "both", by = "Cluster") %>% copy.clipboard()
adults_master %>% make.table("Appearance", "both", "Adult")


# Plot
adult.kid = "Adult"
adults_master %>% plot.bar.cluster("Overall Liking", "Overall Liking", adj.y = 1.29)

adult.kid = "Adults"
adults_master %>% plot.bar.overall("Overall Liking", "Overall Liking", adj.y = 1.29)


# ANOVA table, input a three column df and get out the LSD table
lsd_table <- function (df, attribute){
  # Requires the consumer and product columns be named Consumer and Product
test <- df %>% select (Consumer, Product, attribute)
my_lsd <- aov (test %>% pull(attribute) ~ (Consumer + Product), data = test) %>% LSD.test (trt = "Product")
lsd_groups <- my_lsd$groups[order(rownames(my_lsd$groups)),]
colnames(lsd_groups) <- c(attribute, "groups")
return (lsd_groups)}

get_cleaned_lsd_table <- function (df, attribute){
  df %>% lsd_table(attribute) %>%
  rownames_to_column("Sample") %>% mutate_at(attribute, round, 2) %>% unite_(attribute, c(attribute, "groups"), sep = "")
}

adults_master %>% get_cleaned_lsd_table("Flavor")
do.call ("cbind", lapply ( colnames(adults_master)[c(3,6,7)], get_cleaned_lsd_table, df = adults_master)) %>% copy.clipboard()



# Children
kids_spec_oranges_named <- read_excel("FILEPATH")

kids_clustered <- kids_spec_oranges_named %>% imputed.function() %>% remove_0sd_cons() %>% cluster.consumers(3)
kids_clustered %>% get_cleaned_lsd_table("Texture") %>% copy.clipboard()
kids_clustered %>% select (Appearance, Taste, Texture, `Overall Liking`) %>% cor %>% copy.clipboard()
cor.test(kids_clustered$Appearance, kids_clustered$Texture)

adult.kid = "Children"
kids_plot <- kids_clustered %>% plot.bar.cluster("Overall Liking", "Overall Liking", adj.y = 1.29) 

kids_plot <- kids_clustered %>% plot.bar.overall("Overall Liking", "Overall Liking", adj.y = 1.29) 

#ggsave("Child_Cluster_Liking.jpg", kids_plot, device = "jpg", width = 10)

#write_csv(kids_clustered, "FILEPATH")

