## JAR Analysis (Consumers)
kids_clustered <- read_csv("../Data/kids_spec_clustered.csv")
adults_clustered <- read_csv("../Data/adults_spec_clustered.csv")


jar.percents <- function (attribute, clustered.data){
  
  jar.attribute = clustered.data[,which( names (clustered.data) == attribute)]
  if (length(unique(jar.attribute)) == 5){
  jar.table <- table (clustered.data$Product, jar.attribute %>% pull(attribute))
  table_sums <- cbind ("Too little" = jar.table[,1] + jar.table[,2], JAR =
                         jar.table[,3], "Too much" = jar.table[4] + jar.table[,5])
  jar.table <- as.data.frame ( t ( round ( table_sums / rowSums (table_sums), 2 ) )  )
  jar.table$attribute <- rep ( attribute, 3)
  return (jar.table) }
  else {
    jar.table <- table (clustered.data$Product, jar.attribute %>% pull(attribute))
    table_sums <- cbind ("Too little" = jar.table[,1], JAR =
                           jar.table[,2], "Too much" = jar.table[,3])
    jar.table <- as.data.frame ( t ( round ( table_sums / rowSums (table_sums), 2 ) )  )
    jar.table$attribute <- rep ( attribute, 3)
    return (jar.table) 
    
  }
}



# JAR Percentages 
JAR.list <- c("JAR.sweetness", "JAR.sourness", "JAR.size", "JAR.color", "JAR.juiciness")
JAR.list <- colnames(adults_clustered)[c(4,8,9,10,11)]
copy.clipboard ( do.call (rbind, lapply (JAR.list, jar.percents, clustered.data = adults_clustered ) ) )
jar.percents("JAR.sweetness", kids_clustered)
chisq.test ( table ( adults_clustered$Product, adults_clustered$`JAR Size`) )


# JAR attributes by cluster
jar_att_by_cluster <- function (clustered.data, attribute, print.sig = TRUE){
  if (print.sig == TRUE ) {print (chisq.test(table (clustered.data$Cluster, clustered.data[,attribute])))}
  jar.table <- table (clustered.data$Cluster, clustered.data[,attribute]) / rowSums ( table (clustered.data$Cluster, clustered.data[,attribute] ))
  jar.table <- as.data.frame ( cbind ( "Too little" = jar.table[,1] + jar.table[,2], "JAR" = jar.table[,3], "Too much" = jar.table[,4] + jar.table[,5]) )
  jar.table2 <- cbind (round (jar.table,2), attribute)  
  return (jar.table2)
}
do.call ( rbind, lapply (JAR.list, jar_att_by_cluster, clustered.data = clustered.data, print.sig = FALSE)  ) %>% copy.clipboard
chisq.test(table (clustered.data$Cluster, clustered.data$`JAR Sourness`))


jar_penalty <- function (df, attribute){

bound <- bind_cols(
  df %>% filter(get(attribute) > 3) %>% summarise(mean_high = mean(`Overall Liking`)),
  df %>% filter(get(attribute) == 3) %>% summarise(jar= mean(`Overall Liking`)),
  df %>% filter(get(attribute) < 3) %>% summarise(meanlow = mean(`Overall Liking`))
)
return (bound %>% mutate (high_diff = mean_high - jar, 
                          low_diff = meanlow-jar, attribute = attribute) ) }

jar_penalty(adults_clustered, "JAR Sweetness")
jar_penalty(adults_clustered, "JAR Sweetness")
sapply ( colnames(adults_clustered)[c(4,8,9,10,11)], jar_penalty, df = adults_clustered) %>% copy.clipboard()




jar_penalty_kids <- function (df, attribute){
  
  bound <- bind_cols(
    df %>% filter(get(attribute) > 2) %>% summarise(mean_high = mean(`Overall Liking`)),
    df %>% filter(get(attribute) == 2) %>% summarise(jar= mean(`Overall Liking`)),
    df %>% filter(get(attribute) < 2) %>% summarise(meanlow = mean(`Overall Liking`))
  )
  
  return (bound %>% mutate (high_diff = mean_high - jar, 
                            low_diff = meanlow-jar, attribute = attribute) ) }

sapply ( colnames(kids_clustered)[c(4,8,9,10,11)], jar_penalty_kids, df = kids_clustered) %>% copy.clipboard()







# Extra code from before, unused in these analyses


### JAR Analyses ###
library (readr)
library (reshape2)
library(ggplot2)

# Set appropriate working directory


# Create
adults.file <-  Yr1AdultsMandarinsMaster
AdultCons2 <- AdultsYr1MandarinsMaster



{
  vali.table <- t ( as.matrix ( ( table (AdultCons2$Cluster, AdultCons2$JAR.Firmness ) ) ) )
  vali.table / apply (vali.table, 2, sum)
  
  
  cluster.usage <- table(AdultCons2$Cluster, AdultCons2[["JAR.Firmness"]])
  cluster.usage2 <- t ( cluster.usage / apply ( cluster.usage,1,sum) * 100 )
  
  
  
  cluster.split <- split (AdultsYr2MandarinsMaster,  AdultsYr2MandarinsMaster$Cluster )
  aggregate ( cluster.split[[1]], list (cluster.split[[1]]$Code), mean )
}                        



# This code needs AdultCons2 which is the master file, rounded JAR data, and cluster 

jar.vector <- c("JAR Sweetness", "JAR Sourness", "JAR Firmness", "JAR Juiciness" )



######### Adults ##########

# Labels for adults and children
adult.sweet.labels <- c("Much too sweet", "Somewhat too sweet", "Just about right", 
                        "Not quite sweet enough", "Not at all sweet enough")

adult.sour.labels <- c("Much too sour/tart", "Somewhat too sour/tart", "Just about right", 
                       "Not quite sour/tart enough", "Not at all sour/tart enough")

adult.firm.labels <- c("Much too firm", "Somewhat too firm", "Just about right", 
                       "Somewhat too soft", "Much too soft")

adult.juicy.labels <- c("Much too juicy", "Somewhat too juicy", "Just about right", 
                        "Not quite juicy enough", "Not at all juicy enough")

jar.label.vector <-list (  adult.sweet.labels, adult.sour.labels, adult.firm.labels, adult.juicy.labels )





adult.sweet.labels <- c("Too sweet",  "Just about right", 
                        "Not sweet enough")

adult.sour.labels <- c("Too sour",  "Just about right", 
                       "Not sour enough")

adult.firm.labels <- c("Too firm",  "Just about right", 
                       "Not firm enough")

adult.juicy.labels <- c("Too juicy",  "Just about right", 
                        "Not juicy enough")

jar.label.vector <-list (  adult.sweet.labels, adult.sour.labels, adult.firm.labels, adult.juicy.labels )








## Adults JAR by cluster function 
## Works with AdultCons2 file
jar.plot.fun <- function (JAR.att, jar.labels) {
  
  cluster.usage <- table(AdultCons2$Cluster, AdultCons2[[JAR.att]])
  cluster.usage2 <- t ( cluster.usage / apply ( cluster.usage,1,sum) * 100 )
  clust.us3 <- melt ( cluster.usage2)
  test.chi <- chisq.test(cluster.usage)
  clust.us3$Var1 <- 6-clust.us3$Var1
  
  jarplot <- ggplot() + geom_bar(aes(x = Var2, y =   value , fill =   as.factor ( Var1 ) ), data = clust.us3,
                                 stat="identity", alpha = 0.75) +
    scale_fill_grey(labels = jar.labels,
                    name = "") + 
    ylab (label = "% used") + xlab(label = "Cluster") +
    geom_text(aes (y = value, label = paste0( round ( value),"%") , x=Var2 ), size = 3, position = position_stack(vjust = 0.5), data=clust.us3) + 
    #annotate("text", x = Inf, y = Inf, label = sprintf ( "ChiSquare P Value: %s",round ( test.chi$p.value,4 )  ), vjust=1, hjust=1, cex = 3 ) +
    theme_minimal() + theme ( panel.grid.major = element_blank(), panel.grid.minor = element_blank() )
  print ( chisq.test(cluster.usage))
  return ( jarplot )
}

for ( i in 1:4 ){
  jarplot <- jar.plot.fun ( jar.vector[i], jar.label.vector[[i]] )
  ggsave(sprintf ("Figure___Adults%sClusters.jpg", jar.vector[i]), plot = jarplot, device = "jpeg", width = 15) 
}

jar.plot.fun('JAR Sweetness', adult.sweet.labels) 

## Adults JAR all products function 
adult.jar.all.func <- function (AdultCons2, jar.att, adult.jar.labels) {
  table.prods <- table (AdultCons2[[jar.att]], AdultCons2$Code)
  table.prods2 <- table.prods / apply ( table.prods,2, sum) * 100
  melted2 <- melt (table.prods2)
  
  test.chi <- chisq.test(table.prods2)
  
  jarplot <- ggplot() + geom_bar(aes(y = value, x = Var2, fill = as.factor ( -Var1 )), data = melted2,
                                 stat="identity", alpha = 0.75) +
    scale_fill_grey(labels = adult.jar.labels,
                    name = "JAR Rating") + 
    ylab (label = "% used") + xlab(label = "Product") +
    geom_text(aes (y = value, label = paste0( round ( value),"%") , x=Var2 ), size = 3, position = position_stack(vjust = 0.5), data=melted2) + 
    #annotate("text", x = Inf, y = Inf, label = sprintf ( "ChiSquare P Value: %s",round ( test.chi$p.value,4 )  ), vjust=1, hjust=1, cex = 3 ) +
    theme_minimal() + theme ( panel.grid.major = element_blank(), panel.grid.minor = element_blank() )
  return (jarplot)
}

for ( i in 1:4 ){
  jarplot <- adult.jar.all.func (AdultCons2, jar.vector[i], jar.label.vector[[i]] )
  ggsave(sprintf ("Figure___Adults%s.jpg", jar.vector[i]), plot = jarplot, device = "jpeg", width = 15) 
}


adult.jar.all.func(AdultCons2, "JAR Sweetness", adult.sweet.labels)
adult.jar.all.func(AdultCons2, "JAR Sourness", adult.sour.labels)
adult.jar.all.func(AdultCons2, "JAR.Firmness", adult.firm.labels)
adult.jar.all.func(AdultCons2, "JAR.Juiciness", adult.juicy.labels)





##### Kids  #######

kids.file <- KidsYr1MandarinsMaster
KidsCons2 <- kids.file
#KidsCons2$JAR.Juiciness[which ( KidsCons2$JAR.Juiciness == 0 ) ] <- 1
jar.vector <- c("JAR Sweetness", "JAR Sourness", "JAR Firmness", "JAR Juiciness" )

# Kids labels in order

kids.sweet.labels <- c("Too sweet", "Just right", 
                       "Not sweet enough")
kids.sour.labels <-c("Too sour", "Just right", 
                     "Not sour enough")
kids.firm.labels <-c("Too firm", "Just right", 
                     "Too soft")
kids.juicy.labels <-c("Too juicy", "Just right", 
                      "Not juicy enough")
kids.jar.label.vector <- list (  kids.sweet.labels, kids.sour.labels, kids.firm.labels, kids.juicy.labels )


## Kids JAR by cluster function 

jar.plot.fun.kids <- function (JAR.att, jar.labels) {
  
  cluster.usage <- table(KidsCons2$Cluster, KidsCons2[[JAR.att]])
  cluster.usage2 <- t ( cluster.usage / apply ( cluster.usage,1,sum) * 100 )
  clust.us3 <- melt ( cluster.usage2)
  test.chi <- chisq.test(cluster.usage)
  clust.us3$Var1 <- 3-clust.us3$Var1
  
  jarplot <- ggplot() + geom_bar(aes(x = as.factor ( Var2 ), y = value , fill =   as.factor ( Var1 ) ), data = clust.us3,
                                 stat="identity", alpha = 0.75) +
    scale_fill_hue(labels = jar.labels,
                   name = "") + 
    ylab (label = "% used") + xlab(label = "Cluster") +
    geom_text(aes (y = value, label = paste0( round ( value),"%") , x=Var2 ), size = 3, position = position_stack(vjust = 0.5), data=clust.us3) +
    annotate("text", x = Inf, y = Inf, label = sprintf ( "ChiSquare P Value: %s",round ( test.chi$p.value,4 )  ), vjust=1, hjust=1, cex = 3 ) +
    theme_minimal() + theme ( panel.grid.major = element_blank(), panel.grid.minor = element_blank() )
  
  return ( jarplot )
}
jar.plot.fun.kids (jar.vector[1], kids.jar.label.vector[[1]] )

for ( i in 1:4 ){
  jarplot <- jar.plot.fun.kids (jar.vector[i], kids.jar.label.vector[[i]] )
  ggsave(sprintf ("Figure___Kids%sClusters.jpg", jar.vector[i]), plot = jarplot, device = "jpeg", width = 8) 
}


## Kids JAR by product function

kids.jar.all.func <- function (KidsCons2, jar.att, kids.jar.label) {
  table.prods <- table (KidsCons2[[jar.att]], KidsCons2$Code)
  table.prods2 <- table.prods / apply ( table.prods,2, sum) * 100
  melted2 <- melt (table.prods2)
  test.chi <- chisq.test(table.prods2)
  
  jarplot <- ggplot() + geom_bar(aes(y = value, x = as.factor ( Var2) , fill = as.factor ( -Var1 )), data = melted2,
                                 stat="identity", alpha = 0.75) +
    scale_fill_hue(labels = kids.jar.label,
                   name = "JAR Rating") + 
    ylab (label = "% used") + xlab(label = "Product") +
    geom_text(aes (y = value, label = paste0( round ( value),"%") , x=Var2 ), size = 3, position = position_stack(vjust = 0.5), data=melted2) + 
    #annotate("text", x = Inf, y = Inf, label = sprintf ( "ChiSquare P Value: %s",round ( test.chi$p.value,4 )  ), vjust=1, hjust=1, cex = 3 ) +
    theme_minimal() + theme ( panel.grid.major = element_blank(), panel.grid.minor = element_blank() )
  jarplot
  return (jarplot)
}

for ( i in 1:4 ){
  jarplot <- kids.jar.all.func (KidsCons2, jar.vector[i], kids.jar.label.vector[[i]] )
  ggsave(sprintf ("Figure___Kids%sProducts.jpg", jar.vector[i]), plot = jarplot, device = "jpeg", width = 10) 
}

kids.jar.all.func (KidsCons2, jar.vector[1], kids.jar.label.vector[[1]])










##### Penalty Analysis #####
## Penalty Analysis ##

# Round the data files to do penalty analysis cleanly
# Now starting with cluster.merged.full

master.file <- AdultsYr1MandarinsMaster


overall.liking.column <- 6
jar.start <- 9
jar.end <- jar.start + 3
adults.jar.round <- cbind ( master.file[c(1:2, overall.liking.column)], 
                            round(master.file[jar.start:jar.end], 0 ), master.file["Cluster"])

write.csv ( adults.jar.round, file = "adults.jar.round.csv", row.names = FALSE)

kids.jar.round <- cbind ( master.file[c(1:2,overall.liking.column)], round(master.file[jar.start:jar.end], 0 ))
# Fix the 0 in the row 60
kids.jar.round[60,7] <- 1
write.csv ( kids.jar.round, file = "kids.jar.round.csv", row.names = FALSE)




# Take mean by JAR rating

# JAR Sweetness is JAR Attribute
# 7 is the overall liking column

#file <- AdultsYr2MandarinsMaster
#attribute <- "JAR Sweetness"
#liking.col <- 6

## Adults Penalty Analysis
# jar.col.num is the column number that is the first jar
# liking.col is the column used to compare for penalty analysis
# file is the masters file
{
  one.att.penalty <- function ( jar.col.num, file,  liking.col){
    attribute = names(file)[jar.col.num]
    
    too.little <- file[which (( file [,jar.col.num] ) ==  1  |  file [,jar.col.num] ==  2 ),]
    just.right <- file[which (  file [,jar.col.num] ==  3),]
    too.much <- file[which (( file [,jar.col.num] ) ==  4  |  file [,jar.col.num] ==  5 ),]
    
    tl.percent <- nrow ( too.little) / nrow(file) * 100
    jr.percent <- nrow ( just.right) / nrow(file) * 100
    tm.percent <- nrow ( too.much) / nrow(file) * 100
    
    jr.score <- mean ( just.right[,liking.col] )
    
    tl.penalty <- mean ( just.right[,liking.col] ) -  mean ( too.little[,liking.col])
    tm.penalty <- mean ( just.right[,liking.col] ) -  mean ( too.much[,liking.col])
    
    jar.pen.mat <- matrix (data = c( tl.percent, tm.percent, tl.penalty, tm.penalty, attribute, attribute), nrow = 2, ncol = 3, 
                           dimnames = list ( c("Too little", "Too much"), c("% Used", "Penalty", "Attribute") ) )
    jar.pen.mat <- as.data.frame ( jar.pen.mat)
    
    
    overall.by.jar <- aggregate ( file, list (file[[jar.col.num]]), mean) [,4]
    
    # Extract the penalties
    penalty <- overall.by.jar [3] - overall.by.jar
    
    # Remove JAR
    no.no.pen <- penalty [!penalty == 0]
    
    # Get the proportions 
    props <- ( table (file[attribute]) /  length(file[,3]) ) * 100
    # Extract the JAR name
    attribute.name <- tail(strsplit(attribute,split=" ")[[1]],1)
    
    # Add the proportions
    penalty.matrix <- rbind ( "props" = props [c(1,2,4,5)], no.no.pen )
    
    one.attribute <- data.frame ( t(penalty.matrix), "Attribute" = rep (attribute.name, 4) )
    
    # Create the JAR Levels
    jar.levels <- c("LL", "L", "H", "HH")
    penalty.matrix <- cbind (one.attribute, jar.levels )
    
    
    colnames ( penalty.matrix )[1:2] <- c("% Used", "Mean Drop")
    return (penalty.matrix)
  }
  
  one.att.penalty (9, AdultsYr1MandarinsMaster, 6)
  
  clust.jar.split <- split ( adults.jar.round, adults.jar.round$Cluster )
  
  jar.list.adults <- lapply (4:7, one.att.penalty, file = clust.jar.split[[3]], liking.col = 3 )
  pen.table.adults <- do.call ( rbind, jar.list.adults)
  pen.table.adults$category <- paste0 ( pen.table.adults[,4], pen.table.adults[,3] )
  
  shapes = c( 5:8 )
  
  adults.penalty.plot <- ggplot () +
    geom_point(data = pen.table.adults, aes (x = pen.table.adults$`% Used`, y = pen.table.adults$`Mean Drop`, shape = Attribute)) +
    geom_text_repel (aes(label = pen.table.adults$category, x = pen.table.adults$`% Used`, y = pen.table.adults$`Mean Drop`))  +
    scale_shape_manual(values = shapes) +
    labs (x = "% Used", y = "Mean Drop" ) +
    theme_bw()
  
  ggsave("Figure___AdultsPenaltyAnalysis.jpg", plot =adults.penalty.plot, device = "jpeg", width = 10) 
  
}




one.att.penalty.kids <- function ( jar.col.num, file,  liking.col){
  #jar.col.num <- 7
  #file <- as.data.frame ( kids.jar.round )
  #liking.col <- 3
  
  attribute <- names(file)[jar.col.num]
  
  
  overall.by.jar <- aggregate ( file, list (file[[jar.col.num]]), mean) [,4]
  
  # Extract the penalties
  penalty <- overall.by.jar [2] - overall.by.jar
  
  # Remove JAR
  no.no.pen <- penalty [!penalty == 0]
  
  # Get the proportions 
  props<- ( table (file[attribute]) /  length(file[,3]) ) * 100
  
  # Extract the JAR name
  attribute.name <- tail(strsplit(attribute,split=" ")[[1]],1)
  
  # Add the proportions
  penalty.matrix <- rbind ( "props" = props [c(1,3)], no.no.pen )
  
  one.attribute <- data.frame ( t(penalty.matrix), "Attribute" = rep (attribute.name, 2) )
  
  # Create the JAR Levels
  jar.levels <- c("L", "H")
  penalty.matrix <- cbind (one.attribute, jar.levels )
  
  
  colnames ( penalty.matrix )[1:2] <- c("% Used", "Mean Drop")
  return (penalty.matrix)
}
one.att.penalty.kids(4, kids_jar_round, 3)

jar.list <- lapply (4:7, one.att.penalty.kids,file = kids_jar_round, liking.col = 3 )
pen.table <- do.call ( "rbind", jar.list)
pen.table$`% Used` <- pen.table$`% Used`/1000
pen.table$category <- paste0 ( pen.table[,4], pen.table[,3] )

shapes = c( 5:8 )

kids.penalty.plot <- ggplot () +
  geom_point(data = pen.table, aes (x = pen.table$`% Used`, y = pen.table$`Mean Drop`, shape = Attribute)) +
  geom_text_repel (aes(label = pen.table$category, x = pen.table$`% Used`, y = pen.table$`Mean Drop`), cex = 8)  +
  scale_shape_manual(values = shapes) +
  labs (x = "% Used", y = "Mean Drop") +
  theme_bw() + 
  xlim(c(0,30)) + 
  ylim(c(0,2.4)) + 
  theme(axis.title.x = element_text(size = 50 ), 
        axis.title.y = element_text(size = 50 ), 
        axis.text.x =  element_text(size = 25 ),
        axis.text.y = element_text(size = 25 ))
kids.penalty.plot

ggsave("Figure___KidsPenaltyAnalysis.jpg", plot = kids.penalty.plot, device = "jpeg", width = 10) 
















