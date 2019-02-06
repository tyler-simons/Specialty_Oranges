## CATA Analysis (Consumers)

kids_clustered <- read_csv("../Data/kids_spec_clustered.csv")
adults_clustered <- read_csv("../Data/adults_spec_clustered.csv")


make_cata_table <- function (df, cata_start, cata_end){
  just.cata <- as.data.frame ( df[,c(2,cata_start:cata_end)] )
  cata.list <- split (just.cata, just.cata$Code)
  colsums <- function (list) { 
    return ( apply ( cata.list[[list]][,-which (colnames(cata.list[[list]]) == "Code")], 2, sum ) )
  }
  cata.table <- data.frame ( t ( sapply ( 1:length(cata.list), colsums) ), row.names = names (cata.list) )
  return ( cata.table )
}


## Fishers Exact Test ##
ac1 = 75.6
ac2 = 80.4
c1 = round ( (ac1/100)*79 )
c2 = round ( (ac2/100)*40)
p1 = round (( 1-(ac1/100)) * 79)
p2 =round (( 1-(ac2/100)) * 40)

TeaTasting <-
  t ( matrix(c(c1,p1,c2,p2),
             nrow = 2,
             dimnames = list(Guess = c("Yes", "No"),
                             Truth = c("C1", "C2"))) )
fisher.test(TeaTasting, alternative = "two.sided")
?fisher.test
40*.6
round ( .76*79 )





cata.list <- names (kids_clustered)[12:23]
cochran.func <- function (attribute, clustered.data){
  cochran.data_long <- clustered.data %>% select(Consumer, Product, attribute = attribute)
  c_test <- cochran.qtest(attribute~Product | Consumer, data = cochran.data_long, alpha = 0.05)
  return ( c_test$p.value )
}


cochran.func(cata.list[4], kids_clustered)
cata.pvalues <- sapply (cata.list, cochran.func, kids_clustered)
which (cata.pvalues < 0.05)

kids_clustered %>% 
  select (Product, red.color:chewy) %>% 
  group_by(Product) %>% summarise_all(sum) %>% copy.clipboard()
nrow(kids_clustered) / 8

adults_cata_sums <- adults_clustered %>% 
  select (Product, aromatic:fibrous) %>% 
  group_by(Product) %>% summarise_all(sum) 

# Plot a correspondence analysis for the adults

ac <- adults_cata_sums %>% as.data.frame()
rownames(ac) <- ac[,1]
ac[,1] <- NULL
library(ca)
c_a <- ca(ac)
my_plot <- plot(c_a, mass = TRUE, contrib = "absolute", map =
       "rowgreen", arrows = c(FALSE, TRUE))
my_plot

# Turn into a ggplot
library(ggrepel)
gplot <- ggplot() + 
  geom_point(aes(x = my_plot$rows[,1], y = my_plot$rows[,2]), color = "red") + 
  geom_text_repel(aes(x = my_plot$rows[,1], y = my_plot$rows[,2], label = rownames(my_plot$rows)), color = "red") +
  geom_segment(aes(xend = my_plot$cols[,1], yend = my_plot$cols[,2], x = 0, y = 0), 
               arrow = arrow(length = unit(0.01, "npc"))) + 
  geom_text_repel(aes(x = my_plot$cols[,1], y = my_plot$cols[,2], label = rownames(my_plot$cols))) + 
  theme_minimal() + xlab("Dimension 1 (64.9%)") + ylab("Dimension 2 (18.3%")
# ggsave("../Manuscript/Figures/Adult_CA.jpg",gplot, width = 8, device = "jpg")


