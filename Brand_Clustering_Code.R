library("dplyr")
library("ggplot2")
library(cluster)    # clustering algorithms
library(factoextra) # clustering visualization
library(dendextend) # for comparing two dendrograms
library(tidyr)     # pivot longer
library(fpc)        # cluster validation

data_brand <- readxl::read_excel("brand.xlsx")
data_brand <- as.data.frame(data_brand)

# meng-assign nilai dari kolom brand menjadi rownames
rownames(data_brand) <- data_brand$Brand

# membuang kolom yang tidak digunakan
data_brand <- data_brand %>% 
  select(-Brand)

# scaling
brand_scale <- scale(data_brand)

# Agglomerative
# Dissimilarity matrix
d <- dist(brand_scale, method = "euclidean")

# Hierarchical clustering using Complete Linkage
hc_agg1 <- hclust(d, method = "ward.D" )

# Plot the obtained dendrogram
plot(hc_agg1, cex = 0.6, hang = -1)

# methods to assess
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")

# function to compute coefficient
ac <- function(x) {
  agnes(brand_scale, method = x)$ac
}

library(purrr)
map_dbl(m, ac)

# Divisive
# compute divisive hierarchical clustering
hc_div <- diana(brand_scale)

# plot dendrogram
pltree(hc_div, cex = 0.6, hang = -1, main = "Dendrogram of diana")

# Cut tree into 4 groups
cluster <- cutree(hc_agg1, k = 4)