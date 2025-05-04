# Omar Moustafa 90022400 & Nour Kahky 900221042
# Applied Multivariate Analysis (MACT 4233)
# Project #3 - 'Clustering'
# All 25 Hierarchical & Multiple K-Means Clusterings Included

# Installing the necessary packages
if(!require("robustX")) install.packages("robustX")
if(!require("dplyr")) install.packages("dplyr")
if(!require("MASS")) install.packages("MASS")

# Loading the necessary packages
require(robustX); library(robustbase); library(dplyr); library(MASS) 
library(nnet); library(ggplot2)

# 1. Load and Describe the Data
# Reading the CSV file
df = read.csv("babies.csv")

# Displaying the first 3 rows of the dataset
head(df, 3)

# Displaying the number and rows and columns (dimensions) of the dataset
my_dimensions = dim(df)
cat("Number of Rows:", my_dimensions[1], "\n")
cat("Number of Columns:", my_dimensions[2], "\n")

# View the summary/quantiles of the data
summary(df)

# 2. Scale the Data (Standard Practice)
# Standardization helps most clustering methods work better
# Keep only the quantitative variables
# Standardization helps most clustering methods work better
# Keep only the quantitative variables
df_numeric = df[, c("bwt", "gestation", "age", "height", "weight")]
x = scale(df_numeric)

# Displaying the first 3 rows of the fully quantitative & standardized dataset
head(x, 3)
 
# 3. Five Methods of Distance Between Observations
# Compute distance between observations using 5 different methods (Observation-Level)

# These first 4 methods below are for the quantitative variables
d_euclidean = dist(x, method = "euclidean")

d_manhattan = dist(x, method = "manhattan")

d_canberra = dist(x, method = "canberra")

d_minkowski = dist(x, method = "minkowski")

# The 5th method below is for the categorical variables
# Hamming Distance calculated using (method = "binary")

# parity (0 = 1st pregnancy, 1 = not 1st pregnancy) & smoke (0 = non-smoker, 1 = smoker)
df_categorical = df[, c("parity", "smoke")]

# Ensure that they're treated as numeric binary variables (0 or 1)
df_categorical = data.frame(lapply(df_categorical, as.numeric))

d_hamming = dist(df_categorical, method = "binary")


# 4. Five Methods of Distance/Linkage Between Clusters
# Compute distance between clusters using 5 different methods (Cluster-Level)

# Part 4.1 - Hierarchical Clustering (hclust) with Euclidean Distance
par(mfrow = c(3,2))

# Single Linkage #1 - Euclidean Distance
hc_single_euclidean = hclust(d_euclidean, method = "single")
plot(hc_single_euclidean, labels = F, main = "Single Linkage #1 - Euclidean Distance")
rect.hclust(hc_single_euclidean, k = 3, border = "red")

# Complete Linkage #1 - Euclidean Distance
hc_complete_euclidean = hclust(d_euclidean, method = "complete")
plot(hc_complete_euclidean, labels = F, main = "Complete Linkage #1 - Euclidean Distance")
rect.hclust(hc_complete_euclidean, k = 3, border = "red")

# Average Linkage #1 - Euclidean Distance
hc_average_euclidean = hclust(d_euclidean, method = "average")
plot(hc_average_euclidean, labels = F, main = "Average Linkage #1 - Euclidean Distance")
rect.hclust(hc_average_euclidean, k = 3, border = "red")

# Centroid Linkage #1 - Euclidean Distance
hc_centroid_euclidean = hclust(d_euclidean, method = "centroid")
plot(hc_centroid_euclidean, labels = F, main = "Centroid Linkage #1 - Euclidean Distance")
rect.hclust(hc_centroid_euclidean, k = 3, border = "red")

# Ward’s Method #1 - Euclidean Distance
hc_ward_euclidean = hclust(d_euclidean, method = "ward.D2")
plot(hc_ward_euclidean, labels = F, main = "Ward’s Method #1 - Euclidean Distance")
rect.hclust(hc_ward_euclidean, k = 3, border = "red")


# Part 4.2 - Hierarchical Clustering (hclust) with Manhattan Distance

par(mfrow = c(3,2))

# Single Linkage #2 - Manhattan Distance
hc_single_manhattan = hclust(d_manhattan, method = "single")
plot(hc_single_manhattan, labels = F, main = "Single Linkage #2 - Manhattan Distance")
rect.hclust(hc_single_manhattan, k = 3, border = "red")

# Complete Linkage #2 - Manhattan Distance
hc_complete_manhattan = hclust(d_manhattan, method = "complete")
plot(hc_complete_manhattan, labels = F, main = "Complete Linkage #2 - Manhattan Distance")
rect.hclust(hc_complete_manhattan, k = 3, border = "red")

# Average Linkage #2 - Manhattan Distance
hc_average_manhattan = hclust(d_manhattan, method = "average")
plot(hc_average_manhattan, labels = F, main = "Average Linkage #2 - Manhattan Distance")
rect.hclust(hc_average_manhattan, k = 3, border = "red")

# Centroid Linkage #2 - Manhattan Distance
hc_centroid_manhattan = hclust(d_manhattan, method = "centroid")
plot(hc_centroid_manhattan, labels = F, main = "Centroid Linkage #2 - Manhattan Distance")
rect.hclust(hc_centroid_manhattan, k = 3, border = "red")

# Ward’s Method #2 - Manhattan Distance
hc_ward_manhattan = hclust(d_manhattan, method = "ward.D2")
plot(hc_ward_manhattan, labels = F, main = "Ward’s Method #2 - Manhattan Distance")
rect.hclust(hc_ward_manhattan, k = 3, border = "red")


# Part 4.3 - Hierarchical Clustering (hclust) with Canberra Distance

par(mfrow = c(3,2))

# Single Linkage #3 - Canberra Distance
hc_single_canberra = hclust(d_canberra, method = "single")
plot(hc_single_canberra, labels = F, main = "Single Linkage #3 - Canberra Distance")
rect.hclust(hc_single_canberra, k = 3, border = "red")

# Complete Linkage #3 - Canberra Distance
hc_complete_canberra = hclust(d_canberra, method = "complete")
plot(hc_complete_canberra, labels = F, main = "Complete Linkage #3 - Canberra Distance")
rect.hclust(hc_complete_canberra, k = 3, border = "red")

# Average Linkage #3 - Canberra Distance
hc_average_canberra = hclust(d_canberra, method = "average")
plot(hc_average_canberra, labels = F, main = "Average Linkage #3 - Canberra Distance")
rect.hclust(hc_average_canberra, k = 3, border = "red")

# Centroid Linkage #3 - Canberra Distance
hc_centroid_canberra = hclust(d_canberra, method = "centroid")
plot(hc_centroid_canberra, labels = F, main = "Centroid Linkage #3 - Canberra Distance")
rect.hclust(hc_centroid_canberra, k = 3, border = "red")

# Ward's Method #3 - Canberra Distance
hc_ward_canberra = hclust(d_canberra, method = "ward.D2")
plot(hc_ward_canberra, labels = F, main = "Ward's Method #3 - Canberra Distance")
rect.hclust(hc_ward_canberra, k = 3, border = "red")


# Part 4.4 - Hierarchical Clustering (hclust) with Minkowski Distance

par(mfrow = c(3,2))

# Single Linkage #4 - Minkowski Distance
hc_single_minkowski = hclust(d_minkowski, method = "single")
plot(hc_single_minkowski, labels = F, main = "Single Linkage #4 - Minkowski Distance")
rect.hclust(hc_single_minkowski, k = 3, border = "red")

# Complete Linkage #4 - Minkowski Distance
hc_complete_minkowski = hclust(d_minkowski, method = "complete")
plot(hc_complete_minkowski, labels = F, main = "Complete Linkage #4 - Minkowski Distance")
rect.hclust(hc_complete_minkowski, k = 3, border = "red")

# Average Linkage #4 - Minkowski Distance
hc_average_minkowski = hclust(d_minkowski, method = "average")
plot(hc_average_minkowski, labels = F, main = "Average Linkage #4 - Minkowski Distance")
rect.hclust(hc_average_minkowski, k = 3, border = "red")

# Centroid Linkage #4 - Minkowski Distance
hc_centroid_minkowski = hclust(d_minkowski, method = "centroid")
plot(hc_centroid_minkowski, labels = F, main = "Centroid Linkage #4 - Minkowski Distance")
rect.hclust(hc_centroid_minkowski, k = 3, border = "red")

# Ward's Method #4 - Minkowski Distance
hc_ward_minkowski = hclust(d_minkowski, method = "ward.D2")
plot(hc_ward_minkowski, labels = F, main = "Ward's Method #4 - Minkowski Distance")
rect.hclust(hc_ward_minkowski, k = 3, border = "red")

# Part 4.5 - Hierarchical Clustering (hclust) with Hamming Distance

par(mfrow = c(3,2))

# Single Linkage #5 - Hamming Distance
hc_single_hamming = hclust(d_hamming, method = "single")
plot(hc_single_hamming, labels = F, main = "Single Linkage #5 - Hamming Distance")
rect.hclust(hc_single_hamming, k = 3, border = "red")

# Complete Linkage #5 - Hamming Distance
hc_complete_hamming = hclust(d_hamming, method = "complete")
plot(hc_complete_hamming, labels = F, main = "Complete Linkage #5 - Hamming Distance")
rect.hclust(hc_complete_hamming, k = 3, border = "red")

# Average Linkage #5 - Hamming Distance
hc_average_hamming = hclust(d_hamming, method = "average")
plot(hc_average_hamming, labels = F, main = "Average Linkage #5 - Hamming Distance")
rect.hclust(hc_average_hamming, k = 3, border = "red")

# Centroid Linkage #5 - Hamming Distance
hc_centroid_hamming = hclust(d_hamming, method = "centroid")
plot(hc_centroid_hamming, labels = F, main = "Centroid Linkage #5 - Hamming Distance")
rect.hclust(hc_centroid_hamming, k = 3, border = "red")

# Ward's Method #5 - Hamming Distance
hc_ward_hamming = hclust(d_hamming, method = "ward.D2")
plot(hc_ward_hamming, labels = F, main = "Ward's Method #5 - Hamming Distance")
rect.hclust(hc_ward_hamming, k = 3, border = "red")

# Part 5 - K-Means Clustering on the Standardized Quantitative Variables
# Remove any rows with missing values (required for k-means to work)
# Remove rows with NAs from both x and df at the same time (so they match row-by-row)
df_clean = df[complete.cases(df[, c("bwt", "gestation", "age", "height", "weight")]), ]

# Now you can safely cluster x and assign clusters to df_clean
x = scale(df_clean[, c("bwt", "gestation", "age", "height", "weight")])

# The following code was re-run 5 different times where the first run had k = 2 
# the second run had k = 3, and so on until k = 6

# Run K-means clustering for k = 3 clusters on the scaled numeric data
set.seed(123)  # for reproducibility
km3 = kmeans(x, centers = 3, nstart = 20)

# View cluster sizes
print(km3$size)

# Add k-means cluster labels to the dataset (optional)
df_clean$kmeans_cluster = km3$cluster

# Compute total within-cluster sum of squares
cat("Total within-cluster SS:", km3$tot.withinss, "\n")

# Compute R² using same function as before
R2 <- function(x, clusters, k=3){
  n <- nrow(x)
  tss <- var(x)
  tss <- (n - 1) * sum(diag(tss))
  wss <- 0
  for(j in 1:k){
    cj <- x[clusters == j, , drop = FALSE]
    nj <- nrow(cj)
    vj <- var(cj)
    wssj <- if(nj > 1) (nj - 1) * sum(diag(vj)) else 0
    wss <- wss + wssj
  }
  r2 <- 1 - wss / tss
  cat("R² =", round(r2, 4), "\n")
  return(r2)
}

# Run R² for k-means result
R2(x, km3$cluster, k = 3)

# Recollecting all R² values for the upcoming Elbow plot 
# Recalling the function with different 'k' values
# Empty vector to contain R² values for 5 different 'k' values
r2_scores = numeric(5)

# Empty vector to contain WSS values for 5 different 'k' values
wss_scores = numeric(5)

for (k in 2:6) {
  set.seed(123)
  km  = kmeans(x, centers = k, nstart = 20)
  
  # Store all WSS values  
  wss_scores[k - 1] = km$tot.withinss
  
  # Store all R² values
  r2_scores[k - 1] = R2(x, km$cluster, k)
}

# Displaying the results
data.frame(
  k = 2:6,
  WSS = round(wss_scores, 2),
  R2 = round(r2_scores, 4)
)

# An example of a 2D data set with 3 clusters
# There are 5 quantitative variables (clusters) so below is an example of
# of a 2D data set with 3 clusters

# Visualizing K-Means Clustering (k = 3) on Birthweight and Gestation
k = 3
kmc = kmeans(x, centers = k)
clusters = kmc$cluster

# Extract the 2 dimensions to visualize: bwt and gestation
x_2D = x[, c("bwt", "gestation")]

# Plot
plot(x_2D, pch = 19, col = clusters,
     main = "K-Means Clustering (k=3) Visualized on Birthweight and Gestation Vars",
     xlab = "Birthweight (bwt)", ylab = "Gestation")

# Plot cluster centers (2D version)
centers_2D = kmc$centers[, c("bwt", "gestation")]
points(centers_2D, col = 1:k, pch = 8, cex = 2)
points(centers_2D, col = 1:k, pch = 19, cex = 1)

# Determining the number of clusters
wss = (nrow(x)-1)*sum(apply(x,2,var)) # x has been scaled
for (i in 2:10) {
  wss[i] = sum(kmeans(x, centers = i)$withinss)}
plot(wss, type="b", pch=19, xlab="k", 
     ylab="WSS", main="The L-Curve")

# Elbow Plot to Visualize the Steadily Increasing R² values
# Elbow Plot
plot(2:6, r2_scores, type = "b", pch = 19, xlab = "Number of Clusters (value of k)",
     ylab = "R²", main = "R² for K-Means Clustering (k = 2 to k = 6)")


