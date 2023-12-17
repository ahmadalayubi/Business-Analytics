
# Business Analytics
# Nama    : Ahmad Al Ayubi
# NIM     : 01804220003

#===================================================================

# Memuat Libray
library(readxl) 
library(ggplot2)
library(gplots)
library(cluster)
library(stats)
library(dplyr)
library(ggplot2)
library(e1071)
library(fitdistrplus)
library(tidyr)
library(arules)
library(cowplot)
library(arulesViz)
library(RColorBrewer)
library(plotly)
library(factoextra)

#===================================================================

# 1) Import Data (Memuat Dataset Online Retail)
online_retail <- read_excel(file.choose())

head(online_retail)

#===================================================================

# 2) Clean the Data Set

# Melakukan Pengecekan Missing Value
sapply(online_retail, function(x) sum(is.na(x)))

# Menghapus Missing Value
online_retail <- na.omit(subset(online_retail, select = c(InvoiceNo, StockCode, Description, Quantity, InvoiceDate, UnitPrice, CustomerID, Country)))
sapply(online_retail, function(x) sum(is.na(x)))

#===================================================================

# 3) Train Data atau Processing Data

# Melakukan Training Data K-Means Cluster

# Pertama Melakukan Exploratory Data Analysis dengan RFM

# A. Analisa Recency
online_retail$InvoiceDate <- as.Date(online_retail$InvoiceDate)

reference_date <- max(online_retail$InvoiceDate) + 1
online_retail$days_since_last_purchase <- as.numeric(difftime(reference_date, online_retail$InvoiceDate, units = "days"))

customer_history_df <- online_retail %>%
  group_by(CustomerID) %>%
  summarize(recency = min(days_since_last_purchase,na.rm = TRUE))

summary(customer_history_df$recency)
head(customer_history_df)

# Kernel Density plot Recency
QQ_plot <- function(data, measure) {
  fit <- fitdistr(data, "normal")
  
  data_df <- data.frame(data)
  fig1 <- ggplot(data_df, aes(x = data)) +
    geom_density(fill = viridisLite::plasma(1), color = "black") +
    ggtitle(paste(measure, "Distribution (mu =", round(fit$estimate[1], 2), "and sigma =", round(fit$estimate[2], 2), ")")) +
    xlab(measure) +
    ylab("Frequency")
  
  # QQ plot Recency
  fig2 <- ggplot(data_df, aes(sample = data)) +
    stat_qq() +
    geom_abline(slope = fit$estimate[2], intercept = fit$estimate[1], color = viridisLite::plasma(1)) +
    ggtitle(paste(measure, "Probability (skewness:", round(skewness(data), 2), "and kurtosis:", round(kurtosis(data), 3), ")")) +
    xlab("Theoretical Quantiles") +
    ylab(paste(measure, "Quantiles"))
  
  gridExtra::grid.arrange(fig1, fig2)
}
QQ_plot(customer_history_df$recency, "Recency")

# B. Analisa Frequency

# Analisis Frequency
customer_frequency_df <- online_retail %>%
  group_by(CustomerID) %>%
  summarize(Frequency = n())

summary(customer_frequency_df$Frequency)

head(customer_frequency_df)


QQ_plot_frequency <- function(data, measure) {
  fit <- fitdistr(data, "normal")
  
  data_df <- data.frame(data)
  
  # Kernel Density Frequency
  fig1 <- ggplot(data_df, aes(x = data)) +
    geom_density(fill = viridisLite::plasma(1), color = "black") +
    ggtitle(paste(measure, "Distribution (mu =", round(fit$estimate[1], 2), "and sigma =", round(fit$estimate[2], 2), ")")) +
    xlab(measure) +
    ylab("Frequency")
  
  # QQ Plot Frequency
  fig2 <- ggplot(data_df, aes(sample = data)) +
    stat_qq() +
    geom_abline(slope = fit$estimate[2], intercept = fit$estimate[1], color = viridisLite::plasma(1)) +
    ggtitle(paste(measure, "Probability Plot (skewness:", round(skewness(data), 6), "and kurtosis:", round(kurtosis(data), 6), ")")) +
    xlab("Theoretical Quantiles") +
    ylab(paste(measure, "Quantiles"))
  
  gridExtra::grid.arrange(fig1, fig2)
}
QQ_plot_frequency(customer_frequency_df$Frequency, "Frequency")

# C. Analisa Monetary

online_retail$MonetaryValue <- online_retail$Quantity * online_retail$UnitPrice

head(online_retail)

customer_monetary_df <- online_retail %>%
  group_by(CustomerID) %>%
  summarize(MonetaryValue = sum(MonetaryValue,na.rm = TRUE))

summary(customer_monetary_df$MonetaryValue)
head(customer_monetary_df)


QQ_plot_monetary <- function(data, measure) {
  fit <- fitdistr(data, "normal")
  
  data_df <- data.frame(data)
  
  # Kernel Density Plot Monetary
  fig1 <- ggplot(data_df, aes(x = data)) +
    geom_density(fill = viridisLite::plasma(1), color = "black") +
    ggtitle(paste(measure, "Distribution (mu =", round(fit$estimate[1], 2), "and sigma =", round(fit$estimate[2], 2), ")")) +
    xlab(measure) +
    ylab("Frequency")
  
  # QQ Plot Monetary
  fig2 <- ggplot(data_df, aes(sample = data)) +
    stat_qq() +
    geom_abline(slope = fit$estimate[2], intercept = fit$estimate[1], color = viridisLite::plasma(1)) +
    ggtitle(paste(measure, "Probability Plot (skewness:", round(skewness(data), 6), "and kurtosis:", round(kurtosis(data), 6), ")")) +
    xlab("Theoretical Quantiles") +
    ylab(paste(measure, "Quantiles"))
  
  gridExtra::grid.arrange(fig1, fig2)
}
QQ_plot_monetary(customer_monetary_df$MonetaryValue, "Monetary Value")

customer_history_df <- merge(customer_history_df, customer_monetary_df, by = "CustomerID")
head(customer_history_df)

#===================================================================

# 4) Build the Model

# Membuat Dataset RFM
rfm_data <- online_retail %>%
  group_by(CustomerID) %>%
  summarize(Recency = min(days_since_last_purchase),
            Frequency = n_distinct(InvoiceNo),
            MonetaryValue = sum(Quantity * UnitPrice)) %>%
  ungroup()

# Menghitung WCSS untuk K-Means
set.seed(123)
wcss <- sapply(1:10, function(k) {
  kmeans(rfm_data[, c("Recency", "Frequency", "MonetaryValue")], centers = k, nstart = 25)$tot.withinss
})
print(wcss)

#===================================================================
# 5) Make Prediction

# Membuat Plot Elbow (K-Means)
plot(1:10, wcss, type = "b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K", ylab="Total within-clusters sum of squares")

# Menentukan Jumlah Kluster Optimal Secara Visual dari Plot Elbow
optimal_clusters <- 3 # Misalnya kita pilih 3 sebagai jumlah kluster optimal

# Melakukan k-means dengan jumlah kluster optimal
set.seed(123)
kmeans_optimal <- kmeans(rfm_data[, c("Recency", "Frequency", "MonetaryValue")], centers = optimal_clusters, nstart = 25)

# Melihat hasil kluster
print(kmeans_optimal$centers) # Pusat kluster
print(kmeans_optimal$size) # Ukuran kluster

# Menambahkan kluster ke data RFM
rfm_data$cluster <- kmeans_optimal$cluster

# Melihat distribusi RFM untuk setiap kluster
aggregate(rfm_data[, c("Recency", "Frequency", "MonetaryValue")], 
          by = list(rfm_data$cluster), FUN = mean)


# Recency vs Monetary Value
ggplot(rfm_data, aes(x = Recency, y = MonetaryValue, color = factor(cluster))) +
  geom_point(alpha = 0.7) +
  scale_color_discrete(name = "Cluster") +
  labs(title = "Scatter Plot of Recency vs MonetaryValue by Cluster",
       x = "Recency",
       y = "MonetaryValue") +
  theme_minimal()

# Recency vs Frequency
ggplot(rfm_data, aes(x = Recency, y = Frequency, color = factor(cluster))) +
  geom_point(alpha = 0.7) +
  scale_color_discrete(name = "Cluster") +
  labs(title = "Scatter Plot of Recency vs Frequency by Cluster",
       x = "Recency",
       y = "Frequency") +
  theme_minimal()

# MonetaryValue vs Frequency
ggplot(rfm_data, aes(x = MonetaryValue, y = Frequency, color = factor(cluster))) +
  geom_point(alpha = 0.7) +
  scale_color_discrete(name = "Cluster") +
  labs(title = "Scatter Plot of MonetaryValue vs Frequency by Cluster",
       x = "MonetaryValue",
       y = "Frequency") +
  theme_minimal()

# MonetaryValue vs Recency
ggplot(rfm_data, aes(x = MonetaryValue, y = Recency, color = factor(cluster))) +
  geom_point(alpha = 0.7) +
  scale_color_discrete(name = "Cluster") +
  labs(title = "Scatter Plot of MonetaryValue vs Recency by Cluster",
       x = "MonetaryValue",
       y = "Recency") +
  theme_minimal()

# Frequency vs MonetaryValue
ggplot(rfm_data, aes(x = Frequency, y = MonetaryValue, color = factor(cluster))) +
  geom_point(alpha = 0.7) +
  scale_color_discrete(name = "Cluster") +
  labs(title = "Scatter Plot of Frequency vs MonetaryValue by Cluster",
       x = "Frequency",
       y = "MonetaryValue") +
  theme_minimal()


# Frequency vs Recency
ggplot(rfm_data, aes(x = Frequency, y = Recency, color = factor(cluster))) +
  geom_point(alpha = 0.7) +
  scale_color_discrete(name = "Cluster") +
  labs(title = "Scatter Plot of Frequency vs Recency by Cluster",
       x = "Frequency",
       y = "Recency") +
  theme_minimal()

# Membuat 3d Plot
plot_ly(rfm_data, x = ~Recency, y = ~Frequency, z = ~MonetaryValue, 
        color = ~factor(cluster), colors = RColorBrewer::brewer.pal(length(unique(rfm_data$cluster)), "Set1"),
        type = "scatter3d", mode = "markers") %>%
  layout(title = "3D Scatter Plot",
         scene = list(
           xaxis = list(title = "Recency"),
           yaxis = list(title = "Frequency"),
           zaxis = list(title = "Monetary")
         ))

# Menghitung koefisien korelasi
cor_recency_monetary <- cor(rfm_data$Recency, rfm_data$MonetaryValue)
cor_recency_frequency <- cor(rfm_data$Recency, rfm_data$Frequency)
cor_monetary_frequency <- cor(rfm_data$MonetaryValue, rfm_data$Frequency)
cor_monetary_recency <- cor(rfm_data$MonetaryValue, rfm_data$Recency)
cor_frequency_monetary <- cor(rfm_data$Frequency, rfm_data$MonetaryValue)
cor_frequency_monetary <- cor(rfm_data$Frequency, rfm_data$MonetaryValue)

# Menampilkan hasil
cat("Koefisien Korelasi antara Recency dan MonetaryValue:", cor_recency_monetary, "\n")
cat("Koefisien Korelasi antara Recency dan Frequency:", cor_recency_frequency, "\n")
cat("Koefisien Korelasi antara MonetaryValue dan Frequency:", cor_monetary_frequency, "\n")
cat("Koefisien Korelasi antara MonetaryValue dan Recency:", cor_monetary_recency, "\n")
cat("Koefisien Korelasi antara Frequency dan MonetaryValue:", cor_frequency_monetary, "\n")
cat("Koefisien Korelasi antara Frequency dan MonetaryValue:", cor_frequency_monetary, "\n")

# Membuat tabel ringkasan klaster
cluster_summary <- rfm_data %>%
  group_by(cluster) %>%
  summarize(
    avg_recency = mean(Recency),
    avg_MonetaryValue = mean(MonetaryValue),
    avg_Frequency = mean(Frequency)
  )

# Menampilkan tabel ringkasan klaster
print(cluster_summary)

# Market Basket Analysis (Association Rules)

# Mengonversi Dataset Association Rules
trans <- as(online_retail, "transactions")
trans <- trans[, !(colnames(trans) %in% c("StockCode", "Quantity", "InvoiceDate", "UnitPrice", "CustomerID", "Country"))]

# Market Basket Analysis Menggunakan Algoritma Apriori dengan Support = 0.005, confidence = 0.8, dan minimum length = 2
rules <- apriori(trans, parameter = list(support = 0.005, confidence = 0.8, minlen = 2))

# Mengurutkan Rules Berdasarkan Lift
rules <- sort(rules, by = "lift", decreasing = TRUE)
top_rules <- head(rules, n = 5)

# Printing the top 5 rules
for (i in 1:length(top_rules)) {
  cat("Rule #", i, ":\n")
  cat(paste0(labels(lhs(top_rules))[i], " => ", labels(rhs(top_rules))[i]), "\n")
  cat("Support: ", top_rules@quality[i, "support"], " - Confidence: ", top_rules@quality[i, "confidence"], " - Lift: ", top_rules@quality[i, "lift"], "\n")
  cat(rep("-", 80), "\n")  # Add line separator
}

# Membuat Data Frame
top_rules_df <- data.frame(
  Rule = paste(labels(lhs(top_rules)), "=>", labels(rhs(top_rules))),
  Support = top_rules@quality[, "support"],
  Confidence = top_rules@quality[, "confidence"],
  Lift = top_rules@quality[, "lift"]
)

top_rules_df <- gather(top_rules_df, key = "Variable", value = "Value", -Rule)

# Membuat Bar Chart
ggplot(top_rules_df, aes(x = Rule, y = Value, fill = Variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Variable, scales = "free_y", nrow = 1) +
  labs(title = "Top 5 Association Rules",
       x = "Association Rule",
       y = "Value") +
  scale_fill_viridis_d(option = "plasma")


#===================================================================

# 6) Evaluasi Model

# Measuring Performance (K-Means)

# Evaluasi Model K-Means dengan Silhouette

# Menghitung nilai siluet untuk setiap pengamatan
silhouette_vals <- silhouette(kmeans_optimal$cluster, dist(rfm_data[, c("Recency", "Frequency", "MonetaryValue")]))
silhouette_avg <- mean(silhouette_vals[, "sil_width"])

# Menampilkan plot siluet
fviz_silhouette(silhouette_vals)

# Menampilkan nilai rata-rata siluet
cat("Rata-rata Siluet:", silhouette_avg, "\n")

# Melihat hasil kluster
print(kmeans_optimal$centers) # Pusat kluster
print(kmeans_optimal$size) # Ukuran kluster


# Measuring Performance (Association Rules)

# Menghitung metrik evaluasi untuk setiap aturan
for (i in 1:length(top_rules)) {
  support <- top_rules@quality[i, "support"]
  confidence <- top_rules@quality[i, "confidence"]
  lift <- top_rules@quality[i, "lift"]
  
  cat("Rule #", i, ":\n")
  cat(paste0(labels(lhs(top_rules))[i], " => ", labels(rhs(top_rules))[i]), "\n")
  cat("Support: ", support, " - Confidence: ", confidence, " - Lift: ", lift, "\n")
  cat(rep("-", 80), "\n")  # Add line separator
}

# Menampilkan rata-rata metrik evaluasi
average_support <- mean(top_rules@quality[, "support"])
average_confidence <- mean(top_rules@quality[, "confidence"])
average_lift <- mean(top_rules@quality[, "lift"])

cat("Average Support:", average_support, "\n")
cat("Average Confidence:", average_confidence, "\n")
cat("Average Lift:", average_lift, "\n")

# Tune Hyperparameter (K-Means)

# Function to perform k-means clustering and return silhouette score
perform_kmeans <- function(data, k) {
  set.seed(123)
  kmeans_model <- kmeans(data, centers = k, nstart = 25)
  silhouette_vals <- silhouette(kmeans_model$cluster, dist(data))
  return(mean(silhouette_vals[, "sil_width"]))
}

# Grid Search for k-means hyperparameters
k_values <- 2:10
silhouette_scores <- sapply(k_values, function(k) perform_kmeans(rfm_data[, c("Recency", "Frequency", "MonetaryValue")], k))

# Plot the results
plot(k_values, silhouette_scores, type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters K", ylab = "Average Silhouette Score")

# Find the optimal number of clusters
optimal_k <- k_values[which.max(silhouette_scores)]

# Output the optimal number of clusters
cat("Optimal Number of Clusters:", optimal_k, "\n")

# Tune Hyperparameter (Association rules)

# Function to perform association rules and return average lift
perform_association_rules <- function(transactions, support, confidence, minlen) {
  rules <- apriori(transactions, parameter = list(support = support, confidence = confidence, minlen = minlen))
  rules <- sort(rules, by = "lift", decreasing = TRUE)
  return(mean(rules@quality[, "lift"]))
}

# Grid Search for association rules hyperparameters
support_values <- seq(0.001, 0.01, by = 0.001)
confidence_values <- seq(0.7, 0.9, by = 0.05)
minlen_values <- 2:3

# Initialize variables to store results
optimal_support <- NA
optimal_confidence <- NA
optimal_minlen <- NA
max_average_lift <- -Inf

# Perform grid search
for (support in support_values) {
  for (confidence in confidence_values) {
    for (minlen in minlen_values) {
      average_lift <- perform_association_rules(trans, support, confidence, minlen)
      if (average_lift > max_average_lift) {
        max_average_lift <- average_lift
        optimal_support <- support
        optimal_confidence <- confidence
        optimal_minlen <- minlen
      }
    }
  }
}

# Output the optimal hyperparameters
cat("Optimal Support:", optimal_support, "\n")
cat("Optimal Confidence:", optimal_confidence, "\n")
cat("Optimal Minlen:", optimal_minlen, "\n")
cat("Max Average Lift:", max_average_lift, "\n")

#Selesai