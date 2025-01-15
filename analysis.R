# Generate random data
generate_random_data <- function(n = 100, mean = 0, sd = 1) {
  data <- rnorm(n, mean, sd)
  return(data)
}

# Summarize data
summarize_data <- function(data) {
  summary <- list(
    mean = mean(data),
    median = median(data),
    sd = sd(data),
    min = min(data),
    max = max(data),
    quantiles = quantile(data)
  )
  return(summary)
}

# Visualize data with a histogram
plot_histogram <- function(data, bins = 30, title = "Histogram of Random Data") {
  hist(data, breaks = bins, col = "skyblue", main = title, xlab = "Value", ylab = "Frequency")
}

# Visualize data with a boxplot
plot_boxplot <- function(data, title = "Boxplot of Random Data") {
  boxplot(data, main = title, ylab = "Value", col = "lightgreen")
}

# Add noise to data
add_noise <- function(data, noise_level = 0.1) {
  noise <- rnorm(length(data), mean = 0, sd = noise_level)
  noisy_data <- data + noise
  return(noisy_data)
}

# Generate a random dataset with multiple variables
generate_random_dataset <- function(n = 100) {
  dataset <- data.frame(
    var1 = rnorm(n, mean = 50, sd = 10),
    var2 = runif(n, min = 0, max = 100),
    var3 = sample(letters, n, replace = TRUE)
  )
  return(dataset)
}

# Plot pairwise scatterplots for a dataset
plot_pairwise <- function(dataset) {
  pairs(dataset, main = "Pairwise Scatterplots", col = "blue")
}

# Example workflow
example_workflow <- function() {
  # Generate random data
  data <- generate_random_data(n = 500, mean = 5, sd = 2)
  
  # Summarize the data
  summary <- summarize_data(data)
  print(summary)
  
  # Plot data
  plot_histogram(data)
  plot_boxplot(data)
  
  # Add noise and visualize
  noisy_data <- add_noise(data, noise_level = 0.5)
  plot_histogram(noisy_data, title = "Histogram of Noisy Data")
}
