# R script to create violin plots from Dataset1 and Dataset2 files
# (Without individual data points)

# Load required libraries
library(ggplot2)
library(dplyr)
library(readr)

# Function to read data files
read_data_file <- function(filename) {
  tryCatch({
    data <- read_lines(filename) %>%
            as.numeric()
    return(data)
  }, error = function(e) {
    stop(paste("Error reading file:", filename, "-", e$message))
  })
}

# Create violin plot function
create_violin_plot <- function(minicore_file, compositecore_file, output_file = "violin_plot_r.png") {
  # Read data from files
  minicore_data <- read_data_file(minicore_file)
  compositecore_data <- read_data_file(compositecore_file)
  
  # Create a data frame for plotting
  plot_data <- data.frame(
    Dataset = c(rep("Minicore", length(minicore_data)), 
                rep("Compositecore", length(compositecore_data))),
    Value = c(minicore_data, compositecore_data)
  )
  
  # Calculate summary statistics
  summary_stats <- plot_data %>%
    group_by(Dataset) %>%
    summarize(
      Mean = mean(Value),
      Median = median(Value),
      SD = sd(Value),
      N = n()
    )
  
  # Create the violin plot (without individual data points)
  p <- ggplot(plot_data, aes(x = Dataset, y = Value, fill = Dataset)) +
    geom_violin(trim = FALSE, alpha = 0.8) +
    geom_boxplot(width = 0.1, fill = "white", alpha = 0.5) +
    scale_fill_manual(values = c("Minicore" = "#E69F00", "Compositecore" = "#56B4E9")) +
    labs(
      title = "Distribution Comparison: Minicore vs Compositecore",
      y = "Diversity",
      x = NULL
    ) +
    theme_light() +
    theme(
      legend.position = "none",
      plot.title = element_text(hjust = 0.5, size = 18),
      axis.text = element_text(size = 18),
      axis.title = element_text(size = 18)
    )
  
  # Add statistics as text annotations
  for (i in 1:nrow(summary_stats)) {
    dataset <- summary_stats$Dataset[i]
    y_pos <- max(plot_data$Value[plot_data$Dataset == dataset]) + 0.01
    label <- sprintf("n=%d\nmean=%.4f", summary_stats$N[i], summary_stats$Mean[i])
    
    p <- p + annotate("text", x = i, y = y_pos, label = label, size = 3.5)
  }
  
  # Save the plot
  ggsave(output_file, p, width = 8, height = 6, dpi = 300)
  cat(paste("Violin plot saved as", output_file, "\n"))
  
  # Return the plot object
  return(p)
}

# Command line interface
if (!interactive()) {
  args <- commandArgs(trailingOnly = TRUE)
  
  if (length(args) >= 2) {
    minicore_file <- args[1]
    compositecore_file <- args[2]
    
    output_file <- "violin_plot_r.png"
    if (length(args) >= 3) {
      output_file <- args[3]
    }
    
    create_violin_plot(minicore_file, compositecore_file, output_file)
  } else {
    cat("Usage: Rscript script.R Data1_file Data2_file [output_file]\n")
    cat("Example: Rscript script.R Data1_file Data2_file violin_plot_r.png\n")
  }
}
