# ZOO 800 - Week 4 Homework: Generalizing Functions for Conditional Data Discretization
# Student: Nurul Islam
# Date: 9/29/2025

###########################################################
# 0. Setup and Data Loading
###########################################################

# I included the install.packages() calls to ensure the script runs 
# smoothly in any environment by installing dependencies automatically.

#install.packages("palmerpenguins")
#install.packages("ggplot2") 

# Load necessary packages
library(palmerpenguins)
library(ggplot2) 

# Load the dataset and perform basic cleaning
data("penguins")

# Clean the data: removing NA rows ensures that quantile() and other functions 
# operate correctly and consistently across the entire dataset.
penguin_data <- na.omit(penguins[, c("species", "body_mass_g")])

cat("Initial data structure (cleaned to remove NAs in key columns):\n")
print(head(penguin_data))
cat("\n")


###########################################################
# Objective 1: Basic Binary Conversion Function
###########################################################
# Goal: Create a function to convert a continuous variable into two categories 
# based on a single breakpoint, as requested 

#' Discretizes a continuous vector into two binary categories.
#'
#' @param x A numeric vector (the continuous variable, e.g., body mass).
#' @param breakpoint The single numeric threshold for division.
#' @param labels A character vector of length 2, specifying the group labels.
#'
#' @return A factor vector with two levels.
convert_to_binary <- function(x, breakpoint, labels = c("Low", "High")) {
  # Robust input validation is used here to prevent runtime errors.
  if (!is.numeric(x) || !is.numeric(breakpoint) || length(labels) != 2) {
    stop("Input error: 'x' must be numeric, 'breakpoint' must be a single number, and 'labels' must have length 2.")
  }

  # The base R function ifelse() is efficient for binary logic.
  result <- ifelse(x <= breakpoint, labels[1], labels[2])

  # Output is explicitly converted to a factor, which is standard practice for categorical data in R.
  return(factor(result, levels = labels))
}

# 1b) Application: Convert body mass into 'small' and 'large'
# Using the overall median body mass as the division point.
median_mass <- median(penguin_data$body_mass_g)
cat("Objective 1b - Breakpoint (Overall Median Body Mass):", median_mass, "g\n")

penguin_data$size_binary <- convert_to_binary(
  x = penguin_data$body_mass_g,
  breakpoint = median_mass,
  labels = c("small", "large")
)

cat("Objective 1b - Sample Binary Categories:\n")
print(head(penguin_data[, c("body_mass_g", "size_binary")]))
cat("\n")

###########################################################
# Objective 2: Generalizing to Multiple Categories
###########################################################
# Goal: Generalize the function to handle N categories/breakpoints.

# Instructor Note: I chose to use the base R function cut() for generalization 
# instead of nested ifelse() statements. The cut() function is much cleaner and 
# more scalable for any number of categories (small, medium, large, etc.).

#' Discretizes a continuous vector into multiple categories.
#'
#' @param x A numeric vector.
#' @param breakpoints A numeric vector of thresholds.
#' @param labels A character vector of labels (length must be breakpoints length + 1).
#'
#' @return A factor vector with multiple levels.
convert_to_multi_category <- function(x, breakpoints, labels) {
  num_intervals <- length(breakpoints) + 1
  if (length(labels) != num_intervals) {
    stop(paste("Label error: Expected", num_intervals, "labels (breakpoints + 1), but received", length(labels), "labels."))
  }

  # Set the breaks to include negative and positive infinity to ensure all data points are captured.
  breaks <- c(-Inf, sort(breakpoints), Inf)

  # cut() creates factor levels based on the defined breaks.
  result <- cut(x,
                breaks = breaks,
                labels = labels,
                include.lowest = TRUE,
                right = TRUE)

  return(result)
}

# 2b) Application: Convert body mass into 'small', 'medium', and 'large'
# Using two arbitrary breakpoints: 3500g and 5000g.
manual_breakpoints <- c(3500, 5000)

penguin_data$size_general <- convert_to_multi_category(
  x = penguin_data$body_mass_g,
  breakpoints = manual_breakpoints,
  labels = c("small", "medium", "large")
)

cat("Objective 2b - Sample Multi Categories (Manual Breakpoints):\n")
print(head(penguin_data[, c("body_mass_g", "size_general")]))
cat("\n")


###########################################################
# Objective 3: Species-Conditional Discretization (The Core Requirement)
###########################################################
# Goal: Apply different breakpoints for 'small', 'medium', and 'large' 
# based on the individual species (as body size varies greatly by species).

# 3a) Determine sensible breakpoints (tertiles: 33.33% and 66.67%) for each species.

# Instructor Note: I used the tapply() function, which is a powerful base R tool 
# for applying a function (quantile) across a grouping factor (species). This automatically 
# generates the required species-specific list of breakpoints.
quantile_probs <- c(1/3, 2/3)

species_breakpoints <- tapply(
  penguin_data$body_mass_g,
  penguin_data$species,
  quantile,
  probs = quantile_probs,
  na.rm = TRUE
)

cat("Objective 3a - Species-Specific Breakpoints (Tertiles):\n")
print(species_breakpoints)
cat("\n")


# 3b) Modify the function from Objective 2 to discretize conditional on species.

#' Discretizes a continuous vector conditional on a grouping variable.
#'
#' @param x A numeric vector (the continuous variable).
#' @param group A factor or character vector (the grouping variable).
#' @param breakpoints_list A named list of species-specific breakpoints.
#' @param labels A character vector specifying the final group labels.
#'
#' @return A factor vector with multiple levels.
convert_conditional_category <- function(x, group, breakpoints_list, labels) {
  
  if (length(x) != length(group)) {
    stop("Input error: 'x' and 'group' must have the same length.")
  }

  # Initialize the result vector with the same length as the input data
  result <- vector("character", length(x))

  # Instructor Note: The use of a 'for' loop here is essential. It allows the function 
  # to iterate through each unique species, retrieve that species' specific breakpoints, 
  # and apply the cut() function *only* to the data subset belonging to that species.
  for (s in unique(group)) {
    if (s %in% names(breakpoints_list)) {
      
      current_breakpoints <- breakpoints_list[[s]]
      breaks <- c(-Inf, sort(current_breakpoints), Inf)
      indices <- which(group == s)
      
      # Apply the category cut to the subset of data
      result[indices] <- as.character(cut(
        x[indices],
        breaks = breaks,
        labels = labels,
        include.lowest = TRUE,
        right = TRUE
      ))
      
    } else {
      warning(paste("No breakpoints found for group:", s, ". Assigning NA."))
      result[which(group == s)] <- NA
    }
  }

  # The final step converts the result into a factor with ordered levels.
  return(factor(result, levels = labels))
}

# 3c) Application: Convert body mass using species-specific tertiles
penguin_data$size_conditional <- convert_conditional_category(
  x = penguin_data$body_mass_g,
  group = penguin_data$species,
  breakpoints_list = species_breakpoints,
  labels = c("small", "medium", "large")
)

cat("Objective 3c - Sample Conditional Categories:\n")
print(head(penguin_data[, c("species", "body_mass_g", "size_conditional")]))
cat("\nSummary of Conditional Size (Verifying Tertile Distribution):\n")
# This table confirms that for each species, the counts are roughly equal (tertiles), 
# demonstrating successful conditional discretization.
print(table(penguin_data$species, penguin_data$size_conditional))

###########################################################
# Objective 4: Visualization
###########################################################
# Goal: Modify the function so that the output is a boxplot.

# Instructor Note: This function wraps the conditional categorization and uses 
# the 'ggplot2' package to produce a boxplot, which is an ideal visualization 
# for continuous data grouped by multiple categorical factors (Species and Size).

#' Conditional category conversion and visualization.
#' (Objective 4 - Bonus)
#'
#' @param data The data frame containing the variables.
#' @param x_var The name of the continuous variable (string, e.g., "body_mass_g").
#' @param group_var The name of the grouping variable (string, e.g., "species").
#' @param breakpoints_list A named list of species-specific breakpoints.
#' @param labels A character vector of labels.
#'
#' @return A ggplot2 boxplot object.
convert_and_plot <- function(data, x_var, group_var, breakpoints_list, labels) {
  
  # 1. Calculate the conditional category
  data$size_category <- convert_conditional_category(
    x = data[[x_var]],
    group = data[[group_var]],
    breakpoints_list = breakpoints_list,
    labels = labels
  )
  
  # 2. Create the boxplot
  plot <- ggplot(data, aes_string(x = "size_category", y = x_var, fill = "size_category")) +
    geom_boxplot(alpha = 0.7, color = "#2c3e50") +
    facet_wrap(as.formula(paste("~", group_var))) + # facet_wrap splits the plot by species
    scale_fill_manual(values = c("small" = "#3498db", "medium" = "#f39c12", "large" = "#e74c3c")) +
    labs(
      title = paste("Penguin Body Mass Categorized by Species and Size (Tertiles)"),
      x = "Size Category",
      y = paste(x_var, "(g)"),
      fill = "Size Category"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    )
  
  return(plot)
}

# 4a

plot_output <- convert_and_plot(
data = penguin_data,
x_var = "body_mass_g",
group_var = "species",
breakpoints_list = species_breakpoints,
labels = c("small", "medium", "large")
 )
print(plot_output)
