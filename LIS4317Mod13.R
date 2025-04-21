#•--------------------------------------------------------------------
# SECTION 1   : COURSE AND ASSIGNMENT DETAILS
# --------------------------------------------------------------------
# Course      : LIS-4317
# Assignment  : Module 13 - Animation in R
# URL         : https://usflearn.instructure.com/courses/1934094/assignments/17787203
# Filename    : LIS4317Mod13.R
# Purpose     : Create a simple animation using the animation package and discuss in a blog post
# Author      : Steven Barden
# Email       : StevenBarden@usf.edu
# Created     : 2025-04-21-0530-00
# Updated     : 2025-04-21-0530-00
# License     : The Stunningly & Unscrupulously Free Unlicense Deluxe
# Description : This script generates an animation of 10 scatter plots with random uniform data
#             : using the animation package, saves it as a GIF, and supports a blog post discussion
#             : for data journalism applications.
#             :

# --------------------------------------------------------------------
# SECTION 2: ENVIRONMENT SETUP
# --------------------------------------------------------------------

show_comments <- TRUE  # Set to FALSE to hide all instructional comments
# Set the base directory
baseDir <- r"(C:\Users\Steve\OneDrive\College\_____DESKTOP ICONS\Remeye\Classes\4317\Mod13)"

# Ensure and set the working directory
tryCatch({
  print(paste("Current working directory:", getwd()))
  if (!dir.exists(baseDir)) stop("Directory does not exist: ", baseDir)
  setwd(baseDir)
  print(paste("Working directory successfully set to:", baseDir))
}, error = function(e) {
  stop("Directory setup failed: ", e$message)
})

# Ensure Output Width for Terminal Display
tryCatch({
  options(width = 80)
}, error = function(e) {
  print("Could not set terminal width.")
})

# --------------------------------------------------------------------
# SECTION 3: DEPENDENCIES & INSTALLATION
# --------------------------------------------------------------------

# Required Libraries
required_packages <- c("animation", "ggplot2")

# Check, Install, and Load Required Libraries
tryCatch({
  for (pkg in required_packages) {
    if (!require(pkg, character.only = TRUE)) {
      cat("Installing package:", pkg, "\n")
      install.packages(pkg, dependencies = TRUE)
      if (!require(pkg, character.only = TRUE)) {
        stop("Failed to load package after installation: ", pkg)
      }
      cat("Successfully loaded:", pkg, "\n")
    } else {
      cat("Package already loaded:", pkg, "\n")
    }
  }
}, error = function(e) {
  stop("Library setup failed: ", e$message)
})

# --------------------------------------------------------------------
# SECTION 9: VISUALIZATION FUNCTIONS
# --------------------------------------------------------------------

# Original Scatter Plot Animation
create_scatter_animation <- function() {
  if (show_comments) cat("Creating scatter plot animation...\n")
  tryCatch({
    saveGIF({
      ani.options(interval = 0.5)
      for (i in 1:10) {
        plot(runif(10), ylim = c(0, 1), col = "blue", pch = 19, 
             main = paste("Random Scatter Plot", i), 
             xlab = "Index", ylab = "Value")
      }
    }, movie.name = "random_scatter.gif", ani.width = 600, ani.height = 400)
    if (show_comments) cat("Animation saved as random_scatter.gif\n")
    return(invisible(NULL))
  }, error = function(e) {
    stop("Error creating scatter animation: ", e$message)
  })
}

# Histogram Animation
create_histogram_animation <- function() {
  if (show_comments) cat("Creating histogram animation...\n")
  tryCatch({
    saveGIF({
      ani.options(interval = 0.5)
      for (i in seq(100, 1000, by = 100)) {
        data <- rnorm(i, mean = 0, sd = 1)
        hist(data, breaks = 30, col = "purple", main = paste("Histogram, n =", i),
             xlab = "Value", ylab = "Frequency", xlim = c(-3, 3), ylim = c(0, 200))
      }
    }, movie.name = "histogram_animation.gif", ani.width = 600, ani.height = 400)
    if (show_comments) cat("Animation saved as histogram_animation.gif\n")
    return(invisible(NULL))
  }, error = function(e) {
    stop("Error creating histogram animation: ", e$message)
  })
}

# Time Series Animation
create_timeseries_animation <- function() {
  if (show_comments) cat("Creating time series animation...\n")
  tryCatch({
    data(economics, package = "ggplot2")
    years <- seq(min(economics$date), max(economics$date), by = "5 years")
    saveGIF({
      ani.options(interval = 0.5)
      for (i in 1:length(years)) {
        temp_data <- economics[economics$date <= years[i], ]
        p <- ggplot(temp_data, aes(x = date, y = unemploy / pop)) +
          geom_line(color = "green") +
          labs(title = paste("Unemployment Rate Up to", format(years[i], "%Y")),
               x = "Date", y = "Unemployment/Population") +
          theme_minimal()
        print(p)
      }
    }, movie.name = "unemployment_animation.gif", ani.width = 600, ani.height = 400)
    if (show_comments) cat("Animation saved as unemployment_animation.gif\n")
    return(invisible(NULL))
  }, error = function(e) {
    stop("Error creating time series animation: ", e$message)
  })
}

# --------------------------------------------------------------------
# SECTION 10: MAIN EXECUTION BLOCK
# --------------------------------------------------------------------

main <- function() {
  if (show_comments) cat("Starting script execution...\n")
  tryCatch({
    if (show_comments) cat("Step 1: Creating scatter plot animation...\n")
    create_scatter_animation()
    if (show_comments) cat("Step 2: Creating histogram animation...\n")
    create_histogram_animation()
    if (show_comments) cat("Step 3: Creating time series animation...\n")
    create_timeseries_animation()
    if (show_comments) cat("Script execution completed successfully.\n")
    return(invisible(NULL))
  }, error = function(e) {
    stop("Script execution failed: ", e$message)
  })
}

# Run the main function
main()