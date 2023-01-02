## Simulation Function ##
simulation <- function(samp_size, theta, runs) {
  
  # Initialize an empty matrix for coverage probabilities
  coverage_prob <- matrix(NA, nrow = runs, ncol = 3)
  colnames(coverage_prob) <- c("exact", "wilson", "asymptotic")

  # Initialize an empty matrix for width of intervals
  width <- matrix(NA, nrow = runs, ncol = 3)
  colnames(width) <- c("exact", "wilson", "asymptotic")


  for (i in 1:runs) {
    # Simulate samples using theta
    x <- sum(rbinom(samp_size, 1, theta))

    # Calculate estimates & intervals using all three methods
    out <- binconf(x, samp_size, alpha = 0.05, method = "all")
    
    # Calculate interval widths based on output
    width[i, ] <- out[, 3] - out[, 2]

    # Confirm if theta is contained in each calculated interval
    # 1 = theta is in interval; 0 = theta is not in interval
    for (j in 1:3) {
      if (theta > out[j, 2] && theta < out[j, 3]) {
        coverage_prob[i, j] <- 1
      } else {
        coverage_prob[i, j] <- 0
      }
    }
  }

  # Calculate coverage probabilities for all tests
  # Returns 1x3 vector
  coverage_prob_sum <- apply(coverage_prob, MARGIN = 2, FUN = sum) / runs
  
  # Take the mean of all widths from the simulation by test type
  # Returns 1x3 vector
  width_sum <- apply(width, MARGIN = 2, FUN = mean)


  # Return coverage probabilities and widths for all tests
  return(list(coverage_prob = coverage_prob_sum, 
              width = width_sum))
}




## Table Function ##
table_fnt <- function(array, theta, samp_size, caption) {

  # Arrange interval methods alphabetically
  new_order <- sort(colnames(array[, , 1]))
  
  # Stratify array so each theta value has it's own matrix
  # Round values to 2 decimal places
  # Change row order so interval methods are listed alphabetically
  theta_1 <- round(array[, , 1][, new_order], 2)
  theta_2 <- round(array[, , 2][, new_order], 2)


  # Combine the theta matrices into one 2D table
  total <- cbind.data.frame(capitalize(new_order),
                            t(theta_1), t(theta_2))
  # Set numeric column numbers so table can be fed through flextable
  colnames(total) <- 1:ncol(total)


  # Initialize flextable 
  ft1 <- flextable(total)
  ft1 <- ft1 |>
    set_caption(paste(caption)) |> # Set title
    bold(j = 1, part = "body") |> # Bold the test types
    add_header_row( # Input theta values in top row
      colwidths = c(1, 3, 3),
      values = c(
        "",
        paste("θ =", theta[1]),
        paste("θ =", theta[2])
      )
    ) |>
    align_text_col(align = "center") |> # Center the text of the table  
    theme_vanilla() |> # Set theme
    set_header_labels(values = list( # Rename columns with correct sample sizes
      "1" = "",
      "2" = paste("N =", samp_size[1]),
      "3" = paste("N =", samp_size[2]),
      "4" = paste("N =", samp_size[3]),
      "5" = paste("N =", samp_size[1]),
      "6" = paste("N =", samp_size[2]),
      "7" = paste("N =", samp_size[3])
    ))

  # Return flextable object
  return(table = ft1)
}



# Plotting Function ##
plotter <- function(array, samp_size, theta, caption) {
  
  # Create data frame to plot the data
  # Create column for sample sizes
  n <- as.factor(rep(samp_size, 6)) 
  # Convert array from simulation function to a vector
  values <- as.vector(array)
  # Create column for test type
  test <- rep(capitalize(variable.names(array)), each = 3, times = 2)
  # Create column for theta values
  thetas <- rep(theta, each = 9)

  # Combine the above columns into total data frame
  total <- cbind.data.frame(n, values, test, thetas)

  # Set color palette
  col <- c("orchid4", "dodgerblue2", "darkgoldenrod2")

  # Create plot:
  p <- ggplot(data = total, aes(x = n, y = values, color = test)) +
    geom_point(size = 3.5, alpha = 0.8) + # Set points for each estimate by sample size
    geom_line(aes(group = test), alpha = 0.8) + # Connect points by test type
    facet_grid(~thetas) + # facet based on the two theta values
    xlab("Sample Size") + # Edit x-axis
    ylab(paste(caption)) + # Edit y-axis based on function input
    ggtitle(paste(caption, "by Sample Size (θ = 0.05 & θ = 0.15)")) + # Edit title
    scale_color_manual(values = col, name = "Interval Type") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold")) # Center title


  # Return the completed plot
  return(plot = p)
}
