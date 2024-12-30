


#' The empirical results for n combinations of specified correlations
#'
#' @param n number of combinations of specified correlations want to generate
#' @param lst A list of functions which generate data under specified marginal distributions separately
#' @param dist_X name of ditribution X
#' @param dist_Y name of ditribution Y
#' @param dist_Z name of ditribution Z
#'
#' @return
#' @export
#'
#' @examples
TableGeneration <- function(n, lst, dist_X, dist_Y, dist_Z) {
  # Initialize an empty list to store the results of each replication
  results_list <- list()

  # Loop to replicate the function n times
  for (i in 1:n) {
    # Call the GenCorrTable function and store the result
    result <- GenCorrTable(10000, lst, GenCorMat(lst))

    # Add a replication index to track the iteration
    result$replication <- i

    # Append to the list
    results_list[[i]] <- result
  }

  # Combine all the results into a single data frame
  combined_results <- do.call(rbind, results_list)

  # Remove the "replication" column
  combined_results <- combined_results[, !colnames(combined_results) %in% "replication"]

  # Generate the feasible correlation matrix bounds
  bounds <- GenBoundTable(10000, lst)

  # Convert the bounds matrix to a readable data frame format
  bounds_table <- as.data.frame(bounds)
  colnames(bounds_table) <- c("X", "Y", "Z")
  bounds_table <- cbind(Variable = c("X", "Y", "Z"), bounds_table)
  rownames(bounds_table) <- NULL

  # Add columns specifying the distributions
  dist_data <- data.frame(
    X = rep(dist_X, nrow(combined_results)),
    Y = rep(dist_Y, nrow(combined_results)),
    Z = rep(dist_Z, nrow(combined_results))
  )

  # Combine the distribution columns with the results
  final_table <- cbind(dist_data, combined_results)

  # Replace repeated values with blanks after the first row
  final_table$X[-1] <- ""
  final_table$Y[-1] <- ""
  final_table$Z[-1] <- ""

  # Rename columns for better readability
  colnames(final_table) <- c(
    "X", "Y", "Z",
    "delta_spec_XY", "delta_emp_XY",
    "delta_spec_XZ", "delta_emp_XZ",
    "delta_spec_YZ", "delta_emp_YZ"
  )

  # Create a list to store both tables
  result_tables <- list(
    bounds_table = bounds_table,
    correlation_table = final_table
  )

  # Return the tables
  return(result_tables)
}



