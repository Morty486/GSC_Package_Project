GenCorrTable <- function(n, lst, cor_mat, row.method = 1, digits = 4) {
  if (!(length(lst) == 2 || length(lst) == 3)) {
    stop("This function only can be applied to 2 or 3 variables")
  }
  if (!nrow(cor_mat) == length(lst)) {
    stop("Dimension of correlation matrix does not match the number of variables! \n")
  }

  res <- GenCorDataBiTri1.1(n = n, lst = lst, cor_mat = cor_mat, row.method = row.method)
  spec_cor <- res$spec_cor
  emp_cor  <- res$gen_cor

  if (length(lst) == 2) {
    spec_xy <- round(spec_cor[1, 2], digits)
    emp_xy  <- round(emp_cor[1, 2],  digits)

    # Return a single-row data frame with 2 columns
    output_tbl <- data.frame(
      spec_XY = spec_xy,
      emp_XY  = emp_xy,
      row.names = NULL
    )
  } else {
    spec_xy <- round(spec_cor[1, 2], digits)
    emp_xy  <- round(emp_cor[1, 2],  digits)

    spec_xz <- round(spec_cor[1, 3], digits)
    emp_xz  <- round(emp_cor[1, 3],  digits)

    spec_yz <- round(spec_cor[2, 3], digits)
    emp_yz  <- round(emp_cor[2, 3],  digits)

    output_tbl <- data.frame(
      spec_XY = spec_xy,
      emp_XY  = emp_xy,
      spec_XZ = spec_xz,
      emp_XZ  = emp_xz,
      spec_YZ = spec_yz,
      emp_YZ  = emp_yz,
      row.names = NULL
    )
  }
  return(output_tbl)
}

