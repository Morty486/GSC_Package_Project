


install_gen_corseqsort <- function() {
  if (!requireNamespace("GenCorSeqSort", quietly = TRUE)) {
    message("Installing GenCorSeqSort from a local directory...")
    devtools::install_local(
  "~/Documents/UIC/Fall 2024/Computational Statistics/extra_info_3variate.sorting/Q1/Q1/GenCorSeqSort")
  }
}
