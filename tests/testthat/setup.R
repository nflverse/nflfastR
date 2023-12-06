# CRAN incoming checks can fail if tests use more than 2 cores.
# We can run in this problem by using data.table https://github.com/Rdatatable/data.table/issues/5658
# R CMD Check throws the NOTE because CRAN sets
# _R_CHECK_TEST_TIMING_CPU_TO_ELAPSED_THRESHOLD_ and/or
# _R_CHECK_EXAMPLE_TIMING_CPU_TO_ELAPSED_THRESHOLD_to "2.5"
# (EDIT: They probably use different ones in R DEVEL)
# which means "more than two cores are running".
# We check for these environment variables and if we find them
# we set data.table to two threads.
cpu_check <- as.numeric(
  c(
    Sys.getenv("_R_CHECK_EXAMPLE_TIMING_CPU_TO_ELAPSED_THRESHOLD_", unset = 0),
    Sys.getenv("_R_CHECK_TEST_TIMING_CPU_TO_ELAPSED_THRESHOLD_", unset = 0)
  )
)

if (any(is.na(cpu_check)) || any(cpu_check[!is.na(cpu_check)] != 0)) {
  cores <- min(
    floor(as.integer(Sys.getenv("_R_CHECK_EXAMPLE_TIMING_CPU_TO_ELAPSED_THRESHOLD_"))),
    floor(as.integer(Sys.getenv("_R_CHECK_TEST_TIMING_CPU_TO_ELAPSED_THRESHOLD_"))),
    2L,
    na.rm = TRUE
  )
  Sys.setenv("OMP_THREAD_LIMIT" = cores)
  data.table::setDTthreads(cores)
}
