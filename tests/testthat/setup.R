# Run all tests without the progress messages of nflfastR. The option is reset
# when the tests are done.
withr::local_options(
  nflfastR.verbose = FALSE,
  .local_envir = teardown_env()
)
