# Helper function to replace the internal calls to the models
# with a call to the fastrmodels package
models <- c(
  "ep_model,",
  "wp_model,",
  "wp_model_spread,",
  "fg_model,",
  "cp_model,",
  "xyac_model,",
  "xpass_model,"
)

purrr::walk(models, function(model){
  xfun::gsub_dir(
    # paste0(model,"(?![:alpha:]+)"),
    model,
    paste0("fastrmodels::", model),
    dir = usethis::proj_path("R"),
    ext = "R"
  )
})
