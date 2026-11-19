# load_sysdata_booster names the model and the xgboost versions

    Code
      load_sysdata_booster("test_error", stop("boom"), "0.0.1", "test model")
    Condition
      Error:
      ! Can't load the test model.
      i It was saved with xgboost "0.0.1" and you have "<installed>".
      x boom

