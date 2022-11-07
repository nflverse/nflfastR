# sample games we'll use to check with
game_ids <- c("1999_01_MIN_ATL", "2019_01_GB_CHI")

test_dir <- getwd()

pbp_cache <- tempfile("pbp_cache", fileext = ".rds")

load_test_pbp <- function(pbp = pbp_cache, dir = test_dir){
  if (file.exists(pbp) && !is.null(dir)){
    cli::cli_alert_info("Will return pbp from cache")
    return(readRDS(pbp))
  }
  pbp_data <- build_nflfastR_pbp(game_ids, dir = dir)
  if(!is.null(dir)) saveRDS(pbp_data, pbp)
  pbp_data
}

save_test_object <- function(object){
  obj_name <- deparse(substitute(object))
  tmp_file <- tempfile(obj_name, fileext = ".csv")
  write.csv(object, tmp_file)
  invisible(tmp_file)
}
