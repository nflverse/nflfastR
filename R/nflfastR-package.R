#' @details # Parallel Processing and Progress Updates in nflfastR
#'
#' ## Preface
#'
#' Prior to nflfastR v4.0, parallel processing could be activated with an
#' argument `pp` in the relevant functions and progress updates were always
#' shown. Both of these methods are bad practice and were therefore removed
#' in nflfastR v4.0
#'
#' The next sections describe how to make nflfastR work in parallel processes
#' and show progress updates if the user wants to.
#'
#' ## More Speed Using Parallel Processing
#'
#' Nearly all nflfastR functions support parallel processing
#' using [furrr::future_map()] if it is enabled by a call to [future::plan()]
#' prior to the function call.
#' Please see the documentation of the functions for detailed information.
#'
#' As an example, the following code block will resolve all function calls in the
#' current session using multiple sessions in the background and load play-by-play
#' data for the 2018 through 2020 seasons:
#' ```
#' future::plan("multisession")
#' load_pbp(2018:2020)
#' build_nflfastR_pbp(game_ids)
#' ```
#' We recommended choosing a default parallel processing method and saving it
#' as an environment variable in the R user profile to make sure all futures
#' will be resolved with the chosen method by default.
#' This can be done by following the below given steps.
#'
#' First, tun the following line and the user profile should be opened automatically.
#' If you haven't saved any environment variables yet, this will be an empty file.
#' ```
#' usethis::edit_r_environ()
#'```
#' In the opened file add the next line, then save the file and restart your R session.
#' Please note that this example sets "multisession" as default. For most users
#' this should be the appropriate plan but please make sure it truly is.
#' ```
#' R_FUTURE_PLAN="multisession"
#' ```
#' After the session is freshly retarted please check if the above method worked
#' by running the next line. If the output is `FALSE` you successfully set up a
#' default non-sequential [future::plan()]. If the output is `TRUE` all functions
#' will behave like they were called with [purrr::map()] and NOT in multisession.
#' ```
#' inherits(future::plan(), "sequential")
#' ```
#'
#' ## Get Progress Updates while Functions are Running
#'
#' Most nflfastR functions are able to show progress updates
#' using [progressr::progressor()] if they are turned on before the function is
#' called. There are at least two basic ways to do this by either activating
#' progress updates globally (for the current session) with
#' ```
#' progressr::handlers(global = TRUE)
#' ```
#' or by piping the function call into [progressr::with_progress()]:
#' ```
#' load_pbp(2018:2020) %>%
#'   progressr::with_progress()
#' ```
#'
#' For more information how to work with progress handlers please see [progressr::progressr].
#'
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
#' @import dplyr
#' @importFrom cli rule
#' @importFrom curl curl_fetch_memory
# @importFrom DBI dbConnect dbExistsTable dbWriteTable dbDisconnect dbRemoveTable dbExecute dbGetInfo
#' @importFrom furrr future_map_chr future_map_dfr future_map
#' @importFrom future plan
#' @importFrom glue glue glue_sql
# @importFrom gsisdecoder decode_ids
#' @importFrom janitor clean_names
#' @importFrom lifecycle deprecated is_present deprecate_warn
#' @importFrom lubridate period_to_seconds ms month today
#' @importFrom magrittr %>%
#' @importFrom mgcv predict.bam
# @importFrom purrr walk
#' @importFrom progressr progressor
# @importFrom qs qdeserialize qread
#' @importFrom rlang .data inform dots_list
# @importFrom RSQLite SQLite
#' @importFrom stats predict na.omit
#' @importFrom stringr str_sub str_replace_all str_length str_extract str_detect str_trim str_remove_all str_split str_extract_all
#' @importFrom tibble as_tibble tibble
#' @importFrom tidyr unnest_wider unnest
#' @importFrom tidyselect any_of ends_with
#' @importFrom usethis ui_done ui_todo ui_oops ui_info ui_stop ui_value
#' @importFrom utils packageVersion
#' @importFrom xgboost getinfo
## usethis namespace: end
NULL
