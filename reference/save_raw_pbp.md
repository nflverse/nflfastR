# Download Raw PBP Data to Local Filesystem

The functions
[`build_nflfastR_pbp()`](https://nflfastr.com/reference/build_nflfastR_pbp.md)
and [`fast_scraper()`](https://nflfastr.com/reference/fast_scraper.md)
support loading raw pbp data from local file systems instead of Github
servers. This function is intended to help setting this up. It loads raw
pbp data and saves it in the given directory split by season in
subdirectories.

## Usage

``` r
save_raw_pbp(
  game_ids,
  dir = getOption("nflfastR.raw_directory", default = NULL)
)
```

## Arguments

- game_ids:

  A vector of nflverse game IDs.

- dir:

  Path to local directory (defaults to option "nflfastR.raw_directory").
  nflfastR will download the raw game files split by season into one sub
  directory per season.

## Value

The function returns a data frame with one row for each downloaded file
and the following columns:

- `success` if the HTTP request was successfully performed, regardless
  of the response status code. This is `FALSE` in case of a network
  error, or in case you tried to resume from a server that did not
  support this. A value of `NA` means the download was interrupted while
  in progress.

- `status_code` the HTTP status code from the request. A successful
  download is usually `200` for full requests or `206` for resumed
  requests. Anything else could indicate that the downloaded file
  contains an error page instead of the requested content.

- `resumefrom` the file size before the request, in case a download was
  resumed.

- `url` final url (after redirects) of the request.

- `destfile` downloaded file on disk.

- `error` if `success == FALSE` this column contains an error message.

- `type` the `Content-Type` response header value.

- `modified` the `Last-Modified` response header value.

- `time` total elapsed download time for this file in seconds.

- `headers` vector with http response headers for the request.

## See also

[`build_nflfastR_pbp()`](https://nflfastr.com/reference/build_nflfastR_pbp.md),
[`missing_raw_pbp()`](https://nflfastr.com/reference/missing_raw_pbp.md)

## Examples

``` r
# \donttest{
# CREATE LOCAL TEMP DIRECTORY
local_dir <- tempdir()

# LOAD AND SAVE A GAME TO TEMP DIRECTORY
save_raw_pbp("2021_20_BUF_KC", dir = local_dir)
#> # A tibble: 1 × 10
#>   success status_code resumefrom url    destfile error type  modified
#>   <lgl>         <dbl>      <dbl> <chr>  <chr>    <chr> <chr> <dttm>  
#> 1 TRUE            200          0 https… /tmp/Rt… NA    appl… NA      
#> # ℹ 2 more variables: time <dbl>, headers <list>

# REMOVE THE DIRECTORY
unlink(file.path(local_dir, 2021))
# }
```
