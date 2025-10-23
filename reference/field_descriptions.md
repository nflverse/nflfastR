# nflfastR Field Descriptions

nflfastR Field Descriptions

## Usage

``` r
field_descriptions
```

## Format

A data frame including names and descriptions of all variables in an
nflfastR dataset.

## See also

The searchable table on the [nflfastR
website](https://www.nflfastr.com/articles/field_descriptions.html)

## Examples

``` r
# \donttest{
field_descriptions
#> # A tibble: 372 × 2
#>    Field        Description                                                     
#>    <chr>        <chr>                                                           
#>  1 play_id      Numeric play id that when used with game_id and drive provides …
#>  2 game_id      Ten digit identifier for NFL game.                              
#>  3 old_game_id  Legacy NFL game ID.                                             
#>  4 home_team    String abbreviation for the home team.                          
#>  5 away_team    String abbreviation for the away team.                          
#>  6 season_type  'REG' or 'POST' indicating if the game belongs to regular or po…
#>  7 week         Season week.                                                    
#>  8 posteam      String abbreviation for the team with possession.               
#>  9 posteam_type String indicating whether the posteam team is home or away.     
#> 10 defteam      String abbreviation for the team on defense.                    
#> # ℹ 362 more rows
# }
```
