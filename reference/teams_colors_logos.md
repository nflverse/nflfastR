# NFL Team names, colors and logo urls.

NFL Team names, colors and logo urls.

## Usage

``` r
teams_colors_logos
```

## Format

A data frame with 36 rows and 10 variables containing NFL team level
information, including franchises in multiple cities:

- team_abbr:

  Team abbreviation

- team_name:

  Complete Team name

- team_id:

  Team id used in the roster function

- team_nick:

  Nickname

- team_conf:

  Conference

- team_division:

  Division

- team_color:

  Primary color

- team_color2:

  Secondary color

- team_color3:

  Tertiary color

- team_color4:

  Quaternary color

- team_logo_wikipedia:

  Url to Team logo on wikipedia

- team_logo_espn:

  Url to higher quality logo on espn

- team_wordmark:

  Url to team wordmarks

- team_conference_logo:

  Url to AFC and NFC logos

- team_league_logo:

  Url to NFL logo

The primary and secondary colors have been taken from nfl.com with some
modifications for better team distinction and most recent team color
themes. The tertiary and quaternary colors are taken from Lee Sharpe's
teamcolors.csv who has taken them from the `teamcolors` package created
by Ben Baumer and Gregory Matthews. The Wikipeadia logo urls are taken
from Lee Sharpe's logos.csv Team wordmarks from nfl.com

## Examples

``` r
# \donttest{
teams_colors_logos
#> # A tibble: 36 × 15
#>    team_abbr team_name      team_id team_nick team_conf team_division team_color
#>    <chr>     <chr>          <chr>   <chr>     <chr>     <chr>         <chr>     
#>  1 ARI       Arizona Cardi… 3800    Cardinals NFC       NFC West      #97233F   
#>  2 ATL       Atlanta Falco… 0200    Falcons   NFC       NFC South     #A71930   
#>  3 BAL       Baltimore Rav… 0325    Ravens    AFC       AFC North     #241773   
#>  4 BUF       Buffalo Bills  0610    Bills     AFC       AFC East      #00338D   
#>  5 CAR       Carolina Pant… 0750    Panthers  NFC       NFC South     #0085CA   
#>  6 CHI       Chicago Bears  0810    Bears     NFC       NFC North     #0B162A   
#>  7 CIN       Cincinnati Be… 0920    Bengals   AFC       AFC North     #FB4F14   
#>  8 CLE       Cleveland Bro… 1050    Browns    AFC       AFC North     #FF3C00   
#>  9 DAL       Dallas Cowboys 1200    Cowboys   NFC       NFC East      #002244   
#> 10 DEN       Denver Broncos 1400    Broncos   AFC       AFC West      #002244   
#> # ℹ 26 more rows
#> # ℹ 8 more variables: team_color2 <chr>, team_color3 <chr>, team_color4 <chr>,
#> #   team_logo_wikipedia <chr>, team_logo_espn <chr>, team_wordmark <chr>,
#> #   team_conference_logo <chr>, team_league_logo <chr>
# }
```
