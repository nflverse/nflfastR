# NFL Stats Variables

NFL Stats Variables

## Usage

``` r
nfl_stats_variables
```

## Format

A data frame explaining all variables returned by the function
[`calculate_stats()`](https://nflfastr.com/reference/calculate_stats.md).

## Examples

``` r
# \donttest{
nfl_stats_variables
#>                        variable
#> 1                     player_id
#> 2                   player_name
#> 3           player_display_name
#> 4                      position
#> 5                position_group
#> 6                  headshot_url
#> 7                        season
#> 8                          week
#> 9                   season_type
#> 10                  recent_team
#> 11                         team
#> 12                opponent_team
#> 13                        games
#> 14                  completions
#> 15                     attempts
#> 16                passing_yards
#> 17                  passing_tds
#> 18        passing_interceptions
#> 19               sacks_suffered
#> 20              sack_yards_lost
#> 21                 sack_fumbles
#> 22            sack_fumbles_lost
#> 23            passing_air_yards
#> 24    passing_yards_after_catch
#> 25          passing_first_downs
#> 26                  passing_epa
#> 27                 passing_cpoe
#> 28      passing_2pt_conversions
#> 29                         pacr
#> 30                      carries
#> 31                rushing_yards
#> 32                  rushing_tds
#> 33              rushing_fumbles
#> 34         rushing_fumbles_lost
#> 35          rushing_first_downs
#> 36                  rushing_epa
#> 37      rushing_2pt_conversions
#> 38                   receptions
#> 39                      targets
#> 40              receiving_yards
#> 41                receiving_tds
#> 42            receiving_fumbles
#> 43       receiving_fumbles_lost
#> 44          receiving_air_yards
#> 45  receiving_yards_after_catch
#> 46        receiving_first_downs
#> 47                receiving_epa
#> 48    receiving_2pt_conversions
#> 49                         racr
#> 50                 target_share
#> 51              air_yards_share
#> 52                         wopr
#> 53            special_teams_tds
#> 54             def_tackles_solo
#> 55      def_tackles_with_assist
#> 56           def_tackle_assists
#> 57         def_tackles_for_loss
#> 58   def_tackles_for_loss_yards
#> 59           def_fumbles_forced
#> 60                    def_sacks
#> 61               def_sack_yards
#> 62                  def_qb_hits
#> 63            def_interceptions
#> 64       def_interception_yards
#> 65            def_pass_defended
#> 66                      def_tds
#> 67                  def_fumbles
#> 68                 def_safeties
#> 69                   misc_yards
#> 70          fumble_recovery_own
#> 71    fumble_recovery_yards_own
#> 72          fumble_recovery_opp
#> 73    fumble_recovery_yards_opp
#> 74          fumble_recovery_tds
#> 75                    penalties
#> 76                penalty_yards
#> 77                     timeouts
#> 78                 punt_returns
#> 79            punt_return_yards
#> 80              kickoff_returns
#> 81         kickoff_return_yards
#> 82                      fg_made
#> 83                       fg_att
#> 84                    fg_missed
#> 85                   fg_blocked
#> 86                      fg_long
#> 87                       fg_pct
#> 88                 fg_made_0_19
#> 89                fg_made_20_29
#> 90                fg_made_30_39
#> 91                fg_made_40_49
#> 92                fg_made_50_59
#> 93                  fg_made_60_
#> 94               fg_missed_0_19
#> 95              fg_missed_20_29
#> 96              fg_missed_30_39
#> 97              fg_missed_40_49
#> 98              fg_missed_50_59
#> 99                fg_missed_60_
#> 100                fg_made_list
#> 101              fg_missed_list
#> 102             fg_blocked_list
#> 103            fg_made_distance
#> 104          fg_missed_distance
#> 105         fg_blocked_distance
#> 106                    pat_made
#> 107                     pat_att
#> 108                  pat_missed
#> 109                 pat_blocked
#> 110                     pat_pct
#> 111                   gwfg_made
#> 112                    gwfg_att
#> 113                 gwfg_missed
#> 114                gwfg_blocked
#> 115               gwfg_distance
#> 116          gwfg_distance_list
#> 117              fantasy_points
#> 118          fantasy_points_ppr
#>                                                                                                                                                                                                                                                                   description
#> 1                                                                                                                                                                                                                          GSIS player ID. Available if stat_type = 'player'.
#> 2         Short player name as listed in play-by-play data. Please keep in mind that this name is not always unique for one player and can change from season to season and sometimes even within a season. Do not group by this variable. Available if stat_type = 'player'.
#> 3                                                                                                                                                                                                                     Full name of player. Available if stat_type = 'player'.
#> 4                                                                                                                                                                                                                      Position of player. Available if stat_type = 'player'.
#> 5                                                                                                                                                                                                                Position group of player. Available if stat_type = 'player'.
#> 6                                                                                                                                                                                                          URL to a player headshot image. Available if stat_type = 'player'.
#> 7                                                                                                                                                                                                                                                              The NFL season
#> 8                                                                                                                                                                                                                           The NFL week. Available if summary_level = 'week'
#> 9                                                                                                                                                                                                                                         One of 'REG', 'POST', or 'REG+POST'
#> 10                                                                                                                                                                Most recent team player appears in data with. Available if stat_type = 'player' & summary_level = 'season'.
#> 11                                                                                                                                                                                                                                                Team stats are counted for.
#> 12                                                                                                                                                                                                       The opponent team in that week. Available if summary_level = 'week'.
#> 13                                                                                                                                                                                       The number of games where stats were counted. Available if summary_level = 'season'.
#> 14                                                                                                                                                                                                                                            The number of completed passes.
#> 15                                                                                                                                                                                                                         The number of pass attempts as defined by the NFL.
#> 16                                                                                                                                                                                                                                                Yards gained on pass plays.
#> 17                                                                                                                                                                                                                                          The number of passing touchdowns.
#> 18                                                                                                                                                                                                                                        The number of interceptions thrown.
#> 19                                                                                                                                                                                                                                                The Number of times sacked.
#> 20                                                                                                                                                                                                                                                  Yards lost on sack plays.
#> 21                                                                                                                                                                                                                                         The number of sacks with a fumble.
#> 22                                                                                                                                                                                                                                    The number of sacks with a lost fumble.
#> 23                                                                                                                                                                                                                            Passing air yards (includes incomplete passes).
#> 24                                                                                                                       Yards after the catch gained on plays in which player was the passer (this is an unofficial stat and may differ slightly between different sources).
#> 25                                                                                                                                                                                                                                              First downs on pass attempts.
#> 26  Total expected points added on pass attempts and sacks. NOTE: this uses the variable `qb_epa`, which gives QB credit for EPA for up to the point where a receiver lost a fumble after a completed catch and makes EPA work more like passing yards on plays with fumbles.
#> 27                                                                                                                                                                                                                                     Completion percentage over expectation
#> 28                                                                                                                                                                                                                                               Two-point conversion passes.
#> 29                                                                                                                                                             Passing Air Conversion Ratio. PACR = `passing_yards` / `passing_air_yards`. Available if stat_type = 'player'.
#> 30                                                                                                                                             The number of official rush attempts (incl. scrambles and kneel downs). Rushes after a lateral reception don't count as carry.
#> 31                                                                                             Yards gained when rushing with the ball (incl. scrambles and kneel downs). Also includes yards gained after obtaining a lateral on a play that started with a rushing attempt.
#> 32                                                                                                                      The number of rushing touchdowns (incl. scrambles). Also includes touchdowns after obtaining a lateral on a play that started with a rushing attempt.
#> 33                                                                                                                                                                                                                                        The number of rushes with a fumble.
#> 34                                                                                                                                                                                                                                   The number of rushes with a lost fumble.
#> 35                                                                                                                                                                                                                            First downs on rush attempts (incl. scrambles).
#> 36                                                                                                                                                                                                  Expected points added on rush attempts (incl. scrambles and kneel downs).
#> 37                                                                                                                                                                                                                                               Two-point conversion rushes.
#> 38                                                                                                                                                                                     The number of pass receptions. Lateral receptions officially don't count as reception.
#> 39                                                                                                                                                                                                       The number of pass plays where the player was the targeted receiver.
#> 40                                                                                                                                                Yards gained after a pass reception. Includes yards gained after receiving a lateral on a play that started as a pass play.
#> 41                                                                                                                             The number of touchdowns following a pass reception. Also includes touchdowns after receiving a lateral on a play that started as a pass play.
#> 42                                                                                                                                                                                                                              The number of fumbles after a pass reception.
#> 43                                                                                                                                                                                                                         The number of fumbles lost after a pass reception.
#> 44                                                                                                                                                                                                                             Receiving air yards (incl. incomplete passes).
#> 45                                                                                                                         Yards after the catch gained on plays in which player was receiver (this is an unofficial stat and may differ slightly between different sources).
#> 46                                                                                                                                                                                                                                                 First downs on receptions.
#> 47                                                                                                                                                                                                                                       Expected points added on receptions.
#> 48                                                                                                                                                                                                                                           Two-point conversion receptions.
#> 49                                                                                                                                                        Receiver Air Conversion Ratio. RACR = `receiving_yards` / `receiving_air_yards`. Available if stat_type = 'player'.
#> 50                                                                                                                                                                          The share of targets of the player in all targets of his team. Available if stat_type = 'player'.
#> 51                                                                                                                                                            The share of receiving_air_yards of the player in all air_yards of his team. Available if stat_type = 'player'.
#> 52                                                                                                                                                     Weighted Opportunity Rating. WOPR = 1.5 × `target_share` + 0.7 × `air_yards_share`. Available if stat_type = 'player'.
#> 53                                                                                                                                                                                                                    The number of touchdowns scored in special teams plays.
#> 54                                                                                                                                                                                                                                                              Solo tackles.
#> 55                                                                                                                                                                                                                                     Tackles where another player assisted.
#> 56                                                                                                                                                                                                                                         Assist to another player's tackle.
#> 57                                                                                                                                                                                                                                                          Tackles for loss.
#> 58                                                                                                                                                                                                                          Yards lost by the opposing team through a tackle.
#> 59                                                                                                                                                                                                                                                            Forced fumbles.
#> 60                                                                                                                                                                                                                                                           Number of Sacks.
#> 61                                                                                                                                                                                                                            Yards lost by the opposing team through a sack.
#> 62                                                                                                                                                                                                                                                          Number of QB hits
#> 63                                                                                                                                                                                                                                                      Interceptions caught.
#> 64                                                                                                                                                                                                                                          Yards gained after interceptions.
#> 65                                                                                                                                                                                                                                                 Number of defended passes.
#> 66                                                                                                                                                                                                                                                      Defensive touchdowns.
#> 67                                                                                                                                                                                                                                Number of fumbles while playing on defense.
#> 68                                                                                                                                                                                                                                         Tackles that resulted in a safety.
#> 69                                                                                                                                                          Yardage gained/lost that doesn't fall into any other category. Examples are blocked field goals or blocked punts.
#> 70                                                                                                                                                                                                                  Recovered fumbles where the ball was fumbled by own team.
#> 71                                                                                       Yardage gained/lost by a player after he recovered a fumble by his own team. Includes yardage gained/lost where a team mate recovered a fumble and lateraled the ball to the player.
#> 72                                                                                                                                                                                                             Recovered fumbles where the ball was fumbled by opposing team.
#> 73                                                                                  Yardage gained/lost by a player after he recovered a fumble by the opposing team. Includes yardage gained/lost where a team mate recovered a fumble and lateraled the ball to the player.
#> 74                        Touchdowns scored after a fumble recovery. This can be in any unit. And both the own team and the opposing team can have fumbled the ball initially. Includes touchdowns where a team mate recovered a fumble and lateraled the ball to the player.
#> 75                                                                                                                                                                                                                                                          Penalties caused.
#> 76                                                                                                                                                                                                                                            Yardage lost through penalties.
#> 77                                                                                                                                                                                                         Number of timeouts taken by team. Available if stat_type = 'team'.
#> 78                                                                                                                                                                                                                                                  Number of punts returned.
#> 79                                                                                                                                                                                                                      Yardage gained/lost by a player during a punt return.
#> 80                                                                                                                                                                                                                                               Number of kickoffs returned.
#> 81                                                                                                                                                                                                                   Yardage gained/lost by a player during a kickoff return.
#> 82                                                                                                                                                                                                                                            Successful field goal attempts.
#> 83                                                                                                                                                                                                                                                     Attempted field goals.
#> 84                                                                                                                                                                                                                                                        Missed field goals.
#> 85                                                                                                                                                                                                                                   Attempted field goals that were blocked.
#> 86                                                                                                                                                                                                                                       Distance of longest made field goal.
#> 87                                                                                                                                                                                                                              Percentage of successful field goal attempts.
#> 88                                                                                                                                                                                                  Successful field goal attempts where distance was between 0 and 19 yards.
#> 89                                                                                                                                                                                                 Successful field goal attempts where distance was between 20 and 29 yards.
#> 90                                                                                                                                                                                                 Successful field goal attempts where distance was between 30 and 39 yards.
#> 91                                                                                                                                                                                                 Successful field goal attempts where distance was between 40 and 49 yards.
#> 92                                                                                                                                                                                                 Successful field goal attempts where distance was between 50 and 59 yards.
#> 93                                                                                                                                                                                                               Successful field goal attempts where distance was 60+ yards.
#> 94                                                                                                                                                                                                      Missed field goal attempts where distance was between 0 and 19 yards.
#> 95                                                                                                                                                                                                     Missed field goal attempts where distance was between 20 and 29 yards.
#> 96                                                                                                                                                                                                     Missed field goal attempts where distance was between 30 and 39 yards.
#> 97                                                                                                                                                                                                     Missed field goal attempts where distance was between 40 and 49 yards.
#> 98                                                                                                                                                                                                     Missed field goal attempts where distance was between 50 and 59 yards.
#> 99                                                                                                                                                                                                                   Missed field goal attempts where distance was 60+ yards.
#> 100                                                                                                                                                                                                                          Distances of all successful field goal attempts.
#> 101                                                                                                                                                                                                                              Distances of all missed field goal attempts.
#> 102                                                                                                                                                                                                                             Distances of all blocked field goal attempts.
#> 103                                                                                                                                                                                                                                 Sum of distances of all made field goals.
#> 104                                                                                                                                                                                                                               Sum of distances of all missed field goals.
#> 105                                                                                                                                                                                                                              Sum of distances of all blocked field goals.
#> 106                                                                                                                                                                                                                                          Successful extra point attempts.
#> 107                                                                                                                                                                                                                                                   Attempted extra points.
#> 108                                                                                                                                                                                                                                                      Missed extra points.
#> 109                                                                                                                                                                                                                                         Extra points blocked by opponent.
#> 110                                                                                                                                                                                                                            Percentage of successful extra point attempts.
#> 111                                                                                                                                                                                                                              Successful game winning field goal attempts.
#> 112                                                                                                                                                                                                                                       Attempted game winning field goals.
#> 113                                                                                                                                                                                                                                          Missed game winning field goals.
#> 114                                                                                                                                                                                                                     Game winning field goal attempts blocked by opponent.
#> 115                                                                                                                                                                                         Distance of game winning field goal attempt. Available if summary_level = 'week'.
#> 116                                                                                                                                                                                     Distances of game winning field goal attempts. Available if summary_level = 'season'.
#> 117                                                                                                                                                                                                                                                  Standard fantasy points.
#> 118                                                                                                                                                                                                                                                       PPR fantasy points.
# }
```
