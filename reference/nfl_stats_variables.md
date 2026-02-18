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
#> 10                      game_id
#> 11                  recent_team
#> 12                         team
#> 13                opponent_team
#> 14                        games
#> 15                  completions
#> 16                     attempts
#> 17                passing_yards
#> 18                  passing_tds
#> 19        passing_interceptions
#> 20               sacks_suffered
#> 21              sack_yards_lost
#> 22                 sack_fumbles
#> 23            sack_fumbles_lost
#> 24            passing_air_yards
#> 25    passing_yards_after_catch
#> 26          passing_first_downs
#> 27                  passing_epa
#> 28                 passing_cpoe
#> 29      passing_2pt_conversions
#> 30                         pacr
#> 31                   passing_10
#> 32                   passing_16
#> 33                   passing_20
#> 34                   passing_40
#> 35                      carries
#> 36                rushing_yards
#> 37                  rushing_tds
#> 38              rushing_fumbles
#> 39         rushing_fumbles_lost
#> 40          rushing_first_downs
#> 41                  rushing_epa
#> 42      rushing_2pt_conversions
#> 43                   rushing_10
#> 44                   rushing_12
#> 45                   rushing_20
#> 46                   rushing_40
#> 47                   receptions
#> 48                      targets
#> 49              receiving_yards
#> 50                receiving_tds
#> 51            receiving_fumbles
#> 52       receiving_fumbles_lost
#> 53          receiving_air_yards
#> 54  receiving_yards_after_catch
#> 55        receiving_first_downs
#> 56                receiving_epa
#> 57    receiving_2pt_conversions
#> 58                 receiving_10
#> 59                 receiving_16
#> 60                 receiving_20
#> 61                 receiving_40
#> 62                         racr
#> 63                 target_share
#> 64              air_yards_share
#> 65                         wopr
#> 66            special_teams_tds
#> 67             def_tackles_solo
#> 68      def_tackles_with_assist
#> 69           def_tackle_assists
#> 70         def_tackles_for_loss
#> 71   def_tackles_for_loss_yards
#> 72           def_fumbles_forced
#> 73                    def_sacks
#> 74               def_sack_yards
#> 75                  def_qb_hits
#> 76            def_interceptions
#> 77       def_interception_yards
#> 78            def_pass_defended
#> 79                      def_tds
#> 80                  def_fumbles
#> 81                 def_safeties
#> 82                   misc_yards
#> 83          fumble_recovery_own
#> 84    fumble_recovery_yards_own
#> 85          fumble_recovery_opp
#> 86    fumble_recovery_yards_opp
#> 87          fumble_recovery_tds
#> 88                    penalties
#> 89                penalty_yards
#> 90                     timeouts
#> 91                 punt_returns
#> 92            punt_return_yards
#> 93              kickoff_returns
#> 94         kickoff_return_yards
#> 95                      fg_made
#> 96                       fg_att
#> 97                    fg_missed
#> 98                   fg_blocked
#> 99                      fg_long
#> 100                      fg_pct
#> 101                fg_made_0_19
#> 102               fg_made_20_29
#> 103               fg_made_30_39
#> 104               fg_made_40_49
#> 105               fg_made_50_59
#> 106                 fg_made_60_
#> 107              fg_missed_0_19
#> 108             fg_missed_20_29
#> 109             fg_missed_30_39
#> 110             fg_missed_40_49
#> 111             fg_missed_50_59
#> 112               fg_missed_60_
#> 113                fg_made_list
#> 114              fg_missed_list
#> 115             fg_blocked_list
#> 116            fg_made_distance
#> 117          fg_missed_distance
#> 118         fg_blocked_distance
#> 119                    pat_made
#> 120                     pat_att
#> 121                  pat_missed
#> 122                 pat_blocked
#> 123                     pat_pct
#> 124                   gwfg_made
#> 125                    gwfg_att
#> 126                 gwfg_missed
#> 127                gwfg_blocked
#> 128               gwfg_distance
#> 129          gwfg_distance_list
#> 130              fantasy_points
#> 131          fantasy_points_ppr
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
#> 10                                                                                                                                            The nflverse game id of the form '{season}_{week}_{away abbreviation}_{home abbreviation}'. Available if summary_level = 'week'
#> 11                                                                                                                                                                Most recent team player appears in data with. Available if stat_type = 'player' & summary_level = 'season'.
#> 12                                                                                                                                                                                                                                                Team stats are counted for.
#> 13                                                                                                                                                                                                       The opponent team in that week. Available if summary_level = 'week'.
#> 14                                                                                                                                                                                       The number of games where stats were counted. Available if summary_level = 'season'.
#> 15                                                                                                                                                                                                                                            The number of completed passes.
#> 16                                                                                                                                                                                                                         The number of pass attempts as defined by the NFL.
#> 17                                                                                                                                                                                                                                                Yards gained on pass plays.
#> 18                                                                                                                                                                                                                                          The number of passing touchdowns.
#> 19                                                                                                                                                                                                                                        The number of interceptions thrown.
#> 20                                                                                                                                                                                                                                                The Number of times sacked.
#> 21                                                                                                                                                                                                                                                  Yards lost on sack plays.
#> 22                                                                                                                                                                                                                                         The number of sacks with a fumble.
#> 23                                                                                                                                                                                                                                    The number of sacks with a lost fumble.
#> 24                                                                                                                                                                                                                            Passing air yards (includes incomplete passes).
#> 25                                                                                                                       Yards after the catch gained on plays in which player was the passer (this is an unofficial stat and may differ slightly between different sources).
#> 26                                                                                                                                                                                                                                              First downs on pass attempts.
#> 27  Total expected points added on pass attempts and sacks. NOTE: this uses the variable `qb_epa`, which gives QB credit for EPA for up to the point where a receiver lost a fumble after a completed catch and makes EPA work more like passing yards on plays with fumbles.
#> 28                                                                                                                                                                                                                                     Completion percentage over expectation
#> 29                                                                                                                                                                                                                                               Two-point conversion passes.
#> 30                                                                                                                                                             Passing Air Conversion Ratio. PACR = `passing_yards` / `passing_air_yards`. Available if stat_type = 'player'.
#> 31                                                                                                                                                                                The number of passes that gained 10 or more yards. Some define this as an 'explosive' play.
#> 32                                                                                                                                                                                The number of passes that gained 16 or more yards. Some define this as an 'explosive' play.
#> 33                                                                                                                                                                                The number of passes that gained 20 or more yards. Some define this as an 'explosive' play.
#> 34                                                                                                                                                                                The number of passes that gained 40 or more yards. Some define this as an 'explosive' play.
#> 35                                                                                                                                             The number of official rush attempts (incl. scrambles and kneel downs). Rushes after a lateral reception don't count as carry.
#> 36                                                                                             Yards gained when rushing with the ball (incl. scrambles and kneel downs). Also includes yards gained after obtaining a lateral on a play that started with a rushing attempt.
#> 37                                                                                                                      The number of rushing touchdowns (incl. scrambles). Also includes touchdowns after obtaining a lateral on a play that started with a rushing attempt.
#> 38                                                                                                                                                                                                                                        The number of rushes with a fumble.
#> 39                                                                                                                                                                                                                                   The number of rushes with a lost fumble.
#> 40                                                                                                                                                                                                                            First downs on rush attempts (incl. scrambles).
#> 41                                                                                                                                                                                                  Expected points added on rush attempts (incl. scrambles and kneel downs).
#> 42                                                                                                                                                                                                                                               Two-point conversion rushes.
#> 43                                                                                                                                                                                  The number of runs that gained 10 or more yards. Some define this as an 'explosive' play.
#> 44                                                                                                                                                                                  The number of runs that gained 12 or more yards. Some define this as an 'explosive' play.
#> 45                                                                                                                                                                                  The number of runs that gained 20 or more yards. Some define this as an 'explosive' play.
#> 46                                                                                                                                                                                  The number of runs that gained 40 or more yards. Some define this as an 'explosive' play.
#> 47                                                                                                                                                                                     The number of pass receptions. Lateral receptions officially don't count as reception.
#> 48                                                                                                                                                                                                       The number of pass plays where the player was the targeted receiver.
#> 49                                                                                                                                                Yards gained after a pass reception. Includes yards gained after receiving a lateral on a play that started as a pass play.
#> 50                                                                                                                             The number of touchdowns following a pass reception. Also includes touchdowns after receiving a lateral on a play that started as a pass play.
#> 51                                                                                                                                                                                                                              The number of fumbles after a pass reception.
#> 52                                                                                                                                                                                                                         The number of fumbles lost after a pass reception.
#> 53                                                                                                                                                                                                                             Receiving air yards (incl. incomplete passes).
#> 54                                                                                                                         Yards after the catch gained on plays in which player was receiver (this is an unofficial stat and may differ slightly between different sources).
#> 55                                                                                                                                                                                                                                                 First downs on receptions.
#> 56                                                                                                                                                                                                                                       Expected points added on receptions.
#> 57                                                                                                                                                                                                                                           Two-point conversion receptions.
#> 58                                                                                                                                                                            The number of receptions that gained 10 or more yards. Some define this as an 'explosive' play.
#> 59                                                                                                                                                                            The number of receptions that gained 16 or more yards. Some define this as an 'explosive' play.
#> 60                                                                                                                                                                            The number of receptions that gained 20 or more yards. Some define this as an 'explosive' play.
#> 61                                                                                                                                                                            The number of receptions that gained 40 or more yards. Some define this as an 'explosive' play.
#> 62                                                                                                                                                        Receiver Air Conversion Ratio. RACR = `receiving_yards` / `receiving_air_yards`. Available if stat_type = 'player'.
#> 63                                                                                                                                                                          The share of targets of the player in all targets of his team. Available if stat_type = 'player'.
#> 64                                                                                                                                                            The share of receiving_air_yards of the player in all air_yards of his team. Available if stat_type = 'player'.
#> 65                                                                                                                                                     Weighted Opportunity Rating. WOPR = 1.5 × `target_share` + 0.7 × `air_yards_share`. Available if stat_type = 'player'.
#> 66                                                                                                                                                                                                                    The number of touchdowns scored in special teams plays.
#> 67                                                                                                                                                                                                                                                              Solo tackles.
#> 68                                                                                                                                                                                                                                     Tackles where another player assisted.
#> 69                                                                                                                                                                                                                                         Assist to another player's tackle.
#> 70                                                                                                                                                                                                                                                          Tackles for loss.
#> 71                                                                                                                                                                                                                          Yards lost by the opposing team through a tackle.
#> 72                                                                                                                                                                                                                                                            Forced fumbles.
#> 73                                                                                                                                                                                                                                                           Number of Sacks.
#> 74                                                                                                                                                                                                                            Yards lost by the opposing team through a sack.
#> 75                                                                                                                                                                                                                                                          Number of QB hits
#> 76                                                                                                                                                                                                                                                      Interceptions caught.
#> 77                                                                                                                                                                                                                                          Yards gained after interceptions.
#> 78                                                                                                                                                                                                                                                 Number of defended passes.
#> 79                                                                                                                                                                                                                                                      Defensive touchdowns.
#> 80                                                                                                                                                                                                                                Number of fumbles while playing on defense.
#> 81                                                                                                                                                                                                                                         Tackles that resulted in a safety.
#> 82                                                                                                                                                          Yardage gained/lost that doesn't fall into any other category. Examples are blocked field goals or blocked punts.
#> 83                                                                                                                                                                                                                  Recovered fumbles where the ball was fumbled by own team.
#> 84                                                                                       Yardage gained/lost by a player after he recovered a fumble by his own team. Includes yardage gained/lost where a team mate recovered a fumble and lateraled the ball to the player.
#> 85                                                                                                                                                                                                             Recovered fumbles where the ball was fumbled by opposing team.
#> 86                                                                                  Yardage gained/lost by a player after he recovered a fumble by the opposing team. Includes yardage gained/lost where a team mate recovered a fumble and lateraled the ball to the player.
#> 87                        Touchdowns scored after a fumble recovery. This can be in any unit. And both the own team and the opposing team can have fumbled the ball initially. Includes touchdowns where a team mate recovered a fumble and lateraled the ball to the player.
#> 88                                                                                                                                                                                                                                                          Penalties caused.
#> 89                                                                                                                                                                                                                                            Yardage lost through penalties.
#> 90                                                                                                                                                                                                         Number of timeouts taken by team. Available if stat_type = 'team'.
#> 91                                                                                                                                                                                                                                                  Number of punts returned.
#> 92                                                                                                                                                                                                                      Yardage gained/lost by a player during a punt return.
#> 93                                                                                                                                                                                                                                               Number of kickoffs returned.
#> 94                                                                                                                                                                                                                   Yardage gained/lost by a player during a kickoff return.
#> 95                                                                                                                                                                                                                                            Successful field goal attempts.
#> 96                                                                                                                                                                                                                                                     Attempted field goals.
#> 97                                                                                                                                                                                                                                                        Missed field goals.
#> 98                                                                                                                                                                                                                                   Attempted field goals that were blocked.
#> 99                                                                                                                                                                                                                                       Distance of longest made field goal.
#> 100                                                                                                                                                                                                                             Percentage of successful field goal attempts.
#> 101                                                                                                                                                                                                 Successful field goal attempts where distance was between 0 and 19 yards.
#> 102                                                                                                                                                                                                Successful field goal attempts where distance was between 20 and 29 yards.
#> 103                                                                                                                                                                                                Successful field goal attempts where distance was between 30 and 39 yards.
#> 104                                                                                                                                                                                                Successful field goal attempts where distance was between 40 and 49 yards.
#> 105                                                                                                                                                                                                Successful field goal attempts where distance was between 50 and 59 yards.
#> 106                                                                                                                                                                                                              Successful field goal attempts where distance was 60+ yards.
#> 107                                                                                                                                                                                                     Missed field goal attempts where distance was between 0 and 19 yards.
#> 108                                                                                                                                                                                                    Missed field goal attempts where distance was between 20 and 29 yards.
#> 109                                                                                                                                                                                                    Missed field goal attempts where distance was between 30 and 39 yards.
#> 110                                                                                                                                                                                                    Missed field goal attempts where distance was between 40 and 49 yards.
#> 111                                                                                                                                                                                                    Missed field goal attempts where distance was between 50 and 59 yards.
#> 112                                                                                                                                                                                                                  Missed field goal attempts where distance was 60+ yards.
#> 113                                                                                                                                                                                                                          Distances of all successful field goal attempts.
#> 114                                                                                                                                                                                                                              Distances of all missed field goal attempts.
#> 115                                                                                                                                                                                                                             Distances of all blocked field goal attempts.
#> 116                                                                                                                                                                                                                                 Sum of distances of all made field goals.
#> 117                                                                                                                                                                                                                               Sum of distances of all missed field goals.
#> 118                                                                                                                                                                                                                              Sum of distances of all blocked field goals.
#> 119                                                                                                                                                                                                                                          Successful extra point attempts.
#> 120                                                                                                                                                                                                                                                   Attempted extra points.
#> 121                                                                                                                                                                                                                                                      Missed extra points.
#> 122                                                                                                                                                                                                                                         Extra points blocked by opponent.
#> 123                                                                                                                                                                                                                            Percentage of successful extra point attempts.
#> 124                                                                                                                                                                                                                              Successful game winning field goal attempts.
#> 125                                                                                                                                                                                                                                       Attempted game winning field goals.
#> 126                                                                                                                                                                                                                                          Missed game winning field goals.
#> 127                                                                                                                                                                                                                     Game winning field goal attempts blocked by opponent.
#> 128                                                                                                                                                                                         Distance of game winning field goal attempt. Available if summary_level = 'week'.
#> 129                                                                                                                                                                                     Distances of game winning field goal attempts. Available if summary_level = 'season'.
#> 130                                                                                                                                                                                                                                                  Standard fantasy points.
#> 131                                                                                                                                                                                                                                                       PPR fantasy points.
# }
```
