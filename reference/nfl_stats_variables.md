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
#> 31                      carries
#> 32                rushing_yards
#> 33                  rushing_tds
#> 34              rushing_fumbles
#> 35         rushing_fumbles_lost
#> 36          rushing_first_downs
#> 37                  rushing_epa
#> 38      rushing_2pt_conversions
#> 39                   receptions
#> 40                      targets
#> 41              receiving_yards
#> 42                receiving_tds
#> 43            receiving_fumbles
#> 44       receiving_fumbles_lost
#> 45          receiving_air_yards
#> 46  receiving_yards_after_catch
#> 47        receiving_first_downs
#> 48                receiving_epa
#> 49    receiving_2pt_conversions
#> 50                         racr
#> 51                 target_share
#> 52              air_yards_share
#> 53                         wopr
#> 54            special_teams_tds
#> 55             def_tackles_solo
#> 56      def_tackles_with_assist
#> 57           def_tackle_assists
#> 58         def_tackles_for_loss
#> 59   def_tackles_for_loss_yards
#> 60           def_fumbles_forced
#> 61                    def_sacks
#> 62               def_sack_yards
#> 63                  def_qb_hits
#> 64            def_interceptions
#> 65       def_interception_yards
#> 66            def_pass_defended
#> 67                      def_tds
#> 68                  def_fumbles
#> 69                 def_safeties
#> 70                   misc_yards
#> 71          fumble_recovery_own
#> 72    fumble_recovery_yards_own
#> 73          fumble_recovery_opp
#> 74    fumble_recovery_yards_opp
#> 75          fumble_recovery_tds
#> 76                    penalties
#> 77                penalty_yards
#> 78                     timeouts
#> 79                 punt_returns
#> 80            punt_return_yards
#> 81              kickoff_returns
#> 82         kickoff_return_yards
#> 83                      fg_made
#> 84                       fg_att
#> 85                    fg_missed
#> 86                   fg_blocked
#> 87                      fg_long
#> 88                       fg_pct
#> 89                 fg_made_0_19
#> 90                fg_made_20_29
#> 91                fg_made_30_39
#> 92                fg_made_40_49
#> 93                fg_made_50_59
#> 94                  fg_made_60_
#> 95               fg_missed_0_19
#> 96              fg_missed_20_29
#> 97              fg_missed_30_39
#> 98              fg_missed_40_49
#> 99              fg_missed_50_59
#> 100               fg_missed_60_
#> 101                fg_made_list
#> 102              fg_missed_list
#> 103             fg_blocked_list
#> 104            fg_made_distance
#> 105          fg_missed_distance
#> 106         fg_blocked_distance
#> 107                    pat_made
#> 108                     pat_att
#> 109                  pat_missed
#> 110                 pat_blocked
#> 111                     pat_pct
#> 112                   gwfg_made
#> 113                    gwfg_att
#> 114                 gwfg_missed
#> 115                gwfg_blocked
#> 116               gwfg_distance
#> 117          gwfg_distance_list
#> 118              fantasy_points
#> 119          fantasy_points_ppr
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
#> 31                                                                                                                                             The number of official rush attempts (incl. scrambles and kneel downs). Rushes after a lateral reception don't count as carry.
#> 32                                                                                             Yards gained when rushing with the ball (incl. scrambles and kneel downs). Also includes yards gained after obtaining a lateral on a play that started with a rushing attempt.
#> 33                                                                                                                      The number of rushing touchdowns (incl. scrambles). Also includes touchdowns after obtaining a lateral on a play that started with a rushing attempt.
#> 34                                                                                                                                                                                                                                        The number of rushes with a fumble.
#> 35                                                                                                                                                                                                                                   The number of rushes with a lost fumble.
#> 36                                                                                                                                                                                                                            First downs on rush attempts (incl. scrambles).
#> 37                                                                                                                                                                                                  Expected points added on rush attempts (incl. scrambles and kneel downs).
#> 38                                                                                                                                                                                                                                               Two-point conversion rushes.
#> 39                                                                                                                                                                                     The number of pass receptions. Lateral receptions officially don't count as reception.
#> 40                                                                                                                                                                                                       The number of pass plays where the player was the targeted receiver.
#> 41                                                                                                                                                Yards gained after a pass reception. Includes yards gained after receiving a lateral on a play that started as a pass play.
#> 42                                                                                                                             The number of touchdowns following a pass reception. Also includes touchdowns after receiving a lateral on a play that started as a pass play.
#> 43                                                                                                                                                                                                                              The number of fumbles after a pass reception.
#> 44                                                                                                                                                                                                                         The number of fumbles lost after a pass reception.
#> 45                                                                                                                                                                                                                             Receiving air yards (incl. incomplete passes).
#> 46                                                                                                                         Yards after the catch gained on plays in which player was receiver (this is an unofficial stat and may differ slightly between different sources).
#> 47                                                                                                                                                                                                                                                 First downs on receptions.
#> 48                                                                                                                                                                                                                                       Expected points added on receptions.
#> 49                                                                                                                                                                                                                                           Two-point conversion receptions.
#> 50                                                                                                                                                        Receiver Air Conversion Ratio. RACR = `receiving_yards` / `receiving_air_yards`. Available if stat_type = 'player'.
#> 51                                                                                                                                                                          The share of targets of the player in all targets of his team. Available if stat_type = 'player'.
#> 52                                                                                                                                                            The share of receiving_air_yards of the player in all air_yards of his team. Available if stat_type = 'player'.
#> 53                                                                                                                                                     Weighted Opportunity Rating. WOPR = 1.5 × `target_share` + 0.7 × `air_yards_share`. Available if stat_type = 'player'.
#> 54                                                                                                                                                                                                                    The number of touchdowns scored in special teams plays.
#> 55                                                                                                                                                                                                                                                              Solo tackles.
#> 56                                                                                                                                                                                                                                     Tackles where another player assisted.
#> 57                                                                                                                                                                                                                                         Assist to another player's tackle.
#> 58                                                                                                                                                                                                                                                          Tackles for loss.
#> 59                                                                                                                                                                                                                          Yards lost by the opposing team through a tackle.
#> 60                                                                                                                                                                                                                                                            Forced fumbles.
#> 61                                                                                                                                                                                                                                                           Number of Sacks.
#> 62                                                                                                                                                                                                                            Yards lost by the opposing team through a sack.
#> 63                                                                                                                                                                                                                                                          Number of QB hits
#> 64                                                                                                                                                                                                                                                      Interceptions caught.
#> 65                                                                                                                                                                                                                                          Yards gained after interceptions.
#> 66                                                                                                                                                                                                                                                 Number of defended passes.
#> 67                                                                                                                                                                                                                                                      Defensive touchdowns.
#> 68                                                                                                                                                                                                                                Number of fumbles while playing on defense.
#> 69                                                                                                                                                                                                                                         Tackles that resulted in a safety.
#> 70                                                                                                                                                          Yardage gained/lost that doesn't fall into any other category. Examples are blocked field goals or blocked punts.
#> 71                                                                                                                                                                                                                  Recovered fumbles where the ball was fumbled by own team.
#> 72                                                                                       Yardage gained/lost by a player after he recovered a fumble by his own team. Includes yardage gained/lost where a team mate recovered a fumble and lateraled the ball to the player.
#> 73                                                                                                                                                                                                             Recovered fumbles where the ball was fumbled by opposing team.
#> 74                                                                                  Yardage gained/lost by a player after he recovered a fumble by the opposing team. Includes yardage gained/lost where a team mate recovered a fumble and lateraled the ball to the player.
#> 75                        Touchdowns scored after a fumble recovery. This can be in any unit. And both the own team and the opposing team can have fumbled the ball initially. Includes touchdowns where a team mate recovered a fumble and lateraled the ball to the player.
#> 76                                                                                                                                                                                                                                                          Penalties caused.
#> 77                                                                                                                                                                                                                                            Yardage lost through penalties.
#> 78                                                                                                                                                                                                         Number of timeouts taken by team. Available if stat_type = 'team'.
#> 79                                                                                                                                                                                                                                                  Number of punts returned.
#> 80                                                                                                                                                                                                                      Yardage gained/lost by a player during a punt return.
#> 81                                                                                                                                                                                                                                               Number of kickoffs returned.
#> 82                                                                                                                                                                                                                   Yardage gained/lost by a player during a kickoff return.
#> 83                                                                                                                                                                                                                                            Successful field goal attempts.
#> 84                                                                                                                                                                                                                                                     Attempted field goals.
#> 85                                                                                                                                                                                                                                                        Missed field goals.
#> 86                                                                                                                                                                                                                                   Attempted field goals that were blocked.
#> 87                                                                                                                                                                                                                                       Distance of longest made field goal.
#> 88                                                                                                                                                                                                                              Percentage of successful field goal attempts.
#> 89                                                                                                                                                                                                  Successful field goal attempts where distance was between 0 and 19 yards.
#> 90                                                                                                                                                                                                 Successful field goal attempts where distance was between 20 and 29 yards.
#> 91                                                                                                                                                                                                 Successful field goal attempts where distance was between 30 and 39 yards.
#> 92                                                                                                                                                                                                 Successful field goal attempts where distance was between 40 and 49 yards.
#> 93                                                                                                                                                                                                 Successful field goal attempts where distance was between 50 and 59 yards.
#> 94                                                                                                                                                                                                               Successful field goal attempts where distance was 60+ yards.
#> 95                                                                                                                                                                                                      Missed field goal attempts where distance was between 0 and 19 yards.
#> 96                                                                                                                                                                                                     Missed field goal attempts where distance was between 20 and 29 yards.
#> 97                                                                                                                                                                                                     Missed field goal attempts where distance was between 30 and 39 yards.
#> 98                                                                                                                                                                                                     Missed field goal attempts where distance was between 40 and 49 yards.
#> 99                                                                                                                                                                                                     Missed field goal attempts where distance was between 50 and 59 yards.
#> 100                                                                                                                                                                                                                  Missed field goal attempts where distance was 60+ yards.
#> 101                                                                                                                                                                                                                          Distances of all successful field goal attempts.
#> 102                                                                                                                                                                                                                              Distances of all missed field goal attempts.
#> 103                                                                                                                                                                                                                             Distances of all blocked field goal attempts.
#> 104                                                                                                                                                                                                                                 Sum of distances of all made field goals.
#> 105                                                                                                                                                                                                                               Sum of distances of all missed field goals.
#> 106                                                                                                                                                                                                                              Sum of distances of all blocked field goals.
#> 107                                                                                                                                                                                                                                          Successful extra point attempts.
#> 108                                                                                                                                                                                                                                                   Attempted extra points.
#> 109                                                                                                                                                                                                                                                      Missed extra points.
#> 110                                                                                                                                                                                                                                         Extra points blocked by opponent.
#> 111                                                                                                                                                                                                                            Percentage of successful extra point attempts.
#> 112                                                                                                                                                                                                                              Successful game winning field goal attempts.
#> 113                                                                                                                                                                                                                                       Attempted game winning field goals.
#> 114                                                                                                                                                                                                                                          Missed game winning field goals.
#> 115                                                                                                                                                                                                                     Game winning field goal attempts blocked by opponent.
#> 116                                                                                                                                                                                         Distance of game winning field goal attempt. Available if summary_level = 'week'.
#> 117                                                                                                                                                                                     Distances of game winning field goal attempts. Available if summary_level = 'season'.
#> 118                                                                                                                                                                                                                                                  Standard fantasy points.
#> 119                                                                                                                                                                                                                                                       PPR fantasy points.
# }
```
