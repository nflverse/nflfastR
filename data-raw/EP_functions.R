
## Helper functions

#All of these are heavily borrowed from nflscrapR (Maksim Horowitz, Ronald Yurko, and Samuel Ventura)

#original code
#https://github.com/ryurko/nflscrapR-models/blob/master/R/init_models/init_ep_fg_models.R

########################################################################
#### helper function for building dataset to estimate EPA model ########
########################################################################
#ben is reasonably confident that this works and is finished
#this is only needed to estimate the EP model, not actually adding EP to data
find_game_next_score_half <- function(pbp_dataset) {

  # Which rows are the scoring plays:
  score_plays <- which(pbp_dataset$sp == 1 & pbp_dataset$play_type != "no_play")

  # Define a helper function that takes in the current play index,
  # a vector of the scoring play indices, play-by-play data,
  # and returns the score type and drive number for the next score:
  find_next_score <- function(play_i, score_plays_i,pbp_df) {

    # Find the next score index for the current play
    # based on being the first next score index:
    next_score_i <- score_plays_i[which(score_plays_i >= play_i)[1]]

    # If next_score_i is NA (no more scores after current play)
    # or if the next score is in another half,
    # then return No_Score and the current drive number
    if (is.na(next_score_i) |
        (pbp_df$qtr[play_i] %in% c(1, 2) & pbp_df$qtr[next_score_i] %in% c(3, 4, 5)) |
        (pbp_df$qtr[play_i] %in% c(3, 4) & pbp_df$qtr[next_score_i] == 5)) {

      score_type <- "No_Score"

      # Make it the current play index
      score_drive <- pbp_df$drive[play_i]

      # Else return the observed next score type and drive number:
    } else {

      # Store the score_drive number
      score_drive <- pbp_df$drive[next_score_i]

      # Then check the play types to decide what to return
      # based on several types of cases for the next score:

      # 1: Return TD
      if (pbp_df$touchdown[next_score_i] == 1 & (pbp_df$td_team[next_score_i] != pbp_df$posteam[next_score_i])) {

        # For return touchdowns the current posteam would not have
        # possession at the time of return, so it's flipped:
        if (identical(pbp_df$posteam[play_i], pbp_df$posteam[next_score_i])) {

          score_type <- "Opp_Touchdown"

        } else {

          score_type <- "Touchdown"

        }
      } else if (identical(pbp_df$field_goal_result[next_score_i], "made")) {

        # 2: Field Goal
        # Current posteam made FG
        if (identical(pbp_df$posteam[play_i], pbp_df$posteam[next_score_i])) {

          score_type <- "Field_Goal"

          # Opponent made FG
        } else {

          score_type <- "Opp_Field_Goal"

        }

        # 3: Touchdown (returns already counted for)
      } else if (pbp_df$touchdown[next_score_i] == 1) {

        # Current posteam TD
        if (identical(pbp_df$posteam[play_i], pbp_df$posteam[next_score_i])) {

          score_type <- "Touchdown"

          # Opponent TD
        } else {

          score_type <- "Opp_Touchdown"

        }
        # 4: Safety (similar to returns)
      } else if (pbp_df$safety[next_score_i] == 1) {

        if (identical(pbp_df$posteam[play_i],pbp_df$posteam[next_score_i])) {

          score_type <- "Opp_Safety"

        } else {

          score_type <- "Safety"

        }
        # 5: Extra Points
      } else if (identical(pbp_df$extra_point_result[next_score_i], "good")) {

        # Current posteam Extra Point
        if (identical(pbp_df$posteam[play_i], pbp_df$posteam[next_score_i])) {

          score_type <- "Extra_Point"

          # Opponent Extra Point
        } else {

          score_type <- "Opp_Extra_Point"

        }
        # 6: Two Point Conversions
      } else if (identical(pbp_df$two_point_conv_result[next_score_i], "success")) {

        # Current posteam Two Point Conversion
        if (identical(pbp_df$posteam[play_i], pbp_df$posteam[next_score_i])) {

          score_type <- "Two_Point_Conversion"

          # Opponent Two Point Conversion
        } else {

          score_type <- "Opp_Two_Point_Conversion"

        }

        # 7: Defensive Two Point (like returns)
      } else if (identical(pbp_df$defensive_two_point_conv[next_score_i], 1)) {

        if (identical(pbp_df$posteam[play_i], pbp_df$posteam[next_score_i])) {

          score_type <- "Opp_Defensive_Two_Point"

        } else {

          score_type <- "Defensive_Two_Point"

        }

        # 8: Errors of some sort so return NA (but shouldn't take place)
      } else {

        score_type <- NA

      }
    }

    return(data.frame(Next_Score_Half = score_type,
                      Drive_Score_Half = score_drive))
  }

  # Using lapply and then bind_rows is much faster than
  # using map_dfr() here:
  lapply(c(1:nrow(pbp_dataset)), find_next_score,
         score_plays_i = score_plays, pbp_df = pbp_dataset) %>%
    bind_rows() %>%
    return
}

