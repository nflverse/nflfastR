# calculate_series_conversion_rates covers all series cases

    structure(list(season = c(2022L, 2023L, 2023L, 2023L, 2023L, 
    2022L, 2023L, 2023L), team = c("A", "A", "B", "C", NA, "C", "D", 
    "E"), off_n = c(1L, 3L, 3L, 5L, 1L, NA, NA, NA), off_scr = c(1, 
    0.333333333333333, 0.333333333333333, 0.2, 1, NA, NA, NA), off_scr_1st = c(1, 
    0, 0, 0, 1, NA, NA, NA), off_scr_2nd = c(0, 0.333333333333333, 
    0, 0, 0, NA, NA, NA), off_scr_3rd = c(0, 0, 0.333333333333333, 
    0, 0, NA, NA, NA), off_scr_4th = c(0, 0, 0, 0.2, 0, NA, NA, NA
    ), off_1st = c(0, 0.333333333333333, 0, 0.2, 1, NA, NA, NA), 
        off_td = c(1, 0, 0.333333333333333, 0, 0, NA, NA, NA), off_fg = c(0, 
        0.333333333333333, 0, 0.2, 0, NA, NA, NA), off_punt = c(0, 
        0, 0, 0.2, 0, NA, NA, NA), off_to = c(0, 0.333333333333333, 
        0.666666666666667, 0.4, 0, NA, NA, NA), def_n = c(NA, 3L, 
        3L, NA, NA, 1L, 5L, 1L), def_scr = c(NA, 0.333333333333333, 
        0.333333333333333, NA, NA, 1, 0.2, 1), def_scr_1st = c(NA, 
        0, 0, NA, NA, 1, 0, 1), def_scr_2nd = c(NA, 0, 0.333333333333333, 
        NA, NA, 0, 0, 0), def_scr_3rd = c(NA, 0.333333333333333, 
        0, NA, NA, 0, 0, 0), def_scr_4th = c(NA, 0, 0, NA, NA, 0, 
        0.2, 0), def_1st = c(NA, 0, 0.333333333333333, NA, NA, 0, 
        0.2, 1), def_td = c(NA, 0.333333333333333, 0, NA, NA, 1, 
        0, 0), def_fg = c(NA, 0, 0.333333333333333, NA, NA, 0, 0.2, 
        0), def_punt = c(NA, 0, 0, NA, NA, 0, 0.2, 0), def_to = c(NA, 
        0.666666666666667, 0.333333333333333, NA, NA, 0, 0.4, 0)), row.names = c(NA, 
    -8L), class = c("tbl_df", "tbl", "data.frame"))

---

    structure(list(season = c(2022L, 2023L, 2023L, 2023L, 2023L, 
    2022L, 2023L, 2023L), team = c("A", "A", "B", "C", NA, "C", "D", 
    "E"), week = c(1L, 1L, 1L, 2L, 2L, 1L, 2L, 2L), off_n = c(1L, 
    3L, 3L, 5L, 1L, NA, NA, NA), off_scr = c(1, 0.333333333333333, 
    0.333333333333333, 0.2, 1, NA, NA, NA), off_scr_1st = c(1, 0, 
    0, 0, 1, NA, NA, NA), off_scr_2nd = c(0, 0.333333333333333, 0, 
    0, 0, NA, NA, NA), off_scr_3rd = c(0, 0, 0.333333333333333, 0, 
    0, NA, NA, NA), off_scr_4th = c(0, 0, 0, 0.2, 0, NA, NA, NA), 
        off_1st = c(0, 0.333333333333333, 0, 0.2, 1, NA, NA, NA), 
        off_td = c(1, 0, 0.333333333333333, 0, 0, NA, NA, NA), off_fg = c(0, 
        0.333333333333333, 0, 0.2, 0, NA, NA, NA), off_punt = c(0, 
        0, 0, 0.2, 0, NA, NA, NA), off_to = c(0, 0.333333333333333, 
        0.666666666666667, 0.4, 0, NA, NA, NA), def_n = c(NA, 3L, 
        3L, NA, NA, 1L, 5L, 1L), def_scr = c(NA, 0.333333333333333, 
        0.333333333333333, NA, NA, 1, 0.2, 1), def_scr_1st = c(NA, 
        0, 0, NA, NA, 1, 0, 1), def_scr_2nd = c(NA, 0, 0.333333333333333, 
        NA, NA, 0, 0, 0), def_scr_3rd = c(NA, 0.333333333333333, 
        0, NA, NA, 0, 0, 0), def_scr_4th = c(NA, 0, 0, NA, NA, 0, 
        0.2, 0), def_1st = c(NA, 0, 0.333333333333333, NA, NA, 0, 
        0.2, 1), def_td = c(NA, 0.333333333333333, 0, NA, NA, 1, 
        0, 0), def_fg = c(NA, 0, 0.333333333333333, NA, NA, 0, 0.2, 
        0), def_punt = c(NA, 0, 0, NA, NA, 0, 0.2, 0), def_to = c(NA, 
        0.666666666666667, 0.333333333333333, NA, NA, 0, 0.4, 0)), row.names = c(NA, 
    -8L), class = c("tbl_df", "tbl", "data.frame"))

# calculate_series_conversion_rates checks the columns of pbp

    Code
      calculate_series_conversion_rates(pbp)
    Condition
      Error in `calculate_series_conversion_rates()`:
      ! `pbp` is missing the required variables "series" and "down".

