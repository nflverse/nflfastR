test_that("kneel downs are found in all their wordings (#603)", {
  kneels <- c(
    "(:33) 5-J.Flacco kneels to BAL 33 for -1 yards.",
    "(:38) 17-P.Rivers kneels, dead ball declared at DEN 45 for -1 yards.",
    "(1:17) 13-K.Warner up the middle to NE 15 for -2 yards. kneels",
    "(:01) 16-V.Testaverde to NYJ 33 for -1 yards. Testaverde takes knee to end game",
    "(1:15) 3-J.Kitna to NE 25 for -1 yards. Took a knee.",
    "(1:36) 18-E.Grbac to BAL 46 for -1 yards. Grbac drops to one knee.",
    "(1:43) 5-D.McNabb to PHI 43 for -1 yards. QB #5 on the kneel down.",
    "(:16) 15-J.Miller to CHI 14 for -1 yards. QB Kneeled down to end the half",
    "(1:26) 18-P.Manning to ATL 40 for -1 yards. TOOK KNEE"
  )
  others <- c(
    "(5:00) 22-E.Smith left tackle to DAL 33 for 7 yards (26-E.Brown). E.Smith of Dallas shaken up on the play-right knee.",
    "(6:14) 10-R.Griffin pass short middle to 86-J.Reed to WAS 30 for 12 yards (27-G.Quin). DET-91-J.Jones was injured during the play. He is Out. Knee",
    "(2:00) 9-D.Brees pass short right to 77-B.Kneeland to NO 40 for 5 yards.",
    "(:19) 33-D.Cook left end to MIN 25 for no gain (54-T.Bowser; 96-B.Washington). Fake kneel by QB",
    NA
  )
  expect_all_true(is_qb_kneel(kneels))
  expect_identical(is_qb_kneel(others), c(FALSE, FALSE, FALSE, FALSE, NA))
})

test_that("spikes are found in all their wordings (#603)", {
  spikes <- c(
    "(:25) (No Huddle) 17-D.Jones spiked the ball to stop the clock.",
    "(:36) {No huddle} 18-E.Grbac pass incomplete. Spiked ball",
    "(:13) 10-A.Van Pelt pass incomplete. QB spikes ball",
    "(:20) 2-A.Brooks pass incomplete. Ball is spiked",
    "(:44) 14-B.Johnson pass incomplete. PENALTY on DET, Defensive Offside, 5 yards, enforced at DET 17 - No Play. spikes ball to kill clock",
    "(:43) 17-C.Sauter pass incomplete. QB spike to stop clock.",
    "(:38) 11-P.Ramsey pass incomplete. Spiked"
  )
  others <- c(
    "(5:30) 11-P.Ramsey pass incomplete. Pressure on QB: T.Spikes.",
    "(14:40) 17-Q.Carter pass incomplete to 84-J.Galloway (51-T.Spikes). Spikes almost intercepted the pass at the 50 middle of the field.",
    "(9:23) 6-M.Sanchez pass short middle intended for 17-B.Edwards INTERCEPTED by 55-B.Spikes at NE 2. 55-B.Spikes to NE 7 for 5 yards (68-M.Slauson).",
    "(6:41) 9-D.Brees pass short left to 22-M.Ingram to NO 40 for -4 yards (57-J.Bynes). PENALTY on NO-22-M.Ingram, Delay of Game, 5 yards, enforced between downs. Ingram was penalized for spiking the ball after the play.",
    "(:20) 12-T.Brady pass short left to 81-R.Moss for 6 yards, TOUCHDOWN. fake spike",
    NA
  )
  expect_all_true(is_qb_spike(spikes))
  expect_identical(
    is_qb_spike(others),
    c(FALSE, FALSE, FALSE, FALSE, FALSE, NA)
  )
})
