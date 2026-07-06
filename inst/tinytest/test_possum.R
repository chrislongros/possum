library(possum)

## --- physiological cut-points ---------------------------------------------

## an all-normal patient scores the minimum of 12
expect_equal(
    possum_physiology(age = 55, systolic_bp = 120, pulse = 70, gcs = 15,
                      hb = 14, wbc = 7, urea = 6, sodium = 140, potassium = 4.2,
                      cardiac = 1, respiratory = 1, ecg = 1),
    12L)

## boundary checks on the objective variables (via the exported scorer)
defaults <- list(age = 55, systolic_bp = 120, pulse = 70, gcs = 15, hb = 14,
                 wbc = 7, urea = 6, sodium = 140, potassium = 4.2,
                 cardiac = 1, respiratory = 1, ecg = 1)
score <- function(...) do.call(possum_physiology, modifyList(defaults, list(...)))
expect_equal(score(age = 61) - 12L, 1L)          # 61-70 -> 2 (i.e. +1 over baseline)
expect_equal(score(age = 71) - 12L, 3L)          # >=71 -> 4
expect_equal(score(systolic_bp = 85) - 12L, 7L)  # <=89 -> 8
expect_equal(score(pulse = 130) - 12L, 7L)       # >=121 -> 8
expect_equal(score(gcs = 7) - 12L, 7L)           # <=8 -> 8
expect_equal(score(urea = 20) - 12L, 7L)         # >=15.1 -> 8
expect_equal(score(hb = 9) - 12L, 7L)            # <10 -> 8

## --- operative cut-points --------------------------------------------------

## minimum operative severity score is 6
expect_equal(
    possum_operative(severity = 1, n_procedures = 1, blood_loss = 50,
                     soiling = 1, malignancy = 1, urgency = 1),
    6L)
expect_equal(
    possum_operative(severity = 8, n_procedures = 3, blood_loss = 1200,
                     soiling = 8, malignancy = 8, urgency = 8),
    48L)                                          # 8+8+8+8+8+8

## --- risk equations --------------------------------------------------------

r <- possum(physiological_score = 20, operative_score = 10)
expect_equal(r$morbidity, 0.30789, tolerance = 1e-3)
expect_equal(r$mortality, 0.05520, tolerance = 1e-3)
expect_equal(p_possum(physiological_score = 20, operative_score = 10),
             0.01581, tolerance = 1e-3)

## a fit patient having minor elective surgery has low predicted mortality
expect_true(p_possum(12, 6) < 0.01)

## --- input validation ------------------------------------------------------

expect_error(possum_operative(severity = 3, n_procedures = 1, blood_loss = 50,
                              soiling = 1, malignancy = 1, urgency = 1))
expect_error(possum_physiology(age = 55, systolic_bp = 120, pulse = 70,
                               gcs = 15, hb = 14, wbc = 7, urea = 6,
                               sodium = 140, potassium = 4.2, cardiac = 1,
                               respiratory = 1, ecg = 2))   # ecg must be 1/4/8

## --- data-frame interface --------------------------------------------------

df <- data.frame(
    age = c(45, 82), systolic_bp = c(120, 95), pulse = c(70, 115),
    gcs = c(15, 13), hb = c(14, 9.5), wbc = c(7, 22), urea = c(5, 18),
    sodium = c(140, 128), potassium = c(4.2, 6.1), cardiac = c(1, 4),
    respiratory = c(1, 4), ecg = c(1, 8), severity = c(1, 8),
    n_procedures = c(1, 1), blood_loss = c(50, 1200), soiling = c(1, 8),
    malignancy = c(1, 4), urgency = c(1, 8))
res <- possum_risk(df)

expect_equal(nrow(res), 2L)
expect_true(all(c("physiological_score", "operative_score", "possum_morbidity",
                  "possum_mortality", "p_possum_mortality") %in% names(res)))
## the fit patient in row 1 matches the scalar path
expect_equal(res$physiological_score[1], 12L)
expect_equal(res$operative_score[1], 6L)
expect_equal(res$p_possum_mortality[1], p_possum(12, 6))
## a missing column is reported
expect_error(possum_risk(df[, -1]))
