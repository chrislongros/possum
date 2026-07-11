library(possum)

## Scores are integers and are pinned exactly. Probabilities are compared with a
## relative tolerance of 1e-8: far above the noise floor of plogis() (~1e-16,
## stable on noLD and M1 builds), but tight enough to catch a typo in the last
## digit of any coefficient, which moves a prediction by only ~1e-3 relative.
tol <- 1e-8

## --- physiological cut-points ---------------------------------------------

expect_identical(
    possum_physiology(age = 55, systolic_bp = 120, pulse = 70, gcs = 15,
                      hb = 14, wbc = 7, urea = 6, sodium = 140, potassium = 4.2,
                      cardiac = 1, respiratory = 1, ecg = 1),
    12L)

## worst value on every axis. WCC caps at 4 points, so the maximum is 88, not 96
expect_identical(
    possum_physiology(age = 90, systolic_bp = 80, pulse = 130, gcs = 3, hb = 9,
                      wbc = 25, urea = 20, sodium = 120, potassium = 6.5,
                      cardiac = 8, respiratory = 8, ecg = 8),
    88L)

## every band of every variable, as points above the all-normal baseline of 12
defaults <- list(age = 55, systolic_bp = 120, pulse = 70, gcs = 15, hb = 14,
                 wbc = 7, urea = 6, sodium = 140, potassium = 4.2,
                 cardiac = 1, respiratory = 1, ecg = 1)
score <- function(...) do.call(possum_physiology, modifyList(defaults, list(...)))
band  <- function(...) score(...) - 12L

expect_identical(band(age = 60), 0L)
expect_identical(band(age = 61), 1L)
expect_identical(band(age = 70), 1L)
expect_identical(band(age = 71), 3L)

expect_identical(band(systolic_bp = 110), 0L)
expect_identical(band(systolic_bp = 130), 0L)
expect_identical(band(systolic_bp = 105), 1L)
expect_identical(band(systolic_bp = 170), 1L)
expect_identical(band(systolic_bp = 95),  3L)
expect_identical(band(systolic_bp = 171), 3L)
expect_identical(band(systolic_bp = 85),  7L)

expect_identical(band(pulse = 50),  0L)
expect_identical(band(pulse = 80),  0L)
expect_identical(band(pulse = 45),  1L)
expect_identical(band(pulse = 100), 1L)
expect_identical(band(pulse = 110), 3L)
expect_identical(band(pulse = 35),  7L)
expect_identical(band(pulse = 130), 7L)

expect_identical(band(gcs = 14), 1L)
expect_identical(band(gcs = 12), 1L)
expect_identical(band(gcs = 11), 3L)
expect_identical(band(gcs = 9),  3L)
expect_identical(band(gcs = 7),  7L)

expect_identical(band(hb = 13),   0L)
expect_identical(band(hb = 16),   0L)
expect_identical(band(hb = 11.5), 1L)
expect_identical(band(hb = 17),   1L)
expect_identical(band(hb = 10),   3L)
expect_identical(band(hb = 17.5), 3L)
expect_identical(band(hb = 9),    7L)
expect_identical(band(hb = 19),   7L)

expect_identical(band(wbc = 4),   0L)
expect_identical(band(wbc = 10),  0L)
expect_identical(band(wbc = 3.5), 1L)
expect_identical(band(wbc = 12),  1L)
expect_identical(band(wbc = 2),   3L)
expect_identical(band(wbc = 25),  3L)

expect_identical(band(urea = 7.5), 0L)
expect_identical(band(urea = 8),   1L)
expect_identical(band(urea = 10),  1L)
expect_identical(band(urea = 12),  3L)
expect_identical(band(urea = 15),  3L)
expect_identical(band(urea = 20),  7L)

expect_identical(band(sodium = 136), 0L)
expect_identical(band(sodium = 135), 1L)
expect_identical(band(sodium = 131), 1L)
expect_identical(band(sodium = 130), 3L)
expect_identical(band(sodium = 126), 3L)
expect_identical(band(sodium = 125), 7L)

expect_identical(band(potassium = 3.5), 0L)
expect_identical(band(potassium = 5.0), 0L)
expect_identical(band(potassium = 3.4), 1L)
expect_identical(band(potassium = 5.2), 1L)
expect_identical(band(potassium = 3.0), 3L)
expect_identical(band(potassium = 5.5), 3L)
expect_identical(band(potassium = 2.8), 7L)
expect_identical(band(potassium = 6.0), 7L)

expect_identical(band(cardiac = 8),     7L)
expect_identical(band(respiratory = 4), 3L)
expect_identical(band(ecg = 4),         3L)

## --- operative cut-points --------------------------------------------------

expect_identical(
    possum_operative(severity = 1, n_procedures = 1, blood_loss = 50,
                     soiling = 1, malignancy = 1, urgency = 1),
    6L)
expect_identical(
    possum_operative(severity = 8, n_procedures = 3, blood_loss = 1200,
                     soiling = 8, malignancy = 8, urgency = 8),
    48L)

odefaults <- list(severity = 1, n_procedures = 1, blood_loss = 50,
                  soiling = 1, malignancy = 1, urgency = 1)
oband <- function(...) do.call(possum_operative,
                               modifyList(odefaults, list(...))) - 6L

expect_identical(oband(n_procedures = 1), 0L)
expect_identical(oband(n_procedures = 2), 3L)
expect_identical(oband(n_procedures = 3), 7L)

expect_identical(oband(blood_loss = 100),  0L)
expect_identical(oband(blood_loss = 101),  1L)
expect_identical(oband(blood_loss = 500),  1L)
expect_identical(oband(blood_loss = 501),  3L)
expect_identical(oband(blood_loss = 999),  3L)
expect_identical(oband(blood_loss = 1000), 7L)

expect_identical(oband(severity = 4),   3L)
expect_identical(oband(soiling = 8),    7L)
expect_identical(oband(malignancy = 2), 1L)
expect_identical(oband(urgency = 4),    3L)

## --- risk equations --------------------------------------------------------

## Copeland and others (1991):
##   logit(morbidity) = -5.91 + 0.16 * PS + 0.19 * OSS
##   logit(mortality) = -7.04 + 0.13 * PS + 0.16 * OSS
r <- possum(physiological_score = 20, operative_score = 10)
expect_equal(r$morbidity, 0.307890495698, tolerance = tol)
expect_equal(r$mortality, 0.0552005377829, tolerance = tol)

## Prytherch and others (1998):
##   logit(mortality) = -9.065 + 0.1692 * PS + 0.1550 * OSS
expect_equal(p_possum(physiological_score = 20, operative_score = 10),
             0.0158127437286, tolerance = tol)

expect_true(p_possum(12, 6) < 0.01)

## --- CR-POSSUM -------------------------------------------------------------

expect_identical(
    cr_possum_physiology(age = 55, cardiac = 1, systolic_bp = 120, pulse = 70,
                         hb = 14, urea = 5),
    6L)
expect_identical(
    cr_possum_physiology(age = 85, cardiac = 3, systolic_bp = 80, pulse = 130,
                         hb = 9, urea = 20),
    23L)

cdefaults <- list(age = 55, cardiac = 1, systolic_bp = 120, pulse = 70,
                  hb = 14, urea = 5)
cband <- function(...) do.call(cr_possum_physiology,
                               modifyList(cdefaults, list(...))) - 6L

## CR-POSSUM re-weights the bands: age is 1/3/4/8, the rest cap at 3
expect_identical(cband(age = 65), 2L)
expect_identical(cband(age = 75), 3L)
expect_identical(cband(age = 82), 7L)

expect_identical(cband(systolic_bp = 100), 0L)
expect_identical(cband(systolic_bp = 170), 0L)
expect_identical(cband(systolic_bp = 95),  1L)
expect_identical(cband(systolic_bp = 171), 1L)
expect_identical(cband(systolic_bp = 89),  2L)

expect_identical(cband(pulse = 40),  0L)
expect_identical(cband(pulse = 100), 0L)
expect_identical(cband(pulse = 110), 1L)
expect_identical(cband(pulse = 130), 2L)
expect_identical(cband(pulse = 35),  2L)

expect_identical(cband(hb = 13), 0L)
expect_identical(cband(hb = 16), 0L)
expect_identical(cband(hb = 12), 1L)
expect_identical(cband(hb = 17), 1L)
expect_identical(cband(hb = 9),  2L)
expect_identical(cband(hb = 19), 2L)

expect_identical(cband(urea = 10), 0L)
expect_identical(cband(urea = 12), 1L)
expect_identical(cband(urea = 15), 1L)
expect_identical(cband(urea = 16), 2L)

expect_identical(cband(cardiac = 3), 2L)

expect_identical(cr_possum_operative(severity = 1, soiling = 1,
                                     cancer_staging = 1, urgency = 1), 4L)
expect_identical(cr_possum_operative(severity = 8, soiling = 3,
                                     cancer_staging = 3, urgency = 8), 22L)
expect_identical(cr_possum_operative(severity = 3, soiling = 2,
                                     cancer_staging = 2, urgency = 3), 10L)

## Tekkis and others (2004):
##   logit(mortality) = -9.167 + 0.338 * PS + 0.308 * OS
expect_equal(cr_possum(physiological_score = 12, operative_score = 8),
             0.0661741537967, tolerance = tol)

## --- V-POSSUM and RAAA-POSSUM (use the standard POSSUM scores) -------------

## Neary and others (2003):
##   logit(mortality) = -6.0386 + 0.1539 * PS
##   logit(mortality) = -8.0616 + 0.1552 * PS + 0.1238 * OS
expect_equal(v_possum(physiological_score = 25), 0.100552563921, tolerance = tol)
expect_equal(
    v_possum(physiological_score = 25, operative_score = 15, model = "full"),
    0.0891059888445, tolerance = tol)

##   logit(mortality) = -2.7569 + 0.0968 * PS
##   logit(mortality) = -4.9795 + 0.0913 * PS + 0.0958 * OS
expect_equal(raaa_possum(physiological_score = 30), 0.536708830466,
             tolerance = tol)
expect_equal(
    raaa_possum(physiological_score = 30, operative_score = 18, model = "full"),
    0.373764635406, tolerance = tol)

## The physiology-only default must never quietly drop a supplied operative
## score and return a different mortality from the one the caller asked for.
expect_error(v_possum(25, 15), "not used when model")
expect_error(raaa_possum(30, 18), "not used when model")
expect_error(v_possum(25, 15, model = "physiology"), "not used when model")
expect_error(v_possum(physiological_score = 25, model = "full"),
             "required when model")
expect_error(raaa_possum(physiological_score = 30, model = "full"),
             "required when model")

## --- properties of the equations -------------------------------------------

ps <- c(12, 30, 50, 88)
os <- c(6, 20, 35, 48)
probs <- c(possum(ps, os)$morbidity, possum(ps, os)$mortality, p_possum(ps, os),
           v_possum(ps), raaa_possum(ps), cr_possum(6:9, 4:7))
expect_true(all(probs > 0 & probs < 1))
expect_true(all(diff(p_possum(ps, os)) > 0))
expect_true(all(diff(possum(ps, os)$morbidity) > 0))
expect_true(all(diff(possum(ps, os)$mortality) > 0))
expect_true(all(diff(cr_possum(6:23, 4:21)) > 0))
expect_true(all(diff(v_possum(ps)) > 0))
expect_true(all(diff(raaa_possum(ps)) > 0))

## the equations are vectorised, and agree with the scalar calls
expect_equal(p_possum(ps, os),
             vapply(seq_along(ps), function(i) p_possum(ps[i], os[i]), 0.0),
             tolerance = tol)
expect_equal(possum(ps, os)$mortality,
             vapply(seq_along(ps), function(i) possum(ps[i], os[i])$mortality, 0.0),
             tolerance = tol)

## --- input validation ------------------------------------------------------

expect_error(possum_operative(severity = 3, n_procedures = 1, blood_loss = 50,
                              soiling = 1, malignancy = 1, urgency = 1),
             "severity")
expect_error(possum_physiology(age = 55, systolic_bp = 120, pulse = 70,
                               gcs = 15, hb = 14, wbc = 7, urea = 6,
                               sodium = 140, potassium = 4.2, cardiac = 1,
                               respiratory = 1, ecg = 2),
             "ecg")
expect_error(cr_possum_operative(severity = 2, soiling = 1,
                                 cancer_staging = 1, urgency = 1),
             "severity")
expect_error(cr_possum_physiology(age = 55, cardiac = 4, systolic_bp = 120,
                                  pulse = 70, hb = 14, urea = 5),
             "cardiac")

## missing and non-numeric values are rejected, not silently scored
expect_error(score(age = NA), "age")
expect_error(score(potassium = "4.2"), "potassium")
expect_error(score(cardiac = NA), "cardiac")

## A score outside its possible range is an input error, not something to
## predict from. This is what catches the two scores passed the wrong way round.
expect_error(possum(10, 20), "physiological_score")     # swapped arguments
expect_error(p_possum(10, 20), "physiological_score")
expect_error(p_possum(-50, 6), "physiological_score")
expect_error(possum(12, 5), "operative_score")
expect_error(possum(89, 6), "physiological_score")
expect_error(possum(12, 49), "operative_score")
expect_error(cr_possum(24, 4), "physiological_score")   # CR range is 6-23
expect_error(cr_possum(6, 23), "operative_score")       # CR range is 4-22
expect_error(v_possum(11), "physiological_score")
expect_error(raaa_possum(89), "physiological_score")
expect_error(v_possum(25, 49, model = "full"), "operative_score")

## the boundaries themselves are valid
expect_true(is.numeric(possum(12, 6)$mortality))
expect_true(is.numeric(possum(88, 48)$mortality))
expect_true(is.numeric(cr_possum(6, 4)))
expect_true(is.numeric(cr_possum(23, 22)))

## the offending value is named in the message
expect_error(p_possum(200, 6), "200")

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
expect_true(all(names(df) %in% names(res)))

expect_identical(res$physiological_score[1], 12L)
expect_identical(res$operative_score[1], 6L)
expect_equal(res$p_possum_mortality[1], p_possum(12, 6), tolerance = tol)

expect_true(res$physiological_score[2] > res$physiological_score[1])
expect_true(all(res$possum_morbidity[2] > res$possum_morbidity[1],
                res$possum_mortality[2] > res$possum_mortality[1],
                res$p_possum_mortality[2] > res$p_possum_mortality[1]))

expect_error(possum_risk(df[, -1]), "age")
