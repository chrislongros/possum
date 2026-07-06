## POSSUM / P-POSSUM surgical risk scores.
##
## Physiological and operative variable cut-points follow Copeland GP, Jones D,
## Walters M (1991) "POSSUM: a scoring system for surgical audit", Br J Surg
## 78:355-360. The POSSUM risk equations are from the same source; the P-POSSUM
## mortality equation is from Prytherch DR et al. (1998), Br J Surg 85:1217-1220.
##
## Continuous variables use these units: systolic blood pressure mmHg, pulse
## beats/min, haemoglobin g/dL, white cell count 10^9/L, urea mmol/L, sodium and
## potassium mmol/L, blood loss mL. The subjectively graded variables (cardiac,
## respiratory, ECG, operative severity, soiling, malignancy, urgency) are
## entered directly as their POSSUM points, read from the reference tables in
## ?possum_physiology and ?possum_operative.

.points <- function(x, name, allowed = c(1, 2, 4, 8)) {
    if (!is.numeric(x) || anyNA(x) || any(!x %in% allowed))
        stop(sprintf("`%s` must be one of {%s} (POSSUM points).",
                     name, paste(allowed, collapse = ", ")), call. = FALSE)
    as.integer(x)
}

.num <- function(x, name) {
    if (!is.numeric(x) || anyNA(x))
        stop(sprintf("`%s` must be a non-missing numeric value.", name),
             call. = FALSE)
    x
}

.age_pts       <- function(x) ifelse(x <= 60, 1L, ifelse(x <= 70, 2L, 4L))
.sbp_pts       <- function(x) ifelse(x >= 110 & x <= 130, 1L,
                             ifelse((x >= 100 & x < 110) | (x > 130 & x <= 170), 2L,
                             ifelse((x >= 90 & x < 100) | (x > 170), 4L, 8L)))
.pulse_pts     <- function(x) ifelse(x >= 50 & x <= 80, 1L,
                             ifelse((x >= 40 & x < 50) | (x > 80 & x <= 100), 2L,
                             ifelse(x > 100 & x <= 120, 4L, 8L)))
.gcs_pts       <- function(x) ifelse(x >= 15, 1L, ifelse(x >= 12, 2L,
                             ifelse(x >= 9, 4L, 8L)))
.hb_pts        <- function(x) ifelse(x >= 13 & x <= 16, 1L,
                             ifelse((x >= 11.5 & x < 13) | (x > 16 & x <= 17), 2L,
                             ifelse((x >= 10 & x < 11.5) | (x > 17 & x <= 18), 4L, 8L)))
.wbc_pts       <- function(x) ifelse(x >= 4 & x <= 10, 1L,
                             ifelse((x >= 3.1 & x < 4) | (x > 10 & x <= 20), 2L, 4L))
.urea_pts      <- function(x) ifelse(x <= 7.5, 1L, ifelse(x <= 10, 2L,
                             ifelse(x <= 15, 4L, 8L)))
.sodium_pts    <- function(x) ifelse(x >= 136, 1L, ifelse(x >= 131, 2L,
                             ifelse(x >= 126, 4L, 8L)))
.potassium_pts <- function(x) ifelse(x >= 3.5 & x <= 5.0, 1L,
                             ifelse((x >= 3.2 & x < 3.5) | (x > 5.0 & x <= 5.3), 2L,
                             ifelse((x >= 2.9 & x < 3.2) | (x > 5.3 & x <= 5.9), 4L, 8L)))
.nproc_pts     <- function(x) ifelse(x <= 1, 1L, ifelse(x <= 2, 4L, 8L))
.blood_pts     <- function(x) ifelse(x <= 100, 1L, ifelse(x <= 500, 2L,
                             ifelse(x < 1000, 4L, 8L)))

#' POSSUM physiological score
#'
#' Sums the twelve physiological components of the POSSUM score. The nine
#' objective variables are scored from their raw clinical values; the three
#' graded variables (\code{cardiac}, \code{respiratory}, \code{ecg}) are entered
#' directly as their POSSUM points.
#'
#' Cut-points (points 1 / 2 / 4 / 8):
#' \tabular{ll}{
#'   age (years)        \tab \eqn{\le}60 / 61-70 / \eqn{\ge}71 / -\cr
#'   systolic BP (mmHg) \tab 110-130 / 100-109 or 131-170 / 90-99 or \eqn{\ge}171 / \eqn{\le}89\cr
#'   pulse (/min)       \tab 50-80 / 40-49 or 81-100 / 101-120 / \eqn{\le}39 or \eqn{\ge}121\cr
#'   GCS                \tab 15 / 12-14 / 9-11 / \eqn{\le}8\cr
#'   haemoglobin (g/dL) \tab 13-16 / 11.5-12.9 or 16.1-17 / 10-11.4 or 17.1-18 / <10 or >18\cr
#'   WCC (10^9/L)       \tab 4-10 / 3.1-3.9 or 10.1-20 / \eqn{\le}3 or \eqn{\ge}20.1 / -\cr
#'   urea (mmol/L)      \tab \eqn{\le}7.5 / 7.6-10 / 10.1-15 / \eqn{\ge}15.1\cr
#'   sodium (mmol/L)    \tab \eqn{\ge}136 / 131-135 / 126-130 / \eqn{\le}125\cr
#'   potassium (mmol/L) \tab 3.5-5 / 3.2-3.4 or 5.1-5.3 / 2.9-3.1 or 5.4-5.9 / \eqn{\le}2.8 or \eqn{\ge}6
#' }
#' Graded variables, entered as points: \code{cardiac} and \code{respiratory}
#' take 1, 2, 4 or 8; \code{ecg} takes 1 (normal), 4 (atrial fibrillation
#' 60-90/min) or 8 (any other abnormality). Confirm the grading against the
#' original tables before use.
#'
#' @param age,systolic_bp,pulse,gcs,hb,wbc,urea,sodium,potassium Numeric raw
#'   clinical values in the units above.
#' @param cardiac,respiratory POSSUM points (1, 2, 4 or 8) for the cardiac and
#'   respiratory signs.
#' @param ecg POSSUM points for the ECG (1, 4 or 8).
#' @return Integer physiological score (minimum 12).
#' @examples
#' possum_physiology(age = 65, systolic_bp = 120, pulse = 75, gcs = 15,
#'                   hb = 13.5, wbc = 8, urea = 6, sodium = 140, potassium = 4.2,
#'                   cardiac = 1, respiratory = 1, ecg = 1)
#' @export
possum_physiology <- function(age, systolic_bp, pulse, gcs, hb, wbc, urea,
                              sodium, potassium, cardiac, respiratory, ecg) {
    .age_pts(.num(age, "age")) +
    .sbp_pts(.num(systolic_bp, "systolic_bp")) +
    .pulse_pts(.num(pulse, "pulse")) +
    .gcs_pts(.num(gcs, "gcs")) +
    .hb_pts(.num(hb, "hb")) +
    .wbc_pts(.num(wbc, "wbc")) +
    .urea_pts(.num(urea, "urea")) +
    .sodium_pts(.num(sodium, "sodium")) +
    .potassium_pts(.num(potassium, "potassium")) +
    .points(cardiac, "cardiac") +
    .points(respiratory, "respiratory") +
    .points(ecg, "ecg", allowed = c(1, 4, 8))
}

#' POSSUM operative severity score
#'
#' Sums the six operative components. \code{n_procedures} and \code{blood_loss}
#' are scored from their raw values; \code{severity}, \code{soiling},
#' \code{malignancy} and \code{urgency} are entered as their POSSUM points.
#'
#' Raw cut-points (points 1 / 2 / 4 / 8): number of procedures 1 / - / 2 / >2;
#' blood loss (mL) \eqn{\le}100 / 101-500 / 501-999 / \eqn{\ge}1000. Graded, as
#' points: \code{severity} 1 (minor) / 2 (moderate) / 4 (major) / 8 (major+);
#' \code{soiling} 1 (none) / 2 (serous) / 4 (local pus) / 8 (free bowel content,
#' pus or blood); \code{malignancy} 1 (none) / 2 (primary only) / 4 (nodal
#' metastases) / 8 (distant metastases); \code{urgency} 1 (elective) / 4
#' (emergency, within 24 h) / 8 (emergency, within 2 h).
#'
#' @param severity,soiling,malignancy POSSUM points (1, 2, 4 or 8).
#' @param urgency POSSUM points for the mode of surgery (1, 4 or 8).
#' @param n_procedures Number of procedures at this operation.
#' @param blood_loss Total blood loss in mL.
#' @return Integer operative severity score (minimum 6).
#' @examples
#' possum_operative(severity = 4, n_procedures = 1, blood_loss = 200,
#'                  soiling = 1, malignancy = 2, urgency = 1)
#' @export
possum_operative <- function(severity, n_procedures, blood_loss, soiling,
                             malignancy, urgency) {
    .points(severity, "severity") +
    .nproc_pts(.num(n_procedures, "n_procedures")) +
    .blood_pts(.num(blood_loss, "blood_loss")) +
    .points(soiling, "soiling") +
    .points(malignancy, "malignancy") +
    .points(urgency, "urgency", allowed = c(1, 4, 8))
}

#' POSSUM predicted morbidity and mortality
#'
#' Applies the original POSSUM logistic equations (Copeland and others, 1991)
#' to a physiological and operative severity score:
#' \deqn{\mathrm{logit}(morbidity) = -5.91 + 0.16 \times PS + 0.19 \times OSS}
#' \deqn{\mathrm{logit}(mortality) = -7.04 + 0.13 \times PS + 0.16 \times OSS}
#'
#' @param physiological_score POSSUM physiological score, e.g. from
#'   \code{\link{possum_physiology}}.
#' @param operative_score POSSUM operative severity score, e.g. from
#'   \code{\link{possum_operative}}.
#' @return A list with elements \code{morbidity} and \code{mortality}, the
#'   predicted probabilities (0-1).
#' @examples
#' possum(physiological_score = 20, operative_score = 10)
#' @export
possum <- function(physiological_score, operative_score) {
    ps <- .num(physiological_score, "physiological_score")
    os <- .num(operative_score, "operative_score")
    list(morbidity = stats::plogis(-5.91 + 0.16 * ps + 0.19 * os),
         mortality = stats::plogis(-7.04 + 0.13 * ps + 0.16 * os))
}

#' P-POSSUM predicted mortality
#'
#' Applies the Portsmouth (P-POSSUM) mortality equation (Prytherch and others,
#' 1998), which recalibrates the POSSUM mortality prediction:
#' \deqn{\mathrm{logit}(mortality) = -9.065 + 0.1692 \times PS + 0.1550 \times OSS}
#'
#' @param physiological_score POSSUM physiological score.
#' @param operative_score POSSUM operative severity score.
#' @return Predicted probability of mortality (0-1).
#' @examples
#' p_possum(physiological_score = 20, operative_score = 10)
#' @export
p_possum <- function(physiological_score, operative_score) {
    ps <- .num(physiological_score, "physiological_score")
    os <- .num(operative_score, "operative_score")
    stats::plogis(-9.065 + 0.1692 * ps + 0.1550 * os)
}

#' POSSUM and P-POSSUM risk for a table of patients
#'
#' Convenience wrapper that scores a whole cohort in one call, rather than
#' invoking \code{\link{possum_physiology}}, \code{\link{possum_operative}} and
#' the risk functions separately. All work is vectorised over the rows of
#' \code{data}; a single-row data frame is one patient.
#'
#' @param data A data frame (one row per patient) containing the columns
#'   required by \code{\link{possum_physiology}} and
#'   \code{\link{possum_operative}}: \code{age}, \code{systolic_bp},
#'   \code{pulse}, \code{gcs}, \code{hb}, \code{wbc}, \code{urea},
#'   \code{sodium}, \code{potassium}, \code{cardiac}, \code{respiratory},
#'   \code{ecg}, \code{severity}, \code{n_procedures}, \code{blood_loss},
#'   \code{soiling}, \code{malignancy} and \code{urgency}.
#' @return \code{data} with five columns added: \code{physiological_score},
#'   \code{operative_score}, \code{possum_morbidity}, \code{possum_mortality}
#'   and \code{p_possum_mortality}.
#' @examples
#' df <- data.frame(
#'   age = c(45, 82), systolic_bp = c(120, 95), pulse = c(70, 115),
#'   gcs = c(15, 13), hb = c(14, 9.5), wbc = c(7, 22), urea = c(5, 18),
#'   sodium = c(140, 128), potassium = c(4.2, 6.1), cardiac = c(1, 4),
#'   respiratory = c(1, 4), ecg = c(1, 8), severity = c(1, 8),
#'   n_procedures = c(1, 1), blood_loss = c(50, 1200), soiling = c(1, 8),
#'   malignancy = c(1, 4), urgency = c(1, 8))
#' possum_risk(df)
#' @export
possum_risk <- function(data) {
    required <- c("age", "systolic_bp", "pulse", "gcs", "hb", "wbc", "urea",
                  "sodium", "potassium", "cardiac", "respiratory", "ecg",
                  "severity", "n_procedures", "blood_loss", "soiling",
                  "malignancy", "urgency")
    absent <- setdiff(required, names(data))
    if (length(absent))
        stop("`data` is missing required columns: ",
             paste(absent, collapse = ", "), call. = FALSE)

    ps <- possum_physiology(data$age, data$systolic_bp, data$pulse, data$gcs,
                            data$hb, data$wbc, data$urea, data$sodium,
                            data$potassium, data$cardiac, data$respiratory,
                            data$ecg)
    os <- possum_operative(data$severity, data$n_procedures, data$blood_loss,
                           data$soiling, data$malignancy, data$urgency)
    risk <- possum(ps, os)

    out <- as.data.frame(data)
    out$physiological_score <- ps
    out$operative_score     <- os
    out$possum_morbidity    <- risk$morbidity
    out$possum_mortality    <- risk$mortality
    out$p_possum_mortality  <- p_possum(ps, os)
    out
}
