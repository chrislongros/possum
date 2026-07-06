# possum

POSSUM and P-POSSUM surgical risk scores in R.

POSSUM (the Physiological and Operative Severity Score for the enUmeration of
Mortality and morbidity) is a risk-adjustment system used in surgical audit. It
scores twelve physiological variables and six operative variables, then predicts
morbidity and mortality from logistic equations. P-POSSUM is the Portsmouth
recalibration of the mortality prediction.

## Install

```r
# install.packages("remotes")
remotes::install_github("chrislongros/possum")
```

## Use

```r
library(possum)

ps <- possum_physiology(age = 72, systolic_bp = 105, pulse = 96, gcs = 15,
                        hb = 11.2, wbc = 14, urea = 12, sodium = 133,
                        potassium = 4.6, cardiac = 2, respiratory = 2, ecg = 1)

os <- possum_operative(severity = 4, n_procedures = 1, blood_loss = 400,
                       soiling = 2, malignancy = 4, urgency = 1)

possum(ps, os)     # POSSUM morbidity and mortality
p_possum(ps, os)   # P-POSSUM mortality
```

The objective variables (blood pressure, labs, blood loss, ...) are scored from
their raw values; the graded variables (cardiac, respiratory, ECG, operative
severity, soiling, malignancy, urgency) are entered as their POSSUM points. See
`?possum_physiology` and `?possum_operative` for the cut-point tables and units.

## Sources

- Copeland GP, Jones D, Walters M (1991). POSSUM: a scoring system for surgical
  audit. *British Journal of Surgery* 78:355–360.
- Prytherch DR, Whiteley MS, Higgins B, Weaver PC, Prout WG, Powell SJ (1998).
  POSSUM and Portsmouth POSSUM for predicting mortality. *British Journal of
  Surgery* 85:1217–1220.

## Scope

This package is for audit and research. It is not a validated medical device,
and its output must not be used as the sole basis for any clinical decision.
Confirm the cut-points and coefficients against the original papers before use.

## License

MIT.
