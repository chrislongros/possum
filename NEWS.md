# possum 0.1.0

First release.

* `possum_physiology()` and `possum_operative()` score the twelve physiological
  and six operative variables; `possum()` and `p_possum()` turn those scores
  into predicted morbidity and mortality.
* `possum_risk()` scores a whole cohort from a data frame in one call.
* Variants: `cr_possum()` (colorectal), `v_possum()` (vascular) and
  `raaa_possum()` (ruptured aortic aneurysm), with their own scorers where the
  cut-points differ.
