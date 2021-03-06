abbottr
=======

Abbott's Correction with Confidence Intervals for Bioassay Data
---------------------------------------------------------------

Correct mortality data for control mortality and calculate confidence intervals.

#### Functions

-   `AbbottsCorrection`: perform Abbott's correction and calculate confidence intervals on vectors of control and experimental mortality data.
-   `MultipleAbbottsCorrection`: run `AbbottsCorrection` on multiple samples in a `data.frame` or `data.table` (*e.g.* to correct multiple samples against a baseline dose simultaneously).

#### Installation

-   Requires [`devtools`](https://github.com/hadley/devtools): `install.packages("devtools")`
-   Install `argparsR` from GitHub: `devtools::install_github("TomHarrop/abbottr")`
