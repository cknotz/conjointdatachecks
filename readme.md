# conjointdatachecks

This is an R package that contains two (so far) lightweight functions to check
the quality of data from conjoint survey experiments.

One function, `carryTest()`, checks the data for carryover effects using the
procedure described by Hainmueller et al. (2014, Political Analysis, p. 22).

The second function, `dimRandoTest()`, checks if there are problems with the randomization
of the vignette attributes or dimensions, i.e., if some attribute pairs are correlated.
Correlations can be expected in partially randomized conjoint experiments, where some
attribute combinations are excluded but should otherwise not be present. The function
computes both a chi-squared test and a Cramer's V statistic for each attribute pair
to indicate the strength and statistical significance of any inter-attribute associations.

Additional functions will be added as time allows.

For questions or feedback, please feel free to reach out to carlo.knotz@uis.no.
