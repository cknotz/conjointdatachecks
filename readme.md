# conjointdatachecks

This is an R package that contains two (so far) lightweight functions to check
the quality of data from conjoint survey experiments.

One function, `carryTest()`, checks the data for carryover effects using the
procedure described by Hainmueller et al. (2014, Political Analysis, p. 22).

The second function, `dimRandoTest()`, checks if there were problems with the randomization of the vignette attributes during the experiment by testing if
there are significant associations between each vignette attribute and a
selected respondent-level variable. Ideally, no such associations should exist.

The results from both functions can be exported to tidy data.frames using
as.data.frame() and plotted using plot().

Additional functions will be added as time allows.

You can install the package directly from this GitHub repository with:
```
devtools::install_github("https://github.com/cknotz/conjointdatachecks")  

```

The package includes also a vignette that illustrates how the functions can be used, which can be installed and run with:
```
devtools::install_github("https://github.com/cknotz/conjointdatachecks",
build_vignettes = TRUE)   

vignette("conjointdatachecks")
```
For questions or feedback, please feel free to reach out to [carlo.knotz@uis.no](mailto:carlo.knotz@uis.no).
