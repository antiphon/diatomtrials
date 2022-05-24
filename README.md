# diatomtrials
Result repository for "Diatom growth: How to improve the analysis of noisy data" book chaper.

`simulation trials`: run-all-steps-for-one-set.R Should re-run the simulation-estimation-pipeline that was used to derive the results we presented. There are alternative configurations etc but these can be ignored. The resulting object 'storage/run01_gathered.rds' contains all relevant results, and has been copied relative to the root of the repository to reproduce the plots.


`data_example`: The computations for the data example we showed. Includes data. Run R-scripts in numbered order to produce the estimates and the plots.

Required packages:

- CRAN: `tidyr, dplyr, ggplot2, fANCOVA, growthrates, patchwork, parallel`
- `maxgrowthcomparison`: Tools for the trial, source-package provided (look for the tar.gz file)
- `looptimer`: find it here, http://github.com/antiphon/looptimer

