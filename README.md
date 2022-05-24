# diatomtrials
Result repository for "Diatom growth: How to improve the analysis of noisy data" book chaper.

`simulation trials`: run-all-steps-for-one-set.R Should re-run the simulation-estimation-pipeline that was used to derive the results we presented. There are alternative configurations etc but these can be ignored. The resulting object 'storage/run01_gathered.rds' contains all relevant results, and has been copied relative to the root of the repository to reproduce the plots.

Requires the packages `maxgrowthcomparison`, source-package provided (look for the tar.gz file).

`data_example`: The computations for the data example we showed. Includes data. Run R-scripts in numbered order to produce the estimates and the plots.

