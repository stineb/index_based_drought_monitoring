# Input and output data files

## Input

Input data consist of the driver data file:
`machine_learning_training_data.rds`

Which contains all data required for the analysis.

Spectral indices are downloaded from:
https://awesome-ee-spectral-indices.readthedocs.io/
on 15/02/2024

more info on indices here:
https://www.indexdatabase.de/

## Output

Two models are created, a binary classification (drought day or not), and an
unbounded regression on `flue` values. Model runs are stored in the `analysis`
folder and output is called,

`regression_model.rds` and `classification_model.rds` for the regression and
classification models respectively. These are the best models selected after
cross validation (see code in the `analysis` folder).

### LSO

The LSO directory contains all models of the leave-site-out model training
runs, the model name reflects the run for which the site was ommitted from
training,

`test_*.rds`

would therefore be trained on all data but that from the `test` site.

### Annotated manuscript

An annotated manuscript of the model result is written up in the vignettes 
folder and will be auto generated based upon the provided model data.
