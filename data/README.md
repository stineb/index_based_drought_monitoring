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

subset of spectral indices used for comparison with fLUE
`selected_spectral_indices_table.csv` selected using 
code in `vignettes/fLUE_vs_VIs.Rmd`

ERA 5 data 
`era5_pcwd_sites.csv` and `era5_data_tm_ssrd_sites.csv` extracted for fluxnet 
using codes `01_extract_pcwd_data.R` and `data-raw/01_extract_era5_t2m_ssrd_data.R`
respectively.

pcwd was recalulated with full resolution for years 2018 and 2020 for model 
upscaling over centrale europe using the code : 
https://github.com/geco-bern/cwd_global/branches/full-resolution-ERA5-2018-2020`

## Output

Random forest model for flue prediction is created. Model runs are stored in 
the `analysis` folder and output is called, `model_rf.rds`  for the regression. 

`preds_rf.csv` predicted values from LSO file 

predicted flue rasters for centrale europe, after preparing and stucking downloaded 
rasters predictors `analysis/06yousra_spatial_upscaling_centrale_europe.R`


### Annotated manuscript

An annotated manuscript of the model result is written up in the vignettes 
folder and will be auto generated based upon the provided model data.
