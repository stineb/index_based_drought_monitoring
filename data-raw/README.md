Scripts for downloading MODIS data (reflectance, lst, and lc) and  NDVI 
from Appeears `00_download_modis_data.R` and composing machine learning data 
`02_compose_machine_learning_data.R`

ERA 5 data 
codes to extract pcwd and ERA data for fluxnet sites`01_extract_pcwd_data.R` and 
`data-raw/01_extract_era5_t2m_ssrd_data.R`, respectively.

pcwd was recalulated with full resolution for years 2018 and 2020 for model 
upscaling over centrale europe using the code : 
https://github.com/geco-bern/cwd_global/branches/full-resolution-ERA5-2018-2020` 
from this full resolution recalculated pcwd, we extracted data for cemtrale europe 
for model upscaling. 

all the rasters data output are stored in directory : "~/archive_projects/index_based_physiological_drought"
