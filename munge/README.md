# Alternate Wetting and Drying (AWD) Sheath Blight Interaction Study

## File list

*01-preprocess_2015.R* - this R script reformats the data from wide to long for
2015  

*02-preprocess_2016.R* - this R script reformats and fills the date column in
full and fills the NTIL and NTShB columns for the entire hill where raw data
only has the first observation for each hill filled.

*03-preprocess_data.R* - this R script calculates AUDPS for leaves and tillers
and merges 2015 and 2016 data.

*visualise_data.R* - this R script generates graphs of the raw data and
boxplots of the AUDPS data and puts them in the "graphs" folder.
