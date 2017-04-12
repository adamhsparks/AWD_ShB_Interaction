
# Alternate Wetting and Drying (AWD) and Nitrogen Interaction Effects on Rice Sheath Blight

## Quick start with this repository

Install the [`ProjectTemplate`](https://cran.r-project.org/package=ProjectTemplate) package. 

```r
install.packages("ProjectTemplate")
library(ProjectTemplate)
```
Clone the repository to your local hard drive.

This repository uses the [Packrat](https://cran.r-project.org/package=packrat) package for reproducibility so the initial start up may be slowed with package installations. This will automatically be handled so that all the versions of R packages used for this analysis are the same across computers.

The [munge](munge) folder contains all the preprocessing and raw data visualisation scripts. It is worth looking at these to see how the raw data are handled prior to analysis.

The [graphs](graphs) folder contains graphs from munging and the [README.md](graphs/README.md) file displays them in an ordered fashion.

### Rmd files

Knitting the Rmd files will automatically run the scripts in the [munge](munge) and generate the outputs linked to below in the GitHub `.md` files. This folder has the steps used to create the AUDPS values used in analysis and other summaries and cleaning of the data.

The [`Probability_distribution_checks.Rmd`](Probability_distribution_checks.md) file generates/displays the normal and log distribution checks for the analyses.

The [`Analysis.Rmd`](Analysis.md) file contains the models used for data analysis and shows the analysis carried out.

## Study

**Objective:** Study the effects of different water and nitrogen managements on sheath blight of rice  
**Project PI:** Dr. Adam H Sparks  

**Staff:**  

  * Dr. Nancy P. Castilla (2016)
  * Mr. Michael Noel (2015)

******

### 1. SCHEDULE

IRRI Dry Season (2)  

 * 2015 (December 2014 – April 2015)  
 * 2016 (January 2016 - May 2016)  

### 2. METHODOLOGY

**Experimental Design: Split-plot**

  * Main plot : Water management (2)  
    * W1 – Alternate Wetting and Drying (AWD)  
    * W2 – Flooded/Farmer’s Practice  
  
  * Sub-plot : Nitrogen management (5)
    * 2015  
      * N0 – 0 kg/ha  
      * N1 – 100 kg/ha (farmer’s practice – based from J. Bigornia research)  
      * N2 – 120 kg/ha (IRRI practice – based from L. Willocquet research)  
    * 2016  
      * N3 – 60 kg/ha  
      * N4 - 180 kg/ha  

  * Replication: Four (4)  
  * Plot Sizes (2015)  
    * Main plot size: 12m x 12m (144 sq m)
    * Sub-plot size: 5m x 5m (25 sq m)
    * Replication size: 12m x 24m (288 sq m)
    * Buffer: 1m per sub plot
    * Experiment size: 1,152 sq m

  * Plot Sizes (2016)  
    * Main plot size
      * Block 2007: 21m x 20.5m (412.5 sq m)
      * Block 2008: 20.25m x 21.6m (437.4 sq m)
    * Sub-plot size
      * Block 2007: 21m x 10.25m (215.25 sq m)
      * Block 2008: 20.25m x 10.8m (218.7 sq m)
    * Replication size:
      * Block 2007: 42m x 20.5m (861 sq m)
      * Block 2008: 40.5m x 21.6m (874.8 sq m)
    * Buffer: 0.5m per sub plot
    * Experiment size: 3471.6 sq m
    
******

**Nitrogen Management Rates**  

<table width = "500">
<tr>
  <th rowspan = "2">Nitrogen<br>treatment</th>
  <th rowspan = "2">Year</th>
  <th rowspan = "2">Total N<br>(kg/ha)</th>
  <th colspan = "4">Application Amount (kg/ha)</th>
</tr>
<tr>
  <th>Basal</th>
  <th>Tillering</th>
  <th>Panicle<br>Intitiation</th>
</tr>
<tr>
  <td>N0</td>
  <td>2015</td>
  <td>0</td>
  <td>0</td>
  <td>0</td>
  <td>0</td>
</tr>
<tr>
  <td>N1</td>
  <td>2015</td>
  <td>100</td>
  <td>60</td>
  <td>20</td>
  <td>20</td>
</tr>
<tr>
  <td>N2</td>
  <td>2015</td>
  <td>120</td>
  <td>60</td>
  <td>30</td>
  <td>30</td>
</tr>
<tr>
  <td>N3</td>
  <td>2016</td>
  <td>60</td>
  <td>30</td>
  <td>30</td>
  <td>0</td>
</tr>
<tr>
  <td>N4</td>
  <td>2016</td>
  <td>180</td>
  <td>60</td>
  <td>60</td>
  <td>60</td>
</tr>
</table>

## Resources

### glmm split plot analysis
http://ase.tufts.edu/gsc/gradresources/guidetomixedmodelsinr/mixed%20model%20guide.html

http://www.stat.wisc.edu/courses/st572-larget/Spring2007/handouts16-4.pdf

http://www.maths.bath.ac.uk/~jjf23/mixchange/split.html

### Using MCMCglmm

http://www.maths.bath.ac.uk/~jjf23/mixchange/split.html#mcmcglmm

https://github.com/tmalsburg/MCMCglmm-intro

### General GLMM in R
http://bbolker.github.io/mixedmodels-misc/glmmFAQ.html
