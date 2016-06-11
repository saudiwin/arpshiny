This package exists to load code and data for the ARP Shiny application at <http://kubinec.shinyapps.io/ARP_Shiny>.

It comprises three datasets and plotting functions for the R package emIRT. 

### Datasets: 

* "all_matrices": voting data for nearly 1700 laws and amendments from the 2014-2016 Tunisian parliament. This data was provided by the Tunisian NGO Bawsala.
* "info_legis": includes party information for each of the legislators.
* "model_results": estimates from a binary IRT model of voting data from the 2014-2016 Tunisian parliament. Includes ideal point estimates for both legislators and bills, along with 95% confidence intervals.

### Plot functions:

Function plot.emIRT() implements a standard S3 plotting function for each type of IRT model in the emIRT package, including the dynIRT object (dynamic IRT). Plotting is done via the ggplot2 package, and the function produces ggplot2 objects which can be further manipulated. The function includes options for subsetting and re-labeling data. It can also use rc objects from the pscl package to label legislators. For dynamic IRT, the function produces a facet_wrap of different years from the model.

This function has been tested on all example data from the emIRT package.
