# H-1B Visa Petitions Exploratory Data Analysis

The H-1B is an employment-based, non-immigrant visa category for temporary foreign workers in the United States. Every year, the US immigration department receives over 200,000 petitions and selects 85,000 applications through a random process. The application data is available for public access to perform in-depth longitudinal research and analysis. This data provides key insights into the prevailing wages for job titles being sponsored by US employers under H1-B visa category. In particular, I utilize the 2011-2016 H-1B petition disclosure data to analyze the employers with the most applications, data science related job positions and relationship between salaries offered and cost of living index.



## Data Set Source
The Office of Foreign Labor Certification (OFLC) generates program data that is useful information about the immigration programs including the H1-B visa. The disclosure data updated annually is available at https://www.foreignlaborcert.doleta.gov/performancedata.cfm

- Click on Disclosure Data tab
- Go to Section LCA Programs (H-1B, H-1B1, E-3)
- You will find data from 2008 onwards.

## Requirements
- R
- R Studio
- Packages: readxl, dplyr, hashmap, ggplot2, ggmap, ggrepel

Use `install.packages("package_name")` to install new packages in R.

## Files

- [data_processing.Rmd](https://github.com/sharan-naribole/H1B_visa_eda/blob/master/data_processing.Rmd): R notebook performing the key data transformations on the raw dataset.
- [data_analysis.Rmd](https://github.com/sharan-naribole/H1B_visa_eda/blob/master/data_analysis.Rmd): R notebook with code for plots and corresponding 
- [helpers.R](https://github.com/sharan-naribole/H1B_visa_eda/blob/master/helpers.R): helper functions used mainly for data analysis
- [spell_correcter.R](https://github.com/sharan-naribole/H1B_visa_eda/blob/master/spell_correcter.R): A suite of functions for performing spell correction in a given vector using the frequencies of occurrence of different elements in the vector.
- [coli/](https://github.com/sharan-naribole/H1B_visa_eda/tree/master/coli): Python Scrapy code directory for scraping cost of living plus rent index. The [spider crawl file](https://github.com/sharan-naribole/H1B_visa_eda/blob/master/coli/coli/spiders/coli.py) is the main file describing how the data should be scraped.

## Shiny app
I extended this project to build a Shiny app based on the transformed data set. 

- [Explore the app!](https://sharan-naribole.shinyapps.io/h_1b/)

- [GitHub repo](https://github.com/sharan-naribole/H1b_visa_shiny)

## Blogs

Please read my blogs for key data insights and more details:
 - [Data Analysis](http://blog.nycdatascience.com/student-works/h-1b-visa-petitions-exploratory-data-analysis/)
 
 - [Shiny app](http://blog.nycdatascience.com/student-works/h-1b-visa-applications-exploration-using-shiny/)
 
## Acknowledgements

- [OFLC Performance data](https://www.foreignlaborcert.doleta.gov/performancedata.cfm)

- [How to Write a Spelling Corrector](http://norvig.com/spell-correct.html)
