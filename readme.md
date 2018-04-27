# rB3

Data handling tools for high-frequency lake monitoring buoy observations

## Motivation
Environmental observations are inherently noisy. By date, few resources are made available for generic QA/C of high-frequency (HF) environmental observations (e.g. Horsburgh et al., 2015). Often more specific and complex processes and analysis are required to produce sensible scientific results from lake HF observations. In scientific analysis, critical steps require generic QA/QC, data dimension reduction and representation, and finally analysis of coherence. Multiple lake buoy HF data analysis packages were produced on the philosophy of such scientific procedure through Global Lake Ecological Observatory Network (GLEON; e.g. physical stability - Read et al., 2011; energy flux - Woolway et al., 2015; lake metabolism - Winslow et al., 2016; fluorescence signal mining - Ruan et al., 2017; dissolved oxygen classification – Muraoka et al., in press). Authors of this package were heavily involved in intitiatin and development of B3 software, a lake buoy data QA/QC graphic user interface executable, created by the University of Waikato Lakes Ecosystem Research New Zealand (LERNZ: https://www.lernz.co.nz/tools-and-resources/b3). Siince the initial development of B3 in 2012, LERNZ is continuously receiving download requests, inqueries and request to be in the mailing list, primarily through GLEON community. Although B3 software focuses on usability and standardized practice, further development is not feasible because (a) the software is made in C# language which is difficult for scientific community to collaborate on, (b) development is semi-completed with solid GUI base, meaning it is difficult to add customised functions, and (c) due to its free-ware nature, the project was not developed for open source ready. Furthermore, it is reported that B3 may have issue handling truly large data, and also was not produced on the basis of extension to near future streaming practices. This package rB3, a set of R-language functions, was initiated to overcome these issues; (a) R is a well-used statistical computing language in GLEON and scientific community, (b) modularised functions makes additional or specialized functions very easy to develop, and (c) developed on GitHub with open source and open access ready. We are interested in continuous development of the package, especially to bridge a typical buoy raw data with other available lake analyisis resources. The project was initiated with a strong support from the Waikato Regional Council.

## How does it work


## Module categories

Modules can be classified by their functionalities: (1) file I/O functions, (2) data wrangling functions, and (3) data editing functions, and (4) sub-functions / utilities. Tasks of (1) includes reading and writing data files, as well as saving plot functions. Tasks of category (2) modifies data, but as a results, data shapes are transformed. Example of these might be removing specific variables, or down sample from the original timeseries. Category (3) tasks also modify

### File I/O

### Data wrangling

### Data modification

### sub-functions / utilities

### Custom functions

```
test
```


``` 
Example (outdated)
Use mixed temperature differences to align temperature readings. 
Mixed periods are defined by dTmp < Value1%dTmp, wndspd > Value2%wndspd

Require: 
Vars, StartDate, EndDate, Value1 = percentile temperature differences, Value2 = percentile wind speed
```

## Module categories

### Output (longformat) contents control
#### Sensor_model
#### Sensor_serial

## Getting started

### Prerequisites

To be updated

### Installing

To be updated 

```
To be updated 
```

## Example

To be updated

## Versioning


## Citation

To be updated

## Authors


## License


## Acknowledgments

## References
Horsburgh, J. S., S. L. Reeder, A. S. Jones, and J. Meline. 2015. Open source software for visualization and quality control of continuous hydrologic and water quality sensor data. Environ. Model. Softw. 70: 32–44. doi:10.1016/j.envsoft.2015.04.002

Ruan, G., Hanson, P. C., Dugan, H. A., & Plale, B. 2017. Mining lake time series using symbolic representation. Ecological Informatics, 39, 10-22.

Read, J. S., D. P. Hamilton, I. D. Jones, K. Muraoka, L. A. Winslow, R. Kroiss, C. H. Wu, and E. Gaiser. 2011. Derivation of lake mixing and stratification indices from high-resolution lake buoy data. Environ. Model. Softw. 26: 1325–1336. doi:10.1016/j.envsoft.2011.05.006

Winslow, L. A., J. A. Zwart, R. D. Batt, H. A. Duggan, R. I. Woolway, J. R. Corman, P. C. Hanson, and J. S. Read. 2016. LakeMetabolizer: an R package for estimating lake metabolism from free-water oxygen using diverse statistical models. Inl. waters 6.

Woolway, R. I., I. D. Jones, D. P. Hamilton, S. C. Maberly, K. Muraoka, J. S. Read, R. L. Smyth, and L. A. Winslow. 2015. Automated calculation of surface energy fluxes with high-frequency lake buoy data. Environ. Model. Softw. 70: 191–198. doi:10.1016/j.envsoft.2015.04.013
