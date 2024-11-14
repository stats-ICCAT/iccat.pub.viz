# ICCAT `visualization` `public` library

A set of functions to produce visualizations (rich formatted tables, Excel spreadsheets etc.) for different categories of documents and outputs.

This library is meant for public usage, and for this reason it does not have dependencies from the (development) ICCAT libraries that provide access to the databases.

## Artifacts that can be produced using the functions provided by the library

1) [Stocks](#stocks-metadata)
+ Summary table by species / stock
+ Available sampling areas by stock
2) [T1 nominal catch trends](#t1-nominal-catch-trends)
+ Legend
+ Catch trend table
3) [T1 nominal catch tables](#t1-nominal-catch-tables)
+ Global
+ By gear
+ By CPC
+ Combined
+ Single-species Excel table
+ Species group Excel table
4) [SCRS catalogues](#scrs-catalogues)
+ Catalogue text and color legends
+ Catalogue table
+ Catalogue Excel outputs

## External dependencies (CRAN) <a name="external_deps"></a>
+ `data.table`
+ `flextable`
+ `officer`

### Installation
```
install.packages(c("data.table", "flextable", "officer"))
```

## Internal dependencies <a name="internal_deps"></a>
+ [iccat.pub.data](https://github.com/stats-ICCAT/iccat.pub.data)
+ [iccat.pub.aes](https://github.com/stats-ICCAT/iccat.pub.aes)

### Installation (straight from GitHub)
```
library(devtools)

install_github("stats-ICCAT/iccat.pub.viz")
```
# Building the library

Assuming that all [external](#external_deps) and [internal](#internal_deps) dependencies are already installed in the R environment, and that the `devtools` package and [RTools](https://cran.r-project.org/bin/windows/Rtools/) are both available, the building process can be either started within R studio by selecting the Build > Build Source Package menu entry:

![image](https://github.com/user-attachments/assets/f209d8d4-568c-4200-bcf2-fb1fa0e1d2ef)

or by executing the following statement:

`devtools::document(roclets = c('rd', 'collate', 'namespace'))`

## Usage examples

### Loading the library

For the examples to work, the following statement should be executed once per session:

```
library(iccat.pub.viz)
```

> To run these examples we assume that the `T1NC` object contains all T1 nominal catch data as retrieved using the `iccat.dev.data::t1nc` function (i.e., `T1 = t1nc()`).
 
### Stocks metadata
> All necessary information to visualize stocks' metadata is included in the `iccat.pub.data` library, which is one of the direct dependency of this project.

#### Stock summary table (all species)
```
stock.viz.summary()
```
![image](https://github.com/user-attachments/assets/ccc06f62-c5a8-492c-bbca-ffc4659ca485)

#### Stock summary table for Albacore tuna and Bluefin tuna
```
stock.viz.summary(species_codes = c("ALB", "BFT"))
```
![image](https://github.com/user-attachments/assets/fc8c7f40-0a51-417c-a679-f35441f680fc)

#### Stock summary data for Albacore tuna and Bluefin tuna
```
stock.viz.data(species_codes = c("ALB", "BFT"))
```
![image](https://github.com/user-attachments/assets/2a1667cd-6c61-4787-942b-cb873f10339b)

### T1 nominal catch trends

#### Static table legend
```
t1nc.viz.trends.legend()
```
![image](https://github.com/user-attachments/assets/bfe7a222-2fa8-4587-a793-2d919d182199)

#### Nominal catch trends of Albacore tuna for the years 1994-2023
```
t1nc.viz.trends.table(T1[Species == "ALB"], year_min = 1994, year_max = 2023)
```
![image](https://github.com/user-attachments/assets/ba2dfbf0-d7bf-4b65-af01-db4aaa73ba24)

####

### T1 nominal catch tables

####
####
####

### SCRS catalogues
## Future extensions
+ [ ] standardize functions' signatures for all different types of visualizations
+ [ ] extend the function producing the tabular version of the SCRS catalogue to also show (in light blue) cells for which there is T2 data but not T1 data (this is already available in the Excel version of the catalogue) 
