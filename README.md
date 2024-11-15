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

#### Nominal catch trends of Albacore tuna for the years 1994-2023, stratified by fleet, species, gear group, stock and catch type
```
t1nc.viz.trends.table(T1[Species == "ALB"], year_min = 1994, year_max = 2023)
```
![image](https://github.com/user-attachments/assets/68802982-44ef-4a9f-b95c-e4261d9d8d2b)

#### Nominal catch trends of Albacore tuna for the years 1994-2023, stratified by fleet and stock 
```
t1nc.viz.trends.table(T1[Species == "ALB"], year_min = 1994, year_max = 2023, by_species = FALSE, by_stock = TRUE, by_gear = FALSE, by_catch_type = FALSE)
```
![image](https://github.com/user-attachments/assets/ff63fc57-96b6-4287-bc74-373ddf4bccc5)

#### Nominal catch trends of Albacore tuna for the years 1994-2023, stratified by fleet and stock, limited to strata accounting for a cumulative maximum of 95% of total catches, and with catch gradients turned on
```
t1nc.viz.trends.table(T1[Species == "ALB"], year_min = 1994, year_max = 2023, by_species = FALSE, by_stock = TRUE, by_gear = FALSE, by_catch_type = FALSE, max_cumulative_percentage = .95, rank = TRUE, show_catches_gradient = TRUE)
```
![image](https://github.com/user-attachments/assets/8cfeba26-d8f6-4f5b-8859-bc52a3a4a3dc)

#### Nominal catch trends of Albacore tuna for the years 1994-2023, stratified by fleet and stock, limited to strata accounting for a cumulative maximum of 95% of total catches, with catch gradients turned on and with sensitivity increased to 50%
```
t1nc.viz.trends.table(T1[Species == "ALB"], year_min = 1994, year_max = 2023, by_species = FALSE, by_stock = TRUE, by_gear = FALSE, by_catch_type = FALSE, rank = TRUE, show_catches_gradient = TRUE, max_cumulative_percentage = .95, sensitivity = .5)
```
![image](https://github.com/user-attachments/assets/c21a79fe-2543-488a-b5b2-95dcb05bcd73)

### T1 nominal catch tables

#### Nominal catch SCRS global table for Albacore tuna (1994-2023)
```
t1nc.viz.executive_summary.table.global(T1[Species == "ALB" & YearC %in% 1994:2023])
```
![image](https://github.com/user-attachments/assets/38f3bc83-1c83-4aac-b7bc-28757fd078cc)

#### Nominal catch SCRS gears table for Albacore tuna (1994-2023)
```
t1nc.viz.executive_summary.table.global(T1[Species == "ALB" & YearC %in% 1994:2023])
```
![image](https://github.com/user-attachments/assets/5dc02ca5-ac68-4fb3-81d7-4835573f37fd)

#### Nominal catch SCRS CPC table for Albacore tuna (1994-2023)
```
t1nc.viz.executive_summary.table.CPCs(T1[Species == "ALB" & YearC %in% 1994:2023])
```
![image](https://github.com/user-attachments/assets/552b6798-5e7a-42bd-8bea-d757e793413e)

#### Nominal catch SCRS full table for Albacore tuna (1994-2023)
```
t1nc.viz.executive_summary.table.full(T1[Species == "ALB" & YearC %in% 1994:2023])
```
![image](https://github.com/user-attachments/assets/08a0aaba-cbb8-4718-8cf4-cc21152d4e81)
![image](https://github.com/user-attachments/assets/c76cf81c-8b1e-43cd-8b3a-b9c1c593a98c)

#### Nominal catch SCRS Excel output for Albacore tuna (1994-2023)
```
t1nc.viz.executive_summary.table.all.xlsx(T1[Species == "ALB" & YearC %in% 1994:2023], output_file = "./ALB_1994_2023.xlsx")
```
> Executing the statement above will result in creating the `ALB_1994_2023.xlsx` file in the session's working directory

![image](https://github.com/user-attachments/assets/d6c40a63-6aaa-46f5-9179-73d018873eba)

#### Nominal catch SCRS Excel output for Albacore and Bluefin tuna (1994-2023)
```
t1nc.viz.executive_summary.table.all.xlsx(T1[Species %in% c("ALB", "BFT") & YearC %in% 1994:2023], output_file = "./ALB_BFT_1994_2023.xlsx")
```
> Executing the statement above will result in creating the `ALB_BFT_1994_2023.xlsx` file in the session's working directory

![image](https://github.com/user-attachments/assets/976d140d-2337-4205-b103-63f66ba91a0b)
![image](https://github.com/user-attachments/assets/f4877b2b-d73b-4848-8b5c-47ad8b5d0420)

#### Nominal catch SCRS Excel output for temperate tunas (1994-2023)
```
t1nc.viz.executive_summary.table.all.species_group.xlsx(T1[Species %in% c("ALB", "BFT") & YearC %in% 1994:2023],
                                                        species_group_code = "TEMP", 
                                                        species_group_description = 
                                                          list(
                                                            NAME_EN = "Temperate tunas", 
                                                            NAME_FR = "Thons tempérés", 
                                                            NAME_ES = "Atunes templados"
                                                          ), 
                                                        output_file = "./TEMP_1994_2023.xlsx")
```
> Executing the statement above will result in creating the `TEMP_1994_2023.xlsx` file in the session's working directory

![image](https://github.com/user-attachments/assets/cebfb1fb-0486-40d8-936d-14232949b0bd)
![image](https://github.com/user-attachments/assets/3c4ff06e-61ed-498c-a899-8846a70aa0b8)

### SCRS catalogues

#### Table legend (textual)
```
catalogue.viz.table.legend()
```
![image](https://github.com/user-attachments/assets/97083a2a-fe5b-4dc8-ab0e-71c14c0b7e64)

#### Table legend (colors)
```
catalogue.viz.table.legend.colours()
```
![image](https://github.com/user-attachments/assets/b5318398-c7f5-41dc-ba47-61050cea6da6)

#### SCRS catalogue table for Albacore and Bluefin tuna, for the years 1994-2023, stratified by species, stock, flag, and gear.
```
# ALB_BFT_FR = catalogue.fn_getT1NC_fisheryRanks(species_codes = c("ALB", "BFT"), year_from = 1994) # Requires access to the iccat.dev.data library (and to the ICCAT databases)
# ALB_BFT_CA = catalogue.fn_genT1NC_CatalSCRS   (species_codes = c("ALB", "BFT"), year_from = 1994) # Requires access to the iccat.dev.data library (and to the ICCAT databases)

# ALB_BFT_CAT = catalogue.compile(fishery_ranks_data = ALB_BFT_FR, catalogue_data = ALB_BFT_CA) # The catalogue.compile function is part of the iccat.pub.data library

catalogue.viz.table(ALB_BFT_CAT)
```
![image](https://github.com/user-attachments/assets/71e4e3b6-c5cd-44d2-9dc2-0117467201a5)

#### SCRS catalogue table for temperate tunas, for the years 1994-2023, stratified by flag and gear.
```
# TEMP_FR = catalogue.fn_getT1NC_fisheryRanks(species_codes = c("ALB", "BFT"), year_from = 1994) # Requires access to the **iccat.dev.data** library (and to the ICCAT databases)
# TEMP_CA = catalogue.fn_genT1NC_CatalSCRS   (species_codes = c("ALB", "BFT"), year_from = 1994) # Requires access to the **iccat.dev.data** library (and to the ICCAT databases)

# TEMP_CAT = catalogue.compile(fishery_ranks_data = TEMP_FR, catalogue_data = TEMP_CA, remove_species = TRUE, remove_stock = TRUE) # The **catalogue.compile** function is part of the **iccat.pub.data** library

catalogue.viz.table(ALB_BFT_CAT, remove_species = TRUE, remove_stock = TRUE)
```
![image](https://github.com/user-attachments/assets/5c74bb2d-3cff-462e-80d8-cab50a40f9aa)

#### SCRS catalogue for albacore and  bluefin tuna, for the years 2004-2023, as an Excel file
```
# Require access to the iccat.dev.data library (and to the ICCAT databases)
# ALB_FR = catalogue.fn_getT1NC_fisheryRanks(species_codes = "ALB")
# ALB_CA = catalogue.fn_genT1NC_CatalSCRS   (species_codes = "ALB")

ALB_CAT = catalogue.compile(fishery_ranks_data   = ALB_FR,
                            catalogue_data       = ALB_CA,
                            pretty_print_catches = FALSE, year_from = 2004)

# Require access to the iccat.dev.data library (and to the ICCAT databases)
# BFT_FR = catalogue.fn_getT1NC_fisheryRanks(species_codes = "BFT")
# BFT_CA = catalogue.fn_genT1NC_CatalSCRS   (species_codes = "BFT")

BFT_CAT = catalogue.compile(fishery_ranks_data  = BFT_FR,
                           catalogue_data       = BFT_CA,
                           pretty_print_catches = FALSE, year_from = 2004)

# Creates an empty Excel workbook
output_workbook = openxlsx2::wb_workbook()

# Currently, catalogue scores can only be calculated for a given species and stock
# (using the dbSTAT.dbo.sp_obtainMultipleScores function) and not for a species as a whole.
# For this reason, the score parameter in the below function calls is set to NA

# Appends the albacore tuna catalogue to the Excel workbook, limiting the outputs to all strata accounting for up to 60% of total catches
# and putting the cutoff line at the end of the first stratum accounting for 50% of total catches 
catalogue.viz.table.xlsx.append(
  workbook = output_workbook,
  filtered_catalogue_data = ALB_CAT,
  cutoff_percentage = 50,
  max_percentage = 60,
  stock = "ALB-ALL",
  table_number = 1,
  score = NA, 
  table_label = "Albacore tuna catalogue"
)

# Appends the bluefin tuna catalogue to the Excel workbook, limiting the outputs to all strata accounting for up to 60% of total catches
# and putting the cutoff line at the end of the first stratum accounting for 50% of total catches 
catalogue.viz.table.xlsx.append(
  workbook = output_workbook,
  filtered_catalogue_data = BFT_CAT,
  cutoff_percentage = 60,
  max_percentage = 70,
  stock = "BFT-ALL",
  table_number = 2,
  score = NA, 
  table_label = "Bluefin tuna catalogue"
)

# Saves the workbook to an XLSX file
output_workbook$save(file = "./TEMP_SCRS_Catalogue.xlsx")
```
![image](https://github.com/user-attachments/assets/876461cf-bd37-469e-a3d6-816f78ef2137)
![image](https://github.com/user-attachments/assets/5f215ac5-5318-4471-8b8f-b77f28137d94)

#### SCRS catalogue for north Atlantic and mediterranean albacore tuna, for the years 2004-2023, as an Excel file
```
# Require access to the iccat.dev.data library (and to the ICCAT databases)
# ALB_MED_FR = catalogue.fn_getT1NC_fisheryRanks(species_codes = "ALB", stock_area_codes = "MED")
# ALB_MED_CA = catalogue.fn_genT1NC_CatalSCRS   (species_codes = "ALB", stock_area_codes = "MED")

ALB_MED_CAT = catalogue.compile(fishery_ranks_data   = ALB_MED_FR,
                                catalogue_data       = ALB_MED_CA,
                                pretty_print_catches = TRUE, year_from = 2004)

# Require access to the iccat.dev.data library (and to the ICCAT databases)
# ALB_ATN_FR = catalogue.fn_getT1NC_fisheryRanks(species_codes = "ALB", stock_area_codes = "ATN") 
# ALB_ATN_CA = catalogue.fn_genT1NC_CatalSCRS   (species_codes = "ALB", stock_area_codes = "ATN") 

ALB_ATN_CAT = catalogue.compile(fishery_ranks_data   = ALB_ATN_FR,
                                catalogue_data       = ALB_ATN_CA,
                                pretty_print_catches = TRUE, year_from = 2004)

# Requires access to the ICCAT dbSTAT database 
calculate_score = function(stock) { 
  SQL = paste0("
     SELECT *
     FROM dbo.sp_obtainMultipleScores('ALB', '", stock, "', 20, 2023)
     WHERE YrTo = 2023
  ")
  
  results = tabular_query(
    connection = DB_STAT(),
    statement = SQL
  ) 
  
  return(results$Score)
}

# Creates an empty Excel workbook
output_workbook = openxlsx2::wb_workbook()

# Appends the Mediterranean albacore tuna catalogue to the Excel workbook, limiting the outputs to all strata accounting for up to 95% of total catches
# and putting the cutoff line at the end of the first stratum accounting for 90% of total catches 
catalogue.viz.table.xlsx.append(
  workbook = output_workbook,
  filtered_catalogue_data = ALB_MED_CAT,
  cutoff_percentage = 90,
  max_percentage = 95,
  stock = "ALB-MED",
  table_number = 1,
  score = calculate_score("MED"), # Requires access to the ICCAT dbSTAT database 
  table_label = "Mediterranean albacore tuna catalogue"
)

# Appends the north Atlantic albacore tuna catalogue to the Excel workbook, limiting the outputs to all strata accounting for up to 95% of total catches
# and putting the cutoff line at the end of the first stratum accounting for 90% of total catches 
catalogue.viz.table.xlsx.append(
  workbook = output_workbook,
  filtered_catalogue_data = ALB_ATN_CAT,
  cutoff_percentage = 90,
  max_percentage = 95,
  stock = "ALB-ATN",
  table_number = 2,
  score = calculate_score("ATN"), # Requires access to the ICCAT dbSTAT database
  table_label = "Northern Atlantic albacore tuna catalogue"
)

# Saves the workbook to an XLSX file
output_workbook$save(file = "./ALB_MED_ATN_Catalogue.xlsx")
```
![image](https://github.com/user-attachments/assets/76051046-e959-400b-a85d-77a560c100ec)
![image](https://github.com/user-attachments/assets/3216b4db-00a8-4ac5-a0c4-98c42161a51b)

## Future extensions
+ [ ] standardize functions' signatures for all different types of visualization
+ [ ] update the function producing the T1 nominal catch [static table legend](#static-table-legend) to also consider changes in sensitivity 
+ [ ] extend the function producing the tabular version of the SCRS catalogue to also show (in light blue) cells for which there is T2 data but not T1 data (this is already available in the Excel version of the catalogue)
+ [ ] add options to remove flag and gear from the SCRS catalogue stratification
+ [ ] update the `dbSTAT.dbo.sp_obtainMultipleScores` function to calculate scores for *all* stocks of a given species, as this feature would be of interest when producing the catalogue for a species regardless of its stock areas
