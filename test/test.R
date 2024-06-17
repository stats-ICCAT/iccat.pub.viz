library(iccat.dev.data)
library(iccat.pub.viz)
library(iccat.pub.maps)
library(iccat.pub.plots)
library(rmarkdown)
library(knitr)
library(officedown)

#MAX_NUM_GEARS = 8
LAST_N_YEARS  = 30
#MAX_PERC_CUM  = 95

NC = t1nc(species_codes = "ALB", exclude_zero_catches = TRUE, exclude_discarded_live = TRUE)

NC[Stock == "MED", Stock := "ALB-M"]
NC[Stock == "ATN", Stock := "ALB-N"]
NC[Stock == "ATS", Stock := "ALB-S"]

update_trends_table = function(trends_table, last_n_years = LAST_N_YEARS, font_size = 7, columns_to_remove = NULL, by_gears = TRUE) {
  first_column = ifelse(by_gears, 3, 2)

  TT =
    trends_table %>%
    width   (j = 1, width = 3.0, unit = "cm") %>%
    width   (j = 2, width = 2.0, unit = "cm") %>%
    fontsize(part = "all",  size = font_size) %>%
    fontsize(part = "body", size = font_size, j = first_column:(first_column + last_n_years - 1)) %>%
    width   (j = first_column:(first_column + last_n_years - 1), width = 1.5, unit = "cm")

  if(!is.null(columns_to_remove)) {
    TT = TT %>%
      delete_columns(columns_to_remove) %>%
      merge_v(j = 1)
  }

  #TT = TT %>% line_spacing(space = .1, part = "all") %>% padding(padding.bottom = 2, part = "all")

  if(!by_gears)
    TT = TT %>% valign(j = 1, valign = "center", part = "body")

  TT = TT %>%
    height(part = "body", unit = "cm", height = .5) %>%
    hrule (part = "all", rule = "exact")

  return(
    TT
  )
}

ALB_M_trends = t1nc.viz.trends.table(NC[Stock == "ALB-M"], year_min = max(NC$YearC) - LAST_N_YEARS + 1, by_species = FALSE, by_catch_type = FALSE, by_stock = FALSE, by_gear = FALSE)

render("./test.Rmd",
       output_dir    = ".",
       output_file   = "test.pptx"
)
