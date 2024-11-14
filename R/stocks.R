#' Produces a summary table of the stocks / statistical areas / sampling areas for one or more species
#'
#' @param species_codes a vector of species codes that have a stock / sampling area assigned
#' @return a _flextable_ table summarizing the information by species
#' @export
stock.viz.summary = function(species_codes = NULL) {
  stock_summary = stock.summary(species_codes)

  stock_summary$SCIENTIFIC_NAME = NULL
  stock_summary$NAME_EN = NULL
  stock_summary$NAME_ES = NULL
  stock_summary$NAME_FR = NULL

  stock_table =
    flextable(stock_summary) %>%
    bg(part = "all", bg = "white") %>%
    set_header_labels(values = list(SPECIES_GROUP         = "Species group",
                                    SPECIES_CODE          = "Species code",
                                    STOCK_CODE            = "Stock code",
                                    STATISTICAL_AREA_CODE = "Statistical area code",
                                    NUM_SAMPLING_AREAS    = "Num. sampling areas")) %>%
    bg(part = "header", bg = "gray") %>%
    bold(part = "header") %>%
    merge_v(j = c(1, 2), part = "body") %>%
    valign (j = c(1, 2),  valign = "top", part = "body") %>%

    border(j = 1:5, i = stock_summary[, .(ROW = max(.I)), keyby = .(SPECIES_GROUP)]$ROW, border.bottom = fp_border(), part = "body") %>%
    border(j = 2:5, i = stock_summary[, .(ROW = max(.I)), keyby = .(SPECIES_CODE) ]$ROW, border.bottom = fp_border(), part = "body") %>%

    border(border.left   = fp_border(), j = 5, part = "all") %>%

    autofit() %>%

    fix_border_issues()

  return(
    stock_table
  )
}

#' Produces a summary table of the stocks / ssampling areas for one or more species
#'
#' @param species_codes a vector of species codes that have a stock / sampling area assigned
#' @return a _flextable_ table summarizing the information by stock
#' @export
stock.viz.data = function(species_codes = NULL) {
  stock_data = iccat.pub.data::REF_STOCKS

  if(!is.null(species_codes)) stock_data = stock_data[SPECIES_CODE %in% species_codes]

  current_statistical_areas = unique(stock_data[STATISTICAL_AREA_CODE != "--"]$STATISTICAL_AREA_CODE)
  current_stocks            = unique(stock_data[STOCK_CODE != "--"]$STOCK_CODE)

  has_statistical_areas = length(current_statistical_areas) > 0
  has_stocks            = length(current_stocks) > 0

  table =
    flextable(stock_data[, .(STOCK_CODE, STATISTICAL_AREA_CODE, SAMPLING_AREA_CODE)]) %>%
    bg(part = "all", bg = "white") %>%
    set_header_labels(values = list(STOCK_CODE            = "Stock code",
                                    STATISTICAL_AREA_CODE = ifelse(has_stocks, "Statistical area code", "Management area code"),
                                    SAMPLING_AREA_CODE    = "Sampling area code")) %>%
    bg(part = "header", bg = "gray") %>%
    bold(part = "header") %>%
    valign(j = 1:3, part = "body", valign = "top")

  if(has_statistical_areas) {
    table =
      table %>%
      merge_v(j = 1, part = "body", combine = TRUE) %>%
      merge_v(j = 2, part = "body", combine = TRUE)
  } else {
    table =
      table %>%
      merge_v(j = c(1, 2), part = "body", combine = TRUE)
  }

  table =
    table %>%
    border(j = 1:3, part = "body", i = stock_data[, .(STOCK_CODE, STATISTICAL_AREA_CODE, SAMPLING_AREA_CODE)][, .(ROW = max(.I)), keyby = .(STOCK_CODE)]$ROW, border.bottom = fp_border()) %>%
    border(j = 2:3, part = "body", i = stock_data[, .(STATISTICAL_AREA_CODE, SAMPLING_AREA_CODE)]            [, .(ROW = max(.I)), keyby = .(STATISTICAL_AREA_CODE)]$ROW, border.bottom = fp_border())

  if(!has_statistical_areas) {
    table =
      table %>%
      delete_columns("STATISTICAL_AREA_CODE") %>%
      merge_v(j = 1, part = "body")
  }

  if(!has_stocks) {
    table =
      table %>%
      delete_columns(j = 1) %>%
      merge_v(j = 2, part = "body")
  }

  table =
    table %>%
    autofit() %>%
    fix_border_issues()

  return(
    table
  )
}
