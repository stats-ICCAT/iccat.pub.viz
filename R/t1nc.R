#' The table modeling the default T1NC trends classification limits with respect to the
#' relative magnitude of catch differences.
#'
#' @export
DEFAULT_TRENDS_REL_DIFF_LIMITS =
  list(
    T0_100      = list(LOW = .9),
    T100_1000   = list(LOW = .5 , MED = .9),
    T1000_10000 = list(LOW = .1,  MED = .5, HIGH = .9),
    T10000_     = list(LOW = .05, MED = .1, HIGH = .5, VERY_HIGH = .9)
  )

t1nc.viz.trends.table.bg_matrix = function(data_matrix) {
  bg_matrix =
    ifelse(data_matrix == "", "lightgray",
           ifelse(data_matrix == "+", "yellow",
                  ifelse(data_matrix == "++", "orange",
                         ifelse(data_matrix == "+++", "red",
                                ifelse(data_matrix == "++++", "darkred",
                                       ifelse(data_matrix == "0", "cyan",
                                              "white")
                                )
                         )
                  )
           )
    )

  return(
    bg_matrix
  )
}

t1nc.viz.trends.table.fg_matrix = function(data_matrix) {
  return(
    ifelse(data_matrix == "++++" | data_matrix == "+++", "white", "black")
  )
}

#' A printable version of the T1NC trends legend.
#'
#' @return a _flextable_ formatted according to the agreed style guides
#' @export
t1nc.viz.trends.legend = function() {
  legend = data.table(
    CATCH_MAGNITUDE = c("[0, 10)",
                        "[10, 100)",
                        "[100, 1,000)",
                        "[1,000, 10,000)",
                        "[10,000, ...)"),
    DELTA_0      = c("-",   "0", "0",   "0",    "0"),
    DELTA_0_5    = c("-",  "-",  "-",   "-",    "-"),
    DELTA_5_10   = c("-",  "-",  "-",   "-",    "+"),
    DELTA_10_50  = c("-",  "-",  "-",   "+",   "++"),
    DELTA_50_90  = c("-",  "-",  "+",  "++",  "+++"),
    DELTA_90_100 = c("-",  "+", "++", "+++", "++++"),
    DELTA_NA     = c("-",  "+", "++", "+++", "++++")
  )

  FT = flextable::flextable(legend)

  return(
    FT %>%
      fontsize(part = "all", size = 10) %>%
      align(align = "center", part = "all") %>%

      set_header_labels(values = c("", "0", "[0, 5)", "[5, 10)", "[10, 50)", "[50, 90)", "[90, 100]", "NA")) %>%
      add_header_row(values = c("Catch magnitude (t)", "Delta with other catches (%)"), colwidths = c(1, 7)) %>%

      bold(part = "header") %>%
      bg  (part = "header", bg = "grey", i = 1) %>%
      bg  (part = "header", bg = "lightgrey", i = 2) %>%

      bold(part = "body", j = 1) %>%
      bg  (part = "body",   bg = "lightgrey", j = 1) %>%

      border(i = 1:2, part = "header", border.bottom = fp_border(width = 1)) %>%
      merge_at(j = 1, i = 1:2, part = "header") %>%

      border_inner(border = fp_border(width = 1)) %>%

      border(j = 1, part = "all",  border.left  = fp_border(width = 1)) %>%
      border(j = 8, part = "all",  border.right = fp_border(width = 1)) %>%
      border(i = 1, part = "body", border.top   = fp_border(width = 2)) %>%

      bg   (j = 2:8, part = "body", bg    = t1nc.viz.trends.table.bg_matrix(legend[, 2:8])) %>%
      color(j = 2:8, part = "body", color = t1nc.viz.trends.table.fg_matrix(legend[, 2:8])) %>%
      width(j = 1, width = 4, unit = "cm")
  )
}

#' Produces a T1NC trends table analyzing the relative changes in catch magnitudes across a given time frame and with respect to several different levels
#' of stratification.
#'
#' @param t1nc_data T1 nominal catch data as retrieved using the \code{\link{iccat.dev.data::t1nc}} function
#' @param by_species if the stratification shall include species
#' @param by_stock if the stratification shall include stocks
#' @param by_gear if the stratification shall include gears
#' @param by_catch_type if the stratification shall include catch types
#' @param rank to show / hide the catch ranks of each strata
#' @param max_cumulative_percentage the upper limit of the cumulative catch percentage of the strata included in the final table
#' @param rel_diff_limits the table modeling the default T1NC trends classification limits
#' @param sensitivity a multiplicative factor (between 0 and 1) applied to the trends classification limits
#' @param show_catches_gradient to show / hide gradients for the cumulative catch limits
#' @param colorize_gears whether to colorize or not the cells showing the current gear code for the strata
#' @return the T1NC trends table (in _flextable_ format) produced for the provided T1 nominal catch data and in accordance with the chosen criteria
#' @export
t1nc.viz.trends.table = function(t1nc_data, year_min = NA, year_max = NA,
                                 by_species = TRUE, by_stock = TRUE, by_gear = TRUE, by_catch_type = TRUE,
                                 rank = FALSE, max_cumulative_percentage = NA,
                                 rel_diff_limits = DEFAULT_TRENDS_REL_DIFF_LIMITS, sensitivity = 0,
                                 show_catches_gradient = FALSE, colorize_gears = FALSE) {
  if(sensitivity < 0 | sensitivity > 1) stop("Sensitivity should be set to a value between 0 and 1 (both included)")

  if(!rank) {
    if(!is.na(max_cumulative_percentage)) stop("A maximum cumulative percentage can be provided only if the data is sorted by fishery rank (i.e., rank = TRUE)")
    if(show_catches_gradient) stop("The catch gradient can only be shown if the data is sorted by fishery rank (i.e., rank = TRUE)")
  }

  if(!is.na(max_cumulative_percentage) & ( max_cumulative_percentage < 0 | max_cumulative_percentage > 1 )) stop("The maximum cumulative percentage should be set to a value between 0 and 1 (both included)")

  sensitivity = min(1, max(0, 1 - sensitivity))

  T1NC_proc = t1nc.summarise(t1nc_data, year_min, year_max, by_species, by_stock, by_gear, by_catch_type, rank)

  T1NC_proc_m = T1NC_proc$raw
  T1NC_proc_m[, PREV_YEAR := YEAR - 1]

  formula_components = c()
  grouped_columns = 0

  formula_components = append(formula_components, "FLAG_CODE")
  grouped_columns = grouped_columns + 1

  if(by_species) {
    formula_components = append(formula_components, "SPECIES_CODE")
    grouped_columns = grouped_columns + 1
  }

  if(by_gear) {
    formula_components = append(formula_components, "GEAR_GROUP_CODE")
    grouped_columns = grouped_columns + 1
  }

  if(by_stock) {
    formula_components = append(formula_components, "STOCK_CODE")
    grouped_columns = grouped_columns + 1
  }

  if(by_catch_type) {
    formula_components = append(formula_components, "CATCH_TYPE_CODE")
    grouped_columns = grouped_columns + 1
  }

  formula = paste0(formula_components, collapse = " + ")
  formula = paste0(formula, " ~ YEAR")

  T1NC_proc_m =
    merge(T1NC_proc_m, T1NC_proc_m,
          by.x = append(formula_components, "PREV_YEAR"),
          by.y = append(formula_components, "YEAR"),
          all.x = TRUE, allow.cartesian = TRUE)

  colnames(T1NC_proc_m)[which(colnames(T1NC_proc_m) == "CATCH.x")] = "CATCH"
  colnames(T1NC_proc_m)[which(colnames(T1NC_proc_m) == "CATCH.y")] = "CATCH_PREV"

  T1NC_proc_m[ is.na(CATCH), DIFF := NA_real_ ]
  T1NC_proc_m[!is.na(CATCH), DIFF := CATCH - CATCH_PREV ]
  T1NC_proc_m[ is.na(CATCH), DIFF_REL := NA_real_ ]
  T1NC_proc_m[!is.na(CATCH) & !is.na(CATCH_PREV), DIFF_REL := abs(DIFF) / ifelse(CATCH > CATCH_PREV, CATCH, CATCH_PREV)]
  T1NC_proc_m[!is.na(CATCH) &  is.na(CATCH_PREV), DIFF_REL := 1]
  T1NC_proc_m[ is.na(CATCH) & !is.na(CATCH_PREV), DIFF_REL := 1]

  T1NC_proc_m[ is.na(CATCH) & !is.na(CATCH_PREV), LOG := floor(log10(CATCH_PREV))]
  T1NC_proc_m[!is.na(CATCH) &  is.na(CATCH_PREV), LOG := floor(log10(CATCH))]
  T1NC_proc_m[!is.na(CATCH) & !is.na(CATCH_PREV), LOG := floor(log10(ifelse(CATCH > CATCH_PREV, CATCH, CATCH_PREV)))]

  T1NC_proc_m[ is.na(CATCH), CHANGE := ""]
  T1NC_proc_m[!is.na(CATCH), CHANGE := "="]

  T1NC_proc_m[!is.na(CATCH) & LOG <= 1 & CATCH != 0 & CATCH_PREV != 0 & DIFF_REL >= rel_diff_limits$T0_100$LOW * sensitivity, CHANGE := "+"]

  T1NC_proc_m[!is.na(CATCH) & LOG >= 2 & DIFF_REL >= rel_diff_limits$T100_1000$LOW * sensitivity    , CHANGE := "+"]
  T1NC_proc_m[!is.na(CATCH) & LOG >= 2 & DIFF_REL >= rel_diff_limits$T100_1000$MED * sensitivity    , CHANGE := "++"]

  T1NC_proc_m[!is.na(CATCH) & LOG >= 3 & DIFF_REL >= rel_diff_limits$T1000_10000$LOW * sensitivity  , CHANGE := "+"]
  T1NC_proc_m[!is.na(CATCH) & LOG >= 3 & DIFF_REL >= rel_diff_limits$T1000_10000$MED * sensitivity  , CHANGE := "++"]
  T1NC_proc_m[!is.na(CATCH) & LOG >= 3 & DIFF_REL >= rel_diff_limits$T1000_10000$HIGH * sensitivity , CHANGE := "+++"]

  T1NC_proc_m[!is.na(CATCH) & LOG >= 4 & DIFF_REL >= rel_diff_limits$T10000_$LOW * sensitivity      , CHANGE := "+"]
  T1NC_proc_m[!is.na(CATCH) & LOG >= 4 & DIFF_REL >= rel_diff_limits$T10000_$MED * sensitivity      , CHANGE := "++"]
  T1NC_proc_m[!is.na(CATCH) & LOG >= 4 & DIFF_REL >= rel_diff_limits$T10000_$HIGH * sensitivity     , CHANGE := "+++"]
  T1NC_proc_m[!is.na(CATCH) & LOG >= 4 & DIFF_REL >= rel_diff_limits$T10000_$VERY_HIGH * sensitivity, CHANGE := "++++"]

  T1NC_proc_m[ is.na(CATCH) & !is.na(CATCH_PREV) & LOG == 0, CHANGE := ""]
  T1NC_proc_m[ is.na(CATCH) & !is.na(CATCH_PREV) & LOG == 1, CHANGE := "+"]
  T1NC_proc_m[ is.na(CATCH) & !is.na(CATCH_PREV) & LOG == 2, CHANGE := "++"]
  T1NC_proc_m[ is.na(CATCH) & !is.na(CATCH_PREV) & LOG == 3, CHANGE := "+++"]
  T1NC_proc_m[ is.na(CATCH) & !is.na(CATCH_PREV) & LOG >= 4, CHANGE := "++++"]

  T1NC_proc_m[!is.na(CATCH) &  is.na(CATCH_PREV) & LOG == 0 & YEAR != min(YEAR), CHANGE := "="]
  T1NC_proc_m[!is.na(CATCH) &  is.na(CATCH_PREV) & LOG == 1 & YEAR != min(YEAR), CHANGE := "+"]
  T1NC_proc_m[!is.na(CATCH) &  is.na(CATCH_PREV) & LOG == 2 & YEAR != min(YEAR), CHANGE := "++"]
  T1NC_proc_m[!is.na(CATCH) &  is.na(CATCH_PREV) & LOG == 3 & YEAR != min(YEAR), CHANGE := "+++"]
  T1NC_proc_m[!is.na(CATCH) &  is.na(CATCH_PREV) & LOG >= 4 & YEAR != min(YEAR), CHANGE := "++++"]

  T1NC_proc_m[!is.na(CATCH) &  is.na(CATCH_PREV) & CATCH == 0, CHANGE := "="]
  T1NC_proc_m[!is.na(CATCH) & !is.na(CATCH_PREV) & CATCH == CATCH_PREV & CATCH != 0 & LOG >= 1, CHANGE := "0"]

  T1NC_proc_m[YEAR == min(YEAR) & !is.na(CATCH), CHANGE := "="]

  T1NC_proc_m_w =
    dcast.data.table(
      T1NC_proc_m,
      formula = as.formula(formula),
      fun.aggregate = sum, #function(x) ifelse(is.na(x), NA_real_, x),
      value.var = "CATCH",
      drop = TRUE, fill = FALSE
    )

  T1NC_proc_m_d_w =
    dcast.data.table(
      T1NC_proc_m,
      formula = as.formula(formula),
      fun.aggregate = function(x) ifelse(is.na(x), "", x),
      value.var = "CHANGE",
      drop = TRUE, fill = FALSE
    )

  if(rank) {
    T1NC_proc_m_d_w =
      merge(T1NC_proc$grouped[, 1:(grouped_columns + 2)],
            T1NC_proc_m_d_w)[order(-AVG_CATCH_RATIO)]

    T1NC_proc_m_w =
      merge(T1NC_proc_m_w,
            T1NC_proc_m_d_w[, 1:grouped_columns])

    if(!is.na(max_cumulative_percentage))
      T1NC_proc_m_d_w = T1NC_proc_m_d_w[AVG_CATCH_RATIO_CUM <= max_cumulative_percentage ]

    T1NC_proc_m_d_w$AVG_CATCH_RATIO     = NULL
    T1NC_proc_m_d_w$AVG_CATCH_RATIO_CUM = NULL
  }

  last_row_by_flag = T1NC_proc_m_d_w[, .(ROW = max(.N)), keyby = .(FLAG_CODE)]
  last_row_by_flag[, ROW := cumsum(ROW)]

  bg_matrix = t1nc.viz.trends.table.bg_matrix(T1NC_proc_m_d_w[, (grouped_columns + 1):ncol(T1NC_proc_m_d_w)])
  fg_matrix = t1nc.viz.trends.table.fg_matrix(T1NC_proc_m_d_w[, (grouped_columns + 1):ncol(T1NC_proc_m_d_w)])

  if(by_gear) {
    gear_group_bg_matrix =
      lighten(
        merge(
          T1NC_proc_m_w[, .(GEAR_GROUP_CODE)],
          iccat.pub.aes::REF_GEAR_GROUPS_COLORS,
          by = "GEAR_GROUP_CODE",
          all.x = TRUE,
          sort = FALSE
        )$FILL,
        amount = .3
      )
  }

  if(rank) {
    T1NC_proc_m_w =
      merge(T1NC_proc$grouped[, 1:(grouped_columns + 2)],
            T1NC_proc_m_w)[order(-AVG_CATCH_RATIO)
                           ]
    if(!is.na(max_cumulative_percentage))
      T1NC_proc_m_w = T1NC_proc_m_w[AVG_CATCH_RATIO_CUM <= max_cumulative_percentage ]

    if(show_catches_gradient) {
      bg_matrix_catch = T1NC_proc_m_w[, .(AVG_CATCH_RATIO, AVG_CATCH_RATIO_CUM)]
      bg_matrix_catch$AVG_CATCH_RATIO     = rgb(.6, .6, 1, bg_matrix_catch$AVG_CATCH_RATIO     / max(bg_matrix_catch$AVG_CATCH_RATIO))
      bg_matrix_catch$AVG_CATCH_RATIO_CUM = rgb(.3, 1, .3, bg_matrix_catch$AVG_CATCH_RATIO_CUM / max(bg_matrix_catch$AVG_CATCH_RATIO_CUM))
    }

    T1NC_proc_m_w[, AVG_CATCH_RATIO     := format(round(AVG_CATCH_RATIO     * 100, 2), nsmall = 2)]
    T1NC_proc_m_w[, AVG_CATCH_RATIO_CUM := format(round(AVG_CATCH_RATIO_CUM * 100, 2), nsmall = 2)]
  }

  to_merge = c()

  if(!rank) {
                      to_merge = append(to_merge, "FLAG_CODE")
    if(by_species)    to_merge = append(to_merge, "SPECIES_CODE")
    #if(by_stock)     to_merge = append(to_merge, "STOCK_CODE")
    if(by_gear)       to_merge = append(to_merge, "GEAR_GROUP_CODE")
    #if(by_catch_type) to_merge = append(to_merge, "CATCH_TYPE_CODE")
  }

  delta = ifelse(rank, 2, 0)

  T1NC_FT =
    flextable(T1NC_proc_m_w) %>%
    # This formatter is absolutely necessary, otherwise cells with NA (numeric) will not be formatted correctly
    # and will not respect the line height set at table level
    set_formatter(values = function(v) { return(ifelse(is.na(v), "-", prettyNum(v, big.mark = ",", scientific = FALSE))) }, part = "body") %>%
    bg(part = "all", bg = "white") %>% # Default BG color
    set_header_labels(values = list(SPECIES_CODE    = "Species",
                                    STOCK_CODE      = "Stock",
                                    FLAG_CODE       = "Flag name",
                                    GEAR_GROUP_CODE = "Gear group",
                                    CATCH_TYPE_CODE = "Catch type",
                                    AVG_CATCH_RATIO = "%",
                                    AVG_CATCH_RATIO_CUM = "% (cum.)"
                                    )) %>%
    bold(part = "header") %>%

    padding(part = "all" , padding.left = 5, padding.right = 5) %>%
    padding(part = "body", padding.top  = 0, padding.bottom = 0)

  if(!rank) {
    T1NC_FT = T1NC_FT %>% merge_v(to_merge, combine = TRUE)

    if(by_species) T1NC_FT = T1NC_FT %>% merge_v(j = c("FLAG_CODE", "SPECIES_CODE"), combine = TRUE)

    T1NC_FT = T1NC_FT %>% merge_v(j = c("FLAG_CODE")) # Necessary to properly format the output
  }

  T1NC_FT = T1NC_FT %>%
    valign(j = "FLAG_CODE",                               part = "body",   valign = "top") %>%
    valign(j =  to_merge,                                 part = "body",   valign = "top") %>%
    valign(j = (grouped_columns + 1):ncol(T1NC_proc_m_w), part = "all",    valign = "center") %>%
    align(part = "header", align = "center") %>%
    align(part = "header", align = "left", j = "FLAG_CODE")

  if(rank) {
    T1NC_FT = T1NC_FT %>%
      valign(j = c("AVG_CATCH_RATIO", "AVG_CATCH_RATIO_CUM"), part = "body", valign = "center")
  }

  if(by_species)     T1NC_FT = T1NC_FT %>% align(part = "header", align = "left",  j = "SPECIES_CODE")
  if(by_gear)        T1NC_FT = T1NC_FT %>% align(part = "header", align = "left",  j = "GEAR_GROUP_CODE")
  #if(by_stock)       T1NC_FT = T1NC_FT %>% align(part = "header", align = "left",  j = "STOCK_CODE")
  #if(by_catch_type)  T1NC_FT = T1NC_FT %>% align(part = "header", align = "left",  j = "CATCH_TYPE_CODE")

  if(rank)           T1NC_FT = T1NC_FT %>% align(part = "body",   align = "right", j = c("AVG_CATCH_RATIO", "AVG_CATCH_RATIO_CUM"))

    #align(part = "header", align = "right", j = c("AVG_CATCH_RATIO", "AVG_CATCH_RATIO_CUM")) %>%

  T1NC_FT = T1NC_FT %>%
    bg    (                                                       part = "header", bg    = "grey") %>%
    bg    (j = (grouped_columns + 1 + delta):ncol(T1NC_proc_m_w), part = "body",   bg    = bg_matrix) %>%
    color (j = (grouped_columns + 1 + delta):ncol(T1NC_proc_m_w), part = "body",   color = fg_matrix) %>%

    border(part = "all",    border = fp_border(width = .5)) %>%
    border(part = "header", border.top = fp_border(width = 2), border.bottom = fp_border(width = 2))

  if(!rank)
    T1NC_FT = T1NC_FT %>%
      border(part = "body",   i = last_row_by_flag$ROW, border.bottom = fp_border(width = 2))

  if(rank & show_catches_gradient) {
    T1NC_FT = T1NC_FT %>% bg(part = "body", j = "AVG_CATCH_RATIO",     bg = bg_matrix_catch$AVG_CATCH_RATIO)
    T1NC_FT = T1NC_FT %>% bg(part = "body", j = "AVG_CATCH_RATIO_CUM", bg = bg_matrix_catch$AVG_CATCH_RATIO_CUM)
  }

  T1NC_FT = T1NC_FT %>%
    fontsize(part = "all", size = 7) %>%
    autofit() %>%
    fix_border_issues()

    if(by_stock)      T1NC_FT = T1NC_FT %>% align( j = "STOCK_CODE",      part = "body",    align = "center")
    if(by_catch_type) T1NC_FT = T1NC_FT %>% align( j = "CATCH_TYPE_CODE", part = "body",    align = "center")

    if(colorize_gears & by_gear)
      T1NC_FT = T1NC_FT %>% bg(j = "GEAR_GROUP_CODE", part = "body", bg = gear_group_bg_matrix)

  return(
    T1NC_FT
  )
}


standardize_catch_types = function(t1nc_data) {
  t1nc_data[CatchTypeCode %in% c("C", "FA", "L"),   CATCH_TYPE := "Landings"]
  t1nc_data[CatchTypeCode %in% c("LF"),             CATCH_TYPE := "Landings(FP)"]
  t1nc_data[CatchTypeCode %in% c("DD", "DL", "DM"), CATCH_TYPE := "Discards"]

  t1nc_data$CATCH_TYPE =
    factor(
      t1nc_data$CATCH_TYPE,
      levels = c("Landings", "Landings(FP)", "Discards"),
      labels = c("Landings", "Landings(FP)", "Discards"),
      ordered = TRUE
    )

  return(t1nc_data)
}

standardize_party_status = function(t1nc_data) {
  t1nc_data$PartyStatus =
    factor(
      t1nc_data$PartyStatus,
      levels = c("CP", "NCC", "NCO"),
      labels = c("CP", "NCC", "NCO"),
      ordered = TRUE
    )

  return(t1nc_data)
}

standardize_stocks = function(t1nc_data) {
  t1nc_data$Stock =
    factor(
      t1nc_data$Stock, # To establish a non-lexicographic order where MED comes after ATE but before ATW
      levels = c("", "A+M", "ATL", "ATN", "ANE", "ANW", "ATS", "ASE", "ASW", "ATE", "MED", "ATW"),
      labels = c("", "A+M", "ATL", "ATN", "ANE", "ANW", "ATS", "ASE", "ASW", "ATE", "MED", "ATW"),
      ordered = TRUE
    )

  return(t1nc_data)
}

standardize = function(t1nc_data) {
  return(
    standardize_catch_types(
      standardize_party_status(
        standardize_stocks(
          t1nc_data
        )
      )
    )
  )
}

prepare_t1nc_executive_summary_table_global = function(t1nc_data, fill = NA) {
  t1nc_data = standardize(t1nc_data)[, TOTAL := sum(Qty_t, na.rm = TRUE), by = .(STOCK_CODE = Stock)]

  summary     = t1nc_data[CatchTypeCode != "DL" & TOTAL > 0, .(CATCH = round(sum(Qty_t, na.rm = TRUE), 2)), keyby = .(YEAR = YearC, STOCK = Stock)]

  summary_tot = t1nc_data[CatchTypeCode != "DL" & TOTAL > 0, .(CATCH = round(sum(Qty_t, na.rm = TRUE), 2)), keyby = .(YEAR = YearC)]
  summary_tot$STOCK = ""

  if(length(unique(summary$STOCK)) > 1)
    summary = rbind(summary, summary_tot)

  result =
    dcast.data.table(
      summary,
      STOCK ~ YEAR,
      fun.aggregate = sum,
      value.var = "CATCH",
      drop = c(TRUE, FALSE),
      fill = fill
    )

  return(result)
}

prepare_t1nc_executive_summary_table_stock  = function(t1nc_data, fill = NA) {
  t1nc_data = standardize(t1nc_data)[, TOTAL := sum(Qty_t, na.rm = TRUE), by = .(CATCH_TYPE, STOCK = Stock)]

  summary = t1nc_data[CatchTypeCode != "DL" & TOTAL > 0, .(CATCH = round(sum(Qty_t, na.rm = TRUE), 2)), keyby = .(YEAR = YearC, CATCH_TYPE, STOCK = Stock)]

  return(
    dcast.data.table(
      summary,
      CATCH_TYPE + STOCK ~ YEAR,
      fun.aggregate = sum,
      value.var = "CATCH",
      drop = c(TRUE, FALSE),
      fill = fill
    )
  )
}

prepare_t1nc_executive_summary_table_gears  = function(t1nc_data, fill = NA) {
  t1nc_data = standardize(t1nc_data)[, TOTAL := sum(Qty_t, na.rm = TRUE), by = .(CATCH_TYPE, STOCK = Stock, GEAR_GROUP = SpcGearGrp)]

  summary = t1nc_data[CatchTypeCode != "DL" & TOTAL > 0, .(CATCH = round(sum(Qty_t, na.rm = TRUE), 2)), keyby = .(YEAR = YearC, CATCH_TYPE, STOCK = Stock, GEAR_GROUP = SpcGearGrp)]

  return(
    dcast.data.table(
      summary,
      CATCH_TYPE + STOCK + GEAR_GROUP ~ YEAR,
      fun.aggregate = sum,
      value.var = "CATCH",
      drop = c(TRUE, FALSE),
      fill = fill
    )
  )
}

prepare_t1nc_executive_summary_table_CPCs   = function(t1nc_data, fill = NA) {
  t1nc_data = standardize(t1nc_data)[, TOTAL := sum(Qty_t, na.rm = TRUE), by = .(CATCH_TYPE, STOCK = Stock, PARTY_STATUS = PartyStatus, FLAG = FlagName)]

  summary = t1nc_data[CatchTypeCode != "DL" & TOTAL > 0, .(CATCH = round(sum(Qty_t, na.rm = TRUE), 2)), keyby = .(YEAR = YearC, STOCK = Stock, CATCH_TYPE, PARTY_STATUS = PartyStatus, FLAG = FlagName)]

  return(
    dcast.data.table(
      summary,
      CATCH_TYPE + STOCK + PARTY_STATUS + FLAG ~ YEAR,
      fun.aggregate = sum,
      value.var = "CATCH",
      drop = c(TRUE, FALSE),
      fill = fill
    )
  )
}

prepare_t1nc_executive_summary_table_all = function(t1nc_data, fill = NA) {
  summary_1 = prepare_t1nc_executive_summary_table_global(t1nc_data, fill)
  summary_1 = data.table(COLUMN_1 = "TOTAL", COLUMN_2 = summary_1$STOCK, COLUMN_3 = NA_character_, COLUMN_4 = NA_character_, summary_1[, 2:ncol(summary_1)])

  summary_2 = prepare_t1nc_executive_summary_table_gears (t1nc_data, fill)
  summary_2 = data.table(COLUMN_1 = summary_2$CATCH_TYPE, COLUMN_2 = summary_2$STOCK, COLUMN_3 = NA_character_, COLUMN_4 = summary_2$GEAR_GROUP, summary_2[, 4:ncol(summary_2)])

  summary_3 = prepare_t1nc_executive_summary_table_CPCs  (t1nc_data, fill)
  summary_3 = data.table(COLUMN_1 = summary_3$CATCH_TYPE, COLUMN_2 = summary_3$STOCK, COLUMN_3 = summary_3$PARTY_STATUS, COLUMN_4 = summary_3$FLAG, summary_3[, 5:ncol(summary_3)])

  summary = rbind(summary_1, summary_2, summary_3)

  return(summary)
}

#' Produces a table of T1 nominal catch data summary **for a single species** as expected in the _global_ section of the SCRS executive summary tables
#'
#' @param t1nc_data T1 nominal catch data **for a single species** as retrieved using the \code{\link{iccat.dev.data::t1nc}} function
#' @param fill the value to use to fill cells with no catch data
#' @return a _flextable_ containing the information to be included in the _global_ section of the SCRS executive summaries
#' @export
t1nc.viz.executive_summary.table.global = function(t1nc_data, fill = NA) {
  summary = prepare_t1nc_executive_summary_table_global(t1nc_data, fill)

  summary = data.table(TYPE = "TOTAL", summary)

  last_row_by_stock = summary[, .(ROW = max(.N)), keyby = .(STOCK)]
  last_row_by_stock[, ROW := cumsum(ROW)]

  for(j in c("STOCK"))
    set(summary, i = which(duplicated(rleid(summary[[j]]))), j = j, value = "")

  return(
    flextable(summary) %>% flextable::set_header_labels(TYPE = "", STOCK = "") %>%
      flextable::merge_v(j = 1) %>%
      #flextable::merge_v(j = 1:2) %>%
      flextable::valign(j = 1:2, part = "body", valign = "top") %>%
      flextable::border(i = last_row_by_stock$ROW, part = "body", border.bottom = fp_border_default(width = 1)) %>%
      flextable::colformat_double(j = 3:ncol(summary),
                                  digits = 0, big.mark = "") %>%
      autofit() %>%
      fix_border_issues()
  )
}

#' Produces a table of T1 nominal catch data summary as expected in the _gear_ section of the SCRS executive summary tables
#'
#' @param t1nc_data T1 nominal catch data as retrieved using the \code{\link{iccat.dev.data::t1nc}} function
#' @param fill the value to use to fill cells with no catch data
#' @return a _flextable_ containing the information to be included in the _gear_ section of the SCRS executive summaries
#' @export
t1nc.viz.executive_summary.table.gears = function(t1nc_data, fill = NA) {
  summary = prepare_t1nc_executive_summary_table_gears(t1nc_data, fill)

  last_row_by_catch_type = summary[, .(ROW = max(.N)), keyby = .(CATCH_TYPE)]
  last_row_by_catch_type[, ROW := cumsum(ROW)]

  last_row_by_catch_type_stock = summary[, .(ROW = max(.N)), keyby = .(CATCH_TYPE, STOCK)]
  last_row_by_catch_type_stock[, ROW := cumsum(ROW)]

  for(j in c("CATCH_TYPE", "STOCK", "GEAR_GROUP"))
    set(summary, i = which(duplicated(rleid(summary[[j]]))), j = j, value = "")

  return(
    flextable(summary) %>%
      flextable::set_header_labels(CATCH_TYPE = "Catch type", STOCK = "Stock", GEAR_GROUP = "Gear") %>%
      #flextable::merge_v(j = 1:3) %>%
      #flextable::valign(j = 1:3, part = "body", valign = "top") %>%
      flextable::border(i = last_row_by_catch_type$ROW,       part = "body", border.bottom = fp_border_default(width = 1)) %>%
      flextable::border(i = last_row_by_catch_type_stock$ROW, part = "body", border.bottom = fp_border_default(width = 1)) %>%
      flextable::colformat_double(j = 4:ncol(summary),
                                  digits = 0, big.mark = "") %>%
      autofit() %>%
      fix_border_issues()
  )
}

#' Produces a table of T1 nominal catch data summary as expected in the _CPC_ section of the SCRS executive summary tables
#'
#' @param t1nc_data T1 nominal catch data as retrieved using the \code{\link{iccat.dev.data::t1nc}} function
#' @param fill the value to use to fill cells with no catch data
#' @return a _flextable_ containing the information to be included in the _CPC_ section of the SCRS executive summaries
#' @export
t1nc.viz.executive_summary.table.CPCs = function(t1nc_data, fill = NA) {
  summary = prepare_t1nc_executive_summary_table_CPCs(t1nc_data, fill)

  last_row_by_catch_type = summary[, .(ROW = max(.N)), keyby = .(CATCH_TYPE)]
  last_row_by_catch_type[, ROW := cumsum(ROW)]

  last_row_by_catch_type_stock = summary[, .(ROW = max(.N)), keyby = .(CATCH_TYPE, STOCK)]
  last_row_by_catch_type_stock[, ROW := cumsum(ROW)]

  last_row_by_catch_type_stock_party_status = summary[, .(ROW = max(.N)), keyby = .(CATCH_TYPE, STOCK, PARTY_STATUS)]
  last_row_by_catch_type_stock_party_status[, ROW := cumsum(ROW)]

  for(j in c("CATCH_TYPE", "STOCK", "PARTY_STATUS", "FLAG"))
    set(summary, i = which(duplicated(rleid(summary[[j]]))), j = j, value = "")

  return(
    flextable(summary) %>%
      flextable::set_header_labels(CATCH_TYPE = "Catch type", STOCK = "Stock", PARTY_STATUS = "Status", FLAG = "Flag") %>%
      #flextable::merge_v(j = 1:4) %>%
      #flextable::valign(j = 1:4, part = "body", valign = "top") %>%
      flextable::border(i = last_row_by_catch_type$ROW,                    part = "body", border.bottom = fp_border_default(width = 1)) %>%
      flextable::border(i = last_row_by_catch_type_stock$ROW,              part = "body", border.bottom = fp_border_default(width = 1)) %>%
      flextable::border(i = last_row_by_catch_type_stock_party_status$ROW, part = "body", border.bottom = fp_border_default(width = 1)) %>%
      flextable::colformat_double(j = 5:ncol(summary),
                                  digits = 0, big.mark = "") %>%
      autofit() %>%
      fix_border_issues()
  )
}

#' Produces the global table of T1 nominal catch data summary as expected in the SCRS executive summary tables
#'
#' @param t1nc_data T1 nominal catch data as retrieved using the \code{\link{iccat.dev.data::t1nc}} function
#' @param fill the value to use to fill cells with no catch data
#' @return a _flextable_ containing the information to be included in the SCRS executive summaries
#' @export
t1nc.viz.executive_summary.table.all = function(t1nc_data, fill = NA) {
  species_codes = sort(unique(t1nc_data$Species))

  summary = NULL

  for(species in species_codes) {
    t1nc_data_s = t1nc_data[Species == species]

    summary_1 = prepare_t1nc_executive_summary_table_global(t1nc_data_s, fill)
    summary_1 = data.table(SPECIES = species, TYPE = "GLOBAL", COLUMN_1 = "TOTAL", COLUMN_2 = summary_1$STOCK, COLUMN_3 = NA_character_, COLUMN_4 = NA_character_, summary_1[, 2:ncol(summary_1)])

    summary_2 = prepare_t1nc_executive_summary_table_gears (t1nc_data_s, fill)
    summary_2 = data.table(SPECIES = species, TYPE = "GEARS", COLUMN_1 = summary_2$CATCH_TYPE, COLUMN_2 = summary_2$STOCK, COLUMN_3 = NA_character_, COLUMN_4 = summary_2$GEAR_GROUP, summary_2[, 4:ncol(summary_2)])

    summary_3 = prepare_t1nc_executive_summary_table_CPCs  (t1nc_data_s, fill)
    summary_3 = data.table(SPECIES = species, TYPE = "CPCS", COLUMN_1 = summary_3$CATCH_TYPE, COLUMN_2 = summary_3$STOCK, COLUMN_3 = summary_3$PARTY_STATUS, COLUMN_4 = summary_3$FLAG, summary_3[, 5:ncol(summary_3)])

    if(is.null(summary)) {
      summary = rbind(summary_1, summary_2, summary_3)
    } else {
      summary = rbind(summary, rbind(summary_1, summary_2, summary_3))
    }
  }

  summary$TYPE =
    factor(
      summary$TYPE,
      levels = c("GLOBAL", "GEARS", "CPCS"),
      labels = c("GLOBAL", "GEARS", "CPCS"),
      ordered = TRUE
    )

  summary$COLUMN_1 =
    factor(
      summary$COLUMN_1,
      levels = c("TOTAL", "Landings", "Landings(FP)", "Discards"),
      labels = c("TOTAL", "Landings", "Landings(FP)", "Discards"),
      ordered = TRUE
    )

  last_row_by_species = summary[, .(ROW = max(.N)), keyby = .(SPECIES)][order(SPECIES, ROW)]
  last_row_by_species[, ROW := cumsum(ROW)]

  last_row_by_species_ds = summary[, .(ROW = max(.N)), keyby = .(SPECIES, TYPE)][order(SPECIES, TYPE, ROW)]
  last_row_by_species_ds[, ROW := cumsum(ROW)]

  last_row_by_species_ds_column_1 = summary[, .(ROW = max(.N)), keyby = .(SPECIES, TYPE, COLUMN_1)][order(SPECIES, TYPE, COLUMN_1, ROW)]
  last_row_by_species_ds_column_1[, ROW := cumsum(ROW)]

  last_row_by_species_ds_column_1_2 = summary[, .(ROW = max(.N)), keyby = .(SPECIES, TYPE, COLUMN_1, COLUMN_2)][order(SPECIES, TYPE, COLUMN_1, COLUMN_2, ROW)]
  last_row_by_species_ds_column_1_2[, ROW := cumsum(ROW)]

  last_row_by_species_ds_column_1_2_3 = summary[, .(ROW = max(.N)), keyby = .(SPECIES, TYPE, COLUMN_1, COLUMN_2, COLUMN_3)][order(SPECIES, TYPE, COLUMN_1, COLUMN_2, COLUMN_3, ROW)]
  last_row_by_species_ds_column_1_2_3[, ROW := cumsum(ROW)]

  last_row_by_species_ds_column_1_2_3_4 = summary[, .(ROW = max(.N)), keyby = .(SPECIES, TYPE, COLUMN_1, COLUMN_2, COLUMN_3, COLUMN_4)][order(SPECIES, TYPE, COLUMN_1, COLUMN_2, COLUMN_3, COLUMN_4, ROW)]
  last_row_by_species_ds_column_1_2_3_4[, ROW := cumsum(ROW)]

  for(j in c("SPECIES", "COLUMN_1", "COLUMN_2", "COLUMN_3", "COLUMN_4"))
    set(summary, i = which(duplicated(rleid(summary[[j]]))), j = j, value = "")

  summary$TYPE = NULL

  num_species = length(species_codes)

  SP_COL                            = 1
  DS_COL                            = 2
  DS_COLUMN_1_COL                   = 2
  DS_COLUMN_1_COLUMN_2_COL          = 3
  DS_COLUMN_1_COLUMN_2_COLUMN_3_COL = 4

  DATA_COL = 6

  if(num_species == 1) {
    summary$SPECIES = NULL

    DS_COLUMN_1_COL                   = DS_COLUMN_1_COL - 1
    DS_COLUMN_1_COLUMN_2_COL          = DS_COLUMN_1_COLUMN_2_COL - 1
    DS_COLUMN_1_COLUMN_2_COLUMN_3_COL = DS_COLUMN_1_COLUMN_2_COLUMN_3_COL - 1

    DATA_COL = 5
  }

  result =
    flextable(summary) %>%
      flextable::bold(i = 1, part = "header") %>%
      flextable::border(i = last_row_by_species_ds_column_1_2_3$ROW,   j = DS_COLUMN_1_COLUMN_2_COLUMN_3_COL:ncol(summary), part = "body", border.bottom = fp_border_default(width = 1)) %>%
      flextable::border(i = last_row_by_species_ds_column_1_2$ROW,     j = DS_COLUMN_1_COLUMN_2_COL:ncol(summary),          part = "body", border.bottom = fp_border_default(width = 1)) %>%
      flextable::border(i = last_row_by_species_ds_column_1$ROW,       j = DS_COLUMN_1_COL:ncol(summary),                   part = "body", border.bottom = fp_border_default(width = 1)) %>%
      flextable::border(i = last_row_by_species_ds$ROW,                j = DS_COLUMN_1_COL:ncol(summary),                   part = "body", border.bottom = fp_border_default(width = 2))

  if(num_species > 1)
    result = result %>%
      flextable::border(i = last_row_by_species$ROW,                   j = SP_COL:ncol(summary), part = "body", border.bottom = fp_border_default(width = 3))

  result = result %>%
      flextable::border(i = nrow(summary),                             j = 1:ncol(summary), part = "body", border.bottom = fp_border_default(width = 2)) %>%
      flextable::bold(bold = summary == 0, part = "body") %>%
      flextable::colformat_double(j = DATA_COL:ncol(summary),
                                  digits = 0, big.mark = "") %>%
      flextable::valign(j = 1:(DS_COLUMN_1_COLUMN_2_COLUMN_3_COL + 1), valign = "top") %>%
      autofit() %>%
      fix_border_issues() %>%
      set_header_labels(SPECIES = "", COLUMN_1 = "", COLUMN_2 = "", COLUMN_3 = "", COLUMN_4 = "")

  return(
    result
  )
}

#' Produces a standalone Excel file containing the global table of T1 nominal catch data summary as expected in the SCRS executive summary tables
#' If the original data includes more than one species, the resulting Excel workbook will have a separate tab for each of them.
#'
#' @param t1nc_data T1 nominal catch data as retrieved using the \code{\link{iccat.dev.data::t1nc}} function
#' @param output_file the output Excel file name
#' @param version to specify the table version in accordance with the SCRS executive summary tables template
#' @param fill the value to use to fill cells with no catch data
#' @param legacy_style whether or not applying the 'legacy' table output style
#' @param show_grids whether or not showing cell grids in the Excel spreadsheet
#' @return nothing, as the Excel file is simply stored at the provided path
#' @export
t1nc.viz.executive_summary.table.all.xlsx = function(t1nc_data, output_file, version = 0, fill = NA, legacy_style = FALSE, show_grids = FALSE) {
  if(legacy_style)
    log_warn("Applying legacy style to output table...")

  species_codes = sort(unique(t1nc_data$Species))

  wb = openxlsx2::wb_workbook()

  wb$add_dxfs_style(name = "zeroValued", text_bold = TRUE)

  for(species in species_codes) {
    species_data = copy(iccat.pub.data::REF_SPECIES[CODE == species])

    t1nc_table = prepare_t1nc_executive_summary_table_all(t1nc_data[Species == species], fill = ifelse(legacy_style, NA, fill))

    if(legacy_style) {
      t1nc_table[, 5:(ncol(t1nc_table) - 1)] = replace(t1nc_table[, 5:(ncol(t1nc_table) - 1)], is.na(t1nc_table[, 5:(ncol(t1nc_table) - 1)]), 0)
    }

    t1nc_table_global = prepare_t1nc_executive_summary_table_global(t1nc_data[Species == species], fill)
    t1nc_table_gears  = prepare_t1nc_executive_summary_table_gears (t1nc_data[Species == species], fill)
    t1nc_table_CPCs   = prepare_t1nc_executive_summary_table_CPCs  (t1nc_data[Species == species], fill)

    for(j in c("COLUMN_1", "COLUMN_2", "COLUMN_3", "COLUMN_4"))
      set(t1nc_table, i = which(duplicated(rleid(t1nc_table[[j]]))), j = j, value = "")

    LETTERS_SPACE = append(" ", letters)

    ALL_COLUMNS = CJ(LETTERS_SPACE, letters)
    ALL_COLUMNS = toupper(paste0(str_trim(ALL_COLUMNS[[1]]), ALL_COLUMNS[[2]]))

    LAST_COL  = ALL_COLUMNS[ncol(t1nc_table)]
    FIRST_COL = "D"

    FIRST_ROW = 6
    LAST_ROW  = FIRST_ROW + nrow(t1nc_table)

    ws_name = species

    wb$add_worksheet(sheet = ws_name, grid_lines = show_grids)
    wb$set_active_sheet(ws_name)

    description_en =
      paste0(
        species,
        "-Table 1. Estimated catches (t) of ",
        species_data$NAME_EN,
        " (",
        species_data$SCIENTIFIC_NAME,
        ") by area, gear, and flag (v", version, ", ",
        format(Sys.Date(), "%Y-%m-%d"),
        ")"
      )

    description_es =
      paste0(
        species,
        "-Tabla 1. Capturas estimadas (t) de ",
        species_data$NAME_ES,
        " (",
        species_data$SCIENTIFIC_NAME,
        ") por area, arte y bandera (v", version, ", ",
        format(Sys.Date(), "%Y-%m-%d"),
        ")"
      )

    description_fr =
      paste0(
        species,
        "-Tableau 1. Prises estimées (t) de ",
        species_data$NAME_FR,
        " (",
        species_data$SCIENTIFIC_NAME,
        ") par zone, engin et pavillon (v", version, ", ",
        format(Sys.Date(), "%Y-%m-%d"),
        ")"
      )

    wb$add_data(x = description_en, start_col = 1, start_row = 1)
    wb$add_data(x = description_es, start_col = 1, start_row = 2)
    wb$add_data(x = description_fr, start_col = 1, start_row = 3)

    wb$add_data(x = t(as.integer(colnames(t1nc_table)[5:ncol(t1nc_table)])), start_col = 5, start_row = 5, col_names = FALSE)
    wb$add_data(x = t1nc_table, start_col = 1, start_row = 6, na.strings = "", col_names = FALSE)

    wb$set_col_widths(cols = c(1, 4), widths = c(13, 25))

    # Formatting header (years)
    wb$add_font(dims = "A1:A3", bold = "single")
    wb$add_font(dims = paste0(FIRST_COL, FIRST_ROW - 1, ":", LAST_COL, FIRST_ROW - 1), bold = "single")

    wb$add_border(dims = paste0("A", FIRST_ROW - 1, ":", LAST_COL, FIRST_ROW - 1),
                  top_border = "thick", bottom_border = "thick",
                  left_border = "", right_border = "")

    # Formatting table
    for(r in which(t1nc_table$COLUMN_3 != ""))
      wb$add_border(dims = paste0("C", FIRST_ROW - 1 + r, ":", LAST_COL, FIRST_ROW - 1 + r),
                    top_border  = "thin", bottom_border = "none",
                    left_border = "none", right_border  = "none")

    for(r in which(t1nc_table$COLUMN_2 != ""))
      wb$add_border(dims = paste0("B", FIRST_ROW - 1 + r, ":", LAST_COL, FIRST_ROW - 1 + r),
                    top_border  = "thin", bottom_border = "none",
                    left_border = "none", right_border  = "none")

    for(r in which(t1nc_table$COLUMN_1 != ""))
      wb$add_border(dims = paste0("A", FIRST_ROW - 1 + r, ":", LAST_COL, FIRST_ROW - 1 + r),
                    top_border  = "thin", bottom_border = "none",
                    left_border = "none", right_border  = "none")

    wb$add_border(dims = paste0("A", FIRST_ROW + nrow(t1nc_table_global),                          ":", LAST_COL, FIRST_ROW + nrow(t1nc_table_global)),
                  top_border  = "thick", bottom_border = "none",
                  left_border = "none",  right_border  = "none")

    wb$add_border(dims = paste0("A", FIRST_ROW + nrow(t1nc_table_global) + nrow(t1nc_table_gears), ":", LAST_COL, FIRST_ROW + nrow(t1nc_table_global) + nrow(t1nc_table_gears)),
                  top_border  = "thick", bottom_border = "none",
                  left_border = "none", right_border   = "none")

    wb$add_border(dims = paste0("A", FIRST_ROW + nrow(t1nc_table_global) + nrow(t1nc_table_gears) + nrow(t1nc_table_CPCs), ":", LAST_COL, FIRST_ROW + nrow(t1nc_table_global) + nrow(t1nc_table_gears) + nrow(t1nc_table_CPCs)),
                  top_border  = "thick", bottom_border = "none",
                  left_border = "none", right_border   = "none")

    value_dims = paste0("E", FIRST_ROW, ":", LAST_COL, FIRST_ROW + nrow(t1nc_table_global) + nrow(t1nc_table_gears) + nrow(t1nc_table_CPCs) - 1)

    wb$add_numfmt(dims = value_dims, numfmt = "0")

    if(legacy_style) { # Applies the "zeroValued" conditional formatting to the last column and CPC rows only
      FIRST_ROW = FIRST_ROW + nrow(t1nc_table_global) + nrow(t1nc_table_gears)

      value_dims = paste0(LAST_COL, FIRST_ROW, ":", LAST_COL, FIRST_ROW + nrow(t1nc_table_global) + nrow(t1nc_table_gears) + nrow(t1nc_table_CPCs) - 1)
    }

    wb$add_conditional_formatting(dims = value_dims, rule = "=0", style = "zeroValued")
  }

  wb$set_active_sheet(species_codes[1])

  wb$save(output_file)
}

#' Produces a standalone Excel file containing the global table of T1 nominal catch data summary for a user-defined species group, as expected in the SCRS executive summary tables.
#' The resulting Excel workbook will always have a single tab (labelled with the provided \code{species_group_code}) and the distinct species code will appear with their data directly in the table.
#'
#' @param t1nc_data T1 nominal catch data as retrieved using the \code{\link{iccat.dev.data::t1nc}} function
#' @param species_group_code a species group code
#' @param species_group_descriptions a list containing the species group scientific name as well as its name in the three official ICCAT languages
#' @param output_file the output Excel file name
#' @param version to specify the table version in accordance with the SCRS executive summary tables template
#' @param fill the value to use to fill cells with no catch data
#' @param legacy_style whether or not applying the 'legacy' table output style
#' @param show_grids whether or not showing cell grids in the Excel spreadsheet
#' @return nothing, as the Excel file is simply stored at the provided path
#' @export
t1nc.viz.executive_summary.table.all.species_group.xlsx = function(filtered_t1nc_data, species_group_code, species_group_descriptions, output_file, version = 0, fill = NA, legacy_style = FALSE, show_grids = FALSE) {
  if(legacy_style)
    log_warn("Applying legacy style to output table...")

  wb = openxlsx2::wb_workbook()

  wb$add_dxfs_style(name = "zeroValued", text_bold = TRUE)

  species_codes = sort(unique(filtered_t1nc_data$Species))

  summary = NULL

  for(species in species_codes) {
    t1nc_data_s = filtered_t1nc_data[Species == species]

    summary_1 = prepare_t1nc_executive_summary_table_global(t1nc_data_s, fill)
    summary_1 = data.table(SPECIES = species, TYPE = "GLOBAL", COLUMN_1 = "TOTAL", COLUMN_2 = summary_1$STOCK, COLUMN_3 = NA_character_, COLUMN_4 = NA_character_, summary_1[, 2:ncol(summary_1)])

    summary_2 = prepare_t1nc_executive_summary_table_gears (t1nc_data_s, fill)
    summary_2 = data.table(SPECIES = species, TYPE = "GEARS", COLUMN_1 = summary_2$CATCH_TYPE, COLUMN_2 = summary_2$STOCK, COLUMN_3 = NA_character_, COLUMN_4 = summary_2$GEAR_GROUP, summary_2[, 4:ncol(summary_2)])

    summary_3 = prepare_t1nc_executive_summary_table_CPCs  (t1nc_data_s, fill)
    summary_3 = data.table(SPECIES = species, TYPE = "CPCS", COLUMN_1 = summary_3$CATCH_TYPE, COLUMN_2 = summary_3$STOCK, COLUMN_3 = summary_3$PARTY_STATUS, COLUMN_4 = summary_3$FLAG, summary_3[, 5:ncol(summary_3)])

    if(is.null(summary)) {
      summary = rbind(summary_1, summary_2, summary_3)
    } else {
      summary = rbind(summary, rbind(summary_1, summary_2, summary_3))
    }
  }

  summary$TYPE =
    factor(
      summary$TYPE,
      levels = c("GLOBAL", "GEARS", "CPCS"),
      labels = c("GLOBAL", "GEARS", "CPCS"),
      ordered = TRUE
    )

  summary$COLUMN_1 =
    factor(
      summary$COLUMN_1,
      levels = c("TOTAL", "Landings", "Landings(FP)", "Discards"),
      labels = c("TOTAL", "Landings", "Landings(FP)", "Discards"),
      ordered = TRUE
    )

  for(j in c("SPECIES", "COLUMN_1", "COLUMN_2", "COLUMN_3", "COLUMN_4"))
    set(summary, i = which(duplicated(rleid(summary[[j]]))), j = j, value = "")

  summary$TYPE = NULL

  if(legacy_style) {
    if(legacy_style) {
      summary[, 6:(ncol(summary) - 1)] = replace(summary[, 6:(ncol(summary) - 1)], is.na(summary[, 6:(ncol(summary) - 1)]), 0)
    }
  }

  LETTERS_SPACE = append(" ", letters)

  ALL_COLUMNS = CJ(LETTERS_SPACE, letters)
  ALL_COLUMNS = toupper(paste0(str_trim(ALL_COLUMNS[[1]]), ALL_COLUMNS[[2]]))

  LAST_COL  = ALL_COLUMNS[ncol(summary)]
  FIRST_COL = "E"

  FIRST_ROW = 6
  LAST_ROW  = FIRST_ROW + nrow(summary)

  ws_name = species_group_code

  wb$add_worksheet(sheet = species_group_code, grid_lines = show_grids)

  wb$set_active_sheet(species_group_code)

  description_en =
    paste0(
      species_group_code,
      "-Table 1. Estimated catches (t) of ",
      species_group_descriptions$NAME_EN,
      ifelse(!is.null(species_group_descriptions$SCIENTIFIC_NAME),
                      paste0(" (", species_group_descriptions$SCIENTIFIC_NAME, ") "), " "),
      "by area, gear, and flag (v", version, " ",
      format(Sys.Date(), "%Y-%m-%d"),
      ")"
    )

  description_es =
    paste0(
      species_group_code,
      "-Tabla 1. Capturas estimadas (t) de ",
      species_group_descriptions$NAME_ES,
      ifelse(!is.null(species_group_descriptions$SCIENTIFIC_NAME),
             paste0(" (", species_group_descriptions$SCIENTIFIC_NAME, ") "), " "),
      "por area, arte y bandera (v", version, " ",
      format(Sys.Date(), "%Y-%m-%d"),
      ")"
    )

  description_fr =
    paste0(
      species_group_code,
      "-Tableau 1. Prises estimées (t) de ",
      species_group_descriptions$NAME_FR,
      ifelse(!is.null(species_group_descriptions$SCIENTIFIC_NAME),
             paste0(" (", species_group_descriptions$SCIENTIFIC_NAME, ") "), " "),
      "par zone, engin et pavillon (v", version, " ",
      format(Sys.Date(), "%Y-%m-%d"),
      ")"
    )

  wb$add_data(x = description_en, start_col = 1, start_row = 1)
  wb$add_data(x = description_es, start_col = 1, start_row = 2)
  wb$add_data(x = description_fr, start_col = 1, start_row = 3)

  wb$add_data(x = t(as.integer(colnames(summary)[6:ncol(summary)])), start_col = 6, start_row = 5, col_names = FALSE)
  wb$add_data(x = summary, start_col = 1, start_row = 6, na.strings = "", col_names = FALSE)

  wb$set_col_widths(cols = c(2, 5), widths = c(13, 25))

  # Formatting header (years)
  wb$add_font(dims = "A1:A3", bold = "single")
  wb$add_font(dims = paste0(FIRST_COL, FIRST_ROW - 1, ":", LAST_COL, FIRST_ROW - 1), bold = "single")

  wb$add_border(dims = paste0("A", FIRST_ROW - 1, ":", LAST_COL, FIRST_ROW - 1),
                top_border = "thick", bottom_border = "thick",
                left_border = "", right_border = "")

  # Formatting table
  for(r in which(summary$COLUMN_3 != ""))
    wb$add_border(dims = paste0("D", FIRST_ROW - 1 + r, ":", LAST_COL, FIRST_ROW - 1 + r),
                  top_border  = "thin", bottom_border = "none",
                  left_border = "none", right_border  = "none")

  for(r in which(summary$COLUMN_2 != ""))
    wb$add_border(dims = paste0("C", FIRST_ROW - 1 + r, ":", LAST_COL, FIRST_ROW - 1 + r),
                  top_border  = "thin", bottom_border = "none",
                  left_border = "none", right_border  = "none")

  for(r in which(summary$COLUMN_1 != ""))
    wb$add_border(dims = paste0("B", FIRST_ROW - 1 + r, ":", LAST_COL, FIRST_ROW - 1 + r),
                  top_border  = "thin", bottom_border = "none",
                  left_border = "none", right_border  = "none")

  for(r in which(summary$SPECIES != ""))
    wb$add_border(dims = paste0("A", FIRST_ROW - 1 + r, ":", LAST_COL, FIRST_ROW - 1 + r),
                  top_border  = "thick", bottom_border = "none",
                  left_border = "none", right_border  = "none")

  value_dims = paste0("F", FIRST_ROW, ":", LAST_COL, FIRST_ROW + nrow(summary) - 1)

  wb$add_numfmt(dims = value_dims, numfmt = "0")

  if(legacy_style) {
    value_dims = paste0(LAST_COL, FIRST_ROW, ":", LAST_COL, FIRST_ROW + nrow(summary) - 1)
  }

  wb$add_conditional_formatting(dims = value_dims, rule = "=0", style = "zeroValued")

  wb$set_active_sheet(species_group_code)

  wb$save(output_file)
}
