#' TBD
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

#' TBD
#'
#' @return TBD
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

#' TBD
#'
#' @param t1nc_data TBD
#' @param by_species TBD
#' @param by_stock TBD
#' @param by_gear TBD
#' @param by_catch_type TBD
#' @param rank TBD
#' @param max_cumulative_percentage TBD
#' @param rel_diff_limits TBD
#' @param sensitivity TBD
#' @param show_catches_gradient TBD
#' @param colorize_gears TBD
#' @return TBD
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

  if(!is.na(max_cumulative_percentage) & max_cumulative_percentage < 0 | max_cumulative_percentage > 1) stop("The maximum cumulative percentage should be set to a value between 0 and 1 (both included)")

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
    valign(j = "FLAG_CODE",                                       part = "body",   valign = "top") %>%
    valign(j =  to_merge,                                         part = "body",   valign = "top") %>%
    valign(j = (grouped_columns + 1 + delta):ncol(T1NC_proc_m_w), part = "all",    valign = "center") %>%
    align(part = "header", align = "center") %>%
    align(part = "header", align = "left", j = "FLAG_CODE")

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
