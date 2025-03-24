COLOR_TAB        = "#F79646"

COLOR_NO_T2      = "#FF0000"
COLOR_LIMITED_T2 = "#FFFF00"
COLOR_PARTIAL_T2 = "#92D050"
COLOR_ALL_T2     = "#00B050"
COLOR_NO_T1      = "#00B0F0"
COLOR_NO_DATA    = "#CCCCCC"
COLOR_DEFAULT    = "#FFFFFF"

COLOR_UNCL_GEAR  = "#FF0000"

COLOR_SCORE      = "#FDE9D9"

GRADIENT_CATCH            = c("#F8696B", "#FFEB84", "#63BE7B")
GRADIENT_CATCH_CUMULATIVE = c("#63BE7B", "#FFFFFF", "#F8696B")

#' Internal function computing the background color matrix for the catalogue
#'
#' @param data_matrix the catalogue data matrix
#' @return a table with the specifications of the background color of each data cell in the catalogue
catalogue.viz.table.bg_matrix = function(data_matrix) {
  bg_matrix =
    ifelse(is.na(data_matrix) | data_matrix == "-", COLOR_NO_DATA,
           ifelse(data_matrix == "-1", COLOR_NO_T2,
                  ifelse(data_matrix == "a" | data_matrix == "b" | data_matrix == "c" | data_matrix == "bc", COLOR_LIMITED_T2,
                         ifelse(data_matrix == "ab" | data_matrix == "ac", COLOR_PARTIAL_T2,
                                ifelse(data_matrix == "abc", COLOR_ALL_T2,
                                       COLOR_DEFAULT
                                )
                         )
                  )
           )
    )

  return(
    bg_matrix
  )
}

#' Internal function computing the foreground color matrix for the catalogue
#'
#' @param data_matrix the catalogue data matrix
#' @return a table with the specifications of the foreground color of each data cell in the catalogue
catalogue.viz.table.fg_matrix = function(data_matrix) {
  fg_matrix =
    ifelse(is.na(data_matrix) | data_matrix == "abc", "black", "black") # Originally "white", "black", when the darkest green was "darkgreen"

  return(fg_matrix)
}

#' Produces a _flextable_ corresponding to the legend of the catalogue cell content
#'
#' @return a _flextable_ corresponding to the legend of the catalogue cell content
#' @export
catalogue.viz.table.legend = function() {
  legend = data.table(
    Character = c("a", "b", "c"),
    Represents = c("T2CE", "T2SZ", "T2CS (*)")
  )

  return(
    flextable::flextable(legend) %>%
    bg(part = "all",    bg = "white") %>%
    bg(part = "header", bg = "gray") %>%

    bold(part = "header") %>%

    border_inner(part = "all", border = fp_border(width = .5)) %>%
    border(part = "body", i = seq(2, nrow(legend), 2), border.bottom = fp_border(width = 1)) %>%
    border(part = "all",  j = 1, border.left  = fp_border(width = 1)) %>%
    border(part = "all",  j = 2, border.right = fp_border(width = 1)) %>%
    align(part = "all", j = 1, align = "center") %>%
    autofit()
  )
}

#' Produces a _flextable_ corresponding to the color legend used for the catalogue cell content
#'
#' @return a _flextable_ corresponding to the lcolor legend used for the catalogue cell content
#' @export
catalogue.viz.table.legend.colours = function() {
  colour_scheme = data.table(
    `Concatenated string` = c("-1", "a", "b", "c", "bc", "ab", "ac", "abc"),
    Represents = c("No T2 data",
                   "T2CE only",
                   "T2SZ only",
                   "T2CS only",
                   "T2SZ + T2CS",
                   "T2CE + T2SZ",
                   "T2CE + T2CS",
                   "All")
  )

  return(
    flextable::flextable(colour_scheme) %>%
    bg(part = "all",    bg = "white") %>%
    bg(part = "header", bg = "gray") %>%

    bold(part = "header") %>%

    border_inner(part = "all", border = fp_border(width = .5)) %>%

    border(part = "body", i = seq(2, nrow(colour_scheme), 2), border.bottom = fp_border(width = 1)) %>%
    border(part = "all",  j = 2, border.right = fp_border(width = 1)) %>%
    border(part = "all",  j = 1, border.left  = fp_border(width = 1)) %>%

    align(part = "all", align = "center") %>%

    bg   (part = "body", bg    = catalogue.viz.table.bg_matrix(colour_scheme)) %>%
    color(part = "body", color = catalogue.viz.table.fg_matrix(colour_scheme)) %>%

    autofit()
  )
}

#' Produces a _flextable_ containing a formatted SCRS catalogue
#'
#' @param catalogue_data the SCRS catalogue data as produced using \code{\link{iccat.pub.data::catalogue.compile}}, with T1 nominal catch data and base catalogue data extracted from the ICCAT databases through the **iccat.dev.data** and **iccat.pub.data** libraries
#' @param show_catches_gradient to enable / disable a color gradients on the cumulative catch column
#' @param remove_species if the species shall not be included in the stratification
#' @param remove_stock if the stock shall not be included in the stratification
#' @param truncate_years if year values in the table headers should be truncated to the last two digits
#' @param flag_separator_width TBD
#' @param default_font_size the font size applied by default
#' @param values_font_size the font size used for value cells only
#' @param default_h_padding the horizontal padding applied to all cells by default
#' @param values_h_padding the horizontal padding appliet to value cells only
#' @return a _flextable_ containing a SCRS catalogue formatted according to the provided criteria
#' @export
catalogue.viz.table = function(catalogue_data, show_catches_gradient = TRUE, remove_species = FALSE, remove_stock = FALSE, truncate_years = TRUE,
                               flag_separator_width = 1, default_font_size = 7, values_font_size = 7, default_h_padding = 5, values_h_padding = 2) {
  DEBUG("Building catalog table...")

  first_year_column = 11

  if(truncate_years) {
    column_names = colnames(catalogue_data)
    colnames(catalogue_data)[first_year_column:length(column_names)] = str_sub(colnames(catalogue_data)[first_year_column:length(column_names)], 3, 4)
  }

  catalogue_data[is.na(catalogue_data)] = "-"

  start = Sys.time()

  if(remove_species) {
    catalogue_data$Species = NULL
    first_year_column = first_year_column - 1
  }

  if(remove_stock) {
    catalogue_data$Stock = NULL
    first_year_column = first_year_column - 1
  }

  bg_matrix = catalogue.viz.table.bg_matrix(catalogue_data[, first_year_column:ncol(catalogue_data)])
  fg_matrix = catalogue.viz.table.fg_matrix(catalogue_data[, first_year_column:ncol(catalogue_data)])

  #set_flextable_defaults(extra_css = "th p span, td p span { white-space: nowrap !important; }")

  FT = flextable(catalogue_data) %>%
    set_table_properties(opts_html = list(extra_css = "th p span, td p span { white-space: nowrap !important; }")) %>%
    set_header_labels(values = list(FlagName = "Flag name",
                                    GearGrp = "Gear",
                                    FisheryRank = "Rank",
                                    TotCatches = "Total (t)",
                                    Perc = "%",
                                    PercCum = "% (cum.)",
                                    DSet = "DS")) %>%

    flextable::fontsize(size = default_font_size, part = "all") %>%
    flextable::fontsize(size = values_font_size,  i = seq(1, nrow(catalogue_data), 2), j = first_year_column:ncol(catalogue_data), part = "body") %>%

    flextable::padding(part = "all", padding.top = 0, padding.bottom = 0, padding.left = default_h_padding, padding.right = default_h_padding) %>%
    flextable::padding(part = "all", j = first_year_column:ncol(catalogue_data), padding.left = values_h_padding, padding.right = values_h_padding) %>%

    flextable::bg   (part = "all",    bg = "white") %>%
    flextable::bg   (part = "header", bg = "gray") %>%

    flextable::bold(part = "header") %>%

    flextable::align(j = 1:ncol(catalogue_data), align = "center", part = "all")  %>%
    flextable::align(j = c("FlagName"),          align = "left",   part = "all")  %>%
    flextable::align(j = c("TotCatches",
                           "Perc",
                           "PercCum"),           align = "right",  part = "body") %>%

    flextable::align(j = first_year_column:ncol(catalogue_data),  align = "right",  part = "body") %>%

    flextable::valign(valign = "center", part = "all") %>%

    flextable::merge_v(c("FisheryRank",
                         "Species",
                         "Stock",
                         "FlagName",
                         "Status",
                         "GearGrp",
                         "TotCatches",
                         "Perc",
                         "PercCum"), part = "body", combine = TRUE) %>%

    flextable::border(part = "all",                                         border        = fp_border(width = .5)) %>%
    flextable::border(part = "header",                                      border.top    = fp_border(width = flag_separator_width), border.bottom = fp_border(width = flag_separator_width)) %>%
    flextable::border(part = "body",   i = seq(2, nrow(catalogue_data), 2), border.bottom = fp_border(width = flag_separator_width)) %>%

    flextable::bg(part = "body", j = first_year_column:ncol(catalogue_data), bg = bg_matrix)

  if(show_catches_gradient) {
    bg_matrix_catch = catalogue_data[, c("Perc", "PercCum")]
    bg_matrix_catch$Perc    = rgb(.6, .6, 1, bg_matrix_catch$Perc    / max(bg_matrix_catch$Perc))
    bg_matrix_catch$PercCum = rgb(.3, 1, .3, bg_matrix_catch$PercCum / max(bg_matrix_catch$PercCum))

    FT = FT%>%
      flextable::bg   (part = "body", j = "Perc",    bg = bg_matrix_catch$Perc) %>%
      flextable::bg   (part = "body", j = "PercCum", bg = bg_matrix_catch$PercCum)
  }

  FT = FT %>% flextable::colformat_num(j = c("Perc", "PercCum"), digits = 2)

  FT = FT %>%

    flextable::color(part = "body", j = first_year_column:ncol(catalogue_data), color = fg_matrix) %>%
    flextable::autofit() %>%
    flextable::fix_border_issues()

  end = Sys.time()

  DEBUG(paste0("Finished building catalog table in ", end - start))

  return(FT)
}

#' Appends an Excel version of the SCRS catalogue to an existing Excel workbook
#'
#' @param filtered_catalogue_data filtered SCRS catalogue data (for a single species) as produced using \code{\link{iccat.pub.data::catalogue.compile}}, with T1 nominal catch data and base catalogue data extracted from the ICCAT databases through the **iccat.dev.data library**
#' @param workbook an Excel workbook reference (see \code{\link{}}) as the container for one or more SCRS catalogue worksheets added through this function
#' @param pretty_print_catches to _prettily_ print catch values
#' @param catch_round_digits the number of digits catch values should be rounded to
#' @param perc_round_digits the number of digits percentage values should be rounded to
#' @param cutoff_percentage the maximum value of the cumulative catch percentage used to highlight the most relevant strata
#' @param max_percentage to indicate the maximum cumulative catch percentage for strata to be included in the output
#' @param score the data score for the information presented in the catalogue (calculated externally)
#' @param table_number the table number, in accordance with the SCRS catalogue template structure
#' @param table_label the table label, in accordance with the SCRS catalogue template structure
#' @param stock the stock code for the provided catalogue data
#' @param sheet the name of the worksheet where the catalogue data will be written
#' @param show_grids whether or not showing cell grids in the Excel spreadsheet
#' @return nothing, as the update will happen directly in the provided Excel workbook
#' @export
catalogue.viz.table.xlsx.append = function(filtered_catalogue_data, workbook, pretty_print_catches = FALSE, catch_round_digits = 0, perc_round_digits = 2, cutoff_percentage = 95, max_percentage = 100, score, table_number, table_label, stock, sheet = NA, show_grids = FALSE) {
  DEBUG("Appending catalog data...")

  if(max_percentage < cutoff_percentage) stop(paste0("The maximum percentage (", max_percentage, "%) should be higher than the cutoff percentage (", cutoff_percentage, "%)"))

  workbook$set_base_font(font_name = "Calibri", font_size = 9)

  workbook$add_worksheet(ifelse(is.na(sheet), stock, sheet), tab_color = wb_color(COLOR_TAB),
                         zoom = 90, orientation = "landscape",
                         grid_lines = show_grids)

  workbook$set_active_sheet(stock)

  actual_max_percentage = min(filtered_catalogue_data[PercCum >= max_percentage]$PercCum)

  filtered_catalogue_data = copy(filtered_catalogue_data[PercCum <= actual_max_percentage])

  # Identifies the row of the first record exceeding the provided cutoff percentage (95% by default)
  row_cutoff = min(which(filtered_catalogue_data$PercCum >= cutoff_percentage)) + 2 # To move to the next strata

  # Calculates the quantiles for the percentage and cumulative percentage values
  perc_quantiles     = quantile(filtered_catalogue_data$Perc)
  perc_cum_quantiles = quantile(filtered_catalogue_data$PercCum)

  # See also https://cran.r-project.org/web/packages/openxlsx2/openxlsx2.pdf
  rounded_perc_format  = paste0("#0")

  if(perc_round_digits > 0)
    rounded_perc_format = paste0(rounded_perc_format, ".", paste0(rep("0", perc_round_digits), collapse = ""))

  # See also https://cran.r-project.org/web/packages/openxlsx2/openxlsx2.pdf
  rounded_catch_format = ifelse(pretty_print_catches, "#,##0", "0")

  if(catch_round_digits > 0)
    rounded_catch_format = paste0(rounded_catch_format, ".", paste0(rep("0", catch_round_digits), collapse = ""))

  # Sets to blank the percentage cumulative percentage, and total catch values every other row
  filtered_catalogue_data = filtered_catalogue_data[seq(2, nrow(filtered_catalogue_data), 2), `:=`(Perc = NA, PercCum = NA, TotCatches = NA)]

  # Prepares the final table binding the strata, the data, and the calculated info
  filtered_catalogue_data_rev = filtered_catalogue_data[, .(Species, Stock, Status, FlagName, GearGrp, DSet)]
  filtered_catalogue_data_rev = cbind(filtered_catalogue_data_rev, filtered_catalogue_data[, 11:ncol(filtered_catalogue_data)])
  filtered_catalogue_data_rev = cbind(filtered_catalogue_data_rev, filtered_catalogue_data[, .(Rank = FisheryRank, `%` = Perc, `%cum` = PercCum, `Σ`= TotCatches)])

  # Creates some custom cell styles
  workbook$add_dxfs_style(name = "no_t2",      bg_fill = wb_color(COLOR_NO_T2))
  workbook$add_dxfs_style(name = "limited_t2", bg_fill = wb_color(COLOR_LIMITED_T2))
  workbook$add_dxfs_style(name = "partial_t2", bg_fill = wb_color(COLOR_PARTIAL_T2))
  workbook$add_dxfs_style(name = "all_t2",     bg_fill = wb_color(COLOR_ALL_T2))
  workbook$add_dxfs_style(name = "no_t1",      bg_fill = wb_color(COLOR_NO_T1))

  workbook$add_dxfs_style(name = "UNCL_gear", font_color = wb_color(COLOR_UNCL_GEAR))

  # Fills some of the expected metadata (table reference, overall score, and total catch value)

  table_description = paste0("Table ", table_number, ". ", table_label)

  workbook$merge_cells(dims = "A1:G1")
  workbook$add_data(dims = "A1", x = table_description)
  workbook$add_font(dims = "A1", name = "Calibri", bold = TRUE)

  workbook$add_data(dims = "A3", x = "Score")
  workbook$add_data(dims = "B3", x = score)
  workbook$add_font(dims = "A3:B3", name = "Calibri", bold = TRUE)
  workbook$add_fill(dims = "A3:B3", color = wb_color(COLOR_SCORE))

  workbook$add_numfmt(dims = "B3", numfmt = "#0.00") # Two decimal digits for the score

  workbook$merge_cells(dims = "E2:F2")
  workbook$add_data(dims = "E2", x = "T1 Total")
  workbook$add_cell_style(dims = "E2", horizontal = "center")

  # Calculates total annual catches
  catches =
    data.frame(
      as.list(
        sapply(filtered_catalogue_data_rev[, 7:(ncol(filtered_catalogue_data_rev) - 4)],
               function(x) { sum(ifelse(x == "-1", 0, as.numeric(gsub(",", "", x))), na.rm = TRUE) }
        )
      )
    )

  # Adds the total annual catches to the workbook
  workbook$add_data(x = catches, start_col = 7, start_row = 2, na.strings = "", col_names = FALSE)

  # Formats annual catch value for each row with (or without) a thousands separator
  workbook$add_numfmt(dims = wb_dims(rows = 2, cols = 7:(ncol(filtered_catalogue_data_rev) - 4)), numfmt = rounded_catch_format) # See also https://cran.r-project.org/web/packages/openxlsx2/openxlsx2.pdf

  # Styles the workbook content - BEGIN
  workbook$add_border(dims = wb_dims(rows = 2, cols = 5:(ncol(filtered_catalogue_data_rev) - 4)),
                      top_border = "thin", bottom_border = "thin", left_border = "", right_border = "")

  workbook$add_font  (dims = wb_dims(rows = 4, cols = 1:(ncol(filtered_catalogue_data_rev) - 4)), name = "Calibri", bold = TRUE)
  workbook$add_border(dims = wb_dims(rows = 4, cols = 1:(ncol(filtered_catalogue_data_rev) - 4)),
                top_border = "thin", bottom_border = "thin", left_border = "", right_border = "")

  workbook$add_border(dims = wb_dims(rows = 4 + row_cutoff, cols = ncol(filtered_catalogue_data_rev) - 3),
                top_border = "thin", bottom_border = "", left_border = "", right_border = "")

  workbook$add_cell_style(dims = wb_dims(rows = 4, cols = 7:ncol(filtered_catalogue_data_rev)), horizontal = "center")

  data_dims     = wb_dims(rows = 5:( 5 + nrow(filtered_catalogue_data_rev) ), cols = 7:( ncol(filtered_catalogue_data_rev) - 4 ))

  workbook$add_cell_style(dims = data_dims, horizontal = "right")

  # Styles the workbook content - END

  # Adds conditional formatting to the workbook - BEGIN
  workbook$add_conditional_formatting(dims = paste0("E5:E", 5 + nrow(filtered_catalogue_data_rev)), rule = "=\"UN\"",  style = "UNCL_gear")

  workbook$add_conditional_formatting(dims = data_dims, rule = "=\"-1\"",  style = "no_t2")
  workbook$add_conditional_formatting(dims = data_dims, rule = "=\"a\"",   style = "limited_t2")
  workbook$add_conditional_formatting(dims = data_dims, rule = "=\"b\"",   style = "limited_t2")
  workbook$add_conditional_formatting(dims = data_dims, rule = "=\"c\"",   style = "limited_t2")
  workbook$add_conditional_formatting(dims = data_dims, rule = "=\"bc\"",  style = "limited_t2")
  workbook$add_conditional_formatting(dims = data_dims, rule = "=\"ab\"",  style = "partial_t2")
  workbook$add_conditional_formatting(dims = data_dims, rule = "=\"ac\"",  style = "partial_t2")
  workbook$add_conditional_formatting(dims = data_dims, rule = "=\"abc\"", style = "all_t2")

  for(rownum in seq(5, 5 + nrow(filtered_catalogue_data_rev), 2)) {
    t1_dims = wb_dims(rows = rownum, cols = 7:( ncol(filtered_catalogue_data_rev) - 4 ))

    first_t1_dims = wb_dims(rows = rownum,     cols = 7)
    first_t2_dims = wb_dims(rows = rownum + 1, cols = 7)

    workbook$add_conditional_formatting(dims = t1_dims,
                                        rule = paste0("AND(", "$E", rownum, "<>\"UN\", ", # Checks that the gear group code is not UN...
                                                              first_t1_dims, "=\"\", ",
                                                              first_t2_dims, "<>\"\", ",
                                                              first_t2_dims, "<>\"-1\")"), style = "no_t1")
  }

  perc_col     = ncol(filtered_catalogue_data_rev) - 2
  perc_cum_col = ncol(filtered_catalogue_data_rev) - 1

  workbook$add_conditional_formatting(dims = wb_dims(rows = 4:(4 + nrow(filtered_catalogue_data_rev)), cols = perc_col),
                                      style = GRADIENT_CATCH,
                                      rule = c(perc_quantiles["0%"][[1]], perc_quantiles["75%"][[1]], perc_quantiles["100%"][[1]]),
                                      type = "colorScale")

  workbook$add_conditional_formatting(dims = wb_dims(rows = 4:(4 + nrow(filtered_catalogue_data_rev)), cols = perc_cum_col),
                                      style = GRADIENT_CATCH_CUMULATIVE,
                                      rule = c(perc_cum_quantiles["0%"][[1]], perc_cum_quantiles["50%"][[1]], perc_cum_quantiles["100%"][[1]]),
                                      type = "colorScale")

  workbook$add_ignore_error(dims = data_dims, number_stored_as_text = TRUE)
  workbook$add_ignore_error(dims = wb_dims(rows = 4, cols = 7:( ncol(filtered_catalogue_data_rev) - 4 )), number_stored_as_text = TRUE)
  # Adds conditional formatting to the workbook - END

  # As the original data table contains both numbers and text within each data column, by simply writing the table
  # to Excel we risk the output cells to be interpreted as text, no matter the format we explicitly set for them.
  # The workaround is to write all numeric rows (every other row) separately from the text rows, so that Openxlsx2
  # correctly treats the output cells as numeric.

  # Extracts all rows (from the catalog) containing catch values and converts the columns into numeric values to
  # avoid Excel considering
  filtered_catalogue_data_rev_num = filtered_catalogue_data_rev[seq(1, nrow(filtered_catalogue_data_rev), 2)]
  to_numeric = colnames(filtered_catalogue_data_rev_num[, 7:(ncol(filtered_catalogue_data_rev_num) - 4)])
  filtered_catalogue_data_rev_num = filtered_catalogue_data_rev_num[, (to_numeric) := lapply(.SD, as.numeric), .SDcols = to_numeric]

  filtered_catalogue_data_rev_txt = filtered_catalogue_data_rev[seq(2, nrow(filtered_catalogue_data_rev), 2)]

  # Writes the table header.(it is necessary to convert the column names into a list to ensure these are written as a row)
  workbook$add_data(x = as.list(colnames(filtered_catalogue_data_rev_num)), start_col = 1, start_row = 4)

  # Writes all numeric values
  for(num_row in 1:nrow(filtered_catalogue_data_rev_num))
    workbook$add_data(x = filtered_catalogue_data_rev_num[num_row], start_col = 1, start_row = 5 + ( num_row - 1 ) * 2, na.strings = "", col_names = FALSE)

  # Writes all text values
  for(num_row in 1:nrow(filtered_catalogue_data_rev_txt))
    workbook$add_data(x = filtered_catalogue_data_rev_txt[num_row], start_col = 1, start_row = 6 + ( num_row - 1 ) * 2, na.strings = "", col_names = FALSE)

  # Formats percentages (and cumulative percentages) with two trailing digits
  workbook$add_numfmt(dims = wb_dims(rows = 4:(4 + nrow(filtered_catalogue_data_rev)), cols = perc_col:perc_cum_col), numfmt = rounded_perc_format)

  # Formats total catch value for each row with a thousands separator
  workbook$add_numfmt(dims = wb_dims(rows = 4:(4 + nrow(filtered_catalogue_data_rev)), cols = ncol(filtered_catalogue_data_rev)), numfmt = rounded_catch_format)

  # Formats all catch value with a thousands separator
  workbook$add_numfmt(dims = data_dims, numfmt = rounded_catch_format)

  workbook$set_col_widths(cols = 4, widths = 26.64)

  return(workbook)
}
