#' TBD
#'
#' @param data_matrix TBD
#' @return TBD
catalogue.viz.table.bg_matrix = function(data_matrix) {
  bg_matrix =
    ifelse(is.na(data_matrix) | data_matrix == "-", "lightgray",
           ifelse(data_matrix == "-1", "red",
                  ifelse(data_matrix == "a" | data_matrix == "b" | data_matrix == "c" | data_matrix == "bc", "yellow",
                         ifelse(data_matrix == "ab" | data_matrix == "ac", "green",
                                ifelse(data_matrix == "abc", darken("green", amount = .3),
                                       "white"
                                )
                         )
                  )
           )
    )

  return(
    bg_matrix
  )
}

#' TBD
#'
#' @param data_matrix TBD
#' @return TBD
catalogue.viz.table.fg_matrix = function(data_matrix) {
  fg_matrix =
    ifelse(is.na(data_matrix) | data_matrix == "abc", "black", "black") # Originally "white", "black", when the darkest green was "darkgreen"

  return(fg_matrix)
}

#' TBD
#'
#' @return TBD
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

#' TBD
#'
#' @return TBD
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

#' TBD
#'
#' @param catalogue_data TBD
#' @param remove_species TBD
#' @param remove_stock TBD
#' @param truncate_years TBD
#' @param flag_separator_width TBD
#' @param default_font_size TBD
#' @param values_font_size TBD
#' @param default_h_padding TBD
#' @param values_h_padding TBD
#' @return TBD
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

  FT = FT %>%

    flextable::color(part = "body", j = first_year_column:ncol(catalogue_data), color = fg_matrix) %>%
    flextable::autofit() %>%
    flextable::fix_border_issues()

  end = Sys.time()

  DEBUG(paste0("Finished building catalog table in ", end - start))

  return(FT)
}
