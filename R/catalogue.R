#' TBD
#'
#' @param catalogue_data TBD
#' @param remove_species TBD
#' @param remove_stock TBD
#' @param truncate_years TBD
#' @return TBD
#' @export
catalogue.viz.table = function(catalogue_data, show_catches_gradient = TRUE, remove_species = FALSE, remove_stock = FALSE, truncate_years = TRUE) {
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

  bg_matrix = catalogue_data[, first_year_column:ncol(catalogue_data)]

  bg_matrix =
    ifelse(is.na(bg_matrix) | bg_matrix == "-", "darkgrey",
           ifelse(bg_matrix == "-1", "red",
                  ifelse(bg_matrix == "a" | bg_matrix == "b" | bg_matrix == "c" | bg_matrix == "bc", "yellow",
                         ifelse(bg_matrix == "ab" | bg_matrix == "ac", "green",
                                ifelse(bg_matrix == "abc", darken("green", amount = .3),
                                       "white"
                                )
                         )
                  )
           )
    )

  fg_matrix = catalogue_data[, first_year_column:ncol(catalogue_data)]
  fg_matrix =
    ifelse(is.na(fg_matrix) | fg_matrix == "abc", "black", "black") # Originally "white", "black", when the darkest green was "darkgreen"

  #set_flextable_defaults(extra_css = "th p span, td p span { white-space: nowrap !important; }")

  FT = flextable(catalogue_data) %>%
    set_table_properties(opts_html = list(extra_css = "th p span, td p span { white-space: nowrap !important; }")) %>%
    set_header_labels(values = list(FlagName = "Flag",
                                    GearGrp = "Gear",
                                    FisheryRank = "Rank",
                                    TotCatches = "Total (t)",
                                    Perc = "%",
                                    PercCum = "% (cum.)",
                                    DSet = "DS")) %>%

    fontsize(size = 8, part = "all") %>%
    fontsize(size = 6, i = seq(1, nrow(catalogue_data), 2), j = first_year_column:ncol(catalogue_data), part = "body") %>%

    padding(padding        = 1, part = "all") %>%
    padding(padding.top    = 0, part = "body") %>%
    padding(padding.bottom = 0, part = "body") %>%
    padding(padding.left   = 3, part = "body", j = c("FlagName")) %>%
    padding(padding.right  = 2, part = "body", j = c("TotCatches", "Perc", "PercCum")) %>%

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

    flextable::border_inner(part = "all",                                       border        = fp_border(width =  .5)) %>%
    flextable::border      (part = "body", i = seq(2, nrow(catalogue_data), 2), border.bottom = fp_border(width = 1.0)) %>%

    flextable::color(part = "body", j = "GearGrp", i = catalogue_data[GearGrp == "UN", which = TRUE], color = "red") %>%

    flextable::bg   (part = "body", j = first_year_column:ncol(catalogue_data), bg    = bg_matrix)

  if(show_catches_gradient) {
    bg_matrix_catch = catalogue_data[, c("Perc", "PercCum")]
    #bg_matrix_catch$Perc    = rgb(.3, 1, .3, 1 - bg_matrix_catch$Perc    / 100)
    bg_matrix_catch$PercCum = rgb(.3, 1, .3,     bg_matrix_catch$PercCum / 100)

    FT = FT%>%
      #flextable::bg   (part = "body", j = 8, bg    = bg_matrix_catch$Perc) %>%
      flextable::bg   (part = "body", j = "PercCum", bg    = bg_matrix_catch$PercCum)
  }

  FT = FT %>%

    flextable::color(part = "body", j = first_year_column:ncol(catalogue_data), color = fg_matrix) %>%

    flextable::width(j = first_year_column:ncol(catalogue_data), width = .30) %>%
    flextable::fix_border_issues()

  end = Sys.time()

  DEBUG(paste0("Finished building catalog table in ", end - start))

  return(FT)
}
