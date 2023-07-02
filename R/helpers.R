## Process map styles

.styles_to_url = function (style) {
  .check_map_style(style)
  style |>
    lapply(\(x) paste(names(x), x, sep = ':')) |>
    sapply(\(x) paste0(x, collapse = '|')) |>
    paste0(collapse = '&style=')
}   
