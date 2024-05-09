#' @title Markdown Elements for Report generation.
#'
#' @description
#' Elements are stored in constant named list and are called dynamically.
#'
#' @return named lists of markdown elements.
#'
#' @author Priyansh Srivastava \email{spriyansh29@@gmail.com}
#'
#' @keywords internal


# Dynamic content creation
author_info <- paste("Package Version:", packageVersion("scMaSigPro"))
author_line <- sprintf("\"%s\"", author_info)

# Title YAML elements
title_yaml <- list(
  title = "'ScMaSigPro Analysis Report'",
  author = author_line,
  date = Sys.Date()
)

# Horizontal Rule
hrule <- "---"

# Code-Chunk End
code_chunk_end <- "```"

# Code-chunck headers
code_chunk <- list(
  run_echo = "```{r, echo=TRUE, eval=TRUE, warning=FALSE, message=FALSE}",
  run_no_echo = "```{r, echo=FALSE, eval=TRUE, warning=FALSE, message=FALSE}",
  no_run_echo = "```{r, echo=TRUE, eval=FALSE, warning=FALSE, message=FALSE}"
)

# Empty String
empty_string <- ""

# Custom CSS link
custom_css_link <- '<head><link rel="stylesheet" type="text/css" href="inst/styles/markdown_report.css"></head>'

# Output types
output_style <- list(
  html_output = "html_document:
    css: ['inst/styles/markdown_report.css']"
)

# Table elements
table_elements <- list(
  table_tag_open = "<table>",
  table_tag_close = "</table>",
  table_header_open = "<th>",
  table_header_close = "</th>",
  table_row_open = "<tr>",
  table_row_close = "</tr>",
  table_data_open = "<td>",
  table_data_close = "</td>"
)
