report_domain_title <- function(domain) {
  switch(
    domain,
    time = "Time-Domain Metrics",
    frequency = "Frequency-Domain Metrics",
    nonlinear = "Nonlinear Metrics",
    quality = "Quality Metrics",
    derived = "Derived Metrics",
    domain
  )
}

report_domain_table <- function(result, domain) {
  source <- switch(
    domain,
    time = result$metrics$time,
    frequency = result$metrics$frequency,
    nonlinear = result$metrics$nonlinear,
    quality = result$preprocessing$quality,
    derived = result$metrics$derived,
    NULL
  )
  if (is.null(source)) {
    return(data.frame())
  }

  flattened <- flatten_metrics(stats::setNames(list(source), domain))
  data.frame(
    Measure = pretty_metric_name(names(flattened)),
    Value = vapply(unname(flattened), report_metric_value, character(1)),
    row.names = NULL,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

report_rmd <- function() {
  c(
    "---",
    "title: \"openhrv report\"",
    "output: html_document",
    "---",
    "",
    "<style>",
    ".openhrv-grid {display: grid; grid-template-columns: minmax(320px, 1fr) minmax(320px, 1fr); gap: 24px; align-items: start;}",
    ".openhrv-panel {background: #ffffff; border: 1px solid #d9dee7; border-radius: 10px; padding: 16px; margin-bottom: 20px;}",
    ".openhrv-panel h2 {margin-top: 0;}",
    "@media (max-width: 900px) {.openhrv-grid {grid-template-columns: 1fr;}}",
    "</style>",
    "",
    "```{r setup, include=FALSE}",
    "knitr::opts_chunk$set(echo = FALSE)",
    "```",
    "",
    "## Summary",
    "",
    "```{r}",
    "print(result)",
    "```",
    "",
    "<div class='openhrv-grid'>",
    "<div>",
    "<div class='openhrv-panel'>",
    "<h2>Metrics</h2>",
    "",
    "```{r, results='asis'}",
    "for (domain in c(\"time\", \"frequency\", \"nonlinear\", \"derived\", \"quality\")) {",
    "  tbl <- openhrv:::report_domain_table(result, domain)",
    "  if (nrow(tbl) > 0) {",
    "    cat(sprintf(\"<h3>%s</h3>\\n\", openhrv:::report_domain_title(domain)))",
    "    cat(",
    "      knitr::kable(",
    "        tbl,",
    "        format = \"html\",",
    "        align = c(\"l\", \"r\"),",
    "        escape = TRUE,",
    "        table.attr = \"class='table table-striped table-sm'\"",
    "      ),",
    "      \"\\n\"",
    "    )",
    "  }",
    "}",
    "```",
    "</div>",
    "</div>",
    "<div>",
    "<div class='openhrv-panel'>",
    "<h2>Plots</h2>",
    "",
    "```{r}",
    "print(ggplot2::autoplot(result, type = \"poincare\"))",
    "```",
    "",
    "```{r}",
    "if (!is.null(result$spectrum)) print(ggplot2::autoplot(result, type = \"spectrum\"))",
    "```",
    "",
    "```{r}",
    "print(ggplot2::autoplot(result, type = \"dfa\"))",
    "```",
    "</div>",
    "</div>",
    "</div>"
  )
}

#' Generate an HRV report
#'
#' @param x An `hrv_result` object.
#' @param output Output format. Only `"html"` is currently supported.
#'
#' @return The path to the generated report.
#' @export
hrv_report <- function(x, output = "html") {
  if (!inherits(x, "hrv_result")) {
    stop("`x` must be an `hrv_result` object.", call. = FALSE)
  }
  if (!identical(output, "html")) {
    stop("Only `output = \"html\"` is currently supported.", call. = FALSE)
  }
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    stop("Package `rmarkdown` is required for report generation.", call. = FALSE)
  }

  temp_dir <- tempdir()
  input <- file.path(temp_dir, "openhrv-report.Rmd")
  output_file <- file.path(temp_dir, "openhrv-report.html")
  writeLines(report_rmd(), con = input)
  rmarkdown::render(
    input = input,
    output_file = output_file,
    quiet = TRUE,
    envir = list2env(list(result = x), parent = globalenv())
  )
  output_file
}
