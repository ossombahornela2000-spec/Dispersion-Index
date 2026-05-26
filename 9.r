############################################################
# PROFESSIONAL PNG GRAPH
# Cameroon - SHEET: InfCameroon
# SERIES:
#   Cho = custom blue
#   9S  = orange dashed
#   12S = grey solid
#   15S = green dotdash
#
# IMPORTANT:
# ISO_WEEK is not used because it is often missing.
# The x-axis uses the row order as a time index.
############################################################

##############################
# 1. PACKAGE INSTALLATION
##############################

install_if_missing <- function(packages) {
  missing_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
  
  if (length(missing_packages) > 0) {
    install.packages(missing_packages, dependencies = TRUE)
  }
  
  invisible(lapply(packages, library, character.only = TRUE))
}

required_packages <- c(
  "readxl",
  "dplyr",
  "ggplot2",
  "janitor",
  "stringr",
  "scales",
  "tidyr"
)

install_if_missing(required_packages)

##############################
# 2. PARAMETERS
##############################

excel_file <- "cholera_influenza_nettoye (3).xlsx"
sheet_name <- "InfCameroon"
country_name <- "Cameroon"
output_folder <- "professional_graphs_Cameroon"

if (!dir.exists(output_folder)) {
  dir.create(output_folder, recursive = TRUE)
}

##############################
# 3. GRAPH THEME
##############################

research_theme <- function() {
  ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        face = "bold",
        size = 16,
        hjust = 0
      ),
      plot.subtitle = ggplot2::element_text(
        size = 11,
        color = "gray30"
      ),
      plot.caption = ggplot2::element_text(
        size = 9,
        color = "gray40",
        hjust = 0
      ),
      axis.title = ggplot2::element_text(
        face = "bold",
        size = 11
      ),
      axis.text = ggplot2::element_text(
        size = 10,
        color = "gray20"
      ),
      legend.position = "bottom",
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = 10),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(
        color = "gray88",
        linewidth = 0.35
      ),
      plot.margin = ggplot2::margin(10, 15, 10, 10)
    )
}

##############################
# 4. READ THE EXCEL SHEET
##############################

df <- readxl::read_excel(excel_file, sheet = sheet_name) %>%
  janitor::clean_names()

cat("Detected columns:\n")
print(names(df))

##############################
# 5. DETECT THE REQUIRED COLUMNS
##############################

# Detect Cho
# After janitor::clean_names(), Cho usually becomes cho.
col_cho <- names(df)[
  names(df) %in% c(
    "cho",
    "cholera",
    "cholera_cases",
    "cas_cholera",
    "cas_de_cholera"
  )
]

if (length(col_cho) == 0) {
  col_cho <- names(df)[
    stringr::str_detect(names(df), "^cho$|cholera|cas.*chol")
  ]
}

if (length(col_cho) == 0) {
  stop("The Cho column was not found.")
}

# Detect 9S
# After janitor::clean_names(), a column named 9S usually becomes x9s.
col_9s <- names(df)[
  names(df) %in% c(
    "x9s",
    "x9_s",
    "s9",
    "nine_s",
    "moving_average_9s"
  )
]

if (length(col_9s) == 0) {
  col_9s <- names(df)[
    stringr::str_detect(names(df), "^x?9s$|^x?9_s$")
  ]
}

if (length(col_9s) == 0) {
  stop("The 9S column was not found.")
}

# Detect 12S
# After janitor::clean_names(), a column named 12S usually becomes x12s.
col_12s <- names(df)[
  names(df) %in% c(
    "x12s",
    "x12_s",
    "s12",
    "twelve_s",
    "moving_average_12s"
  )
]

if (length(col_12s) == 0) {
  col_12s <- names(df)[
    stringr::str_detect(names(df), "^x?12s$|^x?12_s$")
  ]
}

if (length(col_12s) == 0) {
  stop("The 12S column was not found.")
}

# Detect 15S
# After janitor::clean_names(), a column named 15S usually becomes x15s.
col_15s <- names(df)[
  names(df) %in% c(
    "x15s",
    "x15_s",
    "s15",
    "fifteen_s",
    "moving_average_15s"
  )
]

if (length(col_15s) == 0) {
  col_15s <- names(df)[
    stringr::str_detect(names(df), "^x?15s$|^x?15_s$")
  ]
}

if (length(col_15s) == 0) {
  stop("The 15S column was not found.")
}

cat("Column used for Cho:", col_cho[1], "\n")
cat("Column used for 9S:", col_9s[1], "\n")
cat("Column used for 12S:", col_12s[1], "\n")
cat("Column used for 15S:", col_15s[1], "\n")

##############################
# 6. PREPARE THE DATA
##############################

# ISO_WEEK is ignored.
# The chronological order is based on the row number.

df_plot <- df %>%
  dplyr::mutate(
    time_index = dplyr::row_number(),
    Cho = as.numeric(.data[[col_cho[1]]]),
    `9S` = as.numeric(.data[[col_9s[1]]]),
    `12S` = as.numeric(.data[[col_12s[1]]]),
    `15S` = as.numeric(.data[[col_15s[1]]])
  ) %>%
  dplyr::select(time_index, Cho, `9S`, `12S`, `15S`) %>%
  tidyr::pivot_longer(
    cols = c(Cho, `9S`, `12S`, `15S`),
    names_to = "series",
    values_to = "value"
  ) %>%
  dplyr::filter(!is.na(value))

##############################
# 7. BUILD THE GRAPH
##############################

g <- ggplot2::ggplot(
  df_plot,
  ggplot2::aes(
    x = time_index,
    y = value,
    color = series,
    linetype = series,
    linewidth = series
  )
) +
  ggplot2::geom_line(
    alpha = 0.95,
    na.rm = TRUE
  ) +
  ggplot2::scale_color_manual(
    values = c(
      "Cho" = "#A9BBCC",
      "9S"  = "#F28E2B",
      "12S" = "#7A7A7A",
      "15S" = "#4E7D4E"
    ),
    labels = c(
      "Cho" = "Cho",
      "9S"  = "9S",
      "12S" = "12S",
      "15S" = "15S"
    )
  ) +
  ggplot2::scale_linetype_manual(
    values = c(
      "Cho" = "solid",
      "9S"  = "dashed",
      "12S" = "solid",
      "15S" = "dotdash"
    ),
    labels = c(
      "Cho" = "Cho",
      "9S"  = "9S",
      "12S" = "12S",
      "15S" = "15S"
    )
  ) +
  ggplot2::scale_linewidth_manual(
    values = c(
      "Cho" = 0.95,
      "9S"  = 1.00,
      "12S" = 0.90,
      "15S" = 0.95
    ),
    labels = c(
      "Cho" = "Cho",
      "9S"  = "9S",
      "12S" = "12S",
      "15S" = "15S"
    )
  ) +
  ggplot2::scale_x_continuous(
    breaks = scales::pretty_breaks(n = 12),
    expand = ggplot2::expansion(mult = c(0.01, 0.02))
  ) +
  ggplot2::scale_y_continuous(
    labels = scales::comma,
    expand = ggplot2::expansion(mult = c(0.02, 0.08))
  ) +
  ggplot2::labs(
    title = paste("Cholera Trend Analysis in", country_name),
    subtitle = "Comparison between observed Cho and the 9S, 12S and 15S smoothed series",
    x = "Observation order",
    y = "Number of cases",
    caption = "Source: provided Excel dataset. Figure generated with R."
  ) +
  research_theme() +
  ggplot2::guides(
    color = ggplot2::guide_legend(nrow = 1),
    linetype = ggplot2::guide_legend(nrow = 1),
    linewidth = "none"
  )

print(g)

##############################
# 8. EXPORT TO PNG
##############################

output_png <- file.path(
  output_folder,
  "cholera_Cho_9S_12S_15S_Cameroon.png"
)

ggplot2::ggsave(
  filename = output_png,
  plot = g,
  width = 12,
  height = 6.5,
  dpi = 600,
  bg = "white"
)

message("PNG image successfully generated: ", output_png)