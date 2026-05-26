############################################################
# PROFESSIONAL PNG GRAPH
# Nigeria - SHEET: InfNigeria
# SERIES: INF_A (custom blue) and InfA12S (orange dashed)
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
  "ISOweek",
  "scales",
  "tidyr"
)

install_if_missing(required_packages)

##############################
# 2. PARAMETERS
##############################

excel_file <- "cholera_influenza_nettoye (3).xlsx"
sheet_name <- "InfNigeria"
country_name <- "Nigeria"
output_folder <- "professional_graphs_Nigeria"

if (!dir.exists(output_folder)) {
  dir.create(output_folder, recursive = TRUE)
}

##############################
# 3. FUNCTIONS
##############################

create_iso_date <- function(year, week) {
  ISOweek::ISOweek2date(
    paste0(
      as.integer(year),
      "-W",
      sprintf("%02d", as.integer(week)),
      "-1"
    )
  )
}

research_theme <- function() {
  ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 16, hjust = 0),
      plot.subtitle = ggplot2::element_text(size = 11, color = "gray30"),
      plot.caption = ggplot2::element_text(size = 9, color = "gray40", hjust = 0),
      axis.title = ggplot2::element_text(face = "bold", size = 11),
      axis.text = ggplot2::element_text(size = 10, color = "gray20"),
      legend.position = "bottom",
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = 10),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(color = "gray88", linewidth = 0.35),
      plot.margin = ggplot2::margin(10, 15, 10, 10)
    )
}

##############################
# 4. READ THE EXCEL FILE
##############################

df <- readxl::read_excel(excel_file, sheet = sheet_name) %>%
  janitor::clean_names()

cat("Detected columns:\n")
print(names(df))

##############################
# 5. DETECT THE REQUIRED COLUMNS
##############################

col_inf_a <- names(df)[names(df) %in% c("inf_a", "infa")]

if (length(col_inf_a) == 0) {
  stop("The INF_A column was not found.")
}

col_inf_a12s <- names(df)[
  names(df) %in% c(
    "infa12s",
    "inf_a12s",
    "inf_a_12s",
    "infas12",
    "inf_as12",
    "x12s"
  )
]

if (length(col_inf_a12s) == 0) {
  col_inf_a12s <- names(df)[
    stringr::str_detect(names(df), "inf.*a.*12.*s|12.*s")
  ]
}

if (length(col_inf_a12s) == 0) {
  stop("The InfA12S column was not found.")
}

cat("Column used for INF_A:", col_inf_a[1], "\n")
cat("Column used for InfA12S:", col_inf_a12s[1], "\n")

##############################
# 6. PREPARE THE DATA
##############################

df_plot <- df %>%
  dplyr::mutate(
    iso_year = as.integer(iso_year),
    iso_week = as.integer(iso_week),
    date = create_iso_date(iso_year, iso_week),
    INF_A = as.numeric(.data[[col_inf_a[1]]]),
    InfA12S = as.numeric(.data[[col_inf_a12s[1]]])
  ) %>%
  dplyr::arrange(date) %>%
  dplyr::select(date, INF_A, InfA12S) %>%
  tidyr::pivot_longer(
    cols = c(INF_A, InfA12S),
    names_to = "series",
    values_to = "value"
  )

##############################
# 7. BUILD THE GRAPH
##############################

g <- ggplot2::ggplot(df_plot, ggplot2::aes(x = date, y = value, color = series, linetype = series)) +
  ggplot2::geom_line(
    linewidth = 0.95,
    alpha = 0.95,
    na.rm = TRUE
  ) +
  ggplot2::scale_color_manual(
    values = c(
      "INF_A" = "#A9BBCC",   # updated blue from your sample
      "InfA12S" = "#F28E2B"  # orange
    ),
    labels = c(
      "INF_A" = "INF_A",
      "InfA12S" = "InfA12S"
    )
  ) +
  ggplot2::scale_linetype_manual(
    values = c(
      "INF_A" = "solid",
      "InfA12S" = "dashed"
    ),
    labels = c(
      "INF_A" = "INF_A",
      "InfA12S" = "InfA12S"
    )
  ) +
  ggplot2::scale_x_date(
    date_breaks = "2 years",
    date_labels = "%Y"
  ) +
  ggplot2::scale_y_continuous(
    labels = scales::comma,
    expand = ggplot2::expansion(mult = c(0.02, 0.08))
  ) +
  ggplot2::labs(
    title = paste("Influenza A Trends in", country_name),
    subtitle = "Comparison between observed INF_A and 12-week smoothed InfA12S",
    x = "Year",
    y = "Number of cases",
    caption = ""
  ) +
  research_theme()

print(g)

##############################
# 8. EXPORT TO PNG
##############################

output_png <- file.path(
  output_folder,
  "influenza_A_vs_InfA12S_Nigeria.png"
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