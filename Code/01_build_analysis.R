# Build U.S. income and employment maps, association plots, and state tables.
# Run from the project root (open the .Rproj file first).

required_packages <- c(
  "dplyr", "ggplot2", "ggrepel", "httr2", "jsonlite", "patchwork",
  "purrr", "readr", "scales", "sf", "stringr", "tibble"
)
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0) {
  stop(
    "Install the missing packages first: ",
    paste(missing_packages, collapse = ", "),
    call. = FALSE
  )
}

if (!file.exists("us-income-employment.Rproj")) {
  stop("Run this script from the project root.", call. = FALSE)
}

dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
dir.create("data/derived", recursive = TRUE, showWarnings = FALSE)
dir.create("figures", recursive = TRUE, showWarnings = FALSE)
dir.create("results", recursive = TRUE, showWarnings = FALSE)

# Include the 50 states and Washington, DC. Puerto Rico and territories are
# excluded so the state and county maps use a consistent U.S. geography.
state_abbr_lookup <- c(
  "01" = "AL", "02" = "AK", "04" = "AZ", "05" = "AR", "06" = "CA",
  "08" = "CO", "09" = "CT", "10" = "DE", "11" = "DC", "12" = "FL",
  "13" = "GA", "15" = "HI", "16" = "ID", "17" = "IL", "18" = "IN",
  "19" = "IA", "20" = "KS", "21" = "KY", "22" = "LA", "23" = "ME",
  "24" = "MD", "25" = "MA", "26" = "MI", "27" = "MN", "28" = "MS",
  "29" = "MO", "30" = "MT", "31" = "NE", "32" = "NV", "33" = "NH",
  "34" = "NJ", "35" = "NM", "36" = "NY", "37" = "NC", "38" = "ND",
  "39" = "OH", "40" = "OK", "41" = "OR", "42" = "PA", "44" = "RI",
  "45" = "SC", "46" = "SD", "47" = "TN", "48" = "TX", "49" = "UT",
  "50" = "VT", "51" = "VA", "53" = "WA", "54" = "WV", "55" = "WI",
  "56" = "WY"
)
valid_state_fips <- names(state_abbr_lookup)

acs_file <- "data/raw/acs2024_5yr_us_states_counties.json"
state_geometry_file <- "data/raw/tiger2024_us_states.geojson"
acs_url <- paste0(
  "https://api.censusreporter.org/1.0/data/show/acs2024_5yr",
  "?table_ids=B19013%2CB23025",
  "&geo_ids=040%7C01000US%2C050%7C01000US"
)
state_geometry_url <- paste0(
  "https://api.censusreporter.org/1.0/geo/show/tiger2024",
  "?geo_ids=040%7C01000US"
)

download_if_missing <- function(url, path) {
  if (file.exists(path) && file.info(path)$size > 0) {
    message("Using cached file: ", path)
    return(invisible(path))
  }

  message("Downloading: ", path)
  response <- httr2::request(url) |>
    httr2::req_user_agent(
      "US-state-county-labor-income-maps/1.0 (educational project)"
    ) |>
    httr2::req_timeout(180) |>
    httr2::req_retry(max_tries = 4) |>
    httr2::req_perform()
  httr2::resp_check_status(response)
  writeBin(httr2::resp_body_raw(response), path)
  invisible(path)
}

download_if_missing(acs_url, acs_file)
download_if_missing(state_geometry_url, state_geometry_file)

# The nationwide county GeoJSON is large, so request five states per file.
county_geometry_files <- sprintf(
  "data/raw/tiger2024_us_counties_part_%02d.geojson",
  seq_along(seq(1, length(valid_state_fips), by = 5))
)
county_chunks <- split(
  valid_state_fips,
  ceiling(seq_along(valid_state_fips) / 5)
)
for (i in seq_along(county_chunks)) {
  geography_ids <- paste0("050|04000US", county_chunks[[i]])
  encoded_ids <- utils::URLencode(
    paste(geography_ids, collapse = ","),
    reserved = TRUE
  )
  county_geometry_url <- paste0(
    "https://api.censusreporter.org/1.0/geo/show/tiger2024?geo_ids=",
    encoded_ids
  )
  download_if_missing(county_geometry_url, county_geometry_files[[i]])
}

acs <- jsonlite::fromJSON(acs_file, simplifyVector = FALSE)

get_value <- function(geography_id, table, part, variable) {
  value <- acs$data[[geography_id]][[table]][[part]][[variable]]
  value <- suppressWarnings(as.numeric(value))
  if (length(value) == 0 || is.na(value) || value < 0) {
    return(NA_real_)
  }
  value
}

build_geography_data <- function(prefix, geography_level) {
  geography_ids <- names(acs$data)[startsWith(names(acs$data), prefix)]

  purrr::map_dfr(geography_ids, function(current_geoid) {
    geography_name <- acs$geography[[current_geoid]]$name
    fips <- stringr::str_remove(current_geoid, prefix)
    current_state_fips <- if (geography_level == "state") {
      fips
    } else {
      stringr::str_sub(fips, 1, 2)
    }

    tibble::tibble(
      geoid = current_geoid,
      state_fips = current_state_fips,
      geography_name = geography_name,
      median_household_income = get_value(
        current_geoid, "B19013", "estimate", "B19013001"
      ),
      income_moe_90 = get_value(
        current_geoid, "B19013", "error", "B19013001"
      ),
      population_16_plus = get_value(
        current_geoid, "B23025", "estimate", "B23025001"
      ),
      armed_forces_16_plus = get_value(
        current_geoid, "B23025", "estimate", "B23025006"
      ),
      civilian_population_16_plus =
        population_16_plus - armed_forces_16_plus,
      employed_civilians = get_value(
        current_geoid, "B23025", "estimate", "B23025004"
      ),
      employment_population_ratio_pct = 100 * employed_civilians /
        civilian_population_16_plus
    )
  })
}

state_data <- build_geography_data("04000US", "state") |>
  dplyr::filter(state_fips %in% valid_state_fips) |>
  dplyr::mutate(
    state_name = geography_name,
    state_abbr = unname(state_abbr_lookup[state_fips])
  ) |>
  dplyr::arrange(state_name)

state_name_lookup <- stats::setNames(state_data$state_name, state_data$state_fips)
county_data <- build_geography_data("05000US", "county") |>
  dplyr::filter(state_fips %in% valid_state_fips) |>
  dplyr::mutate(
    county_name = stringr::str_remove(geography_name, ", [A-Z]{2}$"),
    state_name = unname(state_name_lookup[state_fips]),
    state_abbr = unname(state_abbr_lookup[state_fips])
  ) |>
  dplyr::arrange(state_name, county_name)

if (nrow(state_data) != 51L || anyDuplicated(state_data$geoid) > 0L) {
  stop("Expected one row for each of the 50 states and Washington, DC.")
}
if (nrow(county_data) != 3144L || anyDuplicated(county_data$geoid) > 0L) {
  stop("Expected 3,144 U.S. counties and county equivalents (50 states + DC).")
}
if (anyNA(state_data$median_household_income) ||
    anyNA(state_data$employment_population_ratio_pct) ||
    anyNA(county_data$employment_population_ratio_pct)) {
  stop("Income or employment estimates are unexpectedly missing.")
}

readr::write_csv(
  state_data,
  "data/derived/us_state_acs_2020_2024.csv",
  na = ""
)
readr::write_csv(
  county_data,
  "data/derived/us_county_acs_2020_2024.csv",
  na = ""
)

state_boundaries <- sf::st_read(state_geometry_file, quiet = TRUE) |>
  dplyr::mutate(
    geoid = as.character(geoid),
    state_fips = stringr::str_extract(geoid, "[0-9]{2}$")
  ) |>
  dplyr::filter(state_fips %in% valid_state_fips) |>
  dplyr::left_join(state_data, by = "geoid")

county_boundaries <- purrr::map(
  county_geometry_files,
  sf::st_read,
  quiet = TRUE
) |>
  purrr::reduce(dplyr::bind_rows) |>
  dplyr::mutate(
    geoid = as.character(geoid),
    state_fips = stringr::str_extract(geoid, "[0-9]{2}(?=[0-9]{3}$)")
  ) |>
  dplyr::filter(state_fips %in% valid_state_fips) |>
  dplyr::left_join(county_data, by = "geoid")

if (nrow(state_boundaries) != 51L ||
    anyNA(state_boundaries$median_household_income)) {
  stop("State ACS estimates did not join to all 51 state-level polygons.")
}
if (nrow(county_boundaries) != 3144L ||
    anyDuplicated(county_boundaries$geoid) > 0L ||
    anyNA(county_boundaries$employment_population_ratio_pct)) {
  stop("County ACS estimates did not join uniquely to all county polygons.")
}

# Preserve both joined GIS layers in their source geographic CRS.
gpkg_file <- "results/us_labor_market_maps.gpkg"
sf::st_write(
  state_boundaries,
  dsn = gpkg_file,
  layer = "states",
  driver = "GPKG",
  delete_dsn = TRUE,
  quiet = TRUE
)
sf::st_write(
  county_boundaries,
  dsn = gpkg_file,
  layer = "counties",
  driver = "GPKG",
  append = TRUE,
  quiet = TRUE
)

state_income_limits <- range(state_data$median_household_income, na.rm = TRUE)
county_income_limits <- range(county_data$median_household_income, na.rm = TRUE)
state_employment_limits <- range(
  state_data$employment_population_ratio_pct,
  na.rm = TRUE
)
county_employment_limits <- range(
  county_data$employment_population_ratio_pct,
  na.rm = TRUE
)

get_extreme_counties <- function(
  data,
  variable,
  n_ext = 3,
  min_pop = 0
) {
  valid_data <- data |>
    sf::st_drop_geometry() |>
    dplyr::filter(
      !is.na(.data[[variable]]),
      population_16_plus >= min_pop
    )

  top_n <- valid_data |>
    dplyr::slice_max(
      order_by = .data[[variable]],
      n = n_ext,
      with_ties = FALSE
    ) |>
    dplyr::mutate(extreme_type = "Highest")

  bottom_n <- valid_data |>
    dplyr::slice_min(
      order_by = .data[[variable]],
      n = n_ext,
      with_ties = FALSE
    ) |>
    dplyr::mutate(extreme_type = "Lowest")

  dplyr::bind_rows(top_n, bottom_n) |>
    dplyr::mutate(
      short_county = stringr::str_remove(
        county_name,
        " (County|Parish|Borough|Census Area|Municipality|city)$"
      ),
      val_formatted = if (variable == "median_household_income") {
        scales::dollar(
          .data[[variable]],
          scale = 0.001,
          suffix = "k",
          accuracy = 0.1
        )
      } else {
        scales::percent(.data[[variable]] / 100, accuracy = 0.1)
      },
      callout_label = paste0(
        short_county, ", ", state_abbr, "\n(", val_formatted, ")"
      )
    ) |>
    dplyr::select(geoid, extreme_type, callout_label)
}

make_map_panel <- function(
  geography,
  variable,
  limits,
  legend_title,
  is_state_map,
  is_main_panel,
  panel_title = NULL
) {
  fill_labels <- if (variable == "median_household_income") {
    scales::label_dollar(scale = 0.001, suffix = "k", accuracy = 1)
  } else {
    scales::label_percent(scale = 1, accuracy = 1)
  }

  p <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = geography,
      ggplot2::aes(fill = .data[[variable]]),
      color = "#FFFFFF",
      linewidth = if (is_state_map) 0.35 else 0.06
    ) +
    ggplot2::scale_fill_viridis_c(
      option = "D",
      limits = limits,
      breaks = scales::breaks_pretty(n = 6),
      labels = fill_labels,
      oob = scales::squish,
      na.value = "#D9DEE5",
      name = legend_title
    ) +
    ggplot2::coord_sf(datum = NA, expand = TRUE, clip = "off") +
    ggplot2::theme_void(base_size = 10) +
    ggplot2::theme(
      legend.position = if (is_main_panel) "right" else "none",
      plot.title = ggplot2::element_text(
        face = "bold", size = 9.5, color = "#17212B",
        hjust = 0.5, margin = ggplot2::margin(b = 3)
      ),
      plot.margin = ggplot2::margin(8, 8, 8, 8)
    )

  if (is_state_map) {
    # Use an interior point for each state polygon so labels stay inside the
    # correct state. Do not let geom_text silently drop labels in the crowded
    # Northeast (the previous check_overlap setting could hide PA and others).
    label_points <- sf::st_point_on_surface(geography)
    p <- p +
      ggplot2::geom_sf_text(
        data = label_points,
        ggplot2::aes(label = state_abbr),
        size = 3.0,
        color = "#FFFFFF",
        fontface = "bold",
        check_overlap = FALSE
      ) +
      ggplot2::geom_sf_text(
        data = label_points,
        ggplot2::aes(label = state_abbr),
        size = 2.15,
        color = "#17212B",
        fontface = "bold",
        check_overlap = FALSE
      )
  }

  if (!is.null(panel_title)) {
    p <- p + ggplot2::labs(title = panel_title)
  }
  if (!is_main_panel) {
    p <- p + ggplot2::guides(fill = "none")
  }
  p
}

make_us_map <- function(
  map_data,
  variable,
  level,
  title,
  subtitle,
  file,
  limits,
  legend_title,
  n_extremes = 3,
  min_pop_for_label = 0
) {
  is_state_map <- level == "state"
  extreme_counties <- if (!is_state_map && n_extremes > 0) {
    get_extreme_counties(
      map_data,
      variable,
      n_ext = n_extremes,
      min_pop = min_pop_for_label
    )
  } else {
    NULL
  }

  prepare_region <- function(data, fips, crs) {
    regional_data <- data |>
      dplyr::filter(state_fips %in% fips)

    # Keep the inhabited Hawaiian island chain in view. The full state polygon
    # includes remote Northwestern Hawaiian Islands, which otherwise shrink
    # the useful map area in this small inset.
    if (identical(fips, "15")) {
      hawaii_bbox <- sf::st_bbox(
        c(xmin = -161, ymin = 18.8, xmax = -154, ymax = 22.5),
        crs = sf::st_crs(regional_data)
      )
      regional_data <- sf::st_crop(regional_data, hawaii_bbox)
    }

    sf::st_transform(regional_data, crs = crs)
  }

  map_panel <- function(region, crs, panel_title = NULL) {
    fips <- switch(
      region,
      conus = setdiff(valid_state_fips, c("02", "15")),
      alaska = "02",
      hawaii = "15"
    )
    regional_data <- prepare_region(map_data, fips, crs)
    panel <- make_map_panel(
      regional_data,
      variable,
      limits,
      legend_title,
      is_state_map,
      is_main_panel = region == "conus",
      panel_title = panel_title
    )

    if (!is_state_map) {
      regional_state_borders <- prepare_region(
        state_boundaries,
        fips,
        crs
      )
      panel <- panel + ggplot2::geom_sf(
        data = regional_state_borders,
        fill = NA,
        color = "#626C76",
        linewidth = 0.22,
        inherit.aes = FALSE
      )

      # Highlight and label national extremes on the county maps. The callouts
      # are restricted to the current panel, so Alaska and the lower 48 repel
      # labels independently.
      if (!is.null(extreme_counties)) {
        regional_extremes <- regional_data |>
          dplyr::inner_join(extreme_counties, by = "geoid")

        if (nrow(regional_extremes) > 0L) {
          extreme_points <- sf::st_point_on_surface(regional_extremes)
          point_coordinates <- sf::st_coordinates(extreme_points)
          extreme_points_df <- dplyr::bind_cols(
            sf::st_drop_geometry(extreme_points),
            tibble::as_tibble(point_coordinates)
          )

          panel <- panel +
            ggplot2::geom_sf(
              data = regional_extremes,
              fill = NA,
              color = "#111827",
              linewidth = 0.45,
              inherit.aes = FALSE
            ) +
            ggplot2::geom_point(
              data = extreme_points_df,
              ggplot2::aes(x = X, y = Y),
              size = 1.3,
              color = "#111827",
              fill = "#FFFFFF",
              shape = 21,
              stroke = 0.6,
              inherit.aes = FALSE
            ) +
            ggrepel::geom_label_repel(
              data = extreme_points_df,
              ggplot2::aes(
                x = X,
                y = Y,
                label = callout_label,
                color = extreme_type
              ),
              size = 2.4,
              fontface = "bold",
              lineheight = 0.92,
              fill = ggplot2::alpha("#FFFFFF", 0.90),
              label.size = 0.25,
              label.padding = grid::unit(0.18, "lines"),
              box.padding = grid::unit(0.7, "lines"),
              point.padding = grid::unit(0.25, "lines"),
              segment.color = "#1F2937",
              segment.size = 0.35,
              min.segment.length = 0,
              max.overlaps = Inf,
              seed = 42,
              show.legend = FALSE,
              inherit.aes = FALSE
            ) +
            ggplot2::scale_color_manual(
              values = c("Highest" = "#0F5132", "Lowest" = "#4A154B"),
              guide = "none"
            )
        }
      }
    }
    panel
  }

  conus_panel <- map_panel("conus", 5070)
  alaska_panel <- map_panel("alaska", 3338, "Alaska")
  hawaii_panel <- map_panel("hawaii", "ESRI:102007", "Hawaii")

  map <- patchwork::wrap_plots(
    conus = conus_panel,
    alaska = alaska_panel,
    hawaii = hawaii_panel,
    design = "AAB\nAAC",
    widths = c(1, 1, 0.8),
    heights = c(1, 1),
    guides = "collect"
  ) +
    patchwork::plot_annotation(
      title = title,
      subtitle = subtitle,
      caption = paste(
        "Source: U.S. Census Bureau, ACS 2020–2024 5-year estimates; TIGER 2024 boundaries.",
        "Includes 50 states + DC and 3,144 counties/county equivalents; excludes Puerto Rico and territories.",
        "Alaska and Hawaii use separate local projections and insets; the Hawaii inset focuses on the main island chain.",
        if (variable == "median_household_income") {
          "Gray = no published ACS income estimate."
        } else {
          "Employment ratio estimates are available for all mapped units."
        },
        "Each map's color scale spans the observed range for that geography and measure; compare its numeric legend.",
        if (!is_state_map && n_extremes > 0) {
          paste0(
            "Callouts identify the ", n_extremes,
            " highest and lowest county estimates nationwide; read them with ACS sampling error in mind."
          )
        } else {
          ""
        }
      ),
      theme = ggplot2::theme(
        plot.title = ggplot2::element_text(
          face = "bold", size = 17, color = "#17212B",
          margin = ggplot2::margin(b = 4)
        ),
        plot.subtitle = ggplot2::element_text(
          size = 10.5, color = "#5B6470",
          margin = ggplot2::margin(b = 7)
        ),
        plot.caption = ggplot2::element_text(
          hjust = 0, size = 8, color = "#626C76",
          margin = ggplot2::margin(t = 8)
        ),
        legend.position = "right",
        legend.title = ggplot2::element_text(size = 9),
        legend.text = ggplot2::element_text(size = 8.5)
      )
    )

  ggplot2::ggsave(
    file,
    map,
    width = 11,
    height = 8,
    dpi = 300,
    bg = "white"
  )
  invisible(map)
}

make_us_map(
  state_boundaries,
  "median_household_income",
  "state",
  "Household income by U.S. state",
  "State median household income • ACS 2020–2024 5-year estimates",
  "figures/us_state_median_household_income.png",
  state_income_limits,
  "Median household\nincome (2024 $)"
)
make_us_map(
  county_boundaries,
  "median_household_income",
  "county",
  "Household income across U.S. counties",
  "County median household income • ACS 2020–2024 5-year estimates",
  "figures/us_county_median_household_income.png",
  county_income_limits,
  "Median household\nincome (2024 $)"
)
make_us_map(
  state_boundaries,
  "employment_population_ratio_pct",
  "state",
  "Civilian employment-to-population ratio by state",
  "Employed civilians / civilian population age 16+ • ACS 2020–2024",
  "figures/us_state_employment_population_ratio.png",
  state_employment_limits,
  "Employment-to-\npopulation ratio"
)
make_us_map(
  county_boundaries,
  "employment_population_ratio_pct",
  "county",
  "Civilian employment-to-population ratio across counties",
  "Employed civilians / civilian population age 16+ • ACS 2020–2024",
  "figures/us_county_employment_population_ratio.png",
  county_employment_limits,
  "Employment-to-\npopulation ratio"
)

# Summarize how employment attachment and household income move together.
# Every state/county receives equal weight; these are descriptive, unweighted
# cross-sectional correlations, not estimates of a causal effect.
calculate_association <- function(data, geography_level) {
  data_table <- if (inherits(data, "sf")) {
    sf::st_drop_geometry(data)
  } else {
    data
  }
  complete_data <- data_table |>
    dplyr::filter(
      is.finite(median_household_income),
      is.finite(employment_population_ratio_pct)
    )

  tibble::tibble(
    geography = geography_level,
    geography_count = nrow(complete_data),
    pearson_r = stats::cor(
      complete_data$employment_population_ratio_pct,
      complete_data$median_household_income,
      method = "pearson"
    ),
    spearman_rho = stats::cor(
      complete_data$employment_population_ratio_pct,
      complete_data$median_household_income,
      method = "spearman"
    )
  )
}

states_only <- state_data |>
  dplyr::filter(state_fips != "11")

geography_correlations <- dplyr::bind_rows(
  calculate_association(states_only, "50 states"),
  calculate_association(county_data, "counties and county equivalents")
)
readr::write_csv(
  geography_correlations,
  "results/us_geography_correlations.csv"
)

state_income_employment_table <- states_only |>
  dplyr::transmute(
    state_fips,
    state_abbr,
    state_name,
    median_household_income_usd = median_household_income,
    employment_population_ratio_pct
  ) |>
  dplyr::arrange(state_name)
readr::write_csv(
  state_income_employment_table,
  "results/us_state_income_employment_table.csv",
  na = ""
)

make_relationship_plot <- function(data, geography_level, file, title) {
  data_table <- if (inherits(data, "sf")) {
    sf::st_drop_geometry(data)
  } else {
    data
  }
  plot_data <- data_table |>
    dplyr::filter(
      is.finite(median_household_income),
      is.finite(employment_population_ratio_pct)
    )
  association <- calculate_association(plot_data, geography_level)
  subtitle <- sprintf(
    "N = %s geographic units  |  Pearson r = %.2f  |  Spearman ρ = %.2f",
    scales::comma(association$geography_count),
    association$pearson_r,
    association$spearman_rho
  )

  p <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = employment_population_ratio_pct,
      y = median_household_income
    )
  ) +
    ggplot2::geom_smooth(
      method = "lm",
      formula = y ~ x,
      se = TRUE,
      color = "#137C8B",
      fill = "#A8DADC",
      linewidth = 0.9
    ) +
    ggplot2::geom_point(
      color = "#263746",
      alpha = if (geography_level == "50 states") 0.78 else 0.24,
      size = if (geography_level == "50 states") 2.4 else 1.15
    ) +
    ggplot2::scale_x_continuous(
      labels = scales::label_percent(scale = 1, accuracy = 1),
      expand = ggplot2::expansion(mult = c(0.04, 0.08))
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::label_dollar(
        scale = 0.001, suffix = "k", accuracy = 1
      ),
      expand = ggplot2::expansion(mult = c(0.06, 0.10))
    ) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = "Civilian employment-to-population ratio (age 16+)",
      y = "Median household income (2024 dollars)",
      caption = paste(
        "Each geography receives equal weight. The fitted line is descriptive, not causal.",
        "The shaded band is a conventional 95% confidence interval; it does not account for",
        "ACS margins of error or spatial dependence."
      )
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        face = "bold", color = "#17212B", size = 15
      ),
      plot.subtitle = ggplot2::element_text(color = "#5B6470"),
      plot.caption = ggplot2::element_text(
        color = "#626C76", hjust = 0, size = 8.5
      ),
      panel.grid.minor = ggplot2::element_blank()
    )

  if (geography_level == "50 states") {
    labels <- plot_data |>
      dplyr::filter(state_abbr %in% c("MA", "MS", "WV", "UT", "ND", "PA"))

    p <- p + ggrepel::geom_text_repel(
      data = labels,
      ggplot2::aes(label = state_abbr),
      size = 3.1,
      fontface = "bold",
      color = "#17212B",
      box.padding = 0.35,
      point.padding = 0.25,
      min.segment.length = 0,
      max.overlaps = Inf,
      seed = 42,
      inherit.aes = TRUE
    )
  }

  ggplot2::ggsave(
    file,
    p,
    width = 9,
    height = 6,
    dpi = 300,
    bg = "white"
  )
  invisible(p)
}

make_relationship_plot(
  states_only,
  "50 states",
  "figures/us_state_income_employment_scatter.png",
  "Across states, higher employment attachment tends to accompany higher income"
)
make_relationship_plot(
  county_data,
  "counties and county equivalents",
  "figures/us_county_income_employment_scatter.png",
  "County-level income and employment attachment are positively associated"
)

state_rankings <- state_data |>
  dplyr::select(
    geoid, state_fips, state_name, state_abbr,
    median_household_income, income_moe_90,
    employment_population_ratio_pct
  ) |>
  dplyr::arrange(dplyr::desc(median_household_income))
county_rankings <- county_data |>
  dplyr::select(
    geoid, state_fips, state_name, state_abbr, county_name,
    median_household_income, income_moe_90,
    employment_population_ratio_pct
  ) |>
  dplyr::arrange(dplyr::desc(median_household_income))
readr::write_csv(state_rankings, "results/us_state_rankings.csv", na = "")
readr::write_csv(county_rankings, "results/us_county_rankings.csv", na = "")

summary_statistics <- tibble::tibble(
  geography = c("state", "county"),
  geography_count = c(nrow(state_data), nrow(county_data)),
  missing_income_estimates = c(
    sum(is.na(state_data$median_household_income)),
    sum(is.na(county_data$median_household_income))
  ),
  median_household_income_usd = c(
    stats::median(state_data$median_household_income, na.rm = TRUE),
    stats::median(county_data$median_household_income, na.rm = TRUE)
  ),
  median_employment_population_ratio_pct = c(
    stats::median(state_data$employment_population_ratio_pct, na.rm = TRUE),
    stats::median(county_data$employment_population_ratio_pct, na.rm = TRUE)
  )
)
readr::write_csv(summary_statistics, "results/us_summary_statistics.csv")

message(
  "Finished: ", nrow(state_data), " states/DC and ", nrow(county_data),
  " counties/county equivalents; missing county income estimates = ",
  sum(is.na(county_data$median_household_income)), "."
)
