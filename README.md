# U.S. Income, Employment, and Geography

This project supports a GIS blog post comparing median household income with the civilian employment-to-population ratio across U.S. states and counties. The article uses maps to show where each measure is concentrated, then uses scatterplots and correlations to describe whether higher-employment geographies also tend to have higher household incomes.

## Research question

**Across U.S. states and counties, are higher civilian employment-to-population ratios associated with higher median household incomes, and where do the two measures cluster geographically?**

The analysis is descriptive. Correlations are unweighted across geographic units and do not identify a causal effect. “Income” means ACS median household income, not an arithmetic mean or an individual worker's wage.

## Project structure

```text
us-income-employment.Rproj
README.md
R/
└── 01_build_analysis.R             # downloads/processes data and creates outputs
blog/
└── post.qmd                        # Quarto article; renders to blog/post.html
data/
├── raw/                            # cached ACS and TIGER/Line API responses
└── derived/
    ├── us_state_acs_2020_2024.csv
    └── us_county_acs_2020_2024.csv
figures/
├── us_state_median_household_income.png
├── us_county_median_household_income.png
├── us_state_employment_population_ratio.png
├── us_county_employment_population_ratio.png
├── us_state_income_employment_scatter.png
└── us_county_income_employment_scatter.png
results/
├── us_labor_market_maps.gpkg
├── us_state_rankings.csv
├── us_county_rankings.csv
├── us_summary_statistics.csv
├── us_geography_correlations.csv
└── us_state_income_employment_table.csv
```

Maps include the 50 states and Washington, DC. The county layer contains 3,144 counties and county equivalents across these areas; territories are excluded. The state table and state correlation use the 50 states and exclude DC.

## Reproduce the analysis

1. Install R and [Quarto](https://quarto.org/docs/get-started/), then open `us-income-employment.Rproj` in RStudio.
2. Install the required R packages once:

   ```r
   install.packages(c(
     "dplyr", "ggplot2", "ggrepel", "httr2", "jsonlite", "knitr",
     "patchwork", "purrr", "readr", "scales", "sf", "stringr", "tibble"
   ))
   ```

3. From the project root, run the analysis script:

   ```r
   source("R/01_build_analysis.R")
   ```

4. Render the article to HTML:

   ```sh
   quarto render blog/post.qmd
   ```

The first run needs an internet connection to retrieve Census Reporter ACS responses and TIGER/Line geometry. The script caches downloads in `data/raw/`, then writes all processed data, maps, plots, tables, correlations, and the GeoPackage programmatically. Subsequent runs reuse the cached source files. Delete a cached file only when you want the script to download that source again.

## Measures and spatial methods

| Measure | Construction | Interpretation |
|---|---|---|
| Median household income | ACS B19013, inflation-adjusted 2024 dollars | Household income median; not an individual wage or mean |
| Civilian employment-to-population ratio | ACS B23025 employed civilians ÷ (population age 16+ − Armed Forces) × 100 | Share of the civilian age-16+ population that is employed; not the unemployment rate |
| State/county relationship | Pearson and Spearman correlations across geographic units | Unweighted descriptive association; ACS margins of error are not incorporated |

ACS estimates are joined to TIGER/Line polygons using Census GEOIDs. Maps use EPSG:5070 for the lower 48, EPSG:3338 for Alaska, and ESRI:102007 for Hawaii in separate insets. Each choropleth uses its own numeric scale; compare values using the legend, not color alone. Polygon area represents land area rather than population.

The county maps call out the three highest and lowest published estimates. No population cutoff is applied, so small places may appear among the extremes. ACS estimates have sampling error; these labels are descriptive and should not be read as precise rankings. One county without a published median-income estimate is shown in gray, which means missing data rather than zero income.

## Data sources

- [2020–2024 ACS five-year estimates release](https://www.census.gov/programs-surveys/acs/news/data-releases/2024/release.html)
- [ACS table B19013: median household income](https://api.census.gov/data/2024/acs/acs5/variables/B19013_001E.html)
- [ACS table B23025: employment status](https://api.census.gov/data/2024/acs/acs5/groups/B23025.html)
- [2024 TIGER/Line files](https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.2024.html)
- [Census Reporter API documentation](https://github.com/censusreporter/census-api/blob/master/API.md)
