library(pacman)

p_load(tidyverse)
p_load(stringr)
p_load(scales)
p_load(xml2)

column_names = c(
  year = "year",
  tco2 = "co2.total",
  pCO2 = "co2.atmos",
  alk = "alkalinity.ocean",
  d13Cocn = "delta.13C.ocean",
  d13Catm = "delta.13C.atmos",
  CO3 = "carbonate.ocean",
  WeatC = "carbonate.weathering",
  WeatS = "silicate.weathering",
  TotW = "total.weathering",
  BurC = "carbon.burial",
  Degas = "degassing.rate",
  Tatm = "temp.atmos",
  Tocn = "temp.ocean"
)

column_descr = c(
  year = "year",
  tco2 = "Total CO2",
  pCO2 = "Atmospheric CO2",
  alk = "Ocean Alkalinity",
  d13Cocn = "Delta 13C (ocean)",
  d13Catm = "Delta 13C (atmosphere)",
  CO3 = "Ocean carbonate concentration",
  WeatC = "Carbonate Weathering Rate",
  WeatS = "Silicate Weathering Rate",
  TotW = "Total Weathering Rate",
  BurC = "Carbon Burial Rate",
  Degas = "Degassing Rate",
  Tatm = "Atmospheric Temperature",
  Tocn = "Ocean Temperature"
)

columns = tibble(
  index = names(column_names),
  name = column_names) %>%
  full_join(
    tibble(index = names(column_descr),
           description = column_descr),
    by = "index"
  )


run_geocarb = function(filename, co2_spike,
                       degas_spinup = 7.5,
                       degas_sim = 7.5,
                       plants_spinup = TRUE,
                       plants_sim = TRUE,
                       land_area_spinup = 1,
                       land_area_sim = 1,
                       delta_t2x = 3.0,
                       million_years_ago = 0,
                       mean_latitude_continents = 30) {
  url = str_c("http://climatemodels.uchicago.edu/cgi-bin/geocarb/geocarb.cgi?",
              str_c(
                c('year', 'co2_1', 'co2_2',
                  'dt2x', 'latitude',
                  'plnt_1', 'plnt_2',
                  'lnd_1', 'lnd_2', 'spike'),
                c(million_years_ago,
                  degas_spinup, degas_sim,
                  delta_t2x, mean_latitude_continents,
                  as.integer(plants_spinup), as.integer(plants_sim),
                  land_area_spinup, land_area_sim, co2_spike),
                sep = "=", collapse = "&"
              ))
  results = read_html(url)
  body <- as_list(results) %>% unlist() %>% simplify()
  write(body, filename)
  lines = body %>% str_split("\n") %>% unlist()
  lines %>% str_trim() %>% str_replace_all('[ \t]+', ',') %>%
    str_c(collapse = "\n") %>% read_csv(na = c('NA', 'NaN')) -> df
  names(df) <- column_names[names(df)]
  invisible(df)
}

read_geocarb <- function(filename) {
  f <- file(filename,"r")
  lines <- readLines(f, warn=F)
  close(f)
  lines %>% str_trim() %>% str_replace_all('[ \t]+', ',') %>%
    str_c(collapse = "\n") %>% read_csv() -> df
  names(df) <- column_names[names(df)]
  invisible(df)
}
