library(pacman)
p_load(tidyverse, jsonlite, httr, magrittr)

# rrtm_params <- read_rds(file.path(find_rstudio_root_file("_data"),
#                                   "params.Rds"))

make_rds_filename <- function(s) {
  if (! str_detect(s, "\\.[^.]")) { # check for extension
    s <- s %>% str_replace("\\.+$", "") %>% # strip trailing "."
      str_c(".Rds")
  }
  s
}

closest_layer_index <- function(altitude, alts) {
  a_prev <- 0
  index  <- 0
  for (i in seq_along(alts)) {
    this_a <- alts[i]
    if (altitude < (a_prev + this_a) / 2.0) {
      break
    }
    a_prev <- this_a
    index  <- i
  }
  index
}

read_rrtm <- function(file = NULL, data = NULL) {
  if (! is.null(file)) {
    file <- make_rds_filename(file)
    data <- read_rds(file)
  }

  stratus_index <- closest_layer_index(1.0, data$altitude)
  cirrus_index  <- closest_layer_index(data$tropopause, data$altitude)

  res_out <- with(data,list(
    T_surface = Ts, Q = net_toa,
    co2_ppm = co2, ch4_ppm = ch4, I_solar = scon,
    i_in = SwToa, i_out = -LwToa,
    relative_humidity = relativeHumidity,
    lapse_rate = lapseRate, tropopause_km = tropopause,
    albedo = asdir, cloud_drop_radius = r_liq[1],
    low_cloud_frac = cldf[stratus_index],
    high_cloud_frac = cldf[cirrus_index],
    aerosols = aerosols,
    profile = tibble(altitude = altitude, T = T, P = lev),
    fluxes = tibble(altitude = c(0, altitude), T = c(Ts, T), P = c(ps, lev),
                    sw_up = swuflx, sw_down = swdflx,
                    lw_up = lwuflx, lw_down = lwdflx,
                    total_up = uflx, total_down = dflx
    )
  ))

  invisible(res_out)
}

run_rrtm <- function(file = NULL, I_solar = 1360.0, T_surface = 284.42,
                     lapse_rate = 6.0, tropopause_km = 15.0,
                     co2_ppm = 400.0, ch4_ppm = 1.7, relative_humidity = 80.0,
                     low_cloud_frac = 0.0, high_cloud_frac = 0.0,
                     cloud_drop_radius = 10.0,
                     surface_type = "earth's average",
                     aerosols = "none",
                     albedo = NULL) {

  rrtm_url <- "http://climatemodels.uchicago.edu/cgi-bin/rrtm/rrtm.py"

  surface_table = tribble(
    ~key, ~descr,
    "earth", "Earth's average",
    "earth average", "Earth's average",
    "earths average", "Earth's average",
    "earth s average", "Earth's average",
    "average", "Earth's average",
    "default", "Earth's average",
    "asphalt", "Asphalt",
    "concrete", "Concrete",
    "desert", "Desert",
    "forest", "Forest",
    "grass", "Grass",
    "ocean", "Ocean",
    "snow", "Snow",
    "ice", "Ice",
    "soil", "Soil",
    "dirt", "Soil",
    "bare earth", "Soil",
    "custom", "Custom albedo"
  ) %>% full_join(
    tribble(
      ~descr, ~albedo,
      "Earth's average", 0.30,
      "Asphalt", 0.08,
      "Concrete", 0.55,
      "Desert", 0.40,
      "Forest", 0.15,
      "Grass", 0.25,
      "Ice", 0.60,
      "Ocean", 0.10,
      "Snow", 0.85,
      "Soil", 0.17,
      "Custom albedo", NA
    ), by = "descr")

  aerosol_table = tribble(
    ~key, ~descr,
    "none", "No aerosols",
    "no aerosols", "No aerosols",
    "default", "No aerosols",
    "ocean", "Ocean",
    "desert", "Desert",
    "city", "City",
    "city sulfates", "City, just sulfates",
    "city sulphates", "City, just sulfates",
    "city just sulfates", "City, just sulfates",
    "city just sulphates", "City, just sulfates",
    "city soot", "City, just black carbon",
    "city carbon", "City, just black carbon",
    "city black carbon", "City, just black carbon",
    "city just soot", "City, just black carbon",
    "city just carbon", "City, just black carbon",
    "city just black carbon", "City, just black carbon",
    "land", "Land",
    "polluted land", "Land, polluted",
    "land polluted", "Land, polluted",
    "antarctic", "Antarctic",
    "antarctica", "Antarctic",
    "volcano", "Volcano (Pinatubo)",
    "pinatubo", "Volcano (Pinatubo)",
    "volcano pinatubo", "Volcano (Pinatubo)"
  ) %>% full_join(
    tribble(
      ~descr, ~value,
      "No aerosols", "no aerosols",
      "Ocean", "ocean",
      "Desert", "desert",
      "City", "city",
      "City, just black carbon", "carbon",
      "City, just sulfates", "sulfates",
      "Land", "land",
      "Land, polluted", "polluted",
      "Antarctic", "Antarctic",
      "Volcano (Pinatubo)", "Pinatubo"
    ), by = "descr")

  aerosol_clean <- aerosols %>%
    str_replace_all(c("'" = "", "[^a-zA-Z' ]+" = " ", "  +" = " ")) %>%
    str_trim() %>% str_to_lower()

  surface_clean <- surface_type %>%
    str_replace_all(c("'" = "", "[^a-zA-Z' ]+" = " ", "  +" = " ")) %>%
    str_trim() %>% str_to_lower()

  surface_albedo <- surface_table %>% filter(key == surface_clean) %$% albedo
  if (is.na(surface_albedo)) {
    if (is.null(albedo)) {
      stop("ERROR: If you specify a custom albedo for the surface, you must also supply a value for the albedo.")
    }
    surface_albedo = albedo
  }

  if (length(surface_albedo) != 1) {
    stop("Illegal value for surface_type (", surface_type, ") in run_rrtm()")
  }

  aerosol_key = aerosol_table %>% filter(key == aerosol_clean) %$% value

  if (length(aerosol_key) != 1) {
    stop("Illegal value for aerosol (", aerosols, ") in run_rrtm()")
  }

  n_layers = 51

  params <- list(
    Ts = T_surface, scon = I_solar,
    co2 = co2_ppm, ch4 = ch4_ppm,
    relativeHumidity = relative_humidity,
    lapseRate = lapse_rate, tropopause = tropopause_km,
    r_liq = rep_len(cloud_drop_radius, n_layers),
    aerosols = aerosol_key,
    asdir = surface_albedo
  )

  stratus_index <- closest_layer_index(1.0, params$altitude)
  cirrus_index <- closest_layer_index(params$tropopause, params$altitude)

  params$cldf <- rep_len(0, n_layers)
  params$cldf[stratus_index] <- low_cloud_frac
  params$cldf[cirrus_index]  <- high_cloud_frac

  stratus_index <- closest_layer_index(1.0, params$altitude)
  cirrus_index <- closest_layer_index(params$tropopause, params$altitude)

  params$cldf <- rep_along(params$cldf, 0)
  params$cldf[stratus_index] <- low_cloud_frac
  params$cldf[cirrus_index]  <- high_cloud_frac

  result <- POST(url = rrtm_url, body = params, encode = "json")
  if (result$status_code != 200) {
    warning("ERROR: could not communicate with RRTM model server. Code = ",
            result.Status)
    return(NULL)
  }
  content <- result$content
  if (length(content) <= 10) {
    warning("ERROR: no results from RRTM model server.")
    return(NULL)
  }
  res <- content %>% rawToChar() %>% fromJSON()
  res$aerosols = aerosols

  if (!is.null(file) && ! is.na(file)) {
    write_rds(res, path = make_rds_filename(file))
  }

  res_out <- read_rrtm(data = res)

  invisible(res_out)
}

plot_heat_flows <- function(rrtm, sw = TRUE, lw = TRUE, total = TRUE,
                            text_size = 10) {
  criteria = character(0)
  if (sw) criteria = c(criteria, "SW")
  if (lw) criteria = c(criteria, "LW")
  if (total) criteria = c(criteria, "Total")
  criteria = ordered(criteria, levels = c("Total", "SW", "LW"))

  data <- rrtm$fluxes %>%
    gather(-altitude, key = var, value = val) %>%
    filter(! var %in% c("T", "P")) %>%
    mutate(lambda = ifelse(str_detect(var, "total"), "Total",
                           ifelse(str_detect(var, "sw_"), "SW", "LW")) %>%
             ordered(levels = c("Total", "SW", "LW")),
           dir = ifelse(str_detect(var, "up"), "Up", "Down") %>%
             ordered(levels = c("Down", "Up"))) %>%
    filter(lambda %in% criteria)

    ggplot(data, aes(x = val, y = altitude, color = lambda, size = dir)) +
    geom_line() +
    scale_color_manual(values = c(SW = "#B0B040", LW = "#D00000",
                                  Total = "purple"), name = "Wavelength") +
    scale_size_manual(values = c(Down = .5, Up = 1), name = "Direction") +
    guides(color = guide_legend(override.aes = list(size = 2))) +
    labs(x = expression(paste("Intensity ", (W/m^2))), y = "Altitude (km)") +
    theme_bw(base_size = text_size)
}
