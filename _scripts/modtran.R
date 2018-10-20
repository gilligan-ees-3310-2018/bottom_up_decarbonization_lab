library(pacman)
p_load(tidyverse, scales, xml2)

ssource <- function(filename, chdir = F) {
  if(!file.exists(filename)) {
    if(exists("script_dir")) script_dir = script_dir
    else if (exists("script.dir")) script_dir = script.dir
    else if (dir.exists("scripts")) script_dir = "scripts"
    if(dir.exists(script_dir)) filename = file.path(script_dir, filename)
  }
  source(filename, chdir = chdir,
         local = FALSE, echo=FALSE, print.eval = FALSE, verbose = FALSE)
}

if (! exists("tbr_earth")) ssource("utils.R", chdir = T)
if (! exists("planck")) ssource("planck.R", chdir = T)



model_params = data_frame(
  key = c("co2_ppm",
          "ch4_ppm",
          "trop_o3_ppb",
          "strat_o3_scale",
          "h2o_scale",
          "freon_scale",
          "delta_t",
          "h2o_fixed",
          "atmosphere",
          "clouds",
          "altitude_km",
          "looking"
  ),
  cgi = c("pco2", "ch4", "trop_o3",
          "strat_o3", "h2orat", "scalefreon",
          "Toffset",
          "h2otscaled",
          "model", "icld",
          "altitude", "i_obs"),
  descr = c("CO2 concentration (ppm)",
            "Methane concentration (ppm)",
            "Tropospheric ozone concentration (ppb)",
            "Stratospheric ozone scale",
            "Water vapor scale",
            "Freon scale",
            "Temperature offset (C)",
            "Water vapor specification",
            "Locality",
            "Cloud/Rain",
            "Observation altitude",
            "Observation direction"),
  spec = c( NA, NA, NA,
            NA, NA, NA,
            NA,
            "h2o_fixed",
            "atmos_spec", "cloud_spec",
            NA, "sensor_orientation")
)

atmos_spec <- data_frame(
  key = c("tropical",
          "midlatitude summer", "midlatitude winter",
          "subarctic summer", "subarctic winter",
          "standard", "standard atmosphere",
          "us standard atmosphere", "u s standard atmosphere",
          "1976 standard atmosphere",
          "1976 us standard atmosphere", "1976 u s standard atmosphere"
  ),
  value = c(1, 2, 3, 4, 5, rep(6, 7)),
  descr = c("Tropical",
            "Midlatitude Summer", "Midlatitude Winter",
            "Subarctic Summer", "Subarctic Winter",
            rep("1976 U.S. Standard Atmosphere", 7)
  )
)

h2o_fixed <- data_frame(
  key = c("vapor pressure", "relative humidity"),
  value = c(0, 1),
  descr = c("constant vapor pressure", "constant relative humidity")
)

cloud_spec <- data_frame(
  key = c("none",
          "cumulus",
          "altostratus",
          "stratus",
          "stratocumulus",
          "nimbostratus",
          "drizzle",
          "light rain",
          "moderate rain",
          "heavy rain",
          "extreme rain",
          "standard cirrus",
          "subvisual cirrus",
          "NOAA cirrus"),
  value = c(0:10, 18:20),
  descr = c("no clouds or rain",
            "cumulus cloud base .66km top 2.7km",
            "altostratus cloud base 2.4km top 3.0km",
            "stratus cloud base .33km top 1.0km",
            "stratus/strato cu base .66km top 2.0km",
            "nimbostratus cloud base .16km top .66km",
            "2 mm/hr drizzle (modeled with stratus cloud)",
            "5.0mm/hr light rain (modeled with nimbostratus)",
            "12.5mm/hr moderate rain (modeled with nimbostratus)",
            "25.0mm/hr heavy rain (modeled with cumulus)",
            "75.0mm/hr extreme rain (modeled with cumulus)",
            "standard cirrus model",
            "sub-visual cirrus model",
            "NOAA cirrus model")
)

sensor_orientation <- data_frame(
  key = c("down", "up"),
  value = c(180, 0),
  descr = c("looking down", "looking up")
)

run_modtran <- function(filename = NULL,
                        co2_ppm = 400,
                        ch4_ppm = 1.7,
                        trop_o3_ppb = 28,
                        strat_o3_scale = 1.0,
                        h2o_scale = 1.0,
                        freon_scale = 1.0,
                        delta_t = 0.0,
                        h2o_fixed = get("h2o_fixed", 1)$key,
                        atmosphere = get("atmos_spec", 1)$key,
                        clouds = get("cloud_spec", 1)$key,
                        altitude_km = 70,
                        looking = get("sensor_orientation", 1)$key
)
{
  values <- list(co2_ppm, ch4_ppm, trop_o3_ppb,
                 strat_o3_scale, h2o_scale, freon_scale,
                 delta_t, h2o_fixed,
                 atmosphere, clouds,
                 altitude_km, looking) %>%
    set_names(c("co2_ppm", "ch4_ppm", "trop_o3_ppb",
                "strat_o3_scale", "h2o_scale", "freon_scale",
                "delta_t", "h2o_fixed",
                "atmosphere", "clouds",
                "altitude_km", "looking")) %>%
    map(~as.character(.x[1])) %>%
    modify_at("atmosphere", ~str_to_lower(.x) %>%
                str_replace_all(c("[^a-z0-9]+" = " ", "  +" = " ")) %>%
                str_trim()
    ) %>%
    simplify()
  for(k in c('h2o_fixed', 'atmosphere', 'clouds', 'looking')) {
    # message("Looking up ", k)
    lookup = model_params %>% filter(key == k) %>% select(spec) %>% simplify()
    # message("Lookup = ", lookup)
    lookup = get(lookup, 1)
    # message("Lookup class = ", class(lookup), ", type = ", typeof(lookup), ", dim = ", dim(lookup))
    values[k] = lookup %>% filter(key == values[k]) %>% select(value) %>% simplify()
  }
  args <- data_frame(key = names(values), value = values)
  params = model_params %>% inner_join(args, by = "key")
  url_base = 'http://climatemodels.uchicago.edu/cgi-bin/modtran/modtran.cgi?'
  args = str_c(params$cgi, params$value, sep = "=", collapse = "&")
  args = str_c(args, "i_save=0", sep="&")
  url = paste0(url_base, args)
  # message(url)
  output = read_html(url)
  body <- as_list(output) %>% unlist() %>% simplify()
  # lines <- body %>% str_split("\n") %>% unlist() %>% simplify()
  if (! is.null(filename)) {
    write(body, filename)
  }


  output <- str_c(body, collapse = "\n") %>% read_modtran(text = .)
  invisible(output)
}


read_modtran_profile <- function(filename = NULL, text = NULL) {
  if (! is.null(filename) && ! is.na(filename)) {
    if (isTRUE(any(str_detect(filename, "\n"))) || length(filename) > 1) {
      text = filename
      filename = NULL
    }
  }
  if (is.null(filename) && ! is.null(text)) {
    lines <- text %>% str_split("\n") %>% unlist()
  } else {
    f <- file(filename,"r")
    lines <- readLines(f, warn=F)
    close(f)
  }
  im <- str_detect(lines, "^ +ATMOSPHERIC PROFILES") %>% which()
  header <- im[[1]] + 2
  start <- im[[1]] + 5
  end <- im[[2]] - 2
  col_names <- lines[[header]] %>% str_trim() %>% str_split(" +") %>% unlist() %>%
    str_replace_all("([^a-zA-Z0-9_]+)", ".") %>%
    str_replace_all(c('^\\.' = '', '\\.$' = ''))
  dups <- duplicated(col_names) %>% which()
  col_names[dups] <- col_names[dups] %>% str_c(seq_along(dups), sep = ".")
  profile <- lines[start:end] %>% str_trim() %>% str_c(collapse = "\n") %>%
    read_table2(col_names=col_names[1:10])
  profile <- profile[,2:4]
  names(profile) <- col_names[2:4]
  profile
}

extract_tropopause <- function(profile) {
  profile %>% filter(T <= lead(T)) %>% top_n(-1, Z)
}

read_modtran <- function(filename = NULL, text = NULL, scale_factor = 3.14E+4) {
  if (! is.null(filename) && ! is.na(filename)) {
    if (isTRUE(any(str_detect(filename, "\n"))) || length(filename) > 1) {
      text = filename
      filename = NULL
    }
  }
  if (is.null(filename) && ! is.null(text)) {
    lines <- text %>% str_split("\n") %>% unlist()
  } else {
    f <- file(filename,"r")
    lines <- readLines(f,warn=F)
    close(f)
  }
  atmos_index <- str_detect(lines, "ATMOSPHERIC MODEL") %>% which()
  atmos_spec <- str_match(lines[atmos_index + 1],
                          "^ +TEMPERATURE += +[0-9]+ +(?<atmos>[A-Za-z0-9].+)$") %>%
    { .[1,2] } %>% str_trim()

  profile <- read_modtran_profile(text = lines)
  tropopause <- extract_tropopause(profile)
  t_ground = profile$T[1]

  im <- str_detect(lines, "^0INTEGRATED RADIANCE") %>% which()
  target <- lines[im[1]]
  x <- str_extract(target, "[0-9]\\.[0-9]*E[+-][0-9]+")
  integrated_radiance <- pi * 1E+4 * as.numeric(x)
  radiance_lines <- str_detect(lines, "^1\\s+RADIANCE") %>% which()
  im <- str_detect(lines, fixed(" co2mx co2rat ch4rat h2orat t_o3rat s_o3rat")) %>%
    which()
  target <- lines[im[1]]
  x <- target %>% str_trim() %>% str_split(" +") %>% unlist()
  x <- x[8:14] %>% map_dbl(~as.numeric(.x)) %>% set_names(x[1:7])
  co2 <- x['co2mx']
  ch4 <- x['ch4rat'] * 1.7
  im <- str_detect(lines, fixed("0 SLANT PATH TO SPACE")) %>% which()
  target <-lines[[im + 1]]
  x <- str_match(target, "H1\\s*=\\s*([0-9]+.[0-9]*(E[+-][0-9]+)?) ")
  altitude <- as.numeric(x[1,2])
  target <- lines[[im + 3]]
  x <- str_match(target, "ANGLE\\s*=\\s*([0-9]+.[0-9]*(E[+-][0-9]+)?) ")
  angle <- as.numeric(x[1,2])
  if (angle == 180.0) {
    direction = "down"
  } else if (angle == 0.0) {
    direction = "up"
  } else {
    direction = NA
  }
  direction <- factor(direction, levels=c("up","down"))

  lx <- lines[-(1:(min(radiance_lines) + 2))]
  x <- str_detect(lx, "^   ")
  lx <- lx[x] %>% str_c(collapse = "\n")
  spectrum <- read_table(lx, col_names =
                           c("k","lambda","pk","pl","sk","sl","srk","srl",
                             "tk","tl","int","trans"))
  spectrum$tk <- spectrum$tk * scale_factor
  spectrum$tl <- spectrum$tl * scale_factor
  invisible(list(spectrum=spectrum,
                 co2=co2, ch4=ch4,
                 i_out = integrated_radiance,
                 alt = altitude, sensor_direction = direction,
                 profile = profile,
                 h_tropo = tropopause$Z,
                 t_tropo = tropopause$T,
                 t_ground = t_ground,
                 atmosphere = atmos_spec))
}

plot_modtran <- function(filename = NULL, text = NULL,
                         modtran_data = NULL,
                         descr = NULL, i_out_ref = NA,
                         last_i_out = NA, delta_t = NA,
                         tmin=220, tmax = 300,
                         nc = 5, max_color = 0.8,
                         delta_t_digits = 2,
                         annotate_x_1 = 100, annotate_x_2 = 1200,
                         annotate_y_1 = 0.49, annotate_y_2 = 0.44,
                         annotate_size = 5, text_size = 10,
                         legend_text_size = 10, legend_size = 0.2,
                         line_scale = 1, direction = "out") {
  if (! is.null(modtran_data)) {
    x <- modtran_data
  } else if (! is.null(filename) && is.list(filename) &&
             "spectrum" %in% names(filename)) {
    x <- filename
  } else {
    x  <- read_modtran(filename, text)
  }
  spectrum <- x$spectrum
  alt <- x$alt
  co2 <- x$co2
  i_out <- x$i_out
  k_limits <- c(100, 1500)

  # if (! is.na(delta_t) && (delta_t %in% c(TRUE, 'auto'))) {
  #   delta_t = x$delta_t
  # }

  if (is.null(descr)) {
    descr <- bquote(.(co2) * " ppm " * CO[2] * ", " * .(alt) * " km altitude")
  }

  if (! is.na(i_out_ref)) {
    delta_i <- i_out - i_out_ref
  } else {
    delta_i <- NA
  }

  dt <- (tmax - tmin) / (nc - 1)
  dh <- max_color / (nc - 1)
  tlist <- tmin + dt * seq(0,nc-1)
  hues <- c(hsv(max_color - (dh * seq(0, nc - 1 )), 0.9, 0.8), "black")

  thermal <- data.frame(k = spectrum$k, t = tmin)
  thermal <- bind_rows(thermal, map(seq(tmin + dt, tmax, dt),
                                    ~data_frame(k = spectrum$k, t = .x)))
  thermal <- thermal %>%
    mutate(tk = planck(k, as.numeric(as.character(t)),fudge_factor=1),
           t = paste(t, "K") %>%
             ordered(., levels = c("MODTRAN", sort(unique(.), decreasing = TRUE)))
    ) %>% na.omit() %>% filter(between(k, k_limits[1], k_limits[2]))

  lambda = c(1, 2, 2.5, 3, 3.5, 4, 5:10, 12, 14, 17, 20, 25, 30, 35, 40, 50, 100)

  spectrum <- spectrum %>% select(k, tk) %>% na.omit() %>%
    filter(between(k, k_limits[1], k_limits[2]))

  p1 <- ggplot(spectrum, aes(x=k,y=tk)) +
    geom_line(aes(color="MODTRAN"), size=I(line_scale)) +
    labs(x = expression(paste("Wavenumber (", cm^-1, ")")),
         y = expression(paste("Intensity (", W/m^2 * cm^-1, ")")),
         title = bquote("MODTRAN: " * .(descr))
    ) +
    scale_x_continuous(limits=k_limits,
                       sec.axis = sec_axis(~ ., breaks = 1E4 / lambda,
                                           labels = as.character(lambda),
                                           name = expression(paste("Wavelength ", (mu * m)))))

  p1 <- p1 + geom_line(aes(x=k,y=tk * 3.14E+2, color = t),
                       data=thermal, size=I(0.5 * line_scale)) +
    scale_color_manual(values=hues, breaks=levels(thermal$t), name=NULL)

  caption <- paste("I[", direction, "] == ",
                   formatC(i_out, digits=2, format="f"))
  if (! is.na(annotate_size)) {
    p1 <- p1 + annotate("text", x=annotate_x_1, y=annotate_y_1,
                        label=caption, parse="TRUE", hjust=0, vjust=1,
                        size=annotate_size, color="dark blue")

    if (! is.na(delta_i)) {
      caption <- paste("Delta * I[", direction, "] == ",
                       formatC(delta_i, digits=2, format="f"))
      p1 <- p1 + annotate("text", x=annotate_x_2, y=annotate_y_1,
                          label=caption, parse="TRUE", hjust=1, vjust=1,
                          size=annotate_size, color="dark blue")
    }

    if (! is.na(last_i_out)) {
      caption <- paste("change == ",formatC(i_out - last_i_out,
                                            digits=2, format="f"))
      p1 <- p1 + annotate("text", x=annotate_x_2, y=annotate_y_2,
                          label=caption, parse="TRUE", hjust=1, vjust=1,
                          size=annotate_size, color="dark blue")
    } else if (! is.na(delta_t)) {
      caption <- paste("Delta * T[ground] == ",formatC(delta_t,
                                                       digits=delta_t_digits,
                                                       format="f"),
                       " * K")
      p1 <- p1 + annotate("text", x=annotate_x_2, y=annotate_y_2,
                          label=caption, parse="TRUE", hjust=1, vjust=1,
                          size=annotate_size, color="dark blue")

    }
  }

  p1 <- p1 + theme_bw(base_size = text_size) +
    theme(panel.grid.major=element_line(size=0.1,color="gray85"),
          panel.grid.minor=element_line(size=0.1,color="gray95"),
          legend.key.size = unit(legend_size,"npc"),
          legend.text = element_text(size=legend_text_size),
          legend.position = c(1,1), legend.justification = c(1,1),
          legend.key.height = unit(1, "lines"), legend.key.width = unit(2, "lines"),
          legend.background = element_rect(color = "black", fill = "white"))

  p1
}
