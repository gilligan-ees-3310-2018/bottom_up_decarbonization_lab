#
#  EES 3310 Lab Utility Scripts
#
library(stringr)

sb_const <- 5.6704E-8
sigma_sb <- sb_const
zero_celsius <- 273.15 # K

base_methane_ppm <- 1.7 # parts per billion
base_co2_ppm <- 400 # parts per million

solar_constant <- 1350 # W/m^2
r_earth <- 6.378E+6 # meters

albedo_earth <- 0.30

avg_environ_lapse <- 6 # K/km
dry_adiabatic_lapse <- 10

bare_rock <- function(solar_const = solar_constant, albedo = albedo_earth, au = 1, emissivity = 1) {
  # default solar constant is for earth.
  # scale to other planets by 1 / r^2, where R is the average radius of the orbit
  # around the sun, in astronomical units. R_earth is 1 AU
  solar_const <-   solar_const / au^2
  absorbed_shortwave <- (1 - albedo) * solar_const / 4
  temperature <- (absorbed_shortwave / (emissivity * sb_const))^0.25
  temperature
}

# Functions for converting temperatures

ftoc <- function(x) {
  (x - 32) * 5./9.
}

ctof <- function(x) {
  x * 9./5. + 32.
}

ktoc <- function(x) {
  x - zero_celsius
}

ctok <- function(x) {
  x + zero_celsius
}

ftok <- function(x) {
  ctok(ftoc(x))
}

ktof <- function(x) {
  ctof(ktoc(x))
}
