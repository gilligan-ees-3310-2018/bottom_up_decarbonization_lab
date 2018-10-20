planck <- function(k,t, fudge_factor = 1.0E+5) {
  # fudge_factor = 1.0E+5 # normalize intensity to units of spectrum.
  # see getnim.f
  h = 6.62606957E-34
  c = 2.99792458E8
  kb = 1.3806488E-23
  km = k * 100 # cm-1 to m-1
  return( fudge_factor * 2 * h * c^2 * km^3 / (exp(h*c*km/(kb * t)) - 1))
}
