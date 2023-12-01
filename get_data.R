library("nasapower")
# help("get_power") # montre la vignette de la fonction

# Récupérer les variables d'intérêt Tmin, Tmax, Tmean
daily_single_ag <- get_power(
  community = "ag",
pars = c("T2M_MIN", "T2M_MAX", "T2M"),
  lonlat = c(151.81, -27.48),
  temporal_api = "daily",
  dates = c("1985-01-01", "2023-01-31")
)
