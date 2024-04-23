
apiTrusts = readr::read_csv(here::here("data-raw/hospitalCases_nhsTrust_2021.csv"))
usethis::use_data(apiTrusts)
