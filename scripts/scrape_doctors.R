
scraper <- modules::use('R/scraper.R')
driver <- scraper$start_selenium_firefox()
urls <- readr::read_rds('data/urls.rds')
doctors <- scraper$fetch_all_doctors_data(driver$client, urls)
readr::write_rds(doctors, 'data/cpso_active_family_doctors.rds')
driver$server$stop()
beepr::beep(2)

doctors |> 
  tibble::glimpse()


urls |> tibble::glimpse()

doctors |> 
  dplyr::filter(is.na(name) | is.na(gender))
