# do scraping for all currently-active family doctors 

scraper <- modules::use('R/scraper.R')
driver <- scraper$start_selenium_firefox()

urls <- scraper$fetch_all_doctors_urls(
  remote = driver$client, 
  include_inactive = F, 
  family_only = T
)

# readr::write_rds(urls, here::here('./data/family_doctor_urls.rds'))
beepr::beep(1)

urls <- readr::read_rds(here::here('./data/family_doctor_urls.rds'))
doctors <- scraper$fetch_all_doctors_data(driver$client, urls)
filename <- stringr::str_glue('data/cpso_registered_family_doctors_with_reghx.rds')
readr::write_rds(doctors, here::here(filename))
driver$server$stop()
beepr::beep(2)


