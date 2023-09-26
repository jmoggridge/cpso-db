# do scraping for all currently-active family doctors

scraper <- modules::use('R/scraper.R')
driver <- scraper$start_selenium_firefox()

active_family_doctor_urls <- scraper$fetch_all_doctors_urls(
  remote = driver$client,
  include_inactive = F,
  family_only = T
)
usethis::use_data(active_family_doctor_urls, overwrite = T)
beepr::beep(1)

active_family_doctors <- scraper$fetch_all_doctors_data(driver$client, urls)
filename <- stringr::str_glue('data/cpso_active_family_doctors_{Sys.Date()}.rds')

usethis::use_data(active_family_doctors, overwrite = T)
driver$server$stop()
beepr::beep(2)

