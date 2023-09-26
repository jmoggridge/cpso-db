# do scraping for all currently-active family doctors 

scraper <- modules::use('R/scraper.R')
driver <- scraper$start_selenium_firefox()

urls <- scraper$fetch_all_doctors_urls(
  remote = driver$client, 
  include_inactive = F, 
  family_only = T
)

readr::write_rds(urls, 'data/urls.rds')
beepr::beep(1)

doctors <- scraper$fetch_all_doctors_data(driver$client, urls)
filename <- stringr::str_glue('data/cpso_active_family_doctors_{Sys.Date()}.rds')
readr::write_rds(doctors, filename)
driver$server$stop()
beepr::beep(2)
