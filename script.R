# do scraping for all doctors 

scraper <- modules::use('R/scraper.R')
driver <- scraper$start_selenium_firefox()
remote <- driver$client
doctors <- scraper$fetch_all_doctors(remote, 
                               include_inactive = F, 
                               family_only = T)

readr::write_rds(doctors, 'data/doctors.rds')
beepr::beep()

# kil selenium
driver$server$stop()
