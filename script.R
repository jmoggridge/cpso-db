# do scraping for all doctors 

scraper <- modules::use('R/scraper.R')
driver <- scraper$start_selenium_firefox()
remote <- driver$client
x <- scraper$fetch_all_doctors(remote, 
                               include_inactive = F, 
                               family_only = T)


# kil selenium
driver$server$stop()
