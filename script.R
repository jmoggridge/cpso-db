# do scraping for all doctors 

scraper <- modules::use('R/scraper.R')
driver <- scraper$start_selenium_firefox()
remote <- driver$client
x <- scraper$execute_search(
  remote, 
  include_inactive = T, 
  last_name = 'A',
  )


# kil selenium
driver$server$stop()
