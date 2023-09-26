# tests

# Selenium message:Navigation timed out after 300000 ms

# # Error in `dplyr::mutate()`:
# ℹ In argument: `data = purrr::map(link, scrape_doctor_page,
#                                   remote = remote, .progress = "Visiting doctors")`.
# Caused by error in `purrr::map()`:
#   ℹ In index: 11215.
# Caused by error:
#   ! 	 Summary: Timeout
# Detail: An operation did not complete before its timeout expired.
# class: org.openqa.selenium.TimeoutException
# Further Details: run errorDetails method

scraper <- modules::use('R/scraper.R')
driver <- scraper$start_selenium_firefox()
links <- readr::read_rds('data/urls.rds')


link <- links |> 
  dplyr::filter(dplyr::row_number() == 11215)


rs <- scraper$scrape_doctor_page_unsafe(link$link, remote = driver$client)

remote$navigate(link$link[1])
print('x')
remote$navigate(links$link[1])
print('x')
