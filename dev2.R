library(tidyverse)

doctors <- read_rds('data/cpso_registered_family_doctors_with_reghx.rds') |> 
  select(-info) |> 
  glimpse()

reg_hx <- doctors |> 
  select(cpso_id, registration_history) |> 
  unnest(registration_history) |> 
  unnest(registration_history) |> 
  glimpse()

practice_start_years <- 
  reg_hx |> 
  filter(str_detect(tolower(action), 'independent practice certificate')) |> 
  mutate(
    practice_rego_event = action,
    practice_start_year = issue_date |> 
      str_extract('[0-9]{2}\\s[A-Za-z]{3}\\s[0-9]{4}') |> 
      dmy() |> 
      year(),
  ) |> 
  group_by(cpso_id) |> 
  summarise(practice_start_year = min(practice_start_year, na.rm = T))
  

doctors_for_mike <- doctors |> 
  mutate(start_year_fallback =
           curr_or_past_cpso_reg_class |> 
           str_extract('[0-9]{2}\\s[A-Za-z]{3}\\s[0-9]{4}') |> 
           dmy() |> 
           year()
         ) |> 
  left_join(practice_start_years, by = 'cpso_id') |> 
  mutate(
    start_year = coalesce(practice_start_year, start_year_fallback)
    ) |> 
  glimpse()

doctors_for_mike |> 
  select(cpso_id, name, address, start_year, link) |> 
  view()

summary(doctors_for_mike$start_year)

# doctors |> 
#   select(cpso_id, registration_history) |> 
#   unnest(registration_history, keep_empty = T) |> 
#   unnest(registration_history, keep_empty = T) |> 
#   group_by(cpso_id) |> 
#   summarise(nrow = n()) |> 
#   count(nrow) |> 
#   view()



# TODO need to redo scraping from start because
# not getting the correct curr_or_past_cpso_rego_class
# since there can be an intervening line 'expired on'
# for example, see:
# https://doctors.cpso.on.ca/DoctorDetails/Chantal-Rose-Valiquette/0331991-121376
# 
# need to alter scraper to recognize whether there are 2 or 3 rows... and assign
# data to the correct field from the rows... I think the thing was just assuming the second line was the registration class, didn't expect 'expired on' to be there.... 


link <- 'https://doctors.cpso.on.ca/DoctorDetails/Ahmed-Abbas/0334440-132015'


scraper <- modules::use('R/scraper.R')
driver <- scraper$start_selenium_firefox()
remote <- driver$client

scrape_doctor_page(link, remote) |> 
  glimpse()

scrape_doctor_page <- function(link, remote){
  
  remote$navigate(link)
  scrape_status_table(remote)
  
}


