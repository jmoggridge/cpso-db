library(tidyverse)

family_doctors <- 
  readr::read_rds('data/cpso_registered_family_doctors.rds') |> 
  relocate(id, cpso_id, name, gender, address, contains('status'), contains('class')) |> 
  relocate(-info, -where(is.list)) |> 
  mutate(
    member_status_date = member_status |> 
      str_extract('[0-9]{2}\\s[A-Za-z]{3}\\s[0-9]{4}') |> 
      dmy(),
    member_status = case_when(
      str_detect(tolower(member_status), 'active member') ~ 'Active',
      str_detect(tolower(member_status), 'suspended') ~ 'Suspended',
      TRUE ~ 'Unknown'
    ),
    registration = curr_or_past_cpso_reg_class |> 
      str_extract('^.*?(?=\\sas\\sof\\s)'),
    registration = if_else(is.na(registration), 'Unknown', registration),
    registration_date = curr_or_past_cpso_reg_class |> 
      str_extract('[0-9]{2}\\s[A-Za-z]{3}\\s[0-9]{4}') |> 
      dmy(),
    .after = 'member_status',
  )

# family_doctors |> 
#   select(-where(is.list)) |> 
#   write_csv('data/cpso_currently_registered_family_doctors.csv')

family_doctors |> count(registration_class)

family_doctors |> 
  select(member_status, member_status_date, 
         curr_or_past_cpso_reg_class, info, cpso_id, link) |>
  # count(curr_or_past_cpso_reg_class) |> 
  view()





# 'Year they started practicing "[Transfer of class of registration to: Independent Practice Certificate]"'

# TODO year of first indep. practice has to be taken from "Registration History" table

links <- family_doctors |> 
  filter(registration == 'Restricted') |> 
  slice_head(n = 10)

scraper <- modules::use('R/scraper.R')
driver <- scraper$start_selenium_firefox()

test <- links |> 
  select(link) |> 
  mutate(data = map(
    link,
    ~scraper$scrape_doctor_page(., driver$client)
  ))


test |> unnest(data) |>  view()

