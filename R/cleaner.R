# html dumps from cpso doctor pages to tabular data 

tidy_contact_info <- function(address_string){
  
  tibble(
    address = address_string |> 
      str_remove_all('(Phone|Fax|Electoral District):.*+') |> 
      str_trim(),
    phone = address_string |> 
      str_extract('Phone:.*?(\n|$)') |> 
      str_remove('Phone:') |> 
      str_remove_all('(Fax|Electoral District):.*+') |> 
      str_trim(),
    fax = address_string |> 
      str_extract('Fax:.*?(\n|$)') |> 
      str_remove('Fax:') |> 
      str_remove('Electoral District:.*+') |> 
      str_trim(),
    electoral_district = address_string |> 
      str_extract('Electoral District:.*+') |>
      str_extract('[0-9]+') |> 
      str_trim()
  )  
}
