modules::import(tidyverse)
modules::import(RSelenium)
modules::import(netstat)

modules::export('start_selenium_firefox')
start_selenium_firefox <- function(){
  # setup browser
  driver <- RSelenium::rsDriver(browser = "firefox",
                                chromever = NULL,
                                verbose = F,
                                port = netstat::free_port())
  # remote <- driver$client
  return(driver)
}

# navigates to the advanced search form
visit_search_page <- function(remote){
  remote$navigate("https://doctors.cpso.on.ca/?search=general")
  return(remote)
}

# navigates remote to search results page 1
execute_search <- function(
    remote,
    include_inactive = TRUE,
    last_name = NULL
    ) {
  
  # go to advanced search
  visit_search_page(remote)
  
  if (!is.null(last_name)) {
    input_last_name(remote, last_name)
  }
  
  if (include_inactive) {
    toggle_inactive_doctors(remote)
  }
  
  submit_button <- remote$findElement(
    using = 'name',
    value = 'p$lt$ctl01$pageplaceholder$p$lt$ctl02$CPSO_AllDoctorsSearch$btnSubmit1'
  )
  submit_button$clickElement()
  
  return(remote)
}


input_last_name <- function(remote, last_name){
  last_name_element <- remote$findElement(
    using = 'xpath',
    value = '//*[@id="txtLastName"]'
  )
  print(last_name_element)
  last_name_element$sendKeysToElement(list(last_name))
  invisible(NULL)
}

toggle_inactive_doctors <- function(remote){
  remote$executeScript(
    "document.getElementById('p_lt_ctl01_pageplaceholder_p_lt_ctl02_CPSO_AllDoctorsSearch_chkInactiveDoctors').click()"
  )
  invisible(NULL)
}


get_all_doctors <- function(remote){
  LETTERS |> 
    purrr::map(~execute_search)
}
