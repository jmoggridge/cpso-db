modules::import(dplyr)
modules::import(tidyr)
modules::import(tibble)
modules::import(purrr)
modules::import(stringr)
modules::import(RSelenium)
modules::import(netstat)
modules::import(glue)



# Setup -----

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

# Search -----

# navigates to the advanced search form
visit_search_page <- function(remote){
  remote$navigate("https://doctors.cpso.on.ca/?search=general")
  invisible(NULL)
}

# navigates remote to search results page 1
execute_search <- function(
    remote,
    include_inactive = FALSE,
    last_name = NULL,
    family_only = TRUE
    ) {
  
  # go to advanced search
  visit_search_page(remote)
  
  if (!is.null(last_name)) input_last_name(remote, last_name)
  if (include_inactive) toggle_inactive_doctors(remote)
  if (family_only) toggle_family_doctors(remote)
  
  submit_button <- remote$findElement(
    using = 'name',
    value = 'p$lt$ctl01$pageplaceholder$p$lt$ctl02$CPSO_AllDoctorsSearch$btnSubmit1'
  )
  submit_button$clickElement()
  
  invisible(NULL)
}

toggle_family_doctors <- function(remote) {
  # Select Type of Doctor: Family Doctor
  family_radio_button <- remote$findElement(
    using = 'xpath', 
    '/html/body/form/section/div/div/div[2]/div/div/div/div[1]/div/div[3]/div[3]/div[2]/div/section[1]/fieldset/div[1]/label[1]'
  )
  family_radio_button$clickElement()
  invisible(NULL)
}

toggle_specialist <- function(remote) {
  # Select Type of Doctor: Specialist
  remote$executeScript(
    "document.getElementById('rdoDocTypeSpecialist').click()"
  )
  invisible(NULL)
}

input_last_name <- function(remote, last_name){
  last_name_element <- remote$findElement(
    using = 'xpath',
    value = '//*[@id="txtLastName"]'
  )
  last_name_element$sendKeysToElement(list(last_name))
  invisible(NULL)
}

toggle_inactive_doctors <- function(remote){
  remote$executeScript(
    "document.getElementById('p_lt_ctl01_pageplaceholder_p_lt_ctl02_CPSO_AllDoctorsSearch_chkInactiveDoctors').click()"
  )
  invisible(NULL)
}

# Collect Ids -----

# get up to 10 doctors' links from one results page
links_from_results_page <- function(remote){
  remote$findElements(
    using = 'tag name', 
    value = 'a'
  ) |>
    purrr::map(
      ~.x$getElementAttribute(attrName = 'href')
    ) |>
    purrr::keep(~length(.) == 1) |>
    unlist() |>
    purrr::keep(
      ~stringr::str_detect(., 'https://doctors.cpso.on.ca/DoctorDetails/')
      )
}

# from search results page, find last page number
# if >5 there is a different box for the final page
# if <=5 there isn't a box for this element
get_total_pages <- function(remote){
  
  pages <- 
    remote$findElement(
      using = 'xpath',
      value = '//*[@id="p_lt_ctl01_pageplaceholder_p_lt_ctl03_CPSO_DoctorSearchResults_lnbLastPage"]'
    )$getElementText() |>
    purrr::pluck(1) |>
    as.numeric()
  
  return(pages)
}


# follow links to five results pages
click_through_five_pages <- function(remote){
  
  # scrape links from one results page
  goto_results_page_get_links <- function(xpath){
    remote$findElement('xpath', xpath)$clickElement()
    links_from_results_page(remote)
  }
  
  # handle errors with results page
  possibly_get_links <- purrr::possibly(
    goto_results_page_get_links,
    otherwise = NULL
  )
  
  # iterate over the five page links in the pagination section
  list(int = stringr::str_pad(0:4, 2, pad = '0')) |>
    glue::glue_data('//*[@id="p_lt_ctl01_pageplaceholder_p_lt_ctl03_CPSO_DoctorSearchResults_rptPages_ctl{int}_lnbPage"]') |>
    purrr::map(~possibly_get_links(.) |> suppressMessages())
}

# navigate to next pagination of five pages
next_five_pages <- function(remote){
  
  next_five_element <- NULL
  attempts <- 0
  
  # keep trying to find element if page hasn't loaded yet
  while (is.null(next_five_element) && attempts < 1000) {
    next_five_element <- tryCatch({
      remote$findElement('xpath', '//*[@id="p_lt_ctl01_pageplaceholder_p_lt_ctl03_CPSO_DoctorSearchResults_lnbNextGroup"]')
    },
    error = function(e){NULL})
    attempts <- attempts + 1
  }
  
  if (attempts > 999) {
    stop('Next five pages link isnt displayed as expected.')
  }
  
  next_five_element$clickElement()  
  invisible(NULL)
}


# iterate over all search results pages
get_links_from_results <- function(remote){
  
  pages_of_results <- 
    purrr::possibly(get_total_pages, otherwise = 5)(remote)
  paginations <- 
    ceiling(pages_of_results/5)
  
  if (paginations == 1000) {
    stop('too many search results for this letter')
  }
  
  purrr::map(
    .x = seq(paginations),
    .f = ~{
      dat <- click_through_five_pages(remote)
      next_five_pages(remote)
      return(dat)
    }
  )
}


# Collect Data -----


# get text from an element and simplify to vector
get_element_text <- function(xpath, remote){
  remote$findElement('xpath', xpath)$getElementText() |> unlist()
}

# tidies up text data
str_cleanup <- function(x){
  x |> stringr::str_remove(',') |> stringr::str_trim()
}

## get the table of specialties of the current doctor page
scrape_specialties_table <- function(remote){
  
  # find the right table
  tbl <- remote$findElement('id', 'specialties')
  
  # find the body of that table
  tbl_body <- 
    tbl$findChildElement(using = 'tag name', 'tbody')
  
  # get all the rows of that table
  tbl_rows <- 
    tbl_body$findChildElements(using = 'tag name', 'tr') |>
    tibble::enframe(name = 'row', value = 'element')
  
  # get td cells from each row and parse the 3 values, return table
  tbl_rows |>
    dplyr::mutate(
      cells = purrr::map(
        element,
        ~.$findChildElements('tag name', 'td') |>
          purrr::map(~.$getElementText()) |>
          purrr::set_names(
            'specialty', 
            'specialty_issued_date', 
            'specialty_type'
            )
      )
    ) |>
    tidyr::unnest_wider(cells) |>
    dplyr::select(
      specialty, 
      specialty_issued_date, 
      specialty_type
    ) |>
    dplyr::mutate(
      dplyr::across( 
        dplyr::everything(),
        ~unlist(.) |> paste0()
        )
    )
}


## get data for a single doctor from their cpso page
export('scrape_doctor_page_unsafe')
scrape_doctor_page_unsafe <- function(link, remote){
  remote$navigate(link)
  
  spec_tbl <- scrape_specialties_table(remote)
  
  list(
    name = '//*[@id="docTitle"]',
    cpso_id = '/html/body/form/section/div/div/div[2]/div[3]/div[1]/h3',
    address = '/html/body/form/section/div/div/div[2]/div[4]/section[2]/div/div[2]',
    member_status = '/html/body/form/section/div/div/div[2]/div[3]/div[2]/div[2]/strong',
    curr_or_past_cpso_reg_class = '/html/body/form/section/div/div/div[2]/div[3]/div[3]/div[2]',
    info =  '/html/body/form/section/div/div/div[2]/div[4]/section[1]/div[2]'
  ) |>
    purrr::map(get_element_text, remote = remote) |>
    tibble::as_tibble_row() |>
    dplyr::mutate(spec_tbl = list(spec_tbl))
}

export('scrape_doctor_page')
scrape_doctor_page <- function(link, remote){
  doctor_res <- NULL
  attempts <- 0
  while (is.null(doctor_res) && attempts < 10) {
    doctor_res <- tryCatch({
      scrape_doctor_page_unsafe(link, remote)
    },
    error = function(e){NULL})
    attempts <- attempts + 1
  }
  return(doctor_res)
}

# Search Wrapper -----

query_search_and_fetch_links <- 
  function(remote, last_name, include_inactive, family_only) {
  
  # perform the search and go to paginated results
  execute_search(
    remote, 
    last_name = last_name,
    include_inactive = include_inactive,
    family_only = family_only
  )
  
  # go through paginated results and get urls
  search_res <- get_links_from_results(remote) |>
    tibble::enframe(name = 'group', value = 'link') |>
    tidyr::unnest(link) |>
    tidyr::unnest(link) |>
    dplyr::select(link) |> 
    dplyr::distinct() |> 
    dplyr::mutate(
      id = stringr::str_extract(link, '[^/]+$')
    )

}


# API -----


export('fetch_all_doctors_urls')
fetch_all_doctors_urls <- function(remote, 
                              include_inactive = F, 
                              family_only = T){
  # do a search for each letter because results
  # limited to 10,000 drs. (on 1000 results pages)
  urls <- 
    LETTERS |>
    purrr::map(
      ~query_search_and_fetch_links(
        remote, 
        last_name = .[[1]],
        include_inactive = F, 
        family_only = T
      ),
      .progress = 'Search queries'
    ) |> 
    dplyr::bind_rows() |> 
    dplyr::distinct()
  return(urls)
}

export('fetch_all_doctors_data')
fetch_all_doctors_data <- function(remote, urls){
  
  data <- urls |> 
    dplyr::mutate(
      data = purrr::map(
        link, possibly(scrape_doctor_page, NULL),
        remote = remote,
        .progress = 'Visiting doctors'
      )
    ) |>
    tidyr::unnest_wider(data) |>
    dplyr::mutate(
      cpso_id = stringr::str_remove(cpso_id, '^CPSO#: '),
      gender = info |>
        stringr::str_extract('(Gender:.*?)\n') |>
        stringr::str_remove_all('Gender: |\n')
    )
  return(data)
}

# //*[@id="p_lt_ctl01_pageplaceholder_p_lt_ctl03_CPSO_DoctorSearchResults_lnbNextGroup"]
# //*[@id="p_lt_ctl01_pageplaceholder_p_lt_ctl03_CPSO_DoctorSearchResults_lnbNextGroup"]
