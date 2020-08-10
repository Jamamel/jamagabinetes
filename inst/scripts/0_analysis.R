url <- xml2::read_html('https://es.wikipedia.org/wiki/Anexo:Gabinete_de_Benito_Ju%C3%A1rez')
parsed_dt <- jamagabinetes::scrape_cabinet(url)

# how many ministries are there
parsed_dt[, .N, by = 'ministry']

# how many ministers
parsed_dt[, .N, by = 'name']

# dates included
parsed_dt[, .(start = min(start), end = max(end))]

# ministers with most appointments, average length of appointment


parsed_dt[, .(N = .N,
              avg_days = appt_duration %>%
                as.numeric('days') %>%
                mean %>%
                round,
              avg_years = appt_duration %>%
                as.numeric('years') %>%
                mean %>%
                round(1)),
          by = 'name'] %>%
  .[order(-N)]



