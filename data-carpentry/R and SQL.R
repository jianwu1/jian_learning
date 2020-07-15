library('dplyr')
library('dbplyr')
library('RSQLite')

download.file(url = 'https://ndownloader.figshare.com/files/2292171', 
              destfile = 'data_raw/portal_mammals.sqlite', mode = 'wb')

mammals <- DBI::dbConnect(RSQLite::SQLite(), 'data_raw/portal_mammals.sqlite')

src_dbi(mammals)

tbl(mammals, sql('SELECT year, species_id, plot_id FROM surveys'))

surveys <- tbl(mammals, 'surveys')

surveys %>% 
  select(year, species_id, plot_id)

head(surveys, n = 10)
View(surveys)

nrow(surveys)

show_query(head(surveys, n = 10))

surveys %>% 
  filter(weight < 5) %>% 
  select(species_id, sex, weight)

data_subset <- surveys %>% 
  filter(weight < 5) %>% 
  select(species_id, sex, weight)

data_subset %>% 
  select(-sex)

data_subset <- surveys %>% 
  filter(weight < 5) %>% 
  select(species_id, sex, weight) %>% 
  collect()

plots <- tbl(mammals, 'plots')
plots

