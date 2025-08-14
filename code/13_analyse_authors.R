# Identify author stats
#


source(here::here("config.R"))
source(file.path(codedir,"libraries.R"),echo=FALSE)



# filenames in config.R



# Save files
nberdois <- readRDS(file= doi.file.Rds)
nrow(nberdois)

# subset to the years in-scope: 2024
nberdois %>%
  # nber specific: only published-online
  rename(published=published.online) %>%
  mutate(year=substr(published,1,4)) %>%
  filter(year >= 2024 ) %>%
  filter(published >= pub_from_date &
         published <= pub_until_date) -> nberdois.subset

authorlist.nber.df <- nberdois.subset %>%
  select(author,doi) %>%
  filter(!is.na(author)) %>%
  tidyr::unnest(author) 

unique_authors <- authorlist.nber.df %>%
  select(given,family) %>%
  distinct(given,family) %>%
  arrange(family,given)

unique_authors_published <- nrow(unique_authors)

# merge back on the number of authors per article

authorlist.nber.df %>%
  group_by(doi) %>%
  summarise(n_authors=n()) %>%
  ungroup() %>%
  left_join(nberdois.subset, by="doi") %>%
  saveRDS(file=doi.enhanced.file.Rds)

message("Number of unique authors in NBER between ",pub_from_date," - ",pub_until_date,": ",unique_authors_published)
message("Number of articles in NBER between ",pub_from_date," - ",pub_until_date,": ",nrow(nberdois.subset))



