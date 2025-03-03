# Getting in-scope articles
# Code derived from another project
# NOTE: THIS REQUIRES R 4.2.0 and newer packages!
#



source(here::here("config.R"))
source(file.path(codedir,"libraries.R"),echo=FALSE)


library(rcrossref)

# filenames in config.R


# Each journal has a ISSN
if (!file.exists(issns.file)) {
  issns <- data.frame(matrix(ncol=3,nrow=10))
  names(issns) <- c("journal","issn","lastdate")
  tmp.date <- c("2000-01")
issns[1,] <- c("American Economic Journal: Applied Economics","1945-7790",tmp.date)
issns[2,] <- c("American Economic Journal: Economic Policy","1945-774X",tmp.date)
issns[3,] <- c("American Economic Journal: Macroeconomics", "1945-7715",tmp.date)
issns[4,] <- c("American Economic Journal: Microeconomics", "1945-7685",tmp.date)
issns[5,] <- c("The American Economic Review","1944-7981",tmp.date)
issns[6,] <- c("The American Economic Review","0002-8282",tmp.date)  # print ISSN is needed!
issns[7,] <- c("Journal of Economic Literature","2328-8175",tmp.date)
issns[8,] <- c("Journal of Economic Perspectives","1944-7965",tmp.date)
issns[9,] <- c("American Economic Review: Insights","2640-2068",tmp.date)
issns[10,] <- c("National Bureau of Economic Research","0898-2937",tmp.date)

  saveRDS(issns %>% filter(!is.na(journal)), file= issns.file)
} else {
  warning(paste("Reusing file: ",issns.file))
}

issns <- readRDS(file = issns.file) %>%
  filter(journal=="National Bureau of Economic Research")

# Darn, NBER does not record the ISSN.



if (file.exists(doi.file.Rds) ) {
  warning(paste("Re-using file:",doi.file.Rds))
} else{
  new.df <- NA
  for ( x in 1:nrow(issns) ) {
    message(paste0("Processing ",issns[x,"journal"]," (",issns[x,"issn"],")"))
    new  <- cr_works(
                filter = c(prefix=nber_prefix,from_pub_date=from_date,until_pub_date=until_date),
                select=c("DOI","title","published-online","author"),
                       .progress="text",
                       cursor = "*")
    if ( x == 1 ) {
      new.df <- as.data.frame(new$data)
    } else {
      tmp.df <- as.data.frame(new$data)
      if ( nrow(tmp.df) > 0 ) {
        new.df <- bind_rows(new.df,tmp.df)
      } else {
        warning(paste0("Did not find records"))
      }
      rm(tmp.df)
    }
  }
  # filters
  saveRDS(new.df, file=  file.path(interwrk,"new.Rds"))
  rm(new)
}


# filters
new.df <- readRDS(file.path(interwrk,"new.Rds"))
nrow(new.df)
new.df %>%
  filter(!is.null(author)) %>%
  filter(title!="Front Matter") %>%
  filter(!str_detect(title,"Volume")) %>%
  filter(!str_detect(title,"Forthcoming")) %>%
  # filter(title!="Editor's Note") %>%
  # More robust
  filter(str_sub(doi, start= -1)!="i")-> filtered.df
nrow(filtered.df)
saveRDS(filtered.df, file=  doi.file.Rds)

# clean read-back
nberdois <- readRDS(file= doi.file.Rds)
nrow(nberdois)

# subset to the years in-scope: 2024
nberdois %>%
  # nber specific: only published-online
  rename(published=published.online) %>%
  mutate(year=substr(published,1,4)) %>%
  filter(year >= 2024 ) %>%
  filter(published >= from_date &
         published <= until_date) -> nberdois.subset 
nberdois.subset %>%
  group_by(year) %>%
  summarise(Published=n())    -> nberdois.by.year

nberdois.by.year

nberdois.by.year %>% ungroup() %>% summarize(n=sum(Published)) -> nberdois.total

nberdois.total


