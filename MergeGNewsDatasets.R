library(readxl)
library(tidyverse)

authordata <- read_excel("RawData/gnewsdata_with_authors.xlsx")
gnewsdata <- read_excel("RawData/combined_googlenews_data_with_cleanurls.xlsx")

joinedgnewsdata <- authordata |>
  left_join(
    gnewsdata |>
      select(title, `published date`, Type) |>
      distinct(title, `published date`, .keep_all = TRUE),
    by = c("title", "published date")
  )

write.csv(joinedgnewsdata, "gnewsdata.csv", row.names = FALSE)
