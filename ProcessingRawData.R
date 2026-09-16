##import packages
library(readxl)
library(dplyr)
library(stringr)
library(lubridate)
library(readr)

##import all raw datasets
##delete unneeded columns and standardize column names for each dataset
##add type label if not already present

cannewsstream <- read_csv("RawData/cannewsstream.csv")
cannewsstream <- cannewsstream |>
  select(-Subtitle,
         -Publisher,
         -Volume,
         -Issue,
         -StartPage,
         -EndPage,
         -PageRange,
         -ISSN,
         -EISSN,
         -ISBN,
         -Language, 
         -DocumentUrl,
         -DOI,
         -AlphaDate) |>
  rename(
    Type = SourceType,
    Date = PubDate) |>
  mutate(
    Type = ifelse(Type == "Newspapers", "News", Type)
  ) |>
  mutate(Year = format(Date, "%Y")) |>
  mutate(Date = as.character(Date)) |>
  mutate(Year = as.numeric(Year))


googlenews <- read_csv("RawData/gnewsdata_2026-09-02.csv")
googlenews <- googlenews |>
  select(-keyword,
         -description,
         -cleanurl) |>
  rename(
    Title = title,
    Date = 'published date',
    Publication = publisher,
    Author = author) |>
  mutate(
    Publication = str_extract(
      Publication,
      "(?<='title': ')[^']+"
    ) 
  ) |>
  mutate(
    Date = gsub("\u00A0", " ", Date),
    Date = as.Date(
      as.POSIXct(Date, format = "%a, %d %b %Y %H:%M:%S GMT", tz = "GMT")
    )
  ) |>
  mutate(Year = format(Date, "%Y")) |>
  mutate(Date = as.character(Date)) |>
  mutate(Year = as.numeric(Year))


halo <- read_excel("RawData/halo_2026-07-23.xlsx")
halo <- halo |>
  mutate(Type = "Conference",
         Publication = "HALO Proceedings") |>
  mutate(Year = format(Date, "%Y"),
         indivauthor = "Y") |>
  mutate(Date = as.character(Date)) |>
  mutate(Year = as.numeric(Year))


isace <- read_excel("RawData/isaceproceedings_2026-07-20.xlsx")
isace <- isace |>
  select(-citation,
         -doi,
         -abstract) |>
  rename(
    Title = title,
    Author = author,
    Year = year,
    Date = date) |>
  mutate(Type = "Conference",
         Publication = "ISACE Proceedings",
         indivauthor = "Y") |>
  mutate(Date = as.character(Date)) |>
  mutate(Year = as.numeric(Year))


linhac <- read_excel("RawData/linhacproceedings_2026-07-23.xlsx")
linhac <- linhac |>
  select(-citation,
         -abstract) |>
  rename(
    Title = title,
    Author = author,
    Year = year,
    Date = date
  ) |>
  mutate(Type = "Conference",
         Publication = "LINHAC Proceedings",
         indivauthor = "Y") |>
  mutate(Date = as.character(Date)) |>
  mutate(Year = as.numeric(Year))

mitsloan <- read_excel("RawData/mitsloane_2026-09-09.xlsx")
mitsloan <- mitsloan |>
  select(-Link) |>
  mutate(Date = as.character(Date)) |>
  mutate(Year = as.numeric(Year))

otthac <- read_excel("RawData/ohac.xlsx")  |>
  select(-Link) |>
  mutate(Year = as.numeric(Year)) |>
  mutate(Date = as.character(Date))

scopus <- read_excel("RawData/scopus_2026-07-16.xlsx")
scopus <- scopus |>
  select(-"Author full names",
         -"Author(s) ID",
         -Volume,
         -Issue,
         -"Art. No.",
         -"Page start",
         -"Page end",
         -"Cited by",
         -DOI,
         -Link,
         -Affiliations,
         -"Authors with affiliations",
         -"Abstract",
         -"Author Keywords",
         -"Publisher",
         -"Document Type",
         -"Publication Stage",
         -"Open Access",
         -Source,
         -EID) |>
  rename(Author = Authors,
         Publication = "Source title") |>
  mutate(Type = "Research",
         Date = NA,
         indivauthor = "Y") |>
  mutate(Date = as.character(Date)) |>
  mutate(Year = as.numeric(Year))


sportsmed <- read_excel("RawData/sportsmed_2026-07-17.xlsx", 
                        col_types = c("text", "skip", "text", 
                                      "text", "skip", "skip", "skip", "skip", 
                                      "text", "skip", "skip", "skip", "skip", 
                                      "skip", "skip", "skip", "skip", "skip", 
                                      "skip", "skip"))
sportsmed <- sportsmed |>
  rename(
    Date = PubDate
  ) |>
  mutate(Date = as.Date(Date)) |>
  mutate(Year = format(Date, "%Y"),
         Type = "Research",
         indivauthor = "Y") |>
  mutate(Date = as.character(Date)) |>
  mutate(Year = as.numeric(Year))



usnewsstream <- read_excel("RawData/usnewsstream_2026-07-22.xlsx", 
                           col_types = c("text", "skip", "text", "text", 
                                         "text", "skip", "skip", "skip", "skip", 
                                         "date", "skip", "skip", "skip", "skip", 
                                         "skip", "skip", "skip", "skip", "skip", 
                                         "skip", "skip"))
usnewsstream <- usnewsstream |>
  rename(
    Date = PubDate
  ) |>
  mutate(Date = as.Date(Date)) |>
  mutate(Year = format(Date, "%Y"),
         Type = "Research") |>
  mutate(Date = as.character(Date)) |>
  mutate(Year = as.numeric(Year))


webofscience <- read_excel("RawData/webofscience_2026-07-16.xlsx", 
                           col_types = c("skip", "text", "skip", 
                                         "skip", "skip", "skip", "skip", "skip", 
                                         "text", "text", "skip", "skip", "skip", 
                                         "skip", "skip", "skip", "skip", "skip", 
                                         "skip", "skip", "skip", "skip", "skip", 
                                         "skip", "skip", "skip", "skip", "skip", 
                                         "skip", "skip", "skip", "skip", "skip", 
                                         "skip", "skip", "skip", "skip", "skip", 
                                         "skip", "skip", "skip", "skip", "skip", 
                                         "skip", "skip", "skip", "numeric", 
                                         "skip", "skip", "skip", "skip", "skip", 
                                         "skip", "skip", "skip", "skip", "skip", 
                                         "skip", "skip", "skip", "skip", "skip", 
                                         "skip", "skip", "skip", "skip", "skip", 
                                         "skip", "skip", "skip", "skip", "skip"))
webofscience <- webofscience |>
  rename(
    Author = Authors,
    Publication = "Source Title",
    Year = "Publication Year"
  ) |>
  mutate(Date = NA,
         Type = "Research",
         indivauthor = "Y") |>
  mutate(Date = as.character(Date)) |>
  mutate(Year = as.numeric(Year))


###NOW MERGE INTO ONE SINGLE BIG DATAFRAME

combineddocs <- bind_rows(cannewsstream,
                          googlenews,
                          halo,
                          isace,
                          linhac,
                          mitsloan,
                          otthac,
                          scopus,
                          sportsmed,
                          usnewsstream,
                          webofscience)

##REMOVE DUPLICATES AND DATE COLUMN
combineddocs <- combineddocs |>
  mutate(Title_lower = tolower(Title)) |>
  distinct(Title_lower, .keep_all = TRUE) |>
  select(-Title_lower,
         -Date)


##EXPORT TO CSV
write.csv(combineddocs, "ProcessedData/combineddocs.csv", row.names = FALSE)
