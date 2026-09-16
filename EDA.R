##import packages
library(tidyverse)
library(dplyr)
library(readr)
library(readxl)

library(tm)
library(RColorBrewer)

##install.packages("writexl")
library(writexl)

library(dplyr)
library(stringr)
library(stringdist)

library(gt)


##load combined spreadsheet of documents
combineddocs <- read_csv("ProcessedData/combineddocs.csv", 
                         col_types = cols(Year = col_number()))


##DATA VIZ

##visualize publications by year, coloured by type
combineddocs |>
  filter(!is.na(Year), !is.na(Type)) |>
  count(Year, Type) |>
  mutate(Year = as.integer(Year)) |>
  ggplot(aes(x = Year, y = n, fill = Type)) +
  geom_col() +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  labs(
    x = "Year",
    y = "Number of Publications",
    fill = "Publication Type"
  ) +
  theme_minimal()

##faceted by type instead of coloured
combineddocs |>
  count(Year, Type) |>
  ggplot(aes(x = Year, y = n)) +
  geom_col() +
  facet_wrap(~Type) +
  labs(
    x = "Year",
    y = "Number of Publications"
  ) +
  theme_minimal()


##exploring authorship
##first have to separate out multiple authors
authordata <- combineddocs |>
  select(Title, Year, Author, Type, indivauthor) |>
  filter(!is.na(Author)) |>
  separate_rows(Author, sep = "\\s*(?:;|:)\\s*|\\s+and\\s+") |>
  mutate(Author = str_trim(Author))

##write function to standardize formatting of author column (only for individual author names, not orgs or pseudonyms)
standardize_author <- function(x) {
  
  x <- str_squish(x)
  
  ##Lastname, F.  --> Lastname, F
  if (str_detect(x, ",")) {
    
    parts <- str_split(x, ",", n = 2, simplify = TRUE)
    
    lastname <- str_squish(parts[1])
    firstname <- str_squish(parts[2])
    
    # Take first letter of first name/initial
    initial <- str_sub(str_remove(firstname, "\\."), 1, 1)
    
    return(paste0(lastname, ", ", initial))
  }
  
  ##Lastname F. / Lastname F --> Lastname, F
  words <- str_split(x, "\\s+")[[1]]
  
  if (length(words) >= 2 &&
      nchar(str_remove(words[length(words)], "\\.")) == 1) {
    
    lastname <- paste(words[-length(words)], collapse = " ")
    initial <- str_sub(str_remove(words[length(words)], "\\."), 1, 1)
    
    return(paste0(lastname, ", ", initial))
  }
  
  ##Firstname Lastname --> Lastname, F
  if (length(words) >= 2) {
    
    firstname <- words[1]
    lastname <- paste(words[-1], collapse = " ")
    initial <- str_sub(str_remove(firstname, "\\."), 1, 1)
    
    return(paste0(lastname, ", ", initial))
  }
  
  ##Leave anything unexpected unchanged
  x
}


authordata <- authordata |>
  mutate(
    Author = if_else(
      indivauthor == "Y",
      sapply(Author, standardize_author),
      Author
    )
  )
write_xlsx(authordata, "ProcessedData/authordata.xlsx")

##make author names lowercase
authors <- authordata |>
  mutate(
    Author_original = Author,
    Author_clean = Author |>
      str_replace_all("\u00A0", " ") |>
      str_to_lower() |>
      str_remove_all("[[:punct:]]") |>
      str_squish()
  )

##now we can analyze the author data including
##COUNTS OF INDIVIDUAL AUTHORS
authorcount_total <- authors |>
  count(Author_clean, sort = TRUE)
##make plot
authorcount_total |>
  slice_max(n, n = 30) |>
  ggplot(aes(x = reorder(Author_clean, n), y = n)) +
  geom_col() +
  coord_flip() +
  labs(
    x = "Author",
    y = "Number of Publications"
  ) +
  theme_minimal()


##plot author/type diversity (to see if authors publish multiple types)

##ID authors who have published in more than one Type
multi_type_authors <- authors |>
  group_by(Author_clean) |>
  summarise(
    NumberOfTypes = n_distinct(Type),
    .groups = "drop"
  ) |>
  filter(NumberOfTypes > 1)

author_type_counts <- authors |>
  filter(Author_clean %in% multi_type_authors$Author_clean) |>
  count(Author_clean, Type) |>
  group_by(Author_clean) |>
  mutate(
    TotalPublications = sum(n)
  ) |>
  ungroup()

ggplot(
  author_type_counts,
  aes(
    x = reorder(Author_clean, TotalPublications),
    y = n,
    fill = Type
  )
) +
  geom_col(width = 0.7) +
  coord_flip() +
  labs(
    title = "Authors Publishing Across Multiple Publication Types",
    x = "Author",
    y = "Number of Publications",
    fill = "Publication Type"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 17
    ),
    axis.title = element_text(
      face = "bold"
    ),
    axis.text.y = element_text(
      size = 11
    ),
    legend.title = element_text(
      face = "bold"
    ),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )


##same bar plot but excluding authors whose only two types are 'research' and 'conference'
multi_type_authorsNOCON <- authors |>
  group_by(Author_clean) |>
  summarise(
    NumberOfTypes = n_distinct(Type),
    Types = paste(sort(unique(Type)), collapse = " | "),
    .groups = "drop"
  ) |>
  filter(
    NumberOfTypes > 1,
    Types != "Conference | Research"
  )

author_type_countsNOCON <- authors |>
  filter(Author_clean %in% multi_type_authorsNOCON$Author_clean) |>
  count(Author_clean, Type) |>
  group_by(Author_clean) |>
  mutate(
    TotalPublications = sum(n)
  ) |>
  ungroup()

ggplot(
  author_type_countsNOCON,
  aes(
    x = reorder(Author_clean, TotalPublications),
    y = n,
    fill = Type
  )
) +
  geom_col(width = 0.7) +
  coord_flip() +
  labs(
    title = "Authors Publishing Across Multiple Publication Types \n (Excluding only Research/Conference Pairings)",
    x = "Author",
    y = "Number of Publications",
    fill = "Publication Type"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 17
    ),
    axis.title = element_text(
      face = "bold"
    ),
    axis.text.y = element_text(
      size = 11
    ),
    legend.title = element_text(
      face = "bold"
    ),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )



##now author/type diversity to see specific authors
author_type_diversity <- authors |>
  group_by(Author_clean) |>
  summarise(
    NumberOfTypes = n_distinct(Type),
    .groups = "drop"
  ) |>
  filter(NumberOfTypes > 1) |>
  arrange(desc(NumberOfTypes))

author_type_diversity |>
  gt() |>
  tab_header(
    title = "Authors Publishing Across Multiple Publication Types"
  ) |>
  cols_label(
    Author_clean = "Author",
    NumberOfTypes = "Number of Publications"
  ) |>
  tab_options(
    table.font.size = 12,
    heading.title.font.size = 16,
    heading.title.font.weight = "bold",
    data_row.padding = px(6),
    table.border.top.width = px(1),
    table.border.bottom.width = px(1),
    column_labels.border.top.width = px(1),
    column_labels.border.bottom.width = px(1)
  )

###GRAPHS ABOVE SHOW ONLY THOSE AUTHORS WHO PUBLISH IN MULTIPLE PUB TYPES, VERY VERY FEW AKA LIMITED CROSSOVER!!


##UNIQUE AUTHORS PER YEAR
authorsperyear <- authors |>
  group_by(Year) |>
  summarise(
    UniqueAuthors = n_distinct(Author_clean)
  )
##plot unique authors per year
ggplot(authorsperyear, aes(x = Year, y = UniqueAuthors)) +
  geom_col() +
  labs(
    x = "Year",
    y = "Number of Unique Authors"
  ) +
  theme_minimal()
###SHOWS US THAT THERE ARE NOT ONLY MORE ARTICLES OVER TIME BUT MORE PEOPLE INVOLVED





