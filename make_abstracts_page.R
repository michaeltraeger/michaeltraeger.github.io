library(readxl)
library(dplyr)
library(stringr)
library(glue)

# ---- file paths ----
input_file <- "/Users/michaeltraeger/CV/abstracts_clean.xlsx"
output_file <- "/Users/michaeltraeger/MyAcademicSite/abstracts_generated.md"

# ---- read data ----
abstracts <- read_excel(input_file)

# ---- clean ----
abstracts <- abstracts %>%
  mutate(
    authors    = ifelse(is.na(authors), "", authors),
    title      = ifelse(is.na(title), "", title),
    conference = ifelse(is.na(conference), "", conference),
    date       = ifelse(is.na(date), "", date),
    number     = ifelse(is.na(number), "", number),
    type       = ifelse(is.na(type), "coauthor", tolower(type))
  )

# ---- normalise type ----
abstracts <- abstracts %>%
  mutate(
    type = case_when(
      type %in% c("first_oral", "first oral", "first-oral") ~ "first_oral",
      type %in% c("first_poster", "first poster", "first-poster") ~ "first_poster",
      type %in% c("coauthor", "co-author", "coauthour") ~ "coauthor",
      TRUE ~ type
    )
  )

# ---- bold your name ----
bold_name <- function(x) {
  x <- str_replace_all(x, "\\bTraeger MW\\b", "<strong>Traeger MW</strong>")
  x <- str_replace_all(x, "\\bTraeger M\\b", "<strong>Traeger M</strong>")
  x
}

abstracts <- abstracts %>%
  mutate(authors = bold_name(authors))

# ---- clean date + number ----
abstracts <- abstracts %>%
  mutate(
    date   = str_squish(date),
    number = str_squish(number),
    number_text = ifelse(number != "", paste0(" ", number, "."), "")
  )

# ---- build abstract blocks ----
abstracts <- abstracts %>%
  mutate(
    abs_text = glue(
      '<span class="abstract-number"></span><span class="abstract-text">{authors}. {title}. <em>{conference}</em>. {date}.{number_text}</span>'
    ),
    block = glue(
      '~~~
<div class="abstract {type}">
{abs_text}
</div>
~~~'
    )
  )

# ---- filter buttons ----
filter_buttons <- paste(
  '~~~',
  '<div class="pub-filters">',
  '  <button onclick="filterAbstracts(\'all\')">All</button>',
  '  <button onclick="filterAbstracts(\'first_oral\')">First author oral</button>',
  '  <button onclick="filterAbstracts(\'first_poster\')">First author poster</button>',
  '  <button onclick="filterAbstracts(\'coauthor\')">Co-author</button>',
  '  <span id="abstract-count" style="margin-left: 1rem;"></span>',
  
  
  '</div>',
  '~~~',
  '~~~',
  '<div>', 
  ' <br> ',
  '</div>',
  '~~~',
  '',
  sep = "\n"
)

# ---- header ----
page_header <- paste(
  '## Conference abstracts',
  '',
  filter_buttons,
  '',
  sep = "\n"
)

# ---- combine ----
page_body <- paste(abstracts$block, collapse = "\n\n")

full_output <- paste0(page_header, page_body)

# ---- write ----
writeLines(full_output, output_file)

cat("Done. File written to:\n", output_file, "\n")