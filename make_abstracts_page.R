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
    authors        = ifelse(is.na(authors), "", authors),
    title          = ifelse(is.na(title), "", title),
    conference     = ifelse(is.na(conference), "", conference),
    date           = ifelse(is.na(date), "", date),
    number         = ifelse(is.na(number), "", number),
    abstracttype   = ifelse(is.na(abstracttype), "coauthor", abstracttype),
    conferencetype = ifelse(is.na(conferencetype), "dom", conferencetype),
    
    abstracttype   = str_squish(tolower(abstracttype)),
    conferencetype = str_squish(tolower(conferencetype))
  )

# ---- normalise abstract type + conference type ----
abstracts <- abstracts %>%
  mutate(
    abstracttype = case_when(
      abstracttype %in% c("first_oral", "first oral", "first-oral") ~ "first_oral",
      abstracttype %in% c("first_poster", "first poster", "first-poster") ~ "first_poster",
      abstracttype %in% c("coauthor", "co-author", "coauthour", "co author") ~ "coauthor",
      TRUE ~ abstracttype
    ),
    conferencetype = case_when(
      conferencetype %in% c("int", "international") ~ "int",
      conferencetype %in% c("dom", "domestic") ~ "dom",
      TRUE ~ conferencetype
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
<div class="abstract {abstracttype} {conferencetype}">
{abs_text}
</div>
~~~'
    )
  )

# ---- filter buttons ----
filter_buttons <- paste(
  '~~~',
  '<div class="pub-filters">',
  '  <strong>Abstract type:</strong>',
  '  <button data-group="type" data-value="all" onclick="filterAbstractType(\'all\')">All</button>',
  '  <button data-group="type" data-value="first_oral" onclick="filterAbstractType(\'first_oral\')">First author oral</button>',
  '  <button data-group="type" data-value="first_poster" onclick="filterAbstractType(\'first_poster\')">First author poster</button>',
  '  <button data-group="type" data-value="coauthor" onclick="filterAbstractType(\'coauthor\')">Co-author</button>',
  '',
  '  <br><br>',
  '',
  '  <strong>Conference type:</strong>',
  '  <button data-group="conf" data-value="all" onclick="filterConferenceType(\'all\')">All</button>',
  '  <button data-group="conf" data-value="int" onclick="filterConferenceType(\'int\')">International</button>',
  '  <button data-group="conf" data-value="dom" onclick="filterConferenceType(\'dom\')">Domestic</button>',
  '',
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