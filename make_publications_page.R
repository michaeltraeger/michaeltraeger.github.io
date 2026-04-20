library(readxl)
library(dplyr)
library(stringr)
library(glue)

# ---- file paths ----
input_file <- "/Users/michaeltraeger/CV/papers_clean.xlsx"
output_file <- "/Users/michaeltraeger/MyAcademicSite/publications_generated.md"


# ---- read data ----
pubs <- read_excel(input_file)

# ---- clean ----
pubs <- pubs %>%
  mutate(
    authors = ifelse(is.na(authors), "", authors),
    title   = ifelse(is.na(title), "", title),
    journal = ifelse(is.na(journal), "", journal),
    year    = ifelse(is.na(year), "", as.character(year)),
    doi     = ifelse(is.na(doi), "", doi),
    link    = ifelse(is.na(link), "", link),
    pdf     = ifelse(is.na(pdf), "", pdf),
    role    = ifelse(is.na(role), "coauthor", tolower(role))
  )

# ---- FIX PDF PATH ----
pubs <- pubs %>%
  mutate(
    pdf = basename(pdf),  # remove any existing path
    pdf = ifelse(
      nzchar(pdf),
      paste0("/assets/papers/", pdf),
      ""
    )
  )

# ---- bold your name ----
bold_name <- function(x) {
  x <- str_replace_all(x, "\\bTraeger MW\\b", "<strong>Traeger MW</strong>")
  x <- str_replace_all(x, "\\bTraeger M\\b", "<strong>Traeger M</strong>")
  x
}

pubs <- pubs %>%
  mutate(authors = bold_name(authors))

# ---- links ----
make_links <- function(link, pdf) {
  parts <- c()
  
  if (nzchar(link)) {
    parts <- c(parts, glue('<a href="{link}">Link</a>'))
  }
  
  if (nzchar(pdf)) {
    parts <- c(parts, glue('<a href="{pdf}">PDF</a>'))
  }
  
  paste(parts, collapse = " · ")
}

# ---- build publication blocks ----
pubs <- pubs %>%
  rowwise() %>%
  mutate(
    links = make_links(link, pdf),
    doi_text = if (nzchar(doi)) glue(" doi:{doi}.") else "",
    pub_text = glue(
      '<span class="pub-number"></span><span class="pub-text">{authors}. {title}. <em>{journal}</em>. {year}.{doi_text} {links}</span>'
      ),
    block = glue(
      '~~~
<div class="pub {role}">
{pub_text}
</div>
~~~'
    )
  ) %>%
  ungroup()

# ---- filter buttons ----
filter_buttons <- paste(
  '~~~',
  '<div class="pub-filters">',
  '  <button onclick="filterPubs(\'all\')">All</button>',
  '  <button onclick="filterPubs(\'first\')">First author</button>',
  '  <button onclick="filterPubs(\'senior\')">Senior author</button>',
  '  <button onclick="filterPubs(\'coauthor\')">Co-author</button>',
  '  <span id="pub-count" style="margin-left: 1rem;"></span>',
  
    
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
  "## All publications",
  "",
  filter_buttons,
  "",
  sep = "\n"
)

# ---- combine ----
page_body <- paste(pubs$block, collapse = "\n\n")

full_output <- paste0(page_header, page_body)

# ---- write ----
writeLines(full_output, output_file)

cat("Done. File written to:\n", output_file, "\n")