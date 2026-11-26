# Збір даних про населення України: web-scraping -> tidy -> SQLite
# Запуск: Rscript update_data.R

suppressPackageStartupMessages({
  library(rvest)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(readr)
  library(DBI)
  library(RSQLite)
})

dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)

# --- Допоміжні функції -------------------------------------------------------

fetch_page <- function(url, file) {
  resp <- httr::GET(
    url,
    httr::user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) PiLabBot/1.0")
  )
  stopifnot(httr::status_code(resp) == 200)
  html <- httr::content(resp, as = "text", encoding = "UTF-8")
  writeLines(html, file, useBytes = TRUE)
  read_html(html)
}

# Бере перше число з клітинки: "42 386 ²" -> 42386, "9,4 4" -> 9.4, "—" -> NA
parse_num <- function(x) {
  x <- as.character(x)
  x <- gsub("[−–—]", "-", x)
  m <- str_extract(x, "-?(?:[0-9]{1,3}(?:[[:space:]\u00a0][0-9]{3})+|[0-9]+)(?:[.,][0-9]+)?")
  m <- gsub("[[:space:]\u00a0]", "", m)
  suppressWarnings(as.numeric(sub(",", ".", m, fixed = TRUE)))
}

find_table <- function(tables, pred) {
  for (t in tables) if (isTRUE(pred(t))) return(t)
  stop("Не знайдено потрібну таблицю на сторінці")
}

# --- 1. Національна динаміка: населення, народжуваність, смертність ----------

url_country <- "https://uk.wikipedia.org/wiki/%D0%9D%D0%B0%D1%81%D0%B5%D0%BB%D0%B5%D0%BD%D0%BD%D1%8F_%D0%A3%D0%BA%D1%80%D0%B0%D1%97%D0%BD%D0%B8"
page_country <- fetch_page(url_country, "data/raw/country_wikipedia.html")
tabs_country <- html_table(page_country, fill = TRUE)

t_country <- find_table(tabs_country, function(d)
  all(c("Рік", "Живих народжень", "Смертей", "Природний приріст") %in% names(d)))

country <- tibble(
  year               = parse_num(t_country[[1]]),
  population_thousand = parse_num(t_country[[2]]),
  births             = parse_num(t_country[[3]]),
  deaths             = parse_num(t_country[[4]]),
  natural_increase   = parse_num(t_country[[5]]),
  birth_rate         = parse_num(t_country[[6]]),
  death_rate         = parse_num(t_country[[7]]),
  natural_rate       = parse_num(t_country[[8]]),
  fertility          = parse_num(t_country[[9]])
) |>
  filter(!is.na(year), year >= 1900, year <= 2100) |>
  arrange(year)

stopifnot(nrow(country) > 50, all(country$births > 0, na.rm = TRUE))

# --- 2. Населення регіонів за окремі роки -----------------------------------

url_regions <- "https://uk.wikipedia.org/wiki/%D0%86%D1%81%D1%82%D0%BE%D1%80%D0%B8%D1%87%D0%BD%D0%B0_%D0%B4%D0%B5%D0%BC%D0%BE%D0%B3%D1%80%D0%B0%D1%84%D1%96%D1%8F_%D0%A3%D0%BA%D1%80%D0%B0%D1%97%D0%BD%D0%B8"
page_regions <- fetch_page(url_regions, "data/raw/regions_wikipedia.html")
tabs_regions <- html_table(page_regions, fill = TRUE)

t_regions <- find_table(tabs_regions, function(d)
  all(c("регіон", "1926", "1959", "2014") %in% names(d)))

region_years <- c("1926", "1939", "1959", "1970", "1979", "1989", "2001", "2014")

region_pop <- t_regions |>
  select(region = 1, all_of(region_years)) |>
  mutate(across(all_of(region_years), parse_num)) |>
  filter(region != "Україна") |>
  mutate(region = region |>
           str_remove("\\s*область$") |>
           str_remove("^м\\.\\s*") |>
           str_trim()) |>
  pivot_longer(all_of(region_years), names_to = "year", values_to = "population_thousand") |>
  mutate(year = as.integer(year)) |>
  filter(!is.na(population_thousand)) |>
  arrange(region, year)

regions <- region_pop |>
  distinct(region) |>
  arrange(region) |>
  mutate(region_id = row_number(), .before = 1) |>
  rename(name = region)

region_pop <- region_pop |>
  left_join(regions, by = c("region" = "name")) |>
  select(region_id, year, population_thousand)

# --- 3. Тривалість життя за статтю ------------------------------------------

url_life <- "https://uk.wikipedia.org/wiki/%D0%A2%D1%80%D0%B8%D0%B2%D0%B0%D0%BB%D1%96%D1%81%D1%82%D1%8C_%D0%B6%D0%B8%D1%82%D1%82%D1%8F_%D0%B2_%D0%A3%D0%BA%D1%80%D0%B0%D1%97%D0%BD%D1%96"
page_life <- fetch_page(url_life, "data/raw/life_wikipedia.html")
tabs_life <- html_table(page_life, fill = TRUE)

# Довгий ряд 1950-2019 (кожні 4-5 років)
t_life_long <- find_table(tabs_life, function(d)
  "1950" %in% names(d) && "2019" %in% names(d) &&
    any(grepl("чоловіки", as.character(d[[1]]))))

life_long <- bind_rows(
  tibble(year = as.integer(names(t_life_long)[-1]),
         sex = "male",
         value = parse_num(unlist(t_life_long[grepl("чоловіки", t_life_long[[1]]), -1]))),
  tibble(year = as.integer(names(t_life_long)[-1]),
         sex = "female",
         value = parse_num(unlist(t_life_long[grepl("жінки", t_life_long[[1]]), -1])))
) |>
  filter(!is.na(year), year <= 2001)

# Щорічний ряд 2002-2021
t_life_year <- find_table(tabs_life, function(d)
  ncol(d) >= 4 && any(as.character(d[[1]]) == "2002"))

life_year <- as.data.frame(t_life_year, stringsAsFactors = FALSE)[-1, 1:4]
names(life_year) <- c("year", "both", "male", "female")

life_year <- life_year |>
  transmute(year = as.integer(parse_num(year)),
            male = parse_num(male),
            female = parse_num(female)) |>
  filter(!is.na(year)) |>
  pivot_longer(c(male, female), names_to = "sex", values_to = "value") |>
  filter(!is.na(value))

life <- bind_rows(life_long, life_year) |>
  distinct(year, sex, .keep_all = TRUE) |>
  arrange(year, sex)

stopifnot(all(c("male", "female") %in% life$sex))

# --- 4. Збереження tidy-даних ------------------------------------------------

write_csv(country, "data/processed/country_stats.csv")
write_csv(region_pop |> left_join(regions, by = "region_id") |>
            select(region = name, year, population_thousand),
          "data/processed/region_population.csv")
write_csv(life, "data/processed/life_expectancy.csv")

# --- 5. Реляційна база SQLite -------------------------------------------------

db_path <- "data/ukraine_population.sqlite"
if (file.exists(db_path)) unlink(db_path)

con <- dbConnect(SQLite(), db_path)
dbExecute(con, "PRAGMA foreign_keys = ON")

dbExecute(con, "CREATE TABLE sources (
  source_id  INTEGER PRIMARY KEY,
  title      TEXT NOT NULL,
  url        TEXT NOT NULL,
  scraped_at TEXT NOT NULL
)")

dbExecute(con, "CREATE TABLE country_stats (
  year                INTEGER PRIMARY KEY,
  population_thousand REAL,
  births              INTEGER,
  deaths              INTEGER,
  natural_increase    INTEGER,
  birth_rate          REAL,
  death_rate          REAL,
  natural_rate        REAL,
  fertility           REAL,
  source_id           INTEGER REFERENCES sources(source_id)
)")

dbExecute(con, "CREATE TABLE regions (
  region_id INTEGER PRIMARY KEY,
  name      TEXT NOT NULL UNIQUE
)")

dbExecute(con, "CREATE TABLE region_population (
  region_id           INTEGER NOT NULL REFERENCES regions(region_id),
  year                INTEGER NOT NULL,
  population_thousand REAL,
  source_id           INTEGER REFERENCES sources(source_id),
  PRIMARY KEY (region_id, year)
)")

dbExecute(con, "CREATE TABLE life_expectancy (
  year      INTEGER NOT NULL,
  sex       TEXT NOT NULL CHECK (sex IN ('male', 'female')),
  value     REAL,
  source_id INTEGER REFERENCES sources(source_id),
  PRIMARY KEY (year, sex)
)")

sources <- tibble(
  source_id = 1:3,
  title = c("Населення України (Вікіпедія)",
            "Історична демографія України (Вікіпедія)",
            "Тривалість життя в Україні (Вікіпедія)"),
  url = c(url_country, url_regions, url_life),
  scraped_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
)

dbAppendTable(con, "sources", sources)
dbAppendTable(con, "country_stats", country %>% mutate(source_id = 1L))
dbAppendTable(con, "regions", regions)
dbAppendTable(con, "region_population", region_pop %>% mutate(source_id = 2L))
dbAppendTable(con, "life_expectancy", life %>% mutate(source_id = 3L))

cat("Готово. Рядків у БД:\n")
for (tb in c("country_stats", "regions", "region_population", "life_expectancy")) {
  cat(" ", tb, ":", dbGetQuery(con, paste("SELECT COUNT(*) AS n FROM", tb))$n, "\n")
}

dbDisconnect(con)
