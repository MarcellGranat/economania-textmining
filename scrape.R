library(tidyverse)
library(magrittr)
library(rvest)
library(granatlib)

get_links <- (. %>% 
  read_html() %>% 
  html_nodes("a") %>% 
  html_attr("href") %>% 
  na.omit() %>% 
  unique() %>% 
  keep(str_detect, pattern = "https://economaniablog.hu/") %>% 
  keep(str_detect, pattern = "hu/tag/|hu/author/|hu/bloggerek/", negate = T) %>% 
  keep(str_detect, pattern = "hu/about/|hu/kapcsolat/|hu/tanulj_tolunk/|hu/szakmai_muhely/|hu/category/|#comments|hu/page/", negate = T) %>% 
  setdiff("https://economaniablog.hu/")) %>% 
  possibly(otherwise = NA_character_)



article_links <- str_c("https://economaniablog.hu/page/", 1:40, "/") %>% 
  map(get_links) %>% 
  reduce(c) %>% 
  na.omit()

economania_raw_df <- tibble(article_links) %>% 
  splitted_mutate(
    p = map(article_links, read_html),
    text = map(p, ~ html_text(html_nodes(., "p"))),
    title = map(p, ~ html_text(html_nodes(., "h1.entry-title"))),
    title = map_chr(title, first),
    author = map_chr(p, ~ html_text(html_nodes(., ".byline a"))),
  )

find_author <- function(author_condition, text) {
  author <- author_condition
  if (author_condition == "X") {
    author <- "X"
    for (i in x_authors) {
      if (any(str_detect(text, pattern = i))) {
        author <- i
        break
      }
    }
  }
  author
}

x_authors <- c("Granát Marcell",
               "Sulyok András", 
               "Marczis Dávid", 
               "Szabadkai Dániel",
               "Lóránt Balázs",
               "Nagy Olivér",
               "Malatinszky Gábor",
               "Várgedó Bálint",
               "Nagy Márton",
               "Bartha Kristóf",
               "Heilmann István",
               "Várgedő Bálint",
               "Kiss-Mihály Norbert",
               "Tófalvi Nóra",
               "Kálmán Péter",
               "Moldicz Csaba",
               "Nagy Benjámin",
               "Rácz Olivér",
               "Tóth Gábor",
               "Mikó Szabolcs",
               "Csontos Tamás Tibor",
               "Siket Bence",
               "Szalai Zoltán",
               "Horváth Dániel",
               "Hardi Zsuzsanna és Szapáry György",
               "Pavelka Alexandra",
               "Horváth Gábor",
               "Balázs Flóra",
               "Dancsik Bálint",
               "Marincsák Kálmán Árpád",
               "Farkas Sára és Gutpintér Júlia",
               "Palotai Dániel", 
               "dr. Csillik Péter és Gutpintér Júlia",
               "Oliver Knels")

economania_df <- economania_raw_df %>% 
  select(-p) %>% 
  distinct(article_links, .keep_all = TRUE) %>% 
  mutate(
    author_cleaned = map2_chr(author, text, find_author),
    text = map(text, setdiff, y = "Economania blog"),
    text = map(text, .f = function(t) discard(t, str_detect, "View all")),
    text = map(text, .f = function(t) discard(t, str_detect, "Főoldali kép forrása:")),
    text = map(text, .f = function(t) discard(t, str_detect, "Hozzászólások letiltva.")),
    text = map_chr(text, str_flatten, " "),
    text = map2_chr(text, author, ~ gsub(str_c(gsub("és.*", "", .y), ".*"), "", .x)),
    text = gsub(" Hivatkozások.*", "", text),
    text = gsub(" References.*", "", text),
    lang = textcat::textcat(text),
    time = str_extract(article_links, "20\\d\\d/\\d\\d/\\d\\d"),
    time = lubridate::ymd(time)
  )

save(economania_df, file = ".RData")
