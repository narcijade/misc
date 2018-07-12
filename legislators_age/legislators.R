library(tidyverse)
library(lubridate)
library(cowplot)
library(yaml)
#I need to import the yaml files so I can pull out term information
#https://github.com/unitedstates/congress-legislators


historical <- read_yaml("https://theunitedstates.io/congress-legislators/legislators-historical.yaml")
current <- read_yaml("https://theunitedstates.io/congress-legislators/legislators-current.yaml")

# convert YAML into table
#current[[1]]$terms[[1]]$start
#"1993-01-05"
#> current[[1]]$terms[[1]]$end
#"1995-01-03"
#> current[[1]]$terms[[1]]$party
#"Democrat"
#> current[[1]]$terms[[1]]$type
#"rep"
#> current[[1]]$bio$birthday
#"1952-11-09"
#current[[1]]$name$first
#"Sherrod"
#current[[1]]$name$last
#"Brown"

# for every term create a line in the table with each value.
# map works, but tricky with terms
#current %>% map("name") %>% map("first")

tmp <- lapply(seq(1:length(current)), function(i) {
   lapply(seq(1:length(current[[i]]$terms)), function(j) {
     c(current[[i]]$name$first,
     current[[i]]$name$last,
     current[[i]]$bio$birthday,
     current[[i]]$terms[[j]]$start,
     current[[i]]$terms[[j]]$end,
     current[[i]]$terms[[j]]$type,
     current[[i]]$terms[[j]]$party)
   })
})
tmp2 <- unlist(tmp, recursive=FALSE)

current.df <- data.frame(do.call(rbind, tmp2))
colnames(current.df) <- c("first", "last", "birthday", "term_start", "term_end", "type", "party")



# !!!! do somethign if hte birthday is missing) DISCARD?
tmp <- lapply(seq(1:length(historical)), function(i) {
  lapply(seq(1:length(historical[[i]]$terms)), function(j) {
    
    c(historical[[i]]$name$first,
      historical[[i]]$name$last,
      paste0(historical[[i]]$bio$birthday,"x"),
      historical[[i]]$terms[[j]]$start,
      historical[[i]]$terms[[j]]$end,
      historical[[i]]$terms[[j]]$type,
      historical[[i]]$terms[[j]]$party)
  })
})
tmp2 <- unlist(tmp, recursive=FALSE)

historical.df <- data.frame(do.call(rbind, tmp2))
colnames(historical.df) <- c("first", "last", "birthday", "term_start", "term_end", "type", "party")

df <- rbind(historical.df, current.df)

df <- df %>% 
  mutate(birthday = str_replace(birthday, "x", "")) %>%
  mutate(birthday = ymd(birthday)) %>%
  mutate(term_start = ymd(term_start)) %>%
  mutate(term_end = ymd(term_end)) %>%
  mutate(name = paste0(last, ",", first)) %>%
  select(-first, -last) %>%
  mutate(birth_decade = str_replace(year(birthday), ".$", "0")) %>%
  filter(! is.na(birthday)) 


years <- seq(from = 1790, to=2017, by = 1)


all_years <- lapply(years, function(x) {
  filter(df, ymd(x, truncated = 2L) >= term_start & ymd(x, truncated = 2L) <= term_end) %>%
    mutate(year = x)
   
})

all.df <- do.call(rbind, all_years)

counts.df <- all.df %>% 
  group_by(year, birth_decade) %>%
  summarise(count=n()) 


ggplot(counts.df, aes(x=year, y=count, fill=birth_decade)) +
  geom_area(stat='identity') +
  ylab("number of legislators") +
  ggtitle("Number of legislators by decade of birth (1920-2017)")
