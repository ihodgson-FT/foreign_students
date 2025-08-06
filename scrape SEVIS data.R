library(tidyverse)
library(janitor)
library(rvest)

url <- "https://studyinthestates.dhs.gov/sevis-by-the-numbers/sevis-by-the-numbers-data"

# Read the page
page <- read_html(url)

# Extract all anchor tag hrefs
links <- page %>%
  html_elements("a") %>%
  html_attr("href") %>%
  na.omit() %>%
  unique()

# If you want only full URLs (not relative links)
full_links <- links[str_detect(links, "sevis-data-mapping-tool")]
full_links <- full_links[!str_detect(full_links, "stem")]
full_links <- full_links[-1]

# Look at the first few
head(full_links)

root_url <- "https://studyinthestates.dhs.gov"
df_data <- lapply(full_links,
                  function(x){
                    # x = full_links[16]
                    url <- paste0(root_url, x)
                    page <- read_html(url)

                    tbls <- page %>% html_table(fill = T)
                    df_tbl <- tbls[[1]]
                    if (str_squish(names(df_tbl)[1]) != "Country of Citizenship") {
                      names(df_tbl) <- df_tbl[1,]
                      df_tbl <- df_tbl[-1,]
                      df_tbl <- type_convert(df_tbl)
                      }
                    df_tbl <- df_tbl %>%
                      clean_names %>%
                      mutate(month_year = str_extract(url, "[a-z]*-[0-9]{4}"),
                               date = my(month_year))
                    }) %>%
  bind_rows()
write_csv(df_data, "data/SEVIS/sevis_data_raw.csv")
df_data <- read_csv("data/SEVIS/sevis_data_raw.csv") %>% 
  mutate(country_of_citizenship = country_of_citizenship %>% 
           str_remove_all("\n") %>% 
           str_squish(),
         number_of_active_students = ifelse(is.na(number_of_active_students), 
                                            active_students, 
                                            number_of_active_students)) %>% 
  filter(!is.na(number_of_active_students))

# trends for top 10 countries
mean_students <- df_data %>% group_by(country_of_citizenship) %>% 
  summarise(mean_number = mean(doctorate, na.rm =T)) %>% 
  arrange(desc(mean_number))

df_data %>% 
  filter(country_of_citizenship %in% mean_students$country_of_citizenship[1:10]) %>% 
  ggplot() + 
  geom_smooth(aes(x = date, y = doctorate)) + 
  geom_point(aes(x = date, y = doctorate), alpha = .5) + 
  facet_wrap(facets = "country_of_citizenship", ncol = 2, scales = "free") + 
  theme_minimal()

ggsave("output/charts/number_of_doctorate_students_SEVIS.pdf")



