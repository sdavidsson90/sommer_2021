pacman::p_load(readr,tidyverse,RSQLite,utils,waldo,
               svDialogs,DescTools,zoo,textclean, rvest, usmap)

if (!exists("final_data_2")) {
  donorsChoose <- dbConnect(RSQLite::SQLite(), "SQLdatabase/donorsChoose.sqlite")
  final_data_2 <- dbGetQuery(donorsChoose, 'SELECT * FROM final_data_2')
  dbDisconnect(donorsChoose)
}

url = 'https://raw.githubusercontent.com/cphalpert/census-regions/master/us%20census%20bureau%20regions%20and%20divisions.csv'


regions <- read_csv(url(url))

regions <- regions %>% 
  mutate(
    state = tolower(State)
  ) %>% 
  rename(state_code =`State Code`) %>% 
  select(-State)



url = 'https://en.m.wikipedia.org/wiki/List_of_geographic_centers_of_the_United_States'


table_list <- url %>%
  read_html() %>% 
  html_nodes("table") %>%
  html_table()

(states_table = table_list[[2]] %>% 
    mutate(breddegrad = as.numeric(str_extract(Coordinates,'(?<=; ).{7}')),
           længdegrad = as.numeric(str_extract(Coordinates,'.{7}(?=;)')),
           state = tolower(`State orfederal district`))   %>% 
    select(state,længdegrad,breddegrad) )

states <- left_join(regions,states_table)


complete.cases(states)

navne_filter = c("donation_included_optional_donation",
                 "donation_amount", "project_type", "project_grade_level_category",
                 "project_resource_category", "project_cost", "project_current_status",
                 "donation_date", "donor_state",
                 "donor_is_teacher", "school_metro_type", "school_percentage_free_lunch","school_state",
                 "teacher_prefix", "teacher_first_project_posted_date", "donor_id", "project_id")

a <- final_data_2 %>% 
  select(all_of(navne_filter)) %>% 
  mutate_if(is.character,tolower) %>% 
  left_join(states[,c(2,4)], by=c("donor_state" = "state")) %>% 
  rename(donor_region = Region) %>% 
  left_join(states[,c(2,4)], by=c("school_state" = "state")) %>% 
  rename(school_region = Region) %>% 
  mutate(
    same_region= ifelse(donor_region == school_region,TRUE,FALSE),
    same_state = ifelse(donor_state == school_state,TRUE,FALSE)
  ) %>% 
  mutate_if(is.character,as.factor) %>% 
  as_tibble()

plot_usmap(regions = "states", data = states, values = "Region") + 
  theme(legend.position = "right") +
  scale_fill_discrete(name = "Region")

