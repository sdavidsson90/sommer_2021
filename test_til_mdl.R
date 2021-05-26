pacman::p_load(readr,tidyverse,RSQLite,utils,waldo,
               svDialogs,DescTools,zoo,textclean, rvest)

if (!exists("final_data_2")) {
  donorsChoose <- dbConnect(RSQLite::SQLite(), "SQLdatabase/donorsChoose.sqlite")
  final_data_2 <- dbGetQuery(donorsChoose, 'SELECT * FROM final_data_2')
  dbDisconnect(donorsChoose)
}


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


url = 'https://www.nationsonline.org/oneworld/US-states-by-area.htm'

region_table <- url %>%
  read_html() %>% 
  html_nodes("table") %>%
  html_table() %>% .[[3]]

region_table <- region_table[,c(3,7)] %>% 
  mutate(
    state = tolower(State),
    region = `Census Region`
  ) %>% 
  select(state,region)


states <- left_join(states_table,region_table)

states <- states %>% 
  arrange(længdegrad) %>% 
  mutate(region = ifelse(is.na(region)& lag(region,1) == lead(region,1),lead(region,1),region)) %>% 
  arrange(state)


navne_filter = c("donation_included_optional_donation",
                 "donation_amount", "project_type", "project_grade_level_category",
                 "project_resource_category", "project_cost", "project_current_status",
                 "donation_date", "donor_state",
                 "donor_is_teacher", "school_metro_type", "school_percentage_free_lunch","school_state",
                 "teacher_prefix", "teacher_first_project_posted_date", "donor_id", "project_id")

a <- final_data_2 %>% 
  select(all_of(navne_filter)) %>% 
  mutate_if(is.character,tolower) %>% 
  left_join(states[,c(1,4)], by=c("donor_state" = "state")) %>% 
  rename(donor_region = region) %>% 
  left_join(states[,c(1,4)], by=c("school_state" = "state")) %>% 
  rename(school_region = region) %>% 
  mutate(
    same_region= ifelse(donor_region == school_region,TRUE,FALSE),
    same_state = ifelse(donor_state == school_state,TRUE,FALSE)
  ) %>% 
  mutate_if(is.character,as.factor)
