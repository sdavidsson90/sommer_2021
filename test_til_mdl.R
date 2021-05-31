pacman::p_load(readr,tidyverse,RSQLite,utils,waldo, caret,doParallel, speedglm,
               svDialogs,DescTools,zoo,textclean, rvest, usmap, parallel,randomForest)

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
  dplyr::select(-State)



url = 'https://en.m.wikipedia.org/wiki/List_of_geographic_centers_of_the_United_States'


table_list <- url %>%
  read_html() %>% 
  html_nodes("table") %>%
  html_table()

(states_table = table_list[[2]] %>% 
    mutate(breddegrad = as.numeric(str_extract(Coordinates,'(?<=; ).{7}')),
           længdegrad = as.numeric(str_extract(Coordinates,'.{7}(?=;)')),
           state = tolower(`State orfederal district`))   %>% 
    dplyr::select(state,længdegrad,breddegrad) )

states <- left_join(regions,states_table)


complete.cases(states)

navne_filter = c('donation_included_optional_donation',
  'donation_amount', 'project_grade_level_category',
  'project_resource_category', 'project_cost', 'project_current_status', 'donation_date', 'donor_state', 
  'school_metro_type', 'school_percentage_free_lunch', 'school_state',
  'teacher_prefix', 'teacher_first_project_posted_date', 'donor_id', 'project_id','more_donations')




a <- final_data_2 %>% 
  dplyr::select(all_of(navne_filter)) %>% 
  dplyr::select(-donor_id,-project_id,-project_current_status,-donation_date,
                -teacher_first_project_posted_date,-school_percentage_free_lunch) %>% 
  na.omit() %>%
  mutate_if(is.character,tolower) %>% 
  left_join(states[,c(2,4)], by=c("donor_state" = "state")) %>% 
  rename(donor_region = Region) %>% 
  left_join(states[,c(2,4)], by=c("school_state" = "state")) %>% 
  rename(school_region = Region) %>% 
  mutate(
    same_region= ifelse(donor_region == school_region,TRUE,FALSE),
    same_state = ifelse(donor_state == school_state,TRUE,FALSE),
    project_resource_category = ifelse(project_resource_category %in% c("supplies","books","technology"),
                                       project_resource_category,"other"),
    donor_region = ifelse(is.na(donor_region),NA,donor_region),
    same_region = ifelse(is.na(same_region),NA,ifelse(
    same_region == TRUE, "yes","no")),
    project_cost = log(as.numeric(str_remove(str_remove(project_cost,"\\$"),","))),
    teacher_prefix = ifelse(teacher_prefix %in% c("mrs.","mr.","ms."),
                                       teacher_prefix,NA),
    school_metro_type = ifelse(school_metro_type == 'unknown',NA,school_metro_type),
    project_cost = log(project_cost)
    
  ) %>% 
  dplyr::select(-donor_state,-school_state) %>% 
  na.omit() %>% 
  mutate_if(is.character,as.factor) %>% 
  as_tibble()


plot_usmap(regions = "states", data = states, values = "Region") + 
  theme(legend.position = "right") +
  scale_fill_discrete(name = "Region")

