pacman::p_load(readr,tidyverse,RSQLite,utils,waldo,
               svDialogs,DescTools,zoo,textclean)

# Denne fil skal køres umiddelbart efter data_load og med de variabler 
# der er i global environment efter data_load.R har kørt



# Vi tjekker om full_data er tilstede i vores envionment, 
# hvis ikke henter vi den via SQL
if (!exists("full_data")) {
  donorsChoose <- dbConnect(RSQLite::SQLite(), "SQLdatabase/donorsChoose.sqlite")
  full_data <- dbGetQuery(donorsChoose, 'SELECT * FROM full_data')
  dbDisconnect(donorsChoose)
}


# Der kan være flere forskellige donation_ids vedr. samme donor/projekt på
# samme dag, derfor summerer vi først beløbet fra disse forskellige 
# donation_ids på donor_id,project_id og doation_data. Så her definerer vi en
# donation som det samlede beløb doneret til et projekt på samme dag.
system.time(full_data_distinct <- full_data %>% 
  group_by(donor_id,project_id,school_id,doation_date) %>% 
  mutate(donation_amount = sum(donation_amount,na.rm = T)) %>% 
  select(donor_id,project_id,school_id,doation_date,donation_amount) %>% 
  ungroup() %>% 
  distinct())
    
# Er vores donation_amount det samme efter vi har summeret?
sum(full_data_distinct$donation_amount, na.rm = T) ==
sum(full_data$donation_amount, na.rm = T)


# hvor mange forskellige donorer og derved linjer bør vi ende op med?
antal_forskellige_donorer <- length(unique(full_data$donor_id))


rm(full_data2)

# 350 sekunder - Vi laver en tabel med alle unikke donor ids
# udfra vores full_data_distinct
system.time(donor_id_response  <-  full_data_distinct %>% 
 
  # Vi grupperer på donor 
  group_by(donor_id) %>% 
    
  # hvis der en donor er med i data mere end en gang
  # skriver vi TRUE, ellers er det en donor med kun 1 samlet donation(FALSE)
  mutate(more_donations = ifelse(n_distinct(project_id)>1 &
                                   n_distinct(doation_date)> 1,
                                 TRUE,FALSE)) %>% 
    
  # vi fjerner vores grupper
  ungroup() %>% 
    
  # Vi arrangerer rækkerne efter donor id og stigende doation_date
  arrange(donor_id,doation_date) %>% 
  
  # Vi grupperer erfter donor_id og doation_date
  group_by(donor_id,doation_date) %>% 
  
  # Vi tæller hvor mange forskellige project_ids der er på den unikke donor på 
  # den unikke dag. Da dette måske er en variabel der kan bruges til 
  # forudsigelse af vores y (more_donations)
  mutate(n_projects_donated_first = n_distinct(project_id)) %>% 
  
  # vi fjerner vores grupper
  ungroup() %>% 
  
  # Vi grupperer igen man nu kun på donor_id
  group_by(donor_id) %>% 
  
  # Vi vælger relevante kolonner
  select(donor_id,doation_date,more_donations,n_projects_donated_first) %>% 
  
  # Vi tager den første række i hver enkelt gruppe(unik donor_id), og da vi
  # øvest arrangerede efter stigende dato, vil den række vi får ud indeholde
  # datoen for donoren først donation
  slice(1) %>% 
  
  # For en god ordens skyld ungroup'er vi vores tibble
  ungroup())

# har vi lige så mange unikke donorer som vi har rækker i vores
# donor_id_response tibble og er det det samme antal unikke donorer, som
# er i vores full_data?
n_distinct(donor_id_response$donor_id) == nrow(donor_id_response)
n_distinct(donor_id_response$donor_id)== n_distinct(full_data$donor_id)


# Variabel med info om donor med flest donationer til forskellige projekter
# på første dag med donationer
(flest_donationer <-  donor_id_response %>% 
  filter(n_projects_donated_first == max(n_projects_donated_first)) %>%
  group_by(doation_date) %>% 
  slice(1) %>% 
  ungroup())


# Vi filtrerer på dataen oven for og ser at denne donor har lavet donationer
# til 19 forskellige projekter på den dag, hvor donoren lavede sin første
# donation, det samme antal vi kom frem til ovenfor.
full_data_distinct %>% 
  filter(donor_id ==  flest_donationer$donor_id, doation_date == flest_donationer$doation_date)


  full_data_distinct %>% 
  filter(donor_id == '6f74ffb17cbb2b616b1eef06bd4acd0c') %>% 
  group_by(donor_id) %>% 
  mutate(første_dag = ifelse(doation_date == min(doation_date),1,0)) %>% 
  ungroup() %>% 
  filter(første_dag == 1) %>% 
    select(-første_dag)
  


# Vi fjerner denne variabel igen
rm(flest_donationer)


donorsChoose <- dbConnect(RSQLite::SQLite(), "SQLdatabase/donorsChoose.sqlite")

# Vi tilføjer disse 2 vigtige tabeller til sql databasen
dbWriteTable(donorsChoose,"donor_id_response",donor_id_response, overwrite = T)
dbWriteTable(donorsChoose,"full_data_distinct",full_data_distinct, overwrite = T)

dbDisconnect(donorsChoose)


# Vi vil nu lave en tabel der kun indeholder information
# vores donorers første donationsdag. 
donors_first_donation_date_full <- 
  donor_id_response %>% 
  inner_join(full_data_distinct, by = c("doation_date" = "doation_date",
                                       "donor_id" = "donor_id")) %>% 
  arrange(desc(n_projects_donated_first)) 



# 450 sekunder - Da en del af vores donorer har doneret til mere end et unikt projekt på 
# den første dag de har doneret er vi nødt til at træffe nogle valg når
# vi skal koge data ned til 1 donor id med information om donorens donation
# på den første dag donoren har doneret. Vi er nødt til at summere rækkerne
# i donors_first_donation_date_full. 
donors_first_donation_data_multi <- donors_first_donation_date_full %>%
  left_join(full_data %>% 
              select(-donation_id,-donation_amount), by = c("donor_id", "doation_date", "project_id", "school_id")) %>% 
  unique() %>% 
  group_by(donor_id, project_id) %>% 
  slice(1) %>% 
  ungroup()



# Vi summerer al vores data pr. donor på den første donations dag for donorer 
# med mere end 1 donation på første dag. Vi bruger Mode på de fleste kolonner
# til at få den oftest forekommende værdi på alle kolonner, hvis Mode() returnerer
# mere end 1 værdi eller der er NA værdi i en af rækkerne, skriver vi NA, men ellers
# får vi den oftest forekommende værdi. Udover mode tæller vi antallet af 
# unikke værdier(n_distinct) for hver kolonne
  donors_first_donation_data_multi_group <- donors_first_donation_data_multi %>% 
    filter(n_projects_donated_first > 1 ) %>% 
  group_by(donor_id) %>% 
  mutate(   donation_date = as.Date(mean(doation_date)),
            more_donations = mean(more_donations),
            n_projects_donated_first = mean(n_projects_donated_first),
            donation_included_optional_donation = ifelse(is.na(Mode(donation_included_optional_donation)[1]),NA,Mode(donation_included_optional_donation)),
            donation_amount = max(ifelse(is.na(donation_amount), 0, donation_amount)),
            donation_amount_total = sum(donation_amount, na.rm = TRUE),
            school_id = ifelse(length(Mode(school_id)) > 1 | is.na(Mode(school_id)[1]),NA,Mode(school_id)),
            n_school_id = n_distinct(school_id),
            teacher_id = ifelse(length(Mode(teacher_id)) > 1 | is.na(Mode(teacher_id)[1]),NA,Mode(teacher_id)),
            n_teacher_id = n_distinct(teacher_id),
            teacher_project_posted_sequence = ifelse(length(Mode(teacher_project_posted_sequence)) > 1 | is.na(Mode(teacher_project_posted_sequence)[1]),NA,Mode(teacher_project_posted_sequence)),
            n_teacher_project_posted_sequence = n_distinct(teacher_project_posted_sequence),
            project_type = ifelse(length(Mode(project_type)) > 1 | is.na(Mode(project_type)[1]),NA,Mode(project_type)),
            n_project_type = n_distinct(project_type),
            project_title = ifelse(length(Mode(project_title)) > 1 | is.na(Mode(project_title)[1]),NA,Mode(project_title)),
            n_project_title = n_distinct(project_title),
            project_subject_category_tree = ifelse(length(Mode(project_subject_category_tree)) > 1 | is.na(Mode(project_subject_category_tree)[1]),NA,Mode(project_subject_category_tree)),
            n_project_subject_category_tree = n_distinct(project_subject_category_tree),
            project_subject_subcategory_tree = ifelse(length(Mode(project_subject_subcategory_tree)) > 1 | is.na(Mode(project_subject_subcategory_tree)[1]),NA,Mode(project_subject_subcategory_tree)),
            n_project_subject_subcategory_tree = n_distinct(project_subject_subcategory_tree),
            project_grade_level_category = ifelse(length(Mode(project_grade_level_category)) > 1 | is.na(Mode(project_grade_level_category)[1]),NA,Mode(project_grade_level_category)),
            n_project_grade_level_category = n_distinct(project_grade_level_category),
            project_resource_category = ifelse(length(Mode(project_resource_category)) > 1 | is.na(Mode(project_resource_category)[1]),NA,Mode(project_resource_category)),
            n_project_resource_category = n_distinct(project_resource_category),
            project_cost = ifelse(length(Mode(project_cost)) > 1 | is.na(Mode(project_cost)[1]),NA,Mode(project_cost)),
            n_project_cost = n_distinct(project_cost),
            project_current_status = ifelse(length(Mode(project_current_status)) > 1 | is.na(Mode(project_current_status)[1]),NA,Mode(project_current_status)),
            n_project_current_status = n_distinct(project_current_status),
            project_fully_funded_date = ifelse(length(Mode(project_fully_funded_date)) > 1 | is.na(Mode(project_fully_funded_date)[1]),NA,Mode(project_fully_funded_date)),
            n_project_fully_funded_date = n_distinct(project_fully_funded_date),
            donor_city = ifelse(length(Mode(donor_city)) > 1 | is.na(Mode(donor_city)[1]),NA,Mode(donor_city)),
            n_donor_city = n_distinct(donor_city),
            donor_state = ifelse(length(Mode(donor_state)) > 1 | is.na(Mode(donor_state)[1]),NA,Mode(donor_state)),
            n_donor_state = n_distinct(donor_state),
            donor_is_teacher = ifelse(length(Mode(donor_is_teacher)) > 1 | is.na(Mode(donor_is_teacher)[1]),NA,Mode(donor_is_teacher)),
            n_donor_is_teacher = n_distinct(donor_is_teacher),
            donor_zip = ifelse(length(Mode(donor_zip)) > 1 | is.na(Mode(donor_zip)[1]),NA,Mode(donor_zip)),
            n_donor_zip = n_distinct(donor_zip),
            project_posted_date = ifelse(length(Mode(project_posted_date)) > 1 | is.na(Mode(project_posted_date)[1]),NA,Mode(project_posted_date)),
            n_project_posted_date = n_distinct(project_posted_date),
            school_name = ifelse(length(Mode(school_name)) > 1 | is.na(Mode(school_name)[1]),NA,Mode(school_name)),
            n_school_name = n_distinct(school_name),
            school_metro_type = ifelse(length(Mode(school_metro_type)) > 1 | is.na(Mode(school_metro_type)[1]),NA,Mode(school_metro_type)),
            n_school_metro_type = n_distinct(school_metro_type),
            school_percentage_free_lunch = ifelse(length(Mode(school_percentage_free_lunch)) > 1 | is.na(Mode(school_percentage_free_lunch)[1]),NA,Mode(school_percentage_free_lunch)),
            n_school_percentage_free_lunch = n_distinct(school_percentage_free_lunch),
            school_state = ifelse(length(Mode(school_state)) > 1 | is.na(Mode(school_state)[1]),NA,Mode(school_state)),
            n_school_state = n_distinct(school_state),
            school_zip = ifelse(length(Mode(school_zip)) > 1 | is.na(Mode(school_zip)[1]),NA,Mode(school_zip)),
            n_school_zip = n_distinct(school_zip),
            school_city = ifelse(length(Mode(school_city)) > 1 | is.na(Mode(school_city)[1]),NA,Mode(school_city)),
            n_school_city = n_distinct(school_city),
            school_county = ifelse(length(Mode(school_county)) > 1 | is.na(Mode(school_county)[1]),NA,Mode(school_county)),
            n_school_county = n_distinct(school_county),
            school_district = ifelse(length(Mode(school_district)) > 1 | is.na(Mode(school_district)[1]),NA,Mode(school_district)),
            n_school_district = n_distinct(school_district),
            teacher_prefix = ifelse(length(Mode(teacher_prefix)) > 1 | is.na(Mode(teacher_prefix)[1]),NA,Mode(teacher_prefix)),
            n_teacher_prefix = n_distinct(teacher_prefix),
            teacher_first_project_posted_date = ifelse(length(Mode(teacher_first_project_posted_date)) > 1 | is.na(Mode(teacher_first_project_posted_date)[1]),NA,Mode(teacher_first_project_posted_date)),
            n_teacher_first_project_posted_date = n_distinct(teacher_first_project_posted_date)
         
            ) %>% 
  slice(1) %>% 
  ungroup()
  


# Her tager vi fat i alle donorer med 1 donation på første dag, vi sørger for at 
# oprette de samme variabler som oven for, her skal der heldig vis ikke bruges
# hverken Mode() eller n_distinct(), da vi ved der kun er 1 værdi pr. donor,
# og vi derfor bare kan 1 alle kolonner med n_ prefix.
donors_first_donation_data_multi_single <- donors_first_donation_data_multi %>% 
  filter(n_projects_donated_first==1) %>% 
  mutate(donor_id = donor_id,
         donation_date = as.Date(doation_date),
         more_donations = mean(more_donations),
         n_projects_donated_first = n_projects_donated_first,
         donation_included_optional_donation = donation_included_optional_donation,
         donation_amount = donation_amount,
         donation_amount_total = donation_amount,
         school_id = school_id,
         n_school_id = 1,
         teacher_id = teacher_id,
         n_teacher_id = 1,
         teacher_project_posted_sequence = teacher_project_posted_sequence,
         n_teacher_project_posted_sequence = 1,
         project_type = project_type,
         n_project_type = 1,
         project_title = project_title,
         n_project_title = 1,
         project_subject_category_tree = project_subject_category_tree,
         n_project_subject_category_tree = 1,
         project_subject_subcategory_tree = project_subject_subcategory_tree,
         n_project_subject_subcategory_tree = 1,
         project_grade_level_category = project_grade_level_category,
         n_project_grade_level_category = 1,
         project_resource_category = project_resource_category,
         n_project_resource_category = 1,
         project_cost = project_cost,
         n_project_cost = 1,
         project_current_status = project_current_status,
         n_project_current_status = 1,
         project_fully_funded_date = project_fully_funded_date,
         n_project_fully_funded_date = 1,
         donor_city = donor_city,
         n_donor_city = 1,
         donor_state = donor_state,
         n_donor_state = 1,
         donor_is_teacher = donor_is_teacher,
         n_donor_is_teacher = 1,
         donor_zip = donor_zip,
         n_donor_zip = 1,
         project_posted_date = project_posted_date,
         n_project_posted_date = 1,
         school_name = school_name,
         n_school_name = 1,
         school_metro_type = school_metro_type,
         n_school_metro_type = 1,
         school_percentage_free_lunch = school_percentage_free_lunch,
         n_school_percentage_free_lunch = 1,
         school_state = school_state,
         n_school_state = 1,
         school_zip = school_zip,
         n_school_zip = 1,
         school_city = school_city,
         n_school_city = 1,
         school_county = school_county,
         n_school_county = 1,
         school_district = school_district,
         n_school_district = 1,
         teacher_prefix = teacher_prefix,
         n_teacher_prefix = 1,
         teacher_first_project_posted_date = teacher_first_project_posted_date,
         n_teacher_first_project_posted_date = 1
  )
  


# har vi det rigtige samlede antal donorer i de 2 data sæt inden vi samler dem
antal_forskellige_donorer == (nrow(donors_first_donation_data_multi_single) + nrow(donors_first_donation_data_multi_group))


waldo::compare(names(donors_first_donation_data_multi_single), names(donors_first_donation_data_multi_group))


final_data_1 <- 
  rbind(donors_first_donation_data_multi_single,donors_first_donation_data_multi_group)
             
final_data_2 <- donors_first_donation_data_multi %>% 
  filter(n_projects_donated_first > 1) %>% 
  arrange(donor_id, desc(donation_amount)) %>% 
  group_by(donor_id) %>% 
  slice(1) %>% 
  ungroup() %>% 
  rbind(donors_first_donation_data_multi %>% 
          filter(n_projects_donated_first == 1)) %>% 
    mutate(donation_date = as.Date(doation_date)) %>% 
    select(-doation_date) %>% 
  left_join(final_data_1 %>% 
              select(donor_id,donation_amount_total))


# Hvor mange rækker har vi med komplet data
sum(complete.cases(final_data_1))
sum(complete.cases(final_data_2))






              