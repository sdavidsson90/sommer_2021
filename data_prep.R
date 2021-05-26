pacman::p_load(readr,tidyverse,RSQLite,utils,waldo,
               svDialogs,DescTools,zoo,textclean)

# Denne fil skal køres umiddelbart efter data_load og med de variabler 
# der er i global environment efter data_load.R har kørt. tager ca. 800 sekunder

rm(list = ls()[ls() != "full_data"])

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



# Vi laver en funktion der skal bruges når vi samler vores data pr. donor.
# Hvis en donor har doneret til flere projekter på første dage. Finder vi den 
# oftest fremkommende værdi i kolonnen, hvis der 2 eller flere der optræder
# flest gange eller Mode() returnerer NA, skriver vi NA, ellers skriver vi mode
mode_test <- function(x) {
  ifelse(length(Mode(x)) > 1 | 
           is.na(Mode(x)[1]),NA,
         Mode(x))
}

# Vi laver en liste over de variabler vi ikke vil køre igennem vores funktioner
variabler = c("donor_id","more_donations","n_projects_donated_first",
         "project_id","donation_included_optional_donation",
         "donation_amount")


# Vi summerer al vores data pr. donor på den første donations dag for donorer 
# med mere end 1 donation på første dag.
  donors_first_donation_data_multi_group <- donors_first_donation_data_multi %>% 
    
    # Vi tager kun donorer der har doneret til mere end 1 projekt på første
    # dag og senere kobler vi disse sammen med donorer der kun har doneret til 
    # 1 projekt på første donationsdag.
    filter(n_projects_donated_first>1) %>% 
    
    group_by(donor_id) %>% 
    
    # Vi ændrer alle variabler undtagen de ovenfor definerede variabler
    mutate_at(vars(-variabler),
    
    # Hver af de valgte variabler udsættes for hhv. vores egen mode_test
    # og n_destinct som tæller antal unikke værdier
                  list(mode = mode_test,ndist = n_distinct)) %>%
    
    # Vi vælger kun de kolonner der er genereret af vores 2 ovenstående
    # funktioner og kolonnerne fra variabler
    select(ends_with("_mode"),ends_with("_ndist"),variabler) %>% 
    
    # Vi fjerner _mode fra variabel navnene, så de er får de originale navne
    rename_with(~ gsub("_mode", "", .x, fixed = TRUE), ends_with("mode")) %>% 
    
    # Vi sætter n_ foran variabler der slutter med _ndist
    rename_with(~ paste0("n_",.x), ends_with("_ndist")) %>%
    
    # Vi fjerner _ndist fra variabel navnene
    rename_with(~ gsub("_ndist", "", .x, fixed = TRUE), ends_with("_ndist")) %>% 
    
    mutate(
      # Laver et dato felt - da alle datoer pr. donor er på samme dag, 
      # bruger vi mean(gennemsnit)
      donation_date = as.Date(mean(doation_date)),
      
      # Vores true/false, igen har alle rækker pr. donor samme værdi
      # derfor bruger vi gennemsnit
      more_donations = mean(more_donations),
      
      # som ovenfor
      n_projects_donated_first = mean(n_projects_donated_first),
      
      donation_included_optional_donation = ifelse(is.na(Mode(donation_included_optional_donation)[1]),NA,Mode(donation_included_optional_donation)),
      
      # Hvad er den højeste donation, hvis der er lavet flere på første dag.
      donation_amount = max(ifelse(is.na(donation_amount), 0, donation_amount)),
      
      # Total sum doneret første dag.
      donation_amount_total = sum(donation_amount, na.rm = TRUE),
    ) %>% 
    select(-n_doation_date) %>% 
    slice(1) %>% 
    ungroup()
  
  # vi laver en funktion der sender 1 retur og 1 der sender inputtet
  # retur, så vi kan bruge disse i stedet for vores n_distinct og mode_test
  # Disse skal bruges på de data hvor donor_id kun optræder på 1 projekt på 
  # første donations dag.
  returner_1 <- function(x) 1
  returner_x <- function(x) x
  
    
 
# Her tager vi fat i alle donorer med 1 donation på første dag, vi sørger for at 
# oprette de samme variabler som oven for, her skal der heldig vis ikke bruges
# hverken Mode() eller n_distinct(), da vi ved der kun er 1 værdi pr. donor,
# og vi derfor bare kan 1 alle kolonner med n_ prefix.
  donors_first_donation_data_multi_single <- donors_first_donation_data_multi %>% 
    filter(n_projects_donated_first==1) %>% 
    
    mutate_at(vars(-variabler),
              
              # Hver af de valgte variabler udsættes for hhv. vores egen mode_test
              # og n_destinct som tæller antal unikke værdier
              list(mode = returner_x,ndist = returner_1)) %>%
    
    # Vi vælger kun de kolonner der er genereret af vores 2 ovenstående
    # funktioner og kolonnerne fra variabler
    select(ends_with("_mode"),ends_with("_ndist"),variabler) %>% 
    
    # Vi fjerner _mode fra variabel navnene, så de er får de originale navne
    rename_with(~ gsub("_mode", "", .x, fixed = TRUE), ends_with("mode")) %>% 
    
    # Vi sætter n_ foran variabler der slutter med _ndist
    rename_with(~ paste0("n_",.x), ends_with("_ndist")) %>%
    
    # Vi fjerner _ndist fra variabel navnene
    rename_with(~ gsub("_ndist", "", .x, fixed = TRUE), ends_with("_ndist")) %>%  
    
    mutate(
      # bruger vi mean(gennemsnit)
      donation_date = as.Date(doation_date),
      
      
      
      # Total sum doneret første dag.
      donation_amount_total = sum(donation_amount, na.rm = TRUE)
    ) %>% 
    select(-n_doation_date)  
  
  
  
  
  
  


# har vi det rigtige samlede antal donorer i de 2 data sæt inden vi samler dem
antal_forskellige_donorer == (nrow(donors_first_donation_data_multi_single) + nrow(donors_first_donation_data_multi_group))


waldo::compare(names(donors_first_donation_data_multi_single), names(donors_first_donation_data_multi_group))


# Data der indholder summeret data for første donations dag pr. donor
final_data_1 <- 
  rbind(donors_first_donation_data_multi_single,donors_first_donation_data_multi_group)
             
# Data der indholder data for første donations dag pr. donor, projekt data er
#  fra det project donoren donerede mest til, hvis donoren har doneret til 
# flere projekter på deres første donationsdag
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


donorsChoose <- dbConnect(RSQLite::SQLite(), "SQLdatabase/donorsChoose.sqlite")

# Vi tilføjer disse 2 vigtige tabeller til sql databasen
dbWriteTable(donorsChoose,"final_data_1",final_data_1, overwrite = T)
dbWriteTable(donorsChoose,"final_data_2",final_data_2, overwrite = T)

dbDisconnect(donorsChoose)




              