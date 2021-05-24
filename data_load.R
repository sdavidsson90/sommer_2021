library(readr);library(tidyverse);library(RSQLite);library(utils);library(waldo)
library(svDialogs);library(DescTools); library(zoo);library(textclean)
library(janitor)



# Fjerne evt. eksisterende data i Global Enviroment
rm(list = ls())






# Indlæse data fra csv/rds filer, Vi konstaterer at der er fejl i indlæsningerne
# fra resources og schools, derfor sletter vi disse igen og prøver at indlæse dem
# på anden måde.
donors <- read_csv("data/Donors.csv")
donation <- readRDS("data/donation.rds")
teachers <- read_csv("data/Teachers.csv")
resources <- read_csv("data/Resources.csv")
project <- readRDS("data/project.rds")
schools <- read_csv("data/Schools.csv")


rm(list = c("schools","resources"))



# vi læser hver enkelt linje ind som en tekststreng
res <- readLines(con <-  file("data/Resources.csv", encoding = "UTF-8", ))

# con lukkes og slettes da vi ikke skal bruge den mere.
close(con);rm(con)
# Hvis der er indlæst tomme linjer, sletter vi dem.
res = res[res != ""]

# Hvilke kolonne navne er der i første række af den indlæste data
(kolonnenavne = str_split(res[1],",")[[1]])



# Vi har indlæst 7.223.754 linjer inkl. overskrifterne

# Vi laver en kopi af vores linjer, som vi kan ændre lidt i for at kunne tjekke 
# om linjerne er komplette - forstået som at der er det antal kommaer, som der forventes.
res2 <- res %>% 
  
  # Hvis der er 2 stk anførselstegn ved siden af hinanden skal de slettes.
  str_replace_all('\"{2}','') %>% 
  
  # Tekststenge, med og uden unicode karakterer som starter og slutter med anførselstegn
  # erstattes, da der i sådanne kan indgå kommaer som vi ikke vil tælle med, når vi
  # vil tælle om hvor mange kommaer linjen indeholder.
  str_replace_all('\"[[[:graph:][:space:]]+[\u0001-\uFFFF]+]+\"','tekst med komma her')

# Vi har ændret i 1.806.154 linjer
sum(str_detect(res2,"tekst med komma her"))



# Hvor mange kommaer er der oftest pr. linje i vores indlæste data uden ændringer - talt på 50.000 tilfældigt udvalgte linjer?
# Vi bruger Mode-funktionen fra DescTools pakken til at tælle dette. Derud over tjekker vi at det antal
# også er det antal er der i dataens første række, som jo indeholder tabellens kolonnenavne.
(antal_kommaer_i_data <- Mode(str_count(res[sample(1:length(res), 50000)],","))[1])
(antal_kommaer_kolonne_overskrifter = str_count(res[1],","))
res[1]

# Det ser ud som om vi kan forvente 5 kolonner adskilt af 4 kommaer.
antal_kommaer_i_data == antal_kommaer_kolonne_overskrifter

# Hvordan ser de sidste tre rækker ud, som opfylder kravet om 4 kommaer. Ud fra viden om disse kan 
# vi prøve at bygge et regular expresion, som kan sortere komplette linjer fra ikke komplette linjer.
tail(res[str_count(res,",") == antal_kommaer_i_data], 3)

# Fælles for linjerne er:
# 1. Starter med 32 tegn med tal og bogstaver inden første komma: ^[:alnum:]{32},
# 2. en teksstreng med all mulige tegn:                           .*,
# 3. Noget der kan ligne antal for at være sikre laver 
# vi mulighed for at antal kan være et decimal tal:               ([0-9]+.[0-9]{1,2}|[0-9]+),
# 4. pris med mulighed for op til 999999.99 som ovenfor:         ([0-9]+.[0-9]{1,2}|[0-9]+),  
# 5. Slutter med en tekst der angiver en sælger:                              [[:graph:]+[:space:]*]+$

# Vi laver en TRUE/FALSE vektor der fortæller om en linje lver op til vores krav om:
# 1. Indeholder antal kommaer specificeret
# 2. Har et lige antal anførselstegn, så vi godkender kun tekster i vores data hvor der er 0 eller et lige antal anførselstegn

# vi laver en funktion med et TRUE/FALSE output der fortæller om vores tekststreng opfylder format kravene + indeholder 0 eller et lige antal ".
strict_test = function(x) {
  (str_detect(x,
              "^[:alnum:]{32},.+,([0-9]+.[0-9]{1,2}|[0-9]+),([0-9]+.[0-9]{1,2}|[0-9]+),[[:graph:]+[:space:]*]+$") & 
     (str_count(x,'\"') %% 2) == 0)
}

# vi kører vores indlæste data igennem strict_test funktionen. 
resources_ok <- strict_test(res)

# 7.114.606 af vores fra den originale data linjer ser ok ud.
sum(resources_ok)

# Hvor mange ser ok ud når vi kigger på vores modificerede data med den erstattede tekst-streng
# og vi kun går efter et korrekt antal kommaer og et lige antal anførselstegn?
resources_ok_res2 <-  str_count(res2, ",") == antal_kommaer_i_data & ((str_count(res2,'\"') %% 2) == 0)

# 7.197.169, så metode2 giver flere korrekte linjer. 
sum(resources_ok_res2)

# hvordan ser de 10 af de linjer ud, hvor vores to test metoder ikke er enige?
head(res[resources_ok_res2== TRUE & resources_ok == FALSE], 10)

# Er der nogen hvor vores meget restriktive regex test har sagt ok og den nemme løsning med antal kommaer
# ikke har sagt ok
head(res[resources_ok_res2== FALSE & resources_ok == TRUE], 10)

# Vi går med den knap så restriktive optælling af "gode" linjer (resources_ok_res2),
# da vi muligvis kan leve med manglende information i nogle kolonner(tekst), 
# og hvis det viser sig at det giver problemer kan vi altid sortere linjer 
# med manglende data fra på et senere tidspunkt.


# Hvordan ser "ikke ok" linjer ud i følge resources_ok_res2? 
# Det ser ud som om nogle linjer er blevet delt i 2.
head(res[resources_ok_res2== FALSE], 10)

# Vi laver en data frame med rækkenumrene på de linjer som ikke klarede den "nemme" test
# kolonnen række viser rækkenummeret i det originale dataset. Not_ok er lig med TRUE hvis rækken ikke bestod vores 
# resources_ok_res2 test
rækkenumre_med_problemer <-  data.frame(not_ok = !`resources_ok_res2`, række = 1:length(resources_ok_res2)) %>% 
  # Filter så vi kun ser linjer der ikke er ok.
  filter(not_ok == 1) %>% 
  # tabellen skal kun indeholde kolonnen række
  select(række) 



# Vi laver en ny data frame baseret på rækkenumre_med_problemer og i denne data frame vil vi kæde
# linjer sammen for at se om vi kan sammensætte linjer der efterfølger hinanden til tekststrenge 
# der kan bruges som reel data.
tabel1 <- 
  rækkenumre_med_problemer %>% 
  
  mutate(
    
    # kolonne med originalt rækkenummer på den seneste række
    forrige_række = lag(række,1),
    
    # kolonne med originalt rækkenummer på næste række
    næste_række = lead(række,1),
    
    # kolonne der fortæller om tesksten i denne rækkes originale rækkenummer starter med 32 tal/bogstaver
    # før et komma, og derved kan forventes at være starten på en reel række.
    start = ifelse(str_detect(res2[række],"^[:alnum:]{32},"),
                   TRUE,FALSE),
    
    # kolonne der tæller rækkens nummer i denne nye data frame.
    tabelrække = 1:n(),
    
    # Hvis næste række er en række med start == TRUE eller denne række er den sidste række
    # så skriv originalt rækkenummer ellers NA.
    slut_række = ifelse(lead(start,1)== TRUE | 1:n() == n(),række,NA)
  ) %>% 
  
  # filtrer rækker hvor kolonnen start er TRUE, start er TRUE i den næste række
  # eller hvis det er den sidste række i denne tabel.
  filter(start == TRUE | lead(start,1)== TRUE | 1:n() == n()) %>% 
  mutate(
    
    # udfyld slut_række med værdien i slut_række for den næste linje.
    # det vil sige hvis rækken er en start == TRUE, så henter den rækkenummret
    # i den næste række, som er en slut_række.
    slut_række = lead(slut_række,1)
  ) %>% 
  
  # Vis kun rækker hvor start er TRUE, da vi er interesseret i at have start og slut for 1 linje
  # 1 række
  filter(start == TRUE) %>% 
  # Vis kun kolonnerne række og slut_række
  select(række,slut_række) %>% 
  
  mutate(
    # kolonne der fortæller hvor mange rækker der i mellem vores række og slut_række
    antal = slut_række-række+1,
    # en tækker på hvor mange rækker vi har
    no = 1:n(),
    # kolonne der skal bruges til at holde vores konstruerede tekst
    tekst = NA
  ) %>% 
  # sorter så rækken med størst difference mellem række og slut_række er størst
  arrange(desc(antal))


# Vi skal ikke bruge res2 mere, så for at frigive plads(1.3 GB) sletter vi den
rm(res2)

# Hvordan ser vores tabel ud? Tanken er at række skal fortælle hvor vores ikke komplette
# tekst starter og slut række fortæller hvilken række der sandsynlig vis er
# den sidste række som hører til denne tekst. Antal er en numeriske forskel mellem række og
# slut_række.
head(tabel1,10)

# Vi ser hvordan vores første konstruerede række vil komme til at se ud, og konstaterer at
# dette ligner en farbar vej at gå. Paste funktionen bruges til at binde tekst sammen.
# Collapse kommandoen gør at vi kan sende en vektor med n linjer ind og få en samlet streng tilbage,
# opdelt med kommaer.
paste(res[tabel1$række[1]:tabel1$slut_række[1]],
      collapse = ',')


# For hver række i vores tabel1 indsætter vi en tekst i kolonnen tekst
# Teksten konstrueres vist ovenfor.
for (x in 1:nrow(tabel1)) {
  tabel1$tekst[x] = paste(res[tabel1$række[x]:tabel1$slut_række[x]],
                          collapse = ',')
}

# Vi fjerner alle anførselstegn fra vores konstruerede tekster
(tabel1$tekst <- 
    tabel1$tekst %>% 
    str_replace_all('\"','')) %>% head(2)

# Vi tester, hvor mange af vores 13.280 rækker der lever op
# vores strict_test + vores ønskede antal kommaer
(godkendte = sum((strict_test(tabel1$tekst) & str_count(tabel1$tekst,",") == antal_kommaer_i_data)))

# Vi tjekker hvordan vil de nye tekster, som klarer vores stict_test og kun har 4 kommaer se ud når det 
# køres igennem read_csv funktionen. Det ser korrekt ud med tekst kolonner og numeriske kolonner hvor de skal være.
read_csv(tabel1$tekst[(strict_test(tabel1$tekst) & 
                         str_count(tabel1$tekst,",") == antal_kommaer_i_data)],
         col_names = kolonnenavne) 


# Vi laver en ny tabel til de af vores tekster, der endnu ikke kan klare strict_test'en + komma testen
tabel2 = tabel1 %>% 
  filter(!strict_test(tabel1$tekst) |  str_count(tabel1$tekst,",") != antal_kommaer_i_data)

# Er vores antal godkendte rækker + antal rækker i den nye tabel lig med antal samlede rækker i tabel1
godkendte + nrow(tabel2) == nrow(tabel1)

# Vi fjerner de ikke godkendte tekster fra tabel1 så den kun indeholder tekster
# der kan bruges.
tabel1 = anti_join(tabel1,tabel2)

# Hvad ligner det at problemet med tabel2 teksterne er? Det ligner at der er alt 
# for mange kommaer, og de er ikke pakket ind i anførselstegn. Hvis vi pakker 
# vareteksten ind i 2 anførselstegn, så kan vi måske bruge teksterne.  
tabel2$tekst[1:2]

# Vi opretter en 3. tabel der kan modtage eventuelle tekster, som ikke kan 
# repareres af vores forsøg med indpakning i anførselstegn.
tabel3 = tabel2[0,]

# Loop'e igennem hver række i tabel2
for (x in 1:nrow(tabel2)) {
  
  # Vi bruger et regular expresion til at søge efter tekster der opfylder vores 
  # krav. nchar funktionen bruges til at tælle antal tegn, 
  # grepl ikke kan arbejde med strenge der fylder mere end 256bytes og 
  # 1 byte er ca. = 1 tegn, da vi ikke har æøå og
  # sandsynligvis heller ikke andre byte "tunge" tegn med.
  if(!is.na(str_locate(tabel2$tekst[x],
                       "(?<=^[:alnum:]{32},.{20,250}),[:digit:]*\\.?[:digit:]+,[:digit:]*\\.*[:digit:]+,.+$")[1,1]) &
     nchar(tabel2$tekst[x])< 256){
    
    # Grepl funktion til at indsætte et " før det komma, som kommer inden vores 
    # Resource Quantity, Resource Unit Price, Resource Vendor Name. Str_locate
    # finder indeks på hvor kommaet ligger, og vi indsætter " på dennes position
    tabel2$tekst[x] <-  gsub(paste0("^(.{",
                                    (str_locate(tabel2$tekst[x],
                                                "(?<=^[:alnum:]{32},.{20,250}),[:digit:]*\\.?[:digit:]+,[:digit:]*\\.*[:digit:]+,.+$")[1,1])-1
                                    ,"})(.*)$"),"\\1\"\\2", tabel2$tekst[x])
    
    # Grepl funktion til at indsætte et " efter det komma, som kommer efter de 32 tal/bogstaver, 
    # som udgør vores Project ID, Str_locate inder indeks på hvor kommaet ligger,
    # og vi indsætter " på dennes position
    tabel2$tekst[x] <- gsub(paste0("^(.{",
                                   (str_locate(tabel2$tekst[x],
                                               "(?<=^[:alnum:]{32},).{20,250},[:digit:]*\\.?[:digit:]+,[:digit:]*\\.*[:digit:]+,.+$")[1,1])-1
                                   ,"})(.*)$"),"\\1\"\\2", tabel2$tekst[x])
  } else {
    
    # Linjer som ikke kører gennem vores if sendes over i en ny tabel
    tabel3 = rbind(tabel3,tabel2[x,])
  }
}

# Vi fjerner linjer der er tilføjet til tabel3 fra tabel2
tabel2 = anti_join(tabel2,tabel3)

# Vi tjekker tabel2 i forhold til konvertering.
# Vores kolonner er i de forventede formater og det ser pænt ud igen.
read_csv(tabel2$tekst[strict_test(tabel2$tekst)], col_names = kolonnenavne)


# Vi har nu tabel3 med 2 linjer, og problemet i forhold til vores loop ovenfor
# er tekstens længde, som gør at vi ikke kan bruge grepl til at indpakke 
# Resource Item Name i anførselstegn.
nchar(tabel3$tekst)

# Men det ser ud som om de kommaer, der kan give os problemer alle
# efterfølges af et mellemrum, så det kan vi prøve at erstatte.
# Vores brugbare kommaer, som splitter har ikke mellemrum efter sig.

# Find streng med sammenhængende komma og mellemrum
str_view_all(tabel3$tekst,", ")
# find kommaer som ikke efterfølges et mellemrum
str_view_all(tabel3$tekst,",(?! )")

# Vi udskifter ", " med " - "
tabel3$tekst = str_replace_all(tabel3$tekst,", "," - ")

# Som forventet kan linjerne nu indlæses korrekt.
read_csv(tabel3$tekst,col_names = kolonnenavne)


# Nu samler vi vores tekster til en lang vektor med tekststrenge 
# inden vi indlæser dem som csv.
resources_samlet <- 
  res[resources_ok_res2]  %>% 
  append(tabel1$tekst) %>% 
  append(tabel2$tekst) %>% 
  append(tabel3$tekst)

# Vi har 13.305 færre linjer i vores bearbejde data
# end vi havde efter vi slettede tomme linjer fra den originale indlæsning.
length(res) - length(resources_samlet)

# Vi indlæser vores linjer til en tibble.
resources = read_csv(resources_samlet)

# Som forventet er kolonnerne i de data formater, som passer.
str(resources)

# Vi indlæser dataene direkte til csv, for at se om vi har gjort det bedre.
test = read_csv("data/Resources.csv")

# de 50 første rækker i kolonneet er ens, så vi satser på at 
# vi har lavet vores data korrekt
head(resources$`Project ID`,50) == head(test$`Project ID`,50)

# Vi kan jo ikke være sikre på at vi har fået bedre data ud af vores arbejde, 
# men de 2 test nedenfor fortæller at vi har flere rækker med data i kolonnen 
# Resource Quantity og vi har fået et større samlet antal i Resource Quantity 
# kolonnen ved at lave arbejdet, så vi tror på at data indeholder mere information.
length(is.na(test$`Resource Quantity`));length(is.na(resources$`Resource Quantity`))
sum(test$`Resource Quantity`, na.rm = T);sum(resources$`Resource Quantity`, na.rm = T)

# Vi rydder op i global environment, og sletter det der ikke har med vores 
# færdige data at gøre
rm(list = c(ls()[!ls() %in% c("resources","schools","donors",
                              "teachers","project","donation")]))

# Indlæsning af schools data, vi har tidligere konstateret at der er fejl, hvis denne
# data indlæses direke gennem read_csv, så vi bruger readlines funktionen igen
schools <- readLines("data/schools.csv")
schools[1]


# Vi laver en variabel der fortæller det antal kommaer der typisk er pr. linje 
# i vores data.
komma_schools = Mode(str_count(schools,","))

# Vi tager alle linjer som ikke indeholder det rigtige antal kommaer, inden da 
# sørger vi for at tekst, som er pakket ind i 2 anførselstegn, ændres til tekst, 
# for at sikre at eventuelle kommaer mellem 2 anførselstegn ikke tælles med.
(schools_ikke_ok <- 
    schools[str_count(str_replace_all(schools,',\"[[:graph:][:blank:]]+\",',',tekst,'),",") != komma_schools])

# Samme test som ovenfor her gemmer vi bare de linjer som opfylder testen(==).
schools <- 
  schools[str_count(str_replace_all(schools,',\"[[:graph:][:blank:]]+\",',',tekst,'),",") == komma_schools]


# Da vi så på schools_ikke_ok ovenfor så det ud som om vi kun havde to linjer og 
# at de til sammen udgjorde en samlet linje, så dem prøver vi at samle til en linje.
(schools_ikke_ok <- str_replace_all(paste(schools_ikke_ok, collapse = ","),'\"',''))

# Har vi det antal kommaer som vi forventer
str_count(schools_ikke_ok,",") == komma_schools

# Vi sætter de 2 vektorer sammen til en.
schools  = append(schools,schools_ikke_ok)

# Vi indlæser vores vektor til csv uden fejl.
schools = read_csv(schools)


# Vi slette alt i vores environment som ikke er vores 6 objekter.
rm(list = c(ls()[!ls() %in% c("resources","schools","donors",
                              "teachers","project","donation")]))


# Lave en vektor med navne på vores 6 elementer
names <- ls()


# For loop til at fjerne mellemrum og store bogstaver i kolonne navne.
# Denne metode er ikke så effektiv, da den kopierer objekterne for at 
# kunne omdøbe kolonnerne, men med de datasæt vi har her, kan det gøres uden 
# de store problemer.
for (navn in names) {
  
  # kopier objektet til a via get funktionen
  a = get(navn)
  
  # ændre kolonnenavnene i a til snake_case via make_clean_names
  names(a) = make_clean_names(names(get(navn)),"snake")
  
  # kopier a tilbage i vores originale objekt
  assign(navn, a)
  
  # slet a
  rm(a)
  
}

# Oprette mappe til at holde på vores SQL database, hvis ikke dne eksisterer
if (!dir.exists("SQLdatabase")) dir.create("SQLdatabase")


# Tilgå Database, første gang koden køres laves databasen(donorsChoose) automatisk
donorsChoose <- dbConnect(RSQLite::SQLite(), "SQLdatabase/donorsChoose.sqlite")

# Disconnecte fra databasen
dbDisconnect(donorsChoose)


donorsChoose <- dbConnect(RSQLite::SQLite(), "SQLdatabase/donorsChoose.sqlite")

# for loop til at skrive vores data.frames, indlæst tidligere, til tabeller i databasen,
# hvis tabellen eksisterer i forvejen overskriver vi, således at data ikke duplikeres
for (navn in names) {
  dbWriteTable(donorsChoose, navn, get(navn), overwrite = T)
}

# Er der de tabeller i databasen som vi forventer
dbListTables(donorsChoose)

# compare funktionen fra Waldo pakken kan sammenligne 2 objekter og fortælle
# om de er ens.
compare(sort(dbListTables(donorsChoose)),sort(names))

# For loop til at tjekke at der er data i vores tabeller
# for (name in dbListTables(donorsChoose)) {
# print(name)
#   print(dbGetQuery(donorsChoose, paste0('SELECT * FROM ',name,' LIMIT 5')))
#     
# }

dbDisconnect(donorsChoose)

rm(list = ls())

donorsChoose <- dbConnect(RSQLite::SQLite(), "SQLdatabase/donorsChoose.sqlite")


# For loop til at hente data.frames ned fra SQL og assigne dem i R med
# tabellens navn fra SQLite databasen
for (name in dbListTables(donorsChoose)) {
  assign(name, dbGetQuery(donorsChoose, paste0('SELECT * FROM ',name,';')))
  
}

dbDisconnect(donorsChoose)

# Joine data fra alle tabeller undtagen resources

# metode 1: left_join-funktion fra dplyr
# system.time-funktionen er tilføjet for at vise tiden der bruges på at køre
# den samlede kommando
system.time(
  full_data <-  donation %>% 
    left_join(donors) %>% 
    left_join(project) %>% 
    left_join(schools) %>% 
    left_join(teachers)
)



# metode 2: LEFT JOIN i SQL - OBS project data-framen indeholder kun 1 variabel
# (`Project Posted Date`) som ikke optræder i donation data-framen
donorsChoose <- dbConnect(RSQLite::SQLite(), "SQLdatabase/donorsChoose.sqlite")
system.time(full_data2 <- dbGetQuery(donorsChoose, 'SELECT * FROM donation a
          LEFT JOIN donors b ON a.donor_id = b.donor_id
          LEFT JOIN project c on a.project_id = c.project_id AND a.school_id = c.school_id AND a.teacher_id = c.teacher_id
          AND a.teacher_project_posted_sequence = c.teacher_project_posted_sequence 
          AND a.project_type = c.project_type AND a.project_title = c.project_title AND
          a.project_subject_category_tree = c.project_subject_category_tree AND
          a.project_subject_subcategory_tree = c.project_subject_subcategory_tree AND
          a.project_grade_level_category = c.project_grade_level_category
          AND a.project_resource_category = c.project_resource_category AND
          a.project_cost = c.project_cost AND a.project_current_status = c.project_current_status
          AND a.project_fully_funded_date = c.project_fully_funded_date
          LEFT JOIN schools d ON a.school_id = d.school_id
          LEFT JOIN teachers e ON a.teacher_id = e.teacher_id;
           '))
# I SQL joinet beholder man alle kolonner fra alle tabeller, derfor er der
# dubletter af alle kolonner som er brugt i vores joins, dem sorterer vi fra ved
# at lave en vektor med unikke kolonne navne
kolonner = names(full_data2) %>% unique()

# og bruger vi select fra dplyr, så vi kun vælger
# den første af hver af de kolonner, der optræde rmere end en gang
full_data2 <- full_data2 %>% 
  select(kolonner)

head(full_data, 10000) == head(full_data2,10000)

# Er der forskel på at bruge de 2 metoder. Vi har de samme variabel navne og
# det samme antal rækker, så umiddelbart ikke. 
# Men at joine via dplyr var hurtigere med en faktor +16, 
# så den metode er at foretrække i dette tilfælde, specielt
# når der ikke er oprettet index eller keys på SQL tabellerne.
compare(names(full_data),names(full_data2))
compare(nrow(full_data),nrow(full_data2))


donorsChoose <- dbConnect(RSQLite::SQLite(), "SQLdatabase/donorsChoose.sqlite")

# Vi tilføjer full data til tabellen
dbWriteTable(donorsChoose,"full_data",full_data, overwrite = T)



dbDisconnect(donorsChoose)

