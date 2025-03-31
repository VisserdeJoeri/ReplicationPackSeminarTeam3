#Access all necessary libraries 
library(haven)
library(readxl)
library(dplyr)
library(tidyr)
library(writexl)

#The data that Novosad et al. used, we use this to compare our constructed aggregated data with theirs
US_raw_data = read_dta("C:/Users/michi/OneDrive/Documenten/Eur Jaar 3/Blok 4/Major/Data US/mortality_by_ed_group.dta")

##############################################

#Here we change the US_raw_data such that we can add our 'new' results
# Identify columns to keep unchanged
added_columns = c("year", "edclass", "age", "race", "sex")

# Identify other columns to set to NA
other_columns = setdiff(names(US_raw_data), added_columns)

# Create a data frame for new years (2016-2021)
create_years = expand.grid(
  year = 2016:2021,
  edclass = unique(US_raw_data$edclass),
  age = unique(US_raw_data$age),
  race = unique(US_raw_data$race),
  sex = unique(US_raw_data$sex)
)

# Add NA columns for all other variables
for (col in other_columns) {
  create_years[[col]] = NA
}

# Combine with the original dataset
full_data = bind_rows(US_raw_data, create_years) %>%
  #Add a column for corona mortalities
  mutate(mortrate_v = NA, 
         tmort_v = NA,
         tmort_icd10a = NA,
         tmort_icd10b = NA,
         tmort_icd10d = NA,
         tmort_icd10e = NA,
         tmort_icd10f = NA,
         tmort_icd10g = NA,
         tmort_icd10h = NA,
         tmort_icd10i = NA,
         tmort_icd10j = NA,
         tmort_icd10k = NA,
         tmort_icd10l = NA,
         tmort_icd10m = NA,
         tmort_icd10n = NA,
         tmort_icd10o = NA,
         tmort_icd10p = NA,
         tmort_icd10q = NA,
         tmort_icd10r = NA,
         tmort_or = NA,
         tmort_r = NA,
         mortrate_icd10a = NA,
         mortrate_icd10b = NA,
         mortrate_icd10d = NA,
         mortrate_icd10e = NA,
         mortrate_icd10f = NA,
         mortrate_icd10g = NA,
         mortrate_icd10h = NA,
         mortrate_icd10i = NA,
         mortrate_icd10j = NA,
         mortrate_icd10k = NA,
         mortrate_icd10l = NA,
         mortrate_icd10m = NA,
         mortrate_icd10n = NA,
         mortrate_icd10o = NA,
         mortrate_icd10p = NA,
         mortrate_icd10q = NA,
         mortrate_icd10r = NA,
         mortrate_or = NA,
         mortrate_r = NA
         ) %>%  #set  to zero: Years until 2019 will never have corona mortality
  arrange(year)  # Ensure data is ordered by year


#Extended the dataframe by total mortality
mortrate_cols = c(grep("mortrate_", names(full_data), value = TRUE))

for (col in mortrate_cols) {
  new_col = sub("mortrate_", "tmort_", col)
  full_data[[new_col]] = ifelse(is.na(full_data$tpop), NA, 
                                    full_data[[col]] * full_data$tpop / 100000)
}

full_data = as.data.frame(full_data)

#This one can be used, and later compared to full_data
original_full_data = full_data

#Deleting all the existing values on mortality and population from the file
original_full_data = original_full_data %>%
  mutate(across(-c("year", "edclass", "race", "age", "sex"), ~ NA))

#original_full_data = read.csv("C:/Users/michi/OneDrive/Documenten/Eur Jaar 3/Blok 4/Major/Data US/data_numbers2017_2021.csv", header = TRUE, sep = ";")

##############################################

#Here we start the function that reads and shapes the yearly data
struct9298 = function(df, year){
  #origin
  #remove all the foreigners from the data
  
  df = df[df$restatus <= 3,]
  
  #Remove people from Oklahoma, Georgia, Rhode Island and South Carolina
  #df = df %>% filter(!(df$staters %in% c(11, 38, 41, 42)))
  
  #age
  #keep the people from correct age group(s) in the data (age 25-29 until 70-74)
  df = df[df$ager52>=31 & df$ager52 <= 40,]
  
  #Convert the numerical values to age-groups
  df <- df %>%
    mutate(agegr = case_when(
      ager52 == 31 ~ "25-29",
      ager52 == 32 ~ "30-34",
      ager52 == 33 ~ "35-39",
      ager52 == 34 ~ "40-44",
      ager52 == 35 ~ "45-49",
      ager52 == 36 ~ "50-54",
      ager52 == 37 ~ "55-59",
      ager52 == 38 ~ "60-64",
      ager52 == 39 ~ "65-69",
      ager52 == 40 ~ "70-74",
      TRUE         ~ NA_character_
    ))
  #df$ager52 = as.numeric(df$ager52)
  #sex
  #As long as M = 1 and F = 2 its fine
  if (any(df$sex == "M")) {
    stop("Error: Column 'sex' contains 'M'.")
  }
  
  #race
  #Create 'binary' columns for race
  df = df %>% 
      mutate(
        WNH = ifelse(df$hspanicr == 6, 1, 0),
        BNH = ifelse(df$hspanicr == 7, 1, 0),
        H = ifelse(df$hspanicr <= 5, 1, 0),
        O = ifelse(df$hspanicr == 8, 1, 0),
        #quickly add the year as well
        year = year
      )
  df = df[df$hspanicr != 9, ]
  #'easier' implementable origin ranks as well
  df = df %>%
    mutate(origin = case_when(
      WNH == 1 ~ 1,
      BNH == 1 ~ 2,
      H == 1 ~ 3,
      O == 1 ~ 4
    ))
  
  #education
  #Constructing the education rank for each level, based on the 1989 notation
  df = df %>%
    mutate(eduRank = case_when(
      educ <= 11 ~ 1,
      educ == 12 ~ 2,
      educ <= 15 ~ 3,
      educ <= 17 ~ 4,
      TRUE ~ 9
    ))
  
  
  #Cause of death
  #Create binary variables for the main cause of death -> using the same numbers as Novosad et al. used with ICD-9n data
  #Poisoning
  df = df %>% 
      mutate(
        #Create a variable for all the deceased
        one = 1,
        #Poisoning cases in icd-9: E850-E860 and E980 (Alcohol and drug poisoning with undetermined intent)
        #As explained in the description of NVSS, the data uses E800-E900 instead of 800-900
        poison = case_when(
          ucod >=8500 & ucod<= 8609 ~ 1,
          ucod >=9800 & ucod<= 9804 ~ 1,
          TRUE ~ 0
          ),
        #Deze is nog wel even een dingetje, kijken we nu naar 950.0-959.9 0f E950.0-E959.9?
        suicide = case_when(
          ucod >= 9500 & ucod <= 9599 ~ 1,
          TRUE ~ 0
          ),
        #Liver ziet er kloppend uit in vergelijking met de wikipediapagina
        liver = case_when(
          ucod >= 5710 & ucod<= 5719 ~ 1,
          TRUE ~ 0
          ),
        #Zelfde geldt voor Heart
        heart = case_when(
          ucod >= 3900 & ucod <= 4299 ~ 1,
          TRUE ~ 0
          ),
        #Ook (Lung)Cancer
        cancer = case_when(
          ucod >= 1400 & ucod <= 2089 ~ 1,
          TRUE ~ 0
        ),
        lungCancer = case_when(
          ucod >= 1622 & ucod <= 1629 ~ 1,
          TRUE ~ 0
        ),
        #cerebrovascular ook prima gedefinieerd
        cereb = case_when(
          ucod >= 4300 & ucod <= 4389 ~ 1,
          TRUE ~ 0
        ),
        #CLRD
        clrd = case_when(
          ucod >= 4900 & ucod <= 4969 ~ 1,
          TRUE ~ 0
        ),
        v = case_when(
          ucod < 0 ~ 1, #Should never happen
          TRUE ~ 0
        ))
  
  df = df %>% 
    mutate(
      #accidents/war
      accidents = case_when(
        ucod >= 8000 & ucod <= 9999 & poison + liver + suicide == 0 ~ 1,
        TRUE ~ 0,
      ),,
      #Add the deaths of despair
      despair = ifelse(poison + suicide + liver == 1, 1, 0),
      #Other diseases
      other_diseases = ifelse(accidents + clrd + cereb + cancer + heart + liver + suicide + poison == 0, 1, 0)
    )
  
  #Dan nog de lijst met alle andere diseases
  
  df = df %>% select(year, eduRank, ager52, agegr, origin, sex, poison, liver, despair, v, suicide, heart, accidents, clrd, cereb, cancer, lungCancer, other_diseases, one)
  #en alle toe te voegen death causes
  return(df)
}

struct9902 = function(df, year){
  #origin
  #remove all the foreigners from the data
  
  df = df[df$restatus <= 3, ]
  
  #age
  #keep the people from correct age group(s) in the data (age 25-29 until 70-74)
  df = df[df$ager52>=31 & df$ager52 <= 40,]
  
  #Convert the numerical values to age-groups
  df <- df %>%
    mutate(agegr = case_when(
      ager52 == 31 ~ "25-29",
      ager52 == 32 ~ "30-34",
      ager52 == 33 ~ "35-39",
      ager52 == 34 ~ "40-44",
      ager52 == 35 ~ "45-49",
      ager52 == 36 ~ "50-54",
      ager52 == 37 ~ "55-59",
      ager52 == 38 ~ "60-64",
      ager52 == 39 ~ "65-69",
      ager52 == 40 ~ "70-74",
      TRUE         ~ NA_character_
    ))
  #df$ager52 = as.numeric(df$ager52)
  #sex
  #As long as M = 1 and F = 2 its fine
  if (any(df$sex == "M")) {
    df = df %>% 
      mutate( sex = case_when(
        substr(sex,1,1) %in% "M" ~ 1,
        TRUE ~ 2
      ))
  }
  
  #race
  #Create 'binary' columns for race
  df = df %>% 
    mutate(
      WNH = ifelse(df$hspanicr == 6, 1, 0),
      BNH = ifelse(df$hspanicr == 7, 1, 0),
      H = ifelse(df$hspanicr <= 5, 1, 0),
      O = ifelse(df$hspanicr == 8, 1, 0),
      #quickly add the year as well
      year = year
    )
  df = df[df$hspanic != 9, ]
  #'easier' implementable origin ranks as well
  df = df %>%
    mutate(origin = case_when(
      WNH == 1 ~ 1,
      BNH == 1 ~ 2,
      H == 1 ~ 3,
      O == 1 ~ 4
    ))
  
  #education
  #Constructing the education rank for each level, based on the 1989 notation
  df = df %>%
    mutate(eduRank = case_when(
      educ <= 11 ~ 1,
      educ == 12 ~ 2,
      educ <= 15 ~ 3,
      educ <= 17 ~ 4,
      TRUE ~ 9
    ))
  
  
  #Cause of death
  #Create binary variables for the main cause of death -> using the same numbers as Novosad et al. used with ICD-9n data
  #Poisoning
  df = df %>% 
    mutate(
      #Create a variable for all the deceased
      one = 1,
      poison = case_when(
        substr(ucod,1,3) %in% paste0("Y", 10:15) ~ 1,
        substr(ucod,1,3) %in% paste0("X", 40:45) ~ 1,
        substr(ucod,1,3) %in% paste0("Y", seq(45, 49, by = 2)) ~ 1,
        TRUE ~ 0
      ),
      
      suicide = case_when(
        substr(ucod,1,3) %in% paste0("X", 60:84) ~ 1,
        substr(ucod,1,4) %in% paste0("Y870") ~ 1,
        TRUE ~ 0
      ),
      #Liver ziet er kloppend uit in vergelijking met de wikipediapagina
      liver = case_when(
        substr(ucod,1,3) %in% paste0("K70") ~ 1,
        substr(ucod,1,4) %in% paste0("K70", 0:9) ~ 1,
        substr(ucod,1,4) %in% paste0("K7", 30:49) ~ 1,
        TRUE ~ 0
      ),
      #Zelfde geldt voor Heart
      heart = case_when(
        substr(ucod,1,4) %in% paste0("I0", 10:99) ~ 1,
        substr(ucod,1,4) %in% paste0("I00", 0:9) ~ 1,
        substr(ucod,1,3) %in% paste0("I", seq(11,13,by = 2)) ~ 1,
        substr(ucod,1,4) %in% paste0("I11", 0:9) ~ 1,
        substr(ucod,1,4) %in% paste0("I13", 0:9) ~ 1,
        substr(ucod,1,3) %in% paste0("I", 20:51) ~ 1,
        substr(ucod,1,4) %in% paste0("I", 200:519) ~ 1,
        TRUE ~ 0
      ),
      #Ook (Lung)Cancer
      cancer = case_when(
        substr(ucod,1,1) %in% paste0("C") ~ 1,
        TRUE ~ 0
      ),
      lungCancer = case_when(
        substr(ucod,1,3) %in% paste0("C34") ~ 1,
        TRUE ~ 0
      ),
      #cerebrovascular ook prima gedefinieerd
      cereb = case_when(
        substr(ucod,1,3) %in% paste0("I", 60:69) ~ 1,
        TRUE ~ 0
      ),
      #CLRD
      clrd = case_when(
        substr(ucod,1,3) %in% paste0("J", 40:47) ~ 1,
        TRUE ~ 0
      ),
      v = case_when(
        substr(ucod, 1, 4) %in% "U071" ~ 1,
        TRUE ~ 0
      ))
  
  df = df %>% 
    mutate(
      #Add the deaths of despair
      despair = case_when(
        poison + suicide + liver == 1 ~ 1,
        TRUE ~ 0),
      #accidents/war
      accidents = case_when(
        substr(ucod,1,1) %in% c("V","W","X","Y") & (despair == 0) ~ 1,
        TRUE ~ 0,
      ),
      #Other diseases
      other_diseases = ifelse(accidents + clrd + cereb + cancer + heart + liver + suicide + poison == 0, 1, 0)
    )
  
  df = df %>% 
    mutate(
      icd10a = case_when(
        substr(ucod, 1, 1) == "A" & other_diseases == 1 ~ 1,
        TRUE ~ 0
      ),
      icd10b = case_when(
        substr(ucod, 1, 1) == "B" & other_diseases == 1 ~ 1,
        TRUE ~ 0
      ),
      icd10d = case_when(
        substr(ucod, 1, 1) == "D" & other_diseases == 1 ~ 1,
        TRUE ~ 0
      ),
      icd10e = case_when(
        substr(ucod, 1, 1) == "E" & other_diseases == 1 ~ 1,
        TRUE ~ 0
      ),
      icd10f = case_when(
        substr(ucod, 1, 1) == "F" & other_diseases == 1 ~ 1,
        TRUE ~ 0
      ),
      icd10g = case_when(
        substr(ucod, 1, 1) == "G" & other_diseases == 1 ~ 1,
        TRUE ~ 0
      ),
      icd10h = case_when(
        substr(ucod, 1, 1) == "H" & other_diseases == 1 ~ 1,
        TRUE ~ 0
      ),
      icd10i = case_when(
        substr(ucod, 1, 1) == "I" & other_diseases == 1 ~ 1,
        TRUE ~ 0
      ),
      icd10j = case_when(
        substr(ucod, 1, 1) == "J" & other_diseases == 1 ~ 1,
        TRUE ~ 0
      ),
      #Other deceases in the respiratory system
      other_resp = case_when(
        substr(ucod, 1, 1) == "J" & other_diseases == 1 ~ 1,
        TRUE ~ 0
      ),
      icd10k = case_when(
        substr(ucod, 1, 1) == "K" & other_diseases == 1 ~ 1,
        TRUE ~ 0
      ),
      icd10l = case_when(
        substr(ucod, 1, 1) == "L" & other_diseases == 1 ~ 1,
        TRUE ~ 0
      ),
      icd10m = case_when(
        substr(ucod, 1, 1) == "M" & other_diseases == 1 ~ 1,
        TRUE ~ 0
      ),
      icd10n = case_when(
        substr(ucod, 1, 1) == "N" & other_diseases == 1 ~ 1,
        TRUE ~ 0
      ),
      icd10o = case_when(
        substr(ucod, 1, 1) == "O" & other_diseases == 1 ~ 1,
        TRUE ~ 0
      ),
      icd10p = case_when(
        substr(ucod, 1, 1) == "P" & other_diseases == 1 ~ 1,
        TRUE ~ 0
      ),
      icd10q = case_when(
        substr(ucod, 1, 1) == "Q" & other_diseases == 1 ~ 1,
        TRUE ~ 0
      ),
      icd10r = case_when(
        substr(ucod, 1, 1) == "R" & other_diseases == 1 ~ 1,
        TRUE ~ 0
      ),
      regular = case_when(
        heart + cancer == 1 ~ 1,
        TRUE ~ 0
      ),
      other_viral = case_when(
        substr(ucod, 1, 1) %in% c("A","B") & other_diseases == 1 ~ 1,
        TRUE ~ 0
      )
    )
  
  #Dan nog de lijst met alle andere diseases
  
  df = df %>% select(year, eduRank, ager52, agegr, origin, sex, poison, liver, suicide, despair, v,  heart, accidents, clrd, cereb, cancer, lungCancer, other_diseases, one,
                     icd10a, icd10b, icd10d, icd10e, icd10f, icd10g, icd10h, icd10i, icd10j, icd10k, icd10l, icd10m, icd10n, icd10o, icd10p, icd10q, icd10r, other_viral, other_resp, regular)
  return(df)
}

struct0321 = function(df, year){
  #origin
  #remove all the foreigners from the data
  
  df = df[df$restatus <= 3, ]
  
  #age
  #keep the people from correct age group(s) in the data (age 25-29 until 70-74)
  df = df[df$ager52>=31 & df$ager52 <= 40,]
  
  #Convert the numerical values to age-groups
  df <- df %>%
    mutate(agegr = case_when(
      ager52 == 31 ~ "25-29",
      ager52 == 32 ~ "30-34",
      ager52 == 33 ~ "35-39",
      ager52 == 34 ~ "40-44",
      ager52 == 35 ~ "45-49",
      ager52 == 36 ~ "50-54",
      ager52 == 37 ~ "55-59",
      ager52 == 38 ~ "60-64",
      ager52 == 39 ~ "65-69",
      ager52 == 40 ~ "70-74",
      TRUE         ~ NA_character_
    ))
  #df$ager52 = as.numeric(df$ager52)
  #sex
  #As long as M = 1 and F = 2 its fine
  if (any(df$sex == "M")) {
    df = df %>% 
      mutate( sex = case_when(
        substr(sex,1,1) %in% "M" ~ 1,
        TRUE ~ 2
      ))
  }
  
  #race
  #Create 'binary' columns for race
  if(!all(is.na(df$hspanicr))){
    df = df %>% 
      mutate(
        WNH = ifelse(df$hspanicr == 6, 1, 0),
        BNH = ifelse(df$hspanicr == 7, 1, 0),
        H = ifelse(df$hspanicr <= 5, 1, 0),
        O = ifelse(df$hspanicr == 8, 1, 0),
        #quickly add the year as well
        year = year
      )
    df = df[df$hspanic != 9, ]
    #'easier' implementable origin ranks as well
    df = df %>%
      mutate(origin = case_when(
        WNH == 1 ~ 1,
        BNH == 1 ~ 2,
        H == 1 ~ 3,
        O == 1 ~ 4
      )) 
  } else {
    df <- df %>%
      mutate(
        origin = case_when(
          hispanic %in% 100:199 & race40 == 1 ~ 1,
          hispanic %in% 100:199 & race40 == 2 ~ 2,
          hispanic %in% 200:299 ~ 3,
          hispanic %in% 100:199 & race40 %in% 3:40 ~ 4,
          TRUE ~ 9
        )
      )
    df = df[df$origin != 9,]
  }
    
  #education
  #Constructing the education rank for each level, based on the 1989 and 2003 notation
  if (any(colnames(df) == "educ89")){
    df <- df %>%
      mutate(
        eduRank = case_when(
          educflag == 0 & educ89 <= 11 ~ 1,
          educflag == 0 & educ89 == 12 ~ 2,
          educflag == 0 & educ89 <= 15 ~ 3,
          educflag == 0 & educ89 <= 17 ~ 4,
          educflag == 1 & educ <= 2 ~ 1,
          educflag == 1 & educ == 3 ~ 2,
          educflag == 1 & educ <= 5 ~ 3,
          educflag == 1 & educ <= 8 ~ 4,
          TRUE ~ 9
        )
      )
  } else if (any(colnames(df) == "educ1989" & colnames(df) == "educ2003")){
    df <- df %>%
      mutate(
        eduRank = case_when(
          educflag == 0 & educ1989 <= 11 ~ 1,
          educflag == 0 & educ1989 == 12 ~ 2,
          educflag == 0 & educ1989 <= 15 ~ 3,
          educflag == 0 & educ1989 <= 17 ~ 4,
          educflag == 1 & educ2003 <= 2 ~ 1,
          educflag == 1 & educ2003 == 3 ~ 2,
          educflag == 1 & educ2003 <= 5 ~ 3,
          educflag == 1 & educ2003 <= 8 ~ 4,
          TRUE ~ 9
        )
        #In 2021 the educ1989 is completely deleted from the data
      )
    }else if (any(colnames(df) == "educ2003")){
        df <- df %>%
          mutate(
            eduRank = case_when(
              educflag == 1 & educ2003 <= 2 ~ 1,
              educflag == 1 & educ2003 == 3 ~ 2,
              educflag == 1 & educ2003 <= 5 ~ 3,
              educflag == 1 & educ2003 <= 8 ~ 4,
              TRUE ~ 9
            )
          )
  } else {
    stop("No correct colnames for education")
  }
  
  #Cause of death
  #Create binary variables for the main cause of death -> using the same numbers as Novosad et al. used with ICD-9n data
  #Poisoning
  df = df %>% 
    mutate(
      #Create a variable for all the deceased
      one = 1,
      #Poisoning cases in icd-9: E850-E860 and E980 (Alcohol and drug poisoning with undetermined intent)
      poison = case_when(
        substr(ucod,1,3) %in% paste0("Y", 10:15) ~ 1,
        substr(ucod,1,3) %in% paste0("X", 40:45) ~ 1,
        substr(ucod,1,3) %in% paste0("Y", seq(45, 49, by = 2)) ~ 1,
        TRUE ~ 0
      ),
      #Deze is nog wel even een dingetje, kijken we nu naar 950.0-959.9 0f E950.0-E959.9?
      suicide = case_when(
        substr(ucod,1,3) %in% paste0("X", 60:84) ~ 1,
        substr(ucod,1,4) %in% paste0("Y870") ~ 1,
        TRUE ~ 0
      ),
      #Liver ziet er kloppend uit in vergelijking met de wikipediapagina
      liver = case_when(
        substr(ucod,1,3) %in% paste0("K70") ~ 1,
        substr(ucod,1,4) %in% paste0("K70", 0:9) ~ 1,
        substr(ucod,1,4) %in% paste0("K7", 30:49) ~ 1,
        TRUE ~ 0
      ),
      #Zelfde geldt voor Heart
      heart = case_when(
        substr(ucod,1,4) %in% paste0("I0", 10:99) ~ 1,
        substr(ucod,1,4) %in% paste0("I00", 0:9) ~ 1,
        substr(ucod,1,3) %in% paste0("I", seq(11,13,by = 2)) ~ 1,
        substr(ucod,1,4) %in% paste0("I11", 0:9) ~ 1,
        substr(ucod,1,4) %in% paste0("I13", 0:9) ~ 1,
        substr(ucod,1,3) %in% paste0("I", 20:51) ~ 1,
        substr(ucod,1,4) %in% paste0("I", 200:519) ~ 1,
        TRUE ~ 0
      ),
      #Ook (Lung)Cancer
      cancer = case_when(
        substr(ucod,1,1) %in% paste0("C") ~ 1,
        TRUE ~ 0
      ),
      lungCancer = case_when(
        substr(ucod,1,3) %in% paste0("C34") ~ 1,
        TRUE ~ 0
      ),
      #cerebrovascular ook prima gedefinieerd
      cereb = case_when(
        substr(ucod,1,3) %in% paste0("I", 60:69) ~ 1,
        TRUE ~ 0
      ),
      #CLRD
      clrd = case_when(
        substr(ucod,1,3) %in% paste0("J", 40:47) ~ 1,
        TRUE ~ 0
      ),
      #v = virus (COVID)
      v = case_when(
        substr(ucod, 1, 4) %in% "U071" ~ 1,
        TRUE ~ 0
      ))
  
   df = df %>% 
    mutate(
      #Add the deaths of despair
      despair = case_when(
        poison + suicide + liver == 1 ~ 1,
        TRUE ~ 0),
      #accidents/war
      accidents = case_when(
        substr(ucod,1,1) %in% c("V","W","X","Y") & (despair == 0) ~ 1,
        TRUE ~ 0,
      ),
      #Other diseases
      other_diseases = ifelse(accidents + clrd + cereb + cancer + heart + liver + suicide + poison == 0, 1, 0)
    )
  
  df = df %>% 
    mutate(
      icd10a = case_when(
        substr(ucod, 1, 1) == "A" & other_diseases == 1 ~ 1,
        TRUE ~ 0
      ),
      icd10b = case_when(
        substr(ucod, 1, 1) == "B" & other_diseases == 1 ~ 1,
        TRUE ~ 0
      ),
      icd10d = case_when(
        substr(ucod, 1, 1) == "D" & other_diseases == 1 ~ 1,
        TRUE ~ 0
      ),
      icd10e = case_when(
        substr(ucod, 1, 1) == "E" & other_diseases == 1 ~ 1,
        TRUE ~ 0
      ),
      icd10f = case_when(
        substr(ucod, 1, 1) == "F" & other_diseases == 1 ~ 1,
        TRUE ~ 0
      ),
      icd10g = case_when(
        substr(ucod, 1, 1) == "G" & other_diseases == 1 ~ 1,
        TRUE ~ 0
      ),
      icd10h = case_when(
        substr(ucod, 1, 1) == "H" & other_diseases == 1 ~ 1,
        TRUE ~ 0
      ),
      icd10i = case_when(
        substr(ucod, 1, 1) == "I" & other_diseases == 1 ~ 1,
        TRUE ~ 0
      ),
      icd10j = case_when(
        substr(ucod, 1, 1) == "J" & other_diseases == 1 ~ 1,
        TRUE ~ 0
      ),
      #Other deceases in the respiratory system
      other_resp = case_when(
        substr(ucod, 1, 1) == "J" & other_diseases == 1 ~ 1,
        TRUE ~ 0
      ),
      icd10k = case_when(
        substr(ucod, 1, 1) == "K" & other_diseases == 1 ~ 1,
        TRUE ~ 0
      ),
      icd10l = case_when(
        substr(ucod, 1, 1) == "L" & other_diseases == 1 ~ 1,
        TRUE ~ 0
      ),
      icd10m = case_when(
        substr(ucod, 1, 1) == "M" & other_diseases == 1 ~ 1,
        TRUE ~ 0
      ),
      icd10n = case_when(
        substr(ucod, 1, 1) == "N" & other_diseases == 1 ~ 1,
        TRUE ~ 0
      ),
      icd10o = case_when(
        substr(ucod, 1, 1) == "O" & other_diseases == 1 ~ 1,
        TRUE ~ 0
      ),
      icd10p = case_when(
        substr(ucod, 1, 1) == "P" & other_diseases == 1 ~ 1,
        TRUE ~ 0
      ),
      icd10q = case_when(
        substr(ucod, 1, 1) == "Q" & other_diseases == 1 ~ 1,
        TRUE ~ 0
      ),
      icd10r = case_when(
        substr(ucod, 1, 1) == "R" & other_diseases == 1 ~ 1,
        TRUE ~ 0
      ),
      regular = case_when(
        heart + cancer == 1 ~ 1,
        TRUE ~ 0
      ),
      other_viral = case_when(
        substr(ucod, 1, 1) %in% c("A","B") & other_diseases == 1 ~ 1,
        TRUE ~ 0
      ))
      
  #Dan nog de lijst met alle andere diseases
  
  df = df %>% select(year, eduRank, ager52, agegr, origin, sex, poison, liver, suicide, despair, v, heart, accidents, clrd, cereb, cancer, lungCancer, other_diseases, one,
                     icd10a, icd10b, icd10d, icd10e, icd10f, icd10g, icd10h, icd10i, icd10j, icd10k, icd10l, icd10m, icd10n, icd10o, icd10p, icd10q, icd10r, other_viral, other_resp, regular)
  return(df)
}

aggYear = function(df, pop_data){
  #Group the data such that each cell specifies a different disease
  df = df %>%
    group_by(year, origin, agegr, sex, eduRank) %>%
    summarise(
      tMort = sum(one, na.rm = TRUE),
      poisMort = sum(poison, na.rm = TRUE),
      liverMort = sum (liver, na.rm = TRUE),
      suicideMort = sum(suicide, na.rm = TRUE),
      despMort = sum(despair, na.rm = TRUE),
      vMort = sum(v, na.rm = TRUE),
      cancerMort = sum(cancer, na.rm = TRUE),
      lungCancerMort = sum(lungCancer, na.rm = TRUE),
      heartMort = sum(heart, na.rm = TRUE),
      accidentsMort = sum(accidents, na.rm = TRUE),
      clrdMort = sum(clrd, na.rm = TRUE),
      cerebMort = sum(cereb, na.rm = TRUE),
      icd10aMort = sum(icd10a, na.rm = TRUE),
      icd10bMort = sum(icd10b, na.rm = TRUE),
      icd10dMort = sum(icd10d, na.rm = TRUE),
      icd10eMort = sum(icd10e, na.rm = TRUE),
      icd10fMort = sum(icd10f, na.rm = TRUE),
      icd10gMort = sum(icd10g, na.rm = TRUE),
      icd10hMort = sum(icd10h, na.rm = TRUE),
      icd10iMort = sum(icd10i, na.rm = TRUE),
      icd10jMort = sum(icd10j, na.rm = TRUE),
      icd10kMort = sum(icd10k, na.rm = TRUE),
      icd10lMort = sum(icd10l, na.rm = TRUE),
      icd10mMort = sum(icd10m, na.rm = TRUE),
      icd10nMort = sum(icd10n, na.rm = TRUE),
      icd10oMort = sum(icd10o, na.rm = TRUE),
      icd10pMort = sum(icd10p, na.rm = TRUE),
      icd10qMort = sum(icd10q, na.rm = TRUE),
      icd10rMort = sum(icd10r, na.rm = TRUE),
      orMort = sum(other_resp, na.rm = TRUE),
      ovMort = sum(other_viral, na.rm = TRUE),
      rMort = sum(regular, na.rm = TRUE),
      otherDiseasesMort = sum(other_diseases, na.rm = TRUE),
    )  %>%
    as.data.frame()
  
  #Adding the Totalpopulation to our dataframe
  #Check and add tPop if it doesn't exist already
  if(!("tPop" %in% colnames(df))){
    df$tPop = NA
  }
  
  #Make sure the data is structured exactly right
  df = df %>%
    left_join(pop_data, by = c("agegr", "year", "eduRank", "sex", "origin")) %>%
    mutate(tPop = coalesce(tpop_2, tPop),
           tPop = tPop *1000)
  
  
  #Those can be deleted afterwards, but let's see how it goes
  print("Total of edu 9 in this year")
  print(sum(df$tMort[df$eduRank == 9]))
  print("Total mortality in this year")
  print(sum(df$tMort))
  
  #Adjust unknown education data (eduRank == 9) distributively to the other data
  #Subtract all different mortality causes from the dataframe, Mort$ let it be the end of the variable
  mort_names = grep("Mort$", names(df), value = TRUE)
  #Because our dataframe has 400 inputs by construction
  for(i in 0:79){
    for(mortx in mort_names){
      tot = sum(df[[mortx]][(5*i + 1):(5*i + 4)])
      if (is.na(tot) || tot == 0) next 
      #Calculating the share of unknown education rank that should be added to a specific rank
      df[[mortx]][5*i + 1]  = df[[mortx]][5*i + 1] + (df[[mortx]][5*i + 1]/tot)*df[[mortx]][5*i + 5]
      df[[mortx]][5*i + 2]  = df[[mortx]][5*i + 2] + (df[[mortx]][5*i + 2]/tot)*df[[mortx]][5*i + 5]
      df[[mortx]][5*i + 3]  = df[[mortx]][5*i + 3] + (df[[mortx]][5*i + 3]/tot)*df[[mortx]][5*i + 5]
      df[[mortx]][5*i + 4]  = df[[mortx]][5*i + 4] + (df[[mortx]][5*i + 4]/tot)*df[[mortx]][5*i + 5]
    }
    totpop = sum(df$tPop[(5*i + 1):(5*i + 4)])
    if(is.na(totpop) || totpop == 0) next
    df$tPop[5*i + 1]  = df$tPop[5*i + 1] + (df$tPop[5*i + 1]/totpop)*df$tPop[5*i + 5]
    df$tPop[5*i + 2]  = df$tPop[5*i + 2] + (df$tPop[5*i + 2]/totpop)*df$tPop[5*i + 5]
    df$tPop[5*i + 3]  = df$tPop[5*i + 3] + (df$tPop[5*i + 3]/totpop)*df$tPop[5*i + 5]
    df$tPop[5*i + 4]  = df$tPop[5*i + 4] + (df$tPop[5*i + 4]/totpop)*df$tPop[5*i + 5]
  }

  
  #subtracting unknown edurank, as it is already taken care of
  df = df[df$eduRank != 9,]
  
  #remove tpop_2
  df = df %>% select(-tpop_2)
  
  #rename columns to make convertible to full data frame
  colnames(df)[colnames(df) == "eduRank"] = "edclass"
  colnames(df)[colnames(df) == "agegr"] = "age"
  colnames(df)[colnames(df) == "origin"] = "race"
  return(df)
}

calc = function(df, year, t_pop, full){
  if(year <= 1998){
    x = struct9298(df, year)
  } else if(year <= 2002){
    x = struct9902(df,year)
  } else {
    x = struct0321(df,year)
  }
  y = aggYear(x, t_pop)
  
  rownames(y) = NULL
  if (year <= 2015) {
    compareYear(y, year)
    }
  #Use the obtained data to adjust the full dataset, one step closer to getting all needed info
  full = adjFull(y, full, year)
  return(list(full = full, y = y))
}

#By construction, the dataframe from Novosad and our NCHS aggregation are build the same, we compare the outcomes in both frames
compareYear = function(df, y){
  yf = full_data %>% filter(year == y)
  #Compare total mortality
    print("Agg. NCHS underestimates tMort:")
    print(sum(df$tMort < yf$tmort_t))
    print("Agg. NCHS estimates tMort correctly:")
    print(sum(df$tMort == yf$tmort_t))
  #Compare total deaths of despair
    print("Agg. NCHS underestimates despairMort:")
    print(sum(df$despMort < yf$tmort_d))
    print("Agg. NCHS estimates despairMort correctly:")
    print(sum(df$despMort == yf$tmort_d))
  #Compute the MSE
    print("MSE: total mort")
    print(mean(((df$tMort - yf$tmort_t)/yf$tmort_t * 100) [yf$tmort_t != 0]))
    print("MSE: despair mort")
    print(mean(((df$despMort - yf$tmort_d)/yf$tmort_d*100)[yf$tmort_d != 0]))
}

#This function adjusts the full_data dataframe and it calculates the 'new' mortality rates
adjFull = function(df, full, y){
  #convert the data from the separate files to the 'main' file
  full = full %>%
    left_join(df, by = c("age", "race", "sex", "edclass", "year")) %>%
    mutate(
      tpop = coalesce(tPop, tpop),
      tmort_t = coalesce(tMort, tmort_t),
      tmort_h = coalesce(heartMort, tmort_h),
      tmort_c = coalesce(cancerMort, tmort_c),
      tmort_d = coalesce(despMort, tmort_d),
      tmort_a = coalesce(accidentsMort, tmort_a),
      tmort_cd = coalesce(cerebMort, tmort_cd),
      tmort_resp = coalesce(clrdMort, tmort_resp),
      tmort_icd10a = coalesce(icd10aMort, tmort_icd10a),
      tmort_icd10b = coalesce(icd10bMort, tmort_icd10b),
      tmort_icd10d = coalesce(icd10dMort, tmort_icd10d),
      tmort_icd10e = coalesce(icd10eMort, tmort_icd10e),
      tmort_icd10f = coalesce(icd10fMort, tmort_icd10f),
      tmort_icd10g = coalesce(icd10gMort, tmort_icd10g),
      tmort_icd10h = coalesce(icd10hMort, tmort_icd10h),
      tmort_icd10i = coalesce(icd10iMort, tmort_icd10i),
      tmort_icd10j = coalesce(icd10jMort, tmort_icd10j),
      tmort_icd10k = coalesce(icd10kMort, tmort_icd10k),
      tmort_icd10l = coalesce(icd10lMort, tmort_icd10l),
      tmort_icd10m = coalesce(icd10mMort, tmort_icd10m),
      tmort_icd10n = coalesce(icd10nMort, tmort_icd10n),
      tmort_icd10o = coalesce(icd10oMort, tmort_icd10o),
      tmort_icd10p = coalesce(icd10pMort, tmort_icd10p),
      tmort_icd10q = coalesce(icd10qMort, tmort_icd10q),
      tmort_icd10r = coalesce(icd10rMort, tmort_icd10r),
      tmort_or = coalesce(orMort, tmort_or),
      tmort_icd10ab = coalesce(ovMort, tmort_icd10ab),
      tmort_r = coalesce(rMort, tmort_r),
      tmort_v = coalesce(vMort, tmort_v)
    )
  #calculate (where possible) the mortality rates in the main file
  full = full %>%
    mutate(
      mortrate_t = ifelse(tpop == 0 | is.na(tpop), 0, tmort_t / tpop * 100000),
      mortrate_h = ifelse(tpop == 0 | is.na(tpop), 0, tmort_h / tpop * 100000),
      mortrate_c = ifelse(tpop == 0 | is.na(tpop), 0, tmort_c / tpop * 100000),
      mortrate_d = ifelse(tpop == 0 | is.na(tpop), 0, tmort_d / tpop * 100000),
      mortrate_a = ifelse(tpop == 0 | is.na(tpop), 0, tmort_a / tpop * 100000),
      mortrate_cd = ifelse(tpop == 0 | is.na(tpop), 0, tmort_cd / tpop * 100000),
      mortrate_resp = ifelse(tpop == 0 | is.na(tpop), 0, tmort_resp / tpop * 100000),
      mortrate_icd10a = ifelse(tpop == 0 | is.na(tpop), 0, tmort_icd10a / tpop * 100000),
      mortrate_icd10b = ifelse(tpop == 0 | is.na(tpop), 0, tmort_icd10b / tpop * 100000),
      mortrate_icd10d = ifelse(tpop == 0 | is.na(tpop), 0, tmort_icd10d / tpop * 100000),
      mortrate_icd10e = ifelse(tpop == 0 | is.na(tpop), 0, tmort_icd10e / tpop * 100000),
      mortrate_icd10f = ifelse(tpop == 0 | is.na(tpop), 0, tmort_icd10f / tpop * 100000),
      mortrate_icd10g = ifelse(tpop == 0 | is.na(tpop), 0, tmort_icd10g / tpop * 100000),
      mortrate_icd10h = ifelse(tpop == 0 | is.na(tpop), 0, tmort_icd10h / tpop * 100000),
      mortrate_icd10i = ifelse(tpop == 0 | is.na(tpop), 0, tmort_icd10i / tpop * 100000),
      mortrate_icd10j = ifelse(tpop == 0 | is.na(tpop), 0, tmort_icd10j / tpop * 100000),
      mortrate_icd10k = ifelse(tpop == 0 | is.na(tpop), 0, tmort_icd10k / tpop * 100000),
      mortrate_icd10l = ifelse(tpop == 0 | is.na(tpop), 0, tmort_icd10l / tpop * 100000),
      mortrate_icd10m = ifelse(tpop == 0 | is.na(tpop), 0, tmort_icd10m / tpop * 100000),
      mortrate_icd10n = ifelse(tpop == 0 | is.na(tpop), 0, tmort_icd10n / tpop * 100000),
      mortrate_icd10o = ifelse(tpop == 0 | is.na(tpop), 0, tmort_icd10o / tpop * 100000),
      mortrate_icd10p = ifelse(tpop == 0 | is.na(tpop), 0, tmort_icd10p / tpop * 100000),
      mortrate_icd10q = ifelse(tpop == 0 | is.na(tpop), 0, tmort_icd10q / tpop * 100000),
      mortrate_icd10r = ifelse(tpop == 0 | is.na(tpop), 0, tmort_icd10r / tpop * 100000),
      mortrate_or = ifelse(tpop == 0 | is.na(tpop), 0, tmort_or / tpop * 100000),
      mortrate_icd10ab = ifelse(tpop == 0 | is.na(tpop), 0, tmort_icd10ab / tpop * 100000),
      mortrate_r = ifelse(tpop == 0 | is.na(tpop), 0, tmort_r / tpop * 100000),
      mortrate_v = ifelse(tpop == 0 | is.na(tpop), 0, tmort_c / tpop * 100000)
    ) 
  #subtract the added columns, which are not needed anymore
  full = full %>%
    select( -c("tMort","poisMort", "liverMort","suicideMort","despMort","vMort","cancerMort","lungCancerMort", "heartMort", "accidentsMort", "clrdMort", "cerebMort", "otherDiseasesMort", "tPop",
               "icd10aMort","icd10bMort","icd10dMort","icd10eMort","icd10fMort","icd10gMort","icd10hMort","icd10iMort","icd10jMort","icd10kMort","icd10lMort","icd10mMort","icd10nMort",
               "icd10oMort","icd10pMort","icd10qMort","icd10rMort", "ovMort", "orMort", "rMort"))
  return(full)
}


#Create a function that calculates all education ranks  !!! MOOI TAAKJE VOOR MORGEN !!!
educationRank = function(full){
  #Prevent doublenaming columns
  if(!any(colnames(full) == "tpop_ed_sex")){
    #Constructing the (cumulative) education ranks restricted on age and sex
    #Calculating the total population independent of race
    x = full %>% group_by(age, year, edclass, sex) %>% summarise(tpop_ed_sex = sum(tpop, na.rm = TRUE))
    full = full %>% left_join(x, by = c("age", "year", "edclass", "sex"))
    
    #Calculating the total population independent of race and education
    x = full %>% group_by(age, year, sex) %>% summarise(tpop_sex = sum(tpop, na.rm = TRUE))
    full = full %>% left_join(x, by = c("age", "year", "sex"))
    
    #Calculating the total population independent of race and sex
    x = full %>% group_by(age, year, edclass) %>% summarise(tpop_ed_all = sum(tpop, na.rm = TRUE))
    full = full %>% left_join(x, by = c("age", "year", "edclass"))
  
    #Calculating the total population independent of race, sex and education
    x = full %>% group_by(age, year) %>% summarise(tpop_all = sum(tpop, na.rm = TRUE))
    full = full %>% left_join(x, by = c("age", "year"))
  
    #Calculating the total population independent of nothing
    x = full %>% group_by(age, year, edclass, sex, race) %>% summarise(tpop_ed_race_sex = sum(tpop, na.rm = TRUE))
    full = full %>% left_join(x, by = c("age", "year", "edclass", "sex", "race"))
  
    #Calculating the total population independent of education
    x = full %>% group_by(age, year, sex, race) %>% summarise(tpop_race_sex = sum(tpop, na.rm = TRUE))
    full = full %>% left_join(x, by = c("age", "year", "sex", "race"))
  }
  #print(names(full))
  
  #Constructing the actual (cumulative) education ranks restricted on sex
  if(!any(colnames(original_full_data) == "ed_rank_sex")){
    full = full %>%
      mutate(
        ed_rank_sex = NA,
        cum_ed_rank_sex = NA
      )
  }
  #create the ranks
  full = full %>%
    mutate(
      cum_ed_rank_sex = ifelse(edclass == 1 & !is.na(tpop_sex) & tpop_sex != 0, (tpop_ed_sex/tpop_sex), cum_ed_rank_sex),
      ed_rank_sex = ifelse(edclass == 1, cum_ed_rank_sex / 2, ed_rank_sex)
    ) %>%
    mutate(
      cum_ed_rank_sex = ifelse(edclass > 1 & !is.na(tpop_sex) & tpop_sex != 0, (tpop_ed_sex/tpop_sex) + lag(cum_ed_rank_sex), cum_ed_rank_sex),
      ed_rank_sex = ifelse(edclass > 1 & !is.na(tpop_sex) & tpop_sex != 0, (tpop_ed_sex/tpop_sex)/2 + lag(cum_ed_rank_sex), ed_rank_sex)
      ) %>%
    mutate(
      cum_ed_rank_sex = ifelse(edclass > 1 & !is.na(tpop_sex) & tpop_sex != 0, (tpop_ed_sex/tpop_sex) + lag(cum_ed_rank_sex), cum_ed_rank_sex),
      ed_rank_sex = ifelse(edclass > 1 & !is.na(tpop_sex) & tpop_sex != 0, (tpop_ed_sex/tpop_sex)/2 + lag(cum_ed_rank_sex), ed_rank_sex)
    ) %>%
    mutate(
      cum_ed_rank_sex = ifelse(edclass > 1 & !is.na(tpop_sex) & tpop_sex != 0, (tpop_ed_sex/tpop_sex) + lag(cum_ed_rank_sex), cum_ed_rank_sex),
      ed_rank_sex = ifelse(edclass > 1 & !is.na(tpop_sex) & tpop_sex != 0, (tpop_ed_sex/tpop_sex)/2 + lag(cum_ed_rank_sex), ed_rank_sex)
    ) %>%
    mutate(
      ed_rank_sex = ed_rank_sex * 100
    )
  
  
  #Constructing the annual (cumulative) education ranks on sex and race
  if(!any(colnames(full) == "ed_rank_race_sex")){
    full = full %>%
      mutate(
        ed_rank_race_sex = NA,
        cum_ed_rank_race_sex = NA
      )
  }
  #create the ranks
  full = full %>%
    mutate(
      cum_ed_rank_race_sex = ifelse(edclass == 1 & !is.na(tpop_race_sex) & tpop_race_sex != 0, (tpop_ed_race_sex/tpop_race_sex), cum_ed_rank_race_sex),
      ed_rank_race_sex = ifelse(edclass == 1, cum_ed_rank_race_sex / 2, ed_rank_race_sex)
    ) %>%
    mutate(
      cum_ed_rank_race_sex = ifelse(edclass > 1 & !is.na(tpop_race_sex) & tpop_race_sex != 0, (tpop_ed_race_sex/tpop_race_sex) + lag(cum_ed_rank_race_sex), cum_ed_rank_race_sex),
      ed_rank_race_sex = ifelse(edclass > 1 & !is.na(tpop_race_sex) & tpop_race_sex != 0, (tpop_ed_race_sex/tpop_race_sex)/2 + lag(cum_ed_rank_race_sex), ed_rank_race_sex)
    ) %>%
    mutate(
      cum_ed_rank_race_sex = ifelse(edclass > 1 & !is.na(tpop_race_sex) & tpop_race_sex != 0, (tpop_ed_race_sex/tpop_race_sex) + lag(cum_ed_rank_race_sex), cum_ed_rank_race_sex),
      ed_rank_race_sex = ifelse(edclass > 1 & !is.na(tpop_race_sex) & tpop_race_sex != 0, (tpop_ed_race_sex/tpop_race_sex)/2 + lag(cum_ed_rank_race_sex), ed_rank_race_sex)
    ) %>%
    mutate(
      cum_ed_rank_race_sex = ifelse(edclass > 1 & !is.na(tpop_race_sex) & tpop_race_sex != 0, (tpop_ed_race_sex/tpop_race_sex) + lag(cum_ed_rank_race_sex), cum_ed_rank_race_sex),
      ed_rank_race_sex = ifelse(edclass > 1 & !is.na(tpop_race_sex) & tpop_race_sex != 0, (tpop_ed_race_sex/tpop_race_sex)/2 + lag(cum_ed_rank_race_sex), ed_rank_race_sex)
    ) %>%
    mutate(
      ed_rank_race_sex = ed_rank_race_sex * 100
    )
  

  #Constructing the annual (cumulative) education ranks
  if(!any(colnames(full) == "ed_rank_all")){
    full = full %>%
      mutate(
        ed_rank_all = NA,
        cum_ed_rank_all = NA
      )
  }
  #create the ranks
  full = full %>%
    mutate(
      cum_ed_rank_all = ifelse(edclass == 1 & !is.na(tpop_all) & tpop_all != 0, (tpop_ed_all/tpop_all), cum_ed_rank_all),
      ed_rank_all = ifelse(edclass == 1, cum_ed_rank_all / 2, ed_rank_all)
    ) %>%
    mutate(
      cum_ed_rank_all = ifelse(edclass > 1 & !is.na(tpop_all) & tpop_all != 0, (tpop_ed_all/tpop_all) + lag(cum_ed_rank_all), cum_ed_rank_all),
      ed_rank_all = ifelse(edclass > 1 & !is.na(tpop_all) & tpop_all != 0, (tpop_ed_all/tpop_all)/2 + lag(cum_ed_rank_all), ed_rank_all),
    ) %>%
    mutate(
      cum_ed_rank_all = ifelse(edclass > 1 & !is.na(tpop_all) & tpop_all != 0, (tpop_ed_all/tpop_all) + lag(cum_ed_rank_all), cum_ed_rank_all),
      ed_rank_all = ifelse(edclass > 1 & !is.na(tpop_all) & tpop_all != 0, (tpop_ed_all/tpop_all)/2 + lag(cum_ed_rank_all), ed_rank_all),
    ) %>%
    mutate(
      cum_ed_rank_all = ifelse(edclass > 1 & !is.na(tpop_all) & tpop_all != 0, (tpop_ed_all/tpop_all) + lag(cum_ed_rank_all), cum_ed_rank_all),
      ed_rank_all = ifelse(edclass > 1 & !is.na(tpop_all) & tpop_all != 0, (tpop_ed_all/tpop_all)/2 + lag(cum_ed_rank_all), ed_rank_all),
    ) %>%
    mutate(
      ed_rank_all = ed_rank_all * 100
    )
  
  #In this function we create lagged and lead total population data at the same time
  full = full %>%
    group_by(race, edclass, age, sex) %>%
    mutate(lag1_tpop = lag(tpop, n = 1), 
           lag2_tpop = lag(tpop, n = 2),
           lead1_tpop = lead(tpop, n = 1), 
           lead2_tpop = lead(tpop, n = 2)
           ) %>%
    ungroup() %>%
    mutate(lag1_tpop = ifelse(is.na(lag1_tpop), 0, lag1_tpop), 
           lag2_tpop = ifelse(is.na(lag2_tpop), 0, lag2_tpop),
           lead1_tpop = ifelse(is.na(lead1_tpop), 0, lead1_tpop), 
           lead2_tpop = ifelse(is.na(lead2_tpop), 0, lead2_tpop)
           )
  
  #Creation of rolling5 as well
  firstYear = min(full$year, na.rm = TRUE)
  lastYear = max(full$year, na.rm = TRUE)
  
  full = full %>%
    mutate(
      tpop_rolling5 = NA
    ) %>%
    mutate(
      #Use 5 year bins when all lags are known, i.e. non-NA
      tpop_rolling5 = ifelse(year > (firstYear + 1) & year < (lastYear - 1), (lag2_tpop + lag1_tpop + tpop + lead1_tpop + lead2_tpop)/5, tpop_rolling5),
      #Mutate to 3 year bins as we don't have five data points
      tpop_rolling5 = ifelse(year == (firstYear + 1) | year == (lastYear - 1), (lag1_tpop + tpop + lead1_tpop)/3 , tpop_rolling5)
    )
  #Add an extra variable for each group wrt age, sex, race and edclass
  full = full %>%
    mutate(
      demo_group = as.integer(interaction(age, race, sex, edclass))
    )
  #Use a regression for the first and last years on each specific group
  for(demo in unique(full$demo_group)){
    demo_data = full %>% filter(demo_group == demo)
    #Regress on the last three years to obtain rolling_Tpop for the last year
    if(any(demo_data$year == lastYear)){
      reg_last = lm(tpop ~ year, data = filter(demo_data, year %in% (lastYear - 2):lastYear))
      pred_last = predict(reg_last, newdata = data.frame(year = lastYear))
      full = full %>%
        mutate(
          tpop_rolling5 = ifelse(year == lastYear & demo_group == demo & !is.na(pred_last) & pred_last > 0, pred_last, tpop_rolling5)
        )
    }
    #Do the same thing for the first year
    if(any(demo_data$year == firstYear)){
      reg_first = lm(tpop ~ year, data = filter(demo_data, year %in% firstYear:(firstYear + 2)))
      pred_first = predict(reg_first, newdata = data.frame(year = firstYear))
      full = full %>%
        mutate(
          tpop_rolling5 = ifelse(year == firstYear & demo_group == demo & !is.na(pred_first) & pred_first > 0, pred_first, tpop_rolling5)
        )
    }
  }
  #Now we have obtained a five year rolling window (with some adjustments) for our dataset
  return(full)
}

########################################

makeAppended = function(full){
  both_sex = full %>%
    group_by(year, age, race, edclass) %>%
    summarize(across(c(...), sum), .groups = "drop")
  
  #Survival Rates
}

makeMortMeans = function(full){
  
}

#US_2006 = read.csv("C:/Users/michi/OneDrive/Documenten/Eur Jaar 3/Blok 4/Major/Data US/mort2006.csv")
#updList = calc(US_2006, 2006, tpop, original_full_data)
#nice_2005 = updList$y
#original_full_data = updList$full

appended_rank_mort = read.csv("C:/Users/michi/OneDrive/Documenten/Eur Jaar 3/Blok 4/Major/Data US/appended_rank_mort.csv")
nov_app_rank_mort = appended_rank_mort
col_to_na = setdiff(names(appended_rank_mort), c("year", "age_gp", "race", "edclass","sex"))
for(col in col_to_na){
  appended_rank_mort[[col]] = NA
    } 

mort_means = read.csv("C:/Users/michi/OneDrive/Documenten/Eur Jaar 3/Blok 4/Major/Data US/mort_means.csv")

####################################################

#Automatic file reader
#2021 is not a .csv file so we calculate it 'separately'
path_map = "C:/Users/michi/OneDrive/Documenten/Eur Jaar 3/Blok 4/Major/Data US/"
path_spec = "mort"
path_end = ".csv"

#Read the total population file
tpop = read.csv("C:/Users/michi/OneDrive/Documenten/Eur Jaar 3/Blok 4/Major/Data US/processed_all_races.csv")
tpop = tpop[tpop$year != 1901,]
colnames(tpop)[colnames(tpop) == "educ"] = "eduRank"
colnames(tpop)[colnames(tpop) == "age"] = "agegr"
colnames(tpop)[colnames(tpop) == "race"] = "origin"
tpop = tpop %>% mutate(eduRank = ifelse(eduRank == 0, 9, eduRank))

#Loop over the years 2000-2020 to obtain a full data set
for(i in 2000:2021){
  #Construct a path and aggregate data for the years until 2020
  if(i < 2021){
  path = paste0(path_map, path_spec, i, path_end)
  df_actual = read.csv(path)
  }
  
  
  #For 2021, we have to read a .dta file (.csv was not avaiable)
  if(i == 2021){
    df_actual = read_dta("C:/Users/michi/OneDrive/Documenten/Eur Jaar 3/Blok 4/Major/Data US/mort2021us.dta")
  }
  
  #Adjust the constructed full_data file
  updateList = calc(df_actual, i, tpop, original_full_data)
  name2 = paste0("nice_", i)
  assign(name2, updateList$y)
  original_full_data = updateList$full
  
  #Remove the death records file and clean the working field to enable program to keep running
  rm(df_actual)
  gc()
}

write.csv(original_full_data, "C:/Users/michi/OneDrive/Documenten/Eur Jaar 3/Blok 4/Major/Data US/aggrData.csv")
use_full_data = original_full_data %>% filter(year %in% 2000:2021)
#Now add the education ranks to the full data file
use_full_data = educationRank(use_full_data)





#This code is to compare the generated data with the 'actual' data separately
x = appended_rank_mort[appended_rank_mort$sex != 0,]
x = x[x$race != 0,]
x = x %>% arrange(year, race, age_gp, sex, edclass)

extraCompareYear = function(df, y){
  yf = x %>% filter(year == y)
  #Compare total mortality
  print("Agg. NCHS underestimates tMort:")
  print(sum(df$tMort < yf$tmort))
  print("Agg. NCHS estimates tMort correctly:")
  print(sum(df$tMort == yf$tmort))
  #Compare total deaths of despair
  print("Agg. NCHS underestimates despairMort:")
  print(sum(df$despMort < yf$dmort))
  print("Agg. NCHS estimates despairMort correctly:")
  print(sum(df$despMort == yf$dmort))
  #Compute the MSE
  print("MSE: total mort")
  print(mean(((df$tMort - yf$tmort)/yf$tmort * 100)[yf$tmort != 0]))
  print("MSE: despair mort")
  print(mean(((df$despMort - yf$dmort)/yf$dmort*100)[yf$dmort != 0]))
}
