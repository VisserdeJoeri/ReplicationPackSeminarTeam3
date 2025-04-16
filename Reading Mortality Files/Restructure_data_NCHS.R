#Access all necessary libraries 
library(haven)
library(readxl)
library(dplyr)
library(tidyr)
library(writexl)

#First we create some functions that will convert the data
#these will be used at the end of the code, starting from line 1642

################################################################################
#Adjusts the Dataset provided by Novosad sucht that we can fill it with our own data
adjRaw = function(full){
  #Here we change the US_raw_data such that we can add our 'new' results
  #Identify columns to keep unchanged
  added_columns = c("year", "edclass", "age", "race", "sex")
  #Identify other columns to set to NA
  other_columns = setdiff(names(US_raw_data), added_columns)

  #Create a data frame for new years (2016-2021)
  create_years = expand.grid(
    year = 2016:2021,
    edclass = unique(US_raw_data$edclass),
    age = unique(US_raw_data$age),
    race = unique(US_raw_data$race),
    sex = unique(US_raw_data$sex)
  )
  
  #Add NA columns for all other variables
  for (col in other_columns) {
    create_years[[col]] = NA
  }

  #Combine with the original dataset
  full = bind_rows(US_raw_data, create_years) %>%
    #Add a column for corona mortalities
    mutate(mortrate_v = NA, 
          tmort_v = NA,
          tmort_p = NA,
          tmort_s = NA,
          tmort_l = NA,
          tmort_lungc = NA,
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
          tmort_o = NA,
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
          mortrate_s = NA,
          mortrate_l = NA,
          mortrate_p = NA,
          mortrate_lungc = NA,
          mortrate_r = NA,
          mortrate_o = NA
          ) %>%  #set  to zero: Years until 2019 will never have corona mortality
    arrange(year)  #Ensure data is ordered by year
  
  
  #Extended the dataframe by total mortality
  mortrate_cols = c(grep("mortrate_", names(full), value = TRUE))
  
  for (col in mortrate_cols) {
    new_col = sub("mortrate_", "tmort_", col)
    full[[new_col]] = ifelse(is.na(full$tpop), NA, 
                                      full[[col]] * full$tpop / 100000)
  }
  
  full = as.data.frame(full)
  return(full)
}

################################################################################

#Here we start the functions that read and convert the yearly death certificate data from NCHS
#The years 1992 until 1999 is still included because Novosad analyses these years and we planned on doing it as well
#However, we did not have access to suitable population data for these years
#The years are seperate in this way because of the change in ICD9 to ICD10 notation in 1999
#And because of the change in education notation in 2003
struct9298 = function(df, year){
  #origin
  #remove all the foreigners from the data
  
  df = df[df$restatus <= 3,]
  
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
        suicide = case_when(
          ucod >= 9500 & ucod <= 9599 ~ 1,
          TRUE ~ 0
          ),
        #Liver 
        liver = case_when(
          ucod >= 5710 & ucod<= 5719 ~ 1,
          TRUE ~ 0
          ),
        #Heart
        heart = case_when(
          ucod >= 3900 & ucod <= 4299 ~ 1,
          TRUE ~ 0
          ),
        #(Lung)Cancer
        cancer = case_when(
          ucod >= 1400 & ucod <= 2089 ~ 1,
          TRUE ~ 0
        ),
        lungCancer = case_when(
          ucod >= 1622 & ucod <= 1629 ~ 1,
          TRUE ~ 0
        ),
        #cerebrovascular
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
  
  
  df = df %>% select(year, eduRank, ager52, agegr, origin, sex, poison, liver, despair, v, suicide, heart, accidents, clrd, cereb, cancer, lungCancer, other_diseases, one)
  
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
  #Create binary variables for the main cause of death -> using the same numbers as Novosad et al. used with ICD-10 data
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
      #Liver
      liver = case_when(
        substr(ucod,1,3) %in% paste0("K70") ~ 1,
        substr(ucod,1,4) %in% paste0("K70", 0:9) ~ 1,
        substr(ucod,1,4) %in% paste0("K7", 30:49) ~ 1,
        TRUE ~ 0
      ),
      #Heart
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
      #(Lung)Cancer
      cancer = case_when(
        substr(ucod,1,1) %in% paste0("C") ~ 1,
        TRUE ~ 0
      ),
      lungCancer = case_when(
        substr(ucod,1,3) %in% paste0("C34") ~ 1,
        TRUE ~ 0
      ),
      #cerebrovascular
      cereb = case_when(
        substr(ucod,1,3) %in% paste0("I", 60:69) ~ 1,
        TRUE ~ 0
      ),
      #CLRD
      clrd = case_when(
        substr(ucod,1,3) %in% paste0("J", 40:47) ~ 1,
        TRUE ~ 0
      ),
      #Covid, although it will be zero for this function
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
  #Also the Causes per letter is added for the optimal overview
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
  
  
  df = df %>% select(year, eduRank, ager52, agegr, origin, sex, poison, liver, suicide, despair, v,  heart, accidents, clrd, cereb, cancer, lungCancer, other_diseases, one,
                     icd10a, icd10b, icd10d, icd10e, icd10f, icd10g, icd10h, icd10i, icd10j, icd10k, icd10l, icd10m, icd10n, icd10o, icd10p, icd10q, icd10r, other_viral, other_resp, regular)
  return(df)
}

struct0321 = function(df, year){
  #origin
  #remove all the foreigners from the data
  
  df = df[df$restatus <= 3, ]
  
  #age
  #keep the people from correct age group(s) in the data (age 25-29 until 70-74) We will remove the last group later
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
    #'easier' implementable origin (race) ranks as well
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
  #Create binary variables for the main cause of death -> using the same numbers as Novosad et al. used with ICD-10 data
  df = df %>% 
    mutate(
      #Create a variable for all the deceased
      one = 1,
      #Poisoning
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
      #Liver
      liver = case_when(
        substr(ucod,1,3) %in% paste0("K70") ~ 1,
        substr(ucod,1,4) %in% paste0("K70", 0:9) ~ 1,
        substr(ucod,1,4) %in% paste0("K7", 30:49) ~ 1,
        TRUE ~ 0
      ),
      #Heart
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
      #(Lung)Cancer
      cancer = case_when(
        substr(ucod,1,1) %in% paste0("C") ~ 1,
        TRUE ~ 0
      ),
      lungCancer = case_when(
        substr(ucod,1,3) %in% paste0("C34") ~ 1,
        TRUE ~ 0
      ),
      #cerebrovascular
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
  #All deceases per letter code are added as well
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
      
  df = df %>% select(year, eduRank, ager52, agegr, origin, sex, poison, liver, suicide, despair, v, heart, accidents, clrd, cereb, cancer, lungCancer, other_diseases, one,
                     icd10a, icd10b, icd10d, icd10e, icd10f, icd10g, icd10h, icd10i, icd10j, icd10k, icd10l, icd10m, icd10n, icd10o, icd10p, icd10q, icd10r, other_viral, other_resp, regular)
  return(df)
}

#This function aggregates the adjusted death certificates for each year, creating the mortality numbers
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
           #tPop = tPop *1000
           )
  
  
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
    #These lines of code are not necessary if the tpop data for unknown ditribution rank is already distributed, i.e. no unknown eduRank left
    #totpop = sum(df$tPop[(5*i + 1):(5*i + 4)])
    #if(is.na(totpop) || totpop == 0) next
    #df$tPop[5*i + 1]  = df$tPop[5*i + 1] + (df$tPop[5*i + 1]/totpop)*df$tPop[5*i + 5]
    #df$tPop[5*i + 2]  = df$tPop[5*i + 2] + (df$tPop[5*i + 2]/totpop)*df$tPop[5*i + 5]
    #df$tPop[5*i + 3]  = df$tPop[5*i + 3] + (df$tPop[5*i + 3]/totpop)*df$tPop[5*i + 5]
    #df$tPop[5*i + 4]  = df$tPop[5*i + 4] + (df$tPop[5*i + 4]/totpop)*df$tPop[5*i + 5]
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

#This function compares our generated data with the data from Novosad 
#By construction, the dataframe from Novosad and our NCHS aggregation are build the same, we compare the outcomes in both frames
#Note that y = year needs to be input
compareYear = function(df, y){
  if(y < 2016){ 
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
  } else {
    #This code is to compare the generated data with the 'actual' data separately
    x = nov_app_rank_mort[nov_app_rank_mort$sex != 0,]
    x = x[x$race != 0,]
    x = x %>% arrange(year, race, age_gp, sex, edclass)
    
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
}

#This function adjusts the full_data dataframe and it calculates mortality rates
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
      tmort_p = coalesce(poisMort, tmort_p),
      tmort_l = coalesce(liverMort, tmort_l),
      tmort_s = coalesce(suicideMort, tmort_s),
      tmort_lungc = coalesce(lungCancerMort, tmort_lungc),
      tmort_v = coalesce(vMort, tmort_v),
      tmort_o = coalesce(otherDiseasesMort, tmort_o)
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
      mortrate_l = ifelse(tpop == 0 | is.na(tpop), 0, tmort_l / tpop * 100000),
      mortrate_s = ifelse(tpop == 0 | is.na(tpop), 0, tmort_s / tpop * 100000),
      mortrate_p = ifelse(tpop == 0 | is.na(tpop), 0, tmort_p / tpop * 100000),
      mortrate_lungc = ifelse(tpop == 0 | is.na(tpop), 0, tmort_lungc / tpop * 100000),
      mortrate_v = ifelse(tpop == 0 | is.na(tpop), 0, tmort_v / tpop * 100000),
      mortrate_o = ifelse(tpop == 0 | is.na(tpop), 0, tmort_o / tpop * 100000)
    ) 
  #subtract the added columns, which are not needed anymore
  full = full %>%
    select( -c("tMort","poisMort", "liverMort","suicideMort","despMort","vMort","cancerMort","lungCancerMort", "heartMort", "accidentsMort", "clrdMort", "cerebMort", "otherDiseasesMort", "tPop",
               "icd10aMort","icd10bMort","icd10dMort","icd10eMort","icd10fMort","icd10gMort","icd10hMort","icd10iMort","icd10jMort","icd10kMort","icd10lMort","icd10mMort","icd10nMort",
               "icd10oMort","icd10pMort","icd10qMort","icd10rMort", "ovMort", "orMort", "rMort"))
  return(full)
}

#This function calls the correct functions for each year
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

################################################################################
#This function adds the mortality and population values for each group in the (extended) dataframe created by Novosad
#It also calculates the mortality rates and education ranks
makeAppended = function(full, empty_rank){
  #Adding both sexes to obtain their sum
  both_sex = full %>%
    group_by(year, age, race, edclass) %>%
    summarize(across(c(tpop, tmort_v, tmort_icd10a, tmort_icd10b, tmort_icd10d, tmort_icd10e, tmort_icd10f, 
                       tmort_icd10g, tmort_icd10h, tmort_icd10i, tmort_icd10j, tmort_icd10k, tmort_icd10l, tmort_icd10m,
                       tmort_icd10n, tmort_icd10o, tmort_icd10p, tmort_icd10q, tmort_icd10r, tmort_or, tmort_r, tmort_t, 
                       tmort_h, tmort_c, tmort_cd, tmort_a, tmort_d, tmort_resp, tmort_icd10ab, tmort_p, tmort_l, tmort_s,
                       tmort_lungc, tmort_o), sum), .groups = "drop")%>%
    mutate(sex = 0)
  newdf = bind_rows(full, both_sex) %>%
    arrange(year, age, race, sex, edclass)
  
  #Do the same thing for race, but note not to use the edited df, as it will sum sex twice (1 + 2 & 0)
  #Thats why we use full
  all_race = full %>%
    group_by(year, age, sex, edclass) %>%
    summarize(across(c(tpop, tmort_v, tmort_icd10a, tmort_icd10b, tmort_icd10d, tmort_icd10e, tmort_icd10f, 
                       tmort_icd10g, tmort_icd10h, tmort_icd10i, tmort_icd10j, tmort_icd10k, tmort_icd10l, tmort_icd10m,
                       tmort_icd10n, tmort_icd10o, tmort_icd10p, tmort_icd10q, tmort_icd10r, tmort_or, tmort_r, tmort_t, 
                       tmort_h, tmort_c, tmort_cd, tmort_a, tmort_d, tmort_resp, tmort_icd10ab, tmort_p, tmort_l, tmort_s,
                       tmort_lungc, tmort_o), sum), .groups = "drop") %>%
    mutate(race = 0) 
  #Add the calculated columns to the df with sex == 0 included
  supernewdf = bind_rows(newdf, all_race) %>%
    arrange(year, age, race, sex, edclass)
  
  #Constructs values were both reace and sex are combined
  both_sex_race = full %>%
    group_by(year, age, edclass) %>%
    summarize(across(c(tpop, tmort_v, tmort_icd10a, tmort_icd10b, tmort_icd10d, tmort_icd10e, tmort_icd10f, 
                       tmort_icd10g, tmort_icd10h, tmort_icd10i, tmort_icd10j, tmort_icd10k, tmort_icd10l, tmort_icd10m,
                       tmort_icd10n, tmort_icd10o, tmort_icd10p, tmort_icd10q, tmort_icd10r, tmort_or, tmort_r, tmort_t, 
                       tmort_h, tmort_c, tmort_cd, tmort_a, tmort_d, tmort_resp, tmort_icd10ab, tmort_p, tmort_l, tmort_s,
                       tmort_lungc, tmort_o), sum), .groups = "drop")%>%
    mutate(race = 0, sex = 0)
  superdupernewdf = bind_rows(supernewdf, both_sex_race) %>%
    arrange(year, race, age, sex, edclass)
  
  #Make the full dataframe in the same way as the appended rank mort
  full = superdupernewdf
  full = full %>%
    mutate(
      age = case_when(
        age == "25-29" ~ 25,
        age == "30-34" ~ 30,
        age == "35-39" ~ 35,
        age == "40-44" ~ 40,
        age == "45-49" ~ 45,
        age == "50-54" ~ 50,
        age == "55-59" ~ 55,
        age == "60-64" ~ 60,
        age == "65-69" ~ 65,
        age == "70-74" ~ 70,
        TRUE ~ 0
      )
    ) %>%
    rename(age_gp = age)
  
  app_rank = empty_rank %>%
    arrange(year, race, age_gp, sex, edclass)
  
  #Add variables for COVID, non-Despair and non-COVID to the dataframe
  app_rank = app_rank %>%
    mutate(
      vmort = NA,
      vmortrate = NA,
      nmort = NA,
      nmortrate = NA,
      xmort = NA,
      xmortrate = NA
    )
  
  #Add the new columns to app rank mort
  app_rank = app_rank %>%
    left_join(full %>% select(c("year", "race", "age_gp", "sex", "edclass", "tpop", "tmort_v", "tmort_icd10a", "tmort_icd10b", "tmort_icd10d", "tmort_icd10e", "tmort_icd10f",  
                                "tmort_icd10g", "tmort_icd10h", "tmort_icd10i", "tmort_icd10j", "tmort_icd10k", "tmort_icd10l", "tmort_icd10m",  
                                "tmort_icd10n", "tmort_icd10o", "tmort_icd10p", "tmort_icd10q", "tmort_icd10r", "tmort_or", "tmort_o", "tmort_r", "tmort_t",  
                                "tmort_h", "tmort_c", "tmort_cd", "tmort_a", "tmort_d", "tmort_resp", "tmort_icd10ab", "tmort_p", "tmort_l", "tmort_s", "tmort_lungc")), 
              by = c("year", "race", "age_gp", "sex", "edclass")) %>%
    mutate(
      raw_tpop = coalesce(tpop, raw_tpop),
      tmort = coalesce(tmort_t, tmort),
      pmort = coalesce(tmort_p, pmort),
      lmort = coalesce(tmort_l, lmort),
      smort = coalesce(tmort_s, smort),
      hmort = coalesce(tmort_h, hmort),
      cmort = coalesce(tmort_c, cmort),
      lungcmort = coalesce(tmort_lungc, lungcmort),
      cdmort = coalesce(tmort_cd, cdmort),
      clmort = coalesce(tmort_resp, clmort),
      vmort = coalesce(tmort_v, vmort),
      amort = coalesce(tmort_a, amort),
      omort = coalesce(tmort_o, omort),
      ormort = coalesce(tmort_or, ormort),
      ovmort = coalesce(tmort_icd10ab, ovmort),
      icd10amort = coalesce(tmort_icd10a, icd10amort),
      icd10bmort = coalesce(tmort_icd10b, icd10bmort),
      icd10dmort = coalesce(tmort_icd10d, icd10dmort),
      icd10emort = coalesce(tmort_icd10e, icd10emort),
      icd10fmort = coalesce(tmort_icd10f, icd10fmort),
      icd10gmort = coalesce(tmort_icd10g, icd10gmort),
      icd10hmort = coalesce(tmort_icd10h, icd10hmort),
      icd10imort = coalesce(tmort_icd10i, icd10imort),
      icd10jmort = coalesce(tmort_icd10j, icd10jmort),
      icd10kmort = coalesce(tmort_icd10k, icd10kmort),
      icd10lmort = coalesce(tmort_icd10l, icd10lmort),
      icd10mmort = coalesce(tmort_icd10m, icd10mmort),
      icd10nmort = coalesce(tmort_icd10n, icd10nmort),
      icd10omort = coalesce(tmort_icd10o, icd10omort),
      icd10pmort = coalesce(tmort_icd10p, icd10pmort),
      icd10qmort = coalesce(tmort_icd10q, icd10qmort),
      icd10rmort = coalesce(tmort_icd10r, icd10rmort),
      dmort = coalesce(tmort_d, dmort),
      rmort = coalesce(tmort_r, rmort)
    ) %>%
    mutate(
      nmort = tmort - dmort,
      xmort = tmort - vmort
    )
  app_rank = app_rank %>% 
    select(-c("tpop", "tmort_v", "tmort_icd10a", "tmort_icd10b", "tmort_icd10d", "tmort_icd10e", "tmort_icd10f",
              "tmort_icd10g", "tmort_icd10h", "tmort_icd10i", "tmort_icd10j", "tmort_icd10k", "tmort_icd10l", "tmort_icd10m",
              "tmort_icd10n", "tmort_icd10o", "tmort_icd10p", "tmort_icd10q", "tmort_icd10r", "tmort_or", "tmort_o", "tmort_r", "tmort_t",
              "tmort_h", "tmort_c", "tmort_cd", "tmort_a", "tmort_d", "tmort_resp", "tmort_icd10ab", "tmort_p", "tmort_l", "tmort_s", "tmort_lungc"))
  
  #First we will create the rolling_tpop5, starting of with creating lags and leads
  firstYear = min(app_rank$year, na.rm = TRUE)
  lastYear = max(app_rank$year, na.rm = TRUE)
  
  app_rank = app_rank %>%
    group_by(race, edclass, age_gp, sex) %>%
    mutate(lag_1_tpop = lag(raw_tpop, n = 1), 
           lag_2_tpop = lag(raw_tpop, n = 2),
           lead_1_tpop = lead(raw_tpop, n = 1), 
           lead_2_tpop = lead(raw_tpop, n = 2)
    ) %>%
    ungroup() %>%
    mutate(lag_1_tpop = ifelse(is.na(lag_1_tpop), 0, lag_1_tpop), 
           lag_2_tpop = ifelse(is.na(lag_2_tpop), 0, lag_2_tpop),
           lead_1_tpop = ifelse(is.na(lead_1_tpop), 0, lead_1_tpop), 
           lead_2_tpop = ifelse(is.na(lead_2_tpop), 0, lead_2_tpop)
    )
  
  app_rank = app_rank %>%
    mutate(
      #Use 5 year bins when all lags are known, i.e. non-NA
      tpop_rolling_5 = ifelse(year > (firstYear + 1) & year < (lastYear - 1), (lag_2_tpop + lag_1_tpop + raw_tpop + lead_1_tpop + lead_2_tpop)/5, tpop_rolling_5),
      #Mutate to 3 year bins as we don't have five data points
      tpop_rolling_5 = ifelse(year == (firstYear + 1) | year == (lastYear - 1), (lag_1_tpop + raw_tpop + lead_1_tpop)/3 , tpop_rolling_5)
    )
  #Add an extra variable for each group wrt age, sex, race and edclass
  app_rank = app_rank %>%
    mutate(
      demo_group = as.integer(interaction(age_gp, race, sex, edclass))
    )
  #Use a regression for the first and last years on each specific group
  for(demo in unique(app_rank$demo_group)){
    demo_data = app_rank %>% filter(demo_group == demo)
    #Regress on the last three years to obtain rolling_Tpop for the last year
    if(any(demo_data$year == lastYear)){
      reg_last = lm(raw_tpop ~ year, data = filter(demo_data, year %in% (lastYear - 2):lastYear))
      pred_last = predict(reg_last, newdata = data.frame(year = lastYear))
      app_rank = app_rank %>%
        mutate(
          tpop_rolling_5 = ifelse(year == lastYear & demo_group == demo & !is.na(pred_last) & pred_last > 0, pred_last, tpop_rolling_5)
        )
    }
    #Do the same thing for the first year
    if(any(demo_data$year == firstYear)){
      reg_first = lm(raw_tpop ~ year, data = filter(demo_data, year %in% firstYear:(firstYear + 2)))
      pred_first = predict(reg_first, newdata = data.frame(year = firstYear))
      app_rank = app_rank %>%
        mutate(
          tpop_rolling_5 = ifelse(year == firstYear & demo_group == demo & !is.na(pred_first) & pred_first > 0, pred_first, tpop_rolling_5)
        )
    }
  }
  
  #Now its time to determine the mortality and survival rates for each of the variables
  app_rank = app_rank %>%
    mutate(
      tmortrate = ifelse(!is.na(tpop_rolling_5) & (tpop_rolling_5 != 0), (tmort / tpop_rolling_5)*100000, 0),
      tsurvrate = ifelse(!is.na(tpop_rolling_5) & (tpop_rolling_5 != 0), (100000 - tmortrate), 0),
      pmortrate = ifelse(!is.na(tpop_rolling_5) & (tpop_rolling_5 != 0), (pmort / tpop_rolling_5)*100000, 0),
      psurvrate = ifelse(!is.na(tpop_rolling_5) & (tpop_rolling_5 != 0), (100000 - pmortrate), 0),
      smortrate = ifelse(!is.na(tpop_rolling_5) & (tpop_rolling_5 != 0), (smort / tpop_rolling_5)*100000, 0),
      ssurvrate = ifelse(!is.na(tpop_rolling_5) & (tpop_rolling_5 != 0), (100000 - smortrate), 0),
      lmortrate = ifelse(!is.na(tpop_rolling_5) & (tpop_rolling_5 != 0), (lmort / tpop_rolling_5)*100000, 0),
      lsurvrate = ifelse(!is.na(tpop_rolling_5) & (tpop_rolling_5 != 0), (100000 - lmortrate), 0),
      hmortrate = ifelse(!is.na(tpop_rolling_5) & (tpop_rolling_5 != 0), (hmort / tpop_rolling_5)*100000, 0),
      hsurvrate = ifelse(!is.na(tpop_rolling_5) & (tpop_rolling_5 != 0), (100000 - hmortrate), 0),
      cmortrate = ifelse(!is.na(tpop_rolling_5) & (tpop_rolling_5 != 0), (cmort / tpop_rolling_5)*100000, 0),
      csurvrate = ifelse(!is.na(tpop_rolling_5) & (tpop_rolling_5 != 0), (100000 - cmortrate), 0),
      lungcmortrate = ifelse(!is.na(tpop_rolling_5) & (tpop_rolling_5 != 0), (lungcmort / tpop_rolling_5)*100000, 0),
      lungcsurvrate = ifelse(!is.na(tpop_rolling_5) & (tpop_rolling_5 != 0), (100000 - lungcmortrate), 0),
      cdmortrate = ifelse(!is.na(tpop_rolling_5) & (tpop_rolling_5 != 0), (cdmort / tpop_rolling_5)*100000, 0),
      cdsurvrate = ifelse(!is.na(tpop_rolling_5) & (tpop_rolling_5 != 0), (100000 - cdmortrate), 0),
      clmortrate = ifelse(!is.na(tpop_rolling_5) & (tpop_rolling_5 != 0), (clmort / tpop_rolling_5)*100000, 0),
      clsurvrate = ifelse(!is.na(tpop_rolling_5) & (tpop_rolling_5 != 0), (100000 - clmortrate), 0),
      dmortrate = ifelse(!is.na(tpop_rolling_5) & (tpop_rolling_5 != 0), (dmort / tpop_rolling_5)*100000, 0),
      dsurvrate = ifelse(!is.na(tpop_rolling_5) & (tpop_rolling_5 != 0), (100000 - dmortrate), 0),
      rmortrate = ifelse(!is.na(tpop_rolling_5) & (tpop_rolling_5 != 0), (rmort / tpop_rolling_5)*100000, 0),
      rsurvrate = ifelse(!is.na(tpop_rolling_5) & (tpop_rolling_5 != 0), (100000 - rmortrate), 0),
      amortrate = ifelse(!is.na(tpop_rolling_5) & (tpop_rolling_5 != 0), (amort / tpop_rolling_5)*100000, 0),
      asurvrate = ifelse(!is.na(tpop_rolling_5) & (tpop_rolling_5 != 0), (100000 - amortrate), 0),
      omortrate = ifelse(!is.na(tpop_rolling_5) & (tpop_rolling_5 != 0), (omort / tpop_rolling_5)*100000, 0),
      osurvrate = ifelse(!is.na(tpop_rolling_5) & (tpop_rolling_5 != 0), (100000 - omortrate), 0),
      ormortrate = ifelse(!is.na(tpop_rolling_5) & (tpop_rolling_5 != 0), (ormort / tpop_rolling_5)*100000, 0),
      orsurvrate = ifelse(!is.na(tpop_rolling_5) & (tpop_rolling_5 != 0), (100000 - ormortrate), 0),
      ovmortrate = ifelse(!is.na(tpop_rolling_5) & (tpop_rolling_5 != 0), (ovmort / tpop_rolling_5)*100000, 0),
      ovsurvrate = ifelse(!is.na(tpop_rolling_5) & (tpop_rolling_5 != 0), (100000 - ovmortrate), 0),
      icd10amortrate = ifelse(!is.na(tpop_rolling_5) & (tpop_rolling_5 != 0), (icd10amort / tpop_rolling_5)*100000, 0),
      icd10asurvrate = ifelse(!is.na(tpop_rolling_5) & (tpop_rolling_5 != 0), (100000 - icd10amortrate), 0),
      icd10bmortrate = ifelse(!is.na(tpop_rolling_5) & (tpop_rolling_5 != 0), (icd10bmort / tpop_rolling_5)*100000, 0),
      icd10bsurvrate = ifelse(!is.na(tpop_rolling_5) & (tpop_rolling_5 != 0), (100000 - icd10bmortrate), 0),
      icd10dmortrate = ifelse(!is.na(tpop_rolling_5) & (tpop_rolling_5 != 0), (icd10dmort / tpop_rolling_5)*100000, 0),
      icd10dsurvrate = ifelse(!is.na(tpop_rolling_5) & (tpop_rolling_5 != 0), (100000 - icd10dmortrate), 0),
      icd10emortrate = ifelse(!is.na(tpop_rolling_5) & (tpop_rolling_5 != 0), (icd10emort / tpop_rolling_5)*100000, 0),
      icd10esurvrate = ifelse(!is.na(tpop_rolling_5) & (tpop_rolling_5 != 0), (100000 - icd10emortrate), 0),
      icd10fmortrate = ifelse(!is.na(tpop_rolling_5) & (tpop_rolling_5 != 0), (icd10fmort / tpop_rolling_5)*100000, 0),
      icd10fsurvrate = ifelse(!is.na(tpop_rolling_5) & (tpop_rolling_5 != 0), (100000 - icd10fmortrate), 0),
      icd10gmortrate = ifelse(!is.na(tpop_rolling_5) & (tpop_rolling_5 != 0), (icd10gmort / tpop_rolling_5)*100000, 0),
      icd10gsurvrate = ifelse(!is.na(tpop_rolling_5) & (tpop_rolling_5 != 0), (100000 - icd10gmortrate), 0),
      icd10hmortrate = ifelse(!is.na(tpop_rolling_5) & (tpop_rolling_5 != 0), (icd10hmort / tpop_rolling_5)*100000, 0),
      icd10hsurvrate = ifelse(!is.na(tpop_rolling_5) & (tpop_rolling_5 != 0), (100000 - icd10hmortrate), 0),
      icd10imortrate = ifelse(!is.na(tpop_rolling_5) & (tpop_rolling_5 != 0), (icd10imort / tpop_rolling_5)*100000, 0),
      icd10isurvrate = ifelse(!is.na(tpop_rolling_5) & (tpop_rolling_5 != 0), (100000 - icd10imortrate), 0),
      icd10jmortrate = ifelse(!is.na(tpop_rolling_5) & (tpop_rolling_5 != 0), (icd10jmort / tpop_rolling_5)*100000, 0),
      icd10jsurvrate = ifelse(!is.na(tpop_rolling_5) & (tpop_rolling_5 != 0), (100000 - icd10jmortrate), 0),
      icd10kmortrate = ifelse(!is.na(tpop_rolling_5) & (tpop_rolling_5 != 0), (icd10kmort / tpop_rolling_5)*100000, 0),
      icd10ksurvrate = ifelse(!is.na(tpop_rolling_5) & (tpop_rolling_5 != 0), (100000 - icd10kmortrate), 0),
      icd10lmortrate = ifelse(!is.na(tpop_rolling_5) & (tpop_rolling_5 != 0), (icd10lmort / tpop_rolling_5)*100000, 0),
      icd10lsurvrate = ifelse(!is.na(tpop_rolling_5) & (tpop_rolling_5 != 0), (100000 - icd10lmortrate), 0),
      icd10mmortrate = ifelse(!is.na(tpop_rolling_5) & (tpop_rolling_5 != 0), (icd10mmort / tpop_rolling_5)*100000, 0),
      icd10msurvrate = ifelse(!is.na(tpop_rolling_5) & (tpop_rolling_5 != 0), (100000 - icd10mmortrate), 0),
      icd10nmortrate = ifelse(!is.na(tpop_rolling_5) & (tpop_rolling_5 != 0), (icd10nmort / tpop_rolling_5)*100000, 0),
      icd10nsurvrate = ifelse(!is.na(tpop_rolling_5) & (tpop_rolling_5 != 0), (100000 - icd10nmortrate), 0),
      icd10omortrate = ifelse(!is.na(tpop_rolling_5) & (tpop_rolling_5 != 0), (icd10omort / tpop_rolling_5)*100000, 0),
      icd10osurvrate = ifelse(!is.na(tpop_rolling_5) & (tpop_rolling_5 != 0), (100000 - icd10omortrate), 0),
      icd10pmortrate = ifelse(!is.na(tpop_rolling_5) & (tpop_rolling_5 != 0), (icd10pmort / tpop_rolling_5)*100000, 0),
      icd10psurvrate = ifelse(!is.na(tpop_rolling_5) & (tpop_rolling_5 != 0), (100000 - icd10pmortrate), 0),
      icd10qmortrate = ifelse(!is.na(tpop_rolling_5) & (tpop_rolling_5 != 0), (icd10qmort / tpop_rolling_5)*100000, 0),
      icd10qsurvrate = ifelse(!is.na(tpop_rolling_5) & (tpop_rolling_5 != 0), (100000 - icd10qmortrate), 0),
      icd10rmortrate = ifelse(!is.na(tpop_rolling_5) & (tpop_rolling_5 != 0), (icd10rmort / tpop_rolling_5)*100000, 0),
      icd10rsurvrate = ifelse(!is.na(tpop_rolling_5) & (tpop_rolling_5 != 0), (100000 - icd10rmortrate), 0),
      vmortrate = ifelse(!is.na(tpop_rolling_5) & (tpop_rolling_5 != 0), (vmort / tpop_rolling_5)*100000, 0),
      vsurvrate = ifelse(!is.na(tpop_rolling_5) & (tpop_rolling_5 != 0), (100000 - vmortrate), 0),
      nmortrate = ifelse(!is.na(tpop_rolling_5) & (tpop_rolling_5 != 0), (nmort / tpop_rolling_5)*100000, 0),
      nsurvrate = ifelse(!is.na(tpop_rolling_5) & (tpop_rolling_5 != 0), (100000 - nmortrate), 0),
      xmortrate = ifelse(!is.na(tpop_rolling_5) & (tpop_rolling_5 != 0), (xmort / tpop_rolling_5)*100000, 0),
      xsurvrate = ifelse(!is.na(tpop_rolling_5) & (tpop_rolling_5 != 0), (100000 - xmortrate), 0)
    )
  
  #Last but not least its time for the education ranks, starting of with creating total pop data for each subgroup
  #year, age and sex
  x = app_rank %>% filter(race != 0) #We can keep sex == 0 because it is obtained as a separate case, race should be distinguished though
  y = x %>% group_by(age_gp, year, sex) %>% summarise(add_sum = sum(tpop_rolling_5, na.rm = TRUE))
  app_rank = app_rank %>% 
    left_join(y, by = c("age_gp", "year", "sex")) %>%
    mutate(
      tpop_sex = coalesce(add_sum, tpop_sex),
    )
  app_rank = app_rank %>%
    select(-add_sum)
  y = x %>% group_by(age_gp, year, sex, edclass) %>% summarise(add_sum = sum(tpop_rolling_5, na.rm = TRUE))
  app_rank = app_rank %>% 
    left_join(y, by = c("age_gp", "year", "sex","edclass")) %>%
    mutate(
      tpop_ed_sex = coalesce(add_sum, tpop_ed_sex),
    )
  app_rank = app_rank %>%
    select(-add_sum)
  
  #Also apply to race == 0, to take care of all the cases
  x = app_rank %>% filter(race == 0)
  y = x %>% group_by(age_gp, year, sex) %>% summarise(add_sum = sum(tpop_rolling_5, na.rm = TRUE))
  app_rank = app_rank %>% 
    left_join(y, by = c("age_gp", "year", "sex")) %>%
    mutate(
      tpop_sex = coalesce(add_sum, tpop_sex),
    )
  app_rank = app_rank %>%
    select(-add_sum)
  y = x %>% group_by(age_gp, year, sex, edclass) %>% summarise(add_sum = sum(tpop_rolling_5, na.rm = TRUE))
  app_rank = app_rank %>% 
    left_join(y, by = c("age_gp", "year", "sex","edclass")) %>%
    mutate(
      tpop_ed_sex = coalesce(add_sum, tpop_ed_sex),
    )
  app_rank = app_rank %>%
    select(-add_sum)
  
  #year, age and education level
  x = app_rank %>% filter(sex != 0, race != 0)
  y = x %>% group_by(age_gp, year, edclass) %>% summarise(add_sum = sum(tpop_rolling_5, na.rm = TRUE))
  app_rank = app_rank %>% 
    left_join(y, by = c("age_gp", "year", "edclass")) %>%
    mutate(
      tpop_ed_all = coalesce(add_sum, tpop_ed_all),
    )
  app_rank = app_rank %>%
    select(-add_sum)
  
  x = app_rank %>% filter(sex == 0, race != 0)
  y = x %>% group_by(age_gp, year, edclass) %>% summarise(add_sum = sum(tpop_rolling_5, na.rm = TRUE))
  app_rank = app_rank %>% 
    left_join(y, by = c("age_gp", "year", "edclass")) %>%
    mutate(
      tpop_ed_all = coalesce(add_sum, tpop_ed_all),
    )
  app_rank = app_rank %>%
    select(-add_sum)
  y = x %>% group_by(age_gp, year) %>% summarise(add_sum = sum(tpop_rolling_5, na.rm = TRUE))
  app_rank = app_rank %>% 
    left_join(y, by = c("age_gp", "year")) %>%
    mutate(
      tpop_all = coalesce(add_sum, tpop_all),
    )
  app_rank = app_rank %>%
    select(-add_sum)
  
  x = app_rank %>% filter(sex != 0, race == 0)
  y = x %>% group_by(age_gp, year, edclass) %>% summarise(add_sum = sum(tpop_rolling_5, na.rm = TRUE))
  app_rank = app_rank %>% 
    left_join(y, by = c("age_gp", "year", "edclass")) %>%
    mutate(
      tpop_ed_all = coalesce(add_sum, tpop_ed_all),
    )
  app_rank = app_rank %>%
    select(-add_sum)
  y = x %>% group_by(age_gp, year) %>% summarise(add_sum = sum(tpop_rolling_5, na.rm = TRUE))
  app_rank = app_rank %>% 
    left_join(y, by = c("age_gp", "year")) %>%
    mutate(
      tpop_all = coalesce(add_sum, tpop_all),
    )
  app_rank = app_rank %>%
    select(-add_sum)
  
  #With this one we have had all cases
  x = app_rank %>% filter(sex == 0, race == 0)
  y = x %>% group_by(age_gp, year, edclass) %>% summarise(add_sum = sum(tpop_rolling_5, na.rm = TRUE))
  app_rank = app_rank %>% 
    left_join(y, by = c("age_gp", "year", "edclass")) %>%
    mutate(
      tpop_ed_all = coalesce(add_sum, tpop_ed_all),
    )
  app_rank = app_rank %>%
    select(-add_sum)
  y = x %>% group_by(age_gp, year) %>% summarise(add_sum = sum(tpop_rolling_5, na.rm = TRUE))
  app_rank = app_rank %>% 
    left_join(y, by = c("age_gp", "year")) %>%
    mutate(
      tpop_all = coalesce(add_sum, tpop_all),
    )
  app_rank = app_rank %>%
    select(-add_sum)
  
  #No need to filter for those, as it is done automatically by separating sex and race
  x = app_rank
  y = x %>% group_by(age_gp, year, edclass, sex, race) %>% summarise(add_sum = sum(tpop_rolling_5, na.rm = TRUE))
  app_rank = app_rank %>% 
    left_join(y, by = c("age_gp", "year", "edclass", "sex", "race")) %>%
    mutate(
      tpop_ed_race_sex = coalesce(add_sum, tpop_ed_race_sex),
    )
  app_rank = app_rank %>%
    select(-add_sum)
  #age, year, race and sex
  y = x %>% group_by(age_gp, year, sex, race) %>% summarise(add_sum = sum(tpop_rolling_5, na.rm = TRUE))
  app_rank = app_rank %>% 
    left_join(y, by = c("age_gp", "year", "sex", "race")) %>%
    mutate(
      tpop_race_sex = coalesce(add_sum, tpop_race_sex),
    )
  app_rank = app_rank %>%
    select(-add_sum)
  
  #Make sure that the dataframe is arranged by edclass, otherwise we will get trouble using lag(cum_ed)
  app_rank = app_rank %>%
    arrange(year, age_gp, race, sex, edclass)
  
  #Now all the separate tpop values are determined and we can start with creating the education ranks
  app_rank = app_rank %>%
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
  app_rank = app_rank %>%
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
  app_rank = app_rank %>%
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
  return(app_rank)
}

################################################################################
#This function adds the mort values to the mort means dataframe that is created by Novosad
makeMortMeans = function(app_rank, mort_means){
  app_rank = app_rank %>% 
    mutate(
      race = ifelse(race == 0, 5, race),
      nmort = tmort - dmort,
      omort = omort + cdmort + clmort
    )
  #Add the covid cause to the mort means data frame, as this one did not exist yet
  mort_means = mort_means %>%
    bind_rows( mort_means %>%
                 select(year, age, sex, race) %>%
                 distinct() %>%
                 mutate(cause = "v")
               )
  mort_means = mort_means %>%
    bind_rows( mort_means %>%
                 select(year, age, sex, race) %>%
                 distinct() %>%
                 mutate(cause = "x")
    )
  #Combine the mortality numbers for each group
  cum_rank = app_rank %>%
    group_by(year, age, race, sex) %>%
    summarise(
      tpop_rolling = sum(tpop_rolling_5, na.rm = TRUE),
      morta = sum(amort, na.rm = TRUE),
      mortc = sum(cmort, na.rm = TRUE),
      mortd = sum(dmort, na.rm = TRUE),
      morth = sum(hmort, na.rm = TRUE),
      mortn = sum(nmort, na.rm = TRUE),
      morto = sum(omort, na.rm = TRUE),
      mortt = sum(tmort, na.rm = TRUE),
      mortv = sum(vmort, na.rm = TRUE),
      mortx = sum(xmort, na.rm = TRUE)
    )
  mort_means = mort_means %>%
    left_join(cum_rank, by = c("year", "age", "sex", "race")) %>%
    mutate(
      #Now its time to add the mortality values to the dataframe
      mort = case_when(
        cause == "a" ~ (morta / tpop_rolling) * 100000,
        cause == "c" ~ (mortc / tpop_rolling) * 100000,
        cause == "d" ~ (mortd / tpop_rolling) * 100000,
        cause == "h" ~ (morth / tpop_rolling) * 100000,
        cause == "n" ~ (mortn / tpop_rolling) * 100000,
        cause == "o" ~ (morto / tpop_rolling) * 100000,
        cause == "t" ~ (mortt / tpop_rolling) * 100000,
        cause == "v" ~ (mortv / tpop_rolling) * 100000,
        cause == "x" ~ (mortx / tpop_rolling) * 100000,
        TRUE ~ 0
      )
    )
  mort_means = mort_means %>%
    select(-c("tpop_rolling", "morta", "mortc", "mortd", "morth", "mortn", "morto", "mortt", "mortv", "mortx")) %>%
    arrange(year, age, sex, race, cause)
  return(mort_means)
}

################################################################################
#This function creates a time series for each group on total population, total mortality and deaths of despair
#The results from this function did not make it to the final paper
makeTimeSeries = function(app_rank){
  #Select the data we want in the time series and their control variables
  app_rank = app_rank %>% select(year, edclass, age, race, sex, tpop_rolling_5, tmort, dmort)
  
  #Create a combination of all the variables
  app_rank = app_rank %>%
    mutate(name = paste(age, race, sex, edclass, sep = "_")) %>%
    rename(tpop = tpop_rolling_5) %>%
    select(year, name, tpop, tmort, dmort) %>%
    arrange(year)
  
  timeseries = app_rank %>%
    pivot_wider(names_from = name, values_from = c(tpop, tmort, dmort))
  
  return(timeseries)
}

#This function creates the data for the Table in Appendix B, it calculates the share of each cause of death on the total mortality for a given year
makeProportions = function(app_rank, y){
  #Create a data frame for only a given year and separate race and sex, for age 25-69
  year_app = app_rank %>% filter(year == y, race != 0, sex != 0, age < 70)
  #Subtract all different death causes
  mort_names = grep("mort$", colnames(year_app), value = TRUE)
  mort_names = setdiff(mort_names, "tmort")
  #Create a new data frame for all causes
  df = data.frame(
    cause = mort_names,
    share = NA
  )
  total_mort = sum(year_app$tmort)
  for(mort in mort_names){
    cause_mort = sum(year_app[[mort]])
    df$share[df$cause == mort] = 100 * cause_mort / total_mort
  }
  #NOTE: Diseases in the respiratory system is cdmort + icd10jmort (J0-J99)
  return(df)
}

#This function calculates the share of covid as first death cause to total mortality, on a given year
#The results did not make it to the final paper
shareOfCovid = function(app_rank, y){
  year_app = app_rank %>%
    select(year, age, race, sex, edclass, tmort, vmort) %>% 
    filter(year == y, sex != 0, race != 0, age < 70) 
  
  tmort_all = sum(year_app$tmort)
  
  #calculate share of corona mortality in each education class
  year_app_edclass = year_app %>% 
    group_by(edclass) %>%
    summarise(vmort_edclass = sum(vmort, na.rm = TRUE),
              tmort_edclass = sum(tmort, na.rm = TRUE)) %>%
    mutate(share_edclass = 100 * vmort_edclass / tmort_all,
           share_tmort_edclass = 100 * vmort_edclass / tmort_edclass) %>%
    ungroup()
  
  #calculate the share of corona mortality for each education class
  totalvrate = sum(year_app_edclass$share_edclass)
  
  year_app_edclass = year_app_edclass %>%
    mutate(vshare_edclass = 100 * share_edclass / totalvrate)
  
  #calculate share of corona mortality in each education class per age group
  year_app_age_edclass = year_app %>% 
    group_by(age, edclass) %>%
    summarise(vmort_age_edclass = sum(vmort, na.rm = TRUE),
              tmort_age_edclass = sum(tmort, na.rm = TRUE)) %>%
    mutate(share_age_edclass = 100 * vmort_age_edclass / tmort_all,
           share_tmort_age_edclass = 100 * vmort_age_edclass / tmort_age_edclass) %>%
    ungroup()
  
  extra_year = year_app_age_edclass %>%
    group_by(age) %>%
    summarise(vmort_share_age = sum(share_age_edclass, na.rm = TRUE))
  
  year_app_age_edclass = year_app_age_edclass %>%
    left_join(extra_year, by = "age") %>%
    mutate(vshare_edclass_by_age = ifelse(vmort_share_age != 0, 100 * share_age_edclass / vmort_share_age))
  
  #calculate the share of corona mortality for each age and edclass
  totalvrate = sum(year_app_age_edclass$share_age_edclass)
  
  year_app_age_edclass = year_app_age_edclass %>%
    mutate(vshare_age_edclass = 100 * share_age_edclass / totalvrate)
  
  #calculate share of corona mortality in each age group
  year_app_age = year_app %>%
    group_by(age) %>%
    summarise(vmort_age = sum(vmort, na.rm = TRUE),
              tmort_age = sum(tmort, na.rm = TRUE)) %>%
    mutate(share_age = 100 * vmort_age / tmort_all,
           share_tmort_age = 100 * vmort_age / tmort_age) %>%
    ungroup()
  
  #calculate the share of corona mortality for each age
  totalvrate = sum(year_app_age$share_age)
  
  year_app_age = year_app_age %>%
    mutate(vshare_age = 100 * share_age / totalvrate)
  
  return(list(by_edclass = year_app_edclass, by_age_edclass = year_app_age_edclass, by_age = year_app_age))
}
################################################################################
#Here all the functions are initialized and the program can be run
#starting of with deriving the population data and  mortality numbers per group

#The data that Novosad et al. used, we use this to compare our constructed aggregated data with theirs
#read mortality_by_ed_group_data from Novosad
US_raw_data = read_dta("path")
full_data = adjRaw(US_raw_data)
#Deleting all the existing values on mortality and population from the file
original_full_data = full_data %>%
  mutate(across(-c("year", "edclass", "race", "age", "sex"), ~ NA))

#Read the total population file and adjust such that it is usable as function input
tpop = read.csv("path")
tpop = tpop[tpop$year != 1901,]
tpop = tpop  %>%
  rename( "eduRank" = "edclass",
          "agegr" = "age",
          "origin" = "race") %>% 
  mutate(eduRank = ifelse(eduRank == 0, 9, eduRank),
         inst = ifelse(is.na(inst), 0, inst),
         tpop_2 = non_inst + inst)

#Automatic mortality file reader, use 
##Mortality Data - Vital Statistics NCHS Multiple Cause of Death Data##
#U.S. Data Files - Death Data
#from National Bureau of Economic Research
#2021 is not a .csv file, so we calculate it 'separately'
path_map = "path_start"
path_spec = "mort"
path_end = ".csv"

#Loop over the years 2000-2020 to obtain a full data set
for(i in 2000:2021){
  #Construct a path and aggregate data for the years until 2020
  if(i < 2021){
  path = paste0(path_map, path_spec, i, path_end)
  df_actual = read.csv(path)
  }
  
  #For 2021, we have to read a .dta file (.csv was not avaiable)
  if(i == 2021){
    df_actual = read_dta("path")
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
#Convert to .csv file
write.csv(original_full_data, "path")
#Subtract the part of the data that is useful for our creating data needed to calculate mortality bounds
use_full_data = original_full_data %>% filter(year %in% 2000:2021, age != "70-74")

################################################################################

#Creation of the appended_mort_rank, download file from Novosads github
appended_rank_mort = read.csv("path")
#Store Novosad's values and work with the other frame
nov_app_rank_mort = appended_rank_mort
#Create a data frame for 'new' years (2019-2021), i.e. the years that are not used by Novosad
create_years = expand.grid(
  year = 2019:2021,
  edclass = unique(appended_rank_mort$edclass),
  age_gp = unique(appended_rank_mort$age_gp),
  race = unique(appended_rank_mort$race),
  sex = unique(appended_rank_mort$sex)
)
#Set all useful variables to NA, we will generate those ourselves
col_to_na = setdiff(names(appended_rank_mort), c("year", "age_gp", "race", "edclass","sex"))
for(col in col_to_na){
  appended_rank_mort[[col]] = NA
} 
appended_rank_mort = bind_rows(appended_rank_mort, create_years) %>%
  arrange(year, age_gp, race, sex, edclass)
use_this_app = appended_rank_mort %>% filter(year >= 2000, age_gp != 70)

#Add the empty app_rank to the function
new_appended = makeAppended(use_full_data, use_this_app)
new_appended = new_appended %>% 
  rename(age = age_gp) %>%
  mutate(race = ifelse(race == 0, 5, race))

#Convert to .csv and get rid of the colname
write.csv(new_appended, "path_to_store", quote = FALSE)

#Also create time series for each group on tpop, tmort and dmort
app_time_series = makeTimeSeries(new_appended)
write.csv(app_time_series, "path_to_store", quote = FALSE)

#Now its time for mort_means, downloadable from Novosads github
mort_means = read.csv("path")

#Adjust the mort means frame s.t. its suitable for 2000-2021
empty_mort = mort_means %>%
  mutate(mort = NA)

#Expand the grid and remove the mort values, i.e. set to NA, we will generate those ourselves
other_columns = setdiff(names(empty_mort), c("year", "age", "race", "sex", "cause"))
create_years = expand.grid(
  year = 2019:2021,
  age = unique(empty_mort$age),
  race = unique(empty_mort$race),
  sex = unique(empty_mort$sex),
  cause = unique(empty_mort$cause)
)
# Add NA columns for all other variables
for (col in other_columns) {
  create_years[[col]] = NA
}
#Take the empty mort means for 2000-20212
empty_mort = bind_rows(empty_mort, create_years) %>%
  arrange(year, age, race, sex)
empty_mort = empty_mort %>% filter(year >= 2000, age != 70)
#Determine the mort values
new_mortm = makeMortMeans(new_appended, empty_mort)
write.csv(new_mortm, "path_to_store", quote = FALSE)

################################################################################
#Making a time series on the share of each death cause on the total mortality
#Part of this time series can be seen in the table on the share of death in Appendix B
for(i in 2000:2021){
  if(i == 2000){
    all_share = makeProportions(new_appended, i)
    all_share = all_share %>% rename_with(~ paste0("share", i), .cols = "share")
    next
  }
  new_share = makeProportions(new_appended, i)
  all_share = all_share %>%
    left_join(new_share, by = "cause") %>%
    rename_with(~ paste0("share", i), .cols = "share")
}
write.csv(all_share, "store_path", quote = FALSE)
