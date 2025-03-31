library(writexl)

tpop = read.csv("C:/Users/michi/OneDrive/Documenten/Eur Jaar 3/Blok 4/Major/Data US/processed_all_races.csv")
tpop = tpop[tpop$year != 1901,]
tpop = tpop %>% mutate(educ = ifelse(educ == 0, 9, educ)) %>%
  arrange(year, race, sex, age, educ)


for(i in 0:1759){
  totpop_2 = sum(tpop$tpop_2[(5*i + 1):(5*i + 4)])
  if(is.na(totpop_2) || totpop_2 == 0) next
  tpop$tpop_2[5*i + 1] = tpop$tpop_2[5*i + 1] + (tpop$tpop_2[5*i + 1]/totpop_2)*tpop$tpop_2[5*i + 5]
  tpop$tpop_2[5*i + 2] = tpop$tpop_2[5*i + 2] + (tpop$tpop_2[5*i + 2]/totpop_2)*tpop$tpop_2[5*i + 5]
  tpop$tpop_2[5*i + 3] = tpop$tpop_2[5*i + 3] + (tpop$tpop_2[5*i + 3]/totpop_2)*tpop$tpop_2[5*i + 5]
  tpop$tpop_2[5*i + 4] = tpop$tpop_2[5*i + 4] + (tpop$tpop_2[5*i + 4]/totpop_2)*tpop$tpop_2[5*i + 5]
}
tpop = tpop %>% filter(educ != 9)
write.csv(tpop, "C:/Users/michi/OneDrive/Documenten/Eur Jaar 3/Blok 4/Major/Data US/processed_all_races_adjusted.csv")
