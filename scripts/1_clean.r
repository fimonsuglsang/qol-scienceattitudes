

#loading packages
library(tidyverse)
library(psych)


# BAROM -------------------------------------------------------------------

#reading eurobarometer data
rawbarom <- haven::read_sav("./data/ebs_526_SPSS.sav")

#selecting and recoding variables
rawbarom %>% 
  transmute(
    ###Life satisfaction and life direction###
    lifesat = as_factor(na_if(D70, 5)), #as factor, DK = NA
    lifesat = fct_relevel(lifesat, c("Not at all satisfied", "Not very satisfied", "Fairly satisfied", "Very satisfied")),
    lifedir = as_factor(na_if(D73_4, 4)),
    lifedir = fct_relevel(lifedir, "Things are going in the wrong direction", "Neither the one nor the other (SPONTANEOUS)", "Things are going in the right direction"),
    ###Outcomes###
    across(matches("QA10_\\d"), na_if, 6), #DK = NA
    across(matches("QA11_\\d"), na_if, 6), #DK = NA
    ###Controls###
    country = as_factor(B), #getting country strings
    country = str_replace(country, "WG", "Germany"), #merging germanies
    country = str_replace(country, "EG", "Germany"),
    country = str_remove(country, " \\(..\\)"),
    gender = as_factor(D10), #gender as factor
    age = as_factor(SD5), #age as factor
    pol = case_when(D1<11 ~ D1), #refusal an DK = NA
    rel = case_when(D90.1<11 ~ D90.1),
    class = as_factor(replace(D63, D63>5, NA)),
    edu = case_when(SD3b<31 ~ SD3b),
    ###Science literacy###
    across(c(QA20_2, QA20_4, QA20_8), \(x) 1+x), #fixing direction
    across(matches("QA20"), \(x) case_when(x == 2 ~ T, .default = F)),
    ###Science engagement###
    across(matches("QA14_\\d"), na_if, 5)
  ) %>% 
  #putting together experimental questions
  mutate(
    QA10_1n2 = case_when(
      is.na(QA10_1) == F ~ QA10_1, #different wordings, taken as one
      is.na(QA10_2) == F ~ QA10_2
    ),
    QA11_4n5 = case_when(
      is.na(QA11_4) == F ~ QA11_4, #different wordings, taken as one
      is.na(QA11_5) == F ~ 6-QA11_5 #reversed above
    )
  ) %>% 
  
  #science literary and engagement measure
  mutate(
    scilit = rowSums(select(., paste0("QA20_", c(1:6, 8:9)))),
    scieng = rowMeans(select(., QA14_1:QA14_12))
  ) -> cleanbarom


#factor scores for outcomes
bind_cols(
  cleanbarom,
  
  #getting factor scores, and merging to data
  factor.scores(
    select(
      cleanbarom,
      matches("QA10_\\d"), -QA10_1, -QA10_2,
      matches("QA11_\\d"), -QA11_4, -QA11_5
    ),
    fa(select(
      cleanbarom,
      matches("QA10_\\d"), -QA10_1, -QA10_2,
      matches("QA11_\\d"), -QA11_4, -QA11_5
    ), 2)
  )$scores %>% 
    as_tibble() %>% 
    rename(res = MR1, prom = MR2)
) %>%
  #standardizing and fixing direction
  mutate(
    across(
      c(prom, res, pol, rel, edu, scilit, scieng),
      \(x) (x-mean(x, na.rm = T))/sd(x, na.rm = T)
    ),
    prom = prom*-1, #reversed to have positive high
  )  -> cleanbarom



# WVS ---------------------------------------------------------------------

#reading WVS data
load("data/WVS_Cross-National_Wave_7_rData_v5_0.rdata")
as_tibble(`WVS_Cross-National_Wave_7_v5_0`) -> rawwvs
rm(`WVS_Cross-National_Wave_7_v5_0`)

#selecting and recoding variables
rawwvs %>% 
  transmute(
    ###Life satisfation and life control###
    lifesat = case_when(Q49>0 ~ Q49),
    lifecontrol = case_when(Q48>0 ~ Q48),
    ###Outcomes###
    across(c(Q158:Q163), ~case_when(.>0 ~ .)),
    ###Controls###
    ccode = as_factor(B_COUNTRY_ALPHA),
    sex = case_when(Q260>0 ~ Q260),
    sex = case_when(sex == 1  ~ "Male", sex == 2 ~ "Female"),
    age = case_when(Q262>0 ~ Q262),
    pol = case_when(Q240>0 ~ Q240),
    rel = case_when(Q164>0 ~ Q164),
    class = case_when(Q287>0 ~ Q287) %>% as_factor(),
    edu = case_when(Q275>0 ~ Q275),
    income = case_when(Q288>0 ~ Q288)
  ) -> cleanwvs

#country names
left_join(
  cleanwvs,
  read_csv("data/countrycodes.csv") %>% rename(ccode = 3),
) %>% 
  rename(country = name) -> cleanwvs

#factor scores for outcomes
bind_cols(
  cleanwvs,
  
  #getting factor scores, and merging to data
  factor.scores(
    select(
      cleanwvs,
      Q158:Q163
    ),
    fa(select(
      cleanwvs,
      Q158:Q163
    ), 2)
  )$scores %>% 
    as_tibble() %>% 
    rename(prom = MR1, res = MR2)
) %>% 
  #standardizing
  mutate(
    across(
      c(prom, res, lifesat, lifecontrol, pol, rel, edu, income),
      \(x) (x-mean(x, na.rm = T))/sd(x, na.rm = T)
    )
  ) -> cleanwvs



