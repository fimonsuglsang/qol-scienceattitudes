

#loading packages
library(tidyverse)
library(psych)



# descriptive statistics --------------------------------------------------

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
  #reversing attitude items to have positive always be high
  mutate(across(c(QA10_1n2, QA10_3, QA10_4, QA10_5, QA10_6), \(x) 6-x)) %>% 
  select(-QA10_1, -QA10_2, -QA11_4, -QA11_5) %>% 
  
  modelsummary::datasummary_skim(type = "numeric", output = "flextable") %>% 
  flextable::save_as_docx(path = "tables/eudecrnum.docx")

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
  #reversing attitude items to have positive always be high
  mutate(across(c(QA10_1n2, QA10_3, QA10_4, QA10_5, QA10_6), \(x) 6-x)) %>% 
  
  modelsummary::datasummary_skim(type = "categorical", output = "flextable") %>% 
  flextable::save_as_docx(path = "tables/eudecrcat.docx")


rawwvs %>% 
  transmute(
    ###Life satisfation and life control###
    lifesat = case_when(Q49>0 ~ Q49),
    lifecontrol = case_when(Q48>0 ~ Q48),
    ###Outcomes###
    across(c(Q158:Q163), ~case_when(.>0 ~ .))
  ) %>% 
  #reversing attitude items to have positive always be high
  mutate(across(c(Q160, Q161, Q162), \(x) 11-x))  %>% 
  
  modelsummary::datasummary_skim(type = "numeric", output = "flextable") %>% 
  flextable::save_as_docx(path = "tables/wvsdecr.docx")


# Items -------------------------------------------------------------------

#making tables of factor loadings, and saving as .docx

fa(
  cor(
    select(
      cleanbarom,
      "Science and technology make our lives (easier,) healthier (and more comfortable)" = QA10_1n2,
      "Science prepares the younger generation to act as well-informed citizens" = QA10_3,
      "Thanks to scientific and technological advances, the Earth’s natural resources will be" = QA10_4,
      "Thanks to science and technology, there will be more opportunities for future generation" = QA10_5,
      "Artificial intelligence and automation will create more jobs than they will eliminate" = QA10_6,
      "We depend too much on science and not enough on faith" = QA10_7,
      "The applications of science and technology can threaten human rights" = QA10_8,
      "Science makes our ways of life change too fast" = QA10_9,
      "Because of their knowledge, scientists have a power that makes them dangerous" = QA10_10,
      "We can no longer trust scientists to tell the truth about controversial scientific and technological issues because they depend more and more on money from industry" = QA11_1,
      "Scientists only look at very specific issues and do not consider problems from a wider perspective" = QA11_2,
      "Nowadays, the problems we are facing are so complex that scientists are no longer able to understand them" = QA11_3,
      "Scientists should (not) intervene in political debate [...]" = QA11_4n5,
      "Scientists should be held accountable for the misuse of their discoveries by other people" = QA11_6
    ), 
    use = "pairwise.complete.obs"
  ),
  2
) %>% 
  loadings() %>% 
  unclass() %>% 
  as_tibble(rownames = "var") %>% 
  modelsummary::datasummary_df(
    output = "flextable"
  ) %>% 
  flextable::save_as_docx(path = "tables/euitems.docx")

fa(
  cor(
    select(
      cleanwvs,
      "Science and technology are making our lives healthier, easier, and more comfortable." = Q158,
      "Because of science and technology, there will be more opportunities for the next generation." = Q159,
      "We depend too much on science and not enough on faith." = Q160,
      "One of the bad effects of science is that it breaks down people’s ideas of right and wrong" = Q161,
      "It is not important for me to know about science in my daily life." = Q162,
      "All things considered, would you say that the world is better off, or worse off, because of science and technology?" = Q163
    ), 
    use = "pairwise.complete.obs"
  ),
  2
) %>% 
  loadings() %>% 
  unclass() %>% 
  as_tibble(rownames = "var") %>% 
  modelsummary::datasummary_df(
    output = "flextable"
  ) %>% 
  flextable::save_as_docx(path = "tables/wvsitems.docx")



# cross country factor scores ---------------------------------------------


# barom -------------------------------------------------------------------



cleanbarom %>% 
  select(country) %>% 
  distinct() %>% 
  as_vector() -> clist

cfactoreu <- tibble()

for(i in 1:length(clist)) {
  
  temp <- filter(cleanbarom, country == clist[i])
  
  bind_rows(
    cfactoreu,
    fa(
      cor(
        select(
          #renaming variables
          temp,
          "better lives" = QA10_1n2,
          "informed citizens" = QA10_3,
          "natural resources" = QA10_4,
          "opportunities" = QA10_5,
          "AI = jobs" = QA10_6,
          "science v. faith" = QA10_7,
          "human rights" = QA10_8,
          "life change too fast" = QA10_9,
          "dangerous knowledge" = QA10_10,
          "special interests" = QA11_1,
          "specific knowledge" = QA11_2,
          "too complex" = QA11_3,
          "intervene in politics" = QA11_4n5,
          "misuse by others" = QA11_6
        ), 
        use = "pairwise.complete.obs"
      ),
      2
    ) %>% 
      loadings() %>% 
      unclass() %>% 
      as_tibble(rownames = "var") %>% 
      mutate(country = clist[i])
  ) -> cfactoreu
  
  print(paste(i,"of 38:", unique(temp$country)))
}


#Figure

bind_rows(
  
  filter(
    cfactoreu,
    country != "Albania" &
      country != "Kosovo" &
      country != "Montenegro"
  ) %>% 
    rename(
      Promise = MR2,
      Reservations = MR1
    ),
  filter(
    cfactoreu,
    country == "Albania" |
      country == "Kosovo" |
      country == "Montenegro"
  ) %>% 
    rename(
      Promise = MR1,
      Reservations = MR2
    )
) %>% 
  mutate(var = fct_reorder(var, Promise, mean) %>% fct_rev()) %>% 
  
  ggplot(
    aes(group = var, x = Reservations, y = Promise)
  ) +
  geom_hline(yintercept = 0, linetype = "longdash") +
  geom_vline(xintercept = 0, linetype = "longdash") +
  geom_point() +
  jtools::theme_nice() +
  scale_y_continuous(position = "right") +
  ylab("") +
  xlab("") +
  theme(plot.title = element_text(hjust = .5)) +
  facet_wrap(var~., ncol = 3)


ggsave("plots/figA1.pdf", dpi = 350, units = "cm", width = 18, height = 18)


# wvs ---------------------------------------------------------------------



cleanwvs %>% 
  select(country) %>% 
  distinct() %>% 
  as_vector() -> clist

cfactorwvs <- tibble()

for(i in 1:50) {
  
  temp <- filter(cleanwvs, !is.na(pol), !is.na(class), !is.na(prom), !is.na(country))
  temp <- filter(cleanwvs, country == clist[i])
  
  
  bind_rows(
    cfactorwvs,
    fa(
      cor(
        select(
          #renaming variables
          temp,
          "better lives" = Q158,
          "opportunities" = Q159,
          "science v. faith" = Q160,
          "blur right/wrong" = Q161,
          "daily life" = Q162,
          "world better off" =Q163
        ), 
        use = "pairwise.complete.obs"
      ),
      2
    ) %>% 
      loadings() %>% 
      unclass() %>% 
      as_tibble(rownames = "var") %>% 
      mutate(country = clist[i])
  ) -> cfactorwvs
  print(paste(i,"of 50:", unique(temp$country)))
}

#Figure

bind_rows(
  filter(cfactorwvs, country != "Indonesia") %>% 
    rename(Promise = MR1,Reservations = MR2),
  filter(cfactorwvs, country == "Indonesia") %>% 
    rename(Promise = MR2, Reservations = MR1)
) %>% 
  mutate(var = fct_reorder(var, Promise, mean) %>% fct_rev()) %>% 
  
  ggplot(
    aes(group = var, x = Reservations, y = Promise)
  ) +
  geom_hline(yintercept = 0, linetype = "longdash") +
  geom_vline(xintercept = 0, linetype = "longdash") +
  geom_point() +
  jtools::theme_nice() +
  scale_y_continuous(position = "right") +
  ylab("") +
  xlab("") +
  theme(plot.title = element_text(hjust = .5)) +
  facet_wrap(var~., ncol = 2)


ggsave("plots/figA2.pdf", dpi = 350, units = "cm", width = 18, height = 18)

# barom -------------------------------------------------------------------

cleanbarom %>% 
  select(country) %>% 
  distinct() %>% 
  as_vector() -> clist

cfactoreu <- tibble()

for(i in 1:length(clist)) {
  
  temp <- filter(cleanbarom, country == clist[i])
  
  bind_rows(
    cfactoreu,
    fa(
      cor(
        select(
          #renaming variables
          temp,
          "better lives" = QA10_1n2,
          "informed citizens" = QA10_3,
          "natural resources" = QA10_4,
          "opportunities" = QA10_5,
          "AI = jobs" = QA10_6,
          "science v. faith" = QA10_7,
          "human rights" = QA10_8,
          "life change too fast" = QA10_9,
          "dangerous knowledge" = QA10_10,
          "special interests" = QA11_1,
          "specific knowledge" = QA11_2,
          "too complex" = QA11_3,
          "intervene in politics" = QA11_4n5,
          "misuse by others" = QA11_6
        ), 
        use = "pairwise.complete.obs"
      ),
      2
    ) %>% 
      loadings() %>% 
      unclass() %>% 
      as_tibble(rownames = "var") %>% 
      mutate(country = clist[i])
  ) -> cfactoreu
  
  print(paste(i,"of 38:", unique(temp$country)))
}


#Figure

bind_rows(
  
  filter(
    cfactoreu,
    country != "Albania" &
      country != "Kosovo" &
      country != "Montenegro"
  ) %>% 
    rename(
      Promise = MR2,
      Reservations = MR1
    ),
  filter(
    cfactoreu,
    country == "Albania" |
      country == "Kosovo" |
      country == "Montenegro"
  ) %>% 
    rename(
      Promise = MR1,
      Reservations = MR2
    )
) %>% 
  mutate(var = fct_reorder(var, Promise, mean) %>% fct_rev()) %>% 
  
  ggplot(
    aes(group = var, x = Reservations, y = Promise)
  ) +
  geom_hline(yintercept = 0, linetype = "longdash") +
  geom_vline(xintercept = 0, linetype = "longdash") +
  geom_point() +
  jtools::theme_nice() +
  scale_y_continuous(position = "right") +
  ylab("") +
  xlab("") +
  theme(plot.title = element_text(hjust = .5)) +
  facet_wrap(var~., ncol = 2)

ggsave("plots/figA1.pdf", dpi = 350, units = "cm", width = 18, height = 18)


# wvs ---------------------------------------------------------------------



cleanwvs %>% 
  select(country) %>% 
  distinct() %>% 
  as_vector() -> clist

cfactorwvs <- tibble()

for(i in 1:50) {
  
  temp <- filter(cleanwvs, !is.na(pol), !is.na(class), !is.na(prom), !is.na(country))
  temp <- filter(cleanwvs, country == clist[i])
  
  
  bind_rows(
    cfactorwvs,
    fa(
      cor(
        select(
          #renaming variables
          temp,
          "better lives" = Q158,
          "opportunities" = Q159,
          "science v. faith" = Q160,
          "blur right/wrong" = Q161,
          "daily life" = Q162,
          "world better off" =Q163
        ), 
        use = "pairwise.complete.obs"
      ),
      2
    ) %>% 
      loadings() %>% 
      unclass() %>% 
      as_tibble(rownames = "var") %>% 
      mutate(country = clist[i])
  ) -> cfactorwvs
  print(paste(i,"of 50:", unique(temp$country)))
}

#Figure



bind_rows(
  filter(cfactorwvs, country != "Indonesia") %>% 
    rename(Promise = MR1,Reservations = MR2),
  filter(cfactorwvs, country == "Indonesia") %>% 
    rename(Promise = MR2, Reservations = MR1)
) %>% 
  mutate(var = fct_reorder(var, Promise, mean) %>% fct_rev()) %>% 
  
  ggplot(
    aes(group = var, x = Reservations, y = Promise)
  ) +
  geom_hline(yintercept = 0, linetype = "longdash") +
  geom_vline(xintercept = 0, linetype = "longdash") +
  geom_point() +
  jtools::theme_nice() +
  scale_y_continuous(position = "right") +
  ylab("") +
  xlab("") +
  theme(plot.title = element_text(hjust = .5)) +
  facet_wrap(var~., ncol = 2)


ggsave("plots/figA2.pdf", dpi = 350, units = "cm", width = 18, height = 10)



# Regressions -------------------------------------------------------------

#making tables of regressions and saving as docx

modelsummary::modelsummary(
  list(
    "Reservations" = lm(data = cleanbarom, res~gender+age+pol+rel+edu+class+scilit+scieng+lifesat+country),
    "Reservations" = lm(data = cleanbarom, res~gender+age+pol+rel+edu+class+scilit+scieng+lifedir+country),
    "Promise" = lm(data = cleanbarom, prom~gender+age+pol+rel+edu+class+scilit+scieng+lifesat+country),
    "Promise" = lm(data = cleanbarom, prom~gender+age+pol+rel+edu+class+scilit+scieng+lifedir+country)
  ),
  coef_omit = "country",
  stars = T,
  output = "flextable"
) %>% 
  flextable::save_as_docx(path = "tables/euregs.docx")

modelsummary::modelsummary(
  list(
    "Reservations" = lm(data = cleanwvs, res~sex+age+pol+rel+edu+class+income+lifesat+country),
    "Reservations" = lm(data = cleanwvs, res~sex+age+pol+rel+edu+class+income+lifecontrol+country),
    "Promise" = lm(data = cleanwvs, prom~sex+age+pol+rel+edu+class+income+lifesat+country),
    "Promise" = lm(data = cleanwvs, prom~sex+age+pol+rel+edu+class+income+lifecontrol+country)
  ),
  coef_omit = "country",
  stars = T,
  output = "flextable"
) %>% 
  flextable::save_as_docx(path = "tables/wvsregs.docx")


