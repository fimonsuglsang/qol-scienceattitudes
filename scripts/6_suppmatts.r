

#loading packages
library(tidyverse)
library(psych)



# Items -------------------------------------------------------------------

#making tables of factorloadings, and saving as docx

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


# Res world regs ----------------------------------------------------------

#reproducing figure from "world.r" for the reservations outcome

cpreds_wvs <- tibble()

#countries with data on all vars
for(i in c(1:50)){
  
  temp <- filter(cleanwvs, !is.na(pol), !is.na(class), !is.na(res), !is.na(country))
  temp <- filter(temp, country == unique(temp$country)[i])
  
  temp <- mutate(temp, 
                 lifesat = (lifesat-mean(lifesat, na.rm = T))/sd(lifesat, na.rm = T),
                 lifecontrol = (lifecontrol-mean(lifecontrol, na.rm = T))/sd(lifecontrol, na.rm = T)
  )
  
  print(paste(i,"of 50:", unique(temp$country)))
  
  cpreds_wvs <- bind_rows(
    cpreds_wvs,
    lm(data = temp, res~sex+age+pol+rel+edu+class+income+lifesat) %>% 
      broom::tidy() %>% 
      filter(term == "lifesat") %>%
      mutate(country = unique(temp$country)),
    
    lm(data = temp, res~sex+age+pol+rel+edu+class+income+lifecontrol) %>% 
      broom::tidy() %>% 
      filter(term == "lifecontrol") %>%
      mutate(country = unique(temp$country))
  )
}

cpreds_eu <- tibble()

#countries with data on all vars
for(i in c(1:38)){
  
  temp <- filter(cleanbarom, country == unique(cleanbarom$country)[i])
  
  temp <- mutate(temp, 
                 lifesat = (as.numeric(lifesat)-mean(as.numeric(lifesat), na.rm = T))/sd(as.numeric(lifesat), na.rm = T),
                 lifedir = (as.numeric(lifedir)-mean(as.numeric(lifedir), na.rm = T))/sd(as.numeric(lifedir), na.rm = T)
  )
  
  print(paste(i,"of 38:", unique(cleanbarom$country)[i]))
  
  cpreds_eu <- bind_rows(
    cpreds_eu,
    lm(data = temp, res~gender+age+pol+rel+edu+class+scilit+scieng+lifesat) %>% 
      broom::tidy() %>% 
      filter(term == "lifesat") %>%
      mutate(country = unique(temp$country)),
    
    lm(data = temp, res~gender+age+pol+rel+edu+class+scilit+scieng+lifedir) %>% 
      broom::tidy() %>% 
      filter(term == "lifedir") %>%
      mutate(country = unique(temp$country))
  )
}


#figure
cpreds_wvs %>%
  mutate(
    sig = case_when(p.value<.05 ~ T, .default = F),
    term = case_when(
      term == "lifesat" ~ "Satisfaction",
      term == "lifecontrol" ~ "Control"
    ),
    term = fct_inorder(term)
  ) %>% 
  ggplot(aes(y = tidytext::reorder_within(country, estimate, term))) +
  tidytext::scale_y_reordered() +
  jtools::theme_nice() +
  theme(axis.text.y = element_text(hjust = 1)) +
  ggtitle("WVS") +
  
  cpreds_eu %>%
  mutate(
    sig = case_when(p.value<.05 ~ T, .default = F),
    term = case_when(
      term == "lifesat" ~ "Satisfaction",
      term == "lifedir" ~ "Direction"
    ),
    term = fct_inorder(term)
  ) %>% 
  ggplot(aes(y = tidytext::reorder_within(country, estimate, term))) +
  tidytext::scale_y_reordered(position = "right") +
  jtools::theme_nice() +
  ggtitle("EU") +
  
  plot_layout(ncol=2) &
  ylab("") &
  coord_cartesian(xlim = c(-.2, .3)) &
  theme(plot.title = element_text(hjust = .5)) &
  geom_vline(xintercept = 0, linetype = "dashed") &
  geom_pointrange(
    aes(
      fill = sig,
      color = sig,
      x = estimate, 
      xmin = estimate-1.96*std.error, 
      xmax = estimate+1.96*std.error
    ),
    shape = 21,
    size = .4
  ) &
  scale_fill_manual(values = c("white", "black")) &
  scale_color_manual(values = c("gray40", "black")) &
  scale_x_continuous(breaks = seq(0, .4, by = .2)) &
  facet_wrap(term~., scales = "free_y", ncol = 1) &
  xlab("") &
  theme(legend.position = "none")

ggsave("plots/world_res.png")

