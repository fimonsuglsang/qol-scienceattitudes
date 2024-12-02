

#loading packages
library(tidyverse)
library(patchwork)


#making empty tibble for loop
cpreds <- tibble()

#running regressions for each country (WVS)
for(i in c(1:50)){
  
  #getting couintries with complete data
  temp <- filter(cleanwvs, !is.na(pol), !is.na(class), !is.na(prom), !is.na(country))
  #picking single country
  temp <- filter(temp, country == unique(temp$country)[i])
  
  #standardizing outcomes within country
  temp <- mutate(temp, 
                 lifesat = (lifesat-mean(lifesat, na.rm = T))/sd(lifesat, na.rm = T),
                 lifecontrol = (lifecontrol-mean(lifecontrol, na.rm = T))/sd(lifecontrol, na.rm = T),
                 prom = (prom-mean(prom, na.rm = T))/sd(prom, na.rm = T),
                 res = (res-mean(res, na.rm = T))/sd(res, na.rm = T)
  )
  
  #update message for console
  print(paste(i,"of 50:", unique(temp$country)))
  
  #running regression and extracting info on relevant variable
  cpreds <- bind_rows(
    cpreds,
    lm(data = temp, prom~sex+age+pol+rel+edu+class+income+lifesat) %>% 
      broom::tidy() %>% 
      filter(term == "lifesat") %>%
      mutate(outc = "promise", set = "wvs") %>% 
      mutate(country = unique(temp$country)),
    lm(data = temp, res~sex+age+pol+rel+edu+class+income+lifesat) %>% 
      broom::tidy() %>% 
      filter(term == "lifesat") %>%
      mutate(outc = "reservation", set = "wvs") %>% 
      mutate(country = unique(temp$country)),
    
    lm(data = temp, prom~sex+age+pol+rel+edu+class+income+lifecontrol) %>% 
      broom::tidy() %>% 
      filter(term == "lifecontrol") %>%
      mutate(outc = "promise", set = "wvs") %>% 
      mutate(country = unique(temp$country)),
    lm(data = temp, res~sex+age+pol+rel+edu+class+income+lifecontrol) %>% 
      broom::tidy() %>% 
      filter(term == "lifecontrol") %>%
      mutate(outc = "reservation", set = "wvs") %>% 
      mutate(country = unique(temp$country))
  )
}


#running regressions for each country (EUROBAROMETER)
for(i in c(1:37)){
  
  #picking single country
  temp <- filter(cleanbarom, country == unique(cleanbarom$country)[i])
  
  #making ordinals continuous and standardizing
  temp <- mutate(temp, 
                 lifesat = (as.numeric(lifesat)-mean(as.numeric(lifesat), na.rm = T))/sd(as.numeric(lifesat), na.rm = T),
                 lifedir = (as.numeric(lifedir)-mean(as.numeric(lifedir), na.rm = T))/sd(as.numeric(lifedir), na.rm = T),
                 prom = (prom-mean(prom, na.rm = T))/sd(prom, na.rm = T),
                 res = (res-mean(res, na.rm = T))/sd(res, na.rm = T)
  )
  
  #update message for console
  print(paste(i,"of 37:", unique(cleanbarom$country)[i]))
  
  cpreds <- bind_rows(
    cpreds,
    lm(data = temp, prom~gender+age+pol+rel+edu+class+scilit+scieng+lifesat) %>% 
      broom::tidy() %>% 
      filter(term == "lifesat") %>%
      mutate(outc = "promise", set = "eu") %>% 
      mutate(country = unique(temp$country)),
    lm(data = temp, res~gender+age+pol+rel+edu+class+scilit+scieng+lifesat) %>% 
      broom::tidy() %>% 
      filter(term == "lifesat") %>%
      mutate(outc = "reservation", set = "eu") %>% 
      mutate(country = unique(temp$country)),
    
    lm(data = temp, prom~gender+age+pol+rel+edu+class+scilit+scieng+lifedir) %>% 
      broom::tidy() %>% 
      filter(term == "lifedir") %>%
      mutate(outc = "promise", set = "eu") %>% 
      mutate(country = unique(temp$country)),
    lm(data = temp, res~gender+age+pol+rel+edu+class+scilit+scieng+lifedir) %>% 
      broom::tidy() %>% 
      filter(term == "lifedir") %>%
      mutate(outc = "reservation", set = "eu") %>% 
      mutate(country = unique(temp$country))
  )
}

#joining the two datasets
left_join(
  relationship = "many-to-many",
  cpreds %>% 
    mutate(country = case_when(
      country == "Macedonia" ~ "North Macedonia",
      country == "Turkey" ~ "Türkiye",
      country == "Czech Republic" ~ "Czechia",
      country == "Luxemburg" ~ "Luxembourg",
      .default = country
    )),
  cinfo) %>% 
  mutate(
    sig = case_when(p.value<.05 ~ T, .default = F),
    term = case_when(
      term == "lifesat" ~ "Life\nsatisfaction",
      term == "lifecontrol" ~ "Control\nover life",
      term == "lifedir" ~ "Direction\nof life"
    ),
    term = fct_inorder(term),
    outc = str_to_sentence(outc)
  ) -> cpreds


# figures

ggplot(
  cpreds %>% 
    filter(set == "eu") %>% 
    mutate(country = fct_reorder(country, estimate, max)) %>% 
    mutate(subregion = str_replace(subregion, "Western Asia", "Asia")) %>% 
    mutate(subregion = str_replace(subregion, " ", "\n")) %>% 
    mutate(subregion = fct_rev(subregion))
) + 
  
  aes(
    x = country,
    y = estimate, 
    ymin = estimate-1.96*std.error, 
    ymax = estimate+1.96*std.error,
    fill = term,
    alpha = sig
  ) +
  geom_hline(yintercept = 0, linetype = "longdash", alpha = .3) +
  geom_pointrange(shape = 21, position = position_dodge(.6), size = .4) +
  facet_grid(outc~subregion, scales = "free", space = "free") +
  
  scale_fill_manual(values = c("black", "white")) +
  scale_alpha_manual(values = c(.3, 1), guide = "none") +
  scale_y_continuous(breaks = seq(0, .4, by = .2)) +
  
  jtools::theme_nice() +
  theme(
    axis.text.x = element_text(hjust = 1, angle = 90),
    plot.title = element_text(hjust = .5),
    legend.position = "top",
    legend.title = element_blank(),
    panel.spacing.x = unit(.5, "cm")
  ) +
  
  xlab("") +
  ylab("Estimate")

ggsave("plots/fig4.pdf", dpi = 350, units = "cm", width = 18, height = 13)


ggplot(
  cpreds %>% 
    filter(set == "wvs") %>% 
    mutate(country = fct_reorder(country, estimate, max)) %>% 
    mutate(region = str_replace(region, "Oceania", "OC")) 
) + 
  
  
  aes(
    x = country,
    y = estimate, 
    ymin = estimate-1.96*std.error, 
    ymax = estimate+1.96*std.error,
    fill = term,
    alpha = sig
  ) +
  geom_hline(yintercept = 0, linetype = "longdash", alpha = .3) +
  geom_pointrange(shape = 21, position = position_dodge(.6), size = .4) +
  facet_grid(outc~region, scales = "free", space = "free") +
  
  scale_fill_manual(values = c("black", "white")) +
  scale_alpha_manual(values = c(.3, 1), guide = "none") +
  scale_y_continuous(breaks = seq(0, .4, by = .2)) +
  
  jtools::theme_nice() +
  theme(
    axis.text.x = element_text(hjust = 1, angle = 90),
    plot.title = element_text(hjust = .5),
    legend.position = "top",
    legend.title = element_blank(),
    panel.spacing.x = unit(0.3, "cm")
  ) +
  
  xlab("") +
  ylab("Estimate")

ggsave("plots/fig5.pdf", dpi = 350, units = "cm", width = 18, height = 13)


