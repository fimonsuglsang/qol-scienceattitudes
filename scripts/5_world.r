

#loading packages
library(tidyverse)
library(patchwork)


#making empty tibble for loop
cpreds_wvs <- tibble()

#running regressions for each country
for(i in c(1:50)){
  
  #getting couintries with complete data
  temp <- filter(cleanwvs, !is.na(pol), !is.na(class), !is.na(prom), !is.na(country))
  #picking single country
  temp <- filter(temp, country == unique(temp$country)[i])
  
  #standardizing outcomes within country
  temp <- mutate(temp, 
                 lifesat = (lifesat-mean(lifesat, na.rm = T))/sd(lifesat, na.rm = T),
                 lifecontrol = (lifecontrol-mean(lifecontrol, na.rm = T))/sd(lifecontrol, na.rm = T)
  )
  
  #update message for console
  print(paste(i,"of 50:", unique(temp$country)))
  
  #running regression and extracting info on relevant variable
  cpreds_wvs <- bind_rows(
    cpreds_wvs,
    lm(data = temp, prom~sex+age+pol+rel+edu+class+income+lifesat) %>% 
      broom::tidy() %>% 
      filter(term == "lifesat") %>%
      mutate(country = unique(temp$country)),
    
    lm(data = temp, prom~sex+age+pol+rel+edu+class+income+lifecontrol) %>% 
      broom::tidy() %>% 
      filter(term == "lifecontrol") %>%
      mutate(country = unique(temp$country))
  )
}

#making empty tibble for loop
cpreds_eu <- tibble()

#running regressions for each country
for(i in c(1:38)){
  
  #picking single country
  temp <- filter(cleanbarom, country == unique(cleanbarom$country)[i])
  
  #making ordinals continuous and standardizing
  temp <- mutate(temp, 
                 lifesat = (as.numeric(lifesat)-mean(as.numeric(lifesat), na.rm = T))/sd(as.numeric(lifesat), na.rm = T),
                 lifedir = (as.numeric(lifedir)-mean(as.numeric(lifedir), na.rm = T))/sd(as.numeric(lifedir), na.rm = T)
  )
  
  #update message for console
  print(paste(i,"of 38:", unique(cleanbarom$country)[i]))
  
  
  cpreds_eu <- bind_rows(
    cpreds_eu,
    lm(data = temp, prom~gender+age+pol+rel+edu+class+scilit+scieng+lifesat) %>% 
      broom::tidy() %>% 
      filter(term == "lifesat") %>%
      mutate(country = unique(temp$country)),
    
    lm(data = temp, prom~gender+age+pol+rel+edu+class+scilit+scieng+lifedir) %>% 
      broom::tidy() %>% 
      filter(term == "lifedir") %>%
      mutate(country = unique(temp$country))
  )
}


#figure

#recoding data for visualization
cpreds_wvs %>%
  mutate(
    sig = case_when(p.value<.05 ~ T, .default = F),
    term = case_when(
      term == "lifesat" ~ "Satisfaction",
      term == "lifecontrol" ~ "Control"
    ),
    term = fct_inorder(term)
  ) %>% 
  #plotting data
  ggplot(aes(y = tidytext::reorder_within(country, estimate, term))) +
  tidytext::scale_y_reordered() +
  jtools::theme_nice() +
  theme(axis.text.y = element_text(hjust = 1)) +
  ggtitle("WVS") +
  
  #recoding data for visualization
  cpreds_eu %>%
  mutate(
    sig = case_when(p.value<.05 ~ T, .default = F),
    term = case_when(
      term == "lifesat" ~ "Satisfaction",
      term == "lifedir" ~ "Direction"
    ),
    term = fct_inorder(term)
  ) %>% 
  #plotting data
  ggplot(aes(y = tidytext::reorder_within(country, estimate, term))) +
  tidytext::scale_y_reordered(position = "right") +
  jtools::theme_nice() +
  ggtitle("EU") +
  
  plot_layout(ncol=2) &
  ylab("") &
  coord_cartesian(xlim = c(-.05, .4)) &
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

ggsave("plots/world.png")

