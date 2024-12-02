

#loading packages
library(tidyverse)
library(ggeffects)
library(patchwork)


#using ggpeffects::ggpredict to get predicted science attitude levels
#binding these together
#see first call for explanation
#binding together predictions for each data set
#first eurobarometer
preds_eu <- bind_rows(
  
  ggpredict(
    #first argument is regression
    lm(data = cleanbarom, res~gender+age+pol+rel+edu+class+scilit+scieng+lifesat+country),
    #second indicates winch variable i want predicted levels for
    #manually selecting germany for country
    #all other varlbles held at mean or mode
    terms = c("lifesat", "country [Germany]")
    #turning into tibble and indicating model
  ) %>% as_tibble() %>% mutate(model = "S_Reservations"),
  ggpredict(
    lm(data = cleanbarom, prom~gender+age+pol+rel+edu+class+scilit+scieng+lifesat+country),
    terms = c("lifesat", "country [Germany]")
  ) %>% as_tibble() %>% mutate(model = "S_Promise"),
  ggpredict(
    lm(data = cleanbarom, res~gender+age+pol+rel+edu+class+scilit+scieng+lifedir+country),
    terms = c("lifedir", "country [Germany]")
  ) %>% as_tibble() %>% mutate(model = "D_Reservations"),
  ggpredict(
    lm(data = cleanbarom, prom~gender+age+pol+rel+edu+class+scilit+scieng+lifedir+country),
    terms = c("lifedir", "country [Germany]")
  ) %>% as_tibble() %>% mutate(model = "D_Promise")
)

#then wvs
preds_wvs <- bind_rows(
  
  ggpredict(
    lm(data = cleanwvs, res~sex+age+pol+rel+edu+class+income+lifesat+country),
    terms = c("lifesat[-1:1 by=.1]", "country [Germany]")
  ) %>% as_tibble() %>% mutate(model = "S_Reservations"),
  ggpredict(
    lm(data = cleanwvs, prom~sex+age+pol+rel+edu+class+income+lifesat+country),
    terms = c("lifesat[-1:1 by=.1]", "country [Germany]")
  ) %>% as_tibble() %>% mutate(model = "S_Promise"),
  ggpredict(
    lm(data = cleanwvs, res~sex+age+pol+rel+edu+class+income+lifecontrol+country),
    terms = c("lifecontrol[-1:1 by=.1]", "country [Germany]")
  ) %>% as_tibble() %>% mutate(model = "C_Reservations"),
  ggpredict(
    lm(data = cleanwvs, prom~sex+age+pol+rel+edu+class+income+lifecontrol+country),
    terms = c("lifecontrol[-1:1 by=.1]", "country [Germany]")
  ) %>% as_tibble() %>% mutate(model = "C_Promise")
)

#figure

#recoding data, so lowest predicted value equals 0, for ease of reading
preds_eu %>% 
  mutate(
    conf.low = conf.low-min(predicted), 
    conf.high = conf.high-min(predicted),
    predicted = predicted-min(predicted),
    .by = model
  ) %>% 
  #separating model info
  separate(model, c("pred", "outc"), "_")  %>% 
  #renaming life direction labels
  mutate(
    x = case_when(
      x == "Things are going in the wrong direction" ~ "Wrong direction",
      x == "Neither the one nor the other (SPONTANEOUS)" ~ "Neither/nor",
      x == "Things are going in the right direction" ~ "Right direction",
      .default = x
    )
  ) %>% 
  #fixing order of variables
  mutate(
    pred = case_when(
      pred == "S" ~ "Life\nsatisfaction",
      pred == "D" ~ "Direction\nof life"
    )
  ) %>% 
  mutate(x = fct_inorder(x), pred = fct_inorder(pred)) %>% 
  #plotting
  ggplot(aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high)) +
  geom_pointrange(size = .4) +
  geom_line(aes(group = 1), linetype = "dashed") +
  facet_grid(outc~pred, scales = "free_x") +
  scale_y_continuous(breaks = seq(0, .5, by = .2)) +
  jtools::theme_nice() +
  xlab("") +
  ylab("Eurobarometer") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  
  preds_wvs %>% 
  #recoding data, to center on 0, for ease of reading
  mutate(
    conf.low = conf.low-mean(predicted), 
    conf.high = conf.high-mean(predicted),
    predicted = predicted-mean(predicted),
    .by = model
  ) %>% 
  #separating model info
  separate(model, c("pred", "outc"), "_")  %>% 
  #fixing order of variables
  mutate(
    pred = case_when(
      pred == "S" ~ "Life\nsatisfaction",
      pred == "C" ~ "Control\nover life"
    )
  ) %>% 
  mutate(pred = fct_inorder(pred)) %>% 
  #plotting
  ggplot(aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high)) +
  geom_ribbon(fill = "gray80") +
  geom_smooth(color = "black") +
  facet_grid(outc~pred, scales = "free_x") +
  scale_x_continuous(breaks = seq(-1, 1, by = 1)) +
  scale_y_continuous(breaks = seq(-.2, .2, by = .2)) +
  jtools::theme_nice() +
  xlab("") +
  ylab("World Values Survey") +
  
  plot_layout(nrow=1) &
  theme(panel.spacing = unit(1, "lines"))

ggsave("plots/fig2.pdf", dpi = 350, units = "cm", width = 16, height = 11)


