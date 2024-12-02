

#loading packages
library(tidyverse)
library(patchwork)

#Eurobarometer
#turning ordinals into continuous variables, and standardizing
temp <- mutate(
  cleanbarom, 
  across(c(lifesat, lifedir, matches("QA10_\\d"), matches("QA11_\\d")),
         \(x) (as.numeric(x)-mean(as.numeric(x), na.rm = T))/sd(as.numeric(x), na.rm = T)
  ))

#running single outcome regressions, annotating these, and binding them together
bind_rows(
  
  broom::tidy(lm(data = temp, QA10_1n2~gender+age+pol+rel+edu+class+scilit+scieng+lifesat+country)) %>% 
    mutate(var = "better lives"),
  broom::tidy(lm(data = temp, QA10_3~gender+age+pol+rel+edu+class+scilit+scieng+lifesat+country)) %>% 
    mutate(var = "informed citizens"),
  broom::tidy(lm(data = temp, QA10_4~gender+age+pol+rel+edu+class+scilit+scieng+lifesat+country)) %>% 
    mutate(var = "natural resources"),
  broom::tidy(lm(data = temp, QA10_5~gender+age+pol+rel+edu+class+scilit+scieng+lifesat+country)) %>% 
    mutate(var = "opportunities"),
  broom::tidy(lm(data = temp, QA10_6~gender+age+pol+rel+edu+class+scilit+scieng+lifesat+country)) %>% 
    mutate(var = "AI = jobs"),
  broom::tidy(lm(data = temp, QA10_7~gender+age+pol+rel+edu+class+scilit+scieng+lifesat+country)) %>% 
    mutate(var = "science v. faith"),
  broom::tidy(lm(data = temp, QA10_8~gender+age+pol+rel+edu+class+scilit+scieng+lifesat+country)) %>% 
    mutate(var = "human rights"),
  broom::tidy(lm(data = temp, QA10_9~gender+age+pol+rel+edu+class+scilit+scieng+lifesat+country)) %>% 
    mutate(var = "life change too fast"),
  broom::tidy(lm(data = temp, QA10_10~gender+age+pol+rel+edu+class+scilit+scieng+lifesat+country)) %>% 
    mutate(var = "dangerous knowledge"),
  broom::tidy(lm(data = temp, QA11_1~gender+age+pol+rel+edu+class+scilit+scieng+lifesat+country)) %>% 
    mutate(var = "special interests"),
  broom::tidy(lm(data = temp, QA11_2~gender+age+pol+rel+edu+class+scilit+scieng+lifesat+country)) %>% 
    mutate(var = "specific knowledge"),
  broom::tidy(lm(data = temp, QA11_3~gender+age+pol+rel+edu+class+scilit+scieng+lifesat+country)) %>% 
    mutate(var = "too complex"),
  broom::tidy(lm(data = temp, QA11_4n5~gender+age+pol+rel+edu+class+scilit+scieng+lifesat+country)) %>% 
    mutate(var = "intervene in politics"),
  broom::tidy(lm(data = temp, QA11_6~gender+age+pol+rel+edu+class+scilit+scieng+lifesat+country)) %>% 
    mutate(var = "misuse by others"),
  
  broom::tidy(lm(data = temp, QA10_1n2~gender+age+pol+rel+edu+class+scilit+scieng+lifedir+country)) %>% 
    mutate(var = "better lives"),
  broom::tidy(lm(data = temp, QA10_3~gender+age+pol+rel+edu+class+scilit+scieng+lifedir+country)) %>% 
    mutate(var = "informed citizens"),
  broom::tidy(lm(data = temp, QA10_4~gender+age+pol+rel+edu+class+scilit+scieng+lifedir+country)) %>% 
    mutate(var = "natural resources"),
  broom::tidy(lm(data = temp, QA10_5~gender+age+pol+rel+edu+class+scilit+scieng+lifedir+country)) %>% 
    mutate(var = "opportunities"),
  broom::tidy(lm(data = temp, QA10_6~gender+age+pol+rel+edu+class+scilit+scieng+lifedir+country)) %>% 
    mutate(var = "AI = jobs"),
  broom::tidy(lm(data = temp, QA10_7~gender+age+pol+rel+edu+class+scilit+scieng+lifedir+country)) %>% 
    mutate(var = "science v. faith"),
  broom::tidy(lm(data = temp, QA10_8~gender+age+pol+rel+edu+class+scilit+scieng+lifedir+country)) %>% 
    mutate(var = "human rights"),
  broom::tidy(lm(data = temp, QA10_9~gender+age+pol+rel+edu+class+scilit+scieng+lifedir+country)) %>% 
    mutate(var = "life change too fast"),
  broom::tidy(lm(data = temp, QA10_10~gender+age+pol+rel+edu+class+scilit+scieng+lifedir+country)) %>% 
    mutate(var = "dangerous knowledge"),
  broom::tidy(lm(data = temp, QA11_1~gender+age+pol+rel+edu+class+scilit+scieng+lifedir+country)) %>% 
    mutate(var = "special interests"),
  broom::tidy(lm(data = temp, QA11_2~gender+age+pol+rel+edu+class+scilit+scieng+lifedir+country)) %>% 
    mutate(var = "specific knowledge"),
  broom::tidy(lm(data = temp, QA11_3~gender+age+pol+rel+edu+class+scilit+scieng+lifedir+country)) %>% 
    mutate(var = "too complex"),
  broom::tidy(lm(data = temp, QA11_4n5~gender+age+pol+rel+edu+class+scilit+scieng+lifedir+country)) %>% 
    mutate(var = "intervene in politics"),
  broom::tidy(lm(data = temp, QA11_6~gender+age+pol+rel+edu+class+scilit+scieng+lifedir+country)) %>% 
    mutate(var = "misuse by others")

) -> sitem_eu

#WVS
#standardizing variables
temp <- mutate(
  cleanwvs, 
  across(c(lifesat, lifecontrol, Q158:Q163),
         \(x) (as.numeric(x)-mean(as.numeric(x), na.rm = T))/sd(as.numeric(x), na.rm = T)
  ))

#running single outcome regressions, annotating these, and binding them together
bind_rows(
  
  broom::tidy(lm(data = temp, Q158~sex+age+pol+rel+edu+class+income+lifesat+country)) %>% 
    mutate(var = "better lives"),
  broom::tidy(lm(data = temp, Q159~sex+age+pol+rel+edu+class+income+lifesat+country)) %>% 
    mutate(var = "opportunities"),
  broom::tidy(lm(data = temp, Q160~sex+age+pol+rel+edu+class+income+lifesat+country)) %>% 
    mutate(var = "science v. faith"),
  broom::tidy(lm(data = temp, Q161~sex+age+pol+rel+edu+class+income+lifesat+country)) %>% 
    mutate(var = "blur right/wrong"),
  broom::tidy(lm(data = temp, Q162~sex+age+pol+rel+edu+class+income+lifesat+country)) %>% 
    mutate(var = "daily life"),
  broom::tidy(lm(data = temp, Q163~sex+age+pol+rel+edu+class+income+lifesat+country)) %>% 
    mutate(var = "world better off"),
  
  broom::tidy(lm(data = temp, Q158~sex+age+pol+rel+edu+class+income+lifecontrol+country)) %>% 
    mutate(var = "better lives"),
  broom::tidy(lm(data = temp, Q159~sex+age+pol+rel+edu+class+income+lifecontrol+country)) %>% 
    mutate(var = "opportunities"),
  broom::tidy(lm(data = temp, Q160~sex+age+pol+rel+edu+class+income+lifecontrol+country)) %>% 
    mutate(var = "science v. faith"),
  broom::tidy(lm(data = temp, Q161~sex+age+pol+rel+edu+class+income+lifecontrol+country)) %>% 
    mutate(var = "blur right/wrong"),
  broom::tidy(lm(data = temp, Q162~sex+age+pol+rel+edu+class+income+lifecontrol+country)) %>% 
    mutate(var = "daily life"),
  broom::tidy(lm(data = temp, Q163~sex+age+pol+rel+edu+class+income+lifecontrol+country)) %>% 
    mutate(var = "world better off")
  
) -> sitem_wvs


#figure

#selecting variables of interest and renaming these
sitem_eu %>% 
  filter(term == "lifesat" | term == "lifedir") %>% 
  mutate(
    term = case_when(
      term == "lifesat" ~ "Life\nsatisfaction",
      term == "lifedir" ~ "Direction\nof life",
    )
  ) %>% 
  #fixing order
  mutate(
    term = fct_inorder(term),
    var = fct_reorder(var, estimate, mean)
  ) %>%   
  
  #plotting (adding parameters for patched plot below)
  ggplot() +
  ggtitle("Eurobarometer") +
  coord_cartesian(ylim = c(-.01,.12)) +
  
#selecting variables of interest and renaming these  
sitem_wvs %>% 
  filter(term == "lifesat" | term == "lifecontrol") %>% 
  #fixing order
  mutate(
    term = case_when(
      term == "lifesat" ~ "Life\nsatisfaction",
      term == "lifecontrol" ~ "Control\nover life",
    )
  ) %>% 
  mutate(
    term = fct_inorder(term),
    var = fct_reorder(var, estimate, mean)
  ) %>%   
  
  #plotting (adding parameters for patched plot below)
  ggplot() +
  scale_y_continuous(position = "right", breaks = c(0, .06, .12, .18)) +
  ggtitle("World Values Survey") +
  coord_cartesian(ylim = c(-.015,.18)) +
  
  #plot layout parameters
  plot_layout(widths = c(2, 1)) &
  geom_hline(yintercept = 0, linetype = "dashed") &
  geom_pointrange(
    aes(
      y = estimate, 
      ymin = estimate-1.96*std.error, 
      ymax = estimate+1.96*std.error, 
      x = var, 
      fill = term
    ),
    position = position_dodge(.4), 
    shape = 21
  ) &
  jtools::theme_nice(legend.pos = "topleft") &
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    legend.title = element_blank(),
    legend.position.inside = c(0,1),
    plot.title = element_text(hjust = 0.5)
  ) &
  xlab("") &
  ylab("") &
  scale_fill_manual(values = c("black", "white"))
  
ggsave("plots/fig3.pdf", dpi = 350, units = "cm", width = 18, height = 11)


