

#loading packages
library(tidyverse)
library(patchwork)

#extracting factor loadings
fa(
  cor(
    select(
      #renaming variables
      cleanbarom,
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
  #getting scores as tibble
  loadings() %>% 
  unclass() %>% 
  as_tibble(rownames = "var") %>% 
  
  #plotting positions of indicators in two dimensional space
  ggplot(
    aes(group = var, x = MR1, y = MR2)
  ) +
  geom_hline(yintercept = 0, linetype = "longdash") +
  geom_vline(xintercept = 0, linetype = "longdash") +
  geom_point() +
  ggrepel::geom_label_repel(
    aes(label = var), 
    seed = "123", 
    force = 10,
    min.segment.length = 0
  ) +
  jtools::theme_nice() +
  coord_cartesian(xlim = c(-.22,.8), ylim = c(-.2,.8)) +
  ggtitle("Eurobarometer") +
  ylab("Promise") +
  xlab("Reservations") +
  theme(plot.title = element_text(hjust = .5)) +


#extracting factor loadings  
fa(
  cor(
    select(
      cleanwvs,
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
  #getting scores as tibble
  loadings() %>% 
  unclass() %>% 
  as_tibble(rownames = "var") %>% 
  
  #plotting positions of indicators in two dimensional space
  ggplot(
    aes(group = var, x = MR2, y = MR1)
  ) +
  geom_hline(yintercept = 0, linetype = "longdash") +
  geom_vline(xintercept = 0, linetype = "longdash") +
  geom_point() +
  ggrepel::geom_label_repel(
    aes(label = var), 
    seed = "123",
    force = 10,
    min.segment.length = 0
  ) +
  jtools::theme_nice() +
  coord_cartesian(xlim = c(-.22,.8), ylim = c(-.2,.8)) +
  scale_y_continuous(position = "right") +
  ggtitle("World Values Survey") +
  ylab("Promise") +
  xlab("Reservations") +
  theme(plot.title = element_text(hjust = .5))
  
ggsave("plots/fig1.pdf", dpi = 350, units = "cm", width = 22, height = 12)
  







