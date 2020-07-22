install.packages('tidyverse')
library(tidyverse)
#install remotes
install.packages("remotes")
library(remotes)

#install data
remotes::install_github("allisonhorst/palmerpenguins")
library(palmerpenguins)
penguins

library(tidyverse)
glimpse(penguins)

#exploring import
unique(penguins$species)
unique(penguins$island)

penguins %>%
  count(species)
#data viz scatter sizes x species
ggplot(data = penguins, 
       aes(x = flipper_length_mm,
           y = body_mass_g)) + 
  geom_point(aes(color = species, 
                 shape = species),
             size = 3,
             alpha = 0.8) +
  #theme_minimal() +
  scale_color_manual(values = c("black","pink","grey")) +
  labs(title = "Penguin size, Palmer Station LTER",
       subtitle = "Flipper length and body mass for each island",
       x = "Flipper length (mm)",
       y = "Body mass (g)",
       color = "Penguin species",
       shape = "Penguin species") +
  theme_minimal()

#data viz scatter sizes x island 
ggplot(data = penguins,
       aes(x = flipper_length_mm, 
           y = body_mass_g)) +
  geom_point(aes(color = island, 
                 shape = species), 
             size = 3,
             alpha = 0.8) +
  #theme_minimal() +
  scale_color_manual(values = c("black","pink","grey")) +
  labs(title = "Penguin size, Palmer Station LTER",
       subtitle = "Flipper length and body mass for each island",
       x = "Flipper length (mm)",
       y = "Body mass (g)",
       color = "Penguin island",
       shape = "Penguin species") +
  theme_minimal() 

#Variable class
class(penguins$sex)
class(penguins$body_mass_g)
class(penguins$species)
class(penguins$island)
class(penguins$flipper_length_mm)
class(penguins$bill_length_mm)
class(penguins$bill_depth_mm)

#Variable levels
levels(penguins$sex)
levels(penguins$body_mass_g)
levels(penguins$species)
levels(penguins$island)
levels(penguins$flipper_length_mm)
levels(penguins$bill_length_mm)
levels(penguins$bill_depth_mm)

#Missing data 
is.na(penguins)
is.na(penguins$flipper_length_mm)
is.na(penguins$sex)

#Analysis with NA value
penuins %>%
  group_by(island) %>%
  summarise(mean(bill_length_mm))

#NA count
penguins %>%
  #group_by(species) %>%
  select(everything()) %>%
  summarise_all(funs(sum(is.na(.)))) %>%
  pivot_longer(cols = 1:7, names_to = 'columns', values_to = 'NA_count') %>%
  arrange(desc(NA_count)) %>%
  ggplot(aes(y = columns, x = NA_count)) + geom_col(fill = 'light pink') +
  geom_label(aes(label = NA_count)) +
  #   scale_fill_manual(values = c("blue","purple","grey")) +
  theme_minimal() +
  labs(title = 'NA Count for the Palmer Penguins!')

penguins %>%
  drop_na() %>%
  count(sex, species) %>%
  ggplot() + geom_col(aes(x = species, y = n, fill = species)) +
  geom_label(aes(x = species, y = n, label = n)) +
  scale_fill_manual(values = c("#099E73","#CC79A7","gray")) +
  facet_wrap(~sex) +
  theme_minimal() +
  labs(title = 'Penguins Specified by Gender')

# Bar graph counts > colorblind palettes
penguins %>%
  count(species) %>%
  ggplot() + geom_col(aes(x = species, y = n, fill = species)) + 
  geom_label(aes(x = species, y = n, label = n)) + 
  scale_fill_manual(values = c("grey","light blue","pink")) +
  theme_minimal() + 
  labs (title = 'Penguin Species Frequency')

summary(penguins)
summary(penguins$sex)
summary(penguins$body_mass_g)




