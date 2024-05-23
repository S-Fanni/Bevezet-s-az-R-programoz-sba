# Itt vannak a cuccok:
setwd('C:/BME/2_felev/kog_labor/R_labgyak/jelentés/')

# a fuggvenyek, amiket hasznalok, ebben a csomagban vannak:
library(tidyverse)
library(dplyr)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)


# -- beolvasas -- #

# betöltom a Binford Hunter-Gatherer adathalmaz különböző részeit a dplace-ről:

# a konkrét adatok: milyen kulturális közösség / törzs milyen típusú tulajdonsága milyen érteket vesz föl
BIN_data = read_csv('https://raw.githubusercontent.com/D-PLACE/dplace-data/master/datasets/Binford/data.csv')

# a kulturális közösség / törzs részletes adatai
BIN_societies = read_csv('https://raw.githubusercontent.com/D-PLACE/dplace-data/master/datasets/Binford/societies.csv')

# a tulajdonsagok részletes ismertetői
BIN_variables = read_csv('https://raw.githubusercontent.com/D-PLACE/dplace-data/master/datasets/Binford/variables.csv')

# a tulajdonsagok értekeinek a részletes ismertetői
BIN_codes = read_csv('https://raw.githubusercontent.com/D-PLACE/dplace-data/master/datasets/Binford/codes.csv')

#betöltom a EA adathalmaz különböző részeit a dplace-ről:
EA_data = read.csv('https://raw.githubusercontent.com/D-PLACE/dplace-data/master/datasets/EA/data.csv')

EA_societies = read.csv('https://raw.githubusercontent.com/D-PLACE/dplace-data/master/datasets/EA/societies.csv')

EA_variables = read.csv('https://raw.githubusercontent.com/D-PLACE/dplace-data/master/datasets/EA/variables.csv')

EA_codes = read.csv('https://raw.githubusercontent.com/D-PLACE/dplace-data/master/datasets/EA/codes.csv')

# betöltom az ecoClimate adathalmaz különböző részeit a dplace-ről
eco_data = read_csv('https://raw.githubusercontent.com/D-PLACE/dplace-data/master/datasets/ecoClimate/data.csv')

# ecoCLimate tulajdonsagok részletes ismertetői
eco_var = read_csv('https://raw.githubusercontent.com/D-PLACE/dplace-data/master/datasets/ecoClimate/variables.csv')

# ki hol lakik
location = read_csv('https://raw.githubusercontent.com/D-PLACE/dplace-data/master/legacy/society_locations.csv')

# -- oszlopokra szűrés -- #
#Binford data szükséges adatok
BIN_data = BIN_data %>% 
  select(soc_id,var_id,code)

#Binford soc. szükséges
BIN_societies = BIN_societies %>% 
  select(id,pref_name_for_society,glottocode,Lat,Long)

#Binford var. szükséges
BIN_variables = BIN_variables %>% 
  select(id,category,title,definition) 

#EA szükséges 
EA_data = EA_data %>% 
  select(soc_id,var_id,code)

EA_societies = EA_societies %>% 
  select(id,pref_name_for_society,glottocode,Lat,Long)

EA_variables = EA_variables %>% 
  select(id,category,title,definition)

#ecoClimate adathalmaz szükséges
eco_data = eco_data %>% 
  select(soc_id,var_id,code)

#ecoCLimate tulajdonságok szükséges
eco_var = eco_var %>% 
  select(id,category,title,definition)

#loc szükséges
location = location %>% 
  select(soc_id,region)

# -- oszlopok átnevezese -- #
# Binford
BIN_societies = BIN_societies %>% 
  rename(soc_id = id)

BIN_variables = BIN_variables %>% 
  rename(var_id = id)

#EA 
EA_societies = EA_societies %>% 
  rename(soc_id = id)

EA_variables = EA_variables %>% 
  rename(var_id = id)

EA_variables = EA_variables %>% 
  rename(ea_def = definition)

EA_codes = EA_codes %>% 
  rename(ea_desc = description)

#ecoClimate
eco_var = eco_var %>% 
  rename(var_id = id)

# -- Összegyúrás nagy táblázatba -- #
# Binford data es változók adatai:
BIN_long = left_join(BIN_data,BIN_variables)

BIN_long = left_join(BIN_long,BIN_societies)

BIN_long = left_join(BIN_long,BIN_codes)

BIN_long = left_join(BIN_long,location)

#EA data és változók
EA_long = left_join(EA_data,EA_variables)

EA_long = left_join(EA_long,EA_societies)

EA_long = left_join(EA_long,EA_codes)

#ecoClimate data és változók
Eco_long = left_join(eco_data,eco_var)

#Átnevezgetés, hogy ne keveredjenek össze, meg ne hagyja ki az azonos nevűeket, amikor összekapcsolja
EA_long$ea_id <- EA_long$var_id

EA_long$ea_code <- EA_long$code

EA_long = EA_long %>% 
  select(ea_code, ea_id, glottocode,ea_def,ea_desc)

Eco_long$eco_id <- Eco_long$var_id

Eco_long$eco_code <- Eco_long$code

Eco_long = Eco_long %>% 
  select(soc_id, eco_id, eco_code)


#minden egyben
all_long = left_join(BIN_long, EA_long)

all_long = left_join(all_long,Eco_long)


#EA001	Subsistence, Economy	Subsistence economy: gathering	Dependence on the gathering of wild plants and small land fauna, relative to other subsistence activities.
#EA005	Subsistence, Economy	Subsistence economy: agriculture	Dependence on agriculture, relative to other subsistence activities.
#TemperaturePredictability	Climate	Temperature predictability	Colwell's (1974) information theoretic index. Indicates the extent to which a climate patterns are predictable due to either constancy or contingency. Varies between 0 (completely unpredictable) and 1 (fully predictable).

gatherers = all_long %>% 
  filter(
        ea_code > 0,
         ea_id == c("EA001"),
         eco_id == "TemperaturePredictability")

agri = all_long %>% 
  filter(
    ea_code > 0,
    ea_id == c("EA005"),
    eco_id == "TemperaturePredictability")


#hisztogram arról h melyik kategóriában hány társadalom van GYÜJTÖGETŐK
gatherers$ea_code <- as.character(gatherers$ea_code) #átalakítom karakterré
gatherers %>% 
  ggplot(aes(x = ea_code)) +
  geom_bar(stat = "count") +
  theme_classic() +
  labs(x = "Gyűjtögetés mértéke", y = "Társadalmak száma") + # tengelyen lévő feliratok
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5, size = 5) #bin-ben lévő elemek száma

#hisztogram arról h melyik kategóriában hány társadalom van MEZŐGAZDASÁG
agri$ea_code <- as.character(agri$ea_code) #átalakítom karakterré
agri %>% 
  ggplot(aes(x = ea_code)) +
  geom_bar(stat = "count") +
  theme_classic() +
  labs(x = "Mezőgazdasági tevékenység mértéke", y = "Társadalmak száma") + # tengelyen lévő feliratok
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5, size = 5) #bin-ben lévő elemek száma


# Boxplot arról, hogy milyen viszony van a hőmérséklet és a gyűjtögetés között
ggplot(gatherers, aes(x = factor(ea_code), y = eco_code, group = ea_code)) +
  geom_boxplot() +
  labs(x = "Gyűjtögetés mértéke", y = "Hőmérséklet bejósolhatósága") +
  theme_classic() 

# Boxplot arról, hogy milyen viszony van a hőmérséklet és a mezőgazdaság között
ggplot(agri, aes(x = factor(ea_code), y = eco_code, group = ea_code)) +
  geom_boxplot() +
  labs(x = "Mezőgazdasági tevékenység mértéke", y = "Hőmérséklet bejósolhatósága") +
  theme_classic() 

#Térkép arról, hogy a mezőgazd. és a gyűjt. csoportok hol találhatóak
# Új oszlop oszlop
gatherers <- gatherers %>%
  mutate(Típus = "Gyűjtögetők")

agri <- agri %>%
  mutate(Típus = "Mezőgazdaság")

#egyesítés
combined_data <- bind_rows(gatherers, agri)

world <- ne_countries(scale = "medium", returnclass = "sf")

# Plot, maga a térkép  
ggplot(data = world) +
  geom_sf() +
  geom_point(data = combined_data, aes(x = Long, y = Lat, color = Típus), size = 0.5) +
  scale_color_manual(values = c("Gyűjtögetők" = "blue", "Mezőgazdaság" = "red")) +
  theme_minimal() +
  labs(title = "Valamivalamijópofa",
       color = "Típus",
       x = "Hosszúság",
       y = "Szélesség")

#mezőgazd. és gyűjt. csoportok összehasonlítása, miből mennyi van
common_columns <- intersect(names(gatherers), names(agri)) #közös oszlopneveket a gatherers és agri között.
gatherers <- gatherers[, common_columns] # csak azokat az oszlopokat tartalmazzák, amelyek mindkettőben eredetileg szerepelnek.
agri <- agri[, common_columns]
gatherers$csoport <- "Gyűjtögetés" # új oszlop
agri$csoport <- "Mezőgazdaság" # új oszlop
combined_data <- rbind(gatherers, agri) #sorokat egyesítem
ggplot(combined_data, aes(x = factor(ea_code), fill = csoport)) +
  geom_bar(position = "dodge") +
  labs(x = "Hány %-ban támaszkodik az adott csoport a táplálékszerzés különböző formáira", 
       y = "Csoportok száma", 
       title = "Gyűjtögetés és Mezőgazdaság") +
  theme_classic() +
  scale_fill_manual(values = c("Gyűjtögetés" = "blue", "Mezőgazdaság" = "red"))












