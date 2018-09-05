# reference: https://rpubs.com/soumya2g/CUNY-Coursework

if (!file.exists("./database")) {
    dir.create("./database")
}

# download uci mashroom data
if (!file.exists("./database/uci_mashroom.txt")) {
    datUrl <- "https://archive.ics.uci.edu/ml/machine-learning-databases/mushroom/agaricus-lepiota.data"
    download.file(datUrl, destfile = "./database/uci_mashroom.txt")
}

#libraries
library(plyr)
library(dplyr)
# use to read data from html
#library(htmlTable)
library(magrittr)
library(kableExtra)
library(corrplot)
library(ggplot2)

dat_raw <- read.csv("./database/uci_mashroom.txt",
                   header = FALSE,
                   na.strings = '?',
                   quote = "")

df_mash <- dat_raw

head(df_mash) %>% kable() %>% 
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
    scroll_box(width="100%", height = "300px")

dict_mash <- as.data.frame(read.table("./database/data_dictory.txt", 
                                               header = TRUE, 
                                               sep = "\t"))

dict_mash <- rename(dict_mash, Variable=Column)

#colnames(dict_mash) <- c("Variable", "Values")

# renamed columns
names(df_mash) <- dict_mash$Variable

head(df_mash) %>% kable() %>% 
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
    scroll_box(width="100%", height = "300px")

# replace
#Attribute1: edible_poisonous
df_mash$edible_poisonous <- as.factor(mapvalues(df_mash$edible_poisonous,
                                                c("p","e"),
                                                c("poisonous","edible")))
#Attribute2: cap_shape
df_mash$cap_shape <- as.factor(mapvalues(df_mash$cap_shape,
                                         c("b","c","x","f","k","s"),
                                         c("bell","conical","convex","flat","knobbed","sunken")))
#Attribute3: cap_surface
df_mash$cap_surface <- as.factor(mapvalues(df_mash$cap_surface,
                                           c("f","g","y","s"),
                                           c("fibrous","grooves","scaly","smooth")))
#Attribute4: cap_color
df_mash$cap_color <- as.factor(mapvalues(df_mash$cap_color,
                                         c("n","b","c","g","r","p","u","e","w","y"),
                                         c("brown","buff","cinnamon","gray","green","pink","purple","red","white","yellow")))
#Attribute5: bruises?
df_mash$bruises <- as.factor(mapvalues(df_mash$bruises,
                                       c("t","f"),
                                       c("bruises","no")))
#Attribute6: odor
df_mash$odor <- as.factor(mapvalues(df_mash$odor,
                                    c("a","l","c","y","f","m","n","p","s"),
                                    c("almond","anise","creosote","fishy","foul","musty","none","pungent","spicy")))
#Attribute7: gill_attachment, The following `from` values were not present in `x`: d, n
df_mash$gill_attachment <- as.factor(mapvalues(df_mash$gill_attachment,
                                               c("a","d","f","n"),
                                               c("attached","descending","free","notched")))
#Attribute8: gill_spacing, The following `from` values were not present in `x`: d
df_mash$gill_spacing <- as.factor(mapvalues(df_mash$gill_spacing,
                                            c("c","w","d"),
                                            c("close","crowded","distant")))
#Attribute9: gill_size
df_mash$gill_size <- as.factor(mapvalues(df_mash$gill_size,
                                         c("b","n"),
                                         c("broad","narrow")))
#Attribute10: gill_color
df_mash$gill_color <- as.factor(mapvalues(df_mash$gill_color,
                                          c("k","n","b","h","g","r","o","p","u","e","w","y"),
                                          c("black","brown","buff","chocolate","gray","green","orange","pink","purple","red","white","yellow")))
#Attribute11: stalk_shape
df_mash$stalk_shape <- as.factor(mapvalues(df_mash$stalk_shape,
                                           c("e","t"),
                                           c("enlarging","tapering")))
#Attribute12: stalk_root, this column has missing data, The following `from` values were not present in `x`: u, z, NA
df_mash$stalk_root <- as.factor(mapvalues(df_mash$stalk_root,
                                          c("b","c","u","e","z","r", NA),
                                          c("bulbous","club","cup","equal","rhizomorphs","rooted","missing")))

#Attribute13: stalk_surface_above_ring
df_mash$stalk_surface_above_ring <- as.factor(mapvalues(df_mash$stalk_surface_above_ring,
                                                        c("f","y","k","s"),
                                                        c("fibrous","scaly","silky","smooth")))
#Attribute14: stalk_surface_below_ring
df_mash$stalk_surface_below_ring <- as.factor(mapvalues(df_mash$stalk_surface_below_ring,
                                                        c("f","y","k","s"),
                                                        c("fibrous","scaly","silky","smooth")))
#Attribute15: stalk_color_above_ring
df_mash$stalk_color_above_ring <- as.factor(mapvalues(df_mash$stalk_color_above_ring,
                                                      c("n","b","c","g","o","p","e","w","y"),
                                                      c("brown","buff","cinnamon","gray","orange","pink","red","white","yellow")))
#Attribute16: stalk_color_below_ring
df_mash$stalk_color_below_ring <- as.factor(mapvalues(df_mash$stalk_color_below_ring,
                                                      c("n","b","c","g","o","p","e","w","y"),
                                                      c("brown","buff","cinnamon","gray","orange","pink","red","white","yellow")))
#Attribute17: veil_type The following `from` values were not present in `x`: u
df_mash$veil_type <- as.factor(mapvalues(df_mash$veil_type,
                                         c("p","u"),
                                         c("partial","universal")))
#Attribute18: veil_color
df_mash$veil_color <- as.factor(mapvalues(df_mash$veil_color,
                                          c("n","o","w","y"),
                                          c("brown","orange","white","yellow")))
#Attribute19: ring_number
df_mash$ring_number <- as.factor(mapvalues(df_mash$ring_number,
                                           c("n","o","t"),
                                           c("none","one","two")))
#Attribute20: ring_type The following `from` values were not present in `x`: c, s, z
df_mash$ring_type <- as.factor(mapvalues(df_mash$ring_type,
                                         c("c","e","f","l","n","p","s","z"),
                                         c("cobwebby","evanescent","flaring","large","none","pendant","sheathing","zone")))
#Attribute21: spore_print_color
df_mash$spore_print_color <- as.factor(mapvalues(df_mash$spore_print_color,
                                                 c("k","n","b","h","r","o","u","w","y"),
                                                 c("black","brown","buff","chocolate","green","orange","purple","white","yellow")))
#Attribute22: population
df_mash$population <- as.factor(mapvalues(df_mash$population,
                                          c("a","c","n","s","v","y"),
                                          c("abundant","clustered","numerous","scattered","several","solitary")))
#Attribute23: habitat
df_mash$habitat <- as.factor(mapvalues(df_mash$habitat,
                                       c("g","l","m","p","u","w","d"),
                                       c("grasses","leaves","meadows","paths","urban","waste","woods")))

head(df_mash) %>% kable() %>% 
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>% 
    scroll_box(width="100%",height="300px")
#summary(df_mash)
#View(df_mash)

# subset of original
df_sub <- subset(df_mash, select=c(1,4,6,21,22,23))
head(df_sub) %>% kable() %>% 
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
    scroll_box(width="100%", height = "300px")

summary(df_sub) %>%
    kable() %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
    scroll_box(width = "100%", height = "300px")

str(df_mash)

# create new columns with id for each variable
df_sub$edible_poisonous_id <- as.integer(mapvalues(df_sub$edible_poisonous,
                                                   c("poisonous","edible"),
                                                   1:2))
df_sub$cap_color_id <- as.integer(mapvalues(df_sub$cap_color,
                                            c("brown","buff","cinnamon","gray","green","pink","purple","red","white","yellow"),
                                            1:10))
df_sub$odor_id <- as.integer(mapvalues(df_sub$odor,
                                       c("almond","anise","creosote","fishy","foul","musty","none","pungent","spicy"),
                                       1:9))
df_sub$spore_print_color_id <- as.integer(mapvalues(df_sub$spore_print_color,
                                                    c("black","brown","buff","chocolate","green","orange","purple","white","yellow"),
                                                    1:9))
df_sub$population_id <- as.integer(mapvalues(df_sub$population,
                                             c("abundant","clustered","numerous","scattered","several","solitary"),
                                             1:6))
df_sub$habitat_id <- as.integer(mapvalues(df_sub$habitat,
                                          c("grasses","leaves","meadows","paths","urban","waste","woods"),
                                          1:7))

#correlation plot
# to capture possible association/relationship between 'edible_poisonous' and any other variable
df_id <- subset(df_sub, select=c(7,8,9,10,11,12))
M <- cor(df_id)
corrplot(M, type = "upper", order = "hclust", sig.level = 0.01, insig = "brank")

df_poison <- subset(df_sub, df_sub$edible_poisonous == "poisonous" & df_sub$cap_color =="white" )
poison_habitat <- table(df_poison$habitat)
poison_habitat_ratio <- poison_habitat/sum(poison_habitat)
barplot(poison_habitat_ratio, main="Habitat Distribution for Poisonous Mashrooms")


df_edible <- subset(df_sub, df_sub$edible_poisonous == "edible" & df_sub$cap_color =="white" )
edible_habitat <- table(df_edible$habitat)
edible_habitat_ratio <- edible_habitat/sum(edible_habitat)
barplot(edible_habitat_ratio, main="Habitat Distribution for Edible Mashrooms")
