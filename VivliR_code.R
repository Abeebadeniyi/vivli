library("tidyverse")
library("readxl")
library(forcats) # Required for fct_infreq
library(ggrepel)
# install.packages("plotly")
library(plotly)

options(scipen = 999) # disable scientific notation

main_data <- read_csv("~/Documents/vivli/atlas_antibiotics.csv")



main_data <- main_data %>%
  mutate(continent = if_else(Country %in% c("France", "Spain", "Belgium", "Italy", "Germany", 
                                       "Ireland", "Portugal", "Greece", "United Kingdom", 
                                       "Poland", "Switzerland", "Hungary", "Austria", 
                                       "Finland", "Denmark", "Sweden", "Croatia", 
                                       "Czech Republic", "Netherlands", "Russia", 
                                       "Romania", "Latvia", "Lithuania", "Serbia", 
                                       "Ukraine", "Slovenia", "Bulgaria", "Norway", 
                                       "Slovak Republic", "Estonia"), "Europe",
                        if_else(Country %in% c("Canada", "United States", "Mexico", 
                                               "Guatemala", "Dominican Republic", "Costa Rica", 
                                               "Jamaica", "Honduras", "Puerto Rico", 
                                               "Nicaragua", "Panama","El Salvador"), "North America",
                                if_else(Country %in% c("Colombia", "Chile", "Venezuela", "Argentina", 
                                                       "Brazil"), "South America",
                                        if_else(Country %in% c("Australia", "New Zealand"), "Australia",
                                                if_else(Country %in% c("China", "Hong Kong", "Japan", "Malaysia", 
                                                                       "Thailand", "Philippines", "Korea, South", 
                                                                       "Taiwan", "India", "Singapore", "Vietnam", 
                                                                       "Indonesia"), "Asia",
                                                        if_else(Country %in% c("Nigeria", "Kenya", "South Africa", 
                                                                               "Ivory Coast", "Morocco", "Cameroon", 
                                                                               "Malawi", "Uganda", "Ghana", "Namibia", 
                                                                               "Mauritius", "Tunisia", "Egypt"), "Africa",
                                                                if_else(Country %in% c("Israel", "Kuwait", "Turkey", "Jordan", 
                                                                                       "Saudi Arabia", "Pakistan", "Lebanon", 
                                                                                       "Qatar", "Oman"), "Middle East", 
                                                                        NA_character_))))))))




# Define the lists of Gram-positive and Gram-negative bacteria
gram_positive <- c(
  "Staphylococcus aureus", "Staphylococcus epidermidis", "Staphylococcus haemolyticus",
  "Staphylococcus saprophyticus", "Staphylococcus hominis", "Staphylococcus lugdunensis",
  "Staphylococcus simulans", "Streptococcus pneumoniae", "Streptococcus pyogenes",
  "Streptococcus agalactiae", "Streptococcus bovis", "Streptococcus salivarius",
  "Enterococcus faecalis", "Enterococcus faecium", "Enterococcus hirae",
  "Enterococcus casseliflavus", "Enterococcus gallinarum", "Enterococcus durans",
  "Enterococcus raffinosus", "Enterococcus avium", "Clostridium perfringens",
  "Clostridioides difficile", "Clostridium tertium", "Clostridium butyricum",
  "Clostridium hathewayi", "Clostridium barati", "Peptostreptococcus anaerobius",
  "Peptostreptococcus magnus", "Peptostreptococcus hydrogenalis", "Peptostreptococcus indolicus",
  "Anaerococcus tetradius", "Anaerococcus vaginalis", "Anaerococcus murdochii",
  "Anaerococcus lactolyticus", "Anaerococcus prevotii", "Peptoniphilus asaccharolyticus",
  "Peptoniphilus olsenii", "Peptoniphilus coxii", "Peptoniphilus lacrimalis",
  "Peptoniphilus indolicus", "Clostridium clostridiiformis", "Clostridium subterminale",
  "Clostridium ramosum", "Clostridium citroniae", "Clostridium innocuum",
  "Clostridium scindens", "Clostridium symbiosum", "Clostridium cadaveris",
  "Clostridium sporogenes", "Clostridium sordellii", "Clostridium septicum",
  "Clostridium aldenense", "Clostridium bifermentans", "Clostridium bolteae",
  "Clostridium disporicum", "Clostridium glycolicum", "Clostridium beijerinckii",
  "Clostridium hastiforme", "Clostridium scatalogenes", "Clostridium putrificum",
  "Clostridium citroniae", "Clostridium novyi", "Clostridium difficile", "Staphylococcus arlettae",
  "Staphylococcus xylosus", "Staphylococcus capitis", "Staphylococcus sciuri",
  "Staphylococcus coagulase negative", "Streptococcus mitis", "Streptococcus gordonii",
  "Streptococcus intermedius", "Streptococcus oralis", "Streptococcus anginosus",
  "Streptococcus dysgalactiae", "Streptococcus canis", "Streptococcus salivarius",
  "Streptococcus viridans", "Streptococcus constellatus", "Streptococcus massiliensis",
  "Streptococcus gallolyticus", "Streptococcus parasanguinis", "Streptococcus sue,",
  "Enterococcus, non-speciated", "Enterococcus mundtii", "Enterococcus canintestini",
  "Enterococcus flavescens", "Staphylococcus warneri", "Staphylococcus caprae",
  "Staphylococcus pseudointermedius", "Staphylococcus intermedius",
  "Staphylococcus cohnii", "Staphylococcus schleiferi", "Staphylococcus auricularis",
  "Staphylococcus pettenkoferi", "Staphylococcus saccharolyticus",
  "Staphylococcus argenteus", "Staphylococcus vitulinus", "Staphylococcus hyicus",
  "Staphylococcus condimenti", "Staphylococcus Coagulase Negative",
  "Staphylococcus spp", "Staphylococcus, Beta Hemolytic", "Staphylococcus pasteuri",
  "Streptococcus sanguinis", "Streptococcus castoreus", "Streptococcus, viridans group",
  "Streptococcus sanguis", "Streptococcus suis", "Streptococcus equi", "Streptococcus spp",
  "Streptococcus pyogenes", "Streptococcus agalactiae", "Clostridium paraputrificum",
  "Clostridium limosum", "Clostridium celerecrescens", "Clostridium histolyticum",
  "Clostridium cochlearium", "Clostridium novyi", "Clostridium rectum",
  "Clostridium bifermentans", "Clostridium sphenoides", "Clostridium citrullae",
  "Clostridium difficile", "Clostridium clostridiiformis", "Clostridium barati",
  "Clostridium butyricum", "Clostridium sporogenes", "Clostridium tertium",
  "Peptostreptococcus lactolyticus", "Peptostreptococcus tetradius",
  "Peptostreptococcus spp", "Peptostreptococcus asaccharolyticus", "Corynebacterium aurimucosum",
  "Acidaminococcus fermentans", "Eggerthella spp", "Phocaeicola vulgatus",
  "Bacteroides ovatus", "Prevotella oralis", "Prevotella intermedia",
  "Bacteroides caccae", "Bacteroides salersyae", "Parabacteroides goldsteinii",
  "Peptoniphilus harei", "Prevotella disiens", "Peptoniphilus gorbachii",
  "Bacteroides pyogenes", "Prevotella baroniae", "Bacteroides intestinalis",
  "Bacteroides stercosis", "Parabacteroides johnsonii", "Anaerococcus hydrogenalis",
  "Anaerococcus spp", "Anaerococcus octavius", "Bacteroides coagulans",
  "Bacteroides cellulosilyticus", "Clostridium colicanis", "Clostridium spp",
  "Clostridium novyia", "Paraclostridium bifermentans", "Kerstersia gyiorum",
  "Enterococcus spp", "Enterococcus Group D", "Streptococcus lutetiensis",
  "Streptococcus, Beta Hemolytic",
  "Parvimonas micra", "Peptoniphilus spp", "Prevotella oulorum",
  "Staphylococcus petrasii", "Alcaligenes faecalis", "Enterococcus malodoratus",
  "Enterocloster bolteae")



gram_negative <- c(
  "Pseudomonas aeruginosa", "Serratia marcescens", "Acinetobacter pitii", "Acinetobacter baumannii",
  "Enterobacter cloacae", "Escherichia coli", "Haemophilus influenzae", "Citrobacter freundii",
  "Klebsiella pneumoniae", "Klebsiella aerogenes", "Acinetobacter junii", "Klebsiella oxytoca",
  "Enterobacter kobei", "Acinetobacter lwoffii", "Serratia liquefaciens", "Enterobacter asburiae",
  "Citrobacter koseri", "Serratia fonticola", "Serratia rubidaea", "Acinetobacter schindleri",
  "Acinetobacter guillouiae", "Acinetobacter calcoaceticus", "Acinetobacter nosocomialis",
  "Pluralibacter gergoviae", "Acinetobacter radioresistens", "Acinetobacter johnsonii",
  "Acinetobacter ursingii", "Acinetobacter haemolyticus", "Acinetobacter parvus",
  "Acinetobacter tjernbergiae", "Klebsiella variicola", "Klebsiella, non-speciated",
  "Serratia, non-speciated", "Citrobacter murliniae", "Citrobacter sedlakii", "Citrobacter youngae",
  "Pseudomonas putida", "Pseudomonas stutzeri", "Pseudomonas monteilii", "Pseudomonas nitroreducens",
  "Pseudomonas alcaliphila", "Pseudomonas putida/fluorescens Group", "Burkholderia cepacia",
  "Burkholderia cenocepacia", "Moraxella catarrhalis", "Aeromonas hydrophila", "Aeromonas caviae",
  "Aeromonas veronii", "Vibrio cholerae", "Campylobacter jejuni", "Helicobacter pylori",
  "Neisseria gonorrhoeae", "Neisseria meningitidis", "Helicobacter pylori", "Achromobacter xylosoxidans",
  "Achromobacter insolitus", "Bordetella trematum", "Myroides odoratimimus", "Achromobacter xylosoxidans",
  "Pseudomonas citronellolis", "Pseudomonas pseudoalcaligenes", "Bacteroides fragilis",
  "Parabacteroides distasonis", "Prevotella denticola", "Bacteroides thetaiotaomicron",
  "Bacteroides vulgatus", "Prevotella buccae", "Prevotella oris", "Prevotella bivia",
  "Bacteroides nordii", "Bacteroides uniformis", "Prevotella nanceiensis", "Prevotella melaninogenica",
  "Prevotella loescheii", "Prevotella timonensis", "Prevotella spp", "Prevotella veroralis",
  "Prevotella heparinolytica", "Prevotella tannerae", "Bacteroides eggerthii", "Bacteroides faecis",
  "Bacteroides dorei", "Bacteroides splanchnicus", "Bacteroides bivius", "Bacteroides capsillosis",
  "Bacteroides massiliensis", "Bacteroides merdeae", "Prevotella multiformis", "Fusobacterium nucleatum",
  "Anaerovorax spp", "Veillonella parvula", "Eubacterium lentum", "Eubacterium aerofaciens",
  "Finegoldia magna", "Enterobacter cancerogenus", "Enterobacter intermedium",
  "Enterobacter xiangfangensis", "Citrobacter diversus", "Citrobacter spp", "Enterobacter spp",
  "Pseudomonas graminis", "Pseudomonas spp", "Moraxella osloensis", "Paeniclostridium sordelli",
  "Elizabethkingia anophelis", "Comamonas kerstersii", "Neisseria gonorrhoeae",
  "Neisseria meningitidis", 
  "Acinetobacter, non-speciated", "Acinetobacter baylyi", "Acinetobacter bereziniae",
  "Acinetobacter beijerinckii", "Acinetobacter anitratus", "Acinetobacter seifertii",
  "Acinetobacter dijkshoorniae", "Acinetobacter variabilis", "Acinetobacter gyllenbergii",
  "Acinetobacter colistiniresistens", "Acinetobacter modestus", "Acinetobacter proteolyticus",
  "Acinetobacter spp", "Acinetobacter tandoii", "Acinetobacter vivianii",
  "Acinetobacter lactucae", "Acinetobacter dispersus", "Pseudomonas otitidis",
  "Pseudomonas mendocina", "Pseudomonas fluorescens", "Pseudomonas stewartii",
  "Pseudomonas mosselii", "Pseudomonas alcaligenes", "Pseudomonas guariconensis",
  "Pseudomonas fulva", "Pseudomonas citronellolis", "Enterobacter ludwigii",
  "Enterobacter, non-speciated", "Enterobacter agglomerans", "Enterobacter taylorae",
  "Enterobacter gergoviae", "Enterobacter bugandensis", "Enterobacter hormaechi",
  "Klebsiella ozaenae", "Klebsiella planticola", "Klebsiella spp", "Klebsiella rhinoscleromatis",
  "Klebsiella ornithinolytica", "Serratia odorifera", "Serratia ficaria",
  "Serratia ureilytica", "Serratia grimesii", "Serratia spp", "Citrobacter braakii",
  "Citrobacter farmeri", "Citrobacter gillenii", "Citrobacter amalonaticus", "Citrobacter spp",
  "Stenotrophomonas maltophilia", "Lelliottia amnigena", "Proteus vulgaris",
  "Proteus mirabilis", "Proteus penneri", "Proteus hauseri", "Morganella morganii",
  "Providencia stuartii", "Providencia rettgeri", "Providencia alcalifaciens",
  "Providencia spp", "Raoultella ornithinolytica", "Raoultella planticola",
  "Raoultella terrigena", "Aeromonas spp", "Bordetella spp", "Salmonella spp",
  "Moraxella spp", "Campylobacter ureolyticus", "Haemophilus parainfluenzae",
  "Haemophilus parahaemolyticus", "Haemophilus pittmaniae", "Haemophilus spp",
  "Cronobacter spp", "Kluyvera ascorbata", "Pantoea spp", "Pantoea agglomerans",
  "Pantoea septica", "Escherichia hermanii", "Escherichia vulneris", "Escherichia spp",
  "Phocaeicola vulgatus", "Bacteroides spp", "Prevotella spp", "Parabacteroides spp",
  "Prevotella buccalis", "Prevotella amnii", "Prevotella corporis", "Prevotella nanceinsis",
  "Prevotella nigrescens", "Prevotella jejunii", "Prevotella histicola", "Prevotella pallens",
  "Prevotella bergensis", "Prevotella salivae", "Prevotella maculosa",
  "Eggerthella lenta", "Enterocloster spp", "Enterocloster clostridioformis",
  "Enterocloster citroniae", "Paraclostridium spp",
  "Enterobacter sakazakii", "Serratia plymuthica", "Hafnia alvei",
  "Citrobacter, non-speciated", "Providencia, non-speciated", "Proteus rettgeri",
  "Pantoea dispersa", "Cronobacter sakazakii", "Escherichia fergusonii",
  "Enterobacter roggenkampii", "Acinetobacter towneri", "Acinetobacter soli",
  "Raoultella spp", "Acinetobacter venetianus", "Acinetobacter indicus",
  "Acinetobacter pseudolwoffii", "Acinetobacter alcaligenes", "Escherichia spp",
  "Pseudomonas spp", "Proteus spp", "Acinetobacter courvalinii", "Ochrobactrum anthropi",
  "Providencia rustigianii", "Morganella spp", "Parvimonas spp",
  "Enterobacter liquifaciens")



# Add Gram status column using if_else() and mutate()

bacteria_df <- main_data %>%
  mutate(
    Gram_Status = if_else(
      Species %in% gram_positive, 
      "Gram-positive",
      if_else(
        Species %in% gram_negative,
        "Gram-negative",
        "Unknown"
      )
    )
  )

# View the results
print(bacteria_df)



#gram_pos <- bacteria_df %>% filter(Gram_Status == "Gram-positive")

#gram_neg <- bacteria_df %>% filter(Gram_Status == "Gram-negative")


bacteria_df <- bacteria_df %>% select("Isolate Id", "Study", "Species", "Gram_Status", "Family", "Country","continent", "Gender", "Age Group", "Speciality", "Source",
                       "In / Out Patient", "Year", "Phenotype", "Amikacin", "Amikacin_I", "Amoxycillin clavulanate", "Amoxycillin clavulanate_I",
                       "Ampicillin", "Ampicillin_I", "Azithromycin", "Azithromycin_I", "Cefepime", "Cefepime_I", "Cefoxitin", "Cefoxitin_I",
                       "Ceftazidime", "Ceftazidime_I", "Ceftriaxone", "Ceftriaxone_I", "Clarithromycin", "Clarithromycin_I", "Clindamycin",
                       "Clindamycin_I", "Erythromycin", "Erythromycin_I", "Imipenem", "Imipenem_I", "Levofloxacin", "Levofloxacin_I", 
                       "Linezolid", "Linezolid_I", "Meropenem", "Meropenem_I", "Metronidazole", "Metronidazole_I", "Minocycline", 
                       "Minocycline_I", "Penicillin", "Penicillin_I", "Piperacillin tazobactam", "Piperacillin tazobactam_I", "Tigecycline",
                       "Tigecycline_I", "Vancomycin", "Vancomycin_I", "Ampicillin sulbactam", "Ampicillin sulbactam_I", "Aztreonam", 
                       "Aztreonam_I", "Aztreonam avibactam", "Aztreonam avibactam_I", "Cefixime", "Cefixime_I", "Ceftaroline", "Ceftaroline_I",
                       "Ceftaroline avibactam", "Ceftaroline avibactam_I", "Ceftazidime avibactam", "Ceftazidime avibactam_I", "Ciprofloxacin",
                       "Ciprofloxacin_I", "Colistin", "Colistin_I", "Daptomycin", "Daptomycin_I", "Doripenem", "Doripenem_I", "Ertapenem",
                       "Ertapenem_I", "Gatifloxacin", "Gatifloxacin_I", "Gentamicin", "Gentamicin_I", "Moxifloxacin", "Moxifloxacin_I",
                       "Oxacillin", "Oxacillin_I", "Quinupristin dalfopristin", "Quinupristin dalfopristin_I", "Sulbactam", "Sulbactam_I",
                       "Teicoplanin", "Teicoplanin_I", "Tetracycline", "Tetracycline_I", "Trimethoprim sulfa", "Trimethoprim sulfa_I",
                       "Ceftolozane tazobactam", "Ceftolozane tazobactam_I", "Cefoperazone sulbactam", "Cefoperazone sulbactam_I",
                       "Meropenem vaborbactam", "Meropenem vaborbactam_I", "Cefpodoxime", "Cefpodoxime_I", "Ceftibuten", "Ceftibuten_I",
                       "Ceftibuten avibactam", "Ceftibuten avibactam_I", "Tebipenem", "Tebipenem_I")

#### partitioning the data into two:

epidem_data <- bacteria_df %>% select("Isolate Id", "Study", "Species", "Gram_Status", "Family", "Country","continent", "Gender", "Age Group", "Speciality", "Source",
                                      "In / Out Patient", "Year")
  
mic_data <- bacteria_df %>% select("Isolate Id", ends_with("_I"))


ast_data <-  bacteria_df %>% 
select(!ends_with("_I")) %>% 
select(1, 15:ncol(.))


##### convert all the values in ast_data to numeric

ast_data <- ast_data %>% 
  select(1, 2:ncol(.)) %>%
  mutate(across(-1, ~ as.numeric(as.character(.))))  #, .names = "{col}"))

## combine them together again:
      #combined_df <- epidem_data %>%
       # full_join(breakpoint_data, by = "Isolate Id") %>%
        #full_join(ast_data, by = "Isolate Id")


### ast_combined data

ast_combined_data <- epidem_data %>%
  full_join(ast_data, by = "Isolate Id")


### breakpoint_combined data
mic_combined_data <- epidem_data %>%
  full_join(mic_data, by = "Isolate Id")


### pivot longer:
ast_pivot_long <- ast_combined_data %>%
  pivot_longer(cols = 14:ncol(ast_combined_data), 
               names_to = "antibiotics", 
               values_to = "MIC") %>% drop_na("MIC")


mic_pivot_long <- mic_combined_data %>%
  pivot_longer(cols = 14:ncol(mic_combined_data), 
               names_to = "antibiotics", 
               values_to = "MIC_Interpretation") %>% drop_na("MIC_Interpretation")

# now remove the "_I" suffix extension at the end of the values in antibiotics column of mic_pivot_long

mic_pivot_long <- mic_pivot_long %>%
  mutate(antibiotics = str_replace(antibiotics, "_I$", ""))



## visualisation

mic_pivot_long %>% filter(antibiotics == "Meropenem_I") %>% 
  select("Isolate Id","antibiotics", "MIC_Interpretation") %>% 
  ggplot(aes(x = MIC_Interpretation)) +
  geom_bar(aes(fill = MIC_Interpretation), color = "black") +
  labs(title = "Frequency of MIC Interpretation",
       x = "MIC Interpretation",
       y = "Count") +
  theme_minimal() +
  theme(legend.position = "none")

# convert this to a function that can take argument:

# Load necessary libraries
library(ggplot2)
library(dplyr)

# Define the function
plot_MIC_interpretation <- function(data, antibiotic) {
  data %>%
    filter(antibiotics == antibiotic) %>% 
    select("Isolate Id", "antibiotics", "MIC_Interpretation") %>% 
    ggplot(aes(x = MIC_Interpretation)) +
    geom_bar(aes(fill = MIC_Interpretation), color = "black") +
    labs(
      title = paste("Frequency of MIC Interpretation for", antibiotic),
      x = "MIC Interpretation",
      y = "Count"
    ) +
    theme_minimal() +
    theme(legend.position = "none")
}

# Example usage
# Assuming `breakpoint_pivot_long` is your dataframe

plot_MIC_interpretation(mic_pivot_long, "Meropenem_I")

plot_MIC_interpretation(mic_pivot_long, "Ampicillin_I")


mic_pivot_long %>%
  filter(continent == "Africa" & MIC_Interpretation == "Resistant") %>%
  select("Isolate Id", "antibiotics", "MIC_Interpretation", "Species") %>%
  ggplot(aes(x = fct_infreq(Species))) +  # Reorder Species by frequency
  geom_bar(aes(fill = MIC_Interpretation), color = "black") +
  labs(
    title = "Frequency of Resistance per organism in Africa",
    x = "Species",
    y = "Count"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


# ####################Resistance In the world######################

# Step 1: Count the frequencies and filter by count
filtered_data <- mic_pivot_long %>%
  filter(MIC_Interpretation == "Resistant") %>%
  count(Species) %>%
  filter(n > 100) %>%   ## to plot only resitance count greater than 100
  select(Species)  # Keep only Species column for joining

# Step 2: Plot with the filtered data

x <- mic_pivot_long %>%
  filter(MIC_Interpretation == "Resistant") %>% 
  semi_join(filtered_data, by = "Species")

mic_pivot_long %>%
  filter(MIC_Interpretation == "Resistant") %>%
  semi_join(filtered_data, by = "Species") %>%  # Join with filtered_data
  ggplot(aes(x = fct_infreq(Species))) +  # Reorder Species by frequency
  geom_bar(aes(fill = MIC_Interpretation), color = "black") +
  labs(
    title = "Global Frequency of Resistance per organism",
    x = "Species",
    y = "Count"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 90, hjust = 1, size = 8)
  )





# africa burden in term of infection
mic_pivot_long %>%
  distinct(`Isolate Id`, .keep_all = TRUE) %>%
  filter(continent == "Africa") %>%
  select("Isolate Id", "antibiotics", "MIC_Interpretation", "Species") %>%
  ggplot(aes(x = fct_infreq(Species))) +  # Reorder Species by frequency
  geom_bar(aes(fill = MIC_Interpretation), color = "black") +
  labs(
    title = "Frequency of Infection per organism in Africa",
    x = "Species",
    y = "Count"
  ) +
  theme_minimal() +
  theme(
    legend.position = c(0.5, 0.5), # Center the legend
    legend.justification = "center", # Align legend to its center
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
   

# global burden in term of infection
filtered_data <- mic_pivot_long %>%
  count(Species) %>%
  filter(n > 10000) %>%   ## to plot only resistance count greater than 100
  select(Species) 

  mic_pivot_long %>%
  distinct(`Isolate Id`, .keep_all = TRUE) %>%
  semi_join(filtered_data, by = "Species") %>%
  ggplot(aes(x = fct_infreq(Species))) +  # Reorder Species by frequency
  geom_bar(aes(fill = MIC_Interpretation), color = "black") +
  labs(
    title = "Frequency of Infection per organism globally",
    x = "Species",
    y = "Count"
  ) +
  theme_minimal() +
  theme(
    legend.position = c(0.5, 0.5), # Center the legend
    legend.justification = "center", # Align legend to its center
    axis.text.x = element_text(angle = 45, hjust = 1)
  )




############################ second part : ##################################################

# Create a vector for each antibiotic class
penicillins <- c("Amoxycillin clavulanate", "Ampicillin", "Ampicillin sulbactam", "Piperacillin tazobactam")
cephalosporins <- c("Cefepime", "Ceftazidime", "Ceftriaxone", "Ceftaroline", "Ceftolozane tazobactam", "Cefoperazone sulbactam")
cephalosporin_bli <- c("Ceftazidime avibactam", "Ceftaroline avibactam")
monobactams <- c("Aztreonam", "Aztreonam avibactam")
carbapenems <- c("Meropenem", "Imipenem", "Doripenem", "Ertapenem", "Meropenem vaborbactam")
tetracyclines <- c("Minocycline", "Tigecycline")
macrolides <- c("Azithromycin")
fluoroquinolones <- c("Levofloxacin", "Ciprofloxacin")
aminoglycosides <- c("Gentamicin", "Amikacin")
polymyxins <- c("Colistin")
folate_inhibitors <- c("Trimethoprim sulfa")
beta_lactamase_inhibitors <- c("Sulbactam")

# Perform pivot_longer and create a new column for antibiotic class
ast_pivot_long <- ast_combined_data %>%
  pivot_longer(cols = 14:ncol(ast_combined_data), 
               names_to = "antibiotics", 
               values_to = "MIC") %>%
  drop_na("MIC") %>%
  mutate(
    Antibiotic_Class = case_when(
      antibiotics %in% penicillins ~ "Penicillins",
      antibiotics %in% cephalosporins ~ "Cephalosporins",
      antibiotics %in% cephalosporin_bli ~ "Cephalosporin/Beta-Lactamase Inhibitor Combinations",
      antibiotics %in% monobactams ~ "Monobactams",
      antibiotics %in% carbapenems ~ "Carbapenems",
      antibiotics %in% tetracyclines ~ "Tetracyclines",
      antibiotics %in% macrolides ~ "Macrolides",
      antibiotics %in% fluoroquinolones ~ "Fluoroquinolones",
      antibiotics %in% aminoglycosides ~ "Aminoglycosides",
      antibiotics %in% polymyxins ~ "Polymyxins",
      antibiotics %in% folate_inhibitors ~ "Folate Pathway Inhibitors",
      antibiotics %in% beta_lactamase_inhibitors ~ "Beta-Lactamase Inhibitors",
      TRUE ~ "Unknown"
    )
  )

#############################INTERACTIVE PLOT#########################

# Load necessary libraries
library(ggplot2)
library(plotly)
library(dplyr)
library(forcats)  # for fct_infreq()

# Your data processing and plot


#x <- mic_pivot_long %>%
#  semi_join(filtered_data, by = "Species")
  
filtered_data <- mic_pivot_long %>%
  count(Species) %>%
  filter(n > 5000) %>%   ## to plot only resistance count greater than 100
  select(Species) 
interactive_plot <- mic_pivot_long %>%
  distinct(`Isolate Id`, .keep_all = TRUE) %>%  #Keep only distinct entries by Isolate Id
  semi_join(filtered_data, by = "Species") %>%
  semi_join(filtered_data, by = "Species") %>%
  ggplot(aes(x = fct_infreq(Species))) +  # Reorder Species by frequency
  geom_bar(aes(fill = MIC_Interpretation), color = "black") +
  labs(
    title = "Frequency of Infection per organism globally",
    x = "Species",
    y = "Count"
  ) +
  theme_minimal() +
  theme(
    legend.position = c(0.5, 0.5), # Center the legend
    legend.justification = "center", # Align legend to its center
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Convert ggplot to plotly
interactive_plotly <- ggplotly(interactive_plot)

# Print or show the interactive plot
interactive_plotly


####################

y <- mic_pivot_long %>% 
  group_by(`Isolate Id`, Species, MIC_Interpretation) %>%  # Group by species
  summarise(n = n()) %>% 
  ungroup()
y1 <- y %>% 
  filter(MIC_Interpretation == "Resistant") %>%
  group_by(Species) %>% 
  summarize(total_n = sum(n, na.rm = TRUE))

  
y %>% 
  filter(MIC_Interpretation == "Resistant") %>%
  group_by(Species) %>% 
  summarize(total_n = sum(n, na.rm = TRUE))  %>%
    ggplot(aes(x = fct_reorder(Species, -total_n), y = total_n)) +  # Reorder bars by frequency
    geom_bar(stat = "identity", aes(fill = Species), color = "black") +  # Use identity to use `n` as height
    labs(
      title = "Frequency of Resistant Organisms by Species",
      x = "Species",
      y = "Frequency (n)"
    ) +
    theme_minimal() +
    theme(
      legend.position = "none",  # Remove the legend
      axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis text
    )
  




##########################################

plot_MIC_interpretation_normalised_4 <- function(data, continent = NULL, species = NULL, antibiotic = NULL) {
  # Filter data based on the provided continent, species, and antibiotic arguments
  data_filtered <- data
  
  if (!is.null(continent)) {
    data_filtered <- data_filtered %>% filter(continent == continent)
  }
  
  if (!is.null(species)) {
    data_filtered <- data_filtered %>% filter(Species == species)
  }
  
  if (!is.null(antibiotic)) {
    data_filtered <- data_filtered %>% filter(antibiotics == antibiotic)
  }
  
  # Prepare data for plotting
  if (!is.null(antibiotic)) {
    plot_data <- data_filtered %>%
      select("Isolate Id", "antibiotics", "MIC_Interpretation") %>%
      group_by(MIC_Interpretation) %>%
      summarize(count = n()) %>%
      mutate(percentage = count / sum(count) * 100)
    
    # Create the plot for a specific antibiotic with percentage
    plot <- plot_data %>%
      ggplot(aes(x = MIC_Interpretation, y = percentage, fill = MIC_Interpretation)) +
      geom_bar(stat = "identity", position = "dodge", color = "black") +
      labs(
        title = paste("Percentage Frequency of MIC Interpretation for", 
                      antibiotic, 
                      if (!is.null(continent)) paste("in", continent) else "",
                      if (!is.null(species)) paste("for", species) else ""),
        x = "MIC Interpretation",
        y = "Percentage (%)"
      ) +
      theme_minimal() +
      theme(
        legend.position = "none",  # Remove legend for individual MIC interpretation plot
        axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis text
      )
    
  } else {
    plot_data <- data_filtered %>%
      select("Isolate Id", "antibiotics", "MIC_Interpretation") %>%
      group_by(antibiotics, MIC_Interpretation) %>%
      summarize(count = n()) %>%
      mutate(percentage = count / sum(count) * 100)
    
    # Create the plot for all antibiotics with stacking and percentage
    plot <- plot_data %>%
      ggplot(aes(x = antibiotics, y = percentage, fill = MIC_Interpretation)) +
      geom_bar(stat = "identity", position = "stack", color = "black") +
      labs(
        title = paste("Percentage Frequency of MIC Interpretation",
                      if (!is.null(continent)) paste("in", continent) else "",
                      if (!is.null(species)) paste("for", species) else ""),
        x = "Antibiotics",
        y = "Percentage (%)"
      ) +
      theme_minimal() +
      theme(
        legend.position = "bottom",  # Place legend at the bottom
        axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis text
      )
  }
  
  # Convert ggplot object to plotly for interactivity
  interactive_plot <- ggplotly(plot)
  
  # Update the title in the interactive plot to reflect the filters used
  title <- if (!is.null(antibiotic)) {
    paste("Percentage Frequency of MIC Interpretation for", antibiotic,
          if (!is.null(continent)) paste("in", continent) else "",
          if (!is.null(species)) paste("for", species) else "")
  } else {
    paste("Percentage Frequency of MIC Interpretation",
          if (!is.null(continent)) paste("in", continent) else "",
          if (!is.null(species)) paste("for", species) else "")
  }
  
  interactive_plot <- interactive_plot %>% layout(title = title)
  
  # Return the interactive plot
  return(interactive_plot)
}
plot_MIC_interpretation_normalised_4(mic_pivot_long, "Europe") # Filter by continent
plot_MIC_interpretation_normalised_4(mic_pivot_long, "Europe", "Escherichia coli") # Filter by continent and species
plot_MIC_interpretation_normalised_4(mic_pivot_long, "Europe", "Escherichia coli", "Meropenem") # Filter by continent, species, and antibiotic
plot_MIC_interpretation_normalised_4(mic_pivot_long) # Plot all data








plot_MIC_interpretation_normalised_4 <- function(data, continent = NULL, species = NULL, antibiotic = NULL) {
  # Step 1: Filter data based on the provided continent, species, and antibiotic arguments
  data_filtered <- data
  
  if (!is.null(continent)) {
    data_filtered <- data_filtered %>% filter(Continent == continent)  # Adjusted to 'Continent'
    print(paste("Filtering by continent:", continent))
    print(head(data_filtered))  # Show the first few rows of the filtered data
  }
  
  if (!is.null(species)) {
    data_filtered <- data_filtered %>% filter(Species == species)
    print(paste("Filtering by species:", species))
    print(head(data_filtered))  # Show the first few rows of the filtered data
  }
  
  if (!is.null(antibiotic)) {
    data_filtered <- data_filtered %>% filter(antibiotics == antibiotic)
    print(paste("Filtering by antibiotic:", antibiotic))
    print(head(data_filtered))  # Show the first few rows of the filtered data
  }
  
  # Step 2: Prepare data for plotting
  if (!is.null(antibiotic)) {
    plot_data <- data_filtered %>%
      select("Isolate Id", "antibiotics", "MIC_Interpretation") %>%
      group_by(MIC_Interpretation) %>%
      summarize(count = n()) %>%
      mutate(percentage = count / sum(count) * 100)
    
    # Create the plot for a specific antibiotic with percentage
    plot <- plot_data %>%
      ggplot(aes(x = MIC_Interpretation, y = percentage, fill = MIC_Interpretation)) +
      geom_bar(stat = "identity", position = "dodge", color = "black") +
      labs(
        title = paste("Percentage Frequency of MIC Interpretation for", 
                      antibiotic, 
                      if (!is.null(continent)) paste("in", continent) else "",
                      if (!is.null(species)) paste("for", species) else ""),
        x = "MIC Interpretation",
        y = "Percentage (%)"
      ) +
      theme_minimal() +
      theme(
        legend.position = "none",  # Remove legend for individual MIC interpretation plot
        axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis text
      )
    
  } else {
    plot_data <- data_filtered %>%
      select("Isolate Id", "antibiotics", "MIC_Interpretation") %>%
      group_by(antibiotics, MIC_Interpretation) %>%
      summarize(count = n()) %>%
      mutate(percentage = count / sum(count) * 100)
    
    # Create the plot for all antibiotics with stacking and percentage
    plot <- plot_data %>%
      ggplot(aes(x = antibiotics, y = percentage, fill = MIC_Interpretation)) +
      geom_bar(stat = "identity", position = "stack", color = "black") +
      labs(
        title = paste("Percentage Frequency of MIC Interpretation",
                      if (!is.null(continent)) paste("in", continent) else "",
                      if (!is.null(species)) paste("for", species) else ""),
        x = "Antibiotics",
        y = "Percentage (%)"
      ) +
      theme_minimal() +
      theme(
        legend.position = "bottom",  # Place legend at the bottom
        axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis text
      )
  }
  
  # Convert ggplot object to plotly for interactivity
  interactive_plot <- ggplotly(plot)
  
  # Update the title in the interactive plot to reflect the filters used
  title <- if (!is.null(antibiotic)) {
    paste("Percentage Frequency of MIC Interpretation for", antibiotic,
          if (!is.null(continent)) paste("in", continent) else "",
          if (!is.null(species)) paste("for", species) else "")
  } else {
    paste("Percentage Frequency of MIC Interpretation",
          if (!is.null(continent)) paste("in", continent) else "",
          if (!is.null(species)) paste("for", species) else "")
  }
  
  interactive_plot <- interactive_plot %>% layout(title = title)
  
  # Step 3: Return the interactive plot
  return(interactive_plot)
}

# Run the test again after adjusting the column name
plot_MIC_interpretation_normalised_4(mic_pivot_long, "Africa", "Escherichia coli", "Meropenem") # Filter by continent, species, and antibiotic
plot_MIC_interpretation_normalised_4(mic_pivot_long, "Europe") # Filter by continent
plot_MIC_interpretation_normalised_4(mic_pivot_long, "Europe", "Escherichia coli") # Filter by continent and species
plot_MIC_interpretation_normalised_4(mic_pivot_long, "Europe", "Escherichia coli", "Meropenem") # Filter by continent, species, and antibiotic
plot_MIC_interpretation_normalised_4(mic_pivot_long)



###############################################################################

plot_resistance_2_normalised <- function(data, continent = NULL, country = NULL, phenotype = NULL) {
  # Verify the structure of the data
  if (!all(c("Species", "Continent", "Country", "RIS") %in% colnames(data))) {
    stop("Data must contain columns: Species, Continent, Country, and RIS.")
  }
  
  # Step 1: Filter data based on the provided continent, country, and phenotype arguments
  data_filtered <- data
  
  # Apply filters based on the combinations of continent, country, and phenotype
  if (!is.null(continent) && !is.null(country) && !is.null(phenotype)) {
    data_filtered <- data_filtered %>%
      filter(Continent == continent, Country == country, RIS %in% phenotype)
    plot_title <- paste("Percentage of Resistance in", country, "(", continent, ") for", phenotype, "Organisms")
  } else if (!is.null(continent) && !is.null(country)) {
    data_filtered <- data_filtered %>%
      filter(Continent == continent, Country == country)
    plot_title <- paste("Percentage of Resistance in", country, "(", continent, ")")
  } else if (!is.null(continent) && !is.null(phenotype)) {
    data_filtered <- data_filtered %>%
      filter(Continent == continent, RIS %in% phenotype)
    plot_title <- paste("Percentage of Resistance in", continent, "for", phenotype, "Organisms")
  } else if (!is.null(country) && !is.null(phenotype)) {
    data_filtered <- data_filtered %>%
      filter(Country == country, RIS %in% phenotype)
    plot_title <- paste("Percentage of Resistance in", country, "for", phenotype, "Organisms")
  } else if (!is.null(continent)) {
    data_filtered <- data_filtered %>%
      filter(Continent == continent)
    plot_title <- paste("Percentage of Resistance in", continent)
  } else if (!is.null(country)) {
    data_filtered <- data_filtered %>%
      filter(Country == country)
    plot_title <- paste("Percentage of Resistance in", country)
  } else if (!is.null(phenotype)) {
    data_filtered <- data_filtered %>%
      filter(RIS %in% phenotype)
    plot_title <- paste("Percentage of Resistance for", phenotype, "Organisms")
  } else {
    plot_title <- "Percentage of Resistance for All Data"
  }
  
  # Step 2: Prepare the data for plotting
  plot_data <- data_filtered %>%
    group_by(Species, RIS) %>%
    summarise(count = n(), .groups = 'drop') %>%
    group_by(Species) %>%
    mutate(percentage = count / sum(count) * 100) %>%
    ungroup()
  
  # Calculate the percentage of resistant organisms for each species
  resistant_percentages <- plot_data %>%
    filter(RIS == "Resistant") %>%
    select(Species, percentage)
  
  # Filter out species with less than 30% resistance and order by decreasing resistance
  species_to_plot <- resistant_percentages %>%
    filter(percentage >= 30) %>%
    arrange(desc(percentage)) %>%
    pull(Species)
  
  # Filter the plot data to include only the selected species
  plot_data_filtered <- plot_data %>%
    filter(Species %in% species_to_plot)
  
  # Check if the filtered data is empty
  if (nrow(plot_data_filtered) == 0) {
    stop("No data available for the specified filters or all species have less than 30% resistance. Please try different parameters.")
  }
  
  # Step 3: Create the plot using ggplot2
  plot <- plot_data_filtered %>%
    ggplot(aes(x = factor(Species, levels = species_to_plot), y = percentage, fill = RIS)) +
    geom_bar(position = "stack", stat = "identity", color = "black") +
    labs(
      title = plot_title,
      x = "Species",
      y = "Percentage"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 90, hjust = 1, size = 8)
    )
  
  # Step 4: Convert the ggplot object to a plotly interactive plot
  interactive_plot <- ggplotly(plot)
  
  # Return the interactive plot
  return(interactive_plot)
}

plot_resistance_2_normalised(RIS_pivot_long) # All data
plot_resistance_2_normalised(RIS_pivot_long, continent = "Africa") # Specific continent
plot_resistance_2_normalised(RIS_pivot_long, country = "Nigeria") # Specific country
plot_resistance_2_normalised(RIS_pivot_long, continent = "Africa", country = "Nigeria") # Specific continent and country
plot_resistance_2_normalised(RIS_pivot_long, phenotype = c("Resistant")) # Specific phenotype
plot_resistance_2_normalised(RIS_pivot_long, continent = "Africa", phenotype = c("Resistant")) # Specific continent and phenotype
plot_resistance_2_normalised(RIS_pivot_long, country = "Nigeria", phenotype = c("Resistant")) # Specific country and phenotype
plot_resistance_2_normalised(RIS_pivot_long, continent = "Africa", country = "Nigeria", phenotype = c("Resistant")) # All filters







plot_RIS_normalised_4 <- function(data, continent = NULL, species = NULL, antibiotic = NULL) {
  # Ensure the data is a dataframe
  if (!is.data.frame(data)) {
    stop("The input data must be a dataframe.")
  }
  
  # Step 1: Filter data based on the provided continent, species, and antibiotic arguments
  data_filtered <- data
  
  if (!is.null(continent)) {
    data_filtered <- dplyr::filter(data_filtered, Continent == continent)
    print(paste("Filtering by continent:", continent))
    print(head(data_filtered))  # Show the first few rows of the filtered data
  }
  
  if (!is.null(species)) {
    data_filtered <- dplyr::filter(data_filtered, Species == species)
    print(paste("Filtering by species:", species))
    print(head(data_filtered))  # Show the first few rows of the filtered data
  }
  
  if (!is.null(antibiotic)) {
    data_filtered <- dplyr::filter(data_filtered, antibiotics == antibiotic)
    print(paste("Filtering by antibiotic:", antibiotic))
    print(head(data_filtered))  # Show the first few rows of the filtered data
  }
  
  # Step 2: Prepare data for plotting
  if (!is.null(antibiotic)) {
    plot_data <- data_filtered %>%
      dplyr::select("Isolate Id", "antibiotics", "RIS") %>%
      dplyr::group_by(RIS) %>%
      dplyr::summarize(count = n()) %>%
      dplyr::mutate(percentage = count / sum(count) * 100)
    
    # Create the plot for a specific antibiotic with percentage
    plot <- plot_data %>%
      ggplot(aes(x = RIS, y = percentage, fill = RIS)) +
      geom_bar(stat = "identity", position = "dodge", color = "black") +
      labs(
        title = paste("Percentage Frequency of RIS Interpretation for", 
                      antibiotic, 
                      if (!is.null(continent)) paste("in", continent) else "",
                      if (!is.null(species)) paste("for", species) else ""),
        x = "RIS Interpretation",
        y = "Percentage (%)"
      ) +
      theme_minimal() +
      theme(
        legend.position = "none",  # Remove legend for individual RIS interpretation plot
        axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis text
      )
    
  } else {
    plot_data <- data_filtered %>%
      dplyr::select("Isolate Id", "antibiotics", "RIS") %>%
      dplyr::group_by(antibiotics, RIS) %>%
      dplyr::summarize(count = n()) %>%
      dplyr::mutate(percentage = count / sum(count) * 100)
    
    # Create the plot for all antibiotics with stacking and percentage
    plot <- plot_data %>%
      ggplot(aes(x = antibiotics, y = percentage, fill = RIS)) +
      geom_bar(stat = "identity", position = "stack", color = "black") +
      labs(
        title = paste("Percentage Frequency of RIS Interpretation",
                      if (!is.null(continent)) paste("in", continent) else "",
                      if (!is.null(species)) paste("for", species) else ""),
        x = "Antibiotics",
        y = "Percentage (%)"
      ) +
      theme_minimal() +
      theme(
        legend.position = "bottom",  # Place legend at the bottom
        axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis text
      )
  }
  
  # Convert ggplot object to plotly for interactivity
  interactive_plot <- ggplotly(plot)
  
  # Update the title in the interactive plot to reflect the filters used
  title <- if (!is.null(antibiotic)) {
    paste("Percentage Frequency of RIS Interpretation for", antibiotic,
          if (!is.null(continent)) paste("in", continent) else "",
          if (!is.null(species)) paste("for", species) else "")
  } else {
    paste("Percentage Frequency of RIS Interpretation",
          if (!is.null(continent)) paste("in", continent) else "",
          if (!is.null(species)) paste("for", species) else "")
  }
  
  interactive_plot <- interactive_plot %>% layout(title = title)
  
  # Step 3: Return the interactive plot
  return(interactive_plot)
}



plot_RIS_normalised_4(RIS_pivot_long, continent = "Africa", species = "Escherichia coli", antibiotic = "Ciprofloxacin")
plot_RIS_normalised_4(RIS_pivot_long, continent = "Europe", species = "Staphylococcus aureus")
plot_RIS_normalised_4(RIS_pivot_long, species = "Klebsiella pneumoniae")






plot_resistance_2_normalised <- function(data, continent = NULL, country = NULL, resistance_threshold = 30) {
  # Verify the structure of the data
  if (!all(c("Species", "Continent", "Country", "RIS") %in% colnames(data))) {
    stop("Data must contain columns: Species, Continent, Country, and RIS.")
  }
  
  # Step 1: Filter data based on the provided continent and country arguments
  data_filtered <- data
  
  if (!is.null(continent) && !is.null(country)) {
    data_filtered <- data_filtered %>%
      filter(Continent == continent, Country == country)
    plot_title <- paste("Percentage Distribution of Resistance, Intermediate, and Susceptible in", country, "(", continent, ")")
  } else if (!is.null(continent)) {
    data_filtered <- data_filtered %>%
      filter(Continent == continent)
    plot_title <- paste("Percentage Distribution of Resistance, Intermediate, and Susceptible in", continent)
  } else if (!is.null(country)) {
    data_filtered <- data_filtered %>%
      filter(Country == country)
    plot_title <- paste("Percentage Distribution of Resistance, Intermediate, and Susceptible in", country)
  } else {
    plot_title <- "Percentage Distribution of Resistance, Intermediate, and Susceptible for All Data (Global)"
  }
  
  # Step 2: Prepare the data for plotting
  plot_data <- data_filtered %>%
    group_by(Species, RIS) %>%
    summarise(count = n(), .groups = 'drop') %>%
    group_by(Species) %>%
    mutate(percentage = count / sum(count) * 100) %>%
    ungroup()
  
  # Calculate the percentage of resistant organisms for each species
  resistant_percentages <- plot_data %>%
    filter(RIS == "Resistant") %>%
    select(Species, percentage)
  
  # Filter out species with less than the specified resistance threshold and order by decreasing resistance
  species_to_plot <- resistant_percentages %>%
    filter(percentage >= resistance_threshold) %>%
    arrange(desc(percentage)) %>%
    pull(Species)
  
  # Filter the plot data to include only the selected species
  plot_data_filtered <- plot_data %>%
    filter(Species %in% species_to_plot)
  
  # Check if the filtered data is empty
  if (nrow(plot_data_filtered) == 0) {
    stop("No data available for the specified filters or all species have less resistance than the threshold. Please try different parameters.")
  }
  
  # Step 3: Create the plot using ggplot2
  plot <- plot_data_filtered %>%
    ggplot(aes(x = factor(Species, levels = species_to_plot), y = percentage, fill = RIS)) +
    geom_bar(position = "stack", stat = "identity", color = "black") +
    geom_text(aes(label = count), position = position_stack(vjust = 0.5), size = 3) +  # Add count labels
    labs(
      title = plot_title,
      x = "Species",
      y = "Percentage"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 90, hjust = 1, size = 8)
    )
  
  # Step 4: Convert the ggplot object to a plotly interactive plot
  interactive_plot <- ggplotly(plot) %>%
    layout(hoverlabel = list(bgcolor = "white"))  # Adjust hover label background for readability
  
  # Return the interactive plot
  return(interactive_plot)
}





plot_top_ESKAPE <- function(data, continent = NULL, country = NULL) {
  
  # Specify the pathogens of interest
  pathogens <- c(
    "Enterococcus faecium", 
    "Staphylococcus aureus", 
    "Klebsiella pneumoniae", 
    "Acinetobacter baumannii", 
    "Pseudomonas aeruginosa", 
    "Enterobacter species"
  )
  
  # Filter data based on continent or country if specified
  if (!is.null(country)) {
    filtered_data <- data %>% filter(Country == country & Species %in% pathogens)
    plot_title <- paste("Infection burden of", country)
  } else if (!is.null(continent)) {
    filtered_data <- data %>% filter(Continent == continent & Species %in% pathogens)
    plot_title <- paste("Infection burden of", continent)
  } else {
    filtered_data <- data %>% filter(Species %in% pathogens)
    plot_title <- "Global burden of Infection"
  }
  
  # Step 1: Group by Isolate Id and Species, then count the number of resistant cases
  resistance_count <- filtered_data %>%
    group_by(`Isolate Id`, Species) %>%
    summarise(resistant_count = sum(RIS == "Resistant"), .groups = "drop")
  
  # Step 2: Group by Species, calculate the median of the resistant counts, 
  # and divide by the number of unique antibiotics for each species
  species_resistance <- resistance_count %>%
    group_by(Species) %>%
    summarise(
      median_resistance = median(resistant_count),
      unique_antibiotics = n_distinct(filtered_data$antibiotics),
      normalized_resistance = ((median_resistance / unique_antibiotics) * 100),
      .groups = "drop"
    ) %>%
    arrange(desc(normalized_resistance)) %>%
    top_n(20, normalized_resistance)
  
  # Check if there's data to plot
  if (nrow(species_resistance) == 0) {
    stop("No data available for the specified filters. Please try different parameters.")
  }
  
  # Step 3: Create the plot
  p <- species_resistance %>%
    ggplot(aes(x = reorder(Species, normalized_resistance), 
               y = normalized_resistance, 
               text = paste("Species:", Species, 
                            "<br>Normalized Resistance:", sprintf("%.2f", normalized_resistance),
                            "<br>Median Resistance:", median_resistance,
                            "<br>Unique Antibiotics:", unique_antibiotics,
                            "<br>Count:", median_resistance * unique_antibiotics))) +  # Include count in the tooltip
    geom_col(fill = "steelblue") +
    coord_flip() +  # Flip coordinates for horizontal bars
    labs(title = plot_title,
         x = "Species",
         y = "Normalized Resistance (Median Count / Unique Antibiotics)") +
    theme_minimal() +
    theme(axis.text.y = element_text(size = 8))
  
  # Convert to interactive plotly object
  interactive_plot <- ggplotly(p, tooltip = "text")
  
  return(interactive_plot)
}





plot_resistance_2_normalised <- function(data, continent = NULL, country = NULL, resistance_threshold = 30) {
  # Verify the structure of the data
  if (!all(c("Species", "Continent", "Country", "RIS") %in% colnames(data))) {
    stop("Data must contain columns: Species, Continent, Country, and RIS.")
  }
  
  # Step 1: Filter data based on the provided continent and country arguments
  data_filtered <- data
  
  if (!is.null(continent) && !is.null(country)) {
    data_filtered <- data_filtered %>%
      filter(Continent == continent, Country == country)
    plot_title <- paste("Percentage Distribution of Resistance, Intermediate, and Susceptible in", country, "(", continent, ")")
  } else if (!is.null(continent)) {
    data_filtered <- data_filtered %>%
      filter(Continent == continent)
    plot_title <- paste("Percentage Distribution of Resistance, Intermediate, and Susceptible in", continent)
  } else if (!is.null(country)) {
    data_filtered <- data_filtered %>%
      filter(Country == country)
    plot_title <- paste("Percentage Distribution of Resistance, Intermediate, and Susceptible in", country)
  } else {
    plot_title <- "Percentage Distribution of Resistance, Intermediate, and Susceptible for All Data (Global)"
  }
  
  # Step 2: Prepare the data for plotting
  plot_data <- data_filtered %>%
    group_by(Species, RIS) %>%
    summarise(count = n(), .groups = 'drop') %>%
    group_by(Species) %>%
    mutate(percentage = count / sum(count) * 100) %>%
    ungroup()
  
  # Calculate the percentage of resistant organisms for each species
  resistant_percentages <- plot_data %>%
    filter(RIS == "Resistant") %>%
    select(Species, percentage)
  
  # Filter out species with less than the specified resistance threshold and order by decreasing resistance
  species_to_plot <- resistant_percentages %>%
    filter(percentage >= resistance_threshold) %>%
    arrange(desc(percentage)) %>%
    pull(Species)
  
  # Filter the plot data to include only the selected species
  plot_data_filtered <- plot_data %>%
    filter(Species %in% species_to_plot)
  
  # Check if the filtered data is empty
  if (nrow(plot_data_filtered) == 0) {
    stop("No data available for the specified filters or all species have less resistance than the threshold. Please try different parameters.")
  }
  
  # Step 3: Create the plot using ggplot2
  plot <- plot_data_filtered %>%
    ggplot(aes(x = factor(Species, levels = species_to_plot), y = percentage, fill = RIS)) +
    geom_bar(position = "stack", stat = "identity", color = "black") +
    labs(
      title = plot_title,
      x = "Species",
      y = "Percentage"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 90, hjust = 1, size = 8)
    )
  
  # Convert the ggplot object to a plotly interactive plot with custom hover text
  interactive_plot <- ggplotly(plot, tooltip = c("x", "y", "fill")) %>%
    layout(hoverlabel = list(bgcolor = "white"))  # Adjust hover label background for readability
  
  return(interactive_plot)
}






plot_resistance_2_normalised <- function(data, continent = NULL, country = NULL, resistance_threshold = 30) {
  # Verify the structure of the data
  if (!all(c("Species", "Continent", "Country", "RIS") %in% colnames(data))) {
    stop("Data must contain columns: Species, Continent, Country, and RIS.")
  }
  
  # Step 1: Filter data based on the provided continent and country arguments
  data_filtered <- data
  
  if (!is.null(continent) && !is.null(country)) {
    data_filtered <- data_filtered %>%
      filter(Continent == continent, Country == country)
    plot_title <- paste("Percentage Distribution of Resistance, Intermediate, and Susceptible in", country, "(", continent, ")")
  } else if (!is.null(continent)) {
    data_filtered <- data_filtered %>%
      filter(Continent == continent)
    plot_title <- paste("Percentage Distribution of Resistance, Intermediate, and Susceptible in", continent)
  } else if (!is.null(country)) {
    data_filtered <- data_filtered %>%
      filter(Country == country)
    plot_title <- paste("Percentage Distribution of Resistance, Intermediate, and Susceptible in", country)
  } else {
    plot_title <- "Percentage Distribution of Resistance, Intermediate, and Susceptible for All Data (Global)"
  }
  
  # Step 2: Prepare the data for plotting
  plot_data <- data_filtered %>%
    group_by(Species, RIS) %>%
    summarise(count = n(), .groups = 'drop') %>%
    group_by(Species) %>%
    mutate(percentage = count / sum(count) * 100) %>%
    ungroup()
  
  # Calculate the percentage of resistant organisms for each species
  resistant_percentages <- plot_data %>%
    filter(RIS == "Resistant") %>%
    select(Species, percentage)
  
  # Filter out species with less than the specified resistance threshold and order by decreasing resistance
  species_to_plot <- resistant_percentages %>%
    filter(percentage >= resistance_threshold) %>%
    arrange(desc(percentage)) %>%
    pull(Species)
  
  # Filter the plot data to include only the selected species
  plot_data_filtered <- plot_data %>%
    filter(Species %in% species_to_plot)
  
  # Check if the filtered data is empty
  if (nrow(plot_data_filtered) == 0) {
    stop("No data available for the specified filters or all species have less resistance than the threshold. Please try different parameters.")
  }
  
  # Step 3: Create the plot using ggplot2
  plot <- plot_data_filtered %>%
    ggplot(aes(x = factor(Species, levels = species_to_plot), y = percentage, fill = RIS, 
               text = paste("Species:", Species, "<br>RIS:", RIS, "<br>Count:", count, "<br>Percentage:", round(percentage, 2), "%"))) +
    geom_bar(position = "stack", stat = "identity", color = "black") +
    labs(
      title = plot_title,
      x = "Species",
      y = "Percentage"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 90, hjust = 1, size = 8)
    )
  
  # Step 4: Convert the ggplot object to a plotly interactive plot with custom tooltip
  interactive_plot <- ggplotly(plot, tooltip = "text") %>%
    layout(hoverlabel = list(bgcolor = "white"))  # Adjust hover label background for readability
  
  return(interactive_plot)
}





plot_RIS_normalised_4 <- function(data, continent = NULL, country = NULL, species = NULL, antibiotic = NULL) {
  # Ensure the data is a dataframe
  if (!is.data.frame(data)) {
    stop("The input data must be a dataframe.")
  }
  
  # Step 1: Filter data based on the provided arguments
  data_filtered <- data
  
  if (!is.null(continent)) {
    data_filtered <- dplyr::filter(data_filtered, Continent == continent)
    print(paste("Filtering by continent:", continent))
    print(head(data_filtered))  # Show the first few rows of the filtered data
  }
  
  if (!is.null(country)) {
    data_filtered <- dplyr::filter(data_filtered, Country == country)
    print(paste("Filtering by country:", country))
    print(head(data_filtered))  # Show the first few rows of the filtered data
  }
  
  if (!is.null(species)) {
    data_filtered <- dplyr::filter(data_filtered, Species == species)
    print(paste("Filtering by species:", species))
    print(head(data_filtered))  # Show the first few rows of the filtered data
  }
  
  if (!is.null(antibiotic)) {
    data_filtered <- dplyr::filter(data_filtered, antibiotics == antibiotic)
    print(paste("Filtering by antibiotic:", antibiotic))
    print(head(data_filtered))  # Show the first few rows of the filtered data
  }
  
  # Step 2: Prepare data for plotting
  plot_data <- data_filtered %>%
    dplyr::select("Isolate Id", "antibiotics", "RIS") %>%
    dplyr::group_by(antibiotics, RIS) %>%
    dplyr::summarize(count = n(), .groups = 'drop') %>%
    dplyr::mutate(percentage = count / sum(count) * 100)
  
  # Step 3: Create the plot
  plot <- plot_data %>%
    ggplot(aes(x = antibiotics, y = percentage, fill = RIS)) +
    geom_bar(stat = "identity", position = "stack", color = "black") +
    labs(
      title = paste("Percentage Frequency of RIS Interpretation",
                    if (!is.null(continent)) paste("in", continent) else "",
                    if (!is.null(country)) paste("in", country) else "",
                    if (!is.null(species)) paste("for", species) else ""),
      x = "Antibiotics",
      y = "Percentage (%)"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",  # Place legend at the bottom
      axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis text
    )
  
  # Convert ggplot object to plotly for interactivity
  interactive_plot <- ggplotly(plot)
  
  # Step 4: Return the interactive plot
  return(interactive_plot)
}

plot_RIS_normalised_4(RIS_pivot_long, continent = "Asia")
plot_RIS_normalised_4(RIS_pivot_long, country = "Germany")
plot_RIS_normalised_4(RIS_pivot_long, species = "Escherichia coli")







plot_RIS_normalised_4 <- function(data, continent = NULL, species = NULL, antibiotic = NULL) {
  # Ensure the data is a dataframe
  if (!is.data.frame(data)) {
    stop("The input data must be a dataframe.")
  }
  
  # Step 1: Filter data based on the provided continent, species, and antibiotic arguments
  data_filtered <- data
  
  if (!is.null(continent)) {
    data_filtered <- dplyr::filter(data_filtered, Continent == continent)
  }
  
  if (!is.null(species)) {
    data_filtered <- dplyr::filter(data_filtered, Species == species)
  }
  
  if (!is.null(antibiotic)) {
    data_filtered <- dplyr::filter(data_filtered, antibiotics == antibiotic)
  }
  
  # Step 2: Prepare data for plotting
  if (!is.null(antibiotic)) {
    plot_data <- data_filtered %>%
      dplyr::select("Isolate Id", "antibiotics", "RIS") %>%
      dplyr::group_by(antibiotics, RIS) %>%
      dplyr::summarize(count = n()) %>%
      dplyr::mutate(percentage = count / sum(count) * 100)
    
    # Create the plot for a specific antibiotic with percentage
    plot <- plot_data %>%
      ggplot(aes(x = RIS, y = percentage, fill = RIS)) +
      geom_bar(stat = "identity", position = "dodge", color = "black") +
      labs(
        title = paste("Percentage Frequency of RIS Interpretation for", 
                      antibiotic, 
                      if (!is.null(continent)) paste("in", continent) else "",
                      if (!is.null(species)) paste("for", species) else ""),
        x = "RIS Interpretation",
        y = "Percentage (%)"
      ) +
      theme_minimal() +
      theme(
        legend.position = "none",  # Remove legend for individual RIS interpretation plot
        axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis text
      )
    
  } else {
    plot_data <- data_filtered %>%
      dplyr::select("Isolate Id", "antibiotics", "RIS") %>%
      dplyr::group_by(antibiotics, RIS) %>%
      dplyr::summarize(count = n()) %>%
      dplyr::group_by(antibiotics) %>%
      dplyr::mutate(percentage = count / sum(count) * 100)  # Calculate percentage using counts per antibiotic category
    
    # Create the plot for all antibiotics with stacking and percentage
    plot <- plot_data %>%
      ggplot(aes(x = antibiotics, y = percentage, fill = RIS)) +
      geom_bar(stat = "identity", position = "stack", color = "black") +
      labs(
        title = paste("Percentage Frequency of RIS Interpretation",
                      if (!is.null(continent)) paste("in", continent) else "",
                      if (!is.null(species)) paste("for", species) else ""),
        x = "Antibiotics",
        y = "Percentage (%)"
      ) +
      theme_minimal() +
      theme(
        legend.position = "bottom",  # Place legend at the bottom
        axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis text
      )
  }
  
  # Convert ggplot object to plotly for interactivity
  interactive_plot <- ggplotly(plot)
  
  # Update the title in the interactive plot to reflect the filters used
  title <- if (!is.null(antibiotic)) {
    paste("Percentage Frequency of RIS Interpretation for", antibiotic,
          if (!is.null(continent)) paste("in", continent) else "",
          if (!is.null(species)) paste("for", species) else "")
  } else {
    paste("Percentage Frequency of RIS Interpretation",
          if (!is.null(continent)) paste("in", continent) else "",
          if (!is.null(species)) paste("for", species) else "")
  }
  
  interactive_plot <- interactive_plot %>% layout(title = title)
  
  # Step 3: Return the interactive plot
  return(interactive_plot)
}






