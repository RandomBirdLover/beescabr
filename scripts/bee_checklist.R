# SD Native Bee Checklist
# Created: June 11, 2026
# Data: iNaturalist export, San Diego County, all years, all quality grades

# Run once to install, then leave commented:
# install.packages("tidyverse")
library(tidyverse)

# Load data
bees <- read.csv("data/reference_exports/bees/SD_native_bees_11_june_2026.csv")

# Build checklist of unique taxa
bee_checklist <- bees %>%
  # Select all taxonomic columns
  select(
    taxon_id,
    scientific_name,
    common_name,
    taxon_kingdom_name,
    taxon_phylum_name,
    taxon_class_name,
    taxon_order_name,
    taxon_superfamily_name,
    taxon_family_name,
    taxon_subfamily_name,
    taxon_tribe_name,
    taxon_subtribe_name,
    taxon_genus_name,
    taxon_species_name,
    taxon_subspecies_name
  ) %>%
  # Remove duplicates
  distinct(taxon_id, .keep_all = TRUE) %>%
  # Remove rows with no taxonomic info
  filter(!is.na(taxon_genus_name) | !is.na(taxon_species_name)) %>%
  # Sort by family then genus then species
  arrange(taxon_family_name, taxon_genus_name, taxon_species_name)

# Preview
head(bee_checklist)
nrow(bee_checklist)
