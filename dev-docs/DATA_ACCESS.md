# How to Download This Project and Its Data

This guide explains what is in this repository, what is kept separate, and how to
get everything you need. It is written to be simple, including for people who do
not use R or GitHub.

---

## 1. Just want to see the results?

You do not need to download anything. All of the maps, figures, and tables are
published as a website:

**https://randombirdlover.github.io/beescabr/**

If you only want to view or share the findings, that link is all you need.

---

## 2. What is (and is not) in this repository

When you download this repository, you get:

- **The code** (the `scripts/` folder) that cleans the data and builds the figures,
  maps, and website.
- **The website** (the `docs/` folder) that is shown at the link above.
- **Documentation**, including this file and the README.

You do **not** get the data. Everything in the `data/` folder is deliberately kept
off of GitHub, for three reasons:

1. It contains people's names and email addresses (the survey and identification
   teams), which should not be published.
2. It contains private API keys (passwords for the iNaturalist and IUCN services),
   which must stay secret.
3. Some of it is large, and some of it the program rebuilds on its own, so there is
   no reason to store it online.

So downloading the repository gives you the code and the website, but not the
underlying data. The data is shared separately (see Section 4).

---

## 3. Downloading the code

You have two options.

**Option A: download a ZIP (no software needed).**

1. Go to https://github.com/RandomBirdLover/beescabr
2. Click the green **Code** button.
3. Click **Download ZIP**.
4. Unzip it on your computer.

**Option B: use git (if you already have it).**

```
git clone https://github.com/RandomBirdLover/beescabr
```

Either way, you now have a folder called `beescabr` with the code and the website
inside it.

---

## 4. Getting the data

Because the data is not on GitHub, you get it directly from the project lead. It
will come to you as a `data/` folder (for example on a shared drive, cloud folder,
or USB drive).

To use it, place that `data/` folder **inside** your `beescabr` folder, so the
layout looks like this:

```
beescabr/
  scripts/
  docs/
  data/      <-- the folder you were given goes here
```

Inside `data/` you will find all of the real inputs and results:

- `data/specimens/` — the netted-bee (specimen) records, including the master spreadsheet.
- `data/inat_observations/` — the iNaturalist (photographed-bee) records and cleaned tables.
- `data/project_info/` — the survey logs and the team rosters.
- `data/spatial/shapefiles/` — the park boundary and transect maps.
- `data/checklists/`, `data/reference/`, `data/analysis/` — species lists, lookups, and finished figures.

If all you needed was the data and the finished results, you are done here.

---

## 5. Running the analysis yourself (optional, more technical)

Only follow this section if you want to re-run the analysis and rebuild everything
from scratch. You will need R.

1. **Install R** (free) from https://cran.r-project.org (RStudio is a helpful
   optional add-on).

2. **Install the packages** the code uses. Open R and run:

   ```r
   install.packages(c(
     "DBI","duckdb","dplyr","ggplot2","ggridges","ggspatial","httr2","iNEXT",
     "igraph","jsonlite","leaflet","lubridate","pdftools","purrr","readr",
     "readxl","sf","stringr","tibble","tidyr","vegan"
   ))
   ```

   Note: the `sf` and `ggspatial` mapping packages sometimes need extra system
   libraries (GDAL, GEOS, PROJ). If they fail to install, see the `sf` install
   instructions at https://r-spatial.github.io/sf/.

3. **Add the two API keys.** The program needs two small key files:

   ```
   data/secrets/inat_api.env     (iNaturalist)
   data/secrets/iucn_api.env     (IUCN Red List)
   ```

   The project lead can provide these, or you can use your own free keys from
   iNaturalist and the IUCN Red List.

4. **Run the data pipeline.** Open a terminal in the `beescabr` folder and run:

   ```
   source("scripts/run_data_cleaning_pipeline.R")
   ```

   To skip the online iNaturalist download and just reuse the data you were given,
   run this instead:

   ```
   Sys.setenv(BEESCABR_SKIP_INGEST = "1"); source("scripts/run_data_cleaning_pipeline.R")
   ```

5. **Rebuild the figures and the website (optional):**

   ```
   source("scripts/run_all_analysis_pipeline.R")
   source("scripts/run_publishing_materials_pipeline.R")
   ```

---

## 6. Important: what must never be posted publicly

If you ever pass this project along, keep these two things out of anything public:

- **The API keys** in `data/secrets/` (they are passwords).
- **The team rosters** with people's names and email addresses in
  `data/project_info/rosters/`.

The repository is already set up to keep the entire `data/` folder off of GitHub,
so as long as you do not upload or commit that folder, you are safe.

---

## 7. Questions

Contact either:

- **Brandi Sanchez** — Conservation Legacy, Scientists in Parks, Cabrillo National
  Monument. Built the pipeline; best for questions about how the code or the
  cleaned data works.
- **Taro Katayama** — National Park Service, Cabrillo National Monument. Holds the
  data and can share the `data/` folder directly.

Either can send you the `data/` folder; it is shared person to person, not through
this repository.

