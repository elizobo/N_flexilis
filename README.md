# N_flexilis
### Data and code for statistics and plot generation for dissertation work

Determining viability and germination success of *Najas flexilis* (AKA Slender Naiad, or Nodding Waternymph) seeds of different seed coat colours and in autoclaved and non autoclaved loch sediment. 
The key questions are:
1. Is N. flexilis seed colour an indicator of viability and germination? 
2. Does autoclaving sediment impact N. flexilis seed germination success?
3. How does autoclaving growth sediment alter the germination and growing environment of N. flexilis?



### data
All csv files for data used in the project

##### TTC seed coat stain tests
  - [**Seed colour viability**](data/colvi.csv) : aquatic chemistry from sediment microcosms after 264 hours for each microcosm (DO, DOC, TDN, DIC, SRP, TP, N/P ratio (Nprat))
  - [**Seed colour viability long format**](data/colvi_long.csv) : aquatic chemistry from sediment microcosms after 264 hours for each microcosm (DO, DOC, TDN, DIC, SRP, TP, N/P ratio (Nprat))

##### Germinations trials and seedling success
   - [**Germination successs**](data/seed_germ.csv) : Germination success and seedling shoot length, root length and leaf number
 

##### Inclubated sediment microcosm experiment
  - [**aquatic chemistry**](data/end_chem.csv) : aquatic chemistry from sediment microcosms after 264 hours for each microcosm (DO, DOC, TDN, DIC, SRP, TP, N/P ratio (Nprat))
  - [**average end aquatic chemistry**](data/end_chemo.csv) : average aquatic chemistry for each sediment microcosm treatlent after 264 hours (DO, DOC, TDN, DIC, SRP, TP, N/P ratio (Nprat))
  - [**aquatic chemiistry log**](data/water_log.csv) : aquatic chemistry data for 0, 1, 24, 48, 100, 168 and 264 hours (Conductivity (cond), pH, DO, DOC, TDN, DIC, SRP, TP))
  - [**aquatic metals log**](data/metals.csv) : aquatic metals concentration data for 1, 100, and 264 hours (V, Cr, Mn, Fe, Co, Ni, Cu, Zn, As, Mg)
  - [**avergae end aquatic metals**](data/metals.csv) : average metal concentrations in sediment michrocosms after 264 hours (V, Cr, Mn, Fe, Co, Ni, Cu, Zn, As, Mg)

### scripts
R scripts for data manipulation, statistical analyses and figure generation
  - [**TTC stain, germination and aquatic chemistry script**](data/metals.csv) : processing for seed colour viability, germination success and aquatic chemistry data
  - [**metals analysis script**](data/metals.csv) : processing for aquatic metals data

