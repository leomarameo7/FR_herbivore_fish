**** Title: Consumer-dependent functional response of a herbivorous reef fish in a field experiment ****
 1. Authors: 
 - Leonardo Capitani [Corresponding author: leonardocapitani@icloud.com],
       Affiliation: Post Graduate Program in Ecology, Bioscience Institute, Universidade Federal do Rio Grande do Norte, Natal, 59072-970, Brazil
       
 - Natalia Roos
       Affiliation: Department of Oceanography and Limnology, Universidade Federal do Rio Grande do Norte, Natal, RN, 59014-002, Brazil
       
 - Ronaldo Angelini
       Affiliation: Department of Civil Engineering, Universidade Federal do Rio Grande do Norte, Natal, 59078-970, Brazil
    
 - Guilherme Ortigara Longo 
      Affiliation: Department of Oceanography and Limnology, Universidade Federal do Rio Grande do Norte, Natal, 59014-002, Brazil
      
  - Luca Schenone
       Affiliation: Limnology laboratory, INIBIOMA, UNComahue-CONICET, Quintral 1250, 8400, Bariloche, Argentina

2. Brief description of the project: 

We provided comprehensive empirical comparisons of alternative functional response models for the herbivorous fish *Acanthurus chirurgus* (Bloch, 1787) and the red seaweed Digenea simplex ((Wulfen & Agardh, 1822) as its resource in the Rocas Atoll, a pristine South Atlantic reef ecosystem. 

3. Notes: how to use ? Retriewed from https://kdestasio.github.io/post/r_best_practices/

Structure of the project as follow: 
Atoll_Rocas_project/ (It is the top-level folder and contains all of the folders                         and files associated with that project.)
|--- R (in this folder, you can find R scripts used to clean, manipulate, do and show along the entire research)

|--- data (folder with data used in this project)
     |--- raw (folder with raw data: only read with R, never modify these files)
     |--- processed (folder with data that is processed , clean and manipulated by R                      scripts)

|--- output (folder with all the results genereted by analysis of the data)
     |---figures (contains any plots or figures created and saved by the R scripts. It should be possible to delete and regenerate this folder with the scripts in the project folder)
     |--- tables (same as above for  tables)
     |--- supp (folder with supplementary materials)
|---doc (folder with .PDF and .docx files, genereted by R Markdown framework)
README.md

4. Files meaningful names
Each file has a short, descriptive name that indicates its purpose. Names  include only letters and numbers with underscores _ to separate words. Files that are part of the same work flow are numbered to indicate the order in which they are to be run. We use letters as as follow: 
C = stats for CLEAN DATA 
D = do the statistical analysis 
S = show analysis results 
F = function 
raw = states for files that contains raw data
processed = states for files contains data that were cleaned or manipulated 

Exemple:

01_C_load_biomass_traits.R

02_C_merge_biomass_traits_data.R

5. How to run the analysis with R  

Start with the 
1) "01_clean_raw_data.R" script.  This file clean raw data data.

2) "02_D_fitting_functional_response_models.R"
Fit the observed data to FUNCTIONAL RESPONSE MODELS with package brms    

3)"03_D_Table_2_ELPD_LFOCV_compare_models.R"
compare models fit with ELPD and LFOCV criteria    

4) "04_S_Fig_3.R" 
Figure 3: plot observed and fitted predictions for the hyperbolic P2 model 
     

The rest of the scripts just do Table S1 and plot some others figures for comparison purposes. You can find these scripts in the folder "supplementary_materials"

