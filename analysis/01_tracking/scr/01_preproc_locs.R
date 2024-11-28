#-----------------------------------------------------------------------------------------
# 01_preproc_CAR.R        Pre-process loggerhead tracking data
#-----------------------------------------------------------------------------------------

# This script pre-processes tracking data. The main goal is to standardize among multiple
# formats (tag manufacturers, custom pre-processing from different labs) and generate a
# common and standardized format to then follow a common workflow

# Common workflow based in Sequeira et al., 2021
# A standardisation framework for bio‐logging data to advance ecological research and conservation
# Stadarization made it by J.Menéndez-Blázquez

# Input data is found as Argos raw data per individual (= ptt) in "loc" folder
# example of input folder structure
# (number are OrganismID == ptt (for seaturtles, etc))

# ~input/tracking/loc/loc/34319
#                        /34321
#                        /222021





# tracking functions paths
funs <- list.files("analysis/01_tracking/fun/", pattern = "\\.R$", full.names = TRUE)
# read .R scripts with source
sapply(funs, source)





#-------------------------------------------------------------------------------
# 1. Set data repository

input_data <- paste0(input_dir, "/tracking/loc/loc")
output_data <- paste0(input_dir, "/tracking/loc/L0/")   # L0 as input for further analysis
if (!dir.exists(output_data)) dir.create(output_data, recursive = TRUE)



#-------------------------------------------------------------------------------
# 2. Process deployment metadata

# Use Spatial Marine sea turtle database (TODB) .xlsx
# Note: raw Argos position store together database for the first step of processing
# import metadata

# Note: metadata should be contains the field names using by Sequeira et al., 2021
# https://github.com/ocean-tracking-network/biologging_standardization/tree/master/templates/fields

metadata <- list.files(input_data, pattern = ".xlsx", full.names = TRUE)
metadata <- read.xlsx(metadata)  # sheet 1 = "metadata"

# check names of metada
colnames(metadata)

# custom function to compare metadata fields name with Sequeira et al, 2021
# fields name interested for pre-processing data

fields <- preproc_fields_comp(metadata)
fields  # 0 = Requieres fields are in the metadata database with correct name
if (length(fields) > 0) print("Rename fields in metadata should be necessary for further step")


# prepare deployment metadata
metadata <- metadata %>%
  # rename variables and change to character
  mutate(codeName = getSpeciesCode(scientificName),
         organismID = ptt) %>%
  # recode variables
  dplyr::mutate(organismSex = recode(organismSex, Male = "male", Female = "female")
  ) %>%
  
  # add metadata info
  # mutate(datasetID, codeName, ownerInstitutionAbbrev) %>%
  
  # select variables
  dplyr::select(organismID, scientificName, codeName, 
                deploymentDateTime, deploymentLongitude, deploymentLatitude, deploymentSite,
                instrumentType, instrumentModel, dutyCycle, ptt,
                organismAgeReproductiveClass, organismSex,
                organismSize1,organismSizeMeasurementType1,
                organismSize2,organismSizeMeasurementType2,
                ownerName, ownerEmailContact, ownerInstitution, ownerInstitutionAbbrev)



#-------------------------------------------------------------------------------
# 3. Process tracking data

# Note: One folder per individual - WidlifeComputers for example


# Process delayed mode (location data)

t <- Sys.time()

# folder name == OrganismID or ptt (for seaturtle, seals, etc)
ids <- basename(list.dirs((input_data), recursive = FALSE))

for (i in 1:length(ids)){
  
  print(paste("Processing tag", i, "of", length(ids)))
  
  ## get tag information
  id <- ids[i]
  print(paste("id:",id))
  id_meta <- metadata %>% filter(organismID == id)
  
  # Import Argos data
  loc_file <- list.files(input_data, recursive=TRUE, full.names=TRUE, pattern = sprintf("%s-Locations.csv", id))
  data <- read.csv(loc_file)
  # Standardize data
  # Use animalsensor package D. March ()
  #' Warning: some issues with @param local of the wcloc2L0 function 
  #           - avoid this parameter for the function 
  dataL0 <- animalsensor::wcLoc2L0(data, date_deploy = id_meta$deploymentDateTime) 
  
  # note: outpu_data dir for L0 previously created
  # export data
  outfile <- sprintf(paste0(output_data, "%s_L0_loc.csv"), id)
  write.csv(dataL0, outfile, row.names = F)
  
  # plot track
  # using map_argos function() see fun_track_plot.R
  p <- map_argos(dataL0)
  out_file <- paste0(output_data, "/", id, "_L0_loc.png")
  ggsave(out_file, p, width=30, height=15, units = "cm")
  
}

cat("L0-locs processing ready \n")
Sys.time() - t # 5 min apróx
Sys.sleep(2)

#---------------------------------------------------------------
# 4. Summarize processed data
#---------------------------------------------------------------

loc_files <- list.files(output_data, full.names = TRUE, pattern = "L0_loc.csv") # OrganismID processed


# filter metadata (LO) by the OrganismID processed previosly

# This is done in case the original database contains many more tagged individuals 
# than those processed. For example, to differentiate work with 2D/3D biologging scope

# filter data frame by a series of ids
metadata <- metadata %>%
  filter(organismID %in% ids)

# Export filtered L0 metadata 
write.csv(metadata, paste0(output_data, "/metadataL0.csv"), row.names=F)


# read all location files processed
df <- readTrack(loc_files)

# summarize data per animal id
idstats <- summarizeId(df)

# export table
out_file <- paste0(output_data, "/","L0_summary_organismID.csv")
write.csv(idstats, out_file, row.names = FALSE)

# ----------------------------------------------------------------
Sys.time() - t
cat(" -- Pre-processing finished")

















































