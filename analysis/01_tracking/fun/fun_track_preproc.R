
#-----------------------------------------------------------------------------------------
# fun_track_preproc.R        Pre-process  tracking data
#-----------------------------------------------------------------------------------------

# This script gathers a series of functions to held during the pre-proceing metadata 
# and bio-logging data

# @jmenblaz / J. Menéndez-Blázquez

# functions

# 1 - sequeira_fields()
# 2 - preproc_fields_comp()



# ------------------------------------------------------------------------------
# 1 - sequeira_fields()

# This function return the name of the 92 variables for stadarization bio-logging
# database stablished by Sequeira et al., 2021 (fields names)
# https://github.com/ocean-tracking-network/biologging_standardization/tree/master/templates/fields


sequeira_fields <- function() {
  c("argosErrorRadius", "argosFilterMethod", "argosGDOP", "argosLC", 
    "argosOrientation", "argosSemiMajor", "argosSemiMinor", 
    "attachmentMethod", "axes", "calibrationsDone", "citation", 
    "commonName", "deploymentDateTime", "deploymentEndType", 
    "deploymentID", "deploymentLatitude", "deploymentLongitude", 
    "depthGLS", "detachmentDateTime", "detachmentDetails", 
    "detachmentLatitude", "detachmentLongitude", "dutyCycle", 
    "gpsSatelliteCount", "instrumentID", "instrumentManufacturer", 
    "instrumentModel", "instrumentSerialNumber", "instrumentSettings", 
    "instrumentType", "latitude", "license", "longitude", 
    "lowerSensorDetectionLimit", "organismAgeReproductiveClass", 
    "organismID", "organismIDSource", "organismSex", "organismSize", 
    "organismSizeMeasurementDescription", "organismSizeMeasurementTime", 
    "organismSizeMeasurementType", "organismWeightAtDeployment", 
    "organismWeightRemeasurement", "organismWeightRemeasurementTime", 
    "orientationOfAccelerometerOnOrganism", "otherDataCoowners", 
    "otherDataTypesAssociatedWithDeployment", "otherRelevantIdentifiers", 
    "ownerEmailContact", "ownerInstitutionalContact", "ownerName", 
    "ownerPhoneContact", "positionOfAccelerometerOnOrganism", "ptt", 
    "qcDone", "qcNotes", "qcProblemsFound", "references", 
    "residualsGPS", "resolution", "scientificName", "scientificNameSource", 
    "sensorCalibrationDate", "sensorCalibrationDetails", 
    "sensorDetectionLimits", "sensorDutyCycling", "sensorIMeasurement", 
    "sensorIType", "sensorManufacturer", "sensorModel", "sensorPrecision", 
    "sensorSamplingFrequency", "sensorType", "sunElevationAngle", 
    "temperatureGLS", "time", "trackEndLatitude", "trackEndLongitude", 
    "trackEndTime", "trackStartLatitude", "trackStartLongitude", 
    "trackStartTime", "trackingDevice", "transmissionMode", 
    "transmissionSettings", "trappingMethodDetails", 
    "unitOfAltitudeDepth", "unitsReported", "uplinkInterval", 
    "uplinkIntervalUnits", "upperSensorDetectionLimit")
}

# Example of use:

  # sequeira_fields <- sequeira_fields()



# -----------------------------------------------------------------------------
# 2 - preproc_fields_comp()

#' @param dataframe metadata dataframe

preproc_fields_comp <- function(dataframe) {
  # field names
  dataframe_columns <- colnames(dataframe)
  
  # extacr from Sequeira et al., 2021
  preproc_var <- c(
    "organismID", "scientificName", "codeName", 
    "deploymentDateTime", "deploymentLongitude", "deploymentLatitude", "deploymentSite",
    "instrumentType", "instrumentModel", "dutyCycle", "ptt",
    "organismAgeReproductiveClass", "organismSex",
    "organismSize1", "organismSizeMeasurementType1",
    "organismSize2", "organismSizeMeasurementType2",
    "ownerName", "ownerEmailContact", "ownerInstitution", "ownerInstitutionAbbrev"
  )
  
  # identify fields which are no named as Sequereia et al., 2021
  extra_fields <- setdiff(dataframe_columns, preproc_var)
  
  return(extra_fields)
}


