#  ExtractEMEPMeasurementData

This script parses ".nas" files from the EMEP measurement data portal (http://ebas.nilu.no/) and brings them in a format easier to handle (CSV). Respect the EMEP/EBAS data policy. The EBAS data format is based on the NASA Ames file format specification (https://projects.nilu.no/ccc/tfmm/kjeller_2016/EBAS_Data_Format_2016-10.pdf). This R script creates two CSV files, which can be linked via column "FileID":

 - *Parsed_EMEP_Metadata.csv* with metadata per input file: "StationName", "code_plot", "lat", "lon", "altitude", "component", "matrix", "unit", "InstrumentType", "FileID", "TimeStampFirstMeasurement", "TimeStampLastMeasurement", "FileName"
 
 - *Parsed_EMEP_Data.csv* the actual data: "TimeStampStart", "TimeStampEnd", "value", "substance", "comment", "FileID"


## Caveats
EBAS data files can include data from different "matrices", e.g. first column PM10 data, second column PM2.5 data or from multiple substances, e.g. first column Na, second column Cl. This script cannot safely handle files with data from multiple matrices or substances. Make sure to select only one matrix (pm1, wetdep, etc.) and one substance/parameter in the EMEP measurement data portal when downloading data to be processed with this script. 
 
 
## Data quality flags
Each row in the original data (.nas files) has a quality flag (https://projects.nilu.no//ccc/flags/). In general, rows with quality flags indicating non-valid measurements (i.e. quality flag not in category "V") are deleted. Additional quality flag numbers can be specified which should also cause deletion of the respective measurement value. For example:

  - 781, 	#V 	Value below detection limit, data element contains detection limit
  - 780, 	#V 	Value below detection or quantification limit, data element contains estimated or measured value. Use of flag 147 is encouraged.
  - 771, 	#V 	Value above range, data element contains upper range limit
  - 770 	#V 	Value above range, data element contains estimated value

In addition, the following quality flags indicate that concentrations were not correctly measured due to too little precipitation amount. These data rows are not deleted. Instead, the concentration is set to NA and value of the comment field is set to "LowPrecipVolume".

 - 784	I	Low precipitation, concentration estimated
 - 783	M	Low precipitation, concentration unknown
 - 782	V	Low precipitation, concentration estimated
 - 890	M	Concentration in precipitation undefined, no precipitation

