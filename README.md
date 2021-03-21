# ParseEBASData

This script parses ".nas" files from the EMEP measurement data portal (http://ebas.nilu.no/) and brings them in a format easier to handle (CSV). The EBAS data format is based on the NASA Ames file format specification (https://projects.nilu.no/ccc/tfmm/kjeller_2016/EBAS_Data_Format_2016-10.pdf). This R script creates two CSV files, which can be linked via column "FileID":

 - *Parsed_EMEP_Metadata.csv* with metadata per input file: "StationName", "code_plot", "lat", "lon", "altitude", "component", "matrix", "unit", "InstrumentType", "FileID", "TimeStampFirstMeasurement", "TimeStampLastMeasurement", "FileName"
 
 - *Parsed_EMEP_Data.csv* the actual data: "TimeStampStart", "TimeStampEnd", "value", "substance", "comment", "FileID"
 
Each row in the original data (.nas files) has a quality flag (https://projects.nilu.no//ccc/flags/). Rows with quality flags indicating non-valid measurements (i.e. quality flag not in category "V") are deleted, with four exceptions: The following quality flags indicate that concentrations were not correctly measured due to too little precipitation amount. These data rows are not deleted. Instead, the concentration is set to NA and value of the comment field is set to "LowPrecipVolume".

 - 784	I	Low precipitation, concentration estimated
 - 783	M	Low precipitation, concentration unknown
 - 782	V	Low precipitation, concentration estimated
 - 890	M	Concentration in precipitation undefined, no precipitation


Respect the EMEP/EBAS data policy.



Todo:
 - Add dummy data
