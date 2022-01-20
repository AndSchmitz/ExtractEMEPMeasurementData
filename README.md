#  ExtractEMEPMeasurementData

This script parses ".nas" files from the EMEP measurement data portal (http://ebas.nilu.no/) and brings them in a format easier to handle (CSV). The EBAS data format is based on the [NASA Ames file format specification](https://projects.nilu.no/ccc/tfmm/kjeller_2016/EBAS_Data_Format_2016-10.pdf). This R script creates two CSV files, which can be linked via column "FileID":

 - *Parsed_EMEP_Metadata.csv* with metadata per input file: "StationName", "code_plot", "lat", "lon", "altitude", "component", "matrix", "unit", "InstrumentType", "FileID", "TimeStampFirstMeasurement", "TimeStampLastMeasurement", "FileName"
 
 - *Parsed_EMEP_Data.csv* the actual data: "TimeStampStart", "TimeStampEnd", "value", "substance", "comment", "flag", "FileID"

Note that different data usage policies exist for data from different networks in the EBAS portal.

## Caveats
EBAS data files can include data from different "matrices", e.g. first column PM10 data, second column PM2.5 data or from multiple substances, e.g. first column Na, second column Cl. This script cannot safely handle files with data from multiple matrices or substances. Make sure to select only one matrix (pm1, wetdep, etc.) and one substance/parameter in the EBAS data portal when downloading data to be processed with this script. 
 
 
## Data quality flags
Each row in the original data (.nas files) has a data quality flag (https://projects.nilu.no//ccc/flags/). This script does filter data based on flags. Instead, flags are reported in column "flag" in the output file. Note that even after only including data marked as "valid" (flag type "V"), some values are still
99.999.., 999.9999, 9.99, 9-99999, 9999.9 or similar, indicating missing or invalid data (according to [NASA Ames file format specification](https://projects.nilu.no/ccc/tfmm/kjeller_2016/EBAS_Data_Format_2016-10.pdf)). I.e. for data analyses, filtering based on data flags and based on repeated 9's might be reasonable.
