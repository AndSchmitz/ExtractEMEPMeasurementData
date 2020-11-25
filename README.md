# ParseEBASData
This script parses ".nas" files from the EMEP measurement data portal (http://ebas.nilu.no/) and brings them in a format easier to handle (csv table).

Respect the EMEP/EBAS data policy.

Data comes with one or mulitple quality flags (https://projects.nilu.no//ccc/flags/).
If any of the flags is not of category "V" (valid measurement), the correspondung value(s) are discarded.

Todo:
 - Add dummy data
