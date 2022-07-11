* NOTE: You need to set the Stata working directory to the path
* where the data file is located.

set more off

clear
quietly infix             ///
  int     year     1-4    ///
  double  serial   5-12   ///
  int     pernum   13-16  ///
  byte    sploc    17-18  ///
  byte    relate   19-20  ///
  int     related  21-24  ///
  byte    sex      25-25  ///
  byte    marst    26-26  ///
  using `"workingset.dat"'


format serial  %8.0f

label var year    `"Census year"'
label var serial  `"Household serial number"'
label var pernum  `"Person number in sample unit"'
label var sploc   `"Spouse's location in household"'
label var relate  `"Relationship to household head [general version]"'
label var related `"Relationship to household head [detailed version]"'
label var sex     `"Sex"'
label var marst   `"Marital status"'

label define year_lbl 1850 `"1850"'
label define year_lbl 1860 `"1860"', add
label define year_lbl 1870 `"1870"', add
label define year_lbl 1880 `"1880"', add
label define year_lbl 1900 `"1900"', add
label define year_lbl 1910 `"1910"', add
label define year_lbl 1920 `"1920"', add
label define year_lbl 1930 `"1930"', add
label define year_lbl 1940 `"1940"', add
label define year_lbl 1950 `"1950"', add
label define year_lbl 1960 `"1960"', add
label define year_lbl 1970 `"1970"', add
label define year_lbl 1980 `"1980"', add
label define year_lbl 1990 `"1990"', add
label define year_lbl 2000 `"2000"', add
label define year_lbl 2001 `"2001"', add
label define year_lbl 2002 `"2002"', add
label define year_lbl 2003 `"2003"', add
label define year_lbl 2004 `"2004"', add
label define year_lbl 2005 `"2005"', add
label define year_lbl 2006 `"2006"', add
label define year_lbl 2007 `"2007"', add
label define year_lbl 2008 `"2008"', add
label define year_lbl 2009 `"2009"', add
label define year_lbl 2010 `"2010"', add
label define year_lbl 2011 `"2011"', add
label define year_lbl 2012 `"2012"', add
label define year_lbl 2013 `"2013"', add
label define year_lbl 2014 `"2014"', add
label define year_lbl 2015 `"2015"', add
label define year_lbl 2016 `"2016"', add
label define year_lbl 2017 `"2017"', add
label define year_lbl 2018 `"2018"', add
label define year_lbl 2019 `"2019"', add
label define year_lbl 2020 `"2020"', add
label values year year_lbl

label define relate_lbl 01 `"Head/Householder"'
label define relate_lbl 02 `"Spouse"', add
label define relate_lbl 03 `"Child"', add
label define relate_lbl 04 `"Child-in-law"', add
label define relate_lbl 05 `"Parent"', add
label define relate_lbl 06 `"Parent-in-Law"', add
label define relate_lbl 07 `"Sibling"', add
label define relate_lbl 08 `"Sibling-in-Law"', add
label define relate_lbl 09 `"Grandchild"', add
label define relate_lbl 10 `"Other relatives"', add
label define relate_lbl 11 `"Partner, friend, visitor"', add
label define relate_lbl 12 `"Other non-relatives"', add
label define relate_lbl 13 `"Institutional inmates"', add
label values relate relate_lbl

label define related_lbl 0101 `"Head/Householder"'
label define related_lbl 0201 `"Spouse"', add
label define related_lbl 0202 `"2nd/3rd Wife (Polygamous)"', add
label define related_lbl 0301 `"Child"', add
label define related_lbl 0302 `"Adopted Child"', add
label define related_lbl 0303 `"Stepchild"', add
label define related_lbl 0304 `"Adopted, n.s."', add
label define related_lbl 0401 `"Child-in-law"', add
label define related_lbl 0402 `"Step Child-in-law"', add
label define related_lbl 0501 `"Parent"', add
label define related_lbl 0502 `"Stepparent"', add
label define related_lbl 0601 `"Parent-in-Law"', add
label define related_lbl 0602 `"Stepparent-in-law"', add
label define related_lbl 0701 `"Sibling"', add
label define related_lbl 0702 `"Step/Half/Adopted Sibling"', add
label define related_lbl 0801 `"Sibling-in-Law"', add
label define related_lbl 0802 `"Step/Half Sibling-in-law"', add
label define related_lbl 0901 `"Grandchild"', add
label define related_lbl 0902 `"Adopted Grandchild"', add
label define related_lbl 0903 `"Step Grandchild"', add
label define related_lbl 0904 `"Grandchild-in-law"', add
label define related_lbl 1000 `"Other relatives:"', add
label define related_lbl 1001 `"Other Relatives"', add
label define related_lbl 1011 `"Grandparent"', add
label define related_lbl 1012 `"Step Grandparent"', add
label define related_lbl 1013 `"Grandparent-in-law"', add
label define related_lbl 1021 `"Aunt or Uncle"', add
label define related_lbl 1022 `"Aunt,Uncle-in-law"', add
label define related_lbl 1031 `"Nephew, Niece"', add
label define related_lbl 1032 `"Neph/Niece-in-law"', add
label define related_lbl 1033 `"Step/Adopted Nephew/Niece"', add
label define related_lbl 1034 `"Grand Niece/Nephew"', add
label define related_lbl 1041 `"Cousin"', add
label define related_lbl 1042 `"Cousin-in-law"', add
label define related_lbl 1051 `"Great Grandchild"', add
label define related_lbl 1061 `"Other relatives, nec"', add
label define related_lbl 1100 `"Partner, Friend, Visitor"', add
label define related_lbl 1110 `"Partner/friend"', add
label define related_lbl 1111 `"Friend"', add
label define related_lbl 1112 `"Partner"', add
label define related_lbl 1113 `"Partner/roommate"', add
label define related_lbl 1114 `"Unmarried Partner"', add
label define related_lbl 1115 `"Housemate/Roomate"', add
label define related_lbl 1120 `"Relative of partner"', add
label define related_lbl 1130 `"Concubine/Mistress"', add
label define related_lbl 1131 `"Visitor"', add
label define related_lbl 1132 `"Companion and family of companion"', add
label define related_lbl 1139 `"Allocated partner/friend/visitor"', add
label define related_lbl 1200 `"Other non-relatives"', add
label define related_lbl 1201 `"Roomers/boarders/lodgers"', add
label define related_lbl 1202 `"Boarders"', add
label define related_lbl 1203 `"Lodgers"', add
label define related_lbl 1204 `"Roomer"', add
label define related_lbl 1205 `"Tenant"', add
label define related_lbl 1206 `"Foster child"', add
label define related_lbl 1210 `"Employees:"', add
label define related_lbl 1211 `"Servant"', add
label define related_lbl 1212 `"Housekeeper"', add
label define related_lbl 1213 `"Maid"', add
label define related_lbl 1214 `"Cook"', add
label define related_lbl 1215 `"Nurse"', add
label define related_lbl 1216 `"Other probable domestic employee"', add
label define related_lbl 1217 `"Other employee"', add
label define related_lbl 1219 `"Relative of employee"', add
label define related_lbl 1221 `"Military"', add
label define related_lbl 1222 `"Students"', add
label define related_lbl 1223 `"Members of religious orders"', add
label define related_lbl 1230 `"Other non-relatives"', add
label define related_lbl 1239 `"Allocated other non-relative"', add
label define related_lbl 1240 `"Roomers/boarders/lodgers and foster children"', add
label define related_lbl 1241 `"Roomers/boarders/lodgers"', add
label define related_lbl 1242 `"Foster children"', add
label define related_lbl 1250 `"Employees"', add
label define related_lbl 1251 `"Domestic employees"', add
label define related_lbl 1252 `"Non-domestic employees"', add
label define related_lbl 1253 `"Relative of employee"', add
label define related_lbl 1260 `"Other non-relatives (1990 includes employees)"', add
label define related_lbl 1270 `"Non-inmate 1990"', add
label define related_lbl 1281 `"Head of group quarters"', add
label define related_lbl 1282 `"Employees of group quarters"', add
label define related_lbl 1283 `"Relative of head, staff, or employee group quarters"', add
label define related_lbl 1284 `"Other non-inmate 1940-1959"', add
label define related_lbl 1291 `"Military"', add
label define related_lbl 1292 `"College dormitories"', add
label define related_lbl 1293 `"Residents of rooming houses"', add
label define related_lbl 1294 `"Other non-inmate 1980 (includes employees and non-inmates in"', add
label define related_lbl 1295 `"Other non-inmates 1960-1970 (includes employees)"', add
label define related_lbl 1296 `"Non-inmates in institutions"', add
label define related_lbl 1301 `"Institutional inmates"', add
label define related_lbl 9996 `"Unclassifiable"', add
label define related_lbl 9997 `"Unknown"', add
label define related_lbl 9998 `"Illegible"', add
label define related_lbl 9999 `"Missing"', add
label values related related_lbl

label define sex_lbl 1 `"Male"'
label define sex_lbl 2 `"Female"', add
label values sex sex_lbl

label define marst_lbl 1 `"Married, spouse present"'
label define marst_lbl 2 `"Married, spouse absent"', add
label define marst_lbl 3 `"Separated"', add
label define marst_lbl 4 `"Divorced"', add
label define marst_lbl 5 `"Widowed"', add
label define marst_lbl 6 `"Never married/single"', add
label values marst marst_lbl


