/*
   NOTE: You need to edit the `libname` command to specify the path to the directory
   where the data file is located. For example: "C:\ipums_directory".
   Edit the `filename` command similarly to include the full path (the directory and the data file name).
*/

libname IPUMS ".";
filename ASCIIDAT "workingset.dat";

proc format cntlout = IPUMS.workingset_f;

value YEAR_f
  1850 = "1850"
  1860 = "1860"
  1870 = "1870"
  1880 = "1880"
  1900 = "1900"
  1910 = "1910"
  1920 = "1920"
  1930 = "1930"
  1940 = "1940"
  1950 = "1950"
  1960 = "1960"
  1970 = "1970"
  1980 = "1980"
  1990 = "1990"
  2000 = "2000"
  2001 = "2001"
  2002 = "2002"
  2003 = "2003"
  2004 = "2004"
  2005 = "2005"
  2006 = "2006"
  2007 = "2007"
  2008 = "2008"
  2009 = "2009"
  2010 = "2010"
  2011 = "2011"
  2012 = "2012"
  2013 = "2013"
  2014 = "2014"
  2015 = "2015"
  2016 = "2016"
  2017 = "2017"
  2018 = "2018"
  2019 = "2019"
  2020 = "2020"
;

value RELATE_f
  01 = "Head/Householder"
  02 = "Spouse"
  03 = "Child"
  04 = "Child-in-law"
  05 = "Parent"
  06 = "Parent-in-Law"
  07 = "Sibling"
  08 = "Sibling-in-Law"
  09 = "Grandchild"
  10 = "Other relatives"
  11 = "Partner, friend, visitor"
  12 = "Other non-relatives"
  13 = "Institutional inmates"
;

value RELATED_f
  0101 = "Head/Householder"
  0201 = "Spouse"
  0202 = "2nd/3rd Wife (Polygamous)"
  0301 = "Child"
  0302 = "Adopted Child"
  0303 = "Stepchild"
  0304 = "Adopted, n.s."
  0401 = "Child-in-law"
  0402 = "Step Child-in-law"
  0501 = "Parent"
  0502 = "Stepparent"
  0601 = "Parent-in-Law"
  0602 = "Stepparent-in-law"
  0701 = "Sibling"
  0702 = "Step/Half/Adopted Sibling"
  0801 = "Sibling-in-Law"
  0802 = "Step/Half Sibling-in-law"
  0901 = "Grandchild"
  0902 = "Adopted Grandchild"
  0903 = "Step Grandchild"
  0904 = "Grandchild-in-law"
  1000 = "Other relatives:"
  1001 = "Other Relatives"
  1011 = "Grandparent"
  1012 = "Step Grandparent"
  1013 = "Grandparent-in-law"
  1021 = "Aunt or Uncle"
  1022 = "Aunt,Uncle-in-law"
  1031 = "Nephew, Niece"
  1032 = "Neph/Niece-in-law"
  1033 = "Step/Adopted Nephew/Niece"
  1034 = "Grand Niece/Nephew"
  1041 = "Cousin"
  1042 = "Cousin-in-law"
  1051 = "Great Grandchild"
  1061 = "Other relatives, nec"
  1100 = "Partner, Friend, Visitor"
  1110 = "Partner/friend"
  1111 = "Friend"
  1112 = "Partner"
  1113 = "Partner/roommate"
  1114 = "Unmarried Partner"
  1115 = "Housemate/Roomate"
  1120 = "Relative of partner"
  1130 = "Concubine/Mistress"
  1131 = "Visitor"
  1132 = "Companion and family of companion"
  1139 = "Allocated partner/friend/visitor"
  1200 = "Other non-relatives"
  1201 = "Roomers/boarders/lodgers"
  1202 = "Boarders"
  1203 = "Lodgers"
  1204 = "Roomer"
  1205 = "Tenant"
  1206 = "Foster child"
  1210 = "Employees:"
  1211 = "Servant"
  1212 = "Housekeeper"
  1213 = "Maid"
  1214 = "Cook"
  1215 = "Nurse"
  1216 = "Other probable domestic employee"
  1217 = "Other employee"
  1219 = "Relative of employee"
  1221 = "Military"
  1222 = "Students"
  1223 = "Members of religious orders"
  1230 = "Other non-relatives"
  1239 = "Allocated other non-relative"
  1240 = "Roomers/boarders/lodgers and foster children"
  1241 = "Roomers/boarders/lodgers"
  1242 = "Foster children"
  1250 = "Employees"
  1251 = "Domestic employees"
  1252 = "Non-domestic employees"
  1253 = "Relative of employee"
  1260 = "Other non-relatives (1990 includes employees)"
  1270 = "Non-inmate 1990"
  1281 = "Head of group quarters"
  1282 = "Employees of group quarters"
  1283 = "Relative of head, staff, or employee group quarters"
  1284 = "Other non-inmate 1940-1959"
  1291 = "Military"
  1292 = "College dormitories"
  1293 = "Residents of rooming houses"
  1294 = "Other non-inmate 1980 (includes employees and non-inmates in"
  1295 = "Other non-inmates 1960-1970 (includes employees)"
  1296 = "Non-inmates in institutions"
  1301 = "Institutional inmates"
  9996 = "Unclassifiable"
  9997 = "Unknown"
  9998 = "Illegible"
  9999 = "Missing"
;

value SEX_f
  1 = "Male"
  2 = "Female"
;

value MARST_f
  1 = "Married, spouse present"
  2 = "Married, spouse absent"
  3 = "Separated"
  4 = "Divorced"
  5 = "Widowed"
  6 = "Never married/single"
;

run;

data IPUMS.workingset;
infile ASCIIDAT pad missover lrecl=26;

input
  YEAR      1-4
  SERIAL    5-12
  PERNUM    13-16
  SPLOC     17-18
  RELATE    19-20
  RELATED   21-24
  SEX       25-25
  MARST     26-26
;

label
  YEAR    = "Census year"
  SERIAL  = "Household serial number"
  PERNUM  = "Person number in sample unit"
  SPLOC   = "Spouse's location in household"
  RELATE  = "Relationship to household head [general version]"
  RELATED = "Relationship to household head [detailed version]"
  SEX     = "Sex"
  MARST   = "Marital status"
;

format
  YEAR     YEAR_f.
  RELATE   RELATE_f.
  RELATED  RELATED_f.
  SEX      SEX_f.
  MARST    MARST_f.
;

run;

