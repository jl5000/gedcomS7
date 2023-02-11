# Extract line components

    {
      "type": "integer",
      "attributes": {},
      "value": [0, 1, 2, 2, 3, 1, 1, 2, 2, 2, 3, 4, 3, 1, 2, 1, 1, 1, 0, 1, 1, 2, 2, 2, 2, 2, 1, 0, 1, 2, 2, 1, 1, 2, 2, 2, 3, 1, 2, 2, 1, 2, 1, 1, 1, 2, 0, 1, 2, 2, 1, 1, 2, 2, 1, 0, 1, 2, 2, 1, 1, 2, 2, 1, 1, 2, 1, 2, 0, 1, 1, 1, 1, 2, 2, 0, 1, 1, 0, 1, 2, 3, 3, 2, 1, 1, 1, 2, 0, 1, 1, 2, 2, 2, 2, 2, 0]
    }

---

    {
      "type": "character",
      "attributes": {},
      "value": ["", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "@U1@", "", "", "", "", "", "", "", "", "@I1@", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "@I2@", "", "", "", "", "", "", "", "", "@I3@", "", "", "", "", "", "", "", "", "", "", "", "", "@F1@", "", "", "", "", "", "", "@F2@", "", "", "@S1@", "", "", "", "", "", "", "", "", "", "@R1@", "", "", "", "", "", "", "", ""]
    }

---

    {
      "type": "character",
      "attributes": {},
      "value": ["HEAD", "GEDC", "VERS", "FORM", "VERS", "CHAR", "SOUR", "NAME", "VERS", "CORP", "ADDR", "CITY", "WWW", "DATE", "TIME", "FILE", "LANG", "SUBM", "SUBM", "NAME", "ADDR", "ADR1", "CITY", "STAE", "POST", "CTRY", "PHON", "INDI", "NAME", "SURN", "GIVN", "SEX", "BIRT", "DATE", "PLAC", "SOUR", "PAGE", "DEAT", "DATE", "PLAC", "BURI", "PLAC", "FAMS", "FAMS", "RESI", "DATE", "INDI", "NAME", "SURN", "GIVN", "SEX", "BIRT", "DATE", "PLAC", "FAMS", "INDI", "NAME", "SURN", "GIVN", "SEX", "BIRT", "DATE", "PLAC", "FAMC", "FAMC", "PEDI", "ADOP", "DATE", "FAM", "HUSB", "WIFE", "CHIL", "MARR", "DATE", "PLAC", "FAM", "HUSB", "CHIL", "SOUR", "DATA", "EVEN", "DATE", "PLAC", "AGNC", "TITL", "ABBR", "REPO", "CALN", "REPO", "NAME", "ADDR", "ADR1", "CITY", "STAE", "POST", "CTRY", "TRLR"]
    }

---

    {
      "type": "character",
      "attributes": {},
      "value": ["", "", "5.5.5", "LINEAGE-LINKED", "5.5.5", "UTF-8", "GS", "GEDCOM Specification", "5.5.5", "gedcom.org", "", "LEIDEN", "www.gedcom.org", "2 Oct 2019", "0:00:00", "555Sample.ged", "English", "@U1@", "", "Reldon Poulson", "", "1900 43rd Street West", "Billings", "Montana", "68051", "United States of America", "+1 (406) 555-1232", "", "Robert Eugene /Williams/", "Williams", "Robert Eugene", "M", "", "2 Oct 1822", "Weston, Madison, Connecticut, United States of America", "@S1@", "Sec. 2, p. 45", "", "14 Apr 1905", "Stamford, Fairfield, Connecticut, United States of America", "", "Spring Hill Cemetery, Stamford, Fairfield, Connecticut, United States of America", "@F1@", "@F2@", "", "from 1900 to 1905", "", "Mary Ann /Wilson/", "Wilson", "Mary Ann", "F", "", "BEF 1828", "Connecticut, United States of America", "@F1@", "", "Joe /Williams/", "Williams", "Joe", "M", "", "11 Jun 1861", "Idaho Falls, Bonneville, Idaho, United States of America", "@F1@", "@F2@", "adopted", "", "16 Mar 1864", "", "@I1@", "@I2@", "@I3@", "", "Dec 1859", "Rapid City, Pennington, South Dakota, United States of America", "", "@I1@", "@I3@", "", "", "BIRT, DEAT, MARR", "FROM Jan 1820 TO DEC 1825", "Madison, Connecticut, United States of America", "Madison County Court", "Madison County Birth, Death, and Marriage Records", "Madison BMD Records", "@R1@", "13B-1234.01", "", "Family History Library", "", "35 N West Temple Street", "Salt Lake City", "Utah", "84150", "United States of America", ""]
    }

# delete_ged_section

    {
      "type": "character",
      "attributes": {},
      "value": ["0 HEAD", "1 CHAR UTF-8", "1 SOUR gedcom.org", "0 @U@ SUBM", "1 NAME gedcom.org", "0 TRLR"]
    }

---

    {
      "type": "character",
      "attributes": {},
      "value": ["0 HEAD", "1 GEDC", "2 VERS 5.5.5", "1 CHAR UTF-8", "1 SOUR gedcom.org", "0 @U@ SUBM", "1 NAME gedcom.org", "0 TRLR"]
    }

---

    {
      "type": "character",
      "attributes": {},
      "value": ["0 HEAD", "1 GEDC", "2 VERS 5.5.5", "2 FORM LINEAGE-LINKED", "3 VERS 5.5.5", "1 SOUR gedcom.org", "0 @U@ SUBM", "1 NAME gedcom.org", "0 TRLR"]
    }

---

    {
      "type": "character",
      "attributes": {},
      "value": ["0 HEAD", "1 GEDC", "2 VERS 5.5.5", "2 FORM LINEAGE-LINKED", "3 VERS 5.5.5", "1 CHAR UTF-8", "1 SOUR gedcom.org", "0 @U@ SUBM", "1 NAME gedcom.org"]
    }

# Extraction functions

    {
      "type": "character",
      "attributes": {},
      "value": ["0 CHAN", "1 DATE 23 APR 2001", "2 TIME 12:24:45", "1 NOTE @N1@", "1 NOTE Note for change date"]
    }

---

    {
      "type": "character",
      "attributes": {},
      "value": ["0 BIRT", "1 DATE 2 OCT 1822", "1 PLAC Weston, Madison, Connecticut, United States of America", "1 SOUR @S1@", "2 PAGE Sec. 2, p. 45", "0 DEAT", "1 DATE 14 APR 1905", "1 PLAC Stamford, Fairfield, Connecticut, United States of America", "0 BURI", "1 PLAC Spring Hill Cemetery, Stamford, Fairfield, Connecticut, United States of America", "0 RESI", "1 DATE FROM 1900 TO 1905"]
    }

---

    {
      "type": "character",
      "attributes": {},
      "value": ["0 NAME Robert Eugene /Williams/", "1 GIVN Robert Eugene", "1 SURN Williams"]
    }

# Increase level

    {
      "type": "character",
      "attributes": {},
      "value": ["2 HEAD", "3 GEDC", "4 VERS 5.5.5", "4 FORM LINEAGE-LINKED", "5 VERS 5.5.5", "3 CHAR UTF-8", "3 SOUR GS", "4 NAME GEDCOM Specification", "4 VERS 5.5.5", "4 CORP gedcom.org", "5 ADDR", "6 CITY LEIDEN", "5 WWW www.gedcom.org", "3 DATE 2 Oct 2019", "4 TIME 0:00:00", "3 FILE 555Sample.ged", "3 LANG English", "3 SUBM @U1@", "2 @U1@ SUBM", "3 NAME Reldon Poulson", "3 ADDR ", "4 ADR1 1900 43rd Street West", "4 CITY Billings", "4 STAE Montana", "4 POST 68051", "4 CTRY United States of America", "3 PHON +1 (406) 555-1232", "2 @I1@ INDI", "3 NAME Robert Eugene /Williams/", "4 SURN Williams", "4 GIVN Robert Eugene", "3 SEX M", "3 BIRT", "4 DATE 2 Oct 1822", "4 PLAC Weston, Madison, Connecticut, United States of America", "4 SOUR @S1@", "5 PAGE Sec. 2, p. 45", "3 DEAT", "4 DATE 14 Apr 1905", "4 PLAC Stamford, Fairfield, Connecticut, United States of America", "3 BURI", "4 PLAC Spring Hill Cemetery, Stamford, Fairfield, Connecticut, United States of America", "3 FAMS @F1@", "3 FAMS @F2@", "3 RESI ", "4 DATE from 1900 to 1905", "2 @I2@ INDI", "3 NAME Mary Ann /Wilson/", "4 SURN Wilson", "4 GIVN Mary Ann", "3 SEX F", "3 BIRT", "4 DATE BEF 1828", "4 PLAC Connecticut, United States of America", "3 FAMS @F1@", "2 @I3@ INDI", "3 NAME Joe /Williams/", "4 SURN Williams", "4 GIVN Joe", "3 SEX M", "3 BIRT", "4 DATE 11 Jun 1861", "4 PLAC Idaho Falls, Bonneville, Idaho, United States of America", "3 FAMC @F1@", "3 FAMC @F2@", "4 PEDI adopted", "3 ADOP ", "4 DATE 16 Mar 1864", "2 @F1@ FAM", "3 HUSB @I1@", "3 WIFE @I2@", "3 CHIL @I3@", "3 MARR", "4 DATE Dec 1859", "4 PLAC Rapid City, Pennington, South Dakota, United States of America", "2 @F2@ FAM", "3 HUSB @I1@", "3 CHIL @I3@", "2 @S1@ SOUR", "3 DATA", "4 EVEN BIRT, DEAT, MARR", "5 DATE FROM Jan 1820 TO DEC 1825", "5 PLAC Madison, Connecticut, United States of America", "4 AGNC Madison County Court", "3 TITL Madison County Birth, Death, and Marriage Records", "3 ABBR Madison BMD Records", "3 REPO @R1@", "4 CALN 13B-1234.01", "2 @R1@ REPO", "3 NAME Family History Library", "3 ADDR", "4 ADR1 35 N West Temple Street", "4 CITY Salt Lake City", "4 STAE Utah", "4 POST 84150", "4 CTRY United States of America", "2 TRLR"]
    }

