
ged <- readLines(test_path("maximal70.ged"))

# Fix NOTE in header
move <- ged[4:12]
ged <- append(ged, move, 49)
ged <- ged[-(4:12)]

# Fix CHR.AGE in @I1@
move <- ged[387:388]
ged <- append(ged, move, 385) 
ged <- ged[-(389:390)]

writeLines(ged, test_path("maximal70-fixed.ged"))

