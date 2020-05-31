governors <- spData::us_states[,c("NAME", "geometry")]
governors <- governors[which(governors$NAME != "District of Columbia"),]
governors <- governors[order(governors$NAME),]
governors$abbreviation <- c("AL", "AZ", "AR", "CA", "CO", "CT", "DE", "FL",
                            "GA", "ID", "IL", "IN", "IA", "KS", "KY", "LA",
                            "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT",
                            "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND",
                            "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN",
                            "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")
governors <- governors[,c("geometry", "abbreviation")]
governors <- sf::st_transform(governors, 3857)
governors$party <- c("R","R", "R", "D", "D", "D", "D", "R","R", "R", "D", "R",
                     "R", "D", "D", "D", "D", "R", "R", "D", "D", "R", "R",
                     "D", "R", "D", "R", "D", "D", "D", "D", "R","R", "R",
                     "D", "D", "D", "R","R", "R", "R","R", "R", "D", "D", "R",
                     "D", "R")
governors$party <- ifelse(governors$party == "R", "Republican", "Democrat")
governors$party <- factor(governors$party, c("Republican", "Democrat"))

usethis::use_data(governors, overwrite = TRUE)
