#Title: Script to fetch fixtures data from premiershiprugby.com and enrich

## Load Packages
if (!require("rvest"))
    install.packages("rvest")
if (!require("xlsx"))
    install.packages("xlsx")

library(rvest)
library(xlsx)

## Load functions and variables
substrRight <- function(x, n) {
    substr(x, nchar(x) - n + 1, nchar(x))
}

### Function: Fetch Team roster for Fixture
fetchTeamData <- function(x, pt, n) {

    tempData <- NULL

    tempURL <- paste0("http://www.premiershiprugby.com/matchcentre/fixtures/", x, ".php")
    teamPage <- read_html(tempURL)

    if (pt == "Report | Highlights") {

        tab <- n

        if (tab == 1) {

            tempData <- teamPage %>%
                        html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "team hometeam", " " ))]') %>%
                         html_nodes("table") %>%
                         .[[1]] %>%
                         html_table()
        } else {

            tempData <- teamPage %>%
                        html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "team awayteam", " " ))]') %>%
                         html_nodes("table") %>%
                         .[[1]] %>%
                         html_table()
        }

        if (nrow(tempData) == 0) {

            tempData <- data.frame(matrix(ncol = 23))
        } else {

            tempData <- data.frame(t(tempData[, 2]), stringsAsFactors = FALSE)
        }

        colnames(tempData) <- c("P15", "P14", "P13", "P12", "P11", "P10", "P9", "P1", "P2", "P3", "P4", "P5", "P6", "P7", "P8", "P16", "P17", "P18", "P19", "P20", "P21", "P22", "P23")
        tempData$ID <- x

    } else if (pt == "Preview | Highlights") {

        tempData <- teamPage %>%
                    html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "first15", " " ))]') %>%
                    html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "name", " " ))]') %>%
                    html_text()

        tempData <- data.frame(as.list(tempData), stringsAsFactors = FALSE)

        if (n == 1) {
            tempData <- (tempData[2:24])
        }

        if (n == 2) {
            tempData <- (tempData[25:47])
        }

        colnames(tempData) <- c("P15", "P14", "P13", "P12", "P11", "P10", "P9", "P1", "P2", "P3", "P4", "P5", "P6", "P7", "P8", "P16", "P17", "P18", "P19", "P20", "P21", "P22", "P23")
        tempData$ID <- x

    } else {
        return <- NULL
    }

    return <- tempData


}

###################################################################################
# Main Function
###################################################################################

downloadSeason <- function(filename = "PremiershipRugby", from = 2015, to = 2016) {

    fromSeason <- from
    toSeason <- to

    # Fetch fixtures data from webpage
    fixURL <- paste("http://www.premiershiprugby.com/matchcentre/fixtures/index.php?includeref=9229&season=", as.character(fromSeason), "-", as.character(toSeason), sep = "")
    ## Read page
    page <- read_html(fixURL)
    ## Find HTML table
    data <- page %>%
    html_nodes("table") %>%
    .[[3]] %>%
    html_table()

    ## Set column names
    colnames(data) <- c("LongDate", "ShortDate", "Time", "Home", "HomeShort", "Score", "Away", "AwayShort", "Venue", "TV_Att", "Preview_Report")
    ## Remove empty rows from data
    data <- data[complete.cases(data),]

    # Fetch Team data for fixtures
    ## Find HTML table and extract onclick values
    subData <- page %>%
        html_nodes("table") %>%
        .[[3]] %>%
        html_nodes(xpath = "//*[@data-fixid]") %>%
        html_attr("onclick")
    ## Extract ID from onclick value
    subData <- substr(subData, 60, 64)
    ## Merge ID to data
    data <- cbind(data, subData)
    ## For each record, fetch home and away team data
    for (i in 1:nrow(data)) {

        if (data[i, "subData"] != "match" & data[i, "Preview_Report"] != "Preview" & !is.na(data[i, "subData"])) {

            homeTeamData <- fetchTeamData(data[i, "subData"], data[i, "Preview_Report"], 1)
            awayTeamData <- fetchTeamData(data[i, "subData"], data[i, "Preview_Report"], 2)

            data$Home1[data$subData == homeTeamData$ID] <- homeTeamData$P1
            data$Home2[data$subData == homeTeamData$ID] <- homeTeamData$P2
            data$Home3[data$subData == homeTeamData$ID] <- homeTeamData$P3
            data$Home4[data$subData == homeTeamData$ID] <- homeTeamData$P4
            data$Home5[data$subData == homeTeamData$ID] <- homeTeamData$P5
            data$Home6[data$subData == homeTeamData$ID] <- homeTeamData$P6
            data$Home7[data$subData == homeTeamData$ID] <- homeTeamData$P7
            data$Home8[data$subData == homeTeamData$ID] <- homeTeamData$P8
            data$Home9[data$subData == homeTeamData$ID] <- homeTeamData$P9
            data$Home10[data$subData == homeTeamData$ID] <- homeTeamData$P10
            data$Home11[data$subData == homeTeamData$ID] <- homeTeamData$P11
            data$Home12[data$subData == homeTeamData$ID] <- homeTeamData$P12
            data$Home13[data$subData == homeTeamData$ID] <- homeTeamData$P13
            data$Home14[data$subData == homeTeamData$ID] <- homeTeamData$P14
            data$Home15[data$subData == homeTeamData$ID] <- homeTeamData$P15
            data$Home16[data$subData == homeTeamData$ID] <- homeTeamData$P16
            data$Home17[data$subData == homeTeamData$ID] <- homeTeamData$P17
            data$Home18[data$subData == homeTeamData$ID] <- homeTeamData$P18
            data$Home19[data$subData == homeTeamData$ID] <- homeTeamData$P19
            data$Home20[data$subData == homeTeamData$ID] <- homeTeamData$P20
            data$Home21[data$subData == homeTeamData$ID] <- homeTeamData$P21
            data$Home22[data$subData == homeTeamData$ID] <- homeTeamData$P22
            data$Home23[data$subData == homeTeamData$ID] <- homeTeamData$P23

            data$Away1[data$subData == awayTeamData$ID] <- awayTeamData$P1
            data$Away2[data$subData == awayTeamData$ID] <- awayTeamData$P2
            data$Away3[data$subData == awayTeamData$ID] <- awayTeamData$P3
            data$Away4[data$subData == awayTeamData$ID] <- awayTeamData$P4
            data$Away5[data$subData == awayTeamData$ID] <- awayTeamData$P5
            data$Away6[data$subData == awayTeamData$ID] <- awayTeamData$P6
            data$Away7[data$subData == awayTeamData$ID] <- awayTeamData$P7
            data$Away8[data$subData == awayTeamData$ID] <- awayTeamData$P8
            data$Away9[data$subData == awayTeamData$ID] <- awayTeamData$P9
            data$Away10[data$subData == awayTeamData$ID] <- awayTeamData$P10
            data$Away11[data$subData == awayTeamData$ID] <- awayTeamData$P11
            data$Away12[data$subData == awayTeamData$ID] <- awayTeamData$P12
            data$Away13[data$subData == awayTeamData$ID] <- awayTeamData$P13
            data$Away14[data$subData == awayTeamData$ID] <- awayTeamData$P14
            data$Away15[data$subData == awayTeamData$ID] <- awayTeamData$P15
            data$Away16[data$subData == awayTeamData$ID] <- awayTeamData$P16
            data$Away17[data$subData == awayTeamData$ID] <- awayTeamData$P17
            data$Away18[data$subData == awayTeamData$ID] <- awayTeamData$P18
            data$Away19[data$subData == awayTeamData$ID] <- awayTeamData$P19
            data$Away20[data$subData == awayTeamData$ID] <- awayTeamData$P20
            data$Away21[data$subData == awayTeamData$ID] <- awayTeamData$P21
            data$Away22[data$subData == awayTeamData$ID] <- awayTeamData$P22
            data$Away23[data$subData == awayTeamData$ID] <- awayTeamData$P23

        }
    }

    ## Add to other data
    fixturesData <- data

    # Tidy Fixtures Data
    ## Split match score between Home and Away
    fixturesData$HomeScore <- as.numeric(lapply(strsplit(as.character(fixturesData$Score), split = " - "), "[", 1))
    fixturesData$AwayScore <- as.numeric(lapply(strsplit(as.character(fixturesData$Score), split = "-"), "[", 2))
    ## Determine Home, Away or Draw and set as factor
    fixturesData$Result <- ifelse(fixturesData$HomeScore > fixturesData$AwayScore, "Home", ifelse(fixturesData$HomeScore < fixturesData$AwayScore, "Away", "Draw"))
    fixturesData$Result <- as.factor(fixturesData$Result)
    ## Create formatted date
    fixturesData$Date <- as.Date(as.character(paste(fixturesData$ShortDate, substrRight(fixturesData$LongDate, 2))), format = "%d %b %y")
    ## Clean fixtures data
    fixturesData$Venue[fixturesData$Venue == "Twickenham"] <- "Twickenham Stadium"
    fixturesData$Venue[fixturesData$Venue == "Salford City Stadium"] <- "AJ Bell Stadium"

    # Fetch stadium data from webpage
    url <- "https://en.wikipedia.org/wiki/List_of_stadiums_in_England"
    ## Read page
    page <- read_html(url)
    ## Find HTML table
    venueData <- page %>%
        html_nodes("table") %>%
        .[[2]] %>%
        html_table()

    # Tidy Venue data
    venueData <- venueData[2:3]
    venueData$Stadium[venueData$Stadium == "Salford City Stadium"] <- "AJ Bell Stadium"
    venueData$Stadium[venueData$Stadium == "Kingsholm Stadium"] <- "Kingsholm"
    venueData$Stadium[venueData$Stadium == "Sixways Stadium"] <- "Sixways"
    venueData$Stadium[venueData$Stadium == "Stadium:mk"] <- "stadiummk"

    # Merge fixtures and venue data
    finalData <- merge(fixturesData, venueData, by.x = "Venue", by.y = "Stadium", all.x = TRUE)
    ## Update missing stadium locations
    finalData$Location[finalData$Venue == "Red Bull Arena"] <- "Harrison, New Jersey"


    # Final Data Cleaning
    ## Remove incomplete rows (e.g. Semi Finals with unknown Teams)
    finalData <- finalData[complete.cases(finalData),]

    # Output options
    dataFilename <- paste(filename, as.character(fromSeason), " - ", as.character(toSeason), ".xls", sep = "")
    write.xlsx2(finalData, dataFilename)
}