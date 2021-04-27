---
title: "Flooding Leads to the Most Economic Damage, while Excessive Heat Leads to Most Deaths "
author: "Kathryn Fahnline"
date: "4/24/2021"
output: 
  html_document:
    keep_md: true
---

## Synopsis

I grew up close to Johnstown, PA--the site of one of the greatest floods in the US^[https://www.jaha.org/attractions/johnstown-flood-museum/flood-history/]--so I was not surprised that flooding leads to the most economic damage. Nor was I particularly surprised to learn that drought led to the most crop damage (refer to Table 1).^[This assignment required *at most* 3 figures.  After doing some research, I decided that tables are generally NOT considered to be figures.]  What I did find surprising is that excessive heat causes the most deaths. Tornadoes led to a greater number of combined injuries (refer to Table 3).  Figures 2 and 3 reveal that there is quite a bit of variation in which events have the greatest impact on human health by year -- in 1998 there were a large number of fatalities due to excessive heat, while in 2011 there were a very large number of fatalities/injuries caused by tornadoes.  

On a regional level, flooding in the West led to the most economic damage, while drought in the several regions of the country-- the Northeast, South, and West--led to the most crop damage (refer to Table 2). People were more likely to die in the South from a tornado, but more likely to die from excessive heat in the Midwest or Northeast (refer to Table 4).     

##  I.  Data Processing

There were two options for loading the data: unzipping the file or reading directly from the zipped file.  I decided to read directly from the zipped file.


```r
library(downloader)
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
download.file(fileUrl, destfile = "noaa.csv.bz2", method="curl")
data <- read.csv("noaa.csv.bz2")
```

I wanted to break the analysis down by region, so I also downloaded a csv file with state codes created by KJ Healy. [fipscodes](https://github.com/kjhealy/fips-codes)^[In order to download a csv file from github, you need to click on the link for raw data.]


```r
fileUrl <- "https://raw.githubusercontent.com/kjhealy/fips-codes/master/state_fips_master.csv"
download.file(fileUrl, destfile = "state_codes.csv", method="curl")
state_codes <- read.csv("state_codes.csv")
```

#### Check out data and delete unnecessary columns


```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
str(data)
```

```
## 'data.frame':	902297 obs. of  37 variables:
##  $ STATE__   : num  1 1 1 1 1 1 1 1 1 1 ...
##  $ BGN_DATE  : chr  "4/18/1950 0:00:00" "4/18/1950 0:00:00" "2/20/1951 0:00:00" "6/8/1951 0:00:00" ...
##  $ BGN_TIME  : chr  "0130" "0145" "1600" "0900" ...
##  $ TIME_ZONE : chr  "CST" "CST" "CST" "CST" ...
##  $ COUNTY    : num  97 3 57 89 43 77 9 123 125 57 ...
##  $ COUNTYNAME: chr  "MOBILE" "BALDWIN" "FAYETTE" "MADISON" ...
##  $ STATE     : chr  "AL" "AL" "AL" "AL" ...
##  $ EVTYPE    : chr  "TORNADO" "TORNADO" "TORNADO" "TORNADO" ...
##  $ BGN_RANGE : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ BGN_AZI   : chr  "" "" "" "" ...
##  $ BGN_LOCATI: chr  "" "" "" "" ...
##  $ END_DATE  : chr  "" "" "" "" ...
##  $ END_TIME  : chr  "" "" "" "" ...
##  $ COUNTY_END: num  0 0 0 0 0 0 0 0 0 0 ...
##  $ COUNTYENDN: logi  NA NA NA NA NA NA ...
##  $ END_RANGE : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ END_AZI   : chr  "" "" "" "" ...
##  $ END_LOCATI: chr  "" "" "" "" ...
##  $ LENGTH    : num  14 2 0.1 0 0 1.5 1.5 0 3.3 2.3 ...
##  $ WIDTH     : num  100 150 123 100 150 177 33 33 100 100 ...
##  $ F         : int  3 2 2 2 2 2 2 1 3 3 ...
##  $ MAG       : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ FATALITIES: num  0 0 0 0 0 0 0 0 1 0 ...
##  $ INJURIES  : num  15 0 2 2 2 6 1 0 14 0 ...
##  $ PROPDMG   : num  25 2.5 25 2.5 2.5 2.5 2.5 2.5 25 25 ...
##  $ PROPDMGEXP: chr  "K" "K" "K" "K" ...
##  $ CROPDMG   : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ CROPDMGEXP: chr  "" "" "" "" ...
##  $ WFO       : chr  "" "" "" "" ...
##  $ STATEOFFIC: chr  "" "" "" "" ...
##  $ ZONENAMES : chr  "" "" "" "" ...
##  $ LATITUDE  : num  3040 3042 3340 3458 3412 ...
##  $ LONGITUDE : num  8812 8755 8742 8626 8642 ...
##  $ LATITUDE_E: num  3051 0 0 0 0 ...
##  $ LONGITUDE_: num  8806 0 0 0 0 ...
##  $ REMARKS   : chr  "" "" "" "" ...
##  $ REFNUM    : num  1 2 3 4 5 6 7 8 9 10 ...
```

```r
data <- select(data, c(STATE, BGN_DATE, EVTYPE, FATALITIES, INJURIES, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP))
```

I only kept the columns of data that were necessary to determne which weather events had the greatest impact on economic damage and human health.

#### Filter data based on number of weather events in data set^[Thanks to Mentor Usama Khalil's useful post on cleaning the storm data.[https://www.coursera.org/learn/reproducible-research/discussions/all/threads/38y35MMiEeiERhLphT2-QA]]


From the NOAA website, I learned that starting in 1996, 48 event types are recorded in the Storm Events Database.  Before 1996, only tornado (1950-present), thunderstorm, wind and hail (1955-present) were recorded.[Storm Data](https://www.ncdc.noaa.gov/stormevents/details.jsp)  Because this assignment asks us to determine which events have the biggest effect on human health or largest economic impact, I have filtered for data set to only include records with the complete set of event data from 1996.


```r
library(lubridate)
data$year <- year(mdy_hms(data$BGN_DATE)) 
data<- data[data$year > 1995,]
```

#### Cleaning the event type names

The event names are not all coded in the same way, so I created a new variable, event, that has all lower case letters and standardized event names by making the following transformations: 

* tolower was used to change the event names to lower case
* grepl was used to find the index of for all variations of a specific event type 
* the index was used to replace all variations with the official name, as stated in the documentation.[Storm Data Event Table](https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf)


```r
data$event <- tolower(data$EVTYPE)

official_event_names <- tolower(c("Astronomical Low Tide", "Avalanche,     Blizzard", "Coastal Flood","Cold/Wind Chill", "Debris Flow", "Dense Fog", "Dense Smoke", "Drought", "Dust Devil", "Dust Storm", "Excessive Heat", "Extreme Cold/Wind Chill", "Flash Flood", "Flood", "Frost/Freeze", "Funnel Cloud",  "Freezing Fog", "Hail", "Heat", "Heavy Rain", "Heavy Snow",  "High Surf", "High Wind", "Hurricane/Typhoon", "Ice Storm", "Lake-Effect-Snow", "Lakeshore Flood", "Lightning",  "Marine Hail", "Marine High Wind", "Marine Strong Wind", "Marine Thunderstorm Wind", "Rip Current", "Seiche", "Sleet",
 "Storm Surge/Tide", "Strong Wind", "Thunderstorm Wind", "Tornado", "Tropical Depression", "Tropical Storm",  "Tsunami", "Volcanic Ash", "Waterspout", "Wildfire",  "Winter Storm", "Winter Weather"))
```


```r
a <- grepl("ashfall", data$event)
c <- grepl("coastal flood", data$event)
cf <- grepl("cstl flood", data$event)
cd <- grepl("[Dd]ry", data$event)
d <- grepl("[Cc]old", data$event)
f <- grepl("flash", data$event)
fz <- grepl("freeze", data$event)
fr <- grepl("freezing rain", data$event)
h <- grepl("hail", data$event)
hr <- grepl("hurricane", data$event)
l <- grepl("lake effect", data$event)
rp <- grepl("record precipitation", data$event)
rf <- grepl("rainfall", data$event)
r <- grepl("rain", data$event)
s <- grepl("snow", data$event)
sg <- grepl("surge", data$event)
sf <- grepl("surf", data$event)
t <- grepl("typhoon", data$event)
ts <- grepl("tstm", data$event)
wc <- grepl("windchill", data$event)
```


```r
library(stringr)
data$event[a == TRUE] <- "volcanic ash"
data$event[c == TRUE | cf == TRUE] <- "coastal flood"
data$event[c == TRUE | wc == TRUE] <- "extreme cold/wind chill"
data$event[d== TRUE] <- "drought"
data$event[f == TRUE] <- "flash flood"
data$event[rf==TRUE | rp==TRUE | r==TRUE ] <- "heavy rain"
data$event[fr == TRUE] <- "sleet"
data$event[fz == TRUE] <- "frost/freeze"
data$event[h == TRUE] <- "hail"
data$event[l == TRUE] <- "lake-effect"
data$event[sf == TRUE] <- "high surf"
data$event[s == TRUE] <- "heavy snow"
data$event[sg == TRUE] <- "storm surge/tide"
data$event[t == TRUE | hr == TRUE] <- "hurricane/typhoon"
data$event[ts == TRUE] <- "tsunami"
data$event <- str_replace(data$event, "strong winds", "strong wind")
data$event <- str_replace(data$event, "rip currents", "rip current")
data$event <- str_replace_all(data$event, "high wind (g40)", "high wind")
data$event <- str_replace_all(data$event, "dust devel", "dust devil")
data$event <- str_replace_all(data$event, "ice fog", "freezing fog")
```

There are some event names that are are not clearly in one of the standarized event types.  I decided to delete these instances from the data base. I checked to see which events did not match the official list after the above standardization.  


```r
library(dplyr)
bad_names <- setdiff(data$event, official_event_names)
bn <- length(bad_names)
data <- filter(data, event %in% official_event_names)
```

There are 247 observations with invalid event names which were removed from the data set.    

#### Cleaning the CROPDMG and PROPDMG variables 

I made the following tranformations to the CROPDMG and PROPDMG.^[I found this guide to handling the coding of CROPDMGEXP and PROPDMGEXP very helpful. [https://rstudio-pubs-static.s3.amazonaws.com/58957_37b6723ee52b455990e149edde45e5b6.html]]

* Filter data set to exclude rows w/ either missing data for PROPDMGEXP or CROPDMGEXP
* Created a function to adjust for different units in CROPDMGEXP and PROPDMGEXP


```r
econ_df <- data[data$CROPDMGEXP != "" | 
                        data$PROPDMGEXP != "", ]
```

* Function to adjust for different units in CROPDMGEXP and PROPDMGEXP


```r
transform <- function (x, y) {

        if (x == "H" | x == "h")
                {y <- y * 10
        } else if (x == "K" | x == "k") 
                {y <- y * 10^3
        } else if (x == "M" | x == "m") 
                {y <- y * 10^6
        } else if (x == "B" | x == "b") 
                {y <- y * 10^9
        } else if (x == "-" | x == "?")
                 {y <- 0
        }
        y
}
```

* Use **transform** to adjust CROPDMG and PROPDMG to account for the different units in the original data base.


```r
econ_df$units_adj_cropdmg <- mapply(transform, 
                econ_df$CROPDMGEXP, econ_df$CROPDMG)
econ_df$units_adj_propdmg <- mapply(transform, 
                econ_df$PROPDMGEXP, econ_df$PROPDMG)
econ_length <- nrow(econ_df)
```

After all of the cleaning, the econ_df data set contains 377025 observations.

#### Create a factor variable for the region of the country

* There are 50 states plus the DC and 5 territories.[hud.gov](hud.gov/sites/documents/22211X2CHCH.PDF)


```r
states_territories <- c(state_codes$state_abbr, "AS", "GU", "PR", "VI", "MP")
length <- length(unique(econ_df$STATE))
```

Therefore, there should be 56 unique state codes. However, there are  69 unique codes in the STATE variable. There is no way to determine what code should have been entered, so when I do the regional analysis I have filtered out the observations associated with the 13 invalid state codes plus DC when creating the table on economic damage by region.^[When I created the table on regional effects, there was so little data for DC that it was not filtering properly so it was not included in the regional tables.]    

*  create a factor variable for the region of the country


```r
region <- c(state_codes$region_name, "Territory", 
            "Territory", "Territory","Territory", "Territory")
econ_df$region <- factor(econ_df$STATE, levels = states_territories,
                         labels = region)
```

## II.  Results

### Economic Impact of Storms by Event Type

We were asked to explore which weather event type causes the most economic damage. To answer this question, I first found the total property and crop damage for each event type and ranked by total property damage.  I then found the top three destructive weather types by region of the country.   

* Create a table showing the total property and crop damage by year for each event type and rank to find the 10 most expensive types. I used the package gt to create my tables.  The documentation for this package can be found here.[https://gt.rstudio.com/articles/intro-creating-gt-tables.html] 


```r
library(gt)
library(dplyr)
tab = econ_df %>% 
        tibble() %>%
        group_by(event) %>%
        summarize(total_property_damage = sum(units_adj_propdmg/10^6),
                  total_crop_damage = sum(units_adj_cropdmg/10^6)) %>%
        arrange(desc(total_property_damage)) %>%
        top_n(10) %>%
        gt(rowname_col = "event") %>%
        tab_header(
        title = "Table 1:  Ten Most Costly Weather Events (Millions)",
        subtitle = "1996-2011")  %>% 
        cols_align(align = "left") %>%
        tab_source_note(source_note = "Source:  NOAA Storm Data")  %>% 
        fmt_currency(
                columns = vars(total_property_damage, total_crop_damage), 
                currency = "USD", decimals = 0)  %>%
        cols_label(
                event = "Event",
                total_property_damage= "Total Property Damage",
                total_crop_damage = "Total Crop Damage"
                ) %>%
        opt_row_striping(row_striping = TRUE) %>%
        opt_table_outline() %>%
        tab_options(
                heading.background.color = "#85C1E9",
                row.striping.background_color = "#EBF5FB",
                column_labels.background.color = "#D6EAF8"
        )
```

```
## Selecting by total_crop_damage
```

```r
print(tab)
```

<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#ekjdpmsnly .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 3px;
  border-top-color: #D3D3D3;
  border-right-style: solid;
  border-right-width: 3px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 3px;
  border-bottom-color: #D3D3D3;
  border-left-style: solid;
  border-left-width: 3px;
  border-left-color: #D3D3D3;
}

#ekjdpmsnly .gt_heading {
  background-color: #85C1E9;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#ekjdpmsnly .gt_title {
  color: #FFFFFF;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#ekjdpmsnly .gt_subtitle {
  color: #FFFFFF;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#ekjdpmsnly .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ekjdpmsnly .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#ekjdpmsnly .gt_col_heading {
  color: #333333;
  background-color: #D6EAF8;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#ekjdpmsnly .gt_column_spanner_outer {
  color: #333333;
  background-color: #D6EAF8;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#ekjdpmsnly .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#ekjdpmsnly .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#ekjdpmsnly .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#ekjdpmsnly .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#ekjdpmsnly .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#ekjdpmsnly .gt_from_md > :first-child {
  margin-top: 0;
}

#ekjdpmsnly .gt_from_md > :last-child {
  margin-bottom: 0;
}

#ekjdpmsnly .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#ekjdpmsnly .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#ekjdpmsnly .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ekjdpmsnly .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#ekjdpmsnly .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ekjdpmsnly .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#ekjdpmsnly .gt_striped {
  background-color: #EBF5FB;
}

#ekjdpmsnly .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ekjdpmsnly .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#ekjdpmsnly .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#ekjdpmsnly .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#ekjdpmsnly .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#ekjdpmsnly .gt_left {
  text-align: left;
}

#ekjdpmsnly .gt_center {
  text-align: center;
}

#ekjdpmsnly .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#ekjdpmsnly .gt_font_normal {
  font-weight: normal;
}

#ekjdpmsnly .gt_font_bold {
  font-weight: bold;
}

#ekjdpmsnly .gt_font_italic {
  font-style: italic;
}

#ekjdpmsnly .gt_super {
  font-size: 65%;
}

#ekjdpmsnly .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>
<div id="ekjdpmsnly" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;"><table class="gt_table">
  <thead class="gt_header">
    <tr>
      <th colspan="3" class="gt_heading gt_title gt_font_normal" style>Table 1:  Ten Most Costly Weather Events (Millions)</th>
    </tr>
    <tr>
      <th colspan="3" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style>1996-2011</th>
    </tr>
  </thead>
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1"></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">Total Property Damage</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">Total Crop Damage</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr>
      <td class="gt_row gt_left gt_stub">flood</td>
      <td class="gt_row gt_left">$143,945</td>
      <td class="gt_row gt_left">$4,975</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">hurricane/typhoon</td>
      <td class="gt_row gt_left gt_striped">$81,719</td>
      <td class="gt_row gt_left gt_striped">$5,350</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">flash flood</td>
      <td class="gt_row gt_left">$15,222</td>
      <td class="gt_row gt_left">$1,335</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">hail</td>
      <td class="gt_row gt_left gt_striped">$14,595</td>
      <td class="gt_row gt_left gt_striped">$2,497</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">tropical storm</td>
      <td class="gt_row gt_left">$7,642</td>
      <td class="gt_row gt_left">$678</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">high wind</td>
      <td class="gt_row gt_left gt_striped">$5,248</td>
      <td class="gt_row gt_left gt_striped">$634</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">tsunami</td>
      <td class="gt_row gt_left">$4,680</td>
      <td class="gt_row gt_left">$619</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">drought</td>
      <td class="gt_row gt_left gt_striped">$1,077</td>
      <td class="gt_row gt_left gt_striped">$14,707</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">heavy rain</td>
      <td class="gt_row gt_left">$585</td>
      <td class="gt_row gt_left">$738</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">frost/freeze</td>
      <td class="gt_row gt_left gt_striped">$19</td>
      <td class="gt_row gt_left gt_striped">$1,327</td>
    </tr>
  </tbody>
  <tfoot class="gt_sourcenotes">
    <tr>
      <td class="gt_sourcenote" colspan="3">Source:  NOAA Storm Data</td>
    </tr>
  </tfoot>
  
</table></div>

The most economic damage was cased by floods, followed by hurricanes/typhoons. Droughts led to the most crop damage.  

#### Create plot of yearly economic damage by the top 5 most destructive weather event types.  

* Create a new data set and subset to the top 5 most damaging weather event types
* Change event to a factor variable
* Filter the data set to include only the top 5 most damaging weather event  types
* Create a panel plot of the economic damage for the top 5 most damaging weather event types
* The y axis is total property damage in log(millions of dollars).  Note that the damage from flooding in 2005 is so much larger than the damage from other events that I changed it to a log scale.



```r
library(dplyr)

top_five_damage <- econ_df %>% group_by(event) %>% 
        summarize(total_property_damage = sum(units_adj_propdmg/10^6),
                  total_crop_damage = sum(units_adj_cropdmg/10^6)) %>%
          arrange(desc(total_property_damage, total_crop_damage)) %>% top_n(5)
```

```
## Selecting by total_crop_damage
```

```r
##create new data frame to plot, grouped by both year and event
df <- econ_df %>% group_by(year, event) %>% 
        summarize(total_property_damage = sum(units_adj_propdmg)/10^6,
                  total_crop_damage = sum(units_adj_cropdmg)/10^6)
```

```
## `summarise()` has grouped output by 'year'. You can override using the `.groups` argument.
```

```r
plot <- filter(df, event %in% top_five_damage$event)
plot$event <- as.factor(plot$event)
```


```r
library(ggplot2)

m <- max(plot$total_property_damage)
d <- ggplot(data=plot, aes(year)) +
  geom_line(aes(y=log(total_property_damage), 
                colour="total_property_damage")) +                geom_line(aes(y=log(total_crop_damage),                                                       colour = "total_crop_damage")) +
        labs(y="total cost", 
        title="Total Property and Crop Damage, 1996-2011", 
        subtitle=("log(Millions of US Dollars)"), 
        caption = "Figure 1:  Flooding and hurricanes in the mid-2000s caused the most economic damage.") +
        facet_grid(.~ event) + 
        coord_cartesian(ylim=c(0, log(m))) 
d + scale_colour_manual(values = c("#85C1E9", "#845EC2")) + theme_linedraw() +     theme(plot.caption = element_text(hjust = 0)
         )
```

![](noaa_files/figure-html/fig1-1.png)<!-- -->

This plot reveals that while there is a great deal of yearly variation, flooding and hurricanes caused a huge amount of economic damage in the mid-2000s.  

#### Filter data set to remove invalid state codes

I waited until after I had investigated the effects on a national level before subsetting the data set for valid state codes.  



```r
econ_df <- econ_df[econ_df$STATE %in% states_territories, ]
```

*   The gt package is fairly new, so I read the very helpful tutorial at [towarddatascience.com](https://towardsdatascience.com/exploring-the-gt-grammar-of-tables-package-in-r-7fff9d0b40cd) to create the table on regional economic effects of different weather events.

*  group by region and event and find total property and crop damage
*  arrange in descending order by total property damage and report top 3


```r
library(gt)
library(dplyr)
tab1 = econ_df %>% 
        tibble() %>%
        group_by(region, event) %>%
        summarize(total_property_damage = 
                          sum(units_adj_propdmg)/10^6,
                  total_crop_damage = 
                          sum(units_adj_cropdmg)/10^6)%>%
        arrange(desc(total_property_damage, total_crop_damage)) %>% top_n(3)          %>%
        gt(rowname_col = "event") %>%
        tab_header(
        title = "Table 2:  Most Costly Weather Events by Region (Millions)",
        subtitle = "1996-2011")  %>% 
        cols_align(align = "left") %>%
        tab_source_note(source_note = "Source:  NOAA Storm Data")  %>% 
        fmt_currency(
                columns = vars(total_property_damage, total_crop_damage), 
                currency = "USD", decimals = 0)  %>%
        cols_label(
                total_property_damage = "Total Property Damage",
                total_crop_damage = "Total Crop Damage"
                ) %>%
        opt_row_striping(row_striping = TRUE) %>%
        opt_table_outline() %>%
        tab_options(
                heading.background.color = "#85C1E9",
                row_group.background.color = "#EBF5FB",
                column_labels.background.color = "#D6EAF8"
        )
```

```
## `summarise()` has grouped output by 'region'. You can override using the `.groups` argument.
```

```
## Selecting by total_crop_damage
```

```r
print(tab1)
```

<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#txdxtyhcnv .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 3px;
  border-top-color: #D3D3D3;
  border-right-style: solid;
  border-right-width: 3px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 3px;
  border-bottom-color: #D3D3D3;
  border-left-style: solid;
  border-left-width: 3px;
  border-left-color: #D3D3D3;
}

#txdxtyhcnv .gt_heading {
  background-color: #85C1E9;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#txdxtyhcnv .gt_title {
  color: #FFFFFF;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#txdxtyhcnv .gt_subtitle {
  color: #FFFFFF;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#txdxtyhcnv .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#txdxtyhcnv .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#txdxtyhcnv .gt_col_heading {
  color: #333333;
  background-color: #D6EAF8;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#txdxtyhcnv .gt_column_spanner_outer {
  color: #333333;
  background-color: #D6EAF8;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#txdxtyhcnv .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#txdxtyhcnv .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#txdxtyhcnv .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#txdxtyhcnv .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #EBF5FB;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#txdxtyhcnv .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #EBF5FB;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#txdxtyhcnv .gt_from_md > :first-child {
  margin-top: 0;
}

#txdxtyhcnv .gt_from_md > :last-child {
  margin-bottom: 0;
}

#txdxtyhcnv .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#txdxtyhcnv .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#txdxtyhcnv .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#txdxtyhcnv .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#txdxtyhcnv .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#txdxtyhcnv .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#txdxtyhcnv .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#txdxtyhcnv .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#txdxtyhcnv .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#txdxtyhcnv .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#txdxtyhcnv .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#txdxtyhcnv .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#txdxtyhcnv .gt_left {
  text-align: left;
}

#txdxtyhcnv .gt_center {
  text-align: center;
}

#txdxtyhcnv .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#txdxtyhcnv .gt_font_normal {
  font-weight: normal;
}

#txdxtyhcnv .gt_font_bold {
  font-weight: bold;
}

#txdxtyhcnv .gt_font_italic {
  font-style: italic;
}

#txdxtyhcnv .gt_super {
  font-size: 65%;
}

#txdxtyhcnv .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>
<div id="txdxtyhcnv" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;"><table class="gt_table">
  <thead class="gt_header">
    <tr>
      <th colspan="3" class="gt_heading gt_title gt_font_normal" style>Table 2:  Most Costly Weather Events by Region (Millions)</th>
    </tr>
    <tr>
      <th colspan="3" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style>1996-2011</th>
    </tr>
  </thead>
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1"></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">Total Property Damage</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">Total Crop Damage</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr class="gt_group_heading_row">
      <td colspan="3" class="gt_group_heading">South</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">hurricane/typhoon</td>
      <td class="gt_row gt_left">$78,941</td>
      <td class="gt_row gt_left">$4,793</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">flood</td>
      <td class="gt_row gt_left gt_striped">$10,216</td>
      <td class="gt_row gt_left gt_striped">$905</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">drought</td>
      <td class="gt_row gt_left">$399</td>
      <td class="gt_row gt_left">$10,007</td>
    </tr>
    <tr class="gt_group_heading_row">
      <td colspan="3" class="gt_group_heading">Midwest</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">flood</td>
      <td class="gt_row gt_left gt_striped">$8,513</td>
      <td class="gt_row gt_left gt_striped">$3,681</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">hail</td>
      <td class="gt_row gt_left">$6,170</td>
      <td class="gt_row gt_left">$1,466</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">drought</td>
      <td class="gt_row gt_left gt_striped">$655</td>
      <td class="gt_row gt_left gt_striped">$3,155</td>
    </tr>
    <tr class="gt_group_heading_row">
      <td colspan="3" class="gt_group_heading">Northeast</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">flash flood</td>
      <td class="gt_row gt_left">$4,311</td>
      <td class="gt_row gt_left">$20</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">hail</td>
      <td class="gt_row gt_left gt_striped">$15</td>
      <td class="gt_row gt_left gt_striped">$103</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">drought</td>
      <td class="gt_row gt_left">$1</td>
      <td class="gt_row gt_left">$702</td>
    </tr>
    <tr class="gt_group_heading_row">
      <td colspan="3" class="gt_group_heading">Territory</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">hurricane/typhoon</td>
      <td class="gt_row gt_left gt_striped">$2,777</td>
      <td class="gt_row gt_left gt_striped">$557</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">flood</td>
      <td class="gt_row gt_left">$116</td>
      <td class="gt_row gt_left">$48</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">tropical storm</td>
      <td class="gt_row gt_left gt_striped">$101</td>
      <td class="gt_row gt_left gt_striped">$104</td>
    </tr>
    <tr class="gt_group_heading_row">
      <td colspan="3" class="gt_group_heading">West</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">heavy rain</td>
      <td class="gt_row gt_left">$517</td>
      <td class="gt_row gt_left">$717</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">drought</td>
      <td class="gt_row gt_left gt_striped">$17</td>
      <td class="gt_row gt_left gt_striped">$844</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">frost/freeze</td>
      <td class="gt_row gt_left">$6</td>
      <td class="gt_row gt_left">$503</td>
    </tr>
  </tbody>
  <tfoot class="gt_sourcenotes">
    <tr>
      <td class="gt_sourcenote" colspan="3">Source:  NOAA Storm Data</td>
    </tr>
  </tfoot>
  
</table></div>

Hurricane/typhoons and flooding in the South led to the greatest amount of economic damage. 

### Human Impact of Weather Events 

We were asked to explore which weather event type causes the most harm to human health. To answer this question, I first found the total number of fatalities and injuries for each event type.  I then found the top ten most impactful weather types.   

#### Total US Health Impact

* Find total fatalities and injuries by year for each event type and rank to find the 10 most expensive types.


```r
health <- data %>% group_by(event) %>% 
        summarize(total_fatalities = sum(FATALITIES),
                  total_injuries = sum(INJURIES) )

health <- filter(health, event %in% official_event_names)
health_table <- health %>% 
        arrange(desc(total_fatalities, total_injuries)) %>% top_n(10)
```

```
## Selecting by total_injuries
```



```r
library(gt)
ht_tbl <- data %>%
          tibble() %>%
          group_by(event) %>%
          summarize(total_fatalities = sum(FATALITIES),
                  total_injuries = sum(INJURIES)) %>%
          arrange(desc(total_fatalities)) %>%
          top_n(10) %>%
          gt(rowname_col = "event") %>%
          tab_header(
            title = "Table 3:  Weather Types Ranked by Impact on Human Health",
            subtitle = "1996-2001") %>%
            cols_align(align = "left") %>%
            tab_source_note(source_note = "Source:  NOAA Storm Data") %>%
            cols_label(
              event = "Event",
              total_fatalities = "Total Fatalities",
              total_injuries = "Total Injuries"
            )  %>%
           opt_row_striping(row_striping = TRUE) %>%
           opt_table_outline() %>%
           tab_options(
             heading.background.color = "#85C1E9",
             row.striping.background_color = "EBF5FB",
             column_labels.background.color = "#D6EAF8"
            )
```

```
## Selecting by total_injuries
```

```r
print(ht_tbl)
```

<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#dhbmwznrwg .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 3px;
  border-top-color: #D3D3D3;
  border-right-style: solid;
  border-right-width: 3px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 3px;
  border-bottom-color: #D3D3D3;
  border-left-style: solid;
  border-left-width: 3px;
  border-left-color: #D3D3D3;
}

#dhbmwznrwg .gt_heading {
  background-color: #85C1E9;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#dhbmwznrwg .gt_title {
  color: #FFFFFF;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#dhbmwznrwg .gt_subtitle {
  color: #FFFFFF;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#dhbmwznrwg .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#dhbmwznrwg .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#dhbmwznrwg .gt_col_heading {
  color: #333333;
  background-color: #D6EAF8;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#dhbmwznrwg .gt_column_spanner_outer {
  color: #333333;
  background-color: #D6EAF8;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#dhbmwznrwg .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#dhbmwznrwg .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#dhbmwznrwg .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#dhbmwznrwg .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#dhbmwznrwg .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#dhbmwznrwg .gt_from_md > :first-child {
  margin-top: 0;
}

#dhbmwznrwg .gt_from_md > :last-child {
  margin-bottom: 0;
}

#dhbmwznrwg .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#dhbmwznrwg .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#dhbmwznrwg .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#dhbmwznrwg .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#dhbmwznrwg .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#dhbmwznrwg .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#dhbmwznrwg .gt_striped {
  background-color: EBF5FB;
}

#dhbmwznrwg .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#dhbmwznrwg .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#dhbmwznrwg .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#dhbmwznrwg .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#dhbmwznrwg .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#dhbmwznrwg .gt_left {
  text-align: left;
}

#dhbmwznrwg .gt_center {
  text-align: center;
}

#dhbmwznrwg .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#dhbmwznrwg .gt_font_normal {
  font-weight: normal;
}

#dhbmwznrwg .gt_font_bold {
  font-weight: bold;
}

#dhbmwznrwg .gt_font_italic {
  font-style: italic;
}

#dhbmwznrwg .gt_super {
  font-size: 65%;
}

#dhbmwznrwg .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>
<div id="dhbmwznrwg" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;"><table class="gt_table">
  <thead class="gt_header">
    <tr>
      <th colspan="3" class="gt_heading gt_title gt_font_normal" style>Table 3:  Weather Types Ranked by Impact on Human Health</th>
    </tr>
    <tr>
      <th colspan="3" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style>1996-2001</th>
    </tr>
  </thead>
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1"></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">Total Fatalities</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">Total Injuries</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr>
      <td class="gt_row gt_left gt_stub">excessive heat</td>
      <td class="gt_row gt_left">1797</td>
      <td class="gt_row gt_left">6391</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">tornado</td>
      <td class="gt_row gt_left gt_striped">1511</td>
      <td class="gt_row gt_left gt_striped">20667</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">flash flood</td>
      <td class="gt_row gt_left">887</td>
      <td class="gt_row gt_left">1674</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">lightning</td>
      <td class="gt_row gt_left gt_striped">651</td>
      <td class="gt_row gt_left gt_striped">4141</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">flood</td>
      <td class="gt_row gt_left">414</td>
      <td class="gt_row gt_left">6758</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">tsunami</td>
      <td class="gt_row gt_left gt_striped">289</td>
      <td class="gt_row gt_left gt_striped">3866</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">heat</td>
      <td class="gt_row gt_left">237</td>
      <td class="gt_row gt_left">1222</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">winter storm</td>
      <td class="gt_row gt_left gt_striped">191</td>
      <td class="gt_row gt_left gt_striped">1292</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">thunderstorm wind</td>
      <td class="gt_row gt_left">130</td>
      <td class="gt_row gt_left">1400</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">hurricane/typhoon</td>
      <td class="gt_row gt_left gt_striped">125</td>
      <td class="gt_row gt_left gt_striped">1328</td>
    </tr>
  </tbody>
  <tfoot class="gt_sourcenotes">
    <tr>
      <td class="gt_sourcenote" colspan="3">Source:  NOAA Storm Data</td>
    </tr>
  </tfoot>
  
</table></div>

According to the table, excessive heat leads to the most deaths, while tornadoes lead to the most injuries.  

#### Regional health effects


```r
data$region <- factor(data$STATE, levels = states_territories,
                         labels = region)

health_data <- data[data$STATE %in% states_territories, ]
```


```r
library(gt)
library(dplyr)
tab1 = health_data %>% 
        tibble() %>%
        group_by(region, event) %>%
        summarize(total_fatalities = sum(FATALITIES),
                  total_injuries = sum(INJURIES)) %>%
        arrange(desc(total_fatalities)) %>% top_n(3) %>%
        gt(rowname_col = "event" ) %>%
        tab_header(
        title = "Table 4:  Health Effects of Weather Events by Region",
        subtitle = "1996-2011")  %>% 
        cols_align(align = "left") %>%
        tab_source_note(source_note = "Source:  NOAA Storm Data")  %>% 
        cols_label(
                event = "Event",
                total_fatalities = "Total Fatalities",
                total_injuries = "Total Injuries"
                ) %>%
        opt_row_striping(row_striping = TRUE) %>%
        opt_table_outline() %>%
        tab_options(
                heading.background.color = "#85C1E9",
                row_group.background.color = "#EBF5FB",
                column_labels.background.color = "#D6EAF8"
        )
```

```
## `summarise()` has grouped output by 'region'. You can override using the `.groups` argument.
```

```
## Selecting by total_injuries
```

```r
print(tab1)
```

<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#xqfqjqmgiw .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 3px;
  border-top-color: #D3D3D3;
  border-right-style: solid;
  border-right-width: 3px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 3px;
  border-bottom-color: #D3D3D3;
  border-left-style: solid;
  border-left-width: 3px;
  border-left-color: #D3D3D3;
}

#xqfqjqmgiw .gt_heading {
  background-color: #85C1E9;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#xqfqjqmgiw .gt_title {
  color: #FFFFFF;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#xqfqjqmgiw .gt_subtitle {
  color: #FFFFFF;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#xqfqjqmgiw .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#xqfqjqmgiw .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#xqfqjqmgiw .gt_col_heading {
  color: #333333;
  background-color: #D6EAF8;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#xqfqjqmgiw .gt_column_spanner_outer {
  color: #333333;
  background-color: #D6EAF8;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#xqfqjqmgiw .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#xqfqjqmgiw .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#xqfqjqmgiw .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#xqfqjqmgiw .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #EBF5FB;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#xqfqjqmgiw .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #EBF5FB;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#xqfqjqmgiw .gt_from_md > :first-child {
  margin-top: 0;
}

#xqfqjqmgiw .gt_from_md > :last-child {
  margin-bottom: 0;
}

#xqfqjqmgiw .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#xqfqjqmgiw .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#xqfqjqmgiw .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#xqfqjqmgiw .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#xqfqjqmgiw .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#xqfqjqmgiw .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#xqfqjqmgiw .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#xqfqjqmgiw .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#xqfqjqmgiw .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#xqfqjqmgiw .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#xqfqjqmgiw .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#xqfqjqmgiw .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#xqfqjqmgiw .gt_left {
  text-align: left;
}

#xqfqjqmgiw .gt_center {
  text-align: center;
}

#xqfqjqmgiw .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#xqfqjqmgiw .gt_font_normal {
  font-weight: normal;
}

#xqfqjqmgiw .gt_font_bold {
  font-weight: bold;
}

#xqfqjqmgiw .gt_font_italic {
  font-style: italic;
}

#xqfqjqmgiw .gt_super {
  font-size: 65%;
}

#xqfqjqmgiw .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>
<div id="xqfqjqmgiw" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;"><table class="gt_table">
  <thead class="gt_header">
    <tr>
      <th colspan="3" class="gt_heading gt_title gt_font_normal" style>Table 4:  Health Effects of Weather Events by Region</th>
    </tr>
    <tr>
      <th colspan="3" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style>1996-2011</th>
    </tr>
  </thead>
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1"></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">Total Fatalities</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">Total Injuries</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr class="gt_group_heading_row">
      <td colspan="3" class="gt_group_heading">South</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">tornado</td>
      <td class="gt_row gt_left">1089</td>
      <td class="gt_row gt_left">14399</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">lightning</td>
      <td class="gt_row gt_left gt_striped">354</td>
      <td class="gt_row gt_left gt_striped">2054</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">flood</td>
      <td class="gt_row gt_left">173</td>
      <td class="gt_row gt_left">6379</td>
    </tr>
    <tr class="gt_group_heading_row">
      <td colspan="3" class="gt_group_heading">Midwest</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">excessive heat</td>
      <td class="gt_row gt_left gt_striped">594</td>
      <td class="gt_row gt_left gt_striped">4232</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">tornado</td>
      <td class="gt_row gt_left">397</td>
      <td class="gt_row gt_left">5247</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">tsunami</td>
      <td class="gt_row gt_left gt_striped">82</td>
      <td class="gt_row gt_left gt_striped">1368</td>
    </tr>
    <tr class="gt_group_heading_row">
      <td colspan="3" class="gt_group_heading">Northeast</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">excessive heat</td>
      <td class="gt_row gt_left">424</td>
      <td class="gt_row gt_left">621</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">lightning</td>
      <td class="gt_row gt_left gt_striped">60</td>
      <td class="gt_row gt_left gt_striped">834</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">tornado</td>
      <td class="gt_row gt_left">15</td>
      <td class="gt_row gt_left">720</td>
    </tr>
    <tr class="gt_group_heading_row">
      <td colspan="3" class="gt_group_heading">West</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">lightning</td>
      <td class="gt_row gt_left gt_striped">113</td>
      <td class="gt_row gt_left gt_striped">578</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">winter storm</td>
      <td class="gt_row gt_left">85</td>
      <td class="gt_row gt_left">685</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">wildfire</td>
      <td class="gt_row gt_left gt_striped">44</td>
      <td class="gt_row gt_left gt_striped">696</td>
    </tr>
    <tr class="gt_group_heading_row">
      <td colspan="3" class="gt_group_heading">Territory</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">rip current</td>
      <td class="gt_row gt_left">54</td>
      <td class="gt_row gt_left">58</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">tsunami</td>
      <td class="gt_row gt_left gt_striped">32</td>
      <td class="gt_row gt_left gt_striped">132</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">hurricane/typhoon</td>
      <td class="gt_row gt_left">22</td>
      <td class="gt_row gt_left">360</td>
    </tr>
  </tbody>
  <tfoot class="gt_sourcenotes">
    <tr>
      <td class="gt_sourcenote" colspan="3">Source:  NOAA Storm Data</td>
    </tr>
  </tfoot>
  
</table></div>

When you break down the deaths and injuries by region of the country, some big differences become clear.  For example, in the South there were more deaths caused by tornadoes while in the Northeast and Midwest there were more deaths due to excessive heat.  It was surprising to learn that there were dangerous tsunamis in the Midwest (where I happen to live.)  I did some research and found out that they are a common phenomenon in the Great Lakes called meteotsunamis.^[Technically, the events were originally coded as "tsunami wind."]  


```r
midwest_tsunami <- filter(data, region == "Midwest" & event == "tsunami")
mwt <- filter(data, data$region == "Midwest" & data$event == "tsunami" & data$FATALITIES > 0) 
print(head(mwt))
```

```
##   STATE          BGN_DATE    EVTYPE FATALITIES INJURIES PROPDMG PROPDMGEXP
## 1    IN 4/20/1996 0:00:00 TSTM WIND          2        0      50          K
## 2    MO  8/4/1996 0:00:00 TSTM WIND          3        6       0           
## 3    MO  8/4/1996 0:00:00 TSTM WIND          1        0       0           
## 4    NE  8/6/1996 0:00:00 TSTM WIND          1        0       0           
## 5    OH 4/29/1996 0:00:00 TSTM WIND          1        0     300          K
## 6    OH 4/12/1996 0:00:00 TSTM WIND          1        1       1          M
##   CROPDMG CROPDMGEXP year   event  region
## 1       0            1996 tsunami Midwest
## 2       0            1996 tsunami Midwest
## 3       0            1996 tsunami Midwest
## 4       0            1996 tsunami Midwest
## 5       0            1996 tsunami Midwest
## 6       0            1996 tsunami Midwest
```

#### Plot Fatalities and Injuries for weather events by year 

*  Create a new data frame to plot deaths associated with different storm types.


```r
top_health <- filter(health, event %in% official_event_names) %>%
          arrange(desc(total_fatalities, total_injuries)) %>% top_n(5)
```

```
## Selecting by total_injuries
```

```r
##create new data frame to plot
df2 <- data %>% group_by(year, event) %>% 
        summarize(total_fatalities = sum(FATALITIES),
                  total_injuries = sum(INJURIES))
```

```
## `summarise()` has grouped output by 'year'. You can override using the `.groups` argument.
```

```r
plot2 <- filter(df2, event %in% top_health$event)
plot2$event <- as.factor(plot2$event)

#plot <- filter(df, event %in% top_damage$event)
f<- max(plot2$total_fatalities)

g <- ggplot(data=plot2, aes(x=year)) +
       geom_col(aes(y=total_fatalities, fill=event)) +
       labs(title="Total Fatalties by Weather Event Type, 1996-2011",
       y="total number of persons",
       caption="Figure 2:  Excessive heat consistently causes a lot of yearly deaths, but tornadoes led to the most deaths in 2011.")  
       coord_cartesian(ylim=c(0, f)) 
```

```
## <ggproto object: Class CoordCartesian, Coord, gg>
##     aspect: function
##     backtransform_range: function
##     clip: on
##     default: FALSE
##     distance: function
##     expand: TRUE
##     is_free: function
##     is_linear: function
##     labels: function
##     limits: list
##     modify_scales: function
##     range: function
##     render_axis_h: function
##     render_axis_v: function
##     render_bg: function
##     render_fg: function
##     setup_data: function
##     setup_layout: function
##     setup_panel_guides: function
##     setup_panel_params: function
##     setup_params: function
##     train_panel_guides: function
##     transform: function
##     super:  <ggproto object: Class CoordCartesian, Coord, gg>
```

```r
g + scale_fill_brewer(palette="BuPu") + theme_linedraw() + theme(plot.caption        = element_text(hjust = 0))
```

![](noaa_files/figure-html/fig2-1.png)<!-- -->

*  Plot total injuries by weather type event.


```r
#plot <- filter(df, event %in% top_damage$event)
i<- max(plot2$total_injuries)

h <- ggplot(data=plot2, aes(x=year)) +
       geom_col(aes(y=total_injuries, fill=event)) +
       labs(title="Total Injuries by Weather Event Type, 1996-2011",
       y="total number of persons",
       caption="Figure 3:  While tornadoes generally cause the most injuries, flooding in 1998 led to a spike in injuries.")  
       coord_cartesian(ylim=c(0, i)) 
```

```
## <ggproto object: Class CoordCartesian, Coord, gg>
##     aspect: function
##     backtransform_range: function
##     clip: on
##     default: FALSE
##     distance: function
##     expand: TRUE
##     is_free: function
##     is_linear: function
##     labels: function
##     limits: list
##     modify_scales: function
##     range: function
##     render_axis_h: function
##     render_axis_v: function
##     render_bg: function
##     render_fg: function
##     setup_data: function
##     setup_layout: function
##     setup_panel_guides: function
##     setup_panel_params: function
##     setup_params: function
##     train_panel_guides: function
##     transform: function
##     super:  <ggproto object: Class CoordCartesian, Coord, gg>
```

```r
h + scale_fill_brewer(palette="BuPu") + theme_linedraw() + theme(plot.caption        = element_text(hjust = 0))
```

![](noaa_files/figure-html/fig3-1.png)<!-- -->

```r
##h + theme_bw()
```

Figures 2 and 3 show that there is a great deal of yearly variation in the health effects from different weather events.For example, there was a spike in deaths from excessive heat in 1999 and a large number of deaths from tornadoes in 2011.  Likewise, there was a large number of injuries due to flooding in 1998 and a large number of injuries due to tornadoes in 2011. 


## Conclusion

This was a very interesting exercise.  I learned that at a national level, flooding causes the most economic damage, while excessive heat leads to the most deaths overall.  However, the economic and health impacts associated with different weather events vary greatly from year to year and by region.
