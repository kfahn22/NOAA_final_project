---
title: "Historical Weather Event Dominates Economic Damage, while Excessive Heat Leads to Most Deaths "
author: "Kathryn Fahnline"
date: "4/12/2021"
output: 
  html_document:
    keep_md: true
---



## Synopsis

It is perhaps not surprising that the economic cost of Hurricane Katrina (2005) is an order of magnitude above all other weather events in the Storm data set (refer to Table 1).  What is perhaps unexpected is that excessive heat causes the most deaths (refer to Table 2). As my daughter pointed out, if the news outlets warn of a major impending storm, most reasonable people make plans to move out of harm's way. This is less likely to be true when there is a forecast of higher than normal temperatures; hence, the higher death rate.  While excessive heat causes the most deaths, tornados lead to a greater number of combined injuries and deaths -- and in 2011 there was a very large number of fatalities/injuries caused by tornados (see Figures 2 and 3).  

Does the document have a synopsis that describes and summarizes the data analysis in less than 10 sentences?

##  Data Processing

Thanks to Mentor Usama Khalil's useful post on cleaning the storm data.
[1]
[https://www.coursera.org/learn/reproducible-research/discussions/all/threads/38y35MMiEeiERhLphT2-QA]  and this guide to handling the coding of CROPDMGEXP and PROPDMGEXP [https://rstudio-pubs-static.s3.amazonaws.com/58957_37b6723ee52b455990e149edde45e5b6.html]



```r
library(downloader)
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
download.file(fileUrl, destfile = "noaa.csv.bz2", method="curl")
data <- read.csv("noaa.csv.bz2")
```


```r
fileUrl <- "https://raw.githubusercontent.com/kjhealy/fips-codes/master/state_fips_master.csv"
download.file(fileUrl, destfile = "state_codes.csv", method="curl")
state_codes <- read.csv("state_codes.csv")
```

### I.  Data Transformation

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

#### Filter data based on number of weather events in data set

From the NOAA website, I learned that starting in 1996, 48 event types are recorded in the Storm Events Database.  Before 1996, only tornado (1950-present), thunderstorm, wind and hail (1955-present) were recorded. [https://www.ncdc.noaa.gov/stormevents/details.jsp]  Because this assignment asks us to determine which events have the biggest effect on human health or largest economic impact, I have filtered for data set to only include records with the complete set of event data from 1996.


```r
library(lubridate)
data$year <- year(mdy_hms(data$BGN_DATE)) 
data<- data[data$year > 1995,]
```

#### Cleaning the event type names

The event names are not all coded in the same way, so I created a new variable, event, that has all lower case letters and standardized event names by making the following transformations: 

* tolower was used to change the event names to lower case
* grepl was used to find the index of for all variations of a specific event type 
* the index was used to replace all variations with the official name, as stated in the documentation.[https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf]


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
data <- filter(data, event %in% official_event_names)
```

#### Filter data set to exclude rows w/ either missing data for PROPDMGEXP or 
CROPDMGEXP


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

* Use transform to adjust CROPDMGEXP and PROPDMGEXP


```r
econ_df$units_adj_cropdmg <- mapply(transform, 
                econ_df$CROPDMGEXP, econ_df$CROPDMG)
econ_df$units_adj_propdmg <- mapply(transform, 
                econ_df$PROPDMGEXP, econ_df$PROPDMG)
```

### II.  Data Analysis

#### A.  Economic Impact of Storms by Event Type

We were asked to explore which weather event type causes the most economic damage. To answer this question, I first found the total property and crop damage for each event type.  I then found the top ten most destructive weather types.   

* Find total property and crop damage by year for each event type and rank to find the 10 most expensive types.



```r
table1 <- econ_df %>% group_by(event) %>% 
        summarize(total_property_damage = sum(units_adj_propdmg/10^6),
                  total_crop_damage = sum(units_adj_cropdmg/10^6)) %>%
        arrange(desc(total_property_damage)) %>%
        top_n(10)
```

```
## Selecting by total_crop_damage
```


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
        title = "Ten Most Costly Weather Events",
        subtitle = "1996-2011")  %>% 
        cols_align(align = "left") %>%
        tab_source_note(source_note = "Source:  NOAA Storm Data")  %>% 
        fmt_currency(
                columns = vars(total_property_damage), 
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

#jghvsqdqpg .gt_table {
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

#jghvsqdqpg .gt_heading {
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

#jghvsqdqpg .gt_title {
  color: #FFFFFF;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#jghvsqdqpg .gt_subtitle {
  color: #FFFFFF;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#jghvsqdqpg .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#jghvsqdqpg .gt_col_headings {
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

#jghvsqdqpg .gt_col_heading {
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

#jghvsqdqpg .gt_column_spanner_outer {
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

#jghvsqdqpg .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#jghvsqdqpg .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#jghvsqdqpg .gt_column_spanner {
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

#jghvsqdqpg .gt_group_heading {
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

#jghvsqdqpg .gt_empty_group_heading {
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

#jghvsqdqpg .gt_from_md > :first-child {
  margin-top: 0;
}

#jghvsqdqpg .gt_from_md > :last-child {
  margin-bottom: 0;
}

#jghvsqdqpg .gt_row {
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

#jghvsqdqpg .gt_stub {
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

#jghvsqdqpg .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#jghvsqdqpg .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#jghvsqdqpg .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#jghvsqdqpg .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#jghvsqdqpg .gt_striped {
  background-color: #EBF5FB;
}

#jghvsqdqpg .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#jghvsqdqpg .gt_footnotes {
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

#jghvsqdqpg .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#jghvsqdqpg .gt_sourcenotes {
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

#jghvsqdqpg .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#jghvsqdqpg .gt_left {
  text-align: left;
}

#jghvsqdqpg .gt_center {
  text-align: center;
}

#jghvsqdqpg .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#jghvsqdqpg .gt_font_normal {
  font-weight: normal;
}

#jghvsqdqpg .gt_font_bold {
  font-weight: bold;
}

#jghvsqdqpg .gt_font_italic {
  font-style: italic;
}

#jghvsqdqpg .gt_super {
  font-size: 65%;
}

#jghvsqdqpg .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>
<div id="jghvsqdqpg" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;"><table class="gt_table">
  <thead class="gt_header">
    <tr>
      <th colspan="3" class="gt_heading gt_title gt_font_normal" style>Ten Most Costly Weather Events</th>
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
      <td class="gt_row gt_left">4974.7784</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">hurricane/typhoon</td>
      <td class="gt_row gt_left gt_striped">$81,719</td>
      <td class="gt_row gt_left gt_striped">5350.1078</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">flash flood</td>
      <td class="gt_row gt_left">$15,222</td>
      <td class="gt_row gt_left">1334.9017</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">hail</td>
      <td class="gt_row gt_left gt_striped">$14,595</td>
      <td class="gt_row gt_left gt_striped">2496.8225</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">tropical storm</td>
      <td class="gt_row gt_left">$7,642</td>
      <td class="gt_row gt_left">677.7110</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">high wind</td>
      <td class="gt_row gt_left gt_striped">$5,248</td>
      <td class="gt_row gt_left gt_striped">633.5613</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">tsunami</td>
      <td class="gt_row gt_left">$4,680</td>
      <td class="gt_row gt_left">618.6316</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">drought</td>
      <td class="gt_row gt_left gt_striped">$1,077</td>
      <td class="gt_row gt_left gt_striped">14707.3315</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">heavy rain</td>
      <td class="gt_row gt_left">$585</td>
      <td class="gt_row gt_left">738.4198</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">frost/freeze</td>
      <td class="gt_row gt_left gt_striped">$19</td>
      <td class="gt_row gt_left gt_striped">1326.7610</td>
    </tr>
  </tbody>
  <tfoot class="gt_sourcenotes">
    <tr>
      <td class="gt_sourcenote" colspan="3">Source:  NOAA Storm Data</td>
    </tr>
  </tfoot>
  
</table></div>



Create factor variable for regions, adding territories and District of
Columbia.


```r
states <- c(state_codes$state_abbr, "AS", "GU", "PR", "VI", "DC")
valid <- econ_df$STATE %in% states
```

I filtered the data to only include rows with a valid state code.


```r
region <- c(state_codes$region_name, "Territory", 
            "Territory", "Territory","Territory", "District")
econ_df$region <- factor(econ_df$STATE, levels = states,
                         labels = region)

econ_df <- econ_df[econ_df$STATE %in% states, ]
```


```r
library(gt)
library(dplyr)
tab1 = econ_df %>% 
        tibble() %>%
        group_by(region, event) %>%
        summarize(total_property_damage = 
                          sum(units_adj_propdmg)/10^6) %>%
        arrange(desc(total_property_damage)) %>% top_n(3) %>%
        gt(rowname_col = "event") %>%
        tab_header(
        title = "Most Costly Weather Events by Region",
        subtitle = "1996-2011")  %>% 
        cols_align(align = "left") %>%
        tab_source_note(source_note = "Source:  NOAA Storm Data")  %>% 
        fmt_currency(
                columns = vars(total_property_damage), 
                currency = "USD", decimals = 0)  %>%
        cols_label(
                total_property_damage = "Total Property Damage (Millions)",
                ) %>%
        #opt_row_striping(row_striping = TRUE) %>%
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
## Selecting by total_property_damage
```

```r
print(tab1)
```

<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#rgiwhewbqb .gt_table {
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

#rgiwhewbqb .gt_heading {
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

#rgiwhewbqb .gt_title {
  color: #FFFFFF;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#rgiwhewbqb .gt_subtitle {
  color: #FFFFFF;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#rgiwhewbqb .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#rgiwhewbqb .gt_col_headings {
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

#rgiwhewbqb .gt_col_heading {
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

#rgiwhewbqb .gt_column_spanner_outer {
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

#rgiwhewbqb .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#rgiwhewbqb .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#rgiwhewbqb .gt_column_spanner {
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

#rgiwhewbqb .gt_group_heading {
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

#rgiwhewbqb .gt_empty_group_heading {
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

#rgiwhewbqb .gt_from_md > :first-child {
  margin-top: 0;
}

#rgiwhewbqb .gt_from_md > :last-child {
  margin-bottom: 0;
}

#rgiwhewbqb .gt_row {
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

#rgiwhewbqb .gt_stub {
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

#rgiwhewbqb .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#rgiwhewbqb .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#rgiwhewbqb .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#rgiwhewbqb .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#rgiwhewbqb .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#rgiwhewbqb .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#rgiwhewbqb .gt_footnotes {
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

#rgiwhewbqb .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#rgiwhewbqb .gt_sourcenotes {
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

#rgiwhewbqb .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#rgiwhewbqb .gt_left {
  text-align: left;
}

#rgiwhewbqb .gt_center {
  text-align: center;
}

#rgiwhewbqb .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#rgiwhewbqb .gt_font_normal {
  font-weight: normal;
}

#rgiwhewbqb .gt_font_bold {
  font-weight: bold;
}

#rgiwhewbqb .gt_font_italic {
  font-style: italic;
}

#rgiwhewbqb .gt_super {
  font-size: 65%;
}

#rgiwhewbqb .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>
<div id="rgiwhewbqb" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;"><table class="gt_table">
  <thead class="gt_header">
    <tr>
      <th colspan="2" class="gt_heading gt_title gt_font_normal" style>Most Costly Weather Events by Region</th>
    </tr>
    <tr>
      <th colspan="2" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style>1996-2011</th>
    </tr>
  </thead>
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1"></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">Total Property Damage (Millions)</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr class="gt_group_heading_row">
      <td colspan="2" class="gt_group_heading">West</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">flood</td>
      <td class="gt_row gt_left">$119,082</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">hail</td>
      <td class="gt_row gt_left">$4,556</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">wildfire</td>
      <td class="gt_row gt_left">$3,991</td>
    </tr>
    <tr class="gt_group_heading_row">
      <td colspan="2" class="gt_group_heading">South</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">hurricane/typhoon</td>
      <td class="gt_row gt_left">$78,941</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">storm surge/tide</td>
      <td class="gt_row gt_left">$47,791</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">tornado</td>
      <td class="gt_row gt_left">$15,480</td>
    </tr>
    <tr class="gt_group_heading_row">
      <td colspan="2" class="gt_group_heading">Midwest</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">flood</td>
      <td class="gt_row gt_left">$8,513</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">tornado</td>
      <td class="gt_row gt_left">$7,905</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">hail</td>
      <td class="gt_row gt_left">$6,170</td>
    </tr>
    <tr class="gt_group_heading_row">
      <td colspan="2" class="gt_group_heading">Northeast</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">flood</td>
      <td class="gt_row gt_left">$6,007</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">flash flood</td>
      <td class="gt_row gt_left">$4,311</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">tornado</td>
      <td class="gt_row gt_left">$756</td>
    </tr>
    <tr class="gt_group_heading_row">
      <td colspan="2" class="gt_group_heading">Territory</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">hurricane/typhoon</td>
      <td class="gt_row gt_left">$2,777</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">flash flood</td>
      <td class="gt_row gt_left">$256</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">flood</td>
      <td class="gt_row gt_left">$116</td>
    </tr>
    <tr class="gt_group_heading_row">
      <td colspan="2" class="gt_group_heading">District</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">tropical storm</td>
      <td class="gt_row gt_left">$128</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">flash flood</td>
      <td class="gt_row gt_left">$16</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">flood</td>
      <td class="gt_row gt_left">$10</td>
    </tr>
  </tbody>
  <tfoot class="gt_sourcenotes">
    <tr>
      <td class="gt_sourcenote" colspan="2">Source:  NOAA Storm Data</td>
    </tr>
  </tfoot>
  
</table></div>

#### Create plot of yearly economic damage by the top 5 most destructive weather event types.  

* Create a new data set and subset to the top 5 most damaging weather event types
* Change event to a factor variable
* Filter the data set to include only the top 5 most damaging weather event  types
* Create a panel plot of the economic damage for the top 5 most damaging weather event types
* The y axis is total property damage in log(millions of dollars).  Note that the damage from Hurricane Katrina in 2005 is so much larger than the damage from other events that it was necessary to change to a log scale.



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
##create new data frame to plot
df <- econ_df %>% group_by(year, event) %>% 
        summarize(total_property_damage = sum(units_adj_propdmg)/10^6,
                  total_crop_damage = sum(units_adj_cropdmg)/10^6) 
```

```
## `summarise()` has grouped output by 'year'. You can override using the `.groups` argument.
```

```r
str(top_five_damage)
```

```
## tibble [5 × 3] (S3: tbl_df/tbl/data.frame)
##  $ event                : chr [1:5] "flood" "hurricane/typhoon" "flash flood" "hail" ...
##  $ total_property_damage: num [1:5] 143945 81719 15222 14595 1077
##  $ total_crop_damage    : num [1:5] 4975 5350 1335 2497 14707
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
        caption = "Figure 1") +
        facet_grid(.~ event) + 
        coord_cartesian(ylim=c(0, log(m))) 
        
print(d)
```

![](noaa_files/figure-html/fig1-1.png)<!-- -->

### B.  Human Impact of Weather Events 

We were asked to explore which weather event type causes the most harm to human health. To answer this question, I first found the total number of fatalities and injries for each event type.  I then found the top ten most injurious weather types.   

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
ht_tbl <- gt(health_table)
ht_tbl <- ht_tbl %>%
 tab_header(
    title = "Weather Types Ranked by Impact on Human Health",
    subtitle = "1996-2001"
    ) 
ht_tbl <- ht_tbl %>%
  tab_source_note(
  source_note = "Source:  NOAA Storm Data"
) 
ht_tbl <- ht_tbl %>% cols_label(
  event = "Event",
  total_fatalities = "Total Fatalities",
  total_injuries = "Total Injuries"
)
ht_tbl <- opt_row_striping(ht_tbl, row_striping = TRUE)
ht_tbl <- opt_table_outline(ht_tbl)
ht_tbl
```

```{=html}
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#rsepkrzyxr .gt_table {
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

#rsepkrzyxr .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#rsepkrzyxr .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#rsepkrzyxr .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#rsepkrzyxr .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#rsepkrzyxr .gt_col_headings {
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

#rsepkrzyxr .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
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

#rsepkrzyxr .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#rsepkrzyxr .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#rsepkrzyxr .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#rsepkrzyxr .gt_column_spanner {
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

#rsepkrzyxr .gt_group_heading {
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

#rsepkrzyxr .gt_empty_group_heading {
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

#rsepkrzyxr .gt_from_md > :first-child {
  margin-top: 0;
}

#rsepkrzyxr .gt_from_md > :last-child {
  margin-bottom: 0;
}

#rsepkrzyxr .gt_row {
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

#rsepkrzyxr .gt_stub {
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

#rsepkrzyxr .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#rsepkrzyxr .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#rsepkrzyxr .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#rsepkrzyxr .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#rsepkrzyxr .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#rsepkrzyxr .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#rsepkrzyxr .gt_footnotes {
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

#rsepkrzyxr .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#rsepkrzyxr .gt_sourcenotes {
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

#rsepkrzyxr .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#rsepkrzyxr .gt_left {
  text-align: left;
}

#rsepkrzyxr .gt_center {
  text-align: center;
}

#rsepkrzyxr .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#rsepkrzyxr .gt_font_normal {
  font-weight: normal;
}

#rsepkrzyxr .gt_font_bold {
  font-weight: bold;
}

#rsepkrzyxr .gt_font_italic {
  font-style: italic;
}

#rsepkrzyxr .gt_super {
  font-size: 65%;
}

#rsepkrzyxr .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>
<div id="rsepkrzyxr" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;"><table class="gt_table">
  <thead class="gt_header">
    <tr>
      <th colspan="3" class="gt_heading gt_title gt_font_normal" style>Weather Types Ranked by Impact on Human Health</th>
    </tr>
    <tr>
      <th colspan="3" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style>1996-2001</th>
    </tr>
  </thead>
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">Event</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">Total Fatalities</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">Total Injuries</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr>
      <td class="gt_row gt_left">excessive heat</td>
      <td class="gt_row gt_right">1797</td>
      <td class="gt_row gt_right">6391</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_striped">tornado</td>
      <td class="gt_row gt_right gt_striped">1511</td>
      <td class="gt_row gt_right gt_striped">20667</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">flash flood</td>
      <td class="gt_row gt_right">887</td>
      <td class="gt_row gt_right">1674</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_striped">lightning</td>
      <td class="gt_row gt_right gt_striped">651</td>
      <td class="gt_row gt_right gt_striped">4141</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">flood</td>
      <td class="gt_row gt_right">414</td>
      <td class="gt_row gt_right">6758</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_striped">tsunami</td>
      <td class="gt_row gt_right gt_striped">289</td>
      <td class="gt_row gt_right gt_striped">3866</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">heat</td>
      <td class="gt_row gt_right">237</td>
      <td class="gt_row gt_right">1222</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_striped">winter storm</td>
      <td class="gt_row gt_right gt_striped">191</td>
      <td class="gt_row gt_right gt_striped">1292</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">thunderstorm wind</td>
      <td class="gt_row gt_right">130</td>
      <td class="gt_row gt_right">1400</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_striped">hurricane/typhoon</td>
      <td class="gt_row gt_right gt_striped">125</td>
      <td class="gt_row gt_right gt_striped">1328</td>
    </tr>
  </tbody>
  <tfoot class="gt_sourcenotes">
    <tr>
      <td class="gt_sourcenote" colspan="3">Source:  NOAA Storm Data</td>
    </tr>
  </tfoot>
  
</table></div>
```

*  Create a new data frame to plot health effects by storm type


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
       caption="Figure 2")  
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
print(g)      
```

![](noaa_files/figure-html/fig2-1.png)<!-- -->

*  Create a new data frame to plot health effects by storm type


```r
#plot <- filter(df, event %in% top_damage$event)
i<- max(plot2$total_fatalities)

h <- ggplot(data=plot2, aes(x=year)) +
       geom_col(aes(y=total_injuries, fill=event)) +
       labs(title="Total Injuries by Weather Event Type, 1996-2011",
       y="total number of persons",
       caption="Figure 3")  
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
print(h)  
```

![](noaa_files/figure-html/fig3-1.png)<!-- -->

### Conclusion


