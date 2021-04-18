---
title: "Historical Weather Event Dominates Economic Damage, while Excessive Heat Leads to Most Deaths "
author: "Kathryn Fahnline"
date: "4/12/2021"
output: 
  html_document:
    keep_md: true
---



## Synopsis

This It is perhaps not surprising that the economic cost of Hurricane Katrina (2005) is an order of magnitude above all other weather events in the Storm data set.  What is perhaps unexpected is that excessive heat leads to the most deaths. As my daughter pointed out, if the news outlets warn of a major impending storm, most reasonable people make plans to move out of harm's way. This is less likely to be true when there is a forecast of higher than normal temperatures; hence, the higher death rate.  While excessive heat causes the most deaths, tornados lead to a greater number of toal injuries and deaths.   

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
econ_damage <- econ_df %>% group_by(event) %>% 
                  summarize(total_property_damage =                                             sprintf("%.0f",sum(units_adj_propdmg)/10^6),
                  total_crop_damage = sprintf("%.0f",   sum(units_adj_cropdmg)/10^6) )

table <- econ_damage %>% arrange(desc(total_property_damage, total_crop_damage)) %>% top_n(10)
```

```
## Selecting by total_crop_damage
```


```r
library(gt)
gt_tbl <- gt(table)
gt_tbl <- gt_tbl %>%
 tab_header(
    title = "Top Ten Most Costly Weather Types, by Property and Crop Damage,              1996-2001",
    subtitle = "Millions of Dollars"
) 
gt_tbl <- gt_tbl %>%
  tab_source_note(
  source_note = "Source:  NOAA Storm Data"
)
gt_tbl <- gt_tbl %>% cols_label(
  event = "Event",
  total_property_damage = "Total Property Damage",
  total_crop_damage = "Total Crop Damage"
)
gt_tbl <- opt_row_striping(gt_tbl, row_striping = TRUE)
gt_tbl <- opt_table_outline(gt_tbl)
gt_tbl
```

```{=html}
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#dnsbjbsyzc .gt_table {
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

#dnsbjbsyzc .gt_heading {
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

#dnsbjbsyzc .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#dnsbjbsyzc .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#dnsbjbsyzc .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#dnsbjbsyzc .gt_col_headings {
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

#dnsbjbsyzc .gt_col_heading {
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

#dnsbjbsyzc .gt_column_spanner_outer {
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

#dnsbjbsyzc .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#dnsbjbsyzc .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#dnsbjbsyzc .gt_column_spanner {
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

#dnsbjbsyzc .gt_group_heading {
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

#dnsbjbsyzc .gt_empty_group_heading {
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

#dnsbjbsyzc .gt_from_md > :first-child {
  margin-top: 0;
}

#dnsbjbsyzc .gt_from_md > :last-child {
  margin-bottom: 0;
}

#dnsbjbsyzc .gt_row {
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

#dnsbjbsyzc .gt_stub {
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

#dnsbjbsyzc .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#dnsbjbsyzc .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#dnsbjbsyzc .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#dnsbjbsyzc .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#dnsbjbsyzc .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#dnsbjbsyzc .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#dnsbjbsyzc .gt_footnotes {
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

#dnsbjbsyzc .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#dnsbjbsyzc .gt_sourcenotes {
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

#dnsbjbsyzc .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#dnsbjbsyzc .gt_left {
  text-align: left;
}

#dnsbjbsyzc .gt_center {
  text-align: center;
}

#dnsbjbsyzc .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#dnsbjbsyzc .gt_font_normal {
  font-weight: normal;
}

#dnsbjbsyzc .gt_font_bold {
  font-weight: bold;
}

#dnsbjbsyzc .gt_font_italic {
  font-style: italic;
}

#dnsbjbsyzc .gt_super {
  font-size: 65%;
}

#dnsbjbsyzc .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>
<div id="dnsbjbsyzc" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;"><table class="gt_table">
  <thead class="gt_header">
    <tr>
      <th colspan="3" class="gt_heading gt_title gt_font_normal" style>Top Ten Most Costly Weather Types, by Property and Crop Damage,              1996-2001</th>
    </tr>
    <tr>
      <th colspan="3" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style>Millions of Dollars</th>
    </tr>
  </thead>
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">Event</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">Total Property Damage</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">Total Crop Damage</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr>
      <td class="gt_row gt_left">hurricane/typhoon</td>
      <td class="gt_row gt_left">81719</td>
      <td class="gt_row gt_left">5350</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_striped">excessive heat</td>
      <td class="gt_row gt_left gt_striped">8</td>
      <td class="gt_row gt_left gt_striped">492</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tropical storm</td>
      <td class="gt_row gt_left">7642</td>
      <td class="gt_row gt_left">678</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_striped">lightning</td>
      <td class="gt_row gt_left gt_striped">743</td>
      <td class="gt_row gt_left gt_striped">7</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">heavy snow</td>
      <td class="gt_row gt_left">682</td>
      <td class="gt_row gt_left">71</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_striped">heavy rain</td>
      <td class="gt_row gt_left gt_striped">585</td>
      <td class="gt_row gt_left gt_striped">738</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">high wind</td>
      <td class="gt_row gt_left">5248</td>
      <td class="gt_row gt_left">634</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_striped">tsunami</td>
      <td class="gt_row gt_left gt_striped">4680</td>
      <td class="gt_row gt_left gt_striped">619</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">strong wind</td>
      <td class="gt_row gt_left">177</td>
      <td class="gt_row gt_left">65</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_striped">flood</td>
      <td class="gt_row gt_left gt_striped">143945</td>
      <td class="gt_row gt_left gt_striped">4975</td>
    </tr>
  </tbody>
  <tfoot class="gt_sourcenotes">
    <tr>
      <td class="gt_sourcenote" colspan="3">Source:  NOAA Storm Data</td>
    </tr>
  </tfoot>
  
</table></div>
```

#### Create plot of yearly economic damage by the top 5 most destructive weather event types.  

* Create a new data set and subset to the top 5 most damaging weather event types
* Change event to a factor variable
* Filter the data set to include only the top 5 most damaging weather event  types
* Create a panel plot of the economic damage for the top 5 most damaging weather event types
* The y axis is total property damage in log(millions of dollars).  Note that the damage from Hurricane Katrina in 2005 is so much larger than the damage from other events that it was necessary to change to a log scale.


```r
library(dplyr)
top_damage <- filter(econ_damage, event %in% official_event_names) %>%
          arrange(desc(total_property_damage, total_crop_damage)) %>% top_n(5)
```

```
## Selecting by total_crop_damage
```

```r
##create new data frame to plot
df <- econ_df %>% group_by(year, event) %>% 
        summarize(total_property_damage = sum(units_adj_propdmg/10^6),
                  total_crop_damage = sum(units_adj_cropdmg/10^6)) 
```

```
## `summarise()` has grouped output by 'year'. You can override using the `.groups` argument.
```

```r
plot <- filter(df, event %in% top_damage$event)
plot$event <- as.factor(plot$event)

str(plot)
```

```
## tibble [80 × 4] (S3: grouped_df/tbl_df/tbl/data.frame)
##  $ year                 : num [1:80] 1996 1996 1996 1996 1996 ...
##  $ event                : Factor w/ 5 levels "heavy rain","heavy snow",..: 1 2 3 4 5 1 2 3 4 5 ...
##  $ total_property_damage: num [1:80] 34.605 133.818 34.403 0.256 46.3 ...
##  $ total_crop_damage    : num [1:80] 0.004 0.22 0.0518 0 1 ...
##  - attr(*, "groups")= tibble [16 × 2] (S3: tbl_df/tbl/data.frame)
##   ..$ year : num [1:16] 1996 1997 1998 1999 2000 ...
##   ..$ .rows: list<int> [1:16] 
##   .. ..$ : int [1:5] 1 2 3 4 5
##   .. ..$ : int [1:5] 6 7 8 9 10
##   .. ..$ : int [1:5] 11 12 13 14 15
##   .. ..$ : int [1:5] 16 17 18 19 20
##   .. ..$ : int [1:5] 21 22 23 24 25
##   .. ..$ : int [1:5] 26 27 28 29 30
##   .. ..$ : int [1:5] 31 32 33 34 35
##   .. ..$ : int [1:5] 36 37 38 39 40
##   .. ..$ : int [1:5] 41 42 43 44 45
##   .. ..$ : int [1:5] 46 47 48 49 50
##   .. ..$ : int [1:5] 51 52 53 54 55
##   .. ..$ : int [1:5] 56 57 58 59 60
##   .. ..$ : int [1:5] 61 62 63 64 65
##   .. ..$ : int [1:5] 66 67 68 69 70
##   .. ..$ : int [1:5] 71 72 73 74 75
##   .. ..$ : int [1:5] 76 77 78 79 80
##   .. ..@ ptype: int(0) 
##   ..- attr(*, ".drop")= logi TRUE
```


```r
library(ggplot2)
m <- max(plot$total_property_damage)
p <- ggplot(data=plot, aes(year)) +
  geom_line(aes(y=log(total_property_damage), 
                colour="total_property_damage")) +                geom_line(aes(y=log(total_crop_damage),                                                       colour = "total_crop_damage")) +
        labs(y="total cost", 
        title="Total Property and Crop Damage, 1996-2011", 
        subtitle=("log(Millions of US Dollars)"), 
        caption = "Figure 1") +
        facet_grid(.~ event) + 
        coord_cartesian(ylim=c(0, log(m))) 
        
print(p)
```

![](noaa_files/figure-html/fig1-1.png)<!-- -->

### B.  Human Impact of Weather Events 

We were asked to explore which weather event type causes the most harm to human health. To answer this question, I first found the total number of fatalities and injries for each event type.  I then found the top ten most injurious weather types.   

* Find total fatalities and injuries damage by year for each event type and rank to find the 10 most expensive types.


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

#axyyypygpy .gt_table {
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

#axyyypygpy .gt_heading {
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

#axyyypygpy .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#axyyypygpy .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#axyyypygpy .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#axyyypygpy .gt_col_headings {
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

#axyyypygpy .gt_col_heading {
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

#axyyypygpy .gt_column_spanner_outer {
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

#axyyypygpy .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#axyyypygpy .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#axyyypygpy .gt_column_spanner {
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

#axyyypygpy .gt_group_heading {
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

#axyyypygpy .gt_empty_group_heading {
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

#axyyypygpy .gt_from_md > :first-child {
  margin-top: 0;
}

#axyyypygpy .gt_from_md > :last-child {
  margin-bottom: 0;
}

#axyyypygpy .gt_row {
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

#axyyypygpy .gt_stub {
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

#axyyypygpy .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#axyyypygpy .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#axyyypygpy .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#axyyypygpy .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#axyyypygpy .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#axyyypygpy .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#axyyypygpy .gt_footnotes {
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

#axyyypygpy .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#axyyypygpy .gt_sourcenotes {
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

#axyyypygpy .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#axyyypygpy .gt_left {
  text-align: left;
}

#axyyypygpy .gt_center {
  text-align: center;
}

#axyyypygpy .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#axyyypygpy .gt_font_normal {
  font-weight: normal;
}

#axyyypygpy .gt_font_bold {
  font-weight: bold;
}

#axyyypygpy .gt_font_italic {
  font-style: italic;
}

#axyyypygpy .gt_super {
  font-size: 65%;
}

#axyyypygpy .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>
<div id="axyyypygpy" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;"><table class="gt_table">
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
health_df <- data %>% group_by(year, event) %>% 
        summarize(total_fatalities = sum(FATALITIES),
                  total_injuries = sum(INJURIES))  %>% 
        arrange(desc(total_fatalities, total_injuries)) %>% top_n(5)
```

```
## `summarise()` has grouped output by 'year'. You can override using the `.groups` argument.
```

```
## Selecting by total_injuries
```

```r
m <- max(health_df$total_fatalities)

g <- ggplot(data=health_df, aes(x=year, y=total_fatalities))
g + geom_line(col="red") +
        ggtitle("Total Fatalities, 1996-2011") +
        facet_grid(.~ event) + 
        coord_cartesian(ylim=c(0, log(m))) +
        labs(y="Total Fatalities")
```

```
## geom_path: Each group consists of only one observation. Do you need to adjust
## the group aesthetic?
```

```
## geom_path: Each group consists of only one observation. Do you need to adjust
## the group aesthetic?
```

![](noaa_files/figure-html/fig2-1.png)<!-- -->

