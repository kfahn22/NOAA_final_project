---
title: "Historical Weather Event Dominates Economic Damage, while Excessive Heat Leads to Most Deaths "
author: "Kathryn Fahnline"
date: "4/12/2021"
output: 
  html_document:
    keep_md: true
---



## Synopsis

It is perhaps not surprising that the economic cost of Hurricane Katrina (2005) is an order of magnitude above all other weather events in the Storm data set.  (add more detail here)  What is perhaps unexpected is that excessive heat leads to the most deaths. As my daughter pointed out, if the news outlets warn of a major impending storm, most reasonable people make plans to move out of harm's way.  This is less likely to be true when there is a forecast of higher than normal temperatures; hence, the higher death rate.  Other the other hand, tornados lead to a greater number of injuries, perhaps due to air-born debrie.   

Does the document have a synopsis that describes and summarizes the data analysis in less than 10 sentences?


##  Data Processing

Thanks to Mentor Usama Khalil's useful post on cleaning the storm data. [https://www.coursera.org/learn/reproducible-research/discussions/all/threads/38y35MMiEeiERhLphT2-QA]  and this guide to handling the coding of CROPDMGEXP and PROPDMGEXP [https://rstudio-pubs-static.s3.amazonaws.com/58957_37b6723ee52b455990e149edde45e5b6.html]



```r
library(downloader)
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
download.file(fileUrl, destfile = "noaa.csv.bz2", method="curl")
data <- read.csv("noaa.csv.bz2")
```

### Data Transformation

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

### Data Analysis

#### We were asked to explore which weather event type causes the most economic damage? To answer this question, I first found the total property and crop damage for each event type.  I then found the top ten most destructive weather types.   

* Find summary statistics by year for each event type.


```r
econ_damage <- econ_df %>% group_by(event) %>% 
                  summarize(total_property_damage =                                             sprintf("%.2f",sum(units_adj_propdmg)/10^6),
                  total_crop_damage = sprintf("%.2f", sum(units_adj_cropdmg)/10^6) )
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
    title = "Top Ten Most Costly Weather Types, by Property and Crop Damage",
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
gt_tbl
```

```{=html}
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#acqvnemcfw .gt_table {
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
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#acqvnemcfw .gt_heading {
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

#acqvnemcfw .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#acqvnemcfw .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#acqvnemcfw .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#acqvnemcfw .gt_col_headings {
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

#acqvnemcfw .gt_col_heading {
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

#acqvnemcfw .gt_column_spanner_outer {
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

#acqvnemcfw .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#acqvnemcfw .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#acqvnemcfw .gt_column_spanner {
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

#acqvnemcfw .gt_group_heading {
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

#acqvnemcfw .gt_empty_group_heading {
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

#acqvnemcfw .gt_from_md > :first-child {
  margin-top: 0;
}

#acqvnemcfw .gt_from_md > :last-child {
  margin-bottom: 0;
}

#acqvnemcfw .gt_row {
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

#acqvnemcfw .gt_stub {
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

#acqvnemcfw .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#acqvnemcfw .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#acqvnemcfw .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#acqvnemcfw .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#acqvnemcfw .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#acqvnemcfw .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#acqvnemcfw .gt_footnotes {
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

#acqvnemcfw .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#acqvnemcfw .gt_sourcenotes {
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

#acqvnemcfw .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#acqvnemcfw .gt_left {
  text-align: left;
}

#acqvnemcfw .gt_center {
  text-align: center;
}

#acqvnemcfw .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#acqvnemcfw .gt_font_normal {
  font-weight: normal;
}

#acqvnemcfw .gt_font_bold {
  font-weight: bold;
}

#acqvnemcfw .gt_font_italic {
  font-style: italic;
}

#acqvnemcfw .gt_super {
  font-size: 65%;
}

#acqvnemcfw .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>
<div id="acqvnemcfw" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;"><table class="gt_table">
  <thead class="gt_header">
    <tr>
      <th colspan="3" class="gt_heading gt_title gt_font_normal" style>Top Ten Most Costly Weather Types, by Property and Crop Damage</th>
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
      <td class="gt_row gt_left">81718.89</td>
      <td class="gt_row gt_left">5350.11</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_striped">tropical storm</td>
      <td class="gt_row gt_left gt_striped">7642.48</td>
      <td class="gt_row gt_left gt_striped">677.71</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">lightning</td>
      <td class="gt_row gt_left">743.08</td>
      <td class="gt_row gt_left">6.90</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_striped">excessive heat</td>
      <td class="gt_row gt_left gt_striped">7.72</td>
      <td class="gt_row gt_left gt_striped">492.40</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">heavy snow</td>
      <td class="gt_row gt_left">681.99</td>
      <td class="gt_row gt_left">71.12</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_striped">heavy rain</td>
      <td class="gt_row gt_left gt_striped">585.17</td>
      <td class="gt_row gt_left gt_striped">738.42</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">high wind</td>
      <td class="gt_row gt_left">5247.86</td>
      <td class="gt_row gt_left">633.56</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_striped">tsunami</td>
      <td class="gt_row gt_left gt_striped">4680.42</td>
      <td class="gt_row gt_left gt_striped">618.63</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">strong wind</td>
      <td class="gt_row gt_left">176.99</td>
      <td class="gt_row gt_left">64.95</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_striped">flood</td>
      <td class="gt_row gt_left gt_striped">143944.83</td>
      <td class="gt_row gt_left gt_striped">4974.78</td>
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
library(ggplot2)
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

m <- max(plot$total_property_damage)

g <- ggplot(data=plot, aes(x=year, y=log(total_property_damage)))
g + geom_line(col="steelblue") +
        ggtitle("Total Property and Crop Damage, 1996-2011") +
        facet_grid(.~ event) + 
        coord_cartesian(ylim=c(0, log(m))) +
        labs(y="Total cost in log(Millions of dollars)")
```

![](noaa_files/figure-html/fig2-1.png)<!-- -->

### Which Weather Type Most Impacts Human Health?


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
) 

ht_tbl
```

```{=html}
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#uyzhcseeku .gt_table {
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
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#uyzhcseeku .gt_heading {
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

#uyzhcseeku .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#uyzhcseeku .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#uyzhcseeku .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#uyzhcseeku .gt_col_headings {
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

#uyzhcseeku .gt_col_heading {
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

#uyzhcseeku .gt_column_spanner_outer {
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

#uyzhcseeku .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#uyzhcseeku .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#uyzhcseeku .gt_column_spanner {
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

#uyzhcseeku .gt_group_heading {
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

#uyzhcseeku .gt_empty_group_heading {
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

#uyzhcseeku .gt_from_md > :first-child {
  margin-top: 0;
}

#uyzhcseeku .gt_from_md > :last-child {
  margin-bottom: 0;
}

#uyzhcseeku .gt_row {
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

#uyzhcseeku .gt_stub {
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

#uyzhcseeku .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#uyzhcseeku .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#uyzhcseeku .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#uyzhcseeku .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#uyzhcseeku .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#uyzhcseeku .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#uyzhcseeku .gt_footnotes {
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

#uyzhcseeku .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#uyzhcseeku .gt_sourcenotes {
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

#uyzhcseeku .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#uyzhcseeku .gt_left {
  text-align: left;
}

#uyzhcseeku .gt_center {
  text-align: center;
}

#uyzhcseeku .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#uyzhcseeku .gt_font_normal {
  font-weight: normal;
}

#uyzhcseeku .gt_font_bold {
  font-weight: bold;
}

#uyzhcseeku .gt_font_italic {
  font-style: italic;
}

#uyzhcseeku .gt_super {
  font-size: 65%;
}

#uyzhcseeku .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>
<div id="uyzhcseeku" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;"><table class="gt_table">
  <thead class="gt_header">
    <tr>
      <th colspan="3" class="gt_heading gt_title gt_font_normal" style>Weather Types Ranked by Impact on Human Health</th>
    </tr>
    <tr>
      <th colspan="3" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style></th>
    </tr>
  </thead>
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">event</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">total_fatalities</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">total_injuries</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr>
      <td class="gt_row gt_left">excessive heat</td>
      <td class="gt_row gt_right">1797</td>
      <td class="gt_row gt_right">6391</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tornado</td>
      <td class="gt_row gt_right">1511</td>
      <td class="gt_row gt_right">20667</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">flash flood</td>
      <td class="gt_row gt_right">887</td>
      <td class="gt_row gt_right">1674</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">lightning</td>
      <td class="gt_row gt_right">651</td>
      <td class="gt_row gt_right">4141</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">flood</td>
      <td class="gt_row gt_right">414</td>
      <td class="gt_row gt_right">6758</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tsunami</td>
      <td class="gt_row gt_right">289</td>
      <td class="gt_row gt_right">3866</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">heat</td>
      <td class="gt_row gt_right">237</td>
      <td class="gt_row gt_right">1222</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">winter storm</td>
      <td class="gt_row gt_right">191</td>
      <td class="gt_row gt_right">1292</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">thunderstorm wind</td>
      <td class="gt_row gt_right">130</td>
      <td class="gt_row gt_right">1400</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">hurricane/typhoon</td>
      <td class="gt_row gt_right">125</td>
      <td class="gt_row gt_right">1328</td>
    </tr>
  </tbody>
  
  
</table></div>
```


