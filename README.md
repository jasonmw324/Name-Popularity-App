# Name Popularity App
**Track popular baby names in the U.S. over time, by state and sex.**

## Description
This Shiny app allows users to explore baby-name popularity in the United States. Key features include:  
- Viewing the top names for a selected year, sex (male or female), and location (U.S. or a specific state).  
- Plotting the popularity of one or more names over a user-specified year range.  
- Comparing name-popularity trends across different states and sexes.

## Skills Used
- **R programming** – data manipulation, plotting, and reactive programming.  
- **Shiny** – building interactive web applications with UI and server components.  
- **Data visualization** – creating dynamic plots with `ggplot2` and `plotly`.  
- **Data Manipulation** – filtering, summarizing, and ranking data.  
- **User-friendly design** – making the app easy to run with minimal setup.  


## Features
- **Most Popular Names**: Choose a location (entire U.S. or individual state), select male or female, set how many top names to show, and pick the year.  
- **Track Name Popularity Over Time**: Choose location(s), sex, select a range of years, and enter up to eight names; the app will plot their popularity trends.  
- **Compare States**: Pick two states and a sex to compare how the top names differ between those states in a given year.

## User Interface
- A sidebar with selectors/drop-downs: location(s), sex, year/range, number of names, name input fields.  
- Main panel displays either a table of top names (for the “Most Popular” tab) or a time-series plot of popularity (for the “Track” tab).  
- Tabs to switch between tasks (top names vs. trend over time vs. compare states).

## Data Source
The underlying data comes from the U.S. Social Security Administration (SSA) baby-name dataset containing counts of babies given each name, by year, sex, and state.

## Installation & Running Locally
To run the app locally, you can either:

1. **Download the repository as a ZIP**
   - Click the green **Code** button on GitHub, then **Download ZIP**.
   - Extract the files to a folder on your computer.

2. **Run the app in R**
```r
# install required packages
install.packages(c("shiny", "tidyverse", "plotly", "DT"))  # example

# set working directory to the folder containing app.R
setwd("path/to/your/app/folder")

# launch the app
library(shiny)
runApp("app.R")




[View the app online](https://jasonmw324.shinyapps.io/namepopularityapp/)

