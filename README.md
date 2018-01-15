# WebScrape-Greenwich
Monthly webscrape of planning decisions

## Motivation: 

The planning team at the GLA needs all the planning decisions every month in the London Borough of Greenwich. Greenwich posts
decisions using idox system which the planning team has been manually searching, then cutting and pasting from the webpage. The
planning team asked the GLA's data science group to look into this process to see if somehow the decisions could be quickly gathered,
and automate the gathering to fix this time consuming task. 

## Usage:

greenwich_monthly_planning_decisions_webscrape.R is the main relevant file. The process scrapes the webpage using R vest to navigate. 
There were issues with non-utf-8 text in the navigation which is why sometimes webpages are called as a text pattern and some are called
by css. The process uses a series of functions; some functions calling other functions making a loop which works more adaptevily then a fo-looop. 

## File Descriptions: 

old versions	- previous builds of the process
example_old_method_Decision_List_-_August_2017.xlsx	- the data as produced by manually cutting and copyying (train data)
greenwich_monthly_planning_decisions_webscrape.R	- the most up to date process, automated through jenkins on GIS-FME
greenwich_webscrape_DataCakesExample.R	- the process as an example at data cakes
november_decisions.csv - the data as produced by the script (test data)

## Automation Timing: 

Set to run on the first of every month to get the previous month 

## Author:

Sean O'Donnell
