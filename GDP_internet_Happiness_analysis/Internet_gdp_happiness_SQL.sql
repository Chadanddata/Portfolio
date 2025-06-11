/*CREATE DATABASE "GDP, Internet and Happiness"
    WITH
    OWNER = postgres
    ENCODING = 'UTF8'
    LC_COLLATE = 'English_United States.1252'
    LC_CTYPE = 'English_United States.1252'
    LOCALE_PROVIDER = 'libc'
    TABLESPACE = pg_default
    CONNECTION LIMIT = -1
    IS_TEMPLATE = False;*/

/*CREATE TABLE GDP_per_capita (
Country char(50),
YEAR2021 numeric,
YEAR2020 numeric,
YEAR2019 numeric,
YEAR2018 numeric,
YEAR2017 numeric,
YEAR2016 numeric,
YEAR2015 numeric,
YEAR2014 numeric,
YEAR2013 numeric
);

COPY GDP_per_capita
FROM 'C:\Users\chadh\Downloads\GDP_per_capita.csv'
DELIMITER ',' CSV HEADER; */

/*CREATE TABLE average_wages (
Country char(50),
YEAR2021 numeric,
YEAR2020 numeric,
YEAR2019 numeric,
YEAR2018 numeric,
YEAR2017 numeric,
YEAR2016 numeric,
YEAR2015 numeric,
YEAR2014 numeric,
YEAR2013 numeric
);

COPY average_wages
FROM 'C:\Users\chadh\Downloads\average_after_tax_wages.csv'
DELIMITER ',' CSV HEADER; */

/*CREATE TABLE internet_adoption(
Country char(50),
YEAR2020 numeric,
YEAR2019 numeric,
YEAR2018 numeric,
YEAR2017 numeric,
YEAR2016 numeric,
YEAR2015 numeric,
YEAR2014 numeric,
YEAR2013 numeric
);

COPY internet_adoption
FROM 'C:\Users\chadh\Downloads\internet_adoption.csv'
DELIMITER ',' CSV HEADER; */ 

/*CREATE TABLE happiness_2015 (
Country char(50),
Region char(50),
Happiness_rank int,
Happiness_score numeric,
Standard_error numeric,
Economy numeric,
Family numeric,
Health numeric,
Freedom numeric,
Trust numeric,
Generosity numeric, 
Dystopia numeric
);

COPY happiness_2015
FROM 'C:\Users\chadh\Downloads\2015.csv'
DELIMITER ',' CSV HEADER; */

/*CREATE TABLE happiness_2019 (
happiness_rank int,
Country char(50),
Happiness_score numeric,
GDP numeric,
Social_support numeric,
Health numeric,
Freedom numeric,
Generosity numeric, 
Trust numeric
);

COPY happiness_2019
FROM 'C:\Users\chadh\Downloads\2019.csv'
DELIMITER ',' CSV HEADER;*/



SELECT ia.country, 
       ia.year2020 AS internet_use_proportion, 
	   (ia.year2019 - ia.year2015)AS five_yr_internet_growth, 
	   ROUND(((ia.year2019 - ia.year2015) / ia.year2019),2) AS internet_growth_percent,
	   gpc.year2019 AS GDP,
	   (gpc.year2019 - gpc.year2015) AS five_yr_gdp_growth,
	   ROUND(((gpc.year2019 - gpc.year2015)/ gpc.year2019),2) AS GDP_growth_percent,
	   aw.year2019 AS avg_wages,
	   (aw.year2019 - aw.year2015) AS five_yr_wage_growth,
	   ROUND(((aw.year2019 - aw.year2015)/ aw.year2019),2) AS wage_growth_percent,
	   h19.happiness_rank AS happiness_score,
	   (h19.happiness_rank - h15.happiness_score) AS happiness_change
FROM internet_adoption ia
JOIN GDP_per_capita gpc ON (ia.country = gpc.country)
JOIN average_wages aw ON (ia.country = aw.country)
LEFT JOIN happiness_2019 h19 ON (ia.country = h19.country)
LEFT JOIN happiness_2015 h15 ON (ia.country = h15.country)