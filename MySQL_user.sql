mysql -u Rania061 -pRania061 -h localhost Rania_db

-- Create database if it does not exist
CREATE DATABASE IF NOT EXISTS Rania_db;
USE Rania_db;

-- Drop table if already exists (optional safety)
DROP TABLE IF EXISTS sales_data;

-- Create table structure based on your CSV columns
CREATE TABLE sales_data (
    ordernumber INT,
    quantityordered INT,
    priceeach DECIMAL(10,2),
    orderlinenumber INT,
    sales DECIMAL(12,2),
    status VARCHAR(20),
    qtr_id INT,
    month_id INT,
    year_id INT,
    productline VARCHAR(50),
    msrp INT,
    productcode VARCHAR(50),
    customername VARCHAR(100),
    phone VARCHAR(50),
    addressline1 VARCHAR(100),
    addressline2 VARCHAR(100),
    city VARCHAR(50),
    state VARCHAR(50),
    postalcode VARCHAR(20),
    country VARCHAR(50),
    territory VARCHAR(50),
    contactlastname VARCHAR(50),
    contactfirstname VARCHAR(50),
    dealsize VARCHAR(20),
    order_date DATE
);

-- Load data from CSV file into the table
-- IMPORTANT: Update the file path below to where your Sales_data.csv is located
LOAD DATA INFILE '/path/to/Sales_data.csv'
INTO TABLE sales_data
FIELDS TERMINATED BY ','
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
IGNORE 1 ROWS
(ordernumber, quantityordered, priceeach, orderlinenumber, sales, status, qtr_id, month_id, year_id, productline, msrp, productcode, customername, phone, addressline1, addressline2, city, state, postalcode, country, territory, contactlastname, contactfirstname, dealsize, @order_date)
SET order_date = STR_TO_DATE(@order_date, '%m/%d/%Y');
