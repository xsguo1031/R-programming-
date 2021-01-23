### Access Principal Data ----------------------------------------------------------------------------------
library(RPostgreSQL)
library(DBI) 

# load the postgres driver
pg = dbDriver('PostgreSQL') 

# enter your credentials 
user = 'Capstone_User'
pw = '5ZpMmX!jn0h3'
host = 'qc-aurora.coh4objazhte.us-east-1.rds.amazonaws.com'
name = 'qcdb'
port = 5432

# create a connection object
conn = dbConnect(drv = pg, user = user, password = pw, host = host, port = port, dbname = name) 

# view list of tables
dbListTables(conn)

# read in the data from some of the tables
factor_weekly = dbGetQuery(conn = conn, statement = 'select * from factor_weekly')
#factor_monthly = dbGetQuery(conn = conn, statement = 'select * from factor_monthly')
#macro_daily = dbGetQuery(conn = conn, statement = 'select * from macro_daily')
#macro_monthly = dbGetQuery(conn = conn, statement = 'select * from macro_monthly')
returns_excess_weekly = dbGetQuery(conn = conn, statement = 'select * from returns_excess_weekly')
returns_weekly = dbGetQuery(conn = conn, statement = 'select * from returns_weekly')
#GICS = dbGetQuery(conn = conn, statement = 'select * from GICS')
