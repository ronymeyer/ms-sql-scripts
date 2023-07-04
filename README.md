# ms-sql-scripts
Contains scripts, stored procedures and functions for MS SQL Server

The folder cdc contaisn a script to create a stored procedure usp_CDC_Add_Missing_Columns.sql.

This sproc, called without parameters, e.g. exec usp_CDC_Add_Missing_Columns, adds any missing columns to CDC tables.
It does so without copying all data from one CDC table to another and the triggers are always enabled, so there is no risk of not capturaing any changes.
