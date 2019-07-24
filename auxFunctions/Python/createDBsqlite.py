# -*- coding: utf-8 -*-
"""
Created on Tue Jun 18 11:48:59 2019

@author: andressa.contarato

Insert datasets into sqlite according the data's nature
"""

# packages
import sqlite3, pandas as pd, os

# function that returns a sqlite for each datasets
def sqlite_datas(filename, sheets, nameBD):
    
    if sheets==1:
        
        con = sqlite3.connect(nameBD+".db")
        #df=pd.read_excel(filename,skiprows=1)
        df = pd.read_csv(filename, encoding = 'latin-1', sep = ',')
        print('Dimensao da base de dados: '+str(df.shape))
        df.to_sql(nameBD,con, index=False,if_exists="replace")
        con.commit()
        con.close()
        
    else:
        con = sqlite3.connect(nameBD+".db")
        wb = pd.ExcelFile(filename)
        
        for sheet in wb.sheet_names:
                df=pd.read_excel(filename,sheet_name=sheets)
                df.to_sql(sheet,con, index=False,if_exists="replace")
                
        con.commit()
        con.close()

        
def select_columns_data(filenameDB, query):
    conn = sqlite3.connect(filenameDB)
    c = conn.cursor()
    c.execute(query)
    rows = c.fetchall()
    rows = pd.DataFrame(rows)
    return(rows)


if __name__ == '__main__':
    # DD
    # filename = 'C:/Users/andressa.contarato/Desktop/DD_total_limpeza.csv' 
    # ISP
    filename = 'dir/database.csv'
    sheets = 1 
    nameBD = 'database' 
    sqlite_datas(filename, sheets, nameBD)
    #filenameDB = 'C:/Users/andressa.contarato/Desktop/DD.db'
    #query = 'SELECT * FROM DD'
    #data = select_columns_data(filenameDB, query)
        

        




