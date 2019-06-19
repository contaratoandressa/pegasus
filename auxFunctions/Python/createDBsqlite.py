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
        name = nameBD
        con = sqlite3.connect(name+".db")
        df=pd.read_excel(filename,sheetname=1)
        df.to_sql(sheet,con, index=False,if_exists="replace")
        con.commit()
        con.close()
    else:
        filename = nameBD
        con = sqlite3.connect(name+".db")
        wb = pd.ExcelFile(filename)
        
        for sheet in wb.sheet_names:
                df=pd.read_excel(filename,sheetname=sheet)
                df.to_sql(sheet,con, index=False,if_exists="replace")
        con.commit()
        con.close()

if __name__ == '__main__':
    filename = 'C:/Users/andressa.contarato/Desktop/dapp_2018.xlsx' 
    sheets = 1 
    nameBD = 'teste' 
    sqlite_datas(filename, sheets, nameBD)
        

        




