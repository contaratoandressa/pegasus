# -*- coding: utf-8 -*-
"""
Created on Mon Jun 17 14:32:46 2019

@author: andressa.contarato

data collection through software Podram api
"""

# packages
import pandas as pd, requests

# function that returns a year's collection on multiple pages
def get_data(url, key, ano):
    
    response = requests.get(url, headers={'Authorization': key})
    
    try:
        response.raise_for_status()
    except Exception as e:
        log.error('Problema na coleta: {error}'.format(error = e))
    else:    
        json_data = response.json()
        output = pd.DataFrame(json_data["lstEmpenhos"])
        
    if json_data["metadados"]["qtdPaginas"] > 1:
        
        tam = json_data["metadados"]["qtdPaginas"] + 1
        
        for i in range(2,tam):
            response = requests.get('https://gatewayapi.prodam.sp.gov.br ... anoEmpenho='+str(ano)+' ... numPagina='+str(i), headers={'Authorization': key})
            json_data = response.json()
            aux = pd.DataFrame(json_data["lstEmpenhos"])
            output = output.append(aux)

    return output

# function that returns all the years os the study
def api_prodam(start, end, key):
    dados = []
    for i in range(start,end):
        url = 'https://gatewayapi.prodam.sp.gov.br ... anoEmpenho='+str(i)+'...'
        aux = get_data(url, key, i)
        if i == start:
            dados = aux
        else:
            dados = dados.append(aux)
        
    return(dados)



if __name__ == '__main__':
    
    key = ' '
    dados = api_prodam(start, end, key)
    dados.to_excel("dir/empenho.xlsx")




