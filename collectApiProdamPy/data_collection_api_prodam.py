#####################
# packages
#####################

import pandas as pd, requests, logging, argparse, time

#####################
# coleta de contrato
#####################

def coleta_contratos(url, key, year):
    
    response = requests.get(url, headers={'Authorization': key})
    
    try:
        response.raise_for_status()
    except Exception as e:
        log.error('Problema na coleta: {error}'.format(error = e))
    else:    
        json_data = response.json()
        # contratos
        output = pd.DataFrame(json_data["lstContratos"])
        
    if json_data["metadados"]["qtdPaginas"] > 1:
        
        tam = json_data["metadados"]["qtdPaginas"] + 1
        
        for i in range(2,tam):
            print('pagina: ' + str(i))
            # contrato
            response = requests.get('https://gatewayapi.prodam.sp.gov.br:443/ ... anoContrato='+str(year)+'&numPagina=' + str(i) + '...'  , headers={'Authorization': key})
            json_data = response.json()
            aux = pd.DataFrame(json_data["lstContratos"])
            output = output.append(aux)

    return output

def api_prodam(start, end, key):
    dados = []
    for i in range(start,end):
        print('ano: ' + str(i))
        # contrato
        url = 'https://gatewayapi.prodam.sp.gov.br:443/ ... anoContrato='+str(i)+'...'
        aux = coleta_contratos(url, key, i)
        if i == start:
            dados = aux
        else:
            dados = dados.append(aux)
        
    return(dados)
    

#####################
# coleta de empenho
#####################

def select_empenho(url, key, year, cod):
    
    output = pd.DataFrame()

    try:
        response = requests.get(url, headers={'Authorization': key})
        response.raise_for_status()
        
    except Exception as e:
        
        log.error('Problema na coleta: {error}'.format(error = e))
        pd.DataFrame([url], columns=['URL']).to_csv('missed_urls.csv', mode='a', index=False,  header=False)

        if 'HTTPSConnectionPool' in str(e):
            time.sleep(5*60)
    
    else:
        
        json_data = response.json()
        
        # empenho
        output = pd.DataFrame(json_data["lstEmpenhos"])
    
        if json_data["metadados"]["qtdPaginas"] > 1:
            
            tam = json_data["metadados"]["qtdPaginas"] + 1
            
            for i in range(2,tam):
                
                log.error('pagina: ' + str(i))
                response = requests.get('https://gatewayapi.prodam.sp.gov.br:443/ ... anoEmpenho='+ str(year) +' ... codContrato='+ str(cod) +' ... numPagina=' + str(i)  , headers={'Authorization': key})
                log.error('url select_empenho: ' + url)
                json_data = response.json()
                aux = pd.DataFrame(json_data["lstEmpenhos"])
                output = output.append(aux)
           
    return output    

def consolida_empenho(dirContrato, key, year):
    
    contrato = pd.read_excel(dirContrato)
    contrato = contrato[contrato['anoExercicio'] == year]

    dados = []
    
    for i in range(0,contrato.shape[0]):
        
        log.info('contrato: ' + str(contrato['codContrato'].iloc[i]) + ' ano: '+ str(year))
        
        url = 'https://gatewayapi.prodam.sp.gov.br:443/ ... anoEmpenho='+str(year)+' ... codContrato='+str(contrato['codContrato'].iloc[i])+'...'
        aux = select_empenho(url = url, key = key, year = year, cod = contrato['codContrato'].iloc[i])

        if aux.empty:
            next
        
        if i == 0:
            dados = aux
        else:
            dados = dados.append(aux)
        
    return(dados)
    
def consolida_ano_empenho(dirContrato, key, start, end):
    
    
    dados = []
    
    for i in range(start, end):
        
        log.error('ano: ' + str(i))
        
        aux = consolida_empenho(dirContrato, key, i)
        
        if i == start:
            dados = aux
        else:
            dados = dados.append(aux)
        
    return(dados)

#####################
# automatization
#####################

if __name__ == '__main__':
    
    logging.basicConfig(filename = 'name.log',level=logging.INFO)
    log = logging.getLogger()

    log.info(f'description.')
    key = ' '
    dirContrato = 'dir/nameData.xlsx'
    start = 
    end = 
    dados = api_prodam(start, end, key)
    dados.to_excel('dir/nameData.xlsx')
    dados = consolida_ano_empenho(dirContrato, key, start, end)
    dados.to_excel('dir/nameData.xlsx')

