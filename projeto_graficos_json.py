# -*- coding: utf-8 -*-
"""projeto_graficos_json.ipynb

Automatically generated by Colab.

Original file is located at
    https://colab.research.google.com/drive/1heQsiaaViu8dNPXYmASalLkhUGEk6h4x
"""

import json
import pandas as pd
from urllib.request import urlopen

url = "https://mosaicweb.com.br/aulas/python/vendas.json"

response = urlopen(url)
data_json = json.loads(response.read())

df = pd.DataFrame.from_dict(data_json)

print(df) #

df['Data da Compra']

fp = df['Data da Compra']= pd.to_datetime(df['Data da Compra'], format='%d/%m/%Y')
display(fp) #

df['Preço'].sum()

def format_number(value, prefix = ""):
  for unit in [", 'mil"]:
    if value < 1000:
      return f'{prefix}{value:.2f}{unit}'

format_number(df.shape[0])

df_rec_estado = df.groupby('Local da compra')[['Preço']].sum()

print(df_rec_estado) #

df_rec_estado = df.drop_duplicates(subset='Local da compra')[['Local da compra', 'lat', 'lon']]
print(df_rec_estado) #

df_rec_estado = df.groupby('Local da compra')[['Preço']].sum()

df_rec_estado = df.drop_duplicates(subset='Local da compra')[['Local da compra', 'lat', 'lon']].merge(df_rec_estado, left_on='Local da compra', right_index=True).sort_values('Preço', ascending=False)

print(df_rec_estado) #

import plotly.express as px

grafico_map_estado = px.scatter_geo(
    df_rec_estado,
    lat = 'lat',
    lon = 'lon',
    scope = 'south america',
    size = 'Preço',
    template = 'seaborn',
    hover_name = 'Local da compra',
    hover_data = {'lat': False, 'lon': False},
    title = 'Receita por Estado'
)

# grafico_map_estado

df_rec_mensal = df.set_index('Data da Compra').groupby(pd.Grouper(freq='M'))['Preço'].sum().reset_index()

print(df_rec_mensal) #

df_rec_mensal['Ano'] = df_rec_mensal['Data da Compra'].dt.year

print(df_rec_mensal) #

df_rec_mensal['Mes'] = df_rec_mensal['Data da Compra'].dt.month_name()

print(df_rec_mensal) #

grafico_rec_mensal = px.line(
    df_rec_mensal,
    x = 'Mes',
    y = 'Preço',
    markers = True,
    range_y = (0, df_rec_mensal.max()),
    color = 'Ano',
    line_dash = 'Ano',
    title = 'Receita Mensal'
)

grafico_rec_mensal.update_layout(yaxis_title = 'Receita')

grafico_rec_estado = px.bar(
    df_rec_estado.head(7),
    x = 'Local da compra',
    y = 'Preço',
    text_auto = True,
    title = 'Top Receita por Estados'
)

# grafico_rec_estado

df_rec_categoria = df.groupby('Categoria do Produto')[['Preço']].sum().sort_values('Preço', ascending=False)

print(df_rec_categoria) #

grafico_rec_categoria = px.bar(
    df_rec_categoria.head(7),
    text_auto = True,
    title = 'Top 7 Categorias com Maior Receita'
)

# grafico_rec_categoria

df_vendedores = pd.DataFrame(df.groupby('Vendedor')['Preço'].agg(['sum', 'count']))

print(df_vendedores) #

df_soma = df_vendedores['count'].sum()

grafico_rec_vendedores = px.bar(
    df_vendedores[['sum']].sort_values('sum', ascending=False).head(7),
    x = 'sum',
    y = df_vendedores[['sum']].sort_values('sum', ascending=False).head(7).index,
    text_auto = True,
    title = 'Top 7 Vendedores por Receita'
)

# grafico_rec_vendedores

grafico_vendas_vendedores = px.bar(
    df_vendedores[['count']].sort_values('count', ascending=False).head(7),
    x = 'count',
    y = df_vendedores[['count']].sort_values('count', ascending=False).head(7).index,
    text_auto=True,
    title = 'Top 7 Vendedores por Venda'
)

# grafico_vendas_vendedores

df_fp = df['Tipo de pagamento'].value_counts()
df_fp = pd.DataFrame(df_fp)

print(df_fp)

grafico_formas_pgmto = px.bar(
    df_fp,
    x = 'count',
    text_auto=True,
    title = 'Tipos de pagamento'
)

grafico_formas_pgmto

!pip install -q streamlit

!npm install localtunnel

import urllib

print("SENHA/Enpoint IP para o túnel local:", urllib.request.urlopen('https://ipv4.icanhazip.com').read().decode('utf8').strip("/n"))

# Commented out IPython magic to ensure Python compatibility.
# %%writefile app.py
# 
# import streamlit as st
# st.title("hell word")

!streamlit run app.py &>/content/logs.txt & npx localtunnel --port 8501

# Commented out IPython magic to ensure Python compatibility.
# %%writefile app.py
# 
# import streamlit as st
# st.set_page_config(layout='wide')
# st.title("Dashboard de Vendas :shopping_trolley:")
# 
# aba1, aba2, aba3 = st.tabs(['Dataset', 'Receita', 'Vendedores'])

# Commented out IPython magic to ensure Python compatibility.
# %%writefile app.py
# 
# import streamlit as st
# from vasco import *
# 
# st.set_page_config(layout='wide')
# st.title('Dashboard de Vendas :shopping_trolley:')
# 
# st.sidebar.title('Filtro de Vendedores')
# filtro_vendedor = st.sidebar.multiselect(
# 'Vendedores',
# df['Vendedor'].unique(),
# )
# if filtro_vendedor:
#   df = df[df['Vendedor'].isin(filtro_vendedor)]
# 
# aba1, aba2, aba3, aba4 = st.tabs(['Dataset', 'Receita', 'Vendedores', 'Formas de Pagamento'])
# 
# with aba1:
#   st.dataframe(df)
# 
# with aba2:
#   coluna1, coluna2 = st.columns(2)
# 
# with coluna1:
#   st.metric('Receita Total', format_number(df['Preço'].sum(), 'R$'))
#   st.plotly_chart(grafico_map_estado, use_container_width=True)
#   st.plotly_chart(grafico_rec_estado, use_container_width=True)
# 
# with coluna2:
#   st.metric('Quantidade de Vendas', format_number(df.shape[0]))
#   st.plotly_chart(grafico_rec_mensal, use_container_width=True)
#   st.plotly_chart(grafico_rec_categoria, use_container_width=True)
# 
# with aba3:
#   coluna3, coluna4 = st.columns(2)
# 
# with coluna3:
#   st.metric('Receita total dos vendedors', format_number(df.shape[0]))
#   st.plotly_chart(grafico_rec_vendedores, use_container_width=True)
# 
# with coluna4:
#   st.metric('Vendas totais dos vendedores', (df_soma))
#   st.plotly_chart(grafico_vendas_vendedores, use_container_width=True)
# 
# with aba4:
#   st.plotly_chart(grafico_formas_pgmto, use_container_width=True)