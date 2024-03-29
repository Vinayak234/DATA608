import dash
import dash_core_components as dcc
import dash_html_components as html
import pandas as pd
import numpy as np

from dash.dependencies import Input, Output
from plotly import graph_objs as go
import plotly.io as pio
from plotly.graph_objs import *
from datetime import datetime as dt

pio.templates.default = "plotly_dark"


def get_tree_list():
    soql_url = (
        "https://data.cityofnewyork.us/resource/nwxe-4ae8.json?"
        + "$select=spc_common,count(tree_id)"
        + "&$group=spc_common"
    ).replace(" ", "%20")
    temp = pd.read_json(soql_url).dropna()
    return temp.spc_common.tolist()


def get_human_intervention(x):
    if x == "None":
        return "Nature Only"
    else:
        return "Steward Intervention"


def get_steward_graph_data(boroname="Bronx", tree="American beech"):
    soql_url = (
        "https://data.cityofnewyork.us/resource/nwxe-4ae8.json?"
        + "$select=steward,health,count(tree_id)"
        + "&$where=spc_common='"
        + tree
        + "' AND boroname='"
        + boroname
        + "'"
        + "&$group=steward,health"
    ).replace(" ", "%20")
    
    
    df = pd.read_json(soql_url).dropna().rename(columns={"count_tree_id": "n"})
    df["type"] = df.steward.apply(get_human_intervention)

    df = df.groupby(["type", "health"])["n"].sum().reset_index()

    temp = df.groupby("type")["n"].sum().reset_index().rename(columns={"n": "total"})
    df = pd.merge(df, temp)
    df["share"] = df.n / df.total * 100

    shares = {
        "Steward Intervention": {"Poor": 0.0, "Fair": 0.0, "Good": 0.0},
        "Nature Only": {"Poor": 0.0, "Fair": 0.0, "Good": 0.0},
    }

    for index, row in df.iterrows():
        shares[row["type"]][row["health"]] = row["share"]

    poor = {
        "name": "Poor",
        "type": "bar",
        "x": ["Steward Intervention", "Nature Only"],
        "y": [
            round(shares["Steward Intervention"]["Poor"], 0),
            round(shares["Nature Only"]["Poor"], 0),
        ],
        "marker": {"color": "rgb(215, 48, 39)"},
    }
    fair = {
        "name": "Fair",
        "type": "bar",
        "x": ["Steward Intervention", "Nature Only"],
        "y": [
            round(shares["Steward Intervention"]["Fair"], 0),
            round(shares["Nature Only"]["Fair"], 0),
        ],
        "marker": {"color": "rgb(254, 224, 144)"},
    }
    
    good = {
        "name": "Good",
        "type": "bar",
        "x": ["Steward Intervention", "Nature Only"],
        "y": [
            round(shares["Steward Intervention"]["Good"], 0),
            round(shares["Nature Only"]["Good"], 0),
        ],
        "marker": {"color": "rgb(39, 118, 215)"},
    }
    
    return [poor, fair, good]


def get_tree_health_graph_data(tree="American beech"):
    soql_url = (
        "https://data.cityofnewyork.us/resource/nwxe-4ae8.json?"
        + "$select=boroname,health,count(tree_id)"
        + "&$where=spc_common='"
        + tree
        + "'"
        + "&$group=boroname,health"
    ).replace(" ", "%20")
    
    df = pd.read_json(soql_url).dropna().rename(columns={"count_tree_id": "n"})
    temp = (
        df.groupby("boroname")["n"].sum().reset_index().rename(columns={"n": "total"})
    )
    
    df = pd.merge(df, temp)
    df["share"] = df.n / df.total * 100

    shares = {
        "Bronx": {"Poor": 0.0, "Fair": 0.0, "Good": 0.0},
        "Brooklyn": {"Poor": 0.0, "Fair": 0.0, "Good": 0.0},
        "Manhattan": {"Poor": 0.0, "Fair": 0.0, "Good": 0.0},
        "Queens": {"Poor": 0.0, "Fair": 0.0, "Good": 0.0},
        "Staten Island": {"Poor": 0.0, "Fair": 0.0, "Good": 0.0},
    }

    for index, row in df.iterrows():
        shares[row["boroname"]][row["health"]] = row["share"]

    poor = {
        "name": "Poor",
        "type": "bar",
        "x": ["Bronx", "Brooklyn", "Manhattan", "Queens", "Staten Island"],
        "y": [
            round(shares["Bronx"]["Poor"], 0),
            round(shares["Brooklyn"]["Poor"], 0),
            round(shares["Manhattan"]["Poor"], 0),
            round(shares["Queens"]["Poor"], 0),
            round(shares["Staten Island"]["Poor"], 0),
        ],
        "marker": {"color": "rgb(215, 48, 39)"},
    }
    fair = {
        "name": "Fair",
        "type": "bar",
        "x": ["Bronx", "Brooklyn", "Manhattan", "Queens", "Staten Island"],
        "y": [
            round(shares["Bronx"]["Fair"], 0),
            round(shares["Brooklyn"]["Fair"], 0),
            round(shares["Manhattan"]["Fair"], 0),
            round(shares["Queens"]["Fair"], 0),
            round(shares["Staten Island"]["Fair"], 0),
        ],
        "marker": {"color": "rgb(254, 224, 144)"},
    }
    good = {
        "name": "Good",
        "type": "bar",
        "x": ["Bronx", "Brooklyn", "Manhattan", "Queens", "Staten Island"],
        "y": [
            round(shares["Bronx"]["Good"], 0),
            round(shares["Brooklyn"]["Good"], 0),
            round(shares["Manhattan"]["Good"], 0),
            round(shares["Queens"]["Good"], 0),
            round(shares["Staten Island"]["Good"], 0),
        ],
        "marker": {"color": "rgb(39, 118, 215)"},
    }
    return [poor, fair, good]


app = dash.Dash(
    __name__, meta_tags=[{"name": "viewport", "content": "width=device-width"}]
)
server = app.server

# Layout of Dash App
app.layout = html.Div(
  children = [
    html.Div(
      className = "row",
      children = [
          #Column for user controls
        html.Div(
          className = "four columns div-user-controls",
          children = [
            html.Img(className = "logo", src = app.get_asset_url("dash-logo-new.png")),
            html.H2("Data608 - Module 4  - Health of Various Tree"),
            html.H2("3/22/2020"),
           

            #Change to side - by - side for mobile layout
            html.Div(
              className = "row",
              children = [
                  html.H3("Select a Tree Species"),
                  html.Div(
                  className = "div-for-dropdown",
                  children = [
                      #Dropdown to select times
                      
                    dcc.Dropdown(
                      id = "tree-dropdown",
                      options = [
                          {"label": i.title(),
                          "value": i
                        }
                        for i in get_tree_list()
                      ],
                      
                      placeholder = "Select a tree species ",
                    )
                  ],
                ),
                  html.P(""),
            html.P(""),
            html.P(""),
                  
                html.H3("Select a Borough"),  
                html.Div(
                  className = "div-for-dropdown",
                  children = [
                      #Dropdown for locations on map
                      # Dropdown for borough
                    dcc.Dropdown(
                      id = "borough-dropdown",
                      options = [{
                          "label": i,
                          "value": i
                        }
                        for i in [
                          "Bronx",
                          "Brooklyn",
                          "Manhattan",
                          "Queens",
                          "Staten Island",
                        ]
                      ],
                      placeholder = "Select a Borough",
                    )
                  ],
                ),
              ],
            ),
              
            dcc.Markdown(
              children = [
                "Source: [UBER Plotly Dash](https://dash-gallery.plotly.host/dash-uber-rides-demo/)"
              ]
            ),
          ],
        ), 
        
        #Column for app graphs and plots
        html.Div(
          className = "eight columns div-for-charts bg-grey",
          children = [
              html.Div(
              className = "text-padding",
              children = [
                "Selected Tree condition by boroughs."
              ],
            ),
            dcc.Graph(
              id = "tree-health-graph",
              figure = go.Figure(
                data = get_tree_health_graph_data(),
                layout = go.Layout(
                  yaxis = go.layout.YAxis(
                    title = go.layout.yaxis.Title(text = "Percent"),
                    range = [0, 100],
                  ),
                ),
              ),
            ),
              
            html.Div(
              className = "text-padding",
              children = [
                "Are stewards (steward activity measured by the ‘steward’ variable) having an impact on the health of trees?"
              ],
            ),
            dcc.Graph(id = "steward-graph",
              config = {
                "displayModeBar": False
              },
              figure = go.Figure(
                data = get_steward_graph_data(),
                layout = go.Layout(
                  yaxis = go.layout.YAxis(
                    title = go.layout.yaxis.Title(text = "Percent"),
                    range = [0, 100],
                  ),
                ),
              ),
            ),
          ],
        ),
      ],
    )
  ]
)
@app.callback(
    Output(component_id="tree-health-graph", component_property="figure"),
    [Input(component_id="tree-dropdown", component_property="value")],
)
def update_tree_health_graph(spc_common):
    fig = go.Figure(
        data=get_tree_health_graph_data(spc_common),
        layout=go.Layout(
            yaxis=go.layout.YAxis(
                title=go.layout.yaxis.Title(text="Percent"), range=[0, 100]
            ),
        ),
    )

    return fig


@app.callback(
    Output(component_id="steward-graph", component_property="figure"),
    [
        Input(component_id="borough-dropdown", component_property="value"),
        Input(component_id="tree-dropdown", component_property="value"),
    ],
)
def update_tree_health_graph(borough, spc_common):
    fig = go.Figure(
        data=get_steward_graph_data(borough, spc_common),
        layout=go.Layout(
            yaxis=go.layout.YAxis(
                title=go.layout.yaxis.Title(text="Percent"), range=[0, 100]
            ),
        ),
    )

    return fig




if __name__ == "__main__":
    app.run_server()
