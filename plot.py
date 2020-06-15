from slfpy import SLF
import plotly.graph_objects as go
import numpy as np
import plotly.io as pio
pio.orca.config.use_xvfb = True
pio.orca.config.save() 

import plotly.io as pio


def main():
    slf=SLF("res3d.slf")
    X=np.tile(slf.MESHX,slf.NPLAN)
    Y=np.tile(slf.MESHY,slf.NPLAN)
    # indices=slf.getVarsIndexes(['ELEVATION Z',"PRIVE 1"])
    indices=slf.getVarsIndexes(['ELEVATION Z',"VELOCITY U"])
    r=slf.getVariablesAt(slf.NFRAME-1,indices)
    Z=r[0]
    values=r[1]
    
    # i=np.where((X<11) & (X>9) & (Y<6) & (Y>4) )[0]
    # i=np.where((values>0)  )[0]
    # i=np.where((values>0))[0]
    i=np.where((X<20))[0]
    X=X[i].astype("f4")
    Y=Y[i].astype("f4")
    Z=Z[i].astype("f4")
    values=values[i]
    
    # values = np.random.rand(X.size)
    # values=values/values.max()
    # print(X,Y,Z,values)
    
    # print(Y[slf.NPOIN2:2*slf.NPOIN2])

    layout = go.Layout(
             scene=dict(
                 aspectmode='data'
         ))
    fig = go.Figure(layout=layout,data=go.Scatter3d(
    x=X,
    y=Y,
    z=Z,
    mode='markers',
    marker=dict(
        size=3,
        color=values,                # set color to an array/list of desired values
        # colorscale='RdBu',
        colorscale=[[0.0, "rgba(255,0,0,1.0)"],[0.5, "rgba(0,0,255,0.1)"], [0.9, "rgba(0,0,255,0.0)"],[1.0, "rgba(255,0,0,0.0)"]],
        # colorscale='Viridis',   # choose a colorscale
        # opacity=0.2
    )
    ))
    
    # X, Y, Z = np.mgrid[-5:5:40j, -5:5:40j, -5:5:40j]
    # print(X)

    # ellipsoid
    # values = X * X * 0.5 + Y * Y + Z * Z * 2
    # inn=np.arange(1000,1010)
    # print(X.flatten()[inn],Y.flatten()[inn],Z.flatten()[inn],values.flatten()[inn])
    # print(X,Y,Z,values)
    # fig = go.Figure(data=go.Volume(
    # # x=[0,1,2,0,1,2,0,1,2,0,1,2],
    # # y=[0,0,0,1,1,1,0,0,0,1,1,1],
    # # z=[0,0,0,0,0,0,1,1,1,1,1,1],
    # # value=[1,1,1,1,1,1,1,2,2,2,2,2,2],
    # x=X.flatten(),
    # y=Y.flatten(),
    # z=Z.flatten(),
    # value=values.flatten(),
    # isomin=0.0,
    # isomax=10.0,
    # surface_count=20,
    # # caps=dict(x_show=False, y_show=False)
    # opacity=0.1, # needs to be small to see through all surfaces
    # # surface_count=17, # needs to be a large number for good volume rendering
    # ))
    
    
    # fig.update_layout(
    # scene = dict(
    #     xaxis = dict(nticks=4, range=[np.min(X),np.max(X)],),
    #                  yaxis = dict(nticks=4, range=[np.min(Y),np.max(Y)],),
    #                  zaxis = dict(nticks=4, range=[np.min(Y),np.max(Z)],),),
    #               )   
    # width=700,
    # margin=dict(r=20, l=10, b=10, t=10))        
    # fig.write_html("plot/index.html")
    fig.write_html('plot/index.html')
    # fig.write_image("plot/fig1.png")
    # np.testing.assert_almost_equal(slf1.MESHX,slf2.MESHX,decimal=6)
    # np.testing.assert_almost_equal(slf1.MESHY,slf2.MESHY,decimal=6)
    # np.testing.assert_almost_equal(slf1.IKLE3,slf2.IKLE3)
    
    # obj=SLF.createGrid()
    # np.testing.assert_almost_equal(slf1.MESHX,obj['xy'][:,0],decimal=6)
    # np.testing.assert_almost_equal(slf1.MESHY,obj['xy'][:,1],decimal=6)
    # np.testing.assert_almost_equal(slf1.IKLE3,obj['ikle'])
    
    
if __name__ == "__main__":
  main()