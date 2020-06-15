from slfpy import SLF
import plotly.graph_objects as go
import numpy as np




def main():
    slf=SLF("res3d.slf")
    X=np.tile(slf.MESHX,slf.NPLAN)
    Y=np.tile(slf.MESHY,slf.NPLAN)
    # values=np.arange(0,len(X))/len(X)
    # values=np.tile(values,slf.NPLAN)
    # indices=slf.getVarsIndexes(['ELEVATION Z',"PRIVE 1"])
    indices=slf.getVarsIndexes(['ELEVATION Z',"VELOCITY U"])
    r=slf.getVariablesAt(slf.NFRAME-1,indices)
    Z=r[0]
    values=r[1]
    
    # X, Y, Z = np.mgrid[-0:1:5j, -5:5:40j, -5:5:40j]
    
    # i=np.where((X<11) & (X>9) & (Y<6) & (Y>4) )[0]
    i=np.where((X<1) & (Y<1) & (Z<1))[0]
    # i=np.where((values>0)  )[0]

    X=X[i]
    Y=Y[i]
    Z=Z[i]
    values=values[i]
    # values=np.arange(0,len(X))/len(X)


    layout = go.Layout(
             scene=dict(
                 aspectmode='data'
         ))
    # fig = go.Figure(layout=layout,data=go.Scatter3d(
    # x=X,
    # y=Y,
    # z=Z,
    # mode='markers',
    # marker=dict(
    #     size=3,
    #     color=values,                # set color to an array/list of desired values
    #     # colorscale='RdBu',
    #     colorscale=[[0.0, "rgba(255,0,0,1.0)"],[0.5, "rgba(0,0,255,0.1)"], [0.9, "rgba(0,0,255,0.0)"],[1.0, "rgba(255,0,0,0.0)"]],
    #     # colorscale='Viridis',   # choose a colorscale
    #     # opacity=0.2
    # )
    # ))
    # X, Y, Z = np.mgrid[-5:5:40j, -5:5:40j, -5:5:40j]
    print(X.max())
    print(Y.max())
    print(Z.max())
    print(values.max())
    
    # values[:]=1.0
    # print(values)
    # nvalue=len(X)/slf.NPLAN
    # for i in range(5):
    #     values[int(i*5):int((i+1)*nvalue)]=i
    # print(X.shape)
    # print(X)
    
    fig = go.Figure(layout=layout,data=go.Volume(
    x=X,
    y=Y,
    z=Z,
    value=values,
    isomin=0,
    isomax=100,
    surface_count=10,
    # spaceframe=dict(show=True, fill=1),
    # surface=dict(show=True, fill=1,count=1),


    # caps=dict(x_show=True, y_show=True),
    # slices=dict(x_show=False, y_show=False),
    opacity=0.1, # needs to be small to see through all surfaces
    # surface_count=17, # needs to be a large number for good volume rendering
    ))
    print((values-values.min())/(values.max()-values.min()))
    # print(X,Y)
    # print(values)
    fig.write_html('plot/index.html')

    
if __name__ == "__main__":
  main()