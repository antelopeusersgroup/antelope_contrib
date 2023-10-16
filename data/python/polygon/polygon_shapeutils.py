
try:
    import shapefile as shp
except Exception as __:
    print("module 'shapefile' not found")
    print("please install with the following command:")
    print("  'pip3 install pyshp'")

""" 
read and write support for shapefiles

"""

def readshapefile(shapefile_path):

        # open shape files
    sf = shp.Reader(shapefile_path)
    # load the full list of points of all shapes in a single object
    points = [[], []]
    for shape in sf.shapeRecords():
        for p in shape.shape.points:
            points[0].append(p[0])
            points[1].append(p[1])
    # convert to numpy array for faster math
    return np.array(points)

#return points
    
