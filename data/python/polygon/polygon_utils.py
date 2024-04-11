""" the following code for polygon simplification is from Omar Estrella
        https://github.com/omarestrella/simplify.py
        adapted for the data structures used here
"""

def _getSquareDistance(p1, p2):
    """
    Square distance between two points
    """
    dx = p1[0] - p2[0]
    dy = p1[1] - p2[1]

    return dx * dx + dy * dy


def _getSquareSegmentDistance(p, p1, p2):
    """
    Square distance between point and a segment
    """
    x = p1[0]
    y = p1[1]

    dx = p2[0] - x
    dy = p2[1] - y

    if dx != 0 or dy != 0:
        t = ((p[0] - x) * dx + (p[1] - y) * dy) / (dx * dx + dy * dy)

        if t > 1:
            x = p2[0]
            y = p2[1]
        elif t > 0:
            x += dx * t
            y += dy * t

    dx = p[0] - x
    dy = p[1] - y

    return dx * dx + dy * dy


def _simplifyRadialDistance(points, tolerance):
    length = len(points)
    prev_point = points[0]
    new_points = [prev_point]

    for i in range(length):
        point = points[i]

        if _getSquareDistance(point, prev_point) > tolerance:
            new_points.append(point)
            prev_point = point

    if prev_point != point:
        new_points.append(point)

    return new_points


def _simplifyDouglasPeucker(points, tolerance):
    length = len(points)
    markers = [0] * length  # Maybe not the most efficent way?

    first = 0
    last = length - 1

    first_stack = []
    last_stack = []

    new_points = []

    markers[first] = 1
    markers[last] = 1

    while last:
        max_sqdist = 0

        for i in range(first, last):
            sqdist = _getSquareSegmentDistance(points[i], points[first], points[last])

            if sqdist > max_sqdist:
                index = i
                max_sqdist = sqdist

        if max_sqdist > tolerance:
            markers[index] = 1

            first_stack.append(first)
            last_stack.append(index)

            first_stack.append(index)
            last_stack.append(last)

        # Can pop an empty array in Javascript, but not Python, so check
        # the length of the list first
        if len(first_stack) == 0:
            first = None
        else:
            first = first_stack.pop()

        if len(last_stack) == 0:
            last = None
        else:
            last = last_stack.pop()

    for i in range(length):
        if markers[i]:
            new_points.append(points[i])

    return new_points


def simplify(points, tolerance=0.1, highestQuality=True):
    sqtolerance = tolerance * tolerance

    if not highestQuality:
        points = _simplifyRadialDistance(points, sqtolerance)

    points = _simplifyDouglasPeucker(points, sqtolerance)

    return points


"""
the following code for shapefile buffeing is from martino ferrari
        https://forum.step.esa.int/t/define-a-buffer-around-shapefile/20966

"""
def compute_angular_offset(ipoints, offset):
    """
    Simple script to create an offset polygon around any shapefile.
    More information:
        https://forum.step.esa.int/t/define-a-buffer-around-shapefile/20966

    author: martino.ferrari@c-s.fr
    license: GPLv3
    """

    try:
        import numpy as np
    except Exception as __:
        print("problem importing the numpy package")
        return
    """
    Computes the offset from a list of points using polar coordinate.

    Parameters:
    -----------
     - points: numpy.ndarray containing euclidean coordinates of the geomtry to offset
     - offset: angular offset

    Returns:
    --------
    numpy.ndarray containing the offseted geomtery
    """
    # compute center as mean point of all points
    points=np.array(ipoints)
    center = np.mean(points, 1)

    # translate coordinate system to the center
    points_orig = points.T - center
    # convert cartesian coordinate in polar coordinate
    rho = np.sqrt(np.sum(points_orig**2, 1))
    theta = np.arctan2(points_orig.T[1], points_orig.T[0])

    # setup variable for angular iteration
    N_STEPS = 100
    ANGULAR_RESOLUTION = 2 * np.pi / N_STEPS

    offseted = [[], []]

    # explore polar space
    for alpha in np.linspace(-np.pi, np.pi, N_STEPS):
        # find index of all points around the current angle alpha
        filtered = np.abs(theta - alpha) < ANGULAR_RESOLUTION
        # get filtered distances
        filt_rho = rho[filtered]
        # check if at least one point is in the current angluar step
        if len(filt_rho) > 0:
            filt_theta = theta[filtered]
            # find farther point on the list
            index = np.argmax(filt_rho)
            # compute offseted point
            xoff = (filt_rho[index] + offset) * np.cos(filt_theta[index])
            yoff = (filt_rho[index] + offset) * np.sin(filt_theta[index])
            # added to the offseted point with correct reference system
            offseted[0].append(xoff + center[0])
            offseted[1].append(yoff + center[1])
    # close the polygon
    offseted[0].append(offseted[0][0])
    offseted[1].append(offseted[1][0])
    return offseted
