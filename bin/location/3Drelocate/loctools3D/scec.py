def create_event_list(infile):
    """
    Create and return a list of Event objects.

    Arguments:
    infile - the path to a SCEDC format flat file.

    Return Values:
    A list of Event objects.
    """
    from temp_core_tools import Event, Arrival
    import time as pytime
    import calendar
    infile = open(infile, 'r')
    event = None
    event_list = []
    for line in infile:
        line = line.strip().split()
        #In SCEDC format, event lines begin with '#'
        if line[0] == '#':
            if event != None: event_list += [event]
            year = int(line[1])
            month = int(line[2])
            day = int(line[3])
            hour = int(line[4])
            minute = int(line[5])
            second = float(line[6])
            lat = float(line[7])
            lon = float(line[8])
            depth = float(line[9])
            mag = float(line[10])
            orid = int(line[14])
            evid = orid
            time = calendar.timegm(pytime.struct_time(list([year,
                                          month,
                                          day,
                                          hour,
                                          minute,
                                          second,
                                          0, 0, 0])))
            event = Event(evid,
                          orid,
                          auth='SCEDC',
                          lddate=pytime.time())
            event.add_origin(lat, lon, depth, time, 'SCEDC', orid=orid, evid=evid)
            event.set_preferred_origin(orid)
        else:
            sta = line[0]
            arrtime = float(line[1]) + time
            qual = float(line[2])
            iphase = line[3]
            event.preferred_origin.arrivals += [Arrival(sta,
                                                      arrtime,
                                                      iphase)]
    return event_list
