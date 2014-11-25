def _configure_logging(logfile, level=None):
    import logging
    if level == None:
        level = logging.INFO
    elif level.upper() == 'DEBUG':
        level = logging.DEBUG
    else:
        level = logging.INFO
    for name in (__name__,
                 'loctools3D.core',
                 'loctools3D.ant',
                 'loctools3D.scec'):
        logger = logging.getLogger(name)
        logger.setLevel(level)
        if level == logging.DEBUG:
            formatter = logging.Formatter(fmt='%(asctime)s::%(levelname)s::'\
                    '%(name)s::%(funcName)s():: %(message)s',
                                          datefmt='%Y%j %H:%M:%S')
        else:
            formatter = logging.Formatter(fmt='%(asctime)s::%(levelname)s::'\
                    ' %(message)s',
                                          datefmt='%Y%j %H:%M:%S')
        if logfile:
            file_handler = logging.FileHandler(logfile)
            file_handler.setLevel(level)
            file_handler.setFormatter(formatter)
            logger.addHandler(file_handler)
        stream_handler = logging.StreamHandler()
        stream_handler.setLevel(level)
        stream_handler.setFormatter(formatter)
        logger.addHandler(stream_handler)

def _main():
    """
    Standard main() function. Execution control begins here.
    """
    from logging import getLogger
    from loctools3D.ant import pfile_2_cfg,\
                                   create_event_list,\
                                   write_origin
    from loctools3D.core import Locator,\
                                    parse_cfg,\
                                    verify_config_file

    from antelope.datascope import closing, dbopen
    args = _parse_command_line()
    if args.verbose:
        logging_level = 'DEBUG'
    else:
        logging_level = None
    _configure_logging(args.logfile, level=logging_level)
    logger = getLogger(__name__)
    pfile_2_cfg(args.pfile, '3Dreloc')
    cfg_dict = verify_config_file(parse_cfg('3Dreloc.cfg'))
    locator = Locator(cfg_dict)
    with closing(dbopen(args.db, 'r+')) as db:
        tbl_event = db.schema_tables['event']
        if args.subset:
            view = tbl_event.join('origin')
            tmp = view.subset(args.subset)
            view.free()
            view = tmp
            tbl_event = view.separate('event')
            view.free()
        for record in tbl_event.iter_record():
            evid = record.getv('evid')[0]
            view = tbl_event.subset('evid == %d' % evid)
            event_list = create_event_list(view)
            for event in event_list:
                origin = event.preferred_origin
                logger.info('[evid: %d] Relocating.' % event.evid)
                origin = locator.locate_eq(origin)
                if origin == None:
                    logger.info('[evid: %d] Could not relocate.' % event.evid)
                    continue
                logger.debug('[evid: %d] Writing origin to database.' %\
                        event.evid)
                write_origin(origin, db)
                logger.debug('[evid: %d] Finished writing origin to '\
                        'database.' % event.evid)
    return 0

def _parse_command_line():
    """
    Parse command line arguments. Return dictionary-like object
    containing results.
    """
    from argparse import ArgumentParser
    parser = ArgumentParser()
    parser.add_argument('db', type=str, help='input/output database')
    parser.add_argument('-s', '--subset', type=str, help='subset expression')
    parser.add_argument('-p', '--pfile', type=str, help='parameter file')
    parser.add_argument('-l', '--logfile', type=str, help='log file')
    parser.add_argument('-v', '--verbose', action='store_true',
                        help='increase verbosity')
    args = parser.parse_args()
    if args.logfile:
        if os.path.splitext(args.logfile)[1] != '.log':
            logfile = '%s.log' % args.logfile
    return args

if __name__ == '__main__': sys.exit(_main())
else: raise ImportError
