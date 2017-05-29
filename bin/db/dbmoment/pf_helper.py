# Simple functions to help parsing values
# from parameter files.
# Removing these from the functions.py file
# so we can run without having a logger object
# defined. That object need some values from the
# parameter file to be set correctly.
#

from __main__ import *

def open_verify_pf(pf,mttime=False):
    '''
    Verify that we can get the file and check
    the value of PF_MTTIME if needed.
    Returns pf_object
    '''

    from dbmoment.logging_helper import *
    logging = getLogger('VERBOSE')

    logging.debug( 'Look for parameter file: %s' % pf )

    if mttime:
        logging.debug( 'Verify that %s is newer than %s' % (pf,mttime) )

        PF_STATUS = stock.pfrequire(pf, mttime)
        if PF_STATUS == stock.PF_MTIME_NOT_FOUND:
            logging.warning( 'Problems looking for %s. PF_MTTIME_NOT_FOUND.' % pf )
            logging.error( 'No MTTIME in PF file. Need a new version of the %s file!!!' % pf )
        elif PF_STATUS == stock.PF_MTIME_OLD:
            logging.warning( 'Problems looking for %s. PF_MTTIME_OLD.' % pf )
            logging.error( 'Need a new version of the %s file!!!' % pf )
        elif PF_STATUS == stock.PF_SYNTAX_ERROR:
            logging.warning( 'Problems looking for %s. PF_SYNTAX_ERROR.' % pf )
            logging.error( 'Need a working version of the %s file!!!' % pf )
        elif PF_STATUS == stock.PF_NOT_FOUND:
            logging.warning( 'Problems looking for %s. PF_NOT_FOUND.' % pf )
            logging.error( 'No file  %s found!!!' % pf )

        logging.debug( '%s => PF_MTIME_OK' % pf )

    try:
        return stock.pfread( pf )
    except Exception,e:
        logging.error( 'Problem looking for %s => %s' % ( pf, e ) )


def get_model_pf( mfile, path=[], forced=None):
    '''
    EARTH VELOCITY MODEL FILE:

    Need to verify if we have the listed velocity model.
    The file has a PF format but is not placed on the
    regular folder with the rest of the parameter files
    from contrib. That requires a full search on several
    paths that we get from a parameter in the dbmoment.pf
    file.
    '''
    pf = False

    from dbmoment.logging_helper import *
    logging = getLogger('VERBOSE')
    #from __main__ import logging

    # Maybe we have an entry in command-line
    if forced:
        logging.info('Get model: %s in %s' % (forced, path) )
        try:
            logging.info('Look for model: %s' % os.path.abspath(forced) )
            pf = stock.pfin(os.path.abspath(forced))
        except:
            # Maybe we should look for it on the model_path array
            mfile = forced

    # If not on command-line then get value from parameter file
    if not pf:
        logging.info('Get model: %s in %s' % (mfile, path) )
        for d in path:
            try:
                logging.info('Look for model: %s' % os.path.join(d, mfile) )
                pf = stock.pfin(os.path.join(d, mfile) )
            except:
                pass
            else:
                break # Stop if we find one

    if not pf:
        logging.error('Missing [%s] in [%s]' % ( mfile, ', '.join(path) ) )

    return pf



def safe_pf_get(pf,field,defaultval=False):
    '''
    Safe method to extract values from parameter file
    with a default value option.
    '''

    #from __main__ import logging
    from dbmoment.logging_helper import *
    logging = getLogger('VERBOSE')

    value = defaultval
    if pf.has_key(field):
        try:
            value = pf.get(field,defaultval)
        except Exception,e:
            logging.error('Problems safe_pf_get(%s,%s)' % (field,e))
            pass

    logging.debug( "pf.get(%s,%s) => %s" % (field,defaultval,value) )

    return value



