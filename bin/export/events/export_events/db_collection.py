"""
This module is in charge of pulling information from a
Datascope table ( view? ) and keep all rows in memory.
We create an object with multiple methods to
interact with the databases and attributes to
keep the field information easily accessible to
the parent process.

Juan Reyes
reyes@ucsd.edu


"""

import json
import antelope.datascope as datascope

from export_events.logging_helper import getLogger
from export_events.functions import verify_table, get_all_fields

class Document():
    """
    Class for creating rows storage objects.

    Store all information of a single row from a
    datascope view. Similar to a NoSQL document in
    JSON format.

    """

    def __init__(self, *args):

        self.logging = getLogger()

        self.logging.debug('New Document')

        self.data = args[0]

    def __str__(self):
        return "\n%s" % json.dumps( self.data)

    def __getitem__(self, name):
        try:
            return self.data[ name ]
        except:
            return ''


class Collection( Document ):
    """
    Class for maintaining a Datascope view in memory.


    """

    def __init__(self, database=None, dbpointer=None, table=None ):

        self.logging = getLogger()
        self.logging.debug('New Collection')

        self.documents = {}

        self.database = database    # database name
        self.db = dbpointer         # database pointer
        self.table = table         # database table name

        self.db = verify_table(self.table, self.database, self.db)


    def clean(self):
        self.documents = {}

    def __str__(self):
        return self.documents.keys()

    def exists(self, name):
        try:
            return name in self.documents
        except:
            return False

    def __getitem__(self, name):
        try:
            return self.documents[ name ]
        except:
            return ''

    def keys(self, reverse=False):
        return  self.documents.keys()

    def values(self, sort=False, reverse=False):
        if sort:
            return  sorted(self.documents.values(), key=lambda v: v[sort] , reverse=reverse)
        else:
            return  self.documents.values()


    def get_view(self, steps, key=None):
        """
        Open view, run commands and get entries.
        """

        self.logging.debug( ', '.join(steps) )

        if not self.db:
            self.logging.warning( 'Problems with database pointer' )
            return

        with datascope.freeing(self.db.process( steps )) as dbview:

            # Get NULL values
            dbview.record = datascope.dbNULL
            nulls = get_all_fields( dbview )

            for r in dbview.iter_record():

                self.logging.debug( 'New document' )
                temp = get_all_fields( r,nulls )

                if key:
                    self.documents[ temp[ key ] ] = Document( temp )
                else:
                    self.documents[ len(self.documents) ] = Document( temp )


if __name__ == "__main__": raise ImportError( "\n\n\tAntelope's qml module. Not to run directly!!!! **\n" )
