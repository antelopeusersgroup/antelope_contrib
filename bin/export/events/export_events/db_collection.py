"""
This module is in charge of pulling information from a
Datascope table (view?) and keep all rows in memory.
We create an object with multiple methods to
interact with the databases and attributes to
keep the field information easily accessible to
the parent process.

Juan Reyes
reyes@ucsd.edu
"""

import json
import logging
import antelope.datascope as datascope

from export_events.functions import verify_table, get_all_fields


class Document():
    """
    Class for creating rows storage objects.

    Store all information of a single row from a
    datascope view. Similar to a NoSQL document in
    JSON format.
    """

    def __init__(self, data):

        self.logger = logging.getLogger(__name__)
        self.logger.debug('New Document')

        self.data = data

    def __str__(self):
        return "\n%s" % json.dumps(self.data)

    def __getitem__(self, name):
        try:
            return self.data[name]
        except:
            return ''


class Collection(Document):
    """
    Class for maintaining a Datascope view in memory.
    """

    def __init__(self, database=None, dbpointer=None, table=None):

        self.logger = logging.getLogger(__name__)
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
            return self.documents[name]
        except:
            return ''

    def keys(self, reverse=False):
        return self.documents.keys()

    def values(self, sort=False, reverse=False):
        if sort:
            return sorted(self.documents.values(),
                          key=lambda v: v[sort], reverse=reverse)
        else:
            return self.documents.values()

    def get_view(self, steps, key=None):
        """
        Open view, run commands and get entries.
        """

        self.logger.debug(', '.join(steps))

        if not self.db:
            self.logger.warning('Table does not exist: %s' % self.table)
            return

        with datascope.freeing(self.db.process(steps)) as dbview:

            # Get NULL values
            dbview.record = datascope.dbNULL
            nulls = get_all_fields(dbview)

            for r in dbview.iter_record():

                self.logger.debug('New document')
                data = get_all_fields(r, nulls)

                if key:
                    self.documents[data[key]] = Document(data)
                else:
                    self.documents[len(self.documents)] = Document(data)

if __name__ == "__main__":
    raise ImportError("\n\n\tAntelope's qml module. Do not run directly! **\n")
