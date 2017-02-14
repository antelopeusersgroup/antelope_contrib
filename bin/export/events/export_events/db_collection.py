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
# pylint: disable=logging-not-lazy
from __future__ import print_function

import json
import logging

try:
    from antelope import datascope
except ImportError as ex:
    print('Do you have Antelope installed correctly?')
    print(ex)

from export_events.functions import verify_table, get_all_fields


class Document(object):
    """
    Class for creating rows storage objects.

    Store all information of a single row from a
    datascope view. Similar to a NoSQL document in
    JSON format.
    """

    def __init__(self, data):

        self.data = data

    def __str__(self):
        return "\n%s" % json.dumps(self.data)

    def __getitem__(self, name):
        if name in self.data.keys():
            return self.data[name]
        else:
            return None


class Collection(Document):
    """
    Class for maintaining a Datascope view in memory.
    """

    def __init__(self, database=None, dbpointer=None, table=None):
        '''
        Either a database descriptor or a database pointer must be provided.
        Optionally the name of a table to be verified present can be provided.
        '''

        super(Collection, self).__init__('')

        module_class = '.'.join([self.__class__.__module__,
                                 self.__class__.__name__])
        self.logger = logging.getLogger(module_class)
        self.documents = {}

        self.database = database    # database name
        self.db = dbpointer         # database pointer
        self.table = table         # database table name

        self.db = verify_table(self.table, self.database, self.db)

    def clean(self):
        '''Clear out document data.'''
        self.documents = {}

    def __str__(self):
        return self.documents.keys()

    def exists(self, name):
        '''Test if key is present in document dictionaries.'''
        return name in self.documents

    def __getitem__(self, name):
        return self.documents[name]

    def keys(self):
        '''List of keys present in document dictionaries.'''
        return self.documents.keys()

    def values(self, subset_dict=None, sort_by=None, reverse=False):
        '''
        Return values, optionally sorted and/or subsetted.
        '''
        data = self.documents.values()
        if subset_dict is not None:
            for key, value in subset_dict.items():
                if value is None:
                    continue
                data = [item for item in data if item[key] == value]
        if sort_by is not None:
            return sorted(data, key=lambda item: item[sort_by],
                          reverse=reverse)
        else:
            return data

    def get_view(self, steps, key=None):
        """
        Extract data for each row and all atributes in database view.
        """

        self.logger.debug(', '.join(steps))

        if not self.db:
            self.logger.warning('Table does not exist: %s' % self.table)
            return

        with datascope.freeing(self.db.process(steps)) as dbview:

            # Get NULL values
            dbview.record = datascope.dbNULL
            nulls = get_all_fields(dbview)

            for row in dbview.iter_record():

                data = get_all_fields(row, nulls)

                if key:
                    self.documents[data[key]] = Document(data)
                else:
                    self.documents[len(self.documents)] = Document(data)

if __name__ == "__main__":
    raise ImportError("\n\n\tAntelope's qml module. Do not run directly! **\n")
