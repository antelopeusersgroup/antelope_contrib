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
from __future__ import (absolute_import, division, print_function)

import json
import logging
from operator import itemgetter

from past.builtins import basestring  # pylint: disable=redefined-builtin

try:
    from antelope import datascope
except ImportError as ex:
    print('Is your Antelope environment set up correctly?')
    print(repr(ex))

from export_events.functions import table_present, get_all_fields


class Document(object):
    """
    Storage class for a single row of an Antelope database view.

    Store all information of a single row from a
    datascope view. Similar to a NoSQL document in
    JSON format.
    """

    def __init__(self, data=None):

        self.data = data

    def __str__(self):
        return "\n%s" % json.dumps(self.data)

    def __contains__(self, key):
        return key in self.data.keys()

    def __getitem__(self, key):
        if key in self:
            return self.data[key]
        else:
            return None


class Collection(object):
    """
    Storage class for an Antelope database view.
    """

    def __init__(self, database=None, dbpointer=None, table=None):
        '''
        Either a database descriptor or a database pointer must be provided.

        Note
        ----
        Checking for existence of tables is done when view is constructed.
        The table property is effectively purely a label.
        '''
        self.logger = logging.getLogger(self.__class__.__name__)

        if (not isinstance(dbpointer, datascope.Dbptr) and
                isinstance(database, basestring)):
            dbpointer = datascope.dbopen(database)

        self.documents = {}
        self.db = dbpointer
        self.table = table

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

    def values(self, subset_dict=None, sort_by=None, reverse=False,
               preferred_lists=None):
        '''
        Return rows in view, optionally sorted and/or subsetted.

        Arguments
        =========
        subset_dict: dict
            a set of key-value pairs to successively select (effective AND)
        sort_by: str or list of str
            a set of keys to sort by
        preferred_lists: list or list of lists
            a mapping of sort key values to sort priorities

        Returns
        =======
        data: list of dict
            a dictionary for each row in view
        '''
        data = self.documents.values()

        if subset_dict is not None:
            for key, value in subset_dict.items():
                if value is None:
                    continue
                data = [item for item in data if item[key] == value]

        if sort_by is not None:
            if not isinstance(sort_by, list):
                sort_by = [sort_by]

            if preferred_lists is not None:

                if not isinstance(preferred_lists, list):
                    preferred_lists = [preferred_lists]
                preferred_lists = [
                    item if isinstance(item, (list, tuple)) or item is None
                    else [item]
                    for item in preferred_lists]

                scoring_list = [
                    {key: i for i, key in enumerate(preferred_list)}
                    if preferred_list is not None else {}
                    for preferred_list in preferred_lists]
                # pad scoring dicts to be as long as scoring keys
                scoring_list[len(scoring_list):len(sort_by)] = \
                    [{}]*(len(sort_by) - len(scoring_list))

                def score(item):
                    '''
                    Score items for the purpose of sorting.

                    For each item in sort_by, if there is a scoring list that
                    is used, otherwised the values is used.
                    '''
                    scores = []
                    for key, scoring_dict in zip(sort_by, scoring_list):
                        if scoring_dict != {}:
                            max_score = max(
                                [val for _, val in scoring_dict.items()]) + 1
                            if item[key] in scoring_dict:
                                scores += [scoring_dict[item[key]]]
                            else:
                                scores += [max_score]
                        else:  # fall back to regular sorting
                            scores += [item[key]]
                    return scores

                data.sort(key=score, reverse=reverse)

            else:
                data.sort(key=itemgetter(*sort_by), reverse=reverse)

        return data

    def get_view(self, steps, key=None):
        """
        Extract data for each row and all atributes in database view.
        """
        for step in steps:
            if 'dbopen' in step or 'dbjoin' in step:
                table = next(item for item in step.split()
                             if item not in ['dbopen', 'dbjoin', '-o'])
                if not table_present(self.db, table):
                    self.logger.error('Table does not exist: %s' % table)
                    return

        try:
            with datascope.freeing(self.db.process(steps)) as dbview:

                if dbview.record_count == 0:
                    self.logger.debug('Process returned empty view: ' +
                                      ', '.join(steps))
                    return
                else:
                    self.logger.debug('Processing: ' + ', '.join(steps))

                dbview.record = datascope.dbNULL
                nulls = get_all_fields(dbview)

                for i, row in enumerate(dbview.iter_record()):

                    data = get_all_fields(row, nulls)

                    if key is not None:
                        if key not in data:
                            self.logger.debug(
                                'Key "%s" not found in row %d of view. '
                                'Skipping.' % (key, i))
                            continue
                        self.documents[data[key]] = Document(data)
                    else:
                        self.documents[len(self.documents)] = Document(data)

        except datascope.DbprocessError as ex:
            self.logger.error('Processing: ' + ', '.join(steps))
            self.logger.error(repr(ex))

if __name__ == "__main__":
    raise ImportError("\n\n\tAntelope's qml module. Do not run directly! **\n")
