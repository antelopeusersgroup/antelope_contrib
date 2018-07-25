template <typename Tdata> class DataSetReader : public BasicObjectReader
{
public:
  DataSetReader(list<string>fnames,char form='b',bool drop_failed=false);
  DataSetReader(PfStyleMetadata& pf);
  bool good(){return ok;};
  long number_available(){return number_objects;};
  Tdata read();
  Tdata read(long object_number);
private:
  bool ok;
  /* We compute this in constructor and cache it as it.  Implementation dependent
  on how to get it. */
  long number_objects;
  /* Every time a read is issued this is set.   That allows sequential
  read through the read method with no arg. Serves no purpose in rando access. */
  long current_position;
  /* The data set index is defined by this simple vector for now.
  The assumption is that the number of files is not large.   If it were
  we would have bigger problems with the too many open files for the filehandles
  vector.   The first element of the pair is the dataset offset for handle i.
  The "second" is the offset of the last data member in file i.  That
  creates a reasonably fast linear search for any request. */
  vector<pair<long,long>> ranges;
  vector<shared_ptr<IndexedObjectReader<Tdata>> filehandles;
  /* This private method has common code for constructors.  Avoids
  reptitious code. */
  void CoreDataSetBuilder(list<string> fnames, char form, bool drop_failed);
};
template <typename Tdata>
   DataSetReader::CoreDataSetBuilder(list<string> fnames, char form, bool drop_failed)
{
  const string base_error("DataSetReader core construction:  ");
  try{
    shared_ptr<IndexedObjectReader> nexthandle;
    list<string>::iterator fptr;
    for(fptr=fnames.begin();fptr!=fnames.end();++fptr)
    {
      try{
        nexthandle=shared_ptr<IndexedObjectReader>(new IndexedObjectReader(*sptr));
        filehandles.push_back(nexthandle);
      }catch(SeisppError& serr)
      {
        if(drop_failed)
        {
          cerr << base_error
            << "Warning - failed trying to create handle for index file ="
            << (*fptr) <<endl<<"Message posted:"<<endl;
          serr.log_error();
        }
        else
        {
          /* May want to clean up any already open readers here.  For now assume
          this will always cause a termination so we don't need to worry about
          carrying on with stale file handles */
          throw serr;
        }
      }
    }
    if(filehandles.size()<=0)
    {
      throw SeisppError(base_error + "Open failed on all files defining this dataset");
    }
    /* Now compute offsets to define the range of index values into the
    complete data set */
    int i;
    long j,j0,je;
    for(i=0,j=0;i<filehandles.size();++i)
    {
      j0=j;
      je=j0 + filehandles[i]->number_available() - 1;
      pair<long,long> r(j0,je);
      ranges.push_back(r);
      j+=file_handles[i]->number_available();
    }
    ok=true;
  }catch(...){throw;};
}
template <typename Tdata>
   DataSetReader::DataSetReader(list<string> fnames, char form, bool drop_failed)
{
  try{
    /* This constructor is only wrapper on this private method. */
    this->CoreDataSetBuilder(list<string> fnames, char form, bool drop_failed);
  }catch(...){throw;};
}
/* The pf file constructor uses the pf to assemble the args sent to
CoreDataSetBuilder */
template <typename Tdata> DataSetReader::DataSetReader(PfStyleMetadata& pf)
{
  const string base_error("DataSetReader pf constructor:  ");
  try{
    list<string> fnames=pf.get_tbl("IndexFileList");
    string format=pf.get_string("data_format");
    if(format=="binary")
       form='b';
    else
    {
      /* Not best practice, but with this implementation text format is
      not allowed.   Hence if we get aything else we throw an error */
      throw SeisppError(base_error + "Illegal format requested="
             + format + "\nCurrent implementation requres binary");
    }
    bool drop=pf.get_bool("drop_failed");
    this->CoreDataSetBuilder(fnames,form,drop);
  }catch(...){throw;};
}
template <typename Tdata> Tdata DataSetReader::read(long object_number)
{
  const string base_error("DataSetReader::read(object_number) method: ")
  try{
    current_position=object_number;
    if(current_position>=number_objects)
      throw SeisppError(base_error + "Error - attempted to read beyond end of data set\n"
         + "One of index files may be corrupted");
    int nfiles=filehandles.size();
    int i;
    pair<long,long> r=ranges[i];
    /* linear search */
    for(i=0;i<nfiles;++i)
    {
      if((object_number>=r.first) && (object_number<=r.second)) break;
    }
    long j;
    j=object_number-r.first;
    return filehandles[i]->read(j);
  }catch(...){throw;};
};
template <typename Tdata> Tdata DataSetReader::read()
{
  const string base_error("DataSetReader::read() method: ")
  try{
    ++current_position;
    if(current_position>=number_objects)
      throw SeisppError(base_error + "Error - attempted to read past end of data set");
    return this->read(current_position);
  }catch(...){throw;};
};
