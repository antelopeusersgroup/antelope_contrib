demodb_path = which( 'dbexample_get_demodb_path' );
demodb_path = demodb_path( 1:( findstr( 'dbexample_get_demodb_path.m',demodb_path )-1 ) );
demodb_path = [demodb_path 'demodb/demo'];
demodb_path
