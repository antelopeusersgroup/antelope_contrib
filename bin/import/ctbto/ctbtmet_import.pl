sub ctbtmet_import_handler {
        my( $message, $pfarray ) = @_;

        my( $database ) = %{$pfarray}->{database};
        if( $verbose ) {
                print "Converting met bulletin\n" .
                      "\tSubject: ", $message->get("Subject"), "\n",
                      "\tFrom: ", $message->get("From"), "\n",
                      "\tto database $database\n";
        }

        open( R, "|$antelope/local/bin/ctbtmet2db - $database" );

        print R @{$message->body()};

        close( R );
}

1;
