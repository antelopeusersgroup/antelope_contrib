use filemail;

sub mp_filemail_handler {
	my( $message, $pfarray ) = @_;

	$database = %{$pfarray}->{database};
	$archive_dir = %{$pfarray}->{archive_dir};
	$schema = %{$pfarray}->{schema};

	dbcreate( $database, $schema );

	@db = dbopen( $database, "r+" );

	filemail( $message, $archive_dir, @db );

	dbclose( @db );
}

1;

