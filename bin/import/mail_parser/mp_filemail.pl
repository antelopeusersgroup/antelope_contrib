use filemail;

sub mp_filemail_handler {
	my( $message, $pfarray ) = @_;

	$database = %{$pfarray}->{database};
	$archive_dir = %{$pfarray}->{archive_dir};
	$schema = %{$pfarray}->{schema};
	$dirmode = %{$pfarray}->{dirmode};
	$filemode = %{$pfarray}->{filemode};

	$filemail::Dirmode = $dirmode;
	$filemail::Filemode = $filemode;

	dbcreate( $database, $schema );

	@db = dbopen( $database, "r+" );

	filemail( $message, $archive_dir, @db );

	dbclose( @db );
}

1;

