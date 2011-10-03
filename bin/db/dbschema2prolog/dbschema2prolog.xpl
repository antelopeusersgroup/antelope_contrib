use Datascope;

use Getopt::Std;

%Typetrans = (
	"dbREAL"	=> "real",	
	"dbINTEGER" 	=> "integer",	
	"dbSTRING" 	=> "string",	
	"dbTIME" 	=> "time",	
	"dbYEARDAY" 	=> "yearday",	
	"dbBOOLEAN" 	=> "boolean",	
	"dbDBPTR" 	=> "dbptr",	
);

sub token_escape {
	my( $string ) = @_;
	
	if( ! defined( $string )  ) {
		return undef;
	}

	$string =~ s@&@&amp;@g;
	$string =~ s@<@&lt;@g;
	$string =~ s@>@&gt;@g;
	$string =~ s@"@&quot;@g;
	
	return $string;
}

sub format_detail {
	my( $detail ) = @_;

	if( ! defined( $detail )  ) {
		return undef;
	}

	chomp( $detail );

	$detail = token_escape( $detail );

	$spacer = "\t    ";
	$detail =~ s/^/$spacer/g;
	$detail =~ s/\n/\n$spacer/g;

	return $detail;
}

sub tempdb {
	my( $schema ) = @_;

	$dbname = "/tmp/db_$<_$$";

	open( D, ">$dbname" );
	print D "#\nschema $schema\n";
	close( D );

	return $dbname;
}

sub missing_schema {
	my( $total_schema ) = @_;

	# Allow for expansion schemas
	my( @schema_components ) = split( ":", $total_schema );

	COMPONENT: foreach $component ( @schema_components ) {

		next COMPONENT if( -e "$component" ); 

		if( defined( $ENV{SCHEMA_DIR} ) ) {

			foreach $dir ( split( ":", $ENV{SCHEMA_DIR} ) ) {

				next COMPONENT if( -e "$dir/$component" );
			}
		}

		return $component;
	}

	return undef;
}

if( @ARGV != 1 ) {

	die( "Usage: dbschema2prolog schema\n" );

} else {

	$schema = "$ARGV[0]";

	if( defined( $component = missing_schema( $schema ) ) ) {

		die( "Couldn't find schema $component\n" ); 
	}
}

$dbname = tempdb( $schema );

@db = dbopen( "$dbname", "r" );

@schema_tables = dbquery( @db, "dbSCHEMA_TABLES" );

foreach $table ( sort( @schema_tables ) ) {

	$relations{$table} =  [ dblookup( @db, "", "$table", "", "" ) ];
	
	@attributes = dbquery( @{$relations{$table}}, "dbTABLE_FIELDS" );

	foreach $field( sort( @attributes ) ) {

		next if( defined $attributes{$field} );

		$attributes{$field} =
			[ dblookup( @db, "", "$table", "$field", "" ) ];
	}
}

$description = dbquery( @db, "dbSCHEMA_DESCRIPTION" );
$detail = dbquery( @db, "dbSCHEMA_DETAIL" );
$detail = format_detail( $detail );
$timedate_name = dbquery( @db, "dbTIMEDATE_NAME" );

%link_fields = dbquery( @db, "dbLINK_FIELDS" );
foreach $field ( keys( %link_fields ) ) {
	$Defines{$link_fields{$field}} = $field;
}

print "/* dbschema2prolog: $schema schema wrapped into prolog syntax */\n\n";
print "/* Schema \"$schema\"\n";
defined( $description ) && print "\tDescription( \"$description\" )\n";
defined( $detail ) && print "\tDetail {\n$detail\n\t}\n";
defined( $timedate_name ) && print "\tTimedate $timedate_name\n";
print "*/\n\n";

foreach $field ( sort( keys( %attributes ) ) ) {

	@db = @{$attributes{$field}};

	$type = dbquery( @db, "dbFIELD_TYPE" );
	$size = dbquery( @db, "dbFIELD_SIZE" );
	$format = dbquery( @db, "dbFIELD_FORMAT" );
	$units = dbquery( @db, "dbFIELD_UNITS" );
	$description = dbquery( @db, "dbFIELD_DESCRIPTION" );
	$detail = dbquery( @db, "dbFIELD_DETAIL" );
	$range = dbquery( @db, "dbFIELD_RANGE" );
	$null = dbquery( @db, "dbNULL" );

	$range = token_escape( $range );
	if( $range eq "" ) { undef( $range ); }
	$description = token_escape( $description );
	$detail = format_detail( $detail );

	print "attribute( $field,\n";
	print "\t$Typetrans{$type}( $size ),\n";
	print "\tformat( \"$format\" )";

	defined( $null ) && print ",\n\tnull( \"$null\" )";
	defined( $range ) && print ",\n\trange( \"$range\" )";
	defined( $description ) && print ",\n\tdescription( \"$description\" )";
	defined( $detail ) && print ",\n\tdetail( \"\n$detail\n\t\" )";

	print "\n).\n\n";
}

foreach $table ( sort( keys( %relations ) ) ) {

	@db = @{$relations{$table}};

	@fields = dbquery( @db, "dbTABLE_FIELDS" );
	@primary = dbquery( @db, "dbPRIMARY_KEY" );
	@alternate = dbquery( @db, "dbALTERNATE_KEY" );
	@foreign = dbquery( @db, "dbFOREIGN_KEYS" );

	grep( s/(\w+)::(\w+)/range($1,$2)/g, @primary );
	grep( s/(\w+)::(\w+)/range($1,$2)/g, @alternate );
	grep( s/(\w+)::(\w+)/range($1,$2)/g, @foreign );

	$description = dbquery( @db, "dbTABLE_DESCRIPTION" );
	$detail = dbquery( @db, "dbTABLE_DETAIL" );
	
	$description = token_escape( $description );
	$detail = format_detail( $detail );

	print "relation( $table ,\n";
	print "\tfields([" . join( ",", @fields ) . "])";

	$#primary >= 0 &&
		print ",\n\tprimary([" . join( ",", @primary ) . "])";
	$#alternate >= 0 &&
		print ",\n\talternate([" . join( ",", @alternate ) . "])";
	$#foreign >= 0 &&
		print ",\n\tforeign([" . join( ",", @foreign ) . "])";

	defined( $Defines{$table} ) &&
		print ",\n\tdefines([$Defines{$table}])";
	defined( $description ) && 
		print ",\n\tdescription( \"$description\" )";
	defined( $detail ) && 
		print ",\n\tdetail( \"\n$detail\n\t\" )";

	print "\n).\n\n";
}
