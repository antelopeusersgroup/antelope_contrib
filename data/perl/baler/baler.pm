package baler ;

require Exporter;
@ISA = ('Exporter');

@EXPORT=qw(get_text read_url htmltext text) ;

our ( @Text ) ;

use strict ;
use Datascope ;

sub get_text { # ($good,@text) = &get_text($url, $info) ;
    my ( $url, $name) = @_ ; 
    my ($good, @text);
    $url = "$url/$name" ; 
    ($good, @text) = read_url ($url ) ; 
    grep(s/\r//g, @text) ; 
    return ($good, @text) ; 
}

sub read_url { #     ($good, @text) = &read_url ( $url ) ;
#
#  reads html files from baler
#
    require HTTP::Request;
    use LWP ; 
    use HTML::Parser ;
    use HTML::Entities ;
    my $Ua = LWP::UserAgent->new;

    my ( $url ) = @_ ;
    my ( $good ) ;
    $good = 1 ;
#    elog_notify ( "read_url	url='$url' ") ;
    my $request = HTTP::Request->new(GET => $url ) ;
    my $response = $Ua->request($request);
    if (! $response->is_success) {
	    my $msg = $response->message ;
	    elog_notify( $msg ) ;
        $good = 0;
    }
    return ($good, &htmltext ( $response )) ; 
}

sub htmltext { # &htmltext ( $response ) ;
    require HTTP::Request;
    use LWP ; 
    use HTML::Parser ;
    use HTML::Entities ;
    my $Ua = LWP::UserAgent->new;
    my ( $response ) = @_ ;
    my $p = HTML::Parser->new('text_h' => [\&text, "self, text, is_cdata"]) ;  
    $p->parse($response->content);
    $p->eof;                 
    return @Text ; 
}

sub text {
    my($self, $text, $is_cdata) = @_;
    if ( $text ne "" ) { 
	    $text = decode_entities($text) ;
	    push @Text, $text ; 
    }
}

1;
