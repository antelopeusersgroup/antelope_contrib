function clear( cgg )

cggrid_free( cgg );

evalin( 'caller', ['builtin(''clear'', ''', inputname(1), ''')'] );
