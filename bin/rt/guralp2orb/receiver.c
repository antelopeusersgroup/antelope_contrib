#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <errno.h>
#include "receiver.h"
#include <netinet/in.h>
#include <arpa/inet.h>

static struct sockaddr_in name;
static int Soko;    /* socket identifier */

       /*******************************************************
        *                     socketInit                      *
        *******************************************************/
int socketInit( char *inAddress,    /* IP address of data source */
                int inPortNumber )  /* Port number of data source */
{
   int length;

/* Open a socket
   *************/
   Soko = socket( AF_INET, SOCK_DGRAM, 0 );
   if ( Soko < 0 )
   {
      fprintf( stderr, "socketInit: Can't open the socket.\n" );
      return( -1 );
   }

/* Fill in server's socket address structure
   *****************************************/
   setsockopt( Soko, SOL_SOCKET, SO_REUSEADDR, (char *) 0, 0 );

   memset( (char *) &name, '\0', sizeof(name) );
   name.sin_family      = AF_INET;
   name.sin_port        = htons( (unsigned short) inPortNumber );
   name.sin_addr.s_addr = inet_addr( inAddress );

/* Bind the socket to the port number
   **********************************/
   if ( bind( Soko, (struct sockaddr *)&name, sizeof(name) ) < 0 )
   {
      perror( "socketInit: Can't bind" );
      printf( "socketInit: Can't bind address to socket.\n" );
      return( -1 );
   }

/* Find assigned port value and print it out
   *****************************************/
   length = sizeof( name );
   if ( getsockname( Soko, (struct sockaddr *)&name, &length ) < 0 )
   {
      perror( "socketInit: Error getting socket name" );
      printf( "socketInit: Error getting socket name.\n" );
      return( -1 );
   }
   return( 0 );
}


       /******************************************************
        *                      receiver                      *
        ******************************************************/
int receiver( packet )
	char *packet;
{
   static struct sockaddr_in foreign;
   int lenforeign, length;

   lenforeign = sizeof( foreign );
   length = recvfrom( Soko, packet, PACKET_SIZE, 0,
            (struct sockaddr *)&foreign, &lenforeign );

   if ( length == -1 )
      fprintf( stderr, "receiver: recvfrom failed. errno: %d\n", errno );
   return( length );
}
