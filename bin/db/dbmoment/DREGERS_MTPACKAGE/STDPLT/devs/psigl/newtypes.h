/*
 *  The original program assumes that int is 32 bits. Aztec went the 16 bit
 *  route. This causes a problem because a brush pattern row (32 pixels)
 *  is defined as int. Here, we do it right and define a type called
 *  INT32 which we substitute for int in places where it matters.
 */

typedef long INT32;
