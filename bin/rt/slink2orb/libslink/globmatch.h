
#ifndef GLOBMATCH_H
#define GLOBMATCH_H 1

#ifdef  __cplusplus
extern "C" {
#endif

/* Allow symbol prefixing to avoid collisions when embedded in multiple
 * projects, in particular in libraries that may be used together.
 *
 * For example, if you want to embed this matcher function in a project
 * called "mylib", you can define GLOBMATCH_PREFIX to "mylib_" and then use
 * mylib_globmatch().  This can be defined here in this header file or
 * on the command line when compiling by defining GLOBMATCH_PREFIX, e.g.
 * `cc -D GLOBMATCH_PREFIX=mylib_ ...`.
 */
#ifndef GLOBMATCH_PREFIX
//#define GLOBMATCH_PREFIX mylib_  /* Matching function will be mylib_globmatch() */
#define GLOBMATCH_PREFIX sl_  /* Matching function will be sl_globmatch() */
#endif

#ifdef GLOBMATCH_PREFIX
#  define COMBINE2(a, b) a##b
#  define COMBINE(a, b)  COMBINE2(a, b)
#  define GLOBMATCH(name) COMBINE(GLOBMATCH_PREFIX, name)
#else
#  define GLOBMATCH(name) name  /* default: symbol is globmatch() */
#endif

int GLOBMATCH(globmatch) (const char *string, const char *pattern);

#ifdef  __cplusplus
}
#endif

#endif /* GLOBMATCH_H  */
