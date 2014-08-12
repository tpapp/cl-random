#include <stdio.h>
#include <string.h>
#include <gsl/gsl_rng.h>

/* Return the gsl_rng_type for the GSL random number generator with
   the provided name. */
const gsl_rng_type* type_for_name (const char* name) 
{
    const gsl_rng_type **t, **t0;
    t0 = gsl_rng_types_setup ();
    for (t = t0; *t != 0; t++){
	if (strcmp((*t)->name, name) == 0) {
	    return *t;
	}
    }
    return 0;
}

/* Given the name of a GSL random number generator and (optionally) a
   seed, print the name, the seed, and the first n random chunks to
   stdout. */
int main (int argc, char** argv)
{
    const char * name;
    unsigned long seed;
    const gsl_rng_type * T;
    gsl_rng * r;
    
    int i, n = 10;
    
    if (argc < 2) {
	fprintf (stderr, 
		 "Expects one or two parameters. The first, a string, is the name of a random\n"
		 "number generator and is mandatory. The second, an unsigned long, is a seed\n"
		 "and is optional, if it is not supplied, the default seed is used.\n");
	exit (1);
    } else if (argc < 3) {
	seed = 0; // In GSL, 0 is replaced by the rng's default seed.
    } else {
	seed = atol(argv[2]);
    }
    name = argv[1];
    
    gsl_rng_env_setup();
    
    T = type_for_name (name);

    if (T == 0) {
	fprintf (stderr, "\"%s\" is not the name of a gsl_rng_type.\n", name);
	exit (2);
    }

    r = gsl_rng_alloc (T);
    gsl_rng_set(r, seed);
    
    /* Print name of rng, the seed used to initialized it, and its
       first n random chunks. Items are separated by a <space> and
       terminated with a <newline>. */
    printf("\"%s\" %lu", name, seed);
    for (i = 0; i < n; i++) {
	unsigned long u = gsl_rng_get (r);
	printf (" %lu", u);
    }
    printf("\n");
    
    gsl_rng_free (r);
    return 0;
}
