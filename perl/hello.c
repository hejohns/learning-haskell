#define _GNU_SOURCE
#define _FORTIFY_SOURCE 2
#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <EXTERN.h>
#include <perl.h>

// man perlembed
static PerlInterpreter *my_perl;

// char * -> char *
char *perl(char *x){
    // set up perl
    int argc = 0;
    char **argv = NULL;
    char *my_perl_argv[] = {"", "-e", "use v5.32; use utf8;", NULL};
    PERL_SYS_INIT(&argc, &argv);
    my_perl = perl_alloc();
    perl_construct(my_perl);
    if(perl_parse(my_perl, NULL, 3, my_perl_argv, NULL)){
        goto cleanup;
    }
    PL_exit_flags |= PERL_EXIT_DESTRUCT_END;
    if(perl_run(my_perl)){
        goto cleanup;
    }
    // eval
    eval_pv(x, TRUE);
    char *ret = strdup(SvPV_nolen(get_sv("a", 0)));
    // cleanup perl
cleanup:
    perl_destruct(my_perl);
    perl_free(my_perl);
    PERL_SYS_TERM();
    return ret;
}

int main(int argc, char *argv[]){
    printf("%s\n", perl("$a = \"hi\";"));
#if 0
    char *my_perl_argv[] = {"", "-e", "use v5.32; use utf8;", NULL};
    PERL_SYS_INIT(&argc, &argv);
    my_perl = perl_alloc();
    perl_construct(my_perl);
    if(perl_parse(my_perl, NULL, 3, my_perl_argv, NULL)){
        goto cleanup;
    }
    PL_exit_flags |= PERL_EXIT_DESTRUCT_END;
    if(perl_run(my_perl)){
        goto cleanup;
    }
    eval_pv("$a = \"Hi\";", TRUE);
    printf("%s\n", SvPV_nolen(get_sv("a", 0)));
cleanup:
    perl_destruct(my_perl);
    perl_free(my_perl);
    PERL_SYS_TERM();
#endif
    return(EXIT_SUCCESS);
}
