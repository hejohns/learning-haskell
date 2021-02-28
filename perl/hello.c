#define _GNU_SOURCE
#define _FORTIFY_SOURCE 2
#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include "queue.h"

#include <EXTERN.h>
#include <perl.h>

// man perlembed
static PerlInterpreter *my_perl;
SLIST_HEAD(, slist_string) strings;

struct slist_string{
    SLIST_ENTRY(slist_string);
    char *str;
};

int c_init_perl(){
    int argc = 0;
    char **argv = NULL;
    char *my_perl_argv[] = {"", "-e", "use v5.32; use utf8;", NULL};
    PERL_SYS_INIT(&argc, &argv);
    my_perl = perl_alloc();
    PL_perl_destruct_level = 2;
    perl_construct(my_perl);
    if(perl_parse(my_perl, NULL, 3, my_perl_argv, NULL)){
        goto error;
    }
    PL_exit_flags |= PERL_EXIT_DESTRUCT_END;
    if(perl_run(my_perl)){
        goto error;
    }
    SLIST_INIT(&strings);
    return EXIT_SUCCESS;
error:
    PL_perl_destruct_level = 2;
    perl_destruct(my_perl);
    perl_free(my_perl);
    PERL_SYS_TERM();
    return EXIT_FAILURE;
}

void c_free_perl(){
    PL_perl_destruct_level = 2;
    perl_destruct(my_perl);
    perl_free(my_perl);
    PERL_SYS_TERM();
}

void c_free_strings(){
    SLIST_FOREACH_SAFE(np, &strings, slist_string){
        free(np->str);
        free(np);
    }
    SLIST_INIT(&strings);
}

#define PRAGMAS "use v5.32; use utf8;"
char *c_eval_perl(char *x_){
    // yes I know the str functions suck, I'm being lazy
    static char pragmas[] = PRAGMAS;
    char *x = alloca(strlen(pragmas) + strlen(x_) + 1);
    x[0] = '\0';
    strcat(x, pragmas);
    strcat(x, x_);
    struct slist_string *np = calloc(1, sizeof(struct slist_string));
    eval_pv(x, TRUE);
    np->str = strdup(SvPV_nolen(get_sv("a", 0)));
    SLIST_INSERT_HEAD(&strings, np);
    return np->str;
}

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
    c_init_perl();
    printf("%s\n", c_eval_perl("use v5.32; $a = 1;"));
    printf("%s\n", c_eval_perl("$a = \"hi\";"));
    printf("%s\n", c_eval_perl("$a = 6+7;"));
    printf("%s\n", c_eval_perl("print \"$^V\n\";"));
    printf("%s\n", c_eval_perl("use v5.32; say \"hi\";"));
    printf("%s\n", c_eval_perl("say \"hi\";"));
    c_free_strings();
    c_free_perl();
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
    return EXIT_SUCCESS;
}
