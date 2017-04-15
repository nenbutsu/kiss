/*  -*- coding: utf-8 -*-
  symbols.c --- defines the symbols of ISLisp processor KISS.

  Copyright (C) 2017 Yuji Minejima.

  This file is part of ISLisp processor KISS.

  KISS is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  KISS is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

*/
#include "kiss.h"

#define KISS_SYMBOL_MAX 4096
static size_t Kiss_Symbol_Number = 0;
static kiss_symbol_t* Kiss_Symbols[KISS_SYMBOL_MAX];


void kiss_init_symbols(void) {
    size_t i;
    for (i = 0; i < KISS_SYMBOL_MAX; i++) { if (Kiss_Symbols[i] == NULL) break; }
    Kiss_Symbol_Number = i;
}

static kiss_symbol_t* kiss_make_symbol(char* name) {
    kiss_symbol_t* p = Kiss_Malloc(sizeof(kiss_symbol_t));
    p->type = KISS_SYMBOL;
    p->name = strcpy(Kiss_Malloc(sizeof(char) * (strlen(name) + 1)), name);
    p->info = 0;
    p->fun  = NULL;
    p->var  = NULL;
    p->plist= KISS_NIL;
    return p;
}

/* function: (symbolp obj) → boolean 
   Returns t if obj is a symbol; otherwise, returns nil. The obj may
   be any LISP object */
kiss_obj* kiss_symbolp(kiss_obj* obj) {
    if (KISS_IS_SYMBOL(obj)) { return KISS_T; }
    else                     { return KISS_NIL; }
}

kiss_obj* kiss_gensym(void) {
    return (kiss_obj*)kiss_make_symbol("<anonymous-symbol>");
}

kiss_obj* kiss_symbol_function (kiss_obj* obj) {
    kiss_symbol_t* symbol = Kiss_Symbol(obj);
    if (symbol->fun == NULL) {
	Kiss_Err("Unbound function ~S", obj);
    }
    return (kiss_obj*)symbol->fun;
}

kiss_obj* kiss_fboundp (kiss_obj* obj) {
    kiss_symbol_t* symbol = Kiss_Symbol(obj);
    if (symbol->fun == NULL) {
	return KISS_NIL;
    } else {
	return KISS_T;
    }
}

kiss_obj* kiss_fmakunbound (kiss_obj* obj) {
    kiss_symbol_t* symbol = Kiss_Symbol(obj);
    symbol->fun = NULL;
    return obj;
}

kiss_obj* kiss_set_symbol_function (kiss_obj* definition, kiss_obj* sym) {
    kiss_symbol_t* symbol = Kiss_Symbol(sym);
    symbol->fun = definition;
    return definition;
}

int kiss_is_interned(kiss_symbol_t* p) {
    size_t i;
    for (i = 0; i < Kiss_Symbol_Number; i++) {
	if (p == Kiss_Symbols[i]) return 1;
    }
    return 0;
}

kiss_obj* kiss_intern(kiss_obj* name) {
    kiss_string_t* str = Kiss_String(name);
    size_t i;
    kiss_symbol_t* p;
    for (i = 0; i < Kiss_Symbol_Number; i++) {
	if (strcmp(Kiss_Symbols[i]->name, str->str) == 0) {
	    /* fwprintf(stderr, "leaving intern.\n"); fflush(stderr); */
	    return (kiss_obj*)Kiss_Symbols[i];
	}
    }
    assert(Kiss_Symbol_Number < KISS_SYMBOL_MAX);
    p = kiss_make_symbol(str->str);
    Kiss_Symbols[Kiss_Symbol_Number] = p;
    ++Kiss_Symbol_Number;
    /* fwprintf(stderr, "leaving intern.\n"); fflush(stderr); */
    return (kiss_obj*)p;
}

kiss_obj* kiss_symbol(char* name) {
    return kiss_intern((kiss_obj*)kiss_make_string(name));
}

/************************** Symbol definitions *******************************/

kiss_symbol_t KISS_Snil = {
    KISS_SYMBOL,       /* type  */
    "nil",       /* name  */
    KISS_CONSTANT_VAR, /* info  */
    KISS_NIL,          /* var   */
    NULL,         /* fun   */
    KISS_NIL,          /* plist */
};

kiss_symbol_t KISS_St = {
    KISS_SYMBOL,       /* type */
    "t",         /* name */
    KISS_CONSTANT_VAR, /* info */
    KISS_T,            /* var */
    NULL,         /* fun */
    KISS_NIL,          /* plist */
};

kiss_symbol_t KISS_Skw_rest;
kiss_symbol_t KISS_Skw_rest = {
    KISS_SYMBOL,               /* type */
    ":rest",             /* name */
    0,                    /* info */
    (kiss_obj*)&KISS_Skw_rest, /* var */
    NULL,                 /* fun */
    KISS_NIL,                  /* plist */
};

kiss_symbol_t KISS_Samp_rest;
kiss_symbol_t KISS_Samp_rest = {
    KISS_SYMBOL,               /* type */
    "&rest",             /* name */
    0,                    /* info */
    (kiss_obj*)&KISS_Samp_rest, /* var */
    NULL,                 /* fun */
    KISS_NIL,                  /* plist */
};

/*** cons.c ***/
kiss_symbol_t KISS_Scons;
kiss_cfunction_t KISS_CFcons = {
    KISS_CFUNCTION, /* type */
    &KISS_Scons,    /* name */
    kiss_cons,      /* C function name */
    2,         /* minimum argument number */
    2,         /* maximum argument number */
};
kiss_symbol_t KISS_Scons = {
    KISS_SYMBOL,
    "cons",
    KISS_CONSTANT_FUN,
    NULL,               /* var */
    (kiss_obj*)&KISS_CFcons, /* fun */
    KISS_NIL,                /* plist */
};


kiss_symbol_t KISS_Scar;
kiss_cfunction_t KISS_CFcar = {
    KISS_CFUNCTION, /* type */
    &KISS_Scar,     /* name */
    kiss_car,       /* C function name */
    1,         /* minimum argument number */
    1,         /* maximum argument number */
};
kiss_symbol_t KISS_Scar = {
    KISS_SYMBOL,
    "car",
    KISS_CONSTANT_FUN,
    NULL,              /* var */
    (kiss_obj*)&KISS_CFcar, /* fun */
    KISS_NIL,               /* plist */
};

kiss_symbol_t KISS_Scdr;
kiss_cfunction_t KISS_CFcdr = {
    KISS_CFUNCTION, /* type */
    &KISS_Scdr,     /* name */
    kiss_cdr,       /* C function name */
    1,         /* minimum argument number */
    1,         /* maximum argument number */
};
kiss_symbol_t KISS_Scdr = {
    KISS_SYMBOL,
    "cdr",
    KISS_CONSTANT_FUN,
    NULL,              /* var */
    (kiss_obj*)&KISS_CFcdr, /* fun */
    KISS_NIL,               /* plist */
};

kiss_symbol_t KISS_Scadr;
kiss_cfunction_t KISS_CFcadr = {
    KISS_CFUNCTION, /* type */
    &KISS_Scadr,    /* name */
    kiss_cadr,      /* C function name */
    1,         /* minimum argument number */
    1,         /* maximum argument number */
};
kiss_symbol_t KISS_Scadr = {
    KISS_SYMBOL,
    "cadr",
    KISS_CONSTANT_FUN,
    NULL,               /* var */
    (kiss_obj*)&KISS_CFcadr, /* fun */
    KISS_NIL,                /* plist */
};


kiss_symbol_t KISS_Scddr;
kiss_cfunction_t KISS_CFcddr = {
    KISS_CFUNCTION, /* type */
    &KISS_Scddr,    /* name */
    kiss_cddr,      /* C function name */
    1,         /* minimum argument number */
    1,         /* maximum argument number */
};
kiss_symbol_t KISS_Scddr = {
    KISS_SYMBOL,
    "cddr",
    KISS_CONSTANT_FUN,
    NULL,               /* var */
    (kiss_obj*)&KISS_CFcddr, /* fun */
    KISS_NIL,                /* plist */
};


kiss_symbol_t KISS_Scaddr;
kiss_cfunction_t KISS_CFcaddr = {
    KISS_CFUNCTION, /* type */
    &KISS_Scaddr,   /* name */
    kiss_caddr,     /* C function name */
    1,         /* minimum argument number */
    1,         /* maximum argument number */
};
kiss_symbol_t KISS_Scaddr = {
    KISS_SYMBOL,
    "caddr",
    KISS_CONSTANT_FUN,
    NULL,                /* var */
    (kiss_obj*)&KISS_CFcaddr, /* fun */
    KISS_NIL,                 /* plist */
};


kiss_symbol_t KISS_Sconsp;
kiss_cfunction_t KISS_CFconsp = {
    KISS_CFUNCTION, /* type */
    &KISS_Sconsp,   /* name */
    kiss_consp,     /* C function name */
    1,         /* minimum argument number */
    1,         /* maximum argument number */
};
kiss_symbol_t KISS_Sconsp = {
    KISS_SYMBOL,
    "consp",
    KISS_CONSTANT_FUN,
    NULL,                /* var */
    (kiss_obj*)&KISS_CFconsp, /* fun */
    KISS_NIL,                 /* plist */
};


kiss_symbol_t KISS_Sset_car;
kiss_cfunction_t KISS_CFset_car = {
    KISS_CFUNCTION, /* type */
    &KISS_Sset_car, /* name */
    kiss_set_car,   /* C function name */
    2,         /* minimum argument number */
    2,         /* maximum argument number */
};
kiss_symbol_t KISS_Sset_car = {
    KISS_SYMBOL,
    "set-car",
    KISS_CONSTANT_FUN,
    NULL,                  /* var */
    (kiss_obj*)&KISS_CFset_car, /* fun */
    KISS_NIL,                   /* plist */
};


kiss_symbol_t KISS_Sset_cdr;
kiss_cfunction_t KISS_CFset_cdr = {
    KISS_CFUNCTION, /* type */
    &KISS_Sset_cdr, /* name */
    kiss_set_cdr,   /* C function name */
    2,         /* minimum argument number */
    2,         /* maximum argument number */
};
kiss_symbol_t KISS_Sset_cdr = {
    KISS_SYMBOL,
    "set-cdr",
    KISS_CONSTANT_FUN,
    NULL,                  /* var */
    (kiss_obj*)&KISS_CFset_cdr, /* fun */
    KISS_NIL,                   /* plist */
};


kiss_symbol_t KISS_Slist;
kiss_cfunction_t KISS_CFlist = {
    KISS_CFUNCTION, /* type */
    &KISS_Slist,    /* name */
    kiss_list,      /* C function name */
    0,         /* minimum argument number */
    -1,        /* maximum argument number */
};
kiss_symbol_t KISS_Slist = {
    KISS_SYMBOL,             /* type */
    "list",            /* name */
    KISS_CONSTANT_FUN,       /* info */
    NULL,               /* var */
    (kiss_obj*)&KISS_CFlist, /* fun */
    KISS_NIL,                /* plist */
};


kiss_symbol_t KISS_Sappend;
kiss_cfunction_t KISS_CFappend = {
    KISS_CFUNCTION, /* type */
    &KISS_Sappend,  /* name */
    kiss_append,    /* C function name */
    0,         /* minimum argument number */
    -1,        /* maximum argument number */
};
kiss_symbol_t KISS_Sappend = {
    KISS_SYMBOL,               /* type */
    "append",            /* name */
    KISS_CONSTANT_FUN,         /* info */
    NULL,                 /* var */
    (kiss_obj*)&KISS_CFappend, /* fun */
    KISS_NIL,                  /* plist */
};

kiss_symbol_t KISS_Sappend_s;
kiss_cfunction_t KISS_CFappend_s = {
    KISS_CFUNCTION, /* type */
    &KISS_Sappend_s,  /* name */
    kiss_append_s,    /* C function name */
    0,         /* minimum argument number */
    -1,        /* maximum argument number */
};
kiss_symbol_t KISS_Sappend_s = {
    KISS_SYMBOL,               /* type */
    "append*",            /* name */
    KISS_CONSTANT_FUN,         /* info */
    NULL,                 /* var */
    (kiss_obj*)&KISS_CFappend_s, /* fun */
    KISS_NIL,                  /* plist */
};


kiss_symbol_t KISS_Sreverse;
kiss_cfunction_t KISS_CFreverse = {
    KISS_CFUNCTION, /* type */
    &KISS_Sreverse, /* name */
    kiss_reverse,   /* C function name */
    1,         /* minimum argument number */
    1,         /* maximum argument number */
};
kiss_symbol_t KISS_Sreverse = {
    KISS_SYMBOL,                /* type */
    "reverse",            /* name */
    KISS_CONSTANT_FUN,          /* info */
    NULL,                  /* var */
    (kiss_obj*)&KISS_CFreverse, /* fun */
    KISS_NIL,                   /* plist */
};


kiss_symbol_t KISS_Snreverse;
kiss_cfunction_t KISS_CFnreverse = {
    KISS_CFUNCTION,  /* type */
    &KISS_Snreverse, /* name */
    kiss_nreverse,   /* C function name */
    1,          /* minimum argument number */
    1,          /* maximum argument number */
};
kiss_symbol_t KISS_Snreverse = {
    KISS_SYMBOL,                 /* type */
    "nreverse",            /* name */
    KISS_CONSTANT_FUN,           /* info */
    NULL,                   /* var */
    (kiss_obj*)&KISS_CFnreverse, /* fun */
    KISS_NIL,                    /* plist */
};


kiss_symbol_t KISS_Smember;
kiss_cfunction_t KISS_CFmember = {
    KISS_CFUNCTION, /* type */
    &KISS_Smember,  /* name */
    kiss_member,    /* C function name */
    2,         /* minimum argument number */
    2,         /* maximum argument number */
};
kiss_symbol_t KISS_Smember = {
    KISS_SYMBOL,               /* type */
    "member",            /* name */
    KISS_CONSTANT_FUN,         /* info */
    NULL,                 /* var */
    (kiss_obj*)&KISS_CFmember, /* fun */
    KISS_NIL,                  /* plist */
};


kiss_symbol_t KISS_Sassoc;
kiss_cfunction_t KISS_CFassoc = {
    KISS_CFUNCTION, /* type */
    &KISS_Sassoc,   /* name */
    kiss_assoc,     /* C function name */
    2,         /* minimum argument number */
    2,         /* maximum argument number */
};
kiss_symbol_t KISS_Sassoc = {
    KISS_SYMBOL,              /* type */
    "assoc",            /* name */
    KISS_CONSTANT_FUN,        /* info */
    NULL,                /* var */
    (kiss_obj*)&KISS_CFassoc, /* fun */
    KISS_NIL,                 /* plist */
};


kiss_symbol_t KISS_Scopy_list;
kiss_cfunction_t KISS_CFcopy_list = {
    KISS_CFUNCTION, /* type */
    &KISS_Scopy_list,   /* name */
    kiss_copy_list,     /* C function name */
    1,         /* minimum argument number */
    1,         /* maximum argument number */
};
kiss_symbol_t KISS_Scopy_list = {
    KISS_SYMBOL,              /* type */
    "copy-list",            /* name */
    KISS_CONSTANT_FUN,        /* info */
    NULL,                /* var */
    (kiss_obj*)&KISS_CFcopy_list, /* fun */
    KISS_NIL,                 /* plist */
};


kiss_symbol_t KISS_Splist_member;
kiss_cfunction_t KISS_CFplist_member = {
    KISS_CFUNCTION, /* type */
    &KISS_Splist_member,   /* name */
    kiss_plist_member,     /* C function name */
    2,         /* minimum argument number */
    2,         /* maximum argument number */
};
kiss_symbol_t KISS_Splist_member = {
    KISS_SYMBOL,              /* type */
    "plist-member",            /* name */
    KISS_CONSTANT_FUN,        /* info */
    NULL,                /* var */
    (kiss_obj*)&KISS_CFplist_member, /* fun */
    KISS_NIL,                 /* plist */
};

kiss_symbol_t KISS_Splist_get;
kiss_cfunction_t KISS_CFplist_get = {
    KISS_CFUNCTION, /* type */
    &KISS_Splist_get,   /* name */
    kiss_plist_get,     /* C function name */
    2,         /* minimum argument number */
    2,         /* maximum argument number */
};
kiss_symbol_t KISS_Splist_get = {
    KISS_SYMBOL,              /* type */
    "plist-get",            /* name */
    KISS_CONSTANT_FUN,        /* info */
    NULL,                /* var */
    (kiss_obj*)&KISS_CFplist_get, /* fun */
    KISS_NIL,                 /* plist */
};


kiss_symbol_t KISS_Splist_put;
kiss_cfunction_t KISS_CFplist_put = {
    KISS_CFUNCTION, /* type */
    &KISS_Splist_put,   /* name */
    kiss_plist_put,     /* C function name */
    3,         /* minimum argument number */
    3,         /* maximum argument number */
};
kiss_symbol_t KISS_Splist_put = {
    KISS_SYMBOL,              /* type */
    "plist-put",            /* name */
    KISS_CONSTANT_FUN,        /* info */
    NULL,                /* var */
    (kiss_obj*)&KISS_CFplist_put, /* fun */
    KISS_NIL,                 /* plist */
};




/*** general_vector.c ***/
kiss_symbol_t KISS_Screate_general_vector;
kiss_cfunction_t KISS_CFcreate_general_vector = {
    KISS_CFUNCTION,               /* type */
    &KISS_Screate_general_vector, /* name */
    kiss_create_general_vector,   /* C function name */
    1,         /* minimum argument number */
    2,         /* maximum argument number */
};
kiss_symbol_t KISS_Screate_general_vector = {
    KISS_SYMBOL,                              /* type */
    "create-general-vector",            /* name */
    KISS_CONSTANT_FUN,                        /* info */
    NULL,                                /* var */
    (kiss_obj*)&KISS_CFcreate_general_vector, /* fun */
    KISS_NIL,                                 /* plist */
};

kiss_symbol_t KISS_Sgeneral_vector;
kiss_cfunction_t KISS_CFgeneral_vector = {
    KISS_CFUNCTION,        /* type */
    &KISS_Sgeneral_vector, /* name */
    kiss_general_vector,   /* C function name */
    0,                /* minimum argument number */
    -1,               /* maximum argument number */
};
kiss_symbol_t KISS_Sgeneral_vector = {
    KISS_SYMBOL,                       /* type */
    "general-vector",            /* name */
    KISS_CONSTANT_FUN,                 /* info */
    NULL,                         /* var */
    (kiss_obj*)&KISS_CFgeneral_vector, /* fun */
    KISS_NIL,                          /* plist */
};

kiss_symbol_t KISS_Sgeneral_vector_p;
kiss_cfunction_t KISS_CFgeneral_vector_p = {
    KISS_CFUNCTION,          /* type */
    &KISS_Sgeneral_vector_p, /* name */
    kiss_general_vector_p,   /* C function name */
    1,                  /* minimum argument number */
    1,                  /* maximum argument number */
};
kiss_symbol_t KISS_Sgeneral_vector_p = {
    KISS_SYMBOL,                         /* type */
    "general-vector-p",            /* name */
    KISS_CONSTANT_FUN,                   /* info */
    NULL,                           /* var */
    (kiss_obj*)&KISS_CFgeneral_vector_p, /* fun */
    KISS_NIL,                            /* plist */
};

kiss_symbol_t KISS_Sgvref;
kiss_cfunction_t KISS_CFgvref = {
    KISS_CFUNCTION, /* type */
    &KISS_Sgvref,   /* name */
    kiss_gvref,     /* C function name */
    2,         /* minimum argument number */
    2,         /* maximum argument number */
};
kiss_symbol_t KISS_Sgvref = {
    KISS_SYMBOL,              /* type */
    "gvref",            /* name */
    KISS_CONSTANT_FUN,        /* info */
    NULL,                /* var */
    (kiss_obj*)&KISS_CFgvref, /* fun */
    KISS_NIL,                 /* plist */
};

kiss_symbol_t KISS_Sset_gvref;
kiss_cfunction_t KISS_CFset_gvref = {
    KISS_CFUNCTION, /* type */
    &KISS_Sset_gvref,   /* name */
    kiss_set_gvref,     /* C function name */
    3,         /* minimum argument number */
    3,         /* maximum argument number */
};
kiss_symbol_t KISS_Sset_gvref = {
    KISS_SYMBOL,                  /* type */
    "set-gvref",            /* name */
    KISS_CONSTANT_FUN,            /* info */
    NULL,                    /* var */
    (kiss_obj*)&KISS_CFset_gvref, /* fun */
    KISS_NIL,                     /* plist */
};



/*** function.c ***/
kiss_symbol_t KISS_Ssimple_function_p;
kiss_cfunction_t KISS_CFsimple_function_p = {
    KISS_CFUNCTION,   /* type */
    &KISS_Ssimple_function_p, /* name */
    kiss_simple_function_p,   /* C function name */
    1,          /* minimum argument number */
    1,          /* maximum argument number */
};
kiss_symbol_t KISS_Ssimple_function_p = {
    KISS_SYMBOL,                 /* type */
    "simple-function-p",            /* name */
    KISS_CONSTANT_FUN,           /* info */
    NULL,                   /* var */
    (kiss_obj*)&KISS_CFsimple_function_p, /* fun */
    KISS_NIL,                    /* plist */
};

kiss_symbol_t KISS_Sfunction;
kiss_cfunction_t KISS_CFfunction = {
    KISS_CMACRO,     /* type */
    &KISS_Sfunction, /* name */
    kiss_function,   /* C function name */
    1,          /* minimum argument number */
    1,          /* maximum argument number */
};
kiss_symbol_t KISS_Sfunction = {
    KISS_SYMBOL,                 /* type */
    "function",            /* name */
    KISS_CONSTANT_FUN,           /* info */
    NULL,                   /* var */
    (kiss_obj*)&KISS_CFfunction, /* fun */
    KISS_NIL,                    /* plist */
};


kiss_symbol_t KISS_Slambda;
kiss_cfunction_t KISS_CFlambda = {
    KISS_CMACRO,   /* type */
    &KISS_Slambda, /* name */
    kiss_lambda,   /* C function name */
    1,        /* minimum argument number */
    -1,       /* maximum argument number */
};
kiss_symbol_t KISS_Slambda = {
    KISS_SYMBOL,               /* type */
    "lambda",            /* name */
    KISS_CONSTANT_FUN,         /* info */
    NULL,                 /* var */
    (kiss_obj*)&KISS_CFlambda, /* fun */
    KISS_NIL,                  /* plist */
};


kiss_symbol_t KISS_Sflet;
kiss_cfunction_t KISS_CFflet = {
    KISS_CMACRO, /* type */
    &KISS_Sflet, /* name */
    kiss_flet,   /* C function name */
    1,      /* minimum argument number */
    -1,     /* maximum argument number */
};
kiss_symbol_t KISS_Sflet = {
    KISS_SYMBOL,
    "flet",
    KISS_CONSTANT_FUN,
    NULL,               /* var */
    (kiss_obj*)&KISS_CFflet, /* fun */
    KISS_NIL,                /* plist */
};


kiss_symbol_t KISS_Slabels;
kiss_cfunction_t KISS_CFlabels = {
    KISS_CMACRO,   /* type */
    &KISS_Slabels, /* name */
    kiss_labels,   /* C function name */
    1,        /* minimum argument number */
    -1,       /* maximum argument number */
};
kiss_symbol_t KISS_Slabels = {
    KISS_SYMBOL,
    "labels",
    KISS_CONSTANT_FUN,
    NULL,                 /* var */
    (kiss_obj*)&KISS_CFlabels, /* fun */
    KISS_NIL,                  /* plist */
};


kiss_symbol_t KISS_Sdefun;
kiss_cfunction_t KISS_Cdefun = {
    KISS_CMACRO,  /* type */
    &KISS_Sdefun, /* name */
    kiss_defun,   /* C function name */
    2,       /* minimum argument number */
    -1       /* maximum argument number */
};
kiss_symbol_t KISS_Sdefun = {
    KISS_SYMBOL,
    "defun",
    KISS_CONSTANT_FUN,
    NULL,               /* var */
    (kiss_obj*)&KISS_Cdefun, /* fun */
    KISS_NIL,                /* plist */
};


kiss_symbol_t KISS_Sdefmacro;
kiss_cfunction_t KISS_Cdefmacro = {
    KISS_CMACRO,     /* type */
    &KISS_Sdefmacro, /* name */
    kiss_defmacro,   /* C function name */
    2,          /* minimum argument number */
    -1          /* maximum argument number */
};
kiss_symbol_t KISS_Sdefmacro = {
    KISS_SYMBOL,
    "defmacro",
    KISS_CONSTANT_FUN,
    NULL,                  /* var */
    (kiss_obj*)&KISS_Cdefmacro, /* fun */
    KISS_NIL,                   /* plist */
};


kiss_symbol_t KISS_Sfuncall;
kiss_cfunction_t KISS_CFfuncall = {
    KISS_CFUNCTION, /* type */
    &KISS_Sfuncall, /* name */
    kiss_funcall,   /* C function name */
    1,         /* minimum argument number */
    -1,        /* maximum argument number */
};
kiss_symbol_t KISS_Sfuncall = {
    KISS_SYMBOL,
    "funcall",
    KISS_CONSTANT_FUN,
    NULL,                  /* var */
    (kiss_obj*)&KISS_CFfuncall, /* fun */
    KISS_NIL,                   /* plist */
};


kiss_symbol_t KISS_Sapply;
kiss_cfunction_t KISS_CFapply = {
    KISS_CFUNCTION, /* type */
    &KISS_Sapply,   /* name */
    kiss_apply,     /* C function name */
    2,         /* minimum argument number */
    -1,        /* maximum argument number */
};
kiss_symbol_t KISS_Sapply = {
    KISS_SYMBOL,
    "apply",
    KISS_CONSTANT_FUN,
    NULL,                /* var */
    (kiss_obj*)&KISS_CFapply, /* fun */
    KISS_NIL,                 /* plist */
};

/*** variable.c ***/
kiss_symbol_t KISS_Ssetq;
kiss_cfunction_t KISS_CFsetq = {
    KISS_CMACRO, /* type */
    &KISS_Ssetq, /* name */
    kiss_setq,   /* C function name */
    2,      /* minimum argument number */
    2,      /* maximum argument number */
};
kiss_symbol_t KISS_Ssetq = {
    KISS_SYMBOL,
    "setq",
    KISS_CONSTANT_FUN,
    NULL,               /* var */
    (kiss_obj*)&KISS_CFsetq, /* fun */
    KISS_NIL,                /* plist */
};


kiss_symbol_t KISS_Sdefglobal;
kiss_cfunction_t KISS_CFdefglobal = {
    KISS_CMACRO,      /* type */
    &KISS_Sdefglobal, /* name */
    kiss_defglobal,   /* C function name */
    2,           /* minimum argument number */
    2,           /* maximum argument number */
};
kiss_symbol_t KISS_Sdefglobal = {
    KISS_SYMBOL,
    "defglobal",
    KISS_CONSTANT_FUN,
    NULL,                         /* var */
    (kiss_obj*)&KISS_CFdefglobal, /* fun */
    KISS_NIL,                     /* plist */
};

kiss_symbol_t KISS_Sdefconstant;
kiss_cfunction_t KISS_CFdefconstant = {
    KISS_CMACRO,        /* type */
    &KISS_Sdefconstant, /* name */
    kiss_defconstant,   /* C function name */
    2,                  /* minimum argument number */
    2,                  /* maximum argument number */
};
kiss_symbol_t KISS_Sdefconstant = {
    KISS_SYMBOL,
    "defconstant",
    KISS_CONSTANT_FUN,
    NULL,                           /* var */
    (kiss_obj*)&KISS_CFdefconstant, /* fun */
    KISS_NIL,                       /* plist */
};


kiss_symbol_t KISS_Slet;
kiss_cfunction_t KISS_CFlet = {
    KISS_CMACRO, /* type */
    &KISS_Slet,  /* name */
    kiss_let,    /* C function name */
    1,      /* minimum argument number */
    -1,     /* maximum argument number */
};
kiss_symbol_t KISS_Slet = {
    KISS_SYMBOL,
    "let",
    KISS_CONSTANT_FUN,
    NULL,              /* var */
    (kiss_obj*)&KISS_CFlet, /* fun */
    KISS_NIL,               /* plist */
};


kiss_symbol_t KISS_Slet_s;
kiss_cfunction_t KISS_CFlet_s = {
    KISS_CMACRO,  /* type */
    &KISS_Slet_s, /* name */
    kiss_let_s,   /* C function name */
    1,       /* minimum argument number */
    -1,      /* maximum argument number */
};
kiss_symbol_t KISS_Slet_s = {
    KISS_SYMBOL,
    "let*",
    KISS_CONSTANT_FUN,
    NULL,                /* var */
    (kiss_obj*)&KISS_CFlet_s, /* fun */
    KISS_NIL,                 /* plist */
};


kiss_symbol_t KISS_Sdefdynamic;
kiss_cfunction_t KISS_CFdefdynamic = {
    KISS_CMACRO,       /* type */
    &KISS_Sdefdynamic, /* name */
    kiss_defdynamic,   /* C function name */
    2,            /* minimum argument number */
    2,            /* maximum argument number */
};
kiss_symbol_t KISS_Sdefdynamic = {
    KISS_SYMBOL,
    "defdynamic",
    KISS_CONSTANT_FUN,
    NULL,                     /* var */
    (kiss_obj*)&KISS_CFdefdynamic, /* fun */
    KISS_NIL,                      /* plist */
};

kiss_symbol_t KISS_Sdynamic;
kiss_cfunction_t KISS_CFdynamic = {
    KISS_CMACRO,    /* type */
    &KISS_Sdynamic, /* name */
    kiss_dynamic,   /* C function name */
    1,         /* minimum argument number */
    1,         /* maximum argument number */
};
kiss_symbol_t KISS_Sdynamic = {
    KISS_SYMBOL,
    "dynamic",
    KISS_CONSTANT_FUN,
    NULL,                  /* var */
    (kiss_obj*)&KISS_CFdynamic, /* fun */
    KISS_NIL,                   /* plist */
};


kiss_symbol_t KISS_Sdynamic_let;
kiss_cfunction_t KISS_CFdynamic_let = {
    KISS_CMACRO,        /* type */
    &KISS_Sdynamic_let, /* name */
    kiss_dynamic_let,   /* C function name */
    1,             /* minimum argument number */
    -1,            /* maximum argument number */
};
kiss_symbol_t KISS_Sdynamic_let = {
    KISS_SYMBOL,
    "dynamic-let",
    KISS_CONSTANT_FUN,
    NULL,                      /* var */
    (kiss_obj*)&KISS_CFdynamic_let, /* fun */
    KISS_NIL,                       /* plist */
};

kiss_symbol_t KISS_Sset_dynamic;
kiss_cfunction_t KISS_CFset_dynamic = {
    KISS_CMACRO,        /* type */
    &KISS_Sset_dynamic, /* name */
    kiss_set_dynamic,   /* C function name */
    2,             /* minimum argument number */
    2,            /* maximum argument number */
};
kiss_symbol_t KISS_Sset_dynamic = {
    KISS_SYMBOL,
    "set-dynamic",
    KISS_CONSTANT_FUN,
    NULL,                      /* var */
    (kiss_obj*)&KISS_CFset_dynamic, /* fun */
    KISS_NIL,                       /* plist */
};

/*** control.c ***/
kiss_symbol_t KISS_Squote;
kiss_cfunction_t KISS_CFquote = {
    KISS_CMACRO,  /* type */
    &KISS_Squote, /* name */
    kiss_quote,   /* C function name */
    1,       /* minimum argument number */
    1,       /* maximum argument number */
};
kiss_symbol_t KISS_Squote = {
    KISS_SYMBOL,              /* type */
    "quote",            /* name */
    KISS_CONSTANT_FUN,        /* info */
    NULL,                /* var */
    (kiss_obj*)&KISS_CFquote, /* fun */
    KISS_NIL,                 /* plist */
};

kiss_symbol_t KISS_Sif;
kiss_cfunction_t KISS_CFif = {
    KISS_CMACRO, /* type */
    &KISS_Sif,   /* name */
    kiss_if,    /* C function name */
    2,      /* minimum argument number */
    3,      /* maximum argument number */
};
kiss_symbol_t KISS_Sif = {
    KISS_SYMBOL,
    "if",
    KISS_CONSTANT_FUN,
    NULL,             /* var */
    (kiss_obj*)&KISS_CFif, /* fun */
    KISS_NIL,              /* plist */
};

kiss_symbol_t KISS_Sprogn;
kiss_cfunction_t KISS_CFprogn = {
    KISS_CMACRO,  /* type */
    &KISS_Sprogn, /* name */
    kiss_progn,   /* C function name */
    0,       /* minimum argument number */
    -1,      /* maximum argument number */
};
kiss_symbol_t KISS_Sprogn = {
    KISS_SYMBOL,
    "progn",
    KISS_CONSTANT_FUN,
    NULL,             /* var */
    (kiss_obj*)&KISS_CFprogn, /* fun */
    KISS_NIL,              /* plist */
};


kiss_symbol_t KISS_Seq;
kiss_cfunction_t KISS_CFeq = {
    KISS_CFUNCTION, /* type */
    &KISS_Seq,      /* name */
    kiss_eq,        /* C function name */
    2,         /* minimum argument number */
    2,         /* maximum argument number */
};
kiss_symbol_t KISS_Seq = {
    KISS_SYMBOL,
    "eq",
    KISS_CONSTANT_FUN,
    NULL,             /* var */
    (kiss_obj*)&KISS_CFeq, /* fun */
    KISS_NIL,              /* plist */
};


kiss_symbol_t KISS_Seql;
kiss_cfunction_t KISS_CFeql = {
    KISS_CFUNCTION, /* type */
    &KISS_Seql,     /* name */
    kiss_eql,       /* C function name */
    2,         /* minimum argument number */
    2,         /* maximum argument number */
};
kiss_symbol_t KISS_Seql = {
    KISS_SYMBOL,
    "eql",
    KISS_CONSTANT_FUN,
    NULL,              /* var */
    (kiss_obj*)&KISS_CFeql, /* fun */
    KISS_NIL,               /* plist */
};

kiss_symbol_t KISS_Scatch;
kiss_cfunction_t KISS_Ccatch = {
    KISS_CMACRO,  /* type */
    &KISS_Scatch, /* name */
    kiss_catch,   /* C function name */
    1,       /* minimum argument number */
    -1,      /* maximum argument number */
};
kiss_symbol_t KISS_Scatch = {
    KISS_SYMBOL,
    "catch",
    KISS_CONSTANT_FUN,
    NULL,               /* var */
    (kiss_obj*)&KISS_Ccatch, /* fun */
    KISS_NIL,                /* plist */
};


kiss_symbol_t KISS_Sthrow;
kiss_cfunction_t KISS_Cthrow = {
    KISS_CMACRO,  /* type */
    &KISS_Sthrow, /* name */
    kiss_throw,   /* C function name */
    2,       /* minimum argument number */
    2,       /* maximum argument number */
};
kiss_symbol_t KISS_Sthrow = {
    KISS_SYMBOL,
    "throw",
    KISS_CONSTANT_FUN,
    NULL,               /* var */
    (kiss_obj*)&KISS_Cthrow, /* fun */
    KISS_NIL,                /* plist */
};


kiss_symbol_t KISS_Sunwind_protect;
kiss_cfunction_t KISS_Cunwind_protect = {
    KISS_CMACRO,           /* type */
    &KISS_Sunwind_protect, /* name */
    kiss_unwind_protect,   /* C function name */
    1,                /* minimum argument number */
    -1,               /* maximum argument number */
};
kiss_symbol_t KISS_Sunwind_protect = {
    KISS_SYMBOL,
    "unwind-protect",
    KISS_CONSTANT_FUN,
    NULL,                        /* var */
    (kiss_obj*)&KISS_Cunwind_protect, /* fun */
    KISS_NIL,                         /* plist */
};


kiss_symbol_t KISS_Sblock;
kiss_cfunction_t KISS_Cblock = {
    KISS_CMACRO,  /* type */
    &KISS_Sblock, /* name */
    kiss_block,   /* C function name */
    1,       /* minimum argument number */
    -1,      /* maximum argument number */
};
kiss_symbol_t KISS_Sblock = {
    KISS_SYMBOL,
    "block",
    KISS_CONSTANT_FUN,
    NULL,               /* var */
    (kiss_obj*)&KISS_Cblock, /* fun */
    KISS_NIL,                /* plist */
};


kiss_symbol_t KISS_Sreturn_from;
kiss_cfunction_t KISS_Creturn_from = {
    KISS_CMACRO,        /* type */
    &KISS_Sreturn_from, /* name */
    kiss_return_from,   /* C function name */
    2,             /* minimum argument number */
    2,             /* maximum argument number */
};
kiss_symbol_t KISS_Sreturn_from = {
    KISS_SYMBOL,
    "return-from",
    KISS_CONSTANT_FUN,
    NULL,                     /* var */
    (kiss_obj*)&KISS_Creturn_from, /* fun */
    KISS_NIL,                      /* plist */
};


kiss_symbol_t KISS_Stagbody;
kiss_cfunction_t KISS_Ctagbody = {
    KISS_CMACRO,    /* type */
    &KISS_Stagbody, /* name */
    kiss_tagbody,   /* C function name */
    0,         /* minimum argument number */
    -1,        /* maximum argument number */
};
kiss_symbol_t KISS_Stagbody = {
    KISS_SYMBOL,
    "tagbody",
    KISS_CONSTANT_FUN,
    NULL,                 /* var */
    (kiss_obj*)&KISS_Ctagbody, /* fun */
    KISS_NIL,                  /* plist */
};


kiss_symbol_t KISS_Sgo;
kiss_cfunction_t KISS_Cgo = {
    KISS_CMACRO, /* type */
    &KISS_Sgo,   /* name */
    kiss_go,     /* C function name */
    1,      /* minimum argument number */
    1,      /* maximum argument number */
};
kiss_symbol_t KISS_Sgo = {
    KISS_SYMBOL,
    "go",
    KISS_CONSTANT_FUN,
    NULL,            /* var */
    (kiss_obj*)&KISS_Cgo, /* fun */
    KISS_NIL,             /* plist */
};

/*** number.c ***/
kiss_symbol_t KISS_Sintegerp;
kiss_cfunction_t KISS_CFintegerp = {
    KISS_CFUNCTION, /* type */
    &KISS_Sintegerp,    /* name */
    kiss_integerp,     /* C function name */
    1,         /* minimum argument number */
    1,        /* maximum argument number */
};
kiss_symbol_t KISS_Sintegerp = {
    KISS_SYMBOL,
    "integerp",
    KISS_CONSTANT_FUN,
    NULL,               /* var */
    (kiss_obj*)&KISS_CFintegerp, /* fun */
    KISS_NIL,                /* plist */
};

kiss_symbol_t KISS_Sfloatp;
kiss_cfunction_t KISS_CFfloatp = {
    KISS_CFUNCTION,   /* type */
    &KISS_Sfloatp,    /* name */
    kiss_floatp,      /* C function name */
    1,                /* minimum argument number */
    1,                /* maximum argument number */
};
kiss_symbol_t KISS_Sfloatp = {
    KISS_SYMBOL,
    "floatp",
    KISS_CONSTANT_FUN,
    NULL,                      /* var */
    (kiss_obj*)&KISS_CFfloatp, /* fun */
    KISS_NIL,                  /* plist */
};

kiss_symbol_t KISS_Sfloat;
kiss_cfunction_t KISS_CFfloat = {
    KISS_CFUNCTION,   /* type */
    &KISS_Sfloat,     /* name */
    kiss_float,       /* C function name */
    1,                /* minimum argument number */
    1,                /* maximum argument number */
};
kiss_symbol_t KISS_Sfloat = {
    KISS_SYMBOL,
    "float",
    KISS_CONSTANT_FUN,
    NULL,                     /* var */
    (kiss_obj*)&KISS_CFfloat, /* fun */
    KISS_NIL,                 /* plist */
};


kiss_symbol_t KISS_Splus;
kiss_cfunction_t KISS_CFplus = {
    KISS_CFUNCTION, /* type */
    &KISS_Splus,    /* name */
    kiss_Lplus,     /* C function name */
    0,         /* minimum argument number */
    -1,        /* maximum argument number */
};
kiss_symbol_t KISS_Splus = {
    KISS_SYMBOL,
    "+",
    KISS_CONSTANT_FUN,
    NULL,               /* var */
    (kiss_obj*)&KISS_CFplus, /* fun */
    KISS_NIL,                /* plist */
};

kiss_symbol_t KISS_Smultiply;
kiss_cfunction_t KISS_CFmultiply = {
    KISS_CFUNCTION,  /* type */
    &KISS_Smultiply, /* name */
    kiss_Lmultiply,  /* C function name */
    0,               /* minimum argument number */
    -1,              /* maximum argument number */
};
kiss_symbol_t KISS_Smultiply = {
    KISS_SYMBOL,
    "*",
    KISS_CONSTANT_FUN,
    NULL,                        /* var */
    (kiss_obj*)&KISS_CFmultiply, /* fun */
    KISS_NIL,                    /* plist */
};


kiss_symbol_t KISS_Sminus;
kiss_cfunction_t KISS_CFminus = {
    KISS_CFUNCTION, /* type */
    &KISS_Sminus,   /* name */
    kiss_Lminus,    /* C function name */
    1,              /* minimum argument number */
    -1,             /* maximum argument number */
};
kiss_symbol_t KISS_Sminus = {
    KISS_SYMBOL,
    "-",
    KISS_CONSTANT_FUN,
    NULL,                     /* var */
    (kiss_obj*)&KISS_CFminus, /* fun */
    KISS_NIL,                 /* plist */
};


kiss_symbol_t KISS_Snum_eq;
kiss_cfunction_t KISS_CFnum_eq = {
    KISS_CFUNCTION, /* type */
    &KISS_Snum_eq,  /* name */
    kiss_Lnum_eq,   /* C function name */
    2,         /* minimum argument number */
    2,         /* maximum argument number */
};
kiss_symbol_t KISS_Snum_eq = {
    KISS_SYMBOL,
    "=",
    KISS_CONSTANT_FUN,
    NULL,                 /* var */
    (kiss_obj*)&KISS_CFnum_eq, /* fun */
    KISS_NIL,                  /* plist */
};


kiss_symbol_t KISS_Snum_lessthan;
kiss_cfunction_t KISS_CFnum_lessthan = {
    KISS_CFUNCTION,       /* type */
    &KISS_Snum_lessthan,  /* name */
    kiss_Lnum_lessthan,   /* C function name */
    2,               /* minimum argument number */
    2,               /* maximum argument number */
};
kiss_symbol_t KISS_Snum_lessthan = {
    KISS_SYMBOL,
    "<",
    KISS_CONSTANT_FUN,
    NULL,                       /* var */
    (kiss_obj*)&KISS_CFnum_lessthan, /* fun */
    KISS_NIL,                        /* plist */
};


kiss_symbol_t KISS_Sdiv;
kiss_cfunction_t KISS_CFdiv = {
    KISS_CFUNCTION,       /* type */
    &KISS_Sdiv,  /* name */
    kiss_div,   /* C function name */
    2,               /* minimum argument number */
    2,               /* maximum argument number */
};
kiss_symbol_t KISS_Sdiv = {
    KISS_SYMBOL,
    "div",
    KISS_CONSTANT_FUN,
    NULL,                   /* var */
    (kiss_obj*)&KISS_CFdiv, /* fun */
    KISS_NIL,               /* plist */
};

kiss_symbol_t KISS_Smod;
kiss_cfunction_t KISS_CFmod = {
    KISS_CFUNCTION,       /* type */
    &KISS_Smod,  /* name */
    kiss_mod,   /* C function name */
    2,               /* minimum argument number */
    2,               /* maximum argument number */
};
kiss_symbol_t KISS_Smod = {
    KISS_SYMBOL,
    "mod",
    KISS_CONSTANT_FUN,
    NULL,                   /* var */
    (kiss_obj*)&KISS_CFmod, /* fun */
    KISS_NIL,               /* plist */
};

kiss_symbol_t KISS_Sgcd;
kiss_cfunction_t KISS_CFgcd = {
    KISS_CFUNCTION,       /* type */
    &KISS_Sgcd,  /* name */
    kiss_gcd,   /* C function name */
    2,               /* minimum argument number */
    2,               /* maximum argument number */
};
kiss_symbol_t KISS_Sgcd = {
    KISS_SYMBOL,
    "gcd",
    KISS_CONSTANT_FUN,
    NULL,                   /* var */
    (kiss_obj*)&KISS_CFgcd, /* fun */
    KISS_NIL,               /* plist */
};

kiss_symbol_t KISS_Slcm;
kiss_cfunction_t KISS_CFlcm = {
    KISS_CFUNCTION,       /* type */
    &KISS_Slcm,  /* name */
    kiss_lcm,   /* C function name */
    2,               /* minimum argument number */
    2,               /* maximum argument number */
};
kiss_symbol_t KISS_Slcm = {
    KISS_SYMBOL,
    "lcm",
    KISS_CONSTANT_FUN,
    NULL,                   /* var */
    (kiss_obj*)&KISS_CFlcm, /* fun */
    KISS_NIL,               /* plist */
};

kiss_symbol_t KISS_Sparse_number;
kiss_cfunction_t KISS_CFparse_number = {
    KISS_CFUNCTION,      /* type */
    &KISS_Sparse_number, /* name */
    kiss_parse_number,   /* C function name */
    1,                   /* minimum argument number */
    1,                   /* maximum argument number */
};
kiss_symbol_t KISS_Sparse_number = {
    KISS_SYMBOL,
    "parse-number",
    KISS_CONSTANT_FUN,
    NULL,                            /* var */
    (kiss_obj*)&KISS_CFparse_number, /* fun */
    KISS_NIL,                        /* plist */
};

kiss_symbol_t KISS_Sabs;
kiss_cfunction_t KISS_CFabs = {
    KISS_CFUNCTION,   /* type */
    &KISS_Sabs,       /* name */
    kiss_abs,         /* C function name */
    1,                /* minimum argument number */
    1,                /* maximum argument number */
};
kiss_symbol_t KISS_Sabs = {
    KISS_SYMBOL,
    "abs",
    KISS_CONSTANT_FUN,
    NULL,                    /* var */
    (kiss_obj*)&KISS_CFabs,  /* fun */
    KISS_NIL,                /* plist */
};

kiss_symbol_t KISS_Sexp;
kiss_cfunction_t KISS_CFexp = {
    KISS_CFUNCTION,   /* type */
    &KISS_Sexp,       /* name */
    kiss_exp,         /* C function name */
    1,                /* minimum argument number */
    1,                /* maximum argument number */
};
kiss_symbol_t KISS_Sexp = {
    KISS_SYMBOL,
    "exp",
    KISS_CONSTANT_FUN,
    NULL,                    /* var */
    (kiss_obj*)&KISS_CFexp,  /* fun */
    KISS_NIL,                /* plist */
};

kiss_symbol_t KISS_Slog;
kiss_cfunction_t KISS_CFlog = {
    KISS_CFUNCTION,   /* type */
    &KISS_Slog,       /* name */
    kiss_log,         /* C function name */
    1,                /* minimum argument number */
    1,                /* maximum argument number */
};
kiss_symbol_t KISS_Slog = {
    KISS_SYMBOL,
    "log",
    KISS_CONSTANT_FUN,
    NULL,                    /* var */
    (kiss_obj*)&KISS_CFlog,  /* fun */
    KISS_NIL,                /* plist */
};

kiss_symbol_t KISS_Sfloor;
kiss_cfunction_t KISS_CFfloor = {
    KISS_CFUNCTION,   /* type */
    &KISS_Sfloor,     /* name */
    kiss_floor,       /* C function name */
    1,                /* minimum argument number */
    1,                /* maximum argument number */
};
kiss_symbol_t KISS_Sfloor = {
    KISS_SYMBOL,
    "floor",
    KISS_CONSTANT_FUN,
    NULL,                     /* var */
    (kiss_obj*)&KISS_CFfloor, /* fun */
    KISS_NIL,                 /* plist */
};

kiss_symbol_t KISS_Sceiling;
kiss_cfunction_t KISS_CFceiling = {
    KISS_CFUNCTION,   /* type */
    &KISS_Sceiling,   /* name */
    kiss_ceiling,     /* C function name */
    1,                /* minimum argument number */
    1,                /* maximum argument number */
};
kiss_symbol_t KISS_Sceiling = {
    KISS_SYMBOL,
    "ceiling",
    KISS_CONSTANT_FUN,
    NULL,                       /* var */
    (kiss_obj*)&KISS_CFceiling, /* fun */
    KISS_NIL,                   /* plist */
};

kiss_symbol_t KISS_Struncate;
kiss_cfunction_t KISS_CFtruncate = {
    KISS_CFUNCTION,   /* type */
    &KISS_Struncate,  /* name */
    kiss_truncate,    /* C function name */
    1,                /* minimum argument number */
    1,                /* maximum argument number */
};
kiss_symbol_t KISS_Struncate = {
    KISS_SYMBOL,
    "truncate",
    KISS_CONSTANT_FUN,
    NULL,                        /* var */
    (kiss_obj*)&KISS_CFtruncate, /* fun */
    KISS_NIL,                    /* plist */
};

kiss_symbol_t KISS_Sround;
kiss_cfunction_t KISS_CFround = {
    KISS_CFUNCTION, /* type */
    &KISS_Sround,   /* name */
    kiss_round,     /* C function name */
    1,              /* minimum argument number */
    1,              /* maximum argument number */
};
kiss_symbol_t KISS_Sround = {
    KISS_SYMBOL,
    "round",
    KISS_CONSTANT_FUN,
    NULL,                     /* var */
    (kiss_obj*)&KISS_CFround, /* fun */
    KISS_NIL,                 /* plist */
};


kiss_symbol_t KISS_Ssin;
kiss_cfunction_t KISS_CFsin = {
    KISS_CFUNCTION,   /* type */
    &KISS_Ssin,       /* name */
    kiss_sin,         /* C function name */
    1,                /* minimum argument number */
    1,                /* maximum argument number */
};
kiss_symbol_t KISS_Ssin = {
    KISS_SYMBOL,
    "sin",
    KISS_CONSTANT_FUN,
    NULL,                    /* var */
    (kiss_obj*)&KISS_CFsin,  /* fun */
    KISS_NIL,                /* plist */
};

kiss_symbol_t KISS_Scos;
kiss_cfunction_t KISS_CFcos = {
    KISS_CFUNCTION,   /* type */
    &KISS_Scos,       /* name */
    kiss_cos,         /* C function name */
    1,                /* minimum argument number */
    1,                /* maximum argument number */
};
kiss_symbol_t KISS_Scos = {
    KISS_SYMBOL,
    "cos",
    KISS_CONSTANT_FUN,
    NULL,                    /* var */
    (kiss_obj*)&KISS_CFcos,  /* fun */
    KISS_NIL,                /* plist */
};

kiss_symbol_t KISS_Stan;
kiss_cfunction_t KISS_CFtan = {
    KISS_CFUNCTION,   /* type */
    &KISS_Stan,       /* name */
    kiss_tan,         /* C function name */
    1,                /* minimum argument number */
    1,                /* maximum argument number */
};
kiss_symbol_t KISS_Stan = {
    KISS_SYMBOL,
    "tan",
    KISS_CONSTANT_FUN,
    NULL,                    /* var */
    (kiss_obj*)&KISS_CFtan,  /* fun */
    KISS_NIL,                /* plist */
};

/*** symbols.c ***/
kiss_symbol_t KISS_Ssymbolp;
kiss_cfunction_t KISS_CFsymbolp = {
    KISS_CFUNCTION, /* type */
    &KISS_Ssymbolp,  /* name */
    kiss_symbolp,    /* C function name */
    1,         /* minimum argument number */
    1,         /* maximum argument number */
};
kiss_symbol_t KISS_Ssymbolp = {
    KISS_SYMBOL,
    "symbolp",
    KISS_CONSTANT_FUN,
    NULL,                 /* var */
    (kiss_obj*)&KISS_CFsymbolp, /* fun */
    KISS_NIL,                  /* plist */
};


kiss_symbol_t KISS_Sgensym;
kiss_cfunction_t KISS_CFgensym = {
    KISS_CFUNCTION, /* type */
    &KISS_Sgensym,  /* name */
    kiss_gensym,    /* C function name */
    0,         /* minimum argument number */
    0,         /* maximum argument number */
};
kiss_symbol_t KISS_Sgensym = {
    KISS_SYMBOL,
    "gensym",
    KISS_CONSTANT_FUN,
    NULL,                 /* var */
    (kiss_obj*)&KISS_CFgensym, /* fun */
    KISS_NIL,                  /* plist */
};

kiss_symbol_t KISS_Ssymbol_function;
kiss_cfunction_t KISS_CFsymbol_function = {
    KISS_CFUNCTION, /* type */
    &KISS_Ssymbol_function,  /* name */
    kiss_symbol_function,    /* C function name */
    1,         /* minimum argument number */
    1,         /* maximum argument number */
};
kiss_symbol_t KISS_Ssymbol_function = {
    KISS_SYMBOL,
    "symbol-function",
    KISS_CONSTANT_FUN,
    NULL,                 /* var */
    (kiss_obj*)&KISS_CFsymbol_function, /* fun */
    KISS_NIL,                  /* plist */
};

kiss_symbol_t KISS_Sset_symbol_function;
kiss_cfunction_t KISS_CFset_symbol_function = {
    KISS_CFUNCTION, /* type */
    &KISS_Sset_symbol_function,  /* name */
    kiss_set_symbol_function,    /* C function name */
    2,         /* minimum argument number */
    2,         /* maximum argument number */
};
kiss_symbol_t KISS_Sset_symbol_function = {
    KISS_SYMBOL,
    "set-symbol-function",
    KISS_CONSTANT_FUN,
    NULL,                 /* var */
    (kiss_obj*)&KISS_CFset_symbol_function, /* fun */
    KISS_NIL,                  /* plist */
};

kiss_symbol_t KISS_Sfboundp;
kiss_cfunction_t KISS_CFfboundp = {
    KISS_CFUNCTION, /* type */
    &KISS_Sfboundp,  /* name */
    kiss_fboundp,    /* C function name */
    1,         /* minimum argument number */
    1,         /* maximum argument number */
};
kiss_symbol_t KISS_Sfboundp = {
    KISS_SYMBOL,
    "fboundp",
    KISS_CONSTANT_FUN,
    NULL,                 /* var */
    (kiss_obj*)&KISS_CFfboundp, /* fun */
    KISS_NIL,                  /* plist */
};

kiss_symbol_t KISS_Sfmakunbound;
kiss_cfunction_t KISS_CFfmakunbound = {
    KISS_CFUNCTION, /* type */
    &KISS_Sfmakunbound,  /* name */
    kiss_fmakunbound,    /* C function name */
    1,         /* minimum argument number */
    1,         /* maximum argument number */
};
kiss_symbol_t KISS_Sfmakunbound = {
    KISS_SYMBOL,
    "kiss::fmakunbound",
    KISS_CONSTANT_FUN,
    NULL,                 /* var */
    (kiss_obj*)&KISS_CFfmakunbound, /* fun */
    KISS_NIL,                  /* plist */
};



/*** format_object.c ***/
kiss_symbol_t KISS_Sformat;
kiss_cfunction_t KISS_CFformat = {
    KISS_CFUNCTION,       /* type */
    &KISS_Sformat, /* name */
    kiss_format,   /* C function name */
    2,               /* minimum argument number */
    -1,               /* maximum argument number */
};
kiss_symbol_t KISS_Sformat = {
    KISS_SYMBOL,
    "format",
    KISS_CONSTANT_FUN,
    NULL,                        /* var */
    (kiss_obj*)&KISS_CFformat, /* fun */
    KISS_NIL,                         /* plist */
};

kiss_symbol_t KISS_Sformat_object;
kiss_cfunction_t KISS_CFformat_object = {
    KISS_CFUNCTION,       /* type */
    &KISS_Sformat_object, /* name */
    kiss_format_object,   /* C function name */
    3,               /* minimum argument number */
    3,               /* maximum argument number */
};
kiss_symbol_t KISS_Sformat_object = {
    KISS_SYMBOL,
    "format-object",
    KISS_CONSTANT_FUN,
    NULL,                        /* var */
    (kiss_obj*)&KISS_CFformat_object, /* fun */
    KISS_NIL,                         /* plist */
};


kiss_symbol_t KISS_Sformat_pointer;
kiss_cfunction_t KISS_CFformat_pointer = {
    KISS_CFUNCTION,       /* type */
    &KISS_Sformat_pointer, /* name */
    kiss_format_pointer,   /* C function name */
    2,               /* minimum argument number */
    2,               /* maximum argument number */
};
kiss_symbol_t KISS_Sformat_pointer = {
    KISS_SYMBOL,
    "kiss::format-pointer",
    KISS_CONSTANT_FUN,
    NULL,                        /* var */
    (kiss_obj*)&KISS_CFformat_pointer, /* fun */
    KISS_NIL,                         /* plist */
};


kiss_symbol_t KISS_Sprint;
kiss_cfunction_t KISS_CFprint = {
    KISS_CFUNCTION, /* type */
    &KISS_Sprint,   /* name */
    kiss_print,     /* C function name */
    1,         /* minimum argument number */
    1,         /* maximum argument number */
};
kiss_symbol_t KISS_Sprint = {
    KISS_SYMBOL,
    "print",
    KISS_CONSTANT_FUN,
    NULL,                /* var */
    (kiss_obj*)&KISS_CFprint, /* fun */
    KISS_NIL,                 /* plist */
};

/*** read.c ***/
kiss_symbol_t KISS_Sread;
kiss_cfunction_t KISS_CFread = {
    KISS_CFUNCTION, /* type */
    &KISS_Sread,    /* name */
    kiss_read,     /* C function name */
    0,         /* minimum argument number */
    3,         /* maximum argument number */
};
kiss_symbol_t KISS_Sread = {
    KISS_SYMBOL,
    "read",
    KISS_CONSTANT_FUN,
    NULL,               /* var */
    (kiss_obj*)&KISS_CFread, /* fun */
    KISS_NIL,                /* plist */
};

/*** string.c ***/
kiss_symbol_t KISS_Sstringp;
kiss_cfunction_t KISS_CFstringp = {
    KISS_CFUNCTION,    /* type */
    &KISS_Sstringp,    /* name */
    kiss_stringp,      /* C function name */
    1,                 /* minimum argument number */
    1,                 /* maximum argument number */
};
kiss_symbol_t KISS_Sstringp = {
    KISS_SYMBOL,
    "stringp",
    KISS_CONSTANT_FUN,
    NULL,               /* var */
    (kiss_obj*)&KISS_CFstringp, /* fun */
    KISS_NIL,                /* plist */
};

kiss_symbol_t KISS_Screate_string;
kiss_cfunction_t KISS_CFcreate_string = {
    KISS_CFUNCTION,       /* type */
    &KISS_Screate_string, /* name */
    kiss_create_string,   /* C function name */
    1,                    /* minimum argument number */
    2,                    /* maximum argument number */
};
kiss_symbol_t KISS_Screate_string = {
    KISS_SYMBOL,
    "create-string",
    KISS_CONSTANT_FUN,
    NULL,                             /* var */
    (kiss_obj*)&KISS_CFcreate_string, /* fun */
    KISS_NIL,                         /* plist */
};

kiss_symbol_t KISS_Sstring_append;
kiss_cfunction_t KISS_CFstring_append = {
    KISS_CFUNCTION,       /* type */
    &KISS_Sstring_append, /* name */
    kiss_string_append,   /* C function name */
    0,                    /* minimum argument number */
    -1,                   /* maximum argument number */
};
kiss_symbol_t KISS_Sstring_append = {
    KISS_SYMBOL,
    "string-append",
    KISS_CONSTANT_FUN,
    NULL,                             /* var */
    (kiss_obj*)&KISS_CFstring_append, /* fun */
    KISS_NIL,                         /* plist */
};


/*** sequence.c ***/
kiss_symbol_t KISS_Slength;
kiss_cfunction_t KISS_CFlength = {
    KISS_CFUNCTION, /* type */
    &KISS_Slength,    /* name */
    kiss_length,     /* C function name */
    1,         /* minimum argument number */
    1,         /* maximum argument number */
};
kiss_symbol_t KISS_Slength = {
    KISS_SYMBOL,
    "length",
    KISS_CONSTANT_FUN,
    NULL,               /* var */
    (kiss_obj*)&KISS_CFlength, /* fun */
    KISS_NIL,                /* plist */
};

kiss_symbol_t KISS_Selt;
kiss_cfunction_t KISS_CFelt = {
    KISS_CFUNCTION, /* type */
    &KISS_Selt,    /* name */
    kiss_elt,     /* C function name */
    2,         /* minimum argument number */
    2,         /* maximum argument number */
};
kiss_symbol_t KISS_Selt = {
    KISS_SYMBOL,
    "elt",
    KISS_CONSTANT_FUN,
    NULL,               /* var */
    (kiss_obj*)&KISS_CFelt, /* fun */
    KISS_NIL,                /* plist */
};

/*** eval.c ***/
kiss_symbol_t KISS_Seval;
kiss_cfunction_t KISS_CFeval = {
    KISS_CFUNCTION,        /* type */
    &KISS_Seval, /* name */
    kiss_eval,   /* C function name */
    1,                /* minimum argument number */
    1,                /* maximum argument number */
};
kiss_symbol_t KISS_Seval = {
    KISS_SYMBOL,
    "eval",
    KISS_CONSTANT_FUN,
    NULL,                         /* var */
    (kiss_obj*)&KISS_CFeval, /* fun */
    KISS_NIL,                          /* plist */
};


/*** stream.c ***/
kiss_symbol_t KISS_Sstandard_input;
kiss_cfunction_t KISS_CFstandard_input = {
    KISS_CFUNCTION,        /* type */
    &KISS_Sstandard_input, /* name */
    kiss_standard_input,   /* C function name */
    0,                /* minimum argument number */
    0,                /* maximum argument number */
};
kiss_symbol_t KISS_Sstandard_input = {
    KISS_SYMBOL,
    "standard-input",
    KISS_CONSTANT_FUN,
    NULL,                         /* var */
    (kiss_obj*)&KISS_CFstandard_input, /* fun */
    KISS_NIL,                          /* plist */
};


kiss_symbol_t KISS_Sstandard_output;
kiss_cfunction_t KISS_CFstandard_output = {
    KISS_CFUNCTION,         /* type */
    &KISS_Sstandard_output, /* name */
    kiss_standard_output,   /* C function name */
    0,                 /* minimum argument number */
    0,                 /* maximum argument number */
};
kiss_symbol_t KISS_Sstandard_output = {
    KISS_SYMBOL,
    "standard-output",
    KISS_CONSTANT_FUN,
    NULL,                          /* var */
    (kiss_obj*)&KISS_CFstandard_output, /* fun */
    KISS_NIL,                           /* plist */
};


kiss_symbol_t KISS_Serror_output;
kiss_cfunction_t KISS_CFerror_output = {
    KISS_CFUNCTION,      /* type */
    &KISS_Serror_output, /* name */
    kiss_error_output,   /* C function name */
    0,              /* minimum argument number */
    0,              /* maximum argument number */
};
kiss_symbol_t KISS_Serror_output = {
    KISS_SYMBOL,
    "error-output",
    KISS_CONSTANT_FUN,
    NULL,                       /* var */
    (kiss_obj*)&KISS_CFerror_output, /* fun */
    KISS_NIL,                        /* plist */
};


kiss_symbol_t KISS_Sstreamp;
kiss_cfunction_t KISS_CFstreamp = {
    KISS_CFUNCTION,      /* type */
    &KISS_Sstreamp, /* name */
    kiss_streamp,   /* C function name */
    1,              /* minimum argument number */
    1,              /* maximum argument number */
};
kiss_symbol_t KISS_Sstreamp = {
    KISS_SYMBOL,
    "streamp",
    KISS_CONSTANT_FUN,
    NULL,                       /* var */
    (kiss_obj*)&KISS_CFstreamp, /* fun */
    KISS_NIL,                        /* plist */
};


kiss_symbol_t KISS_Sinput_stream_p;
kiss_cfunction_t KISS_CFinput_stream_p = {
    KISS_CFUNCTION,      /* type */
    &KISS_Sinput_stream_p, /* name */
    kiss_input_stream_p,   /* C function name */
    1,              /* minimum argument number */
    1,              /* maximum argument number */
};
kiss_symbol_t KISS_Sinput_stream_p = {
    KISS_SYMBOL,
    "input-stream-p",
    KISS_CONSTANT_FUN,
    NULL,                       /* var */
    (kiss_obj*)&KISS_CFinput_stream_p, /* fun */
    KISS_NIL,                        /* plist */
};

kiss_symbol_t KISS_Soutput_stream_p;
kiss_cfunction_t KISS_CFoutput_stream_p = {
    KISS_CFUNCTION,      /* type */
    &KISS_Soutput_stream_p, /* name */
    kiss_output_stream_p,   /* C function name */
    1,              /* minimum argument number */
    1,              /* maximum argument number */
};
kiss_symbol_t KISS_Soutput_stream_p = {
    KISS_SYMBOL,
    "output-stream-p",
    KISS_CONSTANT_FUN,
    NULL,                       /* var */
    (kiss_obj*)&KISS_CFoutput_stream_p, /* fun */
    KISS_NIL,                        /* plist */
};


kiss_symbol_t KISS_Screate_string_input_stream;
kiss_cfunction_t KISS_CFcreate_string_input_stream = {
    KISS_CFUNCTION,      /* type */
    &KISS_Screate_string_input_stream, /* name */
    kiss_create_string_input_stream,   /* C function name */
    1,              /* minimum argument number */
    1,              /* maximum argument number */
};
kiss_symbol_t KISS_Screate_string_input_stream = {
    KISS_SYMBOL,
    "create-string-input-stream",
    KISS_CONSTANT_FUN,
    NULL,                       /* var */
    (kiss_obj*)&KISS_CFcreate_string_input_stream, /* fun */
    KISS_NIL,                        /* plist */
};


kiss_symbol_t KISS_Screate_string_output_stream;
kiss_cfunction_t KISS_CFcreate_string_output_stream = {
    KISS_CFUNCTION,      /* type */
    &KISS_Screate_string_output_stream, /* name */
    kiss_create_string_output_stream,   /* C function name */
    0,              /* minimum argument number */
    0,              /* maximum argument number */
};
kiss_symbol_t KISS_Screate_string_output_stream = {
    KISS_SYMBOL,
    "create-string-output-stream",
    KISS_CONSTANT_FUN,
    NULL,                       /* var */
    (kiss_obj*)&KISS_CFcreate_string_output_stream, /* fun */
    KISS_NIL,                        /* plist */
};


kiss_symbol_t KISS_Sread_char;
kiss_cfunction_t KISS_CFread_char = {
    KISS_CFUNCTION,      /* type */
    &KISS_Sread_char, /* name */
    kiss_read_char,   /* C function name */
    0,              /* minimum argument number */
    3,              /* maximum argument number */
};
kiss_symbol_t KISS_Sread_char = {
    KISS_SYMBOL,
    "read-char",
    KISS_CONSTANT_FUN,
    NULL,                       /* var */
    (kiss_obj*)&KISS_CFread_char, /* fun */
    KISS_NIL,                        /* plist */
};

kiss_symbol_t KISS_Spreview_char;
kiss_cfunction_t KISS_CFpreview_char = {
    KISS_CFUNCTION,      /* type */
    &KISS_Spreview_char, /* name */
    kiss_preview_char,   /* C function name */
    0,              /* minimum argument number */
    3,              /* maximum argument number */
};
kiss_symbol_t KISS_Spreview_char = {
    KISS_SYMBOL,
    "preview-char",
    KISS_CONSTANT_FUN,
    NULL,                       /* var */
    (kiss_obj*)&KISS_CFpreview_char, /* fun */
    KISS_NIL,                        /* plist */
};


kiss_symbol_t KISS_Sget_output_stream_string;
kiss_cfunction_t KISS_CFget_output_stream_string = {
    KISS_CFUNCTION,      /* type */
    &KISS_Sget_output_stream_string, /* name */
    kiss_get_output_stream_string,   /* C function name */
    1,              /* minimum argument number */
    1,              /* maximum argument number */
};
kiss_symbol_t KISS_Sget_output_stream_string = {
    KISS_SYMBOL,
    "get-output-stream-string",
    KISS_CONSTANT_FUN,
    NULL,                       /* var */
    (kiss_obj*)&KISS_CFget_output_stream_string, /* fun */
    KISS_NIL,                        /* plist */
};


kiss_symbol_t KISS_Sformat_char;
kiss_cfunction_t KISS_CFformat_char = {
    KISS_CFUNCTION,      /* type */
    &KISS_Sformat_char, /* name */
    kiss_format_char,   /* C function name */
    2,              /* minimum argument number */
    2,              /* maximum argument number */
};
kiss_symbol_t KISS_Sformat_char = {
    KISS_SYMBOL,
    "format-char",
    KISS_CONSTANT_FUN,
    NULL,                       /* var */
    (kiss_obj*)&KISS_CFformat_char, /* fun */
    KISS_NIL,                        /* plist */
};


kiss_symbol_t KISS_Sformat_integer;
kiss_cfunction_t KISS_CFformat_integer = {
    KISS_CFUNCTION,      /* type */
    &KISS_Sformat_integer, /* name */
    kiss_format_integer,   /* C function name */
    3,              /* minimum argument number */
    3,              /* maximum argument number */
};
kiss_symbol_t KISS_Sformat_integer = {
    KISS_SYMBOL,
    "format-integer",
    KISS_CONSTANT_FUN,
    NULL,                       /* var */
    (kiss_obj*)&KISS_CFformat_integer, /* fun */
    KISS_NIL,                        /* plist */
};


kiss_symbol_t KISS_Sformat_float;
kiss_cfunction_t KISS_CFformat_float = {
    KISS_CFUNCTION,      /* type */
    &KISS_Sformat_float, /* name */
    kiss_format_float,   /* C function name */
    2,              /* minimum argument number */
    2,              /* maximum argument number */
};
kiss_symbol_t KISS_Sformat_float = {
    KISS_SYMBOL,
    "format-float",
    KISS_CONSTANT_FUN,
    NULL,                       /* var */
    (kiss_obj*)&KISS_CFformat_float, /* fun */
    KISS_NIL,                        /* plist */
};


/*** error.c ***/
kiss_symbol_t KISS_Serror;
kiss_cfunction_t KISS_CFerror = {
    KISS_CFUNCTION, /* type */
    &KISS_Serror,   /* name */
    kiss_error,    /* C function name */
    1,         /* minimum argument number */
    -1,        /* maximum argument number */
};
kiss_symbol_t KISS_Serror = {
    KISS_SYMBOL,
    "error",
    KISS_CONSTANT_FUN,
    NULL,                /* var */
    (kiss_obj*)&KISS_CFerror, /* fun */
    KISS_NIL,                 /* plist */
};

kiss_symbol_t KISS_Serr;
kiss_cfunction_t KISS_CFerr = {
    KISS_CFUNCTION, /* type */
    &KISS_Serr,   /* name */
    kiss_error,    /* C function name */
    1,         /* minimum argument number */
    -1,        /* maximum argument number */
};
kiss_symbol_t KISS_Serr = {
    KISS_SYMBOL,
    "kiss::err",
    KISS_CONSTANT_FUN,
    NULL,                /* var */
    (kiss_obj*)&KISS_CFerr, /* fun */
    KISS_NIL,                 /* plist */
};


/*** object.c ***/
kiss_symbol_t KISS_Smake_object;
kiss_cfunction_t KISS_CFmake_object = {
    KISS_CFUNCTION, /* type */
    &KISS_Smake_object,   /* name */
    kiss_make_object,    /* C function name */
    1,         /* minimum argument number */
    1,        /* maximum argument number */
};
kiss_symbol_t KISS_Smake_object = {
    KISS_SYMBOL,
    "kiss::make-object",
    KISS_CONSTANT_FUN,
    NULL,                /* var */
    (kiss_obj*)&KISS_CFmake_object, /* fun */
    KISS_NIL,                 /* plist */
};

kiss_symbol_t KISS_Sobject_p;
kiss_cfunction_t KISS_CFobject_p = {
    KISS_CFUNCTION, /* type */
    &KISS_Sobject_p,   /* name */
    kiss_object_p,    /* C function name */
    1,         /* minimum argument number */
    1,        /* maximum argument number */
};
kiss_symbol_t KISS_Sobject_p = {
    KISS_SYMBOL,
    "object-p",
    KISS_CONSTANT_FUN,
    NULL,                /* var */
    (kiss_obj*)&KISS_CFobject_p, /* fun */
    KISS_NIL,                 /* plist */
};

kiss_symbol_t KISS_Sobject_plist;
kiss_cfunction_t KISS_CFobject_plist = {
    KISS_CFUNCTION, /* type */
    &KISS_Sobject_plist,   /* name */
    kiss_object_plist,    /* C function name */
    1,         /* minimum argument number */
    1,        /* maximum argument number */
};
kiss_symbol_t KISS_Sobject_plist = {
    KISS_SYMBOL,
    "object-plist",
    KISS_CONSTANT_FUN,
    NULL,                /* var */
    (kiss_obj*)&KISS_CFobject_plist, /* fun */
    KISS_NIL,                 /* plist */
};


kiss_symbol_t KISS_Sset_object_plist;
kiss_cfunction_t KISS_CFset_object_plist = {
    KISS_CFUNCTION, /* type */
    &KISS_Sset_object_plist,   /* name */
    kiss_set_object_plist,    /* C function name */
    2,         /* minimum argument number */
    2,        /* maximum argument number */
};
kiss_symbol_t KISS_Sset_object_plist = {
    KISS_SYMBOL,
    "set-object-plist",
    KISS_CONSTANT_FUN,
    NULL,                /* var */
    (kiss_obj*)&KISS_CFset_object_plist, /* fun */
    KISS_NIL,                 /* plist */
};

/*** gf_invoke.c ***/
kiss_symbol_t KISS_Smethod_invoke;
kiss_cfunction_t KISS_CFmethod_invoke = {
    KISS_CFUNCTION, /* type */
    &KISS_Smethod_invoke,   /* name */
    kiss_method_invoke,    /* C function name */
    1,         /* minimum argument number */
    1,        /* maximum argument number */
};
kiss_symbol_t KISS_Smethod_invoke = {
    KISS_SYMBOL,
    "method-invoke",
    KISS_CONSTANT_FUN,
    NULL,                /* var */
    (kiss_obj*)&KISS_CFmethod_invoke, /* fun */
    KISS_NIL,                 /* plist */
};

/*** feature.c ***/
kiss_symbol_t KISS_Sfeaturep;
kiss_cfunction_t KISS_CFfeaturep = {
    KISS_CFUNCTION, /* type */
    &KISS_Sfeaturep,   /* name */
    kiss_featurep,    /* C function name */
    1,         /* minimum argument number */
    1,        /* maximum argument number */
};
kiss_symbol_t KISS_Sfeaturep = {
    KISS_SYMBOL,
    "featurep",
    KISS_CONSTANT_FUN,
    NULL,                /* var */
    (kiss_obj*)&KISS_CFfeaturep, /* fun */
    KISS_NIL,                 /* plist */
};


kiss_symbol_t KISS_Sprovide;
kiss_cfunction_t KISS_CFprovide = {
    KISS_CFUNCTION, /* type */
    &KISS_Sprovide,   /* name */
    kiss_provide,    /* C function name */
    1,         /* minimum argument number */
    1,        /* maximum argument number */
};
kiss_symbol_t KISS_Sprovide = {
    KISS_SYMBOL,
    "provide",
    KISS_CONSTANT_FUN,
    NULL,                /* var */
    (kiss_obj*)&KISS_CFprovide, /* fun */
    KISS_NIL,                 /* plist */
};

/*** character.c ***/
kiss_symbol_t KISS_Scharacterp;
kiss_cfunction_t KISS_CFcharacterp = {
    KISS_CFUNCTION, /* type */
    &KISS_Scharacterp,   /* name */
    kiss_characterp,    /* C function name */
    1,         /* minimum argument number */
    1,        /* maximum argument number */
};
kiss_symbol_t KISS_Scharacterp = {
    KISS_SYMBOL,
    "characterp",
    KISS_CONSTANT_FUN,
    NULL,                /* var */
    (kiss_obj*)&KISS_CFcharacterp, /* fun */
    KISS_NIL,                 /* plist */
};

kiss_symbol_t KISS_Schar_eq;
kiss_cfunction_t KISS_CFchar_eq = {
    KISS_CFUNCTION, /* type */
    &KISS_Schar_eq,   /* name */
    kiss_char_eq,    /* C function name */
    2,         /* minimum argument number */
    2,        /* maximum argument number */
};
kiss_symbol_t KISS_Schar_eq = {
    KISS_SYMBOL,
    "char=",
    KISS_CONSTANT_FUN,
    NULL,                /* var */
    (kiss_obj*)&KISS_CFchar_eq, /* fun */
    KISS_NIL,                 /* plist */
};


kiss_symbol_t KISS_Schar_lessthan;
kiss_cfunction_t KISS_CFchar_lessthan = {
    KISS_CFUNCTION, /* type */
    &KISS_Schar_lessthan,   /* name */
    kiss_char_lessthan,    /* C function name */
    2,         /* minimum argument number */
    2,        /* maximum argument number */
};
kiss_symbol_t KISS_Schar_lessthan = {
    KISS_SYMBOL,
    "char<",
    KISS_CONSTANT_FUN,
    NULL,                /* var */
    (kiss_obj*)&KISS_CFchar_lessthan, /* fun */
    KISS_NIL,                 /* plist */
};



/**** symbol table ****/
static kiss_symbol_t* Kiss_Symbols[KISS_SYMBOL_MAX]= {
    &KISS_Snil, &KISS_St,

    &KISS_Skw_rest, &KISS_Samp_rest,


    /* cons.c */
    &KISS_Scar, &KISS_Scdr, &KISS_Scons, &KISS_Scadr, &KISS_Scddr,
    &KISS_Scaddr, &KISS_Sconsp,
    &KISS_Sset_car, &KISS_Sset_cdr,
    &KISS_Slist, &KISS_Sappend, &KISS_Sappend_s,
    &KISS_Sreverse, &KISS_Snreverse,
    &KISS_Smember, &KISS_Sassoc, &KISS_Scopy_list,
    &KISS_Splist_member, &KISS_Splist_put, &KISS_Splist_get, 

    /* general_vector.c */
    &KISS_Screate_general_vector, &KISS_Sgeneral_vector,
    &KISS_Sgeneral_vector_p, &KISS_Sgvref, &KISS_Sset_gvref,
    

    /* function.c */
    &KISS_Ssimple_function_p, &KISS_Sfunction, &KISS_Squote, &KISS_Slambda,
    &KISS_Sflet, &KISS_Slabels, &KISS_Sdefun, &KISS_Sdefmacro, &KISS_Sfuncall,
    &KISS_Sapply,

    /* variable.c */
    &KISS_Ssetq, &KISS_Sdefglobal, &KISS_Sdefconstant, &KISS_Slet, &KISS_Slet_s,
    &KISS_Sdefdynamic, &KISS_Sdynamic, &KISS_Sdynamic_let, &KISS_Sset_dynamic,

    /* control.c */
    &KISS_Sunwind_protect, &KISS_Scatch, &KISS_Sthrow, &KISS_Sblock,
    &KISS_Sreturn_from, &KISS_Stagbody, &KISS_Sgo,
    &KISS_Sif, &KISS_Sprogn,
    &KISS_Seq, &KISS_Seql,

    /* number.c */
    &KISS_Sintegerp, &KISS_Sfloatp, &KISS_Sminus, &KISS_Splus, &KISS_Smultiply, &KISS_Snum_eq,
    &KISS_Snum_lessthan, &KISS_Sabs, &KISS_Sexp, &KISS_Slog, &KISS_Ssin,&KISS_Scos, &KISS_Stan,
    &KISS_Sfloor, &KISS_Sceiling, &KISS_Struncate, &KISS_Sround, 
    &KISS_Sfloat,
    &KISS_Sdiv, &KISS_Smod, &KISS_Sgcd, &KISS_Slcm, 
    &KISS_Sparse_number,


    /* symbols.c */
    &KISS_Sgensym, &KISS_Ssymbolp,
    &KISS_Ssymbol_function, &KISS_Sset_symbol_function,
    &KISS_Sfboundp, &KISS_Sfmakunbound,

    /* string.c */
    &KISS_Sstringp, &KISS_Screate_string, &KISS_Sstring_append, 

    /* sequence.c */
    &KISS_Slength, &KISS_Selt,

    /* eval.c */
    &KISS_Seval,

    /* error.c */
    &KISS_Serror, &KISS_Serr,

    /* stream.c */
    &KISS_Sstandard_input, &KISS_Sstandard_output, &KISS_Serror_output,
    &KISS_Sstreamp,
    &KISS_Screate_string_input_stream, &KISS_Screate_string_output_stream,
    &KISS_Sget_output_stream_string,
    &KISS_Sinput_stream_p, &KISS_Soutput_stream_p, 
    &KISS_Sread_char, &KISS_Spreview_char, &KISS_Sformat_char, &KISS_Sformat_integer,
    &KISS_Sformat_float,

    /* format_object.c */
    &KISS_Sformat, &KISS_Sformat_object, &KISS_Sformat_pointer, &KISS_Sprint, 

    /* read.c */
    &KISS_Sread, 

    /* object.c */
    &KISS_Smake_object, &KISS_Sobject_p,
    &KISS_Sobject_plist, &KISS_Sset_object_plist,

    /* gf_invoke.c */
    &KISS_Smethod_invoke,

    /* feature.c */
    &KISS_Sfeaturep, &KISS_Sprovide,

    /* character.c */
    &KISS_Scharacterp, &KISS_Schar_eq, &KISS_Schar_lessthan, 
};

/* Uninterned symbols used in lisp reader */
kiss_symbol_t KISS_Udot = {
    KISS_SYMBOL, /* type */
    ".",   /* name */
    0,      /* info */
    NULL,   /* fun */
    NULL,   /* var */
    KISS_NIL,    /* plist */
};

kiss_symbol_t KISS_Urparen = {
    KISS_SYMBOL, /* type */
    ")",   /* name */
    0,      /* info */
    NULL,   /* fun */
    NULL,   /* var */
    KISS_NIL,    /* plist */
};

kiss_symbol_t KISS_Ucomma = {
    KISS_SYMBOL, /* type */
    ",",   /* name */
    0,      /* info */
    NULL,   /* fun */
    NULL,   /* var */
    KISS_NIL,    /* plist */
};

kiss_symbol_t KISS_Ucomma_at = {
    KISS_SYMBOL, /* type */
    ",@",  /* name */
    0,      /* info */
    NULL,   /* fun */
    NULL,   /* var */
    KISS_NIL,    /* plist */
};

kiss_symbol_t KISS_Ueos = {
    KISS_SYMBOL, /* type */
    "eos", /* name */
    0,      /* info */
    NULL,   /* fun */
    NULL,   /* var */
    KISS_NIL,    /* plist */
};

