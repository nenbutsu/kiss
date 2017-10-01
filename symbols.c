/*
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

#define KISS_SYMBOL_MAX 1024
size_t Kiss_Symbol_Number = 0;
kiss_symbol_t* Kiss_Symbols[KISS_SYMBOL_MAX];

kiss_hash_table_t* Kiss_Symbol_Hash_Table = NULL;

size_t Kiss_Gensym_Count = 0;

kiss_symbol_t KISS_Sstring_eq;

void kiss_init_symbols(void) {
    size_t i;
    for (i = 0; i < KISS_SYMBOL_MAX; i++) { if (Kiss_Symbols[i] == NULL) break; }
    assert(i < KISS_SYMBOL_MAX);
    Kiss_Symbol_Number = i;

    Kiss_Symbol_Hash_Table = (kiss_hash_table_t*)
         kiss_make_hash_table(kiss_make_fixnum(2347),
                              kiss_function((kiss_obj*)&KISS_Sstring_eq),
                              KISS_NIL,
                              (kiss_obj*)kiss_make_float(1.5),
                              (kiss_obj*)kiss_make_float(0.8));
}

static kiss_symbol_t* kiss_make_symbol(const wchar_t* const name) {
    kiss_symbol_t* p = Kiss_GC_Malloc(sizeof(kiss_symbol_t));
    p->type  = KISS_SYMBOL;
    p->name  = wcscpy(Kiss_Malloc(sizeof(wchar_t) * (wcslen(name) + 1)), name);
    p->flags = 0;
    p->var   = name[0] == L':' ? (kiss_obj*)p : NULL;
    p->fun   = NULL;
    p->plist = KISS_NIL;
    return p;
}

extern inline
kiss_obj* kiss_symbol(const wchar_t* const name);

/* function: (symbolp obj) -> boolean 
   Returns t if obj is a symbol; otherwise, returns nil. The obj may
   be any LISP object */
kiss_obj* kiss_symbolp(const kiss_obj* const obj) {
     return KISS_IS_SYMBOL(obj) ? KISS_T : KISS_NIL;
}

kiss_obj* kiss_gensym(void) {
     wchar_t name[30];
     if (swprintf(name, 30, L"#:%x", Kiss_Gensym_Count++) < 0) {
	  fwprintf(stderr, L"kiss_gensym: swprintf error\n");
	  exit(EXIT_FAILURE);
     }
     return (kiss_obj*)kiss_make_symbol(name);
}

/* kiss function: (symbol-function obj) => <function> */
kiss_obj* kiss_symbol_function (const kiss_obj* const obj) {
    kiss_symbol_t* symbol = Kiss_Symbol(obj);
    if (symbol->fun == NULL) {
	Kiss_Err(L"Unbound function ~S", obj);
    }
    return (kiss_obj*)symbol->fun;
}

/* kiss function: (fboundp obj) => t or nil */
kiss_obj* kiss_fboundp (const kiss_obj* const obj) {
     return Kiss_Symbol(obj)->fun == NULL ? KISS_NIL : KISS_T;
}

/* kiss function: (fmakunbound obj) => obj */
kiss_obj* kiss_fmakunbound (kiss_obj* const obj) {
    Kiss_Symbol(obj)->fun = NULL;
    return obj;
}

/* kiss function: (set-symbol-function definition symbol) => definition */
kiss_obj* kiss_set_symbol_function (const kiss_obj* const definition, kiss_obj* const sym) {
     Kiss_Symbol(sym)->fun = (kiss_obj*)definition;
     return (kiss_obj*)definition;
}

int kiss_is_interned(const kiss_symbol_t* const p) {
     kiss_obj* q = kiss_c_gethash((kiss_obj*)kiss_make_string(p->name),
                                  Kiss_Symbol_Hash_Table,
                                  KISS_DUMMY);
    if (q != KISS_DUMMY)
         return 1;
    else {
         for (size_t i = 0; i < Kiss_Symbol_Number; i++) {
              if (p == Kiss_Symbols[i]) return 1;
         }
         return 0;
    }
}

kiss_obj* kiss_intern(const kiss_obj* const name) {
    kiss_string_t* str = Kiss_String(name);
    kiss_obj* q = kiss_c_gethash(name, Kiss_Symbol_Hash_Table, KISS_DUMMY);

    if (q != KISS_DUMMY) {
         return q;
    }
    size_t i;
    kiss_symbol_t* p;
    for (i = 0; i < Kiss_Symbol_Number; i++) {
	if (wcscmp(Kiss_Symbols[i]->name, str->str) == 0) {
	    return (kiss_obj*)Kiss_Symbols[i];
	}
    }
    p = kiss_make_symbol(str->str);
    kiss_puthash(name, (kiss_obj*)p, (kiss_obj*)Kiss_Symbol_Hash_Table);
    //assert(Kiss_Symbol_Number < KISS_SYMBOL_MAX);
    //p = kiss_make_symbol(str->str);
    //Kiss_Symbols[Kiss_Symbol_Number] = p;
    //++Kiss_Symbol_Number;
    return (kiss_obj*)p;
}



// function: (property symbol property-name [obj]) -> <object>
kiss_obj* kiss_property(const kiss_obj* const symbol, const kiss_obj* const property, const kiss_obj* const rest)
{
     const kiss_symbol_t* const s = Kiss_Symbol(symbol);
     const kiss_obj* const p = kiss_plist_member(s->plist, (kiss_obj*)Kiss_Symbol(property));
     if (KISS_IS_CONS(p)) {
	  return kiss_car(KISS_CDR(p));
     } else {
          return rest == KISS_NIL ? KISS_NIL : kiss_car(rest);
     }
}

// function: (set-property obj symbol property-name) -> <object>
kiss_obj* kiss_set_property(const kiss_obj* const obj, kiss_obj* const symbol, const kiss_obj* const property)
{
     kiss_symbol_t* const s = Kiss_Symbol(symbol);
     s->plist = kiss_plist_put(s->plist, (kiss_obj*)Kiss_Symbol(property), obj);
     return (kiss_obj*)obj;
}

// function: (remove-property symbol property-name) -> <object>
kiss_obj* kiss_remove_property(kiss_obj* const symbol, const kiss_obj* const property) {
     kiss_symbol_t* const s = Kiss_Symbol(symbol);
     const kiss_obj* const obj = kiss_plist_get(s->plist, property);
     s->plist = kiss_plist_remove(s->plist, (kiss_obj*)Kiss_Symbol(property));
     return (kiss_obj*)obj;
}


/************************** Symbol definitions *******************************/

kiss_symbol_t KISS_Snil = {
    KISS_SYMBOL,       /* type  */
    NULL,              /* gc_ptr */
    L"nil",       /* name  */
    KISS_CONSTANT_VAR, /* flags */
    KISS_NIL,          /* var   */
    NULL,         /* fun   */
    KISS_NIL,          /* plist */
};

kiss_symbol_t KISS_St = {
    KISS_SYMBOL,       /* type */
    NULL,              /* gc_ptr */
    L"t",         /* name */
    KISS_CONSTANT_VAR, /* flags */
    KISS_T,            /* var */
    NULL,         /* fun */
    KISS_NIL,          /* plist */
};

kiss_symbol_t KISS_Skw_rest;
kiss_symbol_t KISS_Skw_rest = {
    KISS_SYMBOL,               /* type */
    NULL,              /* gc_ptr */
    L":rest",             /* name */
    0,                    /* flags */
    (kiss_obj*)&KISS_Skw_rest, /* var */
    NULL,                 /* fun */
    KISS_NIL,                  /* plist */
};

kiss_symbol_t KISS_Samp_rest;
kiss_symbol_t KISS_Samp_rest = {
    KISS_SYMBOL,               /* type */
    NULL,              /* gc_ptr */
    L"&rest",             /* name */
    0,                    /* flags */
    (kiss_obj*)&KISS_Samp_rest, /* var */
    NULL,                 /* fun */
    KISS_NIL,                  /* plist */
};

kiss_symbol_t KISS_Skw_size;
kiss_symbol_t KISS_Skw_size = {
    KISS_SYMBOL,               /* type */
    NULL,              /* gc_ptr */
    L":size",             /* name */
    0,                    /* flags */
    (kiss_obj*)&KISS_Skw_size, /* var */
    NULL,                 /* fun */
    KISS_NIL,                  /* plist */
};

kiss_symbol_t KISS_Skw_test;
kiss_symbol_t KISS_Skw_test = {
    KISS_SYMBOL,               /* type */
    NULL,              /* gc_ptr */
    L":test",             /* name */
    0,                    /* flags */
    (kiss_obj*)&KISS_Skw_test, /* var */
    NULL,                 /* fun */
    KISS_NIL,                  /* plist */
};

kiss_symbol_t KISS_Skw_weakness;
kiss_symbol_t KISS_Skw_weakness = {
    KISS_SYMBOL,               /* type */
    NULL,              /* gc_ptr */
    L":weakness",             /* name */
    0,                    /* flags */
    (kiss_obj*)&KISS_Skw_weakness, /* var */
    NULL,                 /* fun */
    KISS_NIL,                  /* plist */
};

kiss_symbol_t KISS_Skw_rehash_size;
kiss_symbol_t KISS_Skw_rehash_size = {
    KISS_SYMBOL,               /* type */
    NULL,              /* gc_ptr */
    L":rehash-size",             /* name */
    0,                    /* flags */
    (kiss_obj*)&KISS_Skw_rehash_size, /* var */
    NULL,                 /* fun */
    KISS_NIL,                  /* plist */
};

kiss_symbol_t KISS_Skw_rehash_threshold;
kiss_symbol_t KISS_Skw_rehash_threshold = {
    KISS_SYMBOL,               /* type */
    NULL,              /* gc_ptr */
    L":rehash-threshold",             /* name */
    0,                    /* flags */
    (kiss_obj*)&KISS_Skw_rehash_threshold, /* var */
    NULL,                 /* fun */
    KISS_NIL,                  /* plist */
};


/*** cons.c ***/
kiss_symbol_t KISS_Scons;
kiss_cfunction_t KISS_CFcons = {
    KISS_CFUNCTION, /* type */
    &KISS_Scons,    /* name */
    (kiss_cf_t*)kiss_cons,      /* C function name */
    2,         /* minimum argument number */
    2,         /* maximum argument number */
};
kiss_symbol_t KISS_Scons = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"cons",
    KISS_CONSTANT_FUN,
    NULL,               /* var */
    (kiss_obj*)&KISS_CFcons, /* fun */
    KISS_NIL,                /* plist */
};


kiss_symbol_t KISS_Scar;
kiss_cfunction_t KISS_CFcar = {
     KISS_CFUNCTION, /* type */
     &KISS_Scar,     /* name */
     (kiss_cf_t*)kiss_car,       /* C function name */
     1,         /* minimum argument number */
     1,         /* maximum argument number */
};
kiss_symbol_t KISS_Scar = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"car",
    KISS_CONSTANT_FUN,
    NULL,              /* var */
    (kiss_obj*)&KISS_CFcar, /* fun */
    KISS_NIL,               /* plist */
};

kiss_symbol_t KISS_Scdr;
kiss_cfunction_t KISS_CFcdr = {
    KISS_CFUNCTION, /* type */
    &KISS_Scdr,     /* name */
    (kiss_cf_t*)kiss_cdr,       /* C function name */
    1,         /* minimum argument number */
    1,         /* maximum argument number */
};
kiss_symbol_t KISS_Scdr = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"cdr",
    KISS_CONSTANT_FUN,
    NULL,              /* var */
    (kiss_obj*)&KISS_CFcdr, /* fun */
    KISS_NIL,               /* plist */
};

kiss_symbol_t KISS_Scadr;
kiss_cfunction_t KISS_CFcadr = {
    KISS_CFUNCTION, /* type */
    &KISS_Scadr,    /* name */
    (kiss_cf_t*)kiss_cadr,      /* C function name */
    1,         /* minimum argument number */
    1,         /* maximum argument number */
};
kiss_symbol_t KISS_Scadr = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"cadr",
    KISS_CONSTANT_FUN,
    NULL,               /* var */
    (kiss_obj*)&KISS_CFcadr, /* fun */
    KISS_NIL,                /* plist */
};


kiss_symbol_t KISS_Scddr;
kiss_cfunction_t KISS_CFcddr = {
    KISS_CFUNCTION, /* type */
    &KISS_Scddr,    /* name */
    (kiss_cf_t*)kiss_cddr,      /* C function name */
    1,         /* minimum argument number */
    1,         /* maximum argument number */
};
kiss_symbol_t KISS_Scddr = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"cddr",
    KISS_CONSTANT_FUN,
    NULL,               /* var */
    (kiss_obj*)&KISS_CFcddr, /* fun */
    KISS_NIL,                /* plist */
};


kiss_symbol_t KISS_Scaddr;
kiss_cfunction_t KISS_CFcaddr = {
    KISS_CFUNCTION, /* type */
    &KISS_Scaddr,   /* name */
    (kiss_cf_t*)kiss_caddr,     /* C function name */
    1,         /* minimum argument number */
    1,         /* maximum argument number */
};
kiss_symbol_t KISS_Scaddr = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"caddr",
    KISS_CONSTANT_FUN,
    NULL,                /* var */
    (kiss_obj*)&KISS_CFcaddr, /* fun */
    KISS_NIL,                 /* plist */
};


kiss_symbol_t KISS_Sconsp;
kiss_cfunction_t KISS_CFconsp = {
    KISS_CFUNCTION, /* type */
    &KISS_Sconsp,   /* name */
    (kiss_cf_t*)kiss_consp,     /* C function name */
    1,         /* minimum argument number */
    1,         /* maximum argument number */
};
kiss_symbol_t KISS_Sconsp = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"consp",
    KISS_CONSTANT_FUN,
    NULL,                /* var */
    (kiss_obj*)&KISS_CFconsp, /* fun */
    KISS_NIL,                 /* plist */
};


kiss_symbol_t KISS_Sset_car;
kiss_cfunction_t KISS_CFset_car = {
    KISS_CFUNCTION, /* type */
    &KISS_Sset_car, /* name */
    (kiss_cf_t*)kiss_set_car,   /* C function name */
    2,         /* minimum argument number */
    2,         /* maximum argument number */
};
kiss_symbol_t KISS_Sset_car = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"set-car",
    KISS_CONSTANT_FUN,
    NULL,                  /* var */
    (kiss_obj*)&KISS_CFset_car, /* fun */
    KISS_NIL,                   /* plist */
};


kiss_symbol_t KISS_Sset_cdr;
kiss_cfunction_t KISS_CFset_cdr = {
    KISS_CFUNCTION, /* type */
    &KISS_Sset_cdr, /* name */
    (kiss_cf_t*)kiss_set_cdr,   /* C function name */
    2,         /* minimum argument number */
    2,         /* maximum argument number */
};
kiss_symbol_t KISS_Sset_cdr = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"set-cdr",
    KISS_CONSTANT_FUN,
    NULL,                  /* var */
    (kiss_obj*)&KISS_CFset_cdr, /* fun */
    KISS_NIL,                   /* plist */
};

kiss_symbol_t KISS_Snull;
kiss_cfunction_t KISS_CFnull = {
    KISS_CFUNCTION, /* type */
    &KISS_Snull, /* name */
    (kiss_cf_t*)kiss_null,   /* C function name */
    1,         /* minimum argument number */
    1,         /* maximum argument number */
};
kiss_symbol_t KISS_Snull = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"null",
    KISS_CONSTANT_FUN,
    NULL,                  /* var */
    (kiss_obj*)&KISS_CFnull, /* fun */
    KISS_NIL,                   /* plist */
};

kiss_symbol_t KISS_Slistp;
kiss_cfunction_t KISS_CFlistp = {
    KISS_CFUNCTION, /* type */
    &KISS_Slistp, /* name */
    (kiss_cf_t*)kiss_listp,   /* C function name */
    1,         /* minimum argument number */
    1,         /* maximum argument number */
};
kiss_symbol_t KISS_Slistp = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"listp",
    KISS_CONSTANT_FUN,
    NULL,                  /* var */
    (kiss_obj*)&KISS_CFlistp, /* fun */
    KISS_NIL,                   /* plist */
};

kiss_symbol_t KISS_Screate_list;
kiss_cfunction_t KISS_CFcreate_list = {
    KISS_CFUNCTION, /* type */
    &KISS_Screate_list, /* name */
    (kiss_cf_t*)kiss_create_list,   /* C function name */
    1,         /* minimum argument number */
    2,         /* maximum argument number */
};
kiss_symbol_t KISS_Screate_list = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"create-list",
    KISS_CONSTANT_FUN,
    NULL,                  /* var */
    (kiss_obj*)&KISS_CFcreate_list, /* fun */
    KISS_NIL,                   /* plist */
};


kiss_symbol_t KISS_Slist;
kiss_cfunction_t KISS_CFlist = {
    KISS_CFUNCTION, /* type */
    &KISS_Slist,    /* name */
    (kiss_cf_t*)kiss_list,      /* C function name */
    0,         /* minimum argument number */
    -1,        /* maximum argument number */
};
kiss_symbol_t KISS_Slist = {
    KISS_SYMBOL,             /* type */
    NULL,              /* gc_ptr */
    L"list",            /* name */
    KISS_CONSTANT_FUN,       /* flags */
    NULL,               /* var */
    (kiss_obj*)&KISS_CFlist, /* fun */
    KISS_NIL,                /* plist */
};


kiss_symbol_t KISS_Sappend;
kiss_cfunction_t KISS_CFappend = {
    KISS_CFUNCTION, /* type */
    &KISS_Sappend,  /* name */
    (kiss_cf_t*)kiss_append,    /* C function name */
    0,         /* minimum argument number */
    -1,        /* maximum argument number */
};
kiss_symbol_t KISS_Sappend = {
    KISS_SYMBOL,               /* type */
    NULL,              /* gc_ptr */
    L"append",            /* name */
    KISS_CONSTANT_FUN,         /* flags */
    NULL,                 /* var */
    (kiss_obj*)&KISS_CFappend, /* fun */
    KISS_NIL,                  /* plist */
};

kiss_symbol_t KISS_Sappend_s;
kiss_cfunction_t KISS_CFappend_s = {
    KISS_CFUNCTION, /* type */
    &KISS_Sappend_s,  /* name */
    (kiss_cf_t*)kiss_append_s,    /* C function name */
    0,         /* minimum argument number */
    -1,        /* maximum argument number */
};
kiss_symbol_t KISS_Sappend_s = {
    KISS_SYMBOL,               /* type */
    NULL,              /* gc_ptr */
    L"append*",            /* name */
    KISS_CONSTANT_FUN,         /* flags */
    NULL,                 /* var */
    (kiss_obj*)&KISS_CFappend_s, /* fun */
    KISS_NIL,                  /* plist */
};


kiss_symbol_t KISS_Sreverse;
kiss_cfunction_t KISS_CFreverse = {
    KISS_CFUNCTION, /* type */
    &KISS_Sreverse, /* name */
    (kiss_cf_t*)kiss_reverse,   /* C function name */
    1,         /* minimum argument number */
    1,         /* maximum argument number */
};
kiss_symbol_t KISS_Sreverse = {
    KISS_SYMBOL,                /* type */
    NULL,              /* gc_ptr */
    L"reverse",            /* name */
    KISS_CONSTANT_FUN,          /* flags */
    NULL,                  /* var */
    (kiss_obj*)&KISS_CFreverse, /* fun */
    KISS_NIL,                   /* plist */
};


kiss_symbol_t KISS_Snreverse;
kiss_cfunction_t KISS_CFnreverse = {
    KISS_CFUNCTION,  /* type */
    &KISS_Snreverse, /* name */
    (kiss_cf_t*)kiss_nreverse,   /* C function name */
    1,          /* minimum argument number */
    1,          /* maximum argument number */
};
kiss_symbol_t KISS_Snreverse = {
    KISS_SYMBOL,                 /* type */
    NULL,              /* gc_ptr */
    L"nreverse",            /* name */
    KISS_CONSTANT_FUN,           /* flags */
    NULL,                   /* var */
    (kiss_obj*)&KISS_CFnreverse, /* fun */
    KISS_NIL,                    /* plist */
};


kiss_symbol_t KISS_Smember;
kiss_cfunction_t KISS_CFmember = {
    KISS_CFUNCTION, /* type */
    &KISS_Smember,  /* name */
    (kiss_cf_t*)kiss_member,    /* C function name */
    2,         /* minimum argument number */
    2,         /* maximum argument number */
};
kiss_symbol_t KISS_Smember = {
    KISS_SYMBOL,               /* type */
    NULL,              /* gc_ptr */
    L"member",            /* name */
    KISS_CONSTANT_FUN,         /* flags */
    NULL,                 /* var */
    (kiss_obj*)&KISS_CFmember, /* fun */
    KISS_NIL,                  /* plist */
};

kiss_symbol_t KISS_Smember_using;
kiss_cfunction_t KISS_CFmember_using = {
    KISS_CFUNCTION, /* type */
    &KISS_Smember_using,  /* name */
    (kiss_cf_t*)kiss_member_using,    /* C function name */
    3,         /* minimum argument number */
    3,         /* maximum argument number */
};
kiss_symbol_t KISS_Smember_using = {
    KISS_SYMBOL,               /* type */
    NULL,              /* gc_ptr */
    L"member-using",            /* name */
    KISS_CONSTANT_FUN,         /* flags */
    NULL,                 /* var */
    (kiss_obj*)&KISS_CFmember_using, /* fun */
    KISS_NIL,                  /* plist */
};

kiss_symbol_t KISS_Smapcar;
kiss_cfunction_t KISS_CFmapcar = {
    KISS_CFUNCTION, /* type */
    &KISS_Smapcar,  /* name */
    (kiss_cf_t*)kiss_mapcar,    /* C function name */
    2,         /* minimum argument number */
    -1,         /* maximum argument number */
};
kiss_symbol_t KISS_Smapcar = {
    KISS_SYMBOL,               /* type */
    NULL,              /* gc_ptr */
    L"mapcar",            /* name */
    KISS_CONSTANT_FUN,         /* flags */
    NULL,                 /* var */
    (kiss_obj*)&KISS_CFmapcar, /* fun */
    KISS_NIL,                  /* plist */
};

kiss_symbol_t KISS_Smapc;
kiss_cfunction_t KISS_CFmapc = {
    KISS_CFUNCTION, /* type */
    &KISS_Smapc,  /* name */
    (kiss_cf_t*)kiss_mapc,    /* C function name */
    2,         /* minimum argument number */
    -1,         /* maximum argument number */
};
kiss_symbol_t KISS_Smapc = {
    KISS_SYMBOL,               /* type */
    NULL,              /* gc_ptr */
    L"mapc",            /* name */
    KISS_CONSTANT_FUN,         /* flags */
    NULL,                 /* var */
    (kiss_obj*)&KISS_CFmapc, /* fun */
    KISS_NIL,                  /* plist */
};


kiss_symbol_t KISS_Sassoc;
kiss_cfunction_t KISS_CFassoc = {
    KISS_CFUNCTION, /* type */
    &KISS_Sassoc,   /* name */
    (kiss_cf_t*)kiss_assoc,     /* C function name */
    2,         /* minimum argument number */
    2,         /* maximum argument number */
};
kiss_symbol_t KISS_Sassoc = {
    KISS_SYMBOL,              /* type */
    NULL,              /* gc_ptr */
    L"assoc",            /* name */
    KISS_CONSTANT_FUN,        /* flags */
    NULL,                /* var */
    (kiss_obj*)&KISS_CFassoc, /* fun */
    KISS_NIL,                 /* plist */
};

kiss_symbol_t KISS_Sassoc_using;
kiss_cfunction_t KISS_CFassoc_using = {
    KISS_CFUNCTION, /* type */
    &KISS_Sassoc_using,   /* name */
    (kiss_cf_t*)kiss_assoc_using,     /* C function name */
    3,         /* minimum argument number */
    3,         /* maximum argument number */
};
kiss_symbol_t KISS_Sassoc_using = {
    KISS_SYMBOL,              /* type */
    NULL,              /* gc_ptr */
    L"assoc-using",            /* name */
    KISS_CONSTANT_FUN,        /* flags */
    NULL,                /* var */
    (kiss_obj*)&KISS_CFassoc_using, /* fun */
    KISS_NIL,                 /* plist */
};

kiss_symbol_t KISS_Slast;
kiss_cfunction_t KISS_CFlast = {
    KISS_CFUNCTION, /* type */
    &KISS_Slast,   /* name */
    (kiss_cf_t*)kiss_last,     /* C function name */
    1,         /* minimum argument number */
    2,         /* maximum argument number */
};
kiss_symbol_t KISS_Slast = {
    KISS_SYMBOL,              /* type */
    NULL,              /* gc_ptr */
    L"last",            /* name */
    KISS_CONSTANT_FUN,        /* flags */
    NULL,                /* var */
    (kiss_obj*)&KISS_CFlast, /* fun */
    KISS_NIL,                 /* plist */
};


kiss_symbol_t KISS_Scopy_list;
kiss_cfunction_t KISS_CFcopy_list = {
    KISS_CFUNCTION, /* type */
    &KISS_Scopy_list,   /* name */
    (kiss_cf_t*)kiss_copy_list,     /* C function name */
    1,         /* minimum argument number */
    1,         /* maximum argument number */
};
kiss_symbol_t KISS_Scopy_list = {
    KISS_SYMBOL,              /* type */
    NULL,              /* gc_ptr */
    L"copy-list",            /* name */
    KISS_CONSTANT_FUN,        /* flags */
    NULL,                /* var */
    (kiss_obj*)&KISS_CFcopy_list, /* fun */
    KISS_NIL,                 /* plist */
};


kiss_symbol_t KISS_Splist_member;
kiss_cfunction_t KISS_CFplist_member = {
    KISS_CFUNCTION, /* type */
    &KISS_Splist_member,   /* name */
    (kiss_cf_t*)kiss_plist_member,     /* C function name */
    2,         /* minimum argument number */
    2,         /* maximum argument number */
};
kiss_symbol_t KISS_Splist_member = {
    KISS_SYMBOL,              /* type */
    NULL,              /* gc_ptr */
    L"plist-member",            /* name */
    KISS_CONSTANT_FUN,        /* flags */
    NULL,                /* var */
    (kiss_obj*)&KISS_CFplist_member, /* fun */
    KISS_NIL,                 /* plist */
};

kiss_symbol_t KISS_Splist_get;
kiss_cfunction_t KISS_CFplist_get = {
    KISS_CFUNCTION, /* type */
    &KISS_Splist_get,   /* name */
    (kiss_cf_t*)kiss_plist_get,     /* C function name */
    2,         /* minimum argument number */
    2,         /* maximum argument number */
};
kiss_symbol_t KISS_Splist_get = {
    KISS_SYMBOL,              /* type */
    NULL,              /* gc_ptr */
    L"plist-get",            /* name */
    KISS_CONSTANT_FUN,        /* flags */
    NULL,                /* var */
    (kiss_obj*)&KISS_CFplist_get, /* fun */
    KISS_NIL,                 /* plist */
};


kiss_symbol_t KISS_Splist_put;
kiss_cfunction_t KISS_CFplist_put = {
    KISS_CFUNCTION, /* type */
    &KISS_Splist_put,   /* name */
    (kiss_cf_t*)kiss_plist_put,     /* C function name */
    3,         /* minimum argument number */
    3,         /* maximum argument number */
};
kiss_symbol_t KISS_Splist_put = {
    KISS_SYMBOL,              /* type */
    NULL,              /* gc_ptr */
    L"plist-put",            /* name */
    KISS_CONSTANT_FUN,        /* flags */
    NULL,                /* var */
    (kiss_obj*)&KISS_CFplist_put, /* fun */
    KISS_NIL,                 /* plist */
};


/*** array.c ***/
kiss_symbol_t KISS_Screate_array;
kiss_cfunction_t KISS_CFcreate_array = {
    KISS_CFUNCTION,               /* type */
    &KISS_Screate_array, /* name */
    (kiss_cf_t*)kiss_create_array,   /* C function name */
    1,         /* minimum argument number */
    2,         /* maximum argument number */
};
kiss_symbol_t KISS_Screate_array = {
    KISS_SYMBOL,                              /* type */
    NULL,              /* gc_ptr */
    L"create-array",            /* name */
    KISS_CONSTANT_FUN,                        /* flags */
    NULL,                                /* var */
    (kiss_obj*)&KISS_CFcreate_array, /* fun */
    KISS_NIL,                                 /* plist */
};

kiss_symbol_t KISS_Sgaref;
kiss_cfunction_t KISS_CFgaref = {
    KISS_CFUNCTION,               /* type */
    &KISS_Sgaref, /* name */
    (kiss_cf_t*)kiss_garef,   /* C function name */
    1,         /* minimum argument number */
    -1,         /* maximum argument number */
};
kiss_symbol_t KISS_Sgaref = {
    KISS_SYMBOL,                              /* type */
    NULL,              /* gc_ptr */
    L"garef",            /* name */
    KISS_CONSTANT_FUN,                        /* flags */
    NULL,                                /* var */
    (kiss_obj*)&KISS_CFgaref, /* fun */
    KISS_NIL,                                 /* plist */
};

kiss_symbol_t KISS_Saref;
kiss_cfunction_t KISS_CFaref = {
    KISS_CFUNCTION,               /* type */
    &KISS_Saref, /* name */
    (kiss_cf_t*)kiss_aref,   /* C function name */
    1,         /* minimum argument number */
    -1,         /* maximum argument number */
};
kiss_symbol_t KISS_Saref = {
    KISS_SYMBOL,                              /* type */
    NULL,              /* gc_ptr */
    L"aref",            /* name */
    KISS_CONSTANT_FUN,                        /* flags */
    NULL,                                /* var */
    (kiss_obj*)&KISS_CFaref, /* fun */
    KISS_NIL,                                 /* plist */
};

kiss_symbol_t KISS_Sset_garef;
kiss_cfunction_t KISS_CFset_garef = {
    KISS_CFUNCTION,               /* type */
    &KISS_Sset_garef, /* name */
    (kiss_cf_t*)kiss_set_garef,   /* C function name */
    2,         /* minimum argument number */
    -1,         /* maximum argument number */
};
kiss_symbol_t KISS_Sset_garef = {
    KISS_SYMBOL,                              /* type */
    NULL,              /* gc_ptr */
    L"set-garef",            /* name */
    KISS_CONSTANT_FUN,                        /* flags */
    NULL,                                /* var */
    (kiss_obj*)&KISS_CFset_garef, /* fun */
    KISS_NIL,                                 /* plist */
};

kiss_symbol_t KISS_Sset_aref;
kiss_cfunction_t KISS_CFset_aref = {
    KISS_CFUNCTION,               /* type */
    &KISS_Sset_aref, /* name */
    (kiss_cf_t*)kiss_set_aref,   /* C function name */
    2,         /* minimum argument number */
    -1,         /* maximum argument number */
};
kiss_symbol_t KISS_Sset_aref = {
    KISS_SYMBOL,                              /* type */
    NULL,              /* gc_ptr */
    L"set-aref",            /* name */
    KISS_CONSTANT_FUN,                        /* flags */
    NULL,                                /* var */
    (kiss_obj*)&KISS_CFset_aref, /* fun */
    KISS_NIL,                                 /* plist */
};


kiss_symbol_t KISS_Sbasic_array_p;
kiss_cfunction_t KISS_CFbasic_array_p = {
    KISS_CFUNCTION,               /* type */
    &KISS_Sbasic_array_p, /* name */
    (kiss_cf_t*)kiss_basic_array_p,   /* C function name */
    1,         /* minimum argument number */
    1,         /* maximum argument number */
};
kiss_symbol_t KISS_Sbasic_array_p = {
    KISS_SYMBOL,                              /* type */
    NULL,              /* gc_ptr */
    L"basic-array-p",            /* name */
    KISS_CONSTANT_FUN,                        /* flags */
    NULL,                                /* var */
    (kiss_obj*)&KISS_CFbasic_array_p, /* fun */
    KISS_NIL,                                 /* plist */
};

kiss_symbol_t KISS_Sbasic_array_s_p;
kiss_cfunction_t KISS_CFbasic_array_s_p = {
    KISS_CFUNCTION,               /* type */
    &KISS_Sbasic_array_s_p, /* name */
    (kiss_cf_t*)kiss_basic_array_s_p,   /* C function name */
    1,         /* minimum argument number */
    1,         /* maximum argument number */
};
kiss_symbol_t KISS_Sbasic_array_s_p = {
    KISS_SYMBOL,                              /* type */
    NULL,              /* gc_ptr */
    L"basic-array*-p",            /* name */
    KISS_CONSTANT_FUN,                        /* flags */
    NULL,                                /* var */
    (kiss_obj*)&KISS_CFbasic_array_s_p, /* fun */
    KISS_NIL,                                 /* plist */
};

kiss_symbol_t KISS_Sgeneral_array_s_p;
kiss_cfunction_t KISS_CFgeneral_array_s_p = {
    KISS_CFUNCTION,               /* type */
    &KISS_Sgeneral_array_s_p, /* name */
    (kiss_cf_t*)kiss_general_array_s_p,   /* C function name */
    1,         /* minimum argument number */
    1,         /* maximum argument number */
};
kiss_symbol_t KISS_Sgeneral_array_s_p = {
    KISS_SYMBOL,                              /* type */
    NULL,              /* gc_ptr */
    L"general-array*-p",            /* name */
    KISS_CONSTANT_FUN,                        /* flags */
    NULL,                                /* var */
    (kiss_obj*)&KISS_CFgeneral_array_s_p, /* fun */
    KISS_NIL,                                 /* plist */
};

kiss_symbol_t KISS_Sarray_dimensions;
kiss_cfunction_t KISS_CFarray_dimensions = {
    KISS_CFUNCTION,               /* type */
    &KISS_Sarray_dimensions, /* name */
    (kiss_cf_t*)kiss_array_dimensions,   /* C function name */
    1,         /* minimum argument number */
    1,         /* maximum argument number */
};
kiss_symbol_t KISS_Sarray_dimensions = {
    KISS_SYMBOL,                              /* type */
    NULL,              /* gc_ptr */
    L"array-dimensions",            /* name */
    KISS_CONSTANT_FUN,                        /* flags */
    NULL,                                /* var */
    (kiss_obj*)&KISS_CFarray_dimensions, /* fun */
    KISS_NIL,                                 /* plist */
};

kiss_symbol_t KISS_Sgeneral_array_s_to_list;
kiss_cfunction_t KISS_CFgeneral_array_s_to_list = {
    KISS_CFUNCTION,               /* type */
    &KISS_Sgeneral_array_s_to_list, /* name */
    (kiss_cf_t*)kiss_general_array_s_to_list,   /* C function name */
    1,         /* minimum argument number */
    1,         /* maximum argument number */
};
kiss_symbol_t KISS_Sgeneral_array_s_to_list = {
    KISS_SYMBOL,                              /* type */
    NULL,              /* gc_ptr */
    L"kiss::general-array*-to-list",            /* name */
    KISS_CONSTANT_FUN,                        /* flags */
    NULL,                                /* var */
    (kiss_obj*)&KISS_CFgeneral_array_s_to_list, /* fun */
    KISS_NIL,                                 /* plist */
};


/*** vector.c ***/
kiss_symbol_t KISS_Screate_general_vector;
kiss_cfunction_t KISS_CFcreate_general_vector = {
    KISS_CFUNCTION,               /* type */
    &KISS_Screate_general_vector, /* name */
    (kiss_cf_t*)kiss_create_general_vector,   /* C function name */
    1,         /* minimum argument number */
    2,         /* maximum argument number */
};
kiss_symbol_t KISS_Screate_general_vector = {
    KISS_SYMBOL,                              /* type */
    NULL,              /* gc_ptr */
    L"create-general-vector",            /* name */
    KISS_CONSTANT_FUN,                        /* flags */
    NULL,                                /* var */
    (kiss_obj*)&KISS_CFcreate_general_vector, /* fun */
    KISS_NIL,                                 /* plist */
};

kiss_symbol_t KISS_Svector;
kiss_cfunction_t KISS_CFvector = {
    KISS_CFUNCTION,        /* type */
    &KISS_Svector, /* name */
    (kiss_cf_t*)kiss_vector,   /* C function name */
    0,                /* minimum argument number */
    -1,               /* maximum argument number */
};
kiss_symbol_t KISS_Svector = {
    KISS_SYMBOL,                       /* type */
    NULL,              /* gc_ptr */
    L"vector",            /* name */
    KISS_CONSTANT_FUN,                 /* flags */
    NULL,                         /* var */
    (kiss_obj*)&KISS_CFvector, /* fun */
    KISS_NIL,                          /* plist */
};

kiss_symbol_t KISS_Sgeneral_vector_p;
kiss_cfunction_t KISS_CFgeneral_vector_p = {
    KISS_CFUNCTION,          /* type */
    &KISS_Sgeneral_vector_p, /* name */
    (kiss_cf_t*)kiss_general_vector_p,   /* C function name */
    1,                  /* minimum argument number */
    1,                  /* maximum argument number */
};
kiss_symbol_t KISS_Sgeneral_vector_p = {
    KISS_SYMBOL,                         /* type */
    NULL,              /* gc_ptr */
    L"general-vector-p",            /* name */
    KISS_CONSTANT_FUN,                   /* flags */
    NULL,                           /* var */
    (kiss_obj*)&KISS_CFgeneral_vector_p, /* fun */
    KISS_NIL,                            /* plist */
};

kiss_symbol_t KISS_Sbasic_vector_p;
kiss_cfunction_t KISS_CFbasic_vector_p = {
    KISS_CFUNCTION,          /* type */
    &KISS_Sbasic_vector_p, /* name */
    (kiss_cf_t*)kiss_basic_vector_p,   /* C function name */
    1,                  /* minimum argument number */
    1,                  /* maximum argument number */
};
kiss_symbol_t KISS_Sbasic_vector_p = {
    KISS_SYMBOL,                         /* type */
    NULL,              /* gc_ptr */
    L"basic-vector-p",            /* name */
    KISS_CONSTANT_FUN,                   /* flags */
    NULL,                           /* var */
    (kiss_obj*)&KISS_CFbasic_vector_p, /* fun */
    KISS_NIL,                            /* plist */
};

kiss_symbol_t KISS_Sgvref;
kiss_cfunction_t KISS_CFgvref = {
    KISS_CFUNCTION, /* type */
    &KISS_Sgvref,   /* name */
    (kiss_cf_t*)kiss_gvref,     /* C function name */
    2,         /* minimum argument number */
    2,         /* maximum argument number */
};
kiss_symbol_t KISS_Sgvref = {
    KISS_SYMBOL,              /* type */
    NULL,              /* gc_ptr */
    L"gvref",            /* name */
    KISS_CONSTANT_FUN,        /* flags */
    NULL,                /* var */
    (kiss_obj*)&KISS_CFgvref, /* fun */
    KISS_NIL,                 /* plist */
};

kiss_symbol_t KISS_Sset_gvref;
kiss_cfunction_t KISS_CFset_gvref = {
    KISS_CFUNCTION, /* type */
    &KISS_Sset_gvref,   /* name */
    (kiss_cf_t*)kiss_set_gvref,     /* C function name */
    3,         /* minimum argument number */
    3,         /* maximum argument number */
};
kiss_symbol_t KISS_Sset_gvref = {
    KISS_SYMBOL,                  /* type */
    NULL,              /* gc_ptr */
    L"set-gvref",            /* name */
    KISS_CONSTANT_FUN,            /* flags */
    NULL,                    /* var */
    (kiss_obj*)&KISS_CFset_gvref, /* fun */
    KISS_NIL,                     /* plist */
};

/*** hash_table.c ***/
kiss_symbol_t KISS_Screate_hash_table;
kiss_cfunction_t KISS_CFcreate_hash_table = {
    KISS_CFUNCTION, /* type */
    &KISS_Screate_hash_table,   /* name */
    (kiss_cf_t*)kiss_create_hash_table,     /* C function name */
    0,         /* minimum argument number */
    -1,         /* maximum argument number */
};
kiss_symbol_t KISS_Screate_hash_table = {
    KISS_SYMBOL,                  /* type */
    NULL,              /* gc_ptr */
    L"create-hash-table",            /* name */
    KISS_CONSTANT_FUN,            /* flags */
    NULL,                    /* var */
    (kiss_obj*)&KISS_CFcreate_hash_table, /* fun */
    KISS_NIL,                     /* plist */
};

kiss_symbol_t KISS_Sgethash;
kiss_cfunction_t KISS_CFgethash = {
    KISS_CFUNCTION, /* type */
    &KISS_Sgethash,   /* name */
    (kiss_cf_t*)kiss_gethash,     /* C function name */
    2,         /* minimum argument number */
    3,         /* maximum argument number */
};
kiss_symbol_t KISS_Sgethash = {
    KISS_SYMBOL,                  /* type */
    NULL,              /* gc_ptr */
    L"gethash",            /* name */
    KISS_CONSTANT_FUN,            /* flags */
    NULL,                    /* var */
    (kiss_obj*)&KISS_CFgethash, /* fun */
    KISS_NIL,                     /* plist */
};

kiss_symbol_t KISS_Sputhash;
kiss_cfunction_t KISS_CFputhash = {
    KISS_CFUNCTION, /* type */
    &KISS_Sputhash,   /* name */
    (kiss_cf_t*)kiss_puthash,     /* C function name */
    3,         /* minimum argument number */
    3,         /* maximum argument number */
};
kiss_symbol_t KISS_Sputhash = {
    KISS_SYMBOL,                  /* type */
    NULL,              /* gc_ptr */
    L"puthash",            /* name */
    KISS_CONSTANT_FUN,            /* flags */
    NULL,                    /* var */
    (kiss_obj*)&KISS_CFputhash, /* fun */
    KISS_NIL,                     /* plist */
};


/*** function.c ***/
kiss_symbol_t KISS_Ssimple_function_p;
kiss_cfunction_t KISS_CFsimple_function_p = {
    KISS_CFUNCTION,   /* type */
    &KISS_Ssimple_function_p, /* name */
    (kiss_cf_t*)kiss_simple_function_p,   /* C function name */
    1,          /* minimum argument number */
    1,          /* maximum argument number */
};
kiss_symbol_t KISS_Ssimple_function_p = {
    KISS_SYMBOL,                 /* type */
    NULL,              /* gc_ptr */
    L"simple-function-p",            /* name */
    KISS_CONSTANT_FUN,           /* info */
    NULL,                   /* var */
    (kiss_obj*)&KISS_CFsimple_function_p, /* fun */
    KISS_NIL,                    /* plist */
};

kiss_symbol_t KISS_Sfunction;
kiss_cfunction_t KISS_CFfunction = {
    KISS_CMACRO,     /* type */
    &KISS_Sfunction, /* name */
    (kiss_cf_t*)kiss_function,   /* C function name */
    1,          /* minimum argument number */
    1,          /* maximum argument number */
};
kiss_symbol_t KISS_Sfunction = {
    KISS_SYMBOL,                 /* type */
    NULL,              /* gc_ptr */
    L"function",            /* name */
    KISS_CONSTANT_FUN,           /* info */
    NULL,                   /* var */
    (kiss_obj*)&KISS_CFfunction, /* fun */
    KISS_NIL,                    /* plist */
};


kiss_symbol_t KISS_Slambda;
kiss_cfunction_t KISS_CFlambda = {
    KISS_CMACRO,   /* type */
    &KISS_Slambda, /* name */
    (kiss_cf_t*)kiss_lambda,   /* C function name */
    1,        /* minimum argument number */
    -1,       /* maximum argument number */
};
kiss_symbol_t KISS_Slambda = {
    KISS_SYMBOL,               /* type */
    NULL,              /* gc_ptr */
    L"lambda",            /* name */
    KISS_CONSTANT_FUN,         /* info */
    NULL,                 /* var */
    (kiss_obj*)&KISS_CFlambda, /* fun */
    KISS_NIL,                  /* plist */
};


kiss_symbol_t KISS_Sflet;
kiss_cfunction_t KISS_CFflet = {
    KISS_CMACRO, /* type */
    &KISS_Sflet, /* name */
    (kiss_cf_t*)kiss_flet,   /* C function name */
    1,      /* minimum argument number */
    -1,     /* maximum argument number */
};
kiss_symbol_t KISS_Sflet = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"flet",
    KISS_CONSTANT_FUN,
    NULL,               /* var */
    (kiss_obj*)&KISS_CFflet, /* fun */
    KISS_NIL,                /* plist */
};


kiss_symbol_t KISS_Slabels;
kiss_cfunction_t KISS_CFlabels = {
    KISS_CMACRO,   /* type */
    &KISS_Slabels, /* name */
    (kiss_cf_t*)kiss_labels,   /* C function name */
    1,        /* minimum argument number */
    -1,       /* maximum argument number */
};
kiss_symbol_t KISS_Slabels = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"labels",
    KISS_CONSTANT_FUN,
    NULL,                 /* var */
    (kiss_obj*)&KISS_CFlabels, /* fun */
    KISS_NIL,                  /* plist */
};


kiss_symbol_t KISS_Sdefun;
kiss_cfunction_t KISS_Cdefun = {
    KISS_CMACRO,  /* type */
    &KISS_Sdefun, /* name */
    (kiss_cf_t*)kiss_defun,   /* C function name */
    2,       /* minimum argument number */
    -1       /* maximum argument number */
};
kiss_symbol_t KISS_Sdefun = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"defun",
    KISS_CONSTANT_FUN,
    NULL,               /* var */
    (kiss_obj*)&KISS_Cdefun, /* fun */
    KISS_NIL,                /* plist */
};


kiss_symbol_t KISS_Sdefmacro;
kiss_cfunction_t KISS_Cdefmacro = {
    KISS_CMACRO,     /* type */
    &KISS_Sdefmacro, /* name */
    (kiss_cf_t*)kiss_defmacro,   /* C function name */
    2,          /* minimum argument number */
    -1          /* maximum argument number */
};
kiss_symbol_t KISS_Sdefmacro = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"defmacro",
    KISS_CONSTANT_FUN,
    NULL,                  /* var */
    (kiss_obj*)&KISS_Cdefmacro, /* fun */
    KISS_NIL,                   /* plist */
};


kiss_symbol_t KISS_Sfuncall;
kiss_cfunction_t KISS_CFfuncall = {
    KISS_CFUNCTION, /* type */
    &KISS_Sfuncall, /* name */
    (kiss_cf_t*)kiss_funcall,   /* C function name */
    1,         /* minimum argument number */
    -1,        /* maximum argument number */
};
kiss_symbol_t KISS_Sfuncall = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"funcall",
    KISS_CONSTANT_FUN,
    NULL,                  /* var */
    (kiss_obj*)&KISS_CFfuncall, /* fun */
    KISS_NIL,                   /* plist */
};


kiss_symbol_t KISS_Sapply;
kiss_cfunction_t KISS_CFapply = {
    KISS_CFUNCTION, /* type */
    &KISS_Sapply,   /* name */
    (kiss_cf_t*)kiss_apply,     /* C function name */
    2,         /* minimum argument number */
    -1,        /* maximum argument number */
};
kiss_symbol_t KISS_Sapply = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"apply",
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
    (kiss_cf_t*)kiss_setq,   /* C function name */
    2,      /* minimum argument number */
    2,      /* maximum argument number */
};
kiss_symbol_t KISS_Ssetq = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"setq",
    KISS_CONSTANT_FUN,
    NULL,               /* var */
    (kiss_obj*)&KISS_CFsetq, /* fun */
    KISS_NIL,                /* plist */
};


kiss_symbol_t KISS_Sdefglobal;
kiss_cfunction_t KISS_CFdefglobal = {
    KISS_CMACRO,      /* type */
    &KISS_Sdefglobal, /* name */
    (kiss_cf_t*)kiss_defglobal,   /* C function name */
    2,           /* minimum argument number */
    2,           /* maximum argument number */
};
kiss_symbol_t KISS_Sdefglobal = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"defglobal",
    KISS_CONSTANT_FUN,
    NULL,                         /* var */
    (kiss_obj*)&KISS_CFdefglobal, /* fun */
    KISS_NIL,                     /* plist */
};

kiss_symbol_t KISS_Sdefconstant;
kiss_cfunction_t KISS_CFdefconstant = {
    KISS_CMACRO,        /* type */
    &KISS_Sdefconstant, /* name */
    (kiss_cf_t*)kiss_defconstant,   /* C function name */
    2,                  /* minimum argument number */
    2,                  /* maximum argument number */
};
kiss_symbol_t KISS_Sdefconstant = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"defconstant",
    KISS_CONSTANT_FUN,
    NULL,                           /* var */
    (kiss_obj*)&KISS_CFdefconstant, /* fun */
    KISS_NIL,                       /* plist */
};


kiss_symbol_t KISS_Slet;
kiss_cfunction_t KISS_CFlet = {
    KISS_CMACRO, /* type */
    &KISS_Slet,  /* name */
    (kiss_cf_t*)kiss_let,    /* C function name */
    1,      /* minimum argument number */
    -1,     /* maximum argument number */
};
kiss_symbol_t KISS_Slet = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"let",
    KISS_CONSTANT_FUN,
    NULL,              /* var */
    (kiss_obj*)&KISS_CFlet, /* fun */
    KISS_NIL,               /* plist */
};


kiss_symbol_t KISS_Slet_s;
kiss_cfunction_t KISS_CFlet_s = {
    KISS_CMACRO,  /* type */
    &KISS_Slet_s, /* name */
    (kiss_cf_t*)kiss_let_s,   /* C function name */
    1,       /* minimum argument number */
    -1,      /* maximum argument number */
};
kiss_symbol_t KISS_Slet_s = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"let*",
    KISS_CONSTANT_FUN,
    NULL,                /* var */
    (kiss_obj*)&KISS_CFlet_s, /* fun */
    KISS_NIL,                 /* plist */
};


kiss_symbol_t KISS_Sdefdynamic;
kiss_cfunction_t KISS_CFdefdynamic = {
    KISS_CMACRO,       /* type */
    &KISS_Sdefdynamic, /* name */
    (kiss_cf_t*)kiss_defdynamic,   /* C function name */
    2,            /* minimum argument number */
    2,            /* maximum argument number */
};
kiss_symbol_t KISS_Sdefdynamic = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"defdynamic",
    KISS_CONSTANT_FUN,
    NULL,                     /* var */
    (kiss_obj*)&KISS_CFdefdynamic, /* fun */
    KISS_NIL,                      /* plist */
};

kiss_symbol_t KISS_Sdynamic;
kiss_cfunction_t KISS_CFdynamic = {
    KISS_CMACRO,    /* type */
    &KISS_Sdynamic, /* name */
    (kiss_cf_t*)kiss_dynamic,   /* C function name */
    1,         /* minimum argument number */
    1,         /* maximum argument number */
};
kiss_symbol_t KISS_Sdynamic = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"dynamic",
    KISS_CONSTANT_FUN,
    NULL,                  /* var */
    (kiss_obj*)&KISS_CFdynamic, /* fun */
    KISS_NIL,                   /* plist */
};


kiss_symbol_t KISS_Sdynamic_let;
kiss_cfunction_t KISS_CFdynamic_let = {
    KISS_CMACRO,        /* type */
    &KISS_Sdynamic_let, /* name */
    (kiss_cf_t*)kiss_dynamic_let,   /* C function name */
    1,             /* minimum argument number */
    -1,            /* maximum argument number */
};
kiss_symbol_t KISS_Sdynamic_let = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"dynamic-let",
    KISS_CONSTANT_FUN,
    NULL,                      /* var */
    (kiss_obj*)&KISS_CFdynamic_let, /* fun */
    KISS_NIL,                       /* plist */
};

kiss_symbol_t KISS_Sset_dynamic;
kiss_cfunction_t KISS_CFset_dynamic = {
    KISS_CMACRO,        /* type */
    &KISS_Sset_dynamic, /* name */
    (kiss_cf_t*)kiss_set_dynamic,   /* C function name */
    2,             /* minimum argument number */
    2,            /* maximum argument number */
};
kiss_symbol_t KISS_Sset_dynamic = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"set-dynamic",
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
    (kiss_cf_t*)kiss_quote,   /* C function name */
    1,       /* minimum argument number */
    1,       /* maximum argument number */
};
kiss_symbol_t KISS_Squote = {
    KISS_SYMBOL,              /* type */
    NULL,              /* gc_ptr */
    L"quote",            /* name */
    KISS_CONSTANT_FUN,        /* info */
    NULL,                /* var */
    (kiss_obj*)&KISS_CFquote, /* fun */
    KISS_NIL,                 /* plist */
};

kiss_symbol_t KISS_Snot;
kiss_cfunction_t KISS_CFnot = {
    KISS_CFUNCTION,  /* type */
    &KISS_Snot, /* name */
    (kiss_cf_t*)kiss_not,   /* C function name */
    1,       /* minimum argument number */
    1,       /* maximum argument number */
};
kiss_symbol_t KISS_Snot = {
    KISS_SYMBOL,              /* type */
    NULL,              /* gc_ptr */
    L"not",            /* name */
    KISS_CONSTANT_FUN,        /* info */
    NULL,                /* var */
    (kiss_obj*)&KISS_CFnot, /* fun */
    KISS_NIL,                 /* plist */
};

kiss_symbol_t KISS_Sand;
kiss_cfunction_t KISS_CFand = {
    KISS_CMACRO,  /* type */
    &KISS_Sand, /* name */
    (kiss_cf_t*)kiss_and,   /* C function name */
    0,       /* minimum argument number */
    -1,       /* maximum argument number */
};
kiss_symbol_t KISS_Sand = {
    KISS_SYMBOL,              /* type */
    NULL,              /* gc_ptr */
    L"and",            /* name */
    KISS_CONSTANT_FUN,        /* info */
    NULL,                /* var */
    (kiss_obj*)&KISS_CFand, /* fun */
    KISS_NIL,                 /* plist */
};

kiss_symbol_t KISS_Sor;
kiss_cfunction_t KISS_CFor = {
    KISS_CMACRO,  /* type */
    &KISS_Sor, /* name */
    (kiss_cf_t*)kiss_or,   /* C function name */
    0,       /* minimum argument number */
    -1,       /* maximum argument number */
};
kiss_symbol_t KISS_Sor = {
    KISS_SYMBOL,              /* type */
    NULL,              /* gc_ptr */
    L"or",            /* name */
    KISS_CONSTANT_FUN,        /* info */
    NULL,                /* var */
    (kiss_obj*)&KISS_CFor, /* fun */
    KISS_NIL,                 /* plist */
};

kiss_symbol_t KISS_Sequal;
kiss_cfunction_t KISS_CFequal = {
    KISS_CFUNCTION,  /* type */
    &KISS_Sequal, /* name */
    (kiss_cf_t*)kiss_equal,   /* C function name */
    2,       /* minimum argument number */
    2,       /* maximum argument number */
};
kiss_symbol_t KISS_Sequal = {
    KISS_SYMBOL,              /* type */
    NULL,              /* gc_ptr */
    L"equal",            /* name */
    KISS_CONSTANT_FUN,        /* info */
    NULL,                /* var */
    (kiss_obj*)&KISS_CFequal, /* fun */
    KISS_NIL,                 /* plist */
};

kiss_symbol_t KISS_Scond;
kiss_cfunction_t KISS_CFcond = {
    KISS_CMACRO,  /* type */
    &KISS_Scond, /* name */
    (kiss_cf_t*)kiss_cond,   /* C function name */
    0,       /* minimum argument number */
    -1,       /* maximum argument number */
};
kiss_symbol_t KISS_Scond = {
    KISS_SYMBOL,              /* type */
    NULL,              /* gc_ptr */
    L"cond",            /* name */
    KISS_CONSTANT_FUN,        /* info */
    NULL,                /* var */
    (kiss_obj*)&KISS_CFcond, /* fun */
    KISS_NIL,                 /* plist */
};


kiss_symbol_t KISS_Sif;
kiss_cfunction_t KISS_CFif = {
    KISS_CMACRO, /* type */
    &KISS_Sif,   /* name */
    (kiss_cf_t*)kiss_if,    /* C function name */
    2,      /* minimum argument number */
    3,      /* maximum argument number */
};
kiss_symbol_t KISS_Sif = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"if",
    KISS_CONSTANT_FUN,
    NULL,             /* var */
    (kiss_obj*)&KISS_CFif, /* fun */
    KISS_NIL,              /* plist */
};

kiss_symbol_t KISS_Scase;
kiss_cfunction_t KISS_CFcase = {
    KISS_CMACRO, /* type */
    &KISS_Scase,   /* name */
    (kiss_cf_t*)kiss_case,    /* C function name */
    1,      /* minimum argument number */
    -1,      /* maximum argument number */
};
kiss_symbol_t KISS_Scase = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"case",
    KISS_CONSTANT_FUN,
    NULL,             /* var */
    (kiss_obj*)&KISS_CFcase, /* fun */
    KISS_NIL,              /* plist */
};

kiss_symbol_t KISS_Scase_using;
kiss_cfunction_t KISS_CFcase_using = {
    KISS_CMACRO, /* type */
    &KISS_Scase_using,   /* name */
    (kiss_cf_t*)kiss_case_using,    /* C function name */
    2,      /* minimum argument number */
    -1,      /* maximum argument number */
};
kiss_symbol_t KISS_Scase_using = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"case-using",
    KISS_CONSTANT_FUN,
    NULL,             /* var */
    (kiss_obj*)&KISS_CFcase_using, /* fun */
    KISS_NIL,              /* plist */
};

kiss_symbol_t KISS_Sprogn;
kiss_cfunction_t KISS_CFprogn = {
    KISS_CMACRO,  /* type */
    &KISS_Sprogn, /* name */
    (kiss_cf_t*)kiss_progn,   /* C function name */
    0,       /* minimum argument number */
    -1,      /* maximum argument number */
};
kiss_symbol_t KISS_Sprogn = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"progn",
    KISS_CONSTANT_FUN,
    NULL,             /* var */
    (kiss_obj*)&KISS_CFprogn, /* fun */
    KISS_NIL,              /* plist */
};

kiss_symbol_t KISS_Sprog1;
kiss_cfunction_t KISS_CFprog1 = {
    KISS_CMACRO,  /* type */
    &KISS_Sprog1, /* name */
    (kiss_cf_t*)kiss_prog1,   /* C function name */
    1,       /* minimum argument number */
    -1,      /* maximum argument number */
};
kiss_symbol_t KISS_Sprog1 = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"prog1",
    KISS_CONSTANT_FUN,
    NULL,             /* var */
    (kiss_obj*)&KISS_CFprog1, /* fun */
    KISS_NIL,              /* plist */
};

kiss_symbol_t KISS_Swhile;
kiss_cfunction_t KISS_CFwhile = {
    KISS_CMACRO,  /* type */
    &KISS_Swhile, /* name */
    (kiss_cf_t*)kiss_while,   /* C function name */
    1,       /* minimum argument number */
    -1,      /* maximum argument number */
};
kiss_symbol_t KISS_Swhile = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"while",
    KISS_CONSTANT_FUN,
    NULL,             /* var */
    (kiss_obj*)&KISS_CFwhile, /* fun */
    KISS_NIL,              /* plist */
};


kiss_symbol_t KISS_Seq;
kiss_cfunction_t KISS_CFeq = {
    KISS_CFUNCTION, /* type */
    &KISS_Seq,      /* name */
    (kiss_cf_t*)kiss_eq,        /* C function name */
    2,         /* minimum argument number */
    2,         /* maximum argument number */
};
kiss_symbol_t KISS_Seq = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"eq",
    KISS_CONSTANT_FUN,
    NULL,             /* var */
    (kiss_obj*)&KISS_CFeq, /* fun */
    KISS_NIL,              /* plist */
};


kiss_symbol_t KISS_Seql;
kiss_cfunction_t KISS_CFeql = {
    KISS_CFUNCTION, /* type */
    &KISS_Seql,     /* name */
    (kiss_cf_t*)kiss_eql,       /* C function name */
    2,         /* minimum argument number */
    2,         /* maximum argument number */
};
kiss_symbol_t KISS_Seql = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"eql",
    KISS_CONSTANT_FUN,
    NULL,              /* var */
    (kiss_obj*)&KISS_CFeql, /* fun */
    KISS_NIL,               /* plist */
};

kiss_symbol_t KISS_Scatch;
kiss_cfunction_t KISS_Ccatch = {
    KISS_CMACRO,  /* type */
    &KISS_Scatch, /* name */
    (kiss_cf_t*)kiss_catch,   /* C function name */
    1,       /* minimum argument number */
    -1,      /* maximum argument number */
};
kiss_symbol_t KISS_Scatch = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"catch",
    KISS_CONSTANT_FUN,
    NULL,               /* var */
    (kiss_obj*)&KISS_Ccatch, /* fun */
    KISS_NIL,                /* plist */
};


kiss_symbol_t KISS_Sthrow;
kiss_cfunction_t KISS_Cthrow = {
    KISS_CMACRO,  /* type */
    &KISS_Sthrow, /* name */
    (kiss_cf_t*)kiss_throw,   /* C function name */
    2,       /* minimum argument number */
    2,       /* maximum argument number */
};
kiss_symbol_t KISS_Sthrow = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"throw",
    KISS_CONSTANT_FUN,
    NULL,               /* var */
    (kiss_obj*)&KISS_Cthrow, /* fun */
    KISS_NIL,                /* plist */
};


kiss_symbol_t KISS_Sunwind_protect;
kiss_cfunction_t KISS_Cunwind_protect = {
    KISS_CMACRO,           /* type */
    &KISS_Sunwind_protect, /* name */
    (kiss_cf_t*)kiss_unwind_protect,   /* C function name */
    1,                /* minimum argument number */
    -1,               /* maximum argument number */
};
kiss_symbol_t KISS_Sunwind_protect = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"unwind-protect",
    KISS_CONSTANT_FUN,
    NULL,                        /* var */
    (kiss_obj*)&KISS_Cunwind_protect, /* fun */
    KISS_NIL,                         /* plist */
};


kiss_symbol_t KISS_Sblock;
kiss_cfunction_t KISS_Cblock = {
    KISS_CMACRO,  /* type */
    &KISS_Sblock, /* name */
    (kiss_cf_t*)kiss_block,   /* C function name */
    1,       /* minimum argument number */
    -1,      /* maximum argument number */
};
kiss_symbol_t KISS_Sblock = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"block",
    KISS_CONSTANT_FUN,
    NULL,               /* var */
    (kiss_obj*)&KISS_Cblock, /* fun */
    KISS_NIL,                /* plist */
};


kiss_symbol_t KISS_Sreturn_from;
kiss_cfunction_t KISS_Creturn_from = {
    KISS_CMACRO,        /* type */
    &KISS_Sreturn_from, /* name */
    (kiss_cf_t*)kiss_return_from,   /* C function name */
    2,             /* minimum argument number */
    2,             /* maximum argument number */
};
kiss_symbol_t KISS_Sreturn_from = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"return-from",
    KISS_CONSTANT_FUN,
    NULL,                     /* var */
    (kiss_obj*)&KISS_Creturn_from, /* fun */
    KISS_NIL,                      /* plist */
};


kiss_symbol_t KISS_Stagbody;
kiss_cfunction_t KISS_Ctagbody = {
    KISS_CMACRO,    /* type */
    &KISS_Stagbody, /* name */
    (kiss_cf_t*)kiss_tagbody,   /* C function name */
    0,         /* minimum argument number */
    -1,        /* maximum argument number */
};
kiss_symbol_t KISS_Stagbody = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"tagbody",
    KISS_CONSTANT_FUN,
    NULL,                 /* var */
    (kiss_obj*)&KISS_Ctagbody, /* fun */
    KISS_NIL,                  /* plist */
};


kiss_symbol_t KISS_Sgo;
kiss_cfunction_t KISS_Cgo = {
    KISS_CMACRO, /* type */
    &KISS_Sgo,   /* name */
    (kiss_cf_t*)kiss_go,     /* C function name */
    1,      /* minimum argument number */
    1,      /* maximum argument number */
};
kiss_symbol_t KISS_Sgo = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"go",
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
    (kiss_cf_t*)kiss_integerp,     /* C function name */
    1,         /* minimum argument number */
    1,        /* maximum argument number */
};
kiss_symbol_t KISS_Sintegerp = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"integerp",
    KISS_CONSTANT_FUN,
    NULL,               /* var */
    (kiss_obj*)&KISS_CFintegerp, /* fun */
    KISS_NIL,                /* plist */
};

kiss_symbol_t KISS_Sfloatp;
kiss_cfunction_t KISS_CFfloatp = {
    KISS_CFUNCTION,   /* type */
    &KISS_Sfloatp,    /* name */
    (kiss_cf_t*)kiss_floatp,      /* C function name */
    1,                /* minimum argument number */
    1,                /* maximum argument number */
};
kiss_symbol_t KISS_Sfloatp = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"floatp",
    KISS_CONSTANT_FUN,
    NULL,                      /* var */
    (kiss_obj*)&KISS_CFfloatp, /* fun */
    KISS_NIL,                  /* plist */
};

kiss_symbol_t KISS_Sfloat;
kiss_cfunction_t KISS_CFfloat = {
    KISS_CFUNCTION,   /* type */
    &KISS_Sfloat,     /* name */
    (kiss_cf_t*)kiss_float,       /* C function name */
    1,                /* minimum argument number */
    1,                /* maximum argument number */
};
kiss_symbol_t KISS_Sfloat = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"float",
    KISS_CONSTANT_FUN,
    NULL,                     /* var */
    (kiss_obj*)&KISS_CFfloat, /* fun */
    KISS_NIL,                 /* plist */
};


kiss_symbol_t KISS_Splus;
kiss_cfunction_t KISS_CFplus = {
    KISS_CFUNCTION, /* type */
    &KISS_Splus,    /* name */
    (kiss_cf_t*)kiss_plus,     /* C function name */
    0,         /* minimum argument number */
    -1,        /* maximum argument number */
};
kiss_symbol_t KISS_Splus = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"+",
    KISS_CONSTANT_FUN,
    NULL,               /* var */
    (kiss_obj*)&KISS_CFplus, /* fun */
    KISS_NIL,                /* plist */
};

kiss_symbol_t KISS_Smultiply;
kiss_cfunction_t KISS_CFmultiply = {
    KISS_CFUNCTION,  /* type */
    &KISS_Smultiply, /* name */
    (kiss_cf_t*)kiss_multiply,  /* C function name */
    0,               /* minimum argument number */
    -1,              /* maximum argument number */
};
kiss_symbol_t KISS_Smultiply = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"*",
    KISS_CONSTANT_FUN,
    NULL,                        /* var */
    (kiss_obj*)&KISS_CFmultiply, /* fun */
    KISS_NIL,                    /* plist */
};


kiss_symbol_t KISS_Sminus;
kiss_cfunction_t KISS_CFminus = {
    KISS_CFUNCTION, /* type */
    &KISS_Sminus,   /* name */
    (kiss_cf_t*)kiss_minus,    /* C function name */
    1,              /* minimum argument number */
    -1,             /* maximum argument number */
};
kiss_symbol_t KISS_Sminus = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"-",
    KISS_CONSTANT_FUN,
    NULL,                     /* var */
    (kiss_obj*)&KISS_CFminus, /* fun */
    KISS_NIL,                 /* plist */
};


kiss_symbol_t KISS_Snum_eq;
kiss_cfunction_t KISS_CFnum_eq = {
    KISS_CFUNCTION, /* type */
    &KISS_Snum_eq,  /* name */
    (kiss_cf_t*)kiss_num_eq,   /* C function name */
    2,         /* minimum argument number */
    2,         /* maximum argument number */
};
kiss_symbol_t KISS_Snum_eq = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"=",
    KISS_CONSTANT_FUN,
    NULL,                 /* var */
    (kiss_obj*)&KISS_CFnum_eq, /* fun */
    KISS_NIL,                  /* plist */
};


kiss_symbol_t KISS_Snum_lessthan;
kiss_cfunction_t KISS_CFnum_lessthan = {
    KISS_CFUNCTION,       /* type */
    &KISS_Snum_lessthan,  /* name */
    (kiss_cf_t*)kiss_num_lessthan,   /* C function name */
    2,               /* minimum argument number */
    2,               /* maximum argument number */
};
kiss_symbol_t KISS_Snum_lessthan = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"<",
    KISS_CONSTANT_FUN,
    NULL,                       /* var */
    (kiss_obj*)&KISS_CFnum_lessthan, /* fun */
    KISS_NIL,                        /* plist */
};


kiss_symbol_t KISS_Sdiv;
kiss_cfunction_t KISS_CFdiv = {
    KISS_CFUNCTION,       /* type */
    &KISS_Sdiv,  /* name */
    (kiss_cf_t*)kiss_div,   /* C function name */
    2,               /* minimum argument number */
    2,               /* maximum argument number */
};
kiss_symbol_t KISS_Sdiv = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"div",
    KISS_CONSTANT_FUN,
    NULL,                   /* var */
    (kiss_obj*)&KISS_CFdiv, /* fun */
    KISS_NIL,               /* plist */
};

kiss_symbol_t KISS_Smod;
kiss_cfunction_t KISS_CFmod = {
    KISS_CFUNCTION,       /* type */
    &KISS_Smod,  /* name */
    (kiss_cf_t*)kiss_mod,   /* C function name */
    2,               /* minimum argument number */
    2,               /* maximum argument number */
};
kiss_symbol_t KISS_Smod = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"mod",
    KISS_CONSTANT_FUN,
    NULL,                   /* var */
    (kiss_obj*)&KISS_CFmod, /* fun */
    KISS_NIL,               /* plist */
};

kiss_symbol_t KISS_Sgcd;
kiss_cfunction_t KISS_CFgcd = {
    KISS_CFUNCTION,       /* type */
    &KISS_Sgcd,  /* name */
    (kiss_cf_t*)kiss_gcd,   /* C function name */
    2,               /* minimum argument number */
    2,               /* maximum argument number */
};
kiss_symbol_t KISS_Sgcd = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"gcd",
    KISS_CONSTANT_FUN,
    NULL,                   /* var */
    (kiss_obj*)&KISS_CFgcd, /* fun */
    KISS_NIL,               /* plist */
};

kiss_symbol_t KISS_Slcm;
kiss_cfunction_t KISS_CFlcm = {
    KISS_CFUNCTION,       /* type */
    &KISS_Slcm,  /* name */
    (kiss_cf_t*)kiss_lcm,   /* C function name */
    2,               /* minimum argument number */
    2,               /* maximum argument number */
};
kiss_symbol_t KISS_Slcm = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"lcm",
    KISS_CONSTANT_FUN,
    NULL,                   /* var */
    (kiss_obj*)&KISS_CFlcm, /* fun */
    KISS_NIL,               /* plist */
};

kiss_symbol_t KISS_Sparse_number;
kiss_cfunction_t KISS_CFparse_number = {
    KISS_CFUNCTION,      /* type */
    &KISS_Sparse_number, /* name */
    (kiss_cf_t*)kiss_parse_number,   /* C function name */
    1,                   /* minimum argument number */
    1,                   /* maximum argument number */
};
kiss_symbol_t KISS_Sparse_number = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"parse-number",
    KISS_CONSTANT_FUN,
    NULL,                            /* var */
    (kiss_obj*)&KISS_CFparse_number, /* fun */
    KISS_NIL,                        /* plist */
};

kiss_symbol_t KISS_Sabs;
kiss_cfunction_t KISS_CFabs = {
    KISS_CFUNCTION,   /* type */
    &KISS_Sabs,       /* name */
    (kiss_cf_t*)kiss_abs,         /* C function name */
    1,                /* minimum argument number */
    1,                /* maximum argument number */
};
kiss_symbol_t KISS_Sabs = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"abs",
    KISS_CONSTANT_FUN,
    NULL,                    /* var */
    (kiss_obj*)&KISS_CFabs,  /* fun */
    KISS_NIL,                /* plist */
};

kiss_symbol_t KISS_Sexp;
kiss_cfunction_t KISS_CFexp = {
    KISS_CFUNCTION,   /* type */
    &KISS_Sexp,       /* name */
    (kiss_cf_t*)kiss_exp,         /* C function name */
    1,                /* minimum argument number */
    1,                /* maximum argument number */
};
kiss_symbol_t KISS_Sexp = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"exp",
    KISS_CONSTANT_FUN,
    NULL,                    /* var */
    (kiss_obj*)&KISS_CFexp,  /* fun */
    KISS_NIL,                /* plist */
};

kiss_symbol_t KISS_Slog;
kiss_cfunction_t KISS_CFlog = {
    KISS_CFUNCTION,   /* type */
    &KISS_Slog,       /* name */
    (kiss_cf_t*)kiss_log,         /* C function name */
    1,                /* minimum argument number */
    1,                /* maximum argument number */
};
kiss_symbol_t KISS_Slog = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"log",
    KISS_CONSTANT_FUN,
    NULL,                    /* var */
    (kiss_obj*)&KISS_CFlog,  /* fun */
    KISS_NIL,                /* plist */
};

kiss_symbol_t KISS_Sfloor;
kiss_cfunction_t KISS_CFfloor = {
    KISS_CFUNCTION,   /* type */
    &KISS_Sfloor,     /* name */
    (kiss_cf_t*)kiss_floor,       /* C function name */
    1,                /* minimum argument number */
    1,                /* maximum argument number */
};
kiss_symbol_t KISS_Sfloor = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"floor",
    KISS_CONSTANT_FUN,
    NULL,                     /* var */
    (kiss_obj*)&KISS_CFfloor, /* fun */
    KISS_NIL,                 /* plist */
};

kiss_symbol_t KISS_Sceiling;
kiss_cfunction_t KISS_CFceiling = {
    KISS_CFUNCTION,   /* type */
    &KISS_Sceiling,   /* name */
    (kiss_cf_t*)kiss_ceiling,     /* C function name */
    1,                /* minimum argument number */
    1,                /* maximum argument number */
};
kiss_symbol_t KISS_Sceiling = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"ceiling",
    KISS_CONSTANT_FUN,
    NULL,                       /* var */
    (kiss_obj*)&KISS_CFceiling, /* fun */
    KISS_NIL,                   /* plist */
};

kiss_symbol_t KISS_Struncate;
kiss_cfunction_t KISS_CFtruncate = {
    KISS_CFUNCTION,   /* type */
    &KISS_Struncate,  /* name */
    (kiss_cf_t*)kiss_truncate,    /* C function name */
    1,                /* minimum argument number */
    1,                /* maximum argument number */
};
kiss_symbol_t KISS_Struncate = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"truncate",
    KISS_CONSTANT_FUN,
    NULL,                        /* var */
    (kiss_obj*)&KISS_CFtruncate, /* fun */
    KISS_NIL,                    /* plist */
};

kiss_symbol_t KISS_Sround;
kiss_cfunction_t KISS_CFround = {
    KISS_CFUNCTION, /* type */
    &KISS_Sround,   /* name */
    (kiss_cf_t*)kiss_round,     /* C function name */
    1,              /* minimum argument number */
    1,              /* maximum argument number */
};
kiss_symbol_t KISS_Sround = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"round",
    KISS_CONSTANT_FUN,
    NULL,                     /* var */
    (kiss_obj*)&KISS_CFround, /* fun */
    KISS_NIL,                 /* plist */
};


kiss_symbol_t KISS_Ssin;
kiss_cfunction_t KISS_CFsin = {
    KISS_CFUNCTION,   /* type */
    &KISS_Ssin,       /* name */
    (kiss_cf_t*)kiss_sin,         /* C function name */
    1,                /* minimum argument number */
    1,                /* maximum argument number */
};
kiss_symbol_t KISS_Ssin = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"sin",
    KISS_CONSTANT_FUN,
    NULL,                    /* var */
    (kiss_obj*)&KISS_CFsin,  /* fun */
    KISS_NIL,                /* plist */
};

kiss_symbol_t KISS_Scos;
kiss_cfunction_t KISS_CFcos = {
    KISS_CFUNCTION,   /* type */
    &KISS_Scos,       /* name */
    (kiss_cf_t*)kiss_cos,         /* C function name */
    1,                /* minimum argument number */
    1,                /* maximum argument number */
};
kiss_symbol_t KISS_Scos = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"cos",
    KISS_CONSTANT_FUN,
    NULL,                    /* var */
    (kiss_obj*)&KISS_CFcos,  /* fun */
    KISS_NIL,                /* plist */
};

kiss_symbol_t KISS_Stan;
kiss_cfunction_t KISS_CFtan = {
    KISS_CFUNCTION,   /* type */
    &KISS_Stan,       /* name */
    (kiss_cf_t*)kiss_tan,         /* C function name */
    1,                /* minimum argument number */
    1,                /* maximum argument number */
};
kiss_symbol_t KISS_Stan = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"tan",
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
    (kiss_cf_t*)kiss_symbolp,    /* C function name */
    1,         /* minimum argument number */
    1,         /* maximum argument number */
};
kiss_symbol_t KISS_Ssymbolp = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"symbolp",
    KISS_CONSTANT_FUN,
    NULL,                 /* var */
    (kiss_obj*)&KISS_CFsymbolp, /* fun */
    KISS_NIL,                  /* plist */
};


kiss_symbol_t KISS_Sgensym;
kiss_cfunction_t KISS_CFgensym = {
    KISS_CFUNCTION, /* type */
    &KISS_Sgensym,  /* name */
    (kiss_cf_t*)kiss_gensym,    /* C function name */
    0,         /* minimum argument number */
    0,         /* maximum argument number */
};
kiss_symbol_t KISS_Sgensym = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"gensym",
    KISS_CONSTANT_FUN,
    NULL,                 /* var */
    (kiss_obj*)&KISS_CFgensym, /* fun */
    KISS_NIL,                  /* plist */
};

kiss_symbol_t KISS_Ssymbol_function;
kiss_cfunction_t KISS_CFsymbol_function = {
    KISS_CFUNCTION, /* type */
    &KISS_Ssymbol_function,  /* name */
    (kiss_cf_t*)kiss_symbol_function,    /* C function name */
    1,         /* minimum argument number */
    1,         /* maximum argument number */
};
kiss_symbol_t KISS_Ssymbol_function = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"symbol-function",
    KISS_CONSTANT_FUN,
    NULL,                 /* var */
    (kiss_obj*)&KISS_CFsymbol_function, /* fun */
    KISS_NIL,                  /* plist */
};

kiss_symbol_t KISS_Sset_symbol_function;
kiss_cfunction_t KISS_CFset_symbol_function = {
    KISS_CFUNCTION, /* type */
    &KISS_Sset_symbol_function,  /* name */
    (kiss_cf_t*)kiss_set_symbol_function,    /* C function name */
    2,         /* minimum argument number */
    2,         /* maximum argument number */
};
kiss_symbol_t KISS_Sset_symbol_function = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"set-symbol-function",
    KISS_CONSTANT_FUN,
    NULL,                 /* var */
    (kiss_obj*)&KISS_CFset_symbol_function, /* fun */
    KISS_NIL,                  /* plist */
};

kiss_symbol_t KISS_Sfboundp;
kiss_cfunction_t KISS_CFfboundp = {
    KISS_CFUNCTION, /* type */
    &KISS_Sfboundp,  /* name */
    (kiss_cf_t*)kiss_fboundp,    /* C function name */
    1,         /* minimum argument number */
    1,         /* maximum argument number */
};
kiss_symbol_t KISS_Sfboundp = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"fboundp",
    KISS_CONSTANT_FUN,
    NULL,                 /* var */
    (kiss_obj*)&KISS_CFfboundp, /* fun */
    KISS_NIL,                  /* plist */
};

kiss_symbol_t KISS_Sfmakunbound;
kiss_cfunction_t KISS_CFfmakunbound = {
    KISS_CFUNCTION, /* type */
    &KISS_Sfmakunbound,  /* name */
    (kiss_cf_t*)kiss_fmakunbound,    /* C function name */
    1,         /* minimum argument number */
    1,         /* maximum argument number */
};
kiss_symbol_t KISS_Sfmakunbound = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"kiss::fmakunbound",
    KISS_CONSTANT_FUN,
    NULL,                 /* var */
    (kiss_obj*)&KISS_CFfmakunbound, /* fun */
    KISS_NIL,                  /* plist */
};

kiss_symbol_t KISS_Sproperty;
kiss_cfunction_t KISS_CFproperty = {
    KISS_CFUNCTION, /* type */
    &KISS_Sproperty,  /* name */
    (kiss_cf_t*)kiss_property,    /* C function name */
    2,         /* minimum argument number */
    3,         /* maximum argument number */
};
kiss_symbol_t KISS_Sproperty = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"property",
    KISS_CONSTANT_FUN,
    NULL,                 /* var */
    (kiss_obj*)&KISS_CFproperty, /* fun */
    KISS_NIL,                  /* plist */
};

kiss_symbol_t KISS_Sset_property;
kiss_cfunction_t KISS_CFset_property = {
    KISS_CFUNCTION, /* type */
    &KISS_Sset_property,  /* name */
    (kiss_cf_t*)kiss_set_property,    /* C function name */
    3,         /* minimum argument number */
    3,         /* maximum argument number */
};
kiss_symbol_t KISS_Sset_property = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"set-property",
    KISS_CONSTANT_FUN,
    NULL,                 /* var */
    (kiss_obj*)&KISS_CFset_property, /* fun */
    KISS_NIL,                  /* plist */
};

kiss_symbol_t KISS_Sremove_property;
kiss_cfunction_t KISS_CFremove_property = {
    KISS_CFUNCTION, /* type */
    &KISS_Sremove_property,  /* name */
    (kiss_cf_t*)kiss_remove_property,    /* C function name */
    2,         /* minimum argument number */
    2,         /* maximum argument number */
};
kiss_symbol_t KISS_Sremove_property = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"remove-property",
    KISS_CONSTANT_FUN,
    NULL,                 /* var */
    (kiss_obj*)&KISS_CFremove_property, /* fun */
    KISS_NIL,                  /* plist */
};



/*** format.c ***/
kiss_symbol_t KISS_Sformat;
kiss_cfunction_t KISS_CFformat = {
    KISS_CFUNCTION,       /* type */
    &KISS_Sformat, /* name */
    (kiss_cf_t*)kiss_format,   /* C function name */
    2,               /* minimum argument number */
    -1,               /* maximum argument number */
};
kiss_symbol_t KISS_Sformat = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"format",
    KISS_CONSTANT_FUN,
    NULL,                        /* var */
    (kiss_obj*)&KISS_CFformat, /* fun */
    KISS_NIL,                         /* plist */
};

kiss_symbol_t KISS_Sformat_object;
kiss_cfunction_t KISS_CFformat_object = {
    KISS_CFUNCTION,       /* type */
    &KISS_Sformat_object, /* name */
    (kiss_cf_t*)kiss_format_object,   /* C function name */
    3,               /* minimum argument number */
    3,               /* maximum argument number */
};
kiss_symbol_t KISS_Sformat_object = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"format-object",
    KISS_CONSTANT_FUN,
    NULL,                        /* var */
    (kiss_obj*)&KISS_CFformat_object, /* fun */
    KISS_NIL,                         /* plist */
};


kiss_symbol_t KISS_Sformat_pointer;
kiss_cfunction_t KISS_CFformat_pointer = {
    KISS_CFUNCTION,       /* type */
    &KISS_Sformat_pointer, /* name */
    (kiss_cf_t*)kiss_format_pointer,   /* C function name */
    2,               /* minimum argument number */
    2,               /* maximum argument number */
};
kiss_symbol_t KISS_Sformat_pointer = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"kiss::format-pointer",
    KISS_CONSTANT_FUN,
    NULL,                        /* var */
    (kiss_obj*)&KISS_CFformat_pointer, /* fun */
    KISS_NIL,                         /* plist */
};

kiss_symbol_t KISS_Sformat_fresh_line;
kiss_cfunction_t KISS_CFformat_fresh_line = {
    KISS_CFUNCTION,       /* type */
    &KISS_Sformat_fresh_line, /* name */
    (kiss_cf_t*)kiss_format_fresh_line,   /* C function name */
    1,               /* minimum argument number */
    1,               /* maximum argument number */
};
kiss_symbol_t KISS_Sformat_fresh_line = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"format-fresh-line",
    KISS_CONSTANT_FUN,
    NULL,                        /* var */
    (kiss_obj*)&KISS_CFformat_fresh_line, /* fun */
    KISS_NIL,                         /* plist */
};


kiss_symbol_t KISS_Sprint;
kiss_cfunction_t KISS_CFprint = {
    KISS_CFUNCTION, /* type */
    &KISS_Sprint,   /* name */
    (kiss_cf_t*)kiss_print,     /* C function name */
    1,         /* minimum argument number */
    1,         /* maximum argument number */
};
kiss_symbol_t KISS_Sprint = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"print",
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
    (kiss_cf_t*)kiss_read,     /* C function name */
    0,         /* minimum argument number */
    3,         /* maximum argument number */
};
kiss_symbol_t KISS_Sread = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"read",
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
    (kiss_cf_t*)kiss_stringp,      /* C function name */
    1,                 /* minimum argument number */
    1,                 /* maximum argument number */
};
kiss_symbol_t KISS_Sstringp = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"stringp",
    KISS_CONSTANT_FUN,
    NULL,               /* var */
    (kiss_obj*)&KISS_CFstringp, /* fun */
    KISS_NIL,                /* plist */
};

kiss_symbol_t KISS_Screate_string;
kiss_cfunction_t KISS_CFcreate_string = {
    KISS_CFUNCTION,       /* type */
    &KISS_Screate_string, /* name */
    (kiss_cf_t*)kiss_create_string,   /* C function name */
    1,                    /* minimum argument number */
    2,                    /* maximum argument number */
};
kiss_symbol_t KISS_Screate_string = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"create-string",
    KISS_CONSTANT_FUN,
    NULL,                             /* var */
    (kiss_obj*)&KISS_CFcreate_string, /* fun */
    KISS_NIL,                         /* plist */
};

kiss_symbol_t KISS_Sstring_eq;
kiss_cfunction_t KISS_CFstring_eq = {
    KISS_CFUNCTION,       /* type */
    &KISS_Sstring_eq, /* name */
    (kiss_cf_t*)kiss_string_eq,   /* C function name */
    2,                    /* minimum argument number */
    2,                    /* maximum argument number */
};
kiss_symbol_t KISS_Sstring_eq = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"string=",
    KISS_CONSTANT_FUN,
    NULL,                             /* var */
    (kiss_obj*)&KISS_CFstring_eq, /* fun */
    KISS_NIL,                         /* plist */
};

kiss_symbol_t KISS_Sstring_neq;
kiss_cfunction_t KISS_CFstring_neq = {
    KISS_CFUNCTION,       /* type */
    &KISS_Sstring_neq, /* name */
    (kiss_cf_t*)kiss_string_neq,   /* C function name */
    2,                    /* minimum argument number */
    2,                    /* maximum argument number */
};
kiss_symbol_t KISS_Sstring_neq = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"string/=",
    KISS_CONSTANT_FUN,
    NULL,                             /* var */
    (kiss_obj*)&KISS_CFstring_neq, /* fun */
    KISS_NIL,                         /* plist */
};


kiss_symbol_t KISS_Sstring_append;
kiss_cfunction_t KISS_CFstring_append = {
    KISS_CFUNCTION,       /* type */
    &KISS_Sstring_append, /* name */
    (kiss_cf_t*)kiss_string_append,   /* C function name */
    0,                    /* minimum argument number */
    -1,                   /* maximum argument number */
};
kiss_symbol_t KISS_Sstring_append = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"string-append",
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
    (kiss_cf_t*)kiss_length,     /* C function name */
    1,         /* minimum argument number */
    1,         /* maximum argument number */
};
kiss_symbol_t KISS_Slength = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"length",
    KISS_CONSTANT_FUN,
    NULL,               /* var */
    (kiss_obj*)&KISS_CFlength, /* fun */
    KISS_NIL,                /* plist */
};

kiss_symbol_t KISS_Selt;
kiss_cfunction_t KISS_CFelt = {
    KISS_CFUNCTION, /* type */
    &KISS_Selt,    /* name */
    (kiss_cf_t*)kiss_elt,     /* C function name */
    2,         /* minimum argument number */
    2,         /* maximum argument number */
};
kiss_symbol_t KISS_Selt = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"elt",
    KISS_CONSTANT_FUN,
    NULL,               /* var */
    (kiss_obj*)&KISS_CFelt, /* fun */
    KISS_NIL,                /* plist */
};

kiss_symbol_t KISS_Sset_elt;
kiss_cfunction_t KISS_CFset_elt = {
    KISS_CFUNCTION, /* type */
    &KISS_Sset_elt,    /* name */
    (kiss_cf_t*)kiss_set_elt,     /* C function name */
    3,         /* minimum argument number */
    3,         /* maximum argument number */
};
kiss_symbol_t KISS_Sset_elt = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"set-elt",
    KISS_CONSTANT_FUN,
    NULL,               /* var */
    (kiss_obj*)&KISS_CFset_elt, /* fun */
    KISS_NIL,                /* plist */
};

kiss_symbol_t KISS_Ssubseq;
kiss_cfunction_t KISS_CFsubseq = {
    KISS_CFUNCTION, /* type */
    &KISS_Ssubseq,    /* name */
    (kiss_cf_t*)kiss_subseq,     /* C function name */
    3,         /* minimum argument number */
    3,         /* maximum argument number */
};
kiss_symbol_t KISS_Ssubseq = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"subseq",
    KISS_CONSTANT_FUN,
    NULL,               /* var */
    (kiss_obj*)&KISS_CFsubseq, /* fun */
    KISS_NIL,                /* plist */
};

kiss_symbol_t KISS_Smap_into;
kiss_cfunction_t KISS_CFmap_into = {
    KISS_CFUNCTION, /* type */
    &KISS_Smap_into,    /* name */
    (kiss_cf_t*)kiss_map_into,     /* C function name */
    2,         /* minimum argument number */
    -1,         /* maximum argument number */
};
kiss_symbol_t KISS_Smap_into = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"map-into",
    KISS_CONSTANT_FUN,
    NULL,               /* var */
    (kiss_obj*)&KISS_CFmap_into, /* fun */
    KISS_NIL,                /* plist */
};

/*** eval.c ***/
kiss_symbol_t KISS_Seval;
kiss_cfunction_t KISS_CFeval = {
    KISS_CFUNCTION,        /* type */
    &KISS_Seval, /* name */
    (kiss_cf_t*)kiss_eval,   /* C function name */
    1,                /* minimum argument number */
    1,                /* maximum argument number */
};
kiss_symbol_t KISS_Seval = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"eval",
    KISS_CONSTANT_FUN,
    NULL,                         /* var */
    (kiss_obj*)&KISS_CFeval, /* fun */
    KISS_NIL,                          /* plist */
};

/*** load.c ***/
kiss_symbol_t KISS_Sload;
kiss_cfunction_t KISS_CFload = {
    KISS_CFUNCTION,        /* type */
    &KISS_Sload, /* name */
    (kiss_cf_t*)kiss_load,   /* C function name */
    1,                /* minimum argument number */
    1,                /* maximum argument number */
};
kiss_symbol_t KISS_Sload = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"load",
    KISS_CONSTANT_FUN,
    NULL,                         /* var */
    (kiss_obj*)&KISS_CFload, /* fun */
    KISS_NIL,                          /* plist */
};


/*** stream.c ***/
kiss_symbol_t KISS_Sstandard_input;
kiss_cfunction_t KISS_CFstandard_input = {
    KISS_CFUNCTION,        /* type */
    &KISS_Sstandard_input, /* name */
    (kiss_cf_t*)kiss_standard_input,   /* C function name */
    0,                /* minimum argument number */
    0,                /* maximum argument number */
};
kiss_symbol_t KISS_Sstandard_input = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"standard-input",
    KISS_CONSTANT_FUN,
    NULL,                         /* var */
    (kiss_obj*)&KISS_CFstandard_input, /* fun */
    KISS_NIL,                          /* plist */
};


kiss_symbol_t KISS_Sstandard_output;
kiss_cfunction_t KISS_CFstandard_output = {
    KISS_CFUNCTION,         /* type */
    &KISS_Sstandard_output, /* name */
    (kiss_cf_t*)kiss_standard_output,   /* C function name */
    0,                 /* minimum argument number */
    0,                 /* maximum argument number */
};
kiss_symbol_t KISS_Sstandard_output = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"standard-output",
    KISS_CONSTANT_FUN,
    NULL,                          /* var */
    (kiss_obj*)&KISS_CFstandard_output, /* fun */
    KISS_NIL,                           /* plist */
};


kiss_symbol_t KISS_Serror_output;
kiss_cfunction_t KISS_CFerror_output = {
    KISS_CFUNCTION,      /* type */
    &KISS_Serror_output, /* name */
    (kiss_cf_t*)kiss_error_output,   /* C function name */
    0,              /* minimum argument number */
    0,              /* maximum argument number */
};
kiss_symbol_t KISS_Serror_output = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"error-output",
    KISS_CONSTANT_FUN,
    NULL,                       /* var */
    (kiss_obj*)&KISS_CFerror_output, /* fun */
    KISS_NIL,                        /* plist */
};

extern kiss_file_stream_t Kiss_Standard_Input;
extern kiss_file_stream_t Kiss_Standard_Output;
extern kiss_file_stream_t Kiss_Error_Output;

kiss_symbol_t KISS_Ss_standard_input_s = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"*kiss::standard-input*",
    KISS_CONSTANT_VAR,
    (kiss_obj*)(&Kiss_Standard_Input), /* var */
    NULL,                              /* fun */
    KISS_NIL,                          /* plist */
};

kiss_symbol_t KISS_Ss_standard_output_s = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"*kiss::standard-output*",
    KISS_CONSTANT_VAR,
    (kiss_obj*)(&Kiss_Standard_Output), /* var */
    NULL,                               /* fun */
    KISS_NIL,                           /* plist */
};

kiss_symbol_t KISS_Ss_error_output_s = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"*kiss::error-output*",
    KISS_CONSTANT_VAR,
    (kiss_obj*)(&Kiss_Error_Output), /* var */
    NULL,                            /* fun */
    KISS_NIL,                        /* plist */
};

kiss_symbol_t KISS_Sstreamp;
kiss_cfunction_t KISS_CFstreamp = {
    KISS_CFUNCTION,      /* type */
    &KISS_Sstreamp, /* name */
    (kiss_cf_t*)kiss_streamp,   /* C function name */
    1,              /* minimum argument number */
    1,              /* maximum argument number */
};
kiss_symbol_t KISS_Sstreamp = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"streamp",
    KISS_CONSTANT_FUN,
    NULL,                       /* var */
    (kiss_obj*)&KISS_CFstreamp, /* fun */
    KISS_NIL,                        /* plist */
};

kiss_symbol_t KISS_Sopen_stream_p;
kiss_cfunction_t KISS_CFopen_stream_p = {
    KISS_CFUNCTION,      /* type */
    &KISS_Sopen_stream_p, /* name */
    (kiss_cf_t*)kiss_open_stream_p,   /* C function name */
    1,              /* minimum argument number */
    1,              /* maximum argument number */
};
kiss_symbol_t KISS_Sopen_stream_p = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"open-stream-p",
    KISS_CONSTANT_FUN,
    NULL,                       /* var */
    (kiss_obj*)&KISS_CFopen_stream_p, /* fun */
    KISS_NIL,                        /* plist */
};


kiss_symbol_t KISS_Sinput_stream_p;
kiss_cfunction_t KISS_CFinput_stream_p = {
    KISS_CFUNCTION,      /* type */
    &KISS_Sinput_stream_p, /* name */
    (kiss_cf_t*)kiss_input_stream_p,   /* C function name */
    1,              /* minimum argument number */
    1,              /* maximum argument number */
};
kiss_symbol_t KISS_Sinput_stream_p = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"input-stream-p",
    KISS_CONSTANT_FUN,
    NULL,                       /* var */
    (kiss_obj*)&KISS_CFinput_stream_p, /* fun */
    KISS_NIL,                        /* plist */
};

kiss_symbol_t KISS_Soutput_stream_p;
kiss_cfunction_t KISS_CFoutput_stream_p = {
    KISS_CFUNCTION,      /* type */
    &KISS_Soutput_stream_p, /* name */
    (kiss_cf_t*)kiss_output_stream_p,   /* C function name */
    1,              /* minimum argument number */
    1,              /* maximum argument number */
};
kiss_symbol_t KISS_Soutput_stream_p = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"output-stream-p",
    KISS_CONSTANT_FUN,
    NULL,                       /* var */
    (kiss_obj*)&KISS_CFoutput_stream_p, /* fun */
    KISS_NIL,                        /* plist */
};

kiss_symbol_t KISS_Sstream_ready_p;
kiss_cfunction_t KISS_CFstream_ready_p = {
    KISS_CFUNCTION,      /* type */
    &KISS_Sstream_ready_p, /* name */
    (kiss_cf_t*)kiss_stream_ready_p,   /* C function name */
    1,              /* minimum argument number */
    1,              /* maximum argument number */
};
kiss_symbol_t KISS_Sstream_ready_p = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"stream-ready-p",
    KISS_CONSTANT_FUN,
    NULL,                       /* var */
    (kiss_obj*)&KISS_CFstream_ready_p, /* fun */
    KISS_NIL,                        /* plist */
};


kiss_symbol_t KISS_Screate_string_input_stream;
kiss_cfunction_t KISS_CFcreate_string_input_stream = {
    KISS_CFUNCTION,      /* type */
    &KISS_Screate_string_input_stream, /* name */
    (kiss_cf_t*)kiss_create_string_input_stream,   /* C function name */
    1,              /* minimum argument number */
    1,              /* maximum argument number */
};
kiss_symbol_t KISS_Screate_string_input_stream = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"create-string-input-stream",
    KISS_CONSTANT_FUN,
    NULL,                       /* var */
    (kiss_obj*)&KISS_CFcreate_string_input_stream, /* fun */
    KISS_NIL,                        /* plist */
};


kiss_symbol_t KISS_Screate_string_output_stream;
kiss_cfunction_t KISS_CFcreate_string_output_stream = {
    KISS_CFUNCTION,      /* type */
    &KISS_Screate_string_output_stream, /* name */
    (kiss_cf_t*)kiss_create_string_output_stream,   /* C function name */
    0,              /* minimum argument number */
    0,              /* maximum argument number */
};
kiss_symbol_t KISS_Screate_string_output_stream = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"create-string-output-stream",
    KISS_CONSTANT_FUN,
    NULL,                       /* var */
    (kiss_obj*)&KISS_CFcreate_string_output_stream, /* fun */
    KISS_NIL,                        /* plist */
};


kiss_symbol_t KISS_Sread_char;
kiss_cfunction_t KISS_CFread_char = {
    KISS_CFUNCTION,      /* type */
    &KISS_Sread_char, /* name */
    (kiss_cf_t*)kiss_read_char,   /* C function name */
    0,              /* minimum argument number */
    3,              /* maximum argument number */
};
kiss_symbol_t KISS_Sread_char = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"read-char",
    KISS_CONSTANT_FUN,
    NULL,                       /* var */
    (kiss_obj*)&KISS_CFread_char, /* fun */
    KISS_NIL,                        /* plist */
};

kiss_symbol_t KISS_Spreview_char;
kiss_cfunction_t KISS_CFpreview_char = {
    KISS_CFUNCTION,      /* type */
    &KISS_Spreview_char, /* name */
    (kiss_cf_t*)kiss_preview_char,   /* C function name */
    0,              /* minimum argument number */
    3,              /* maximum argument number */
};
kiss_symbol_t KISS_Spreview_char = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"preview-char",
    KISS_CONSTANT_FUN,
    NULL,                       /* var */
    (kiss_obj*)&KISS_CFpreview_char, /* fun */
    KISS_NIL,                        /* plist */
};

kiss_symbol_t KISS_Sread_line;
kiss_cfunction_t KISS_CFread_line = {
    KISS_CFUNCTION,      /* type */
    &KISS_Sread_line, /* name */
    (kiss_cf_t*)kiss_read_line,   /* C function name */
    0,              /* minimum argument number */
    3,              /* maximum argument number */
};
kiss_symbol_t KISS_Sread_line = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"read-line",
    KISS_CONSTANT_FUN,
    NULL,                       /* var */
    (kiss_obj*)&KISS_CFread_line, /* fun */
    KISS_NIL,                        /* plist */
};

kiss_symbol_t KISS_Sread_byte;
kiss_cfunction_t KISS_CFread_byte = {
    KISS_CFUNCTION,      /* type */
    &KISS_Sread_byte, /* name */
    (kiss_cf_t*)kiss_read_byte,   /* C function name */
    1,              /* minimum argument number */
    3,              /* maximum argument number */
};
kiss_symbol_t KISS_Sread_byte = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"read-byte",
    KISS_CONSTANT_FUN,
    NULL,                       /* var */
    (kiss_obj*)&KISS_CFread_byte, /* fun */
    KISS_NIL,                        /* plist */
};

kiss_symbol_t KISS_Swrite_byte;
kiss_cfunction_t KISS_CFwrite_byte = {
    KISS_CFUNCTION,      /* type */
    &KISS_Swrite_byte, /* name */
    (kiss_cf_t*)kiss_write_byte,   /* C function name */
    2,              /* minimum argument number */
    2,              /* maximum argument number */
};
kiss_symbol_t KISS_Swrite_byte = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"write-byte",
    KISS_CONSTANT_FUN,
    NULL,                       /* var */
    (kiss_obj*)&KISS_CFwrite_byte, /* fun */
    KISS_NIL,                        /* plist */
};


kiss_symbol_t KISS_Sget_output_stream_string;
kiss_cfunction_t KISS_CFget_output_stream_string = {
    KISS_CFUNCTION,      /* type */
    &KISS_Sget_output_stream_string, /* name */
    (kiss_cf_t*)kiss_get_output_stream_string,   /* C function name */
    1,              /* minimum argument number */
    1,              /* maximum argument number */
};
kiss_symbol_t KISS_Sget_output_stream_string = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"get-output-stream-string",
    KISS_CONSTANT_FUN,
    NULL,                       /* var */
    (kiss_obj*)&KISS_CFget_output_stream_string, /* fun */
    KISS_NIL,                        /* plist */
};


kiss_symbol_t KISS_Sformat_char;
kiss_cfunction_t KISS_CFformat_char = {
    KISS_CFUNCTION,      /* type */
    &KISS_Sformat_char, /* name */
    (kiss_cf_t*)kiss_format_char,   /* C function name */
    2,              /* minimum argument number */
    2,              /* maximum argument number */
};
kiss_symbol_t KISS_Sformat_char = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"format-char",
    KISS_CONSTANT_FUN,
    NULL,                       /* var */
    (kiss_obj*)&KISS_CFformat_char, /* fun */
    KISS_NIL,                        /* plist */
};


kiss_symbol_t KISS_Sformat_integer;
kiss_cfunction_t KISS_CFformat_integer = {
    KISS_CFUNCTION,      /* type */
    &KISS_Sformat_integer, /* name */
    (kiss_cf_t*)kiss_format_integer,   /* C function name */
    3,              /* minimum argument number */
    3,              /* maximum argument number */
};
kiss_symbol_t KISS_Sformat_integer = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"format-integer",
    KISS_CONSTANT_FUN,
    NULL,                       /* var */
    (kiss_obj*)&KISS_CFformat_integer, /* fun */
    KISS_NIL,                        /* plist */
};


kiss_symbol_t KISS_Sformat_float;
kiss_cfunction_t KISS_CFformat_float = {
    KISS_CFUNCTION,      /* type */
    &KISS_Sformat_float, /* name */
    (kiss_cf_t*)kiss_format_float,   /* C function name */
    2,              /* minimum argument number */
    2,              /* maximum argument number */
};
kiss_symbol_t KISS_Sformat_float = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"format-float",
    KISS_CONSTANT_FUN,
    NULL,                       /* var */
    (kiss_obj*)&KISS_CFformat_float, /* fun */
    KISS_NIL,                        /* plist */
};


kiss_symbol_t KISS_Sopen_input_file;
kiss_cfunction_t KISS_CFopen_input_file = {
    KISS_CFUNCTION,      /* type */
    &KISS_Sopen_input_file, /* name */
    (kiss_cf_t*)kiss_open_input_file,   /* C function name */
    1,              /* minimum argument number */
    2,              /* maximum argument number */
};
kiss_symbol_t KISS_Sopen_input_file = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"open-input-file",
    KISS_CONSTANT_FUN,
    NULL,                       /* var */
    (kiss_obj*)&KISS_CFopen_input_file, /* fun */
    KISS_NIL,                        /* plist */
};

kiss_symbol_t KISS_Sopen_output_file;
kiss_cfunction_t KISS_CFopen_output_file = {
    KISS_CFUNCTION,      /* type */
    &KISS_Sopen_output_file, /* name */
    (kiss_cf_t*)kiss_open_output_file,   /* C function name */
    1,              /* minimum argument number */
    2,              /* maximum argument number */
};
kiss_symbol_t KISS_Sopen_output_file = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"open-output-file",
    KISS_CONSTANT_FUN,
    NULL,                       /* var */
    (kiss_obj*)&KISS_CFopen_output_file, /* fun */
    KISS_NIL,                        /* plist */
};

kiss_symbol_t KISS_Sopen_io_file;
kiss_cfunction_t KISS_CFopen_io_file = {
    KISS_CFUNCTION,      /* type */
    &KISS_Sopen_io_file, /* name */
    (kiss_cf_t*)kiss_open_io_file,   /* C function name */
    1,              /* minimum argument number */
    2,              /* maximum argument number */
};
kiss_symbol_t KISS_Sopen_io_file = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"open-io-file",
    KISS_CONSTANT_FUN,
    NULL,                       /* var */
    (kiss_obj*)&KISS_CFopen_io_file, /* fun */
    KISS_NIL,                        /* plist */
};

kiss_symbol_t KISS_Sfinish_output;
kiss_cfunction_t KISS_CFfinish_output = {
    KISS_CFUNCTION,      /* type */
    &KISS_Sfinish_output, /* name */
    (kiss_cf_t*)kiss_finish_output,   /* C function name */
    1,              /* minimum argument number */
    1,              /* maximum argument number */
};
kiss_symbol_t KISS_Sfinish_output = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"finish-output",
    KISS_CONSTANT_FUN,
    NULL,                       /* var */
    (kiss_obj*)&KISS_CFfinish_output, /* fun */
    KISS_NIL,                        /* plist */
};


kiss_symbol_t KISS_Sclose;
kiss_cfunction_t KISS_CFclose = {
    KISS_CFUNCTION,      /* type */
    &KISS_Sclose, /* name */
    (kiss_cf_t*)kiss_close,   /* C function name */
    1,              /* minimum argument number */
    1,              /* maximum argument number */
};
kiss_symbol_t KISS_Sclose = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"close",
    KISS_CONSTANT_FUN,
    NULL,                       /* var */
    (kiss_obj*)&KISS_CFclose, /* fun */
    KISS_NIL,                        /* plist */
};


/*** error.c ***/
kiss_symbol_t KISS_Sassure_list;
kiss_cfunction_t KISS_CFassure_list = {
    KISS_CFUNCTION, /* type */
    &KISS_Sassure_list,   /* name */
    (kiss_cf_t*)Kiss_List,    /* C function name */
    1,         /* minimum argument number */
    1,        /* maximum argument number */
};
kiss_symbol_t KISS_Sassure_list = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"kiss::assure-list",
    KISS_CONSTANT_FUN,
    NULL,                /* var */
    (kiss_obj*)&KISS_CFassure_list, /* fun */
    KISS_NIL,                 /* plist */
};


/*** object.c ***/


/*** character.c ***/
kiss_symbol_t KISS_Scharacterp;
kiss_cfunction_t KISS_CFcharacterp = {
    KISS_CFUNCTION, /* type */
    &KISS_Scharacterp,   /* name */
    (kiss_cf_t*)kiss_characterp,    /* C function name */
    1,         /* minimum argument number */
    1,        /* maximum argument number */
};
kiss_symbol_t KISS_Scharacterp = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"characterp",
    KISS_CONSTANT_FUN,
    NULL,                /* var */
    (kiss_obj*)&KISS_CFcharacterp, /* fun */
    KISS_NIL,                 /* plist */
};

kiss_symbol_t KISS_Schar_eq;
kiss_cfunction_t KISS_CFchar_eq = {
    KISS_CFUNCTION, /* type */
    &KISS_Schar_eq,   /* name */
    (kiss_cf_t*)kiss_char_eq,    /* C function name */
    2,         /* minimum argument number */
    2,        /* maximum argument number */
};
kiss_symbol_t KISS_Schar_eq = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"char=",
    KISS_CONSTANT_FUN,
    NULL,                /* var */
    (kiss_obj*)&KISS_CFchar_eq, /* fun */
    KISS_NIL,                 /* plist */
};


kiss_symbol_t KISS_Schar_lessthan;
kiss_cfunction_t KISS_CFchar_lessthan = {
    KISS_CFUNCTION, /* type */
    &KISS_Schar_lessthan,   /* name */
    (kiss_cf_t*)kiss_char_lessthan,    /* C function name */
    2,         /* minimum argument number */
    2,        /* maximum argument number */
};
kiss_symbol_t KISS_Schar_lessthan = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"char<",
    KISS_CONSTANT_FUN,
    NULL,                /* var */
    (kiss_obj*)&KISS_CFchar_lessthan, /* fun */
    KISS_NIL,                 /* plist */
};



/*** feature.c ***/
kiss_symbol_t KISS_Sfeaturep;
kiss_cfunction_t KISS_CFfeaturep = {
    KISS_CFUNCTION, /* type */
    &KISS_Sfeaturep,   /* name */
    (kiss_cf_t*)kiss_featurep,    /* C function name */
    1,         /* minimum argument number */
    1,        /* maximum argument number */
};
kiss_symbol_t KISS_Sfeaturep = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"featurep",
    KISS_CONSTANT_FUN,
    NULL,                /* var */
    (kiss_obj*)&KISS_CFfeaturep, /* fun */
    KISS_NIL,                 /* plist */
};


kiss_symbol_t KISS_Sprovide;
kiss_cfunction_t KISS_CFprovide = {
    KISS_CFUNCTION, /* type */
    &KISS_Sprovide,   /* name */
    (kiss_cf_t*)kiss_provide,    /* C function name */
    1,         /* minimum argument number */
    1,        /* maximum argument number */
};
kiss_symbol_t KISS_Sprovide = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"provide",
    KISS_CONSTANT_FUN,
    NULL,                /* var */
    (kiss_obj*)&KISS_CFprovide, /* fun */
    KISS_NIL,                 /* plist */
};

/*** ilos.c ***/
kiss_symbol_t KISS_Sk_classes = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"kiss::classes",
    0,
    NULL,                /* var */
    NULL,                /* fun */
    KISS_NIL,                 /* plist */
};

kiss_symbol_t KISS_Skw_class;
kiss_symbol_t KISS_Skw_class = {
    KISS_SYMBOL,
    NULL,  /* gc_ptr */
    L":class",
    0,
    (kiss_obj*)&KISS_Skw_class,  /* var */
    NULL,                        /* fun */
    KISS_NIL,                    /* plist */
};


kiss_symbol_t KISS_Smake_ilos_obj;
kiss_cfunction_t KISS_CFmake_ilos_obj = {
    KISS_CFUNCTION, /* type */
    &KISS_Smake_ilos_obj,   /* name */
    (kiss_cf_t*)kiss_make_ilos_obj,    /* C function name */
    1,         /* minimum argument number */
    1,        /* maximum argument number */
};
kiss_symbol_t KISS_Smake_ilos_obj = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"kiss::make-ilos-obj",
    KISS_CONSTANT_FUN,
    NULL,                /* var */
    (kiss_obj*)&KISS_CFmake_ilos_obj, /* fun */
    KISS_NIL,                 /* plist */
};

kiss_symbol_t KISS_Sk_class;
kiss_cfunction_t KISS_CFk_class = {
    KISS_CFUNCTION, /* type */
    &KISS_Sk_class,   /* name */
    (kiss_cf_t*)kiss_k_class,    /* C function name */
    1,         /* minimum argument number */
    1,        /* maximum argument number */
};
kiss_symbol_t KISS_Sk_class = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"kiss::class",
    KISS_CONSTANT_FUN,
    NULL,                /* var */
    (kiss_obj*)&KISS_CFk_class, /* fun */
    KISS_NIL,                 /* plist */
};

kiss_symbol_t KISS_Sobject_p;
kiss_cfunction_t KISS_CFobject_p = {
    KISS_CFUNCTION, /* type */
    &KISS_Sobject_p,   /* name */
    (kiss_cf_t*)kiss_object_p,    /* C function name */
    1,         /* minimum argument number */
    1,        /* maximum argument number */
};
kiss_symbol_t KISS_Sobject_p = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"object-p",
    KISS_CONSTANT_FUN,
    NULL,                /* var */
    (kiss_obj*)&KISS_CFobject_p, /* fun */
    KISS_NIL,                 /* plist */
};

kiss_symbol_t KISS_Silos_obj_plist;
kiss_cfunction_t KISS_CFilos_obj_plist = {
    KISS_CFUNCTION, /* type */
    &KISS_Silos_obj_plist,   /* name */
    (kiss_cf_t*)kiss_ilos_obj_plist,    /* C function name */
    1,         /* minimum argument number */
    1,        /* maximum argument number */
};
kiss_symbol_t KISS_Silos_obj_plist = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"ilos-obj-plist",
    KISS_CONSTANT_FUN,
    NULL,                /* var */
    (kiss_obj*)&KISS_CFilos_obj_plist, /* fun */
    KISS_NIL,                 /* plist */
};


kiss_symbol_t KISS_Sset_ilos_obj_plist;
kiss_cfunction_t KISS_CFset_ilos_obj_plist = {
    KISS_CFUNCTION, /* type */
    &KISS_Sset_ilos_obj_plist,   /* name */
    (kiss_cf_t*)kiss_set_ilos_obj_plist,    /* C function name */
    2,         /* minimum argument number */
    2,        /* maximum argument number */
};
kiss_symbol_t KISS_Sset_ilos_obj_plist = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"set-ilos-obj-plist",
    KISS_CONSTANT_FUN,
    NULL,                /* var */
    (kiss_obj*)&KISS_CFset_ilos_obj_plist, /* fun */
    KISS_NIL,                 /* plist */
};

kiss_symbol_t KISS_Soref;
kiss_cfunction_t KISS_CForef = {
    KISS_CFUNCTION, /* type */
    &KISS_Soref,   /* name */
    (kiss_cf_t*)kiss_oref,    /* C function name */
    2,         /* minimum argument number */
    2,        /* maximum argument number */
};
kiss_symbol_t KISS_Soref = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"kiss::oref",
    KISS_CONSTANT_FUN,
    NULL,                /* var */
    (kiss_obj*)&KISS_CForef, /* fun */
    KISS_NIL,                 /* plist */
};

kiss_symbol_t KISS_Sset_oref;
kiss_cfunction_t KISS_CFset_oref = {
    KISS_CFUNCTION, /* type */
    &KISS_Sset_oref,   /* name */
    (kiss_cf_t*)kiss_set_oref,    /* C function name */
    3,         /* minimum argument number */
    3,        /* maximum argument number */
};
kiss_symbol_t KISS_Sset_oref = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"kiss::set-oref",
    KISS_CONSTANT_FUN,
    NULL,                /* var */
    (kiss_obj*)&KISS_CFset_oref, /* fun */
    KISS_NIL,                 /* plist */
};

kiss_symbol_t KISS_Sclass_of;
kiss_cfunction_t KISS_CFclass_of = {
    KISS_CFUNCTION, /* type */
    &KISS_Sclass_of,   /* name */
    (kiss_cf_t*)kiss_class_of,    /* C function name */
    1,         /* minimum argument number */
    1,        /* maximum argument number */
};
kiss_symbol_t KISS_Sclass_of = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"class-of",
    KISS_CONSTANT_FUN,
    NULL,                /* var */
    (kiss_obj*)&KISS_CFclass_of, /* fun */
    KISS_NIL,                 /* plist */
};

/*** gf_invoke.c ***/
kiss_symbol_t KISS_Smethod_invoke;
kiss_cfunction_t KISS_CFmethod_invoke = {
    KISS_CFUNCTION, /* type */
    &KISS_Smethod_invoke,   /* name */
    (kiss_cf_t*)kiss_method_invoke,    /* C function name */
    1,         /* minimum argument number */
    1,        /* maximum argument number */
};
kiss_symbol_t KISS_Smethod_invoke = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"method-invoke",
    KISS_CONSTANT_FUN,
    NULL,                /* var */
    (kiss_obj*)&KISS_CFmethod_invoke, /* fun */
    KISS_NIL,                 /* plist */
};

/**** -------------- Predefined class names --------------------- ****/
kiss_symbol_t KISS_Sc_object = {
    KISS_SYMBOL,
    NULL,         /* gc_ptr */
    L"<object>",
    0,
    NULL,         /* var */
    NULL,         /* fun */
    KISS_NIL,     /* plist */
};
kiss_symbol_t KISS_Sc_built_in_class = {
    KISS_SYMBOL,
    NULL,         /* gc_ptr */
    L"<built-in-class>",
    0,
    NULL,         /* var */
    NULL,         /* fun */
    KISS_NIL,     /* plist */
};
kiss_symbol_t KISS_Sc_standard_class = {
    KISS_SYMBOL,
    NULL,         /* gc_ptr */
    L"<standard-class>",
    0,
    NULL,         /* var */
    NULL,         /* fun */
    KISS_NIL,     /* plist */
};
kiss_symbol_t KISS_Sc_null = {
    KISS_SYMBOL,
    NULL,         /* gc_ptr */
    L"<null>",
    0,
    NULL,         /* var */
    NULL,         /* fun */
    KISS_NIL,     /* plist */
};
kiss_symbol_t KISS_Sc_cons = {
    KISS_SYMBOL,
    NULL,         /* gc_ptr */
    L"<cons>",
    0,
    NULL,         /* var */
    NULL,         /* fun */
    KISS_NIL,     /* plist */
};
kiss_symbol_t KISS_Sc_symbol = {
    KISS_SYMBOL,
    NULL,         /* gc_ptr */
    L"<symbol>",
    0,
    NULL,         /* var */
    NULL,         /* fun */
    KISS_NIL,     /* plist */
};
kiss_symbol_t KISS_Sc_character = {
    KISS_SYMBOL,
    NULL,         /* gc_ptr */
    L"<character>",
    0,
    NULL,         /* var */
    NULL,         /* fun */
    KISS_NIL,     /* plist */
};
kiss_symbol_t KISS_Sc_integer = {
    KISS_SYMBOL,
    NULL,         /* gc_ptr */
    L"<integer>",
    0,
    NULL,         /* var */
    NULL,         /* fun */
    KISS_NIL,     /* plist */
};
kiss_symbol_t KISS_Sc_float = {
    KISS_SYMBOL,
    NULL,         /* gc_ptr */
    L"<float>",
    0,
    NULL,         /* var */
    NULL,         /* fun */
    KISS_NIL,     /* plist */
};
kiss_symbol_t KISS_Sc_string = {
    KISS_SYMBOL,
    NULL,         /* gc_ptr */
    L"<string>",
    0,
    NULL,         /* var */
    NULL,         /* fun */
    KISS_NIL,     /* plist */
};
kiss_symbol_t KISS_Sc_general_vector = {
    KISS_SYMBOL,
    NULL,         /* gc_ptr */
    L"<general-vector>",
    0,
    NULL,         /* var */
    NULL,         /* fun */
    KISS_NIL,     /* plist */
};
kiss_symbol_t KISS_Sc_general_array_s = {
    KISS_SYMBOL,
    NULL,         /* gc_ptr */
    L"<general-array*>",
    0,
    NULL,         /* var */
    NULL,         /* fun */
    KISS_NIL,     /* plist */
};
kiss_symbol_t KISS_Sc_stream = {
    KISS_SYMBOL,
    NULL,         /* gc_ptr */
    L"<stream>",
    0,
    NULL,         /* var */
    NULL,         /* fun */
    KISS_NIL,     /* plist */
};
kiss_symbol_t KISS_Sc_function = {
    KISS_SYMBOL,
    NULL,         /* gc_ptr */
    L"<function>",
    0,
    NULL,         /* var */
    NULL,         /* fun */
    KISS_NIL,     /* plist */
};
kiss_symbol_t KISS_Sc_hash_table = {
    KISS_SYMBOL,
    NULL,         /* gc_ptr */
    L"<hash-table>",
    0,
    NULL,         /* var */
    NULL,         /* fun */
    KISS_NIL,     /* plist */
};

/*** gc.c ***/
kiss_symbol_t KISS_Sgc;
kiss_cfunction_t KISS_CFgc = {
    KISS_CFUNCTION, /* type */
    &KISS_Sgc,   /* name */
    (kiss_cf_t*)kiss_gc,    /* C function name */
    0,         /* minimum argument number */
    0,        /* maximum argument number */
};
kiss_symbol_t KISS_Sgc = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"gc",
    KISS_CONSTANT_FUN,
    NULL,                /* var */
    (kiss_obj*)&KISS_CFgc, /* fun */
    KISS_NIL,                 /* plist */
};

kiss_symbol_t KISS_Sgc_info;
kiss_cfunction_t KISS_CFgc_info = {
    KISS_CFUNCTION, /* type */
    &KISS_Sgc_info,   /* name */
    (kiss_cf_t*)kiss_gc_info,    /* C function name */
    0,         /* minimum argument number */
    0,        /* maximum argument number */
};
kiss_symbol_t KISS_Sgc_info = {
    KISS_SYMBOL,
    NULL,              /* gc_ptr */
    L"gc-info",
    KISS_CONSTANT_FUN,
    NULL,                /* var */
    (kiss_obj*)&KISS_CFgc_info, /* fun */
    KISS_NIL,                 /* plist */
};


/**** symbol table ****/
kiss_symbol_t* Kiss_Symbols[KISS_SYMBOL_MAX]= {
    &KISS_Snil, &KISS_St,

    /* keywords */
    &KISS_Skw_rest, &KISS_Samp_rest,
    &KISS_Skw_size, &KISS_Skw_test, &KISS_Skw_weakness,
    &KISS_Skw_rehash_size, &KISS_Skw_rehash_threshold,
    &KISS_Skw_class,

    /* cons.c */
    &KISS_Scar, &KISS_Scdr, &KISS_Scons, &KISS_Scadr, &KISS_Scddr,
    &KISS_Scaddr, &KISS_Sconsp,
    &KISS_Sset_car, &KISS_Sset_cdr, &KISS_Screate_list,
    &KISS_Snull, &KISS_Slistp,
    &KISS_Slist, &KISS_Sappend, &KISS_Sappend_s,
    &KISS_Sreverse, &KISS_Snreverse,
    &KISS_Smember, &KISS_Smember_using,
    &KISS_Smapcar, &KISS_Smapc,
    &KISS_Sassoc, &KISS_Sassoc_using, &KISS_Slast,
    &KISS_Scopy_list,
    &KISS_Splist_member, &KISS_Splist_put, &KISS_Splist_get, 

    /* array.c */
    &KISS_Sbasic_array_p, &KISS_Sbasic_array_s_p, &KISS_Sgeneral_array_s_p,
    &KISS_Screate_array, &KISS_Sgaref, &KISS_Sset_garef, &KISS_Saref, &KISS_Sset_aref,
    &KISS_Sarray_dimensions, &KISS_Sgeneral_array_s_to_list,
    
    /* vector.c */
    &KISS_Screate_general_vector, &KISS_Svector,
    &KISS_Sgeneral_vector_p, &KISS_Sbasic_vector_p, &KISS_Sgvref, &KISS_Sset_gvref,

    /* hash_table */
    &KISS_Screate_hash_table, &KISS_Sgethash, &KISS_Sputhash,
    
    /* function.c */
    &KISS_Ssimple_function_p, &KISS_Sfunction, &KISS_Slambda,
    &KISS_Sflet, &KISS_Slabels, &KISS_Sdefun, &KISS_Sdefmacro, &KISS_Sfuncall,
    &KISS_Sapply,

    /* variable.c */
    &KISS_Ssetq, &KISS_Sdefglobal, &KISS_Sdefconstant, &KISS_Slet, &KISS_Slet_s,
    &KISS_Sdefdynamic, &KISS_Sdynamic, &KISS_Sdynamic_let, &KISS_Sset_dynamic,

    /* control.c */
    &KISS_Sunwind_protect, &KISS_Scatch, &KISS_Sthrow, &KISS_Sblock,
    &KISS_Sreturn_from, &KISS_Stagbody, &KISS_Sgo,
    &KISS_Sif, &KISS_Sprogn, &KISS_Sprog1, &KISS_Swhile,
    &KISS_Scase, &KISS_Scase_using,
    &KISS_Seq, &KISS_Seql, &KISS_Squote, &KISS_Snot, &KISS_Sand, &KISS_Sor,
    &KISS_Sequal, &KISS_Scond,
    
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
    &KISS_Sfboundp, &KISS_Sfmakunbound, &KISS_Sproperty, &KISS_Sset_property,
    &KISS_Sremove_property,

    /* string.c */
    &KISS_Sstringp, &KISS_Screate_string, &KISS_Sstring_eq, &KISS_Sstring_neq,
    &KISS_Sstring_append, 

    /* sequence.c */
    &KISS_Slength, &KISS_Selt, &KISS_Sset_elt, &KISS_Ssubseq, &KISS_Smap_into,

    /* eval.c */
    &KISS_Seval,

    /* load.c */
    &KISS_Sload,

    /* error.c */
    &KISS_Sassure_list, // kiss::assure-list is used in map functions when assure is not ready yet
    
    /* stream.c */
    &KISS_Sstandard_input, &KISS_Sstandard_output, &KISS_Serror_output,
    &KISS_Sstreamp,
    &KISS_Screate_string_input_stream, &KISS_Screate_string_output_stream,
    &KISS_Sget_output_stream_string,
    &KISS_Sinput_stream_p, &KISS_Soutput_stream_p, 
    &KISS_Sread_char, &KISS_Spreview_char, &KISS_Sformat_char, &KISS_Sformat_integer,
    &KISS_Sformat_float,
    &KISS_Sread_byte, &KISS_Swrite_byte,
    &KISS_Ss_standard_input_s, &KISS_Ss_standard_output_s, &KISS_Ss_error_output_s,
    &KISS_Sclose, &KISS_Sopen_stream_p,
    &KISS_Sopen_input_file, &KISS_Sopen_output_file, &KISS_Sopen_io_file,
    &KISS_Sfinish_output, &KISS_Sread_line, &KISS_Sstream_ready_p,
    

    /* format.c */
    &KISS_Sformat, &KISS_Sformat_object, &KISS_Sformat_pointer, &KISS_Sprint,
    &KISS_Sformat_fresh_line,

    /* read.c */
    &KISS_Sread, 

    /* ilos.c */
    &KISS_Sk_classes, &KISS_Sk_class,
    &KISS_Soref, &KISS_Sset_oref,
    &KISS_Sclass_of,
    &KISS_Smake_ilos_obj, &KISS_Sobject_p, &KISS_Silos_obj_plist, &KISS_Sset_ilos_obj_plist,
    
    /* predefined class names */
    &KISS_Sc_object, &KISS_Sc_built_in_class, &KISS_Sc_standard_class,
    &KISS_Sc_null, &KISS_Sc_cons, &KISS_Sc_symbol, &KISS_Sc_character,
    &KISS_Sc_integer, &KISS_Sc_float, &KISS_Sc_string, &KISS_Sc_general_vector,
    &KISS_Sc_general_array_s, &KISS_Sc_stream, &KISS_Sc_function,

    /* gf_invoke.c */
    &KISS_Smethod_invoke,

    /* feature.c */
    &KISS_Sfeaturep, &KISS_Sprovide,

    /* character.c */
    &KISS_Scharacterp, &KISS_Schar_eq, &KISS_Schar_lessthan,

    /* gc.c */
    &KISS_Sgc, &KISS_Sgc_info,

    NULL,
};

/* Uninterned symbols used in lisp reader */
kiss_symbol_t KISS_Udot = {
    KISS_SYMBOL, /* type */
    NULL,              /* gc_ptr */
    L".",   /* name */
    0,      /* flags */
    NULL,   /* fun */
    NULL,   /* var */
    KISS_NIL,    /* plist */
};

kiss_symbol_t KISS_Urparen = {
    KISS_SYMBOL, /* type */
    NULL,              /* gc_ptr */
    L")",   /* name */
    0,      /* flags */
    NULL,   /* fun */
    NULL,   /* var */
    KISS_NIL,    /* plist */
};

kiss_symbol_t KISS_Ucomma = {
    KISS_SYMBOL, /* type */
    NULL,              /* gc_ptr */
    L",",   /* name */
    0,      /* flags */
    NULL,   /* fun */
    NULL,   /* var */
    KISS_NIL,    /* plist */
};

kiss_symbol_t KISS_Ucomma_at = {
    KISS_SYMBOL, /* type */
    NULL,              /* gc_ptr */
    L",@",  /* name */
    0,      /* flags */
    NULL,   /* fun */
    NULL,   /* var */
    KISS_NIL,    /* plist */
};

kiss_symbol_t KISS_Ueos = {
    KISS_SYMBOL, /* type */
    NULL,              /* gc_ptr */
    L"eos", /* name */
    0,      /* flags */
    NULL,   /* fun */
    NULL,   /* var */
    KISS_NIL,    /* plist */
};

/* Uninterned symbols used in Kiss_Symbol_Hash_Table */
kiss_symbol_t KISS_Udummy = {
    KISS_SYMBOL, /* type */
    NULL,              /* gc_ptr */
    L"dummy", /* name */
    0,      /* flags */
    NULL,   /* fun */
    NULL,   /* var */
    KISS_NIL,    /* plist */
};
