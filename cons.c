/*  -*- coding: utf-8 -*-
  cons.c --- defines the mechanism handling cons of ISLisp processor KISS.

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

inline kiss_cons_t* kiss_init_cons(kiss_cons_t* const p, const kiss_obj* const left, const kiss_obj* const right)
{
    p->type = KISS_CONS;
    p->car = (kiss_obj*)left;
    p->cdr = (kiss_obj*)right;
    return p;
}

inline void kiss_push(const kiss_obj* const elm, kiss_obj** const list) {
    *list = kiss_cons(elm, *list);
}

/* function: (cons obj1 obj2) -> <cons>
   Builds a cons from two objects, with OBJ1 as its car (or `left') part and
   with OBJ2 as its cdr (or `right') part.
   An error shall be signaled if the requested cons cannot be allocated 
   (error-id. cannot-create-cons). 
   Both OBJ1 and OBJ2 may be any ISLISP object. */
inline kiss_obj* kiss_cons(const kiss_obj* const car, const kiss_obj* const cdr) {
     return (kiss_obj*)kiss_init_cons(Kiss_GC_Malloc(sizeof(kiss_cons_t)), car, cdr);
}

/* function: (consp obj) -> boolean
   Returns t if OBJ is a cons (instance of class <cons>); otherwise, returns nil.
   OBJ may be any ISLISP object. */
inline kiss_obj* kiss_consp(const kiss_obj* const obj) {
     return KISS_IS_CONS(obj) ? KISS_T : KISS_NIL;
}

/* function: (car cons) -> <object>
   Returns the left component of the CONS.
   An error shall be signaled if CONS is not a cons (error-id. domain-error). */
inline kiss_obj* kiss_car(const kiss_obj* const p) { return KISS_CAR(Kiss_Cons(p)); }

/* function: (cdr cons) -> <object>
   Returns the right component of the CONS.
   An error shall be signaled if CONS is not a cons (error-id. domain-error). */
inline kiss_obj* kiss_cdr(const kiss_obj* const p) { return KISS_CDR(Kiss_Cons(p)); }
inline kiss_obj* kiss_cadr(const kiss_obj* const p)  { return kiss_car(kiss_cdr(p)); }
inline kiss_obj* kiss_cddr(const kiss_obj* const p)  { return kiss_cdr(kiss_cdr(p)); }
inline kiss_obj* kiss_caddr(const kiss_obj* const p) { return kiss_car(kiss_cddr(p)); }

/* function: (set-car obj cons) -> <object>
   Updates the left component of CONS with OBJ.
   The returned value is OBJ.
   An error shall be signaled if CONS is not a cons (error-id. domain-error).
   OBJ may be any ISLISP object */
inline kiss_obj* kiss_set_car(const kiss_obj* const obj, kiss_obj* const cons) {
    kiss_cons_t* const p = Kiss_Cons(cons);
    p->car = (kiss_obj*)obj;
    return (kiss_obj*)obj;
}

/* function: (set-cdr obj cons) -> <object>
   Updates the right component of CONS with OBJ. The returned value is OBJ.
   An error shall be signaled if CONS is not a cons (error-id. domain-error).
   OBJ may be any ISLISP object. */
inline kiss_obj* kiss_set_cdr(const kiss_obj* const obj, kiss_obj* const cons) {
    kiss_cons_t* const p = Kiss_Cons(cons);
    p->cdr = (kiss_obj*)obj;
    return (kiss_obj*)obj;
}

/* function: (create-list i [initial-element]) -> <list>
   Returns a list of length I. If INITIAL-ELEMENT is given,
   the elements of the new list are initialized with this object;
   otherwise, the initialization is implementation defined.
   An error shall be signaled if the requested list cannot be allocated
   (error-id. cannot-create-list).
   An error shall be signaled if I is not a non-negative integer (error-id. domain-error).
   INITIAL-ELEMENT may be any ISLISP object. */
kiss_obj* kiss_create_list(const kiss_obj* const i, const kiss_obj* const rest) {
    long int n = Kiss_Non_Negative_Integer(i);
    kiss_obj* init = rest == KISS_NIL ? KISS_NIL : KISS_CAR(rest);
    kiss_obj* p = KISS_NIL;
    for (; n > 0; n--) {
         kiss_push(init, &p);
    }
    return p;
}


/* CL function: (copy-list list) -> copy
   Returns a copy of LIST. If LIST is a dotted list, the resulting list will
   also be a dotted list. Only the list structure of LIST is copied;
   the elements of the resulting list are the same as the corresponding
   elements of the given LIST. */
kiss_obj* kiss_copy_list(const kiss_obj* p) {
     kiss_cons_t head;
     kiss_init_cons(&head, KISS_NIL, KISS_NIL);
     kiss_obj* const result = (kiss_obj*)&head;
     kiss_obj* here = result;
     for (p = Kiss_List((kiss_obj*)p); KISS_IS_CONS(p); p = KISS_CDR(p)) {
          kiss_set_cdr(kiss_cons(KISS_CAR(p), KISS_NIL), here);
          here= KISS_CDR(here);
     }
     kiss_set_cdr(p, here);
     return KISS_CDR(result);
}

/* function: (list obj*)-> <list>
   Returns a new list whose length is the number of arguments and whose
   elements are the arguments in the same order as in the list-form.
   An error shall be signaled if the requested list cannot be allocated
   (error-id. cannot-create-list). Each OBJ may be any ISLisp object. */
inline kiss_obj* kiss_list(kiss_obj* const p) { return kiss_copy_list(p); }

kiss_obj* kiss_c_list(int nargs, ...) {
     kiss_cons_t head;
     kiss_init_cons(&head, KISS_NIL, KISS_NIL);
     kiss_obj* const result = (kiss_obj*)&head;
     kiss_obj* here = result;
     va_list args;
     va_start(args, nargs);
     while (nargs-- > 0) {
          kiss_set_cdr(kiss_cons(va_arg(args, kiss_obj*), KISS_NIL), here);
          here= KISS_CDR(here);
     }
     va_end(args);
     return KISS_CDR(result);
}

/* kiss_c_mapcar(function, list) -> new_list */
kiss_obj* kiss_c_mapcar(const kiss_cf1_t f, kiss_obj* list) {
     kiss_cons_t head;
     kiss_init_cons(&head, KISS_NIL, KISS_NIL);
     kiss_obj* const result = (kiss_obj*)&head;
     kiss_obj* here = result;
     for (list = Kiss_List(list); KISS_IS_CONS(list); list = KISS_CDR(list)) {
          kiss_set_cdr(kiss_cons(f(KISS_CAR(list)), KISS_NIL), here);
          here= KISS_CDR(here);
     }
     return KISS_CDR(result);
}

/* kiss_c_mapc(function, list) -> list */
kiss_obj* kiss_c_mapc(const kiss_cf1_t f, kiss_obj* const list) {
    for (kiss_obj* p = Kiss_List(list); KISS_IS_CONS(p); p = KISS_CDR(p)) {
        f(KISS_CAR(p));
    }
    return list;
}

/* function: (append list*) -> <list>
   Returns the result of appending all of the LISTS, or () if given no lists.
   An error shall be signaled if any LIST is not a list (error-id. domain-error).
   This function does not modify its arguments.
   It is implementation defined whether and when the result shares structure with its
   LIST arguments.
   An error shall be signaled if the list cannot be allocated (error-id. cannot-create-list). */
kiss_obj* kiss_append(kiss_obj* const p) {
     kiss_c_mapc((kiss_cf1_t)Kiss_List, p);
     return kiss_append_s(p);
}

/* kiss function: (append* [list* last]) -> <list>
   LAST doesn't have to be a list. This behaviour is the same as the Common Lisp's append */
inline kiss_obj* kiss_append_s(kiss_obj* p) {
     if (p == KISS_NIL) { return KISS_NIL; } /* (append*) -> nil       */
     else if (!KISS_IS_CONS(KISS_CDR(p))) {  /* (append* last) -> last */
          return KISS_CAR(p);
     } else {                                /* (append* '(1 2) '(3 4) 'a) => (1 2 3 4 . a) */
          kiss_cons_t head;
          kiss_init_cons(&head, KISS_NIL, KISS_NIL);
          kiss_cons_t* tail = &head;
          kiss_obj *list1, *list2, *rest;
          do {
               list1 = Kiss_List(KISS_CAR(p));
               list2 = KISS_CADR(p);
               rest  = KISS_CDDR(p);
               while (KISS_IS_CONS(list1)) {
                    tail->cdr = kiss_cons(KISS_CAR(list1), KISS_NIL);
                    tail = (kiss_cons_t*)tail->cdr;
                    list1 = KISS_CDR(list1);
               }
               tail->cdr = list2;
               p = KISS_CDR(p);
          } while (KISS_IS_CONS(rest));
          return head.cdr;
     }
}

kiss_obj* kiss_c_append(int nargs, ...) {
    va_list args;
    kiss_obj* stack = KISS_NIL;
    va_start(args, nargs);  
    while (nargs-- > 0) { kiss_push(va_arg(args, kiss_obj*), &stack); }
    va_end(args);
    return kiss_append(kiss_nreverse(stack));
}

/* function: (reverse list) -> <list>
   Return a list whose elements are those of the given LIST, but in reverse
   order. An error shall be signaled if LIST is not a list (error-id. domain-error).
   no side-effect to the given LIST occurs. The resulting list is permitted
   but not required to share structure with the input LIST.*/
kiss_obj* kiss_reverse(kiss_obj* p) {
    kiss_obj* stack = KISS_NIL;
    for (p = Kiss_List(p); KISS_IS_CONS(p); p = KISS_CDR(p)) {
        kiss_push(KISS_CAR(p), &stack);
    }
    return stack;
}


/* function (nreverse list) -> <list>
   Return a list whose elements are those of the given LIST, but in reverse
   order.  An error shall be signaled if LIST is not a list (error-id. domain-error ).
   the conses which make up the top level of the given list are permitted,
   but not required, to be side-effected in order to produce this new list.
   nreverse should never be called on a literal object. */
kiss_obj* kiss_nreverse(kiss_obj* p) {
     p = Kiss_List(p);
     if (p == KISS_NIL) {
          return p;
     } else {
          /*
             +---+    +---+    
             |   |--->|   |--->nil
             +---+    +---+    
          */
          kiss_obj* p2 = KISS_CDR(p);
          ((kiss_cons_t*)p)->cdr = KISS_NIL;
          while (KISS_IS_CONS(p2)) {
               kiss_obj* p3 = KISS_CDR(p2);
               /*
                 +---+           +---+    p3
                 |p  |--->nil    |p2 |--->nil
                 +---+           +---+    
               */
               ((kiss_cons_t*)p2)->cdr = p;
               p = p2;
               p2 = p3;
               /*                        p2
                        +---+    +---+   p3
                 nil<---|   |<---|p  |   nil
                        +---+    +---+
               */
          }
          return p;
     }
}

/* function: (member obj list) -> <list>
   If LIST contains at least one occurrence of OBJ (as determined by eql),
   the first sublist of LIST whose car is OBJ is returned.
   Otherwise, nil is returned.
   An error shall be signaled if LIST is not a list (error-id. domain-error ).

   Example: (member 'c '(a b c d e f)) => (c d e f) */
inline kiss_obj* kiss_member(kiss_obj* const obj, kiss_obj* const list) {
     for (const kiss_obj* p = Kiss_List(list); KISS_IS_CONS(p); p = KISS_CDR(p)) {
          if (KISS_CAR(p) == obj) { return (kiss_obj*)p; }
     }
     return KISS_NIL;
}

/* function: (assoc obj association-list) -> <list>
   If ASSOCATION-LIST contains at least one cons whose car is OBJ (as
   determined by eql), the first such cons is returned. Otherwise, nil is
   returned. An error shall be signaled if ASSOCIATION-LIST is not a list
   of conses (error-id. domain-error ).

   Example:
   (assoc 'a '((a . 1) (b . 2))) => (a . 1) */
inline kiss_obj* kiss_assoc(const kiss_obj* const obj, kiss_obj* const alist) {
    for (const kiss_obj* p = Kiss_List(alist); KISS_IS_CONS(p); p = KISS_CDR(p)) {
        kiss_cons_t* x = Kiss_Cons(KISS_CAR(p));
        if (kiss_eql(obj, x->car) == KISS_T) { return (kiss_obj*)x; }
    }
    return KISS_NIL;
}

/* Emacs Lisp function: (plist-member plist property) -> <tail of plist>
   This returns non-`nil' if PLIST contains the given PROPERTY.
   Unlike `plist-get', this allows you to distinguish between a
   missing property and a property with the value `nil'.  The value
   is actually the tail of PLIST whose `car' is PROPERTY. */
kiss_obj* kiss_plist_member (kiss_obj* plist, const kiss_obj* const property) {
     for (plist = Kiss_List(plist); KISS_IS_CONS(plist); plist = KISS_CDR(plist)) {
          if (KISS_CAR(plist) == property) {
               return plist;
          }
          plist = KISS_CDR(plist);
          if (!KISS_IS_CONS(plist)) { return KISS_NIL; }
     }
     return KISS_NIL;
}

kiss_obj* kiss_plist_remove(kiss_obj* plist, const kiss_obj* const property) {
     kiss_obj* prev = KISS_NIL;
     kiss_obj* p = Kiss_List(plist);
     while (KISS_IS_CONS(p)) {
          if (KISS_CAR(p) == property) {
               if (prev == KISS_NIL) {
                    p = kiss_cddr(p);
                    plist = p;
               } else {
                    p = kiss_cddr(p);
                    kiss_set_cdr(p, prev);
               }
          } else {
               p = KISS_CDR(p);
               prev = p;
               p = kiss_cdr(p);
          }
     }
     return plist;
}

/* Emacs Lisp function: (plist-get plist property) -> value
   This returns the value of the PROPERTY property stored in the
   property list PLIST.  It accepts a malformed PLIST argument.  If
   PROPERTY is not found in the PLIST, it returns `nil'.

   Example:
   (plist-get '(foo 4) 'foo)     => 4
   (plist-get '(foo 4 bad) 'foo) => 4
   (plist-get '(foo 4 bad) 'bad) => nil
   (plist-get '(foo 4 bad) 'bar) => nil */
kiss_obj* kiss_plist_get (kiss_obj* plist, const kiss_obj* const property) {
    kiss_obj* here = kiss_plist_member(plist, property);
    if (here == KISS_NIL) {
        return KISS_NIL;
    } else {
        here = KISS_CDR(here);
        if (KISS_IS_CONS(here)) {
            return KISS_CAR(here);
        } else {
            return KISS_NIL;
        }
    }
}

/* Emacs Lisp function: (plist-put plist property value) -> modified-plist
     This stores VALUE as the value of the PROPERTY property in the
     property list PLIST.  It may modify PLIST destructively, or it may
     construct a new list structure without altering the old.  The
     function returns the modified property list, so you can store that
     back in the place where you got PLIST.  For example,

     Example:
     (setq my-plist '(bar t foo 4)) => (bar t foo 4)
     (setq my-plist (plist-put my-plist 'foo 69)) => (bar t foo 69)
     (setq my-plist (plist-put my-plist 'quux '(a))) => (bar t foo 69 quux (a))
 */
kiss_obj* kiss_plist_put (kiss_obj* plist, const kiss_obj* const property, const kiss_obj* const value)
{
    kiss_obj* here = kiss_plist_member(plist, property);
    if (here == KISS_NIL) {
        return kiss_c_append(2, plist, kiss_c_list(2, property, value));
    } else {
        kiss_set_car(value, kiss_cdr(here));
        return plist;
    }
}
