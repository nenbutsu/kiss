/*  -*- coding: utf-8 -*-
  ilos.c --- defines the ILOS mechanism of ISLisp processor KISS.

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

kiss_obj* kiss_make_ilos_obj(kiss_obj* plist) {
    kiss_ilos_obj_t* p = Kiss_GC_Malloc(sizeof(kiss_ilos_obj_t));
    p->type = KISS_ILOS_OBJ;
    p->plist = plist;
    return (kiss_obj*)p;
}

kiss_obj* kiss_object_p(kiss_obj* obj) {
    if (KISS_IS_ILOS_OBJ(obj)) { return KISS_T; }
    else                     { return KISS_NIL; }
}

kiss_obj* kiss_object_plist(kiss_obj* obj) {
    kiss_ilos_obj_t* p = Kiss_Object(obj);
    return p->plist;
}

kiss_obj* kiss_set_object_plist(kiss_obj* plist, kiss_obj* obj) {
    kiss_ilos_obj_t* p = Kiss_Object(obj);
    p->plist = plist;
    return obj;
}

kiss_obj* kiss_object_plist_get(kiss_obj* obj, kiss_obj* property) {
    return kiss_plist_get(kiss_object_plist(obj), property);
}

kiss_obj* kiss_object_plist_put(kiss_obj* obj, kiss_obj* property, kiss_obj* value) {
    kiss_obj* plist = kiss_plist_put(kiss_object_plist(obj), property, value);
    return kiss_set_object_plist(plist, obj);    
}
