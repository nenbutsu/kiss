/*
  malloc.c --- malloc mechanism of ISLisp processor KISS.

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


/* An error shall be signaled if the requested memory cannot be allocated
   (error-id. <storage-exhausted>). */
void* Kiss_Malloc(size_t size) {
    void* p = GC_MALLOC(size);
    if (p == NULL) { Kiss_System_Error(); }
    return p;
}


void* Kiss_Malloc_Atomic(size_t size) {
    void* p = GC_MALLOC_ATOMIC(size);
    if (p == NULL) { Kiss_System_Error(); }
    return p;
}
