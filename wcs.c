/*  -*- coding: utf-8 -*-
  wcs.c --- defines the wide character string mechanism of ISLisp processor KISS.

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

char* kiss_wctombs(const wchar_t c) {
     wchar_t wcs[2];
     wcs[0] = c;
     wcs[1] = L'\0';
     return kiss_wcstombs(wcs);
}

char* kiss_wcstombs(const wchar_t* const src) {
     size_t size = sizeof(char) * MB_LEN_MAX * wcslen(src) + 1;
     char* buf = Kiss_Malloc(size);
     if (wcstombs(buf, src, size) == -1) {
	  Kiss_System_Error();
     }
     return buf;
}

wchar_t* kiss_mbstowcs(const char* const src) {
     size_t size = strlen (src) + 1;
     wchar_t *buf = Kiss_Malloc (size * sizeof (wchar_t));
     if (mbstowcs(buf, src, size) == -1) {
	  Kiss_System_Error();
     }
     return buf;
}
