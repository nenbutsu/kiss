/*  -*- coding: utf-8 -*-
  main.c --- defines the main mechanism of ISLisp processor KISS.

  Copyright (C) 2017, 2018, 2019 Yuji Minejima <yuji@minejima.jp>

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

int main(void) {
     size_t saved_heap_top = Kiss_Heap_Top;
     kiss_initialize();
     Kiss_Heap_Top = saved_heap_top;
     kiss_read_eval_print_loop();
     return EXIT_SUCCESS;
}
