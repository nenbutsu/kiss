/*  -*- coding: utf-8 -*-
  init.c --- defines the initialization mechanism of ISLisp processor KISS.

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

void kiss_init_error_catcher(void) {
    kiss_environment_t* env = Kiss_Get_Environment();
    size_t saved_heap_index = env->heap_index;
    kiss_obj* tag = kiss_symbol(L"kiss::error");
    kiss_catcher_t* c = kiss_make_catcher(tag, env->top_level);
    env->dynamic_env.jumpers = kiss_cons((kiss_obj*)c, env->dynamic_env.jumpers);
    env->heap_index = saved_heap_index;
}

void kiss_initialize(void) {
     kiss_init_environment();
     kiss_init_symbols();
     kiss_init_streams();
     kiss_init_error_catcher();
}
