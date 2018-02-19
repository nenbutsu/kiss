/*  -*- coding: utf-8 -*-
  environment.c --- defines the environment mechanism of ISLisp processor KISS.

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

static kiss_environment_t Kiss_Environment;
static jmp_buf Top_Level;

kiss_environment_t* Kiss_Get_Environment(void) { return &Kiss_Environment; }

void kiss_init_environment(void) {
    kiss_environment_t* env = Kiss_Get_Environment();

    env->lexical_env.vars           = KISS_NIL;
    env->lexical_env.funs           = KISS_NIL;
    env->lexical_env.jumpers        = KISS_NIL;

    env->dynamic_env.vars           = KISS_NIL;
    env->dynamic_env.jumpers        = KISS_NIL;
    env->dynamic_env.backquote_nest = 0;
    env->dynamic_env.gf_invocations = KISS_NIL;

    env->lexeme_chars               = KISS_NIL;

    env->throw_result               = KISS_NIL;
    env->block_result               = KISS_NIL;
    env->current_tagbody            = NULL;

    env->top_level                  = Top_Level;
    env->global_dynamic_vars        = KISS_NIL;
    env->call_stack                 = KISS_NIL;
    env->error_call_stack           = KISS_NIL;
}
