﻿#pragma once

#include "system/angband.h"

extern int summon_specific_who;
extern bool summon_unique_okay;

bool summon_specific(player_type *player_ptr, MONSTER_IDX who, POSITION y1, POSITION x1, DEPTH lev, int type, BIT_FLAGS mode);
bool summon_named_creature(player_type *player_ptr, MONSTER_IDX who, POSITION oy, POSITION ox, MONRACE_IDX r_idx, BIT_FLAGS mode);
bool multiply_monster(player_type *player_ptr, MONSTER_IDX m_idx, bool clone, BIT_FLAGS mode);
