﻿#pragma once

#include "system/angband.h"

bool make_object(player_type *owner_ptr, object_type *j_ptr, BIT_FLAGS mode);
bool make_gold(floor_type *floor_ptr, object_type *j_ptr);
void delete_all_items_from_floor(player_type *owner_ptr, POSITION y, POSITION x);
void floor_item_increase(floor_type *floor_ptr, INVENTORY_IDX item, ITEM_NUMBER num);
void floor_item_optimize(player_type *owner_ptr, INVENTORY_IDX item);
