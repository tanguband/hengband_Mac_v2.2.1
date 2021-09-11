﻿#pragma once
/*!
 * @file throw-util.h
 * @brief 投擲処理関連ヘッダ
 */

#include "system/angband.h"
#include "system/system-variables.h"

#include "object-enchant/tr-flags.h"
#include "util/flag-group.h"

struct grid_type;
struct monster_type;
struct object_type;
struct player_type;
class ObjectThrowEntity {
public:
    ObjectThrowEntity() = default;
    ObjectThrowEntity(
        player_type *player_ptr, object_type *q_ptr, const int delay_factor_val, const int mult, const bool boomerang, const OBJECT_IDX shuriken);
    virtual ~ObjectThrowEntity() = default;

    object_type *q_ptr;
    OBJECT_IDX item{};
    POSITION y{};
    POSITION x{};
    POSITION prev_y{};
    POSITION prev_x{};
    bool equiped_item = false;
    bool hit_body = false;
    PERCENTAGE corruption_possibility{};

    bool check_can_throw();
    void calc_throw_range();
    bool calc_throw_grid();
    void reflect_inventory_by_throw();
    void set_class_specific_throw_params();
    void set_racial_chance();
    void exe_throw();
    void display_figurine_throw();
    void display_potion_throw();
    void check_boomerang_throw();
    void process_boomerang_back();
    void drop_thrown_item();

private:
    player_type *player_ptr;
    OBJECT_IDX shuriken;
    int mult;
    int msec;
    bool boomerang;
    POSITION ty{};
    POSITION tx{};
    POSITION ny[19]{};
    POSITION nx[19]{};
    int chance{};
    int tdam{};
    int tdis{};
    int cur_dis{};
    int visible{};
    object_type *o_ptr{};
    bool hit_wall = false;
    bool return_when_thrown = false;
    GAME_TEXT o_name[MAX_NLEN]{};
    TrFlags obj_flags{};
    bool come_back = false;
    bool do_drop = true;
    grid_type *g_ptr{};
    monster_type *m_ptr{};
    GAME_TEXT m_name[MAX_NLEN]{};
    int back_chance{};
    char o2_name[MAX_NLEN]{};
    bool super_boomerang{};

    bool check_what_throw();
    bool check_throw_boomerang(concptr *q, concptr *s);
    bool check_racial_target_bold();
    void check_racial_target_seen();
    bool check_racial_target_monster();
    void attack_racial_power();
    void display_attack_racial_power();
    void calc_racial_power_damage();
    void process_boomerang_throw();
    void display_boomerang_throw();
};
