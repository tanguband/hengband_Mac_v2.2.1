﻿#include "player/race-info-table.h"

/*!
 * @brief 種族情報 /
 * Player Races
 * @details
 * <pre>
 *      Title,
 *      {STR,INT,WIS,DEX,CON,CHR},
 *      r_dis, r_dev, r_sav, r_stl, r_srh, r_fos, r_thn, r_thb,
 *      hitdie, exp base,
 *      Age (Base, Mod),
 *      Male (Average height in inch, Max height deviation in percent, Average weight in pound, Max weight deviation in percent),
 *      Female (Same as above)
 *      infra,
 *      class-choices
 * </pre>
 */
const player_race race_info[MAX_RACES] =
{
	{
#ifdef JP
		"人間",
#endif
		"Human",

		{  0,  0,  0,  0,  0,  0 },
		0,  0,  0,  0,  0,  10,  0,  0,
		10,  100,
		14,  6,
		72,  6, 180, 25,
		66,  4, 150, 20,
		0,
		0x1FFFFFFF,
	},
	{
#ifdef JP
		"ハーフエルフ",
#endif
		"Half-Elf",

		{ -1,  1,  1,  1, -1,  1 },
		2,  3,  2,  1, 6,  11, -1,  5,
		9,  110,
		24, 16,
		66,  6, 130, 15,
		62,  6, 100, 10,
		2,
		0x1E77E7FF,
	},
	{
#ifdef JP
		"エルフ",
#endif
		"Elf",

		{ -1,  2,  0,  1, -2,  2 },
		5,  6,  4,  2, 8,  12, -5, 15,
		8,  120,
		75, 75,
		60,  4, 100,  6,
		54,  4, 80,  6,
		3,
		0x1E77E75B,

	},
	{
#ifdef JP
		"ホビット",
#endif
		"Hobbit",

		{ -2,  1,  1,  3,  2,  1 },
		15, 12, 10, 5, 12,  15, -10, 20,
		7,  110,
		21, 12,
		36,  3, 60,  3,
		33,  3, 50,  3,
		4,
		0x1F6FFC0B,
	},
	{
#ifdef JP
		"ノーム",
#endif
		"Gnome",

		{ -1,  2,  0,  2,  1, -1 },
		10, 8, 7,  3, 6,  13, -8, 12,
		8,  120,
		50, 40,
		42,  3, 90,  6,
		39,  3, 75,  3,
		4,
		0x1F67D60F,
	},
	{
#ifdef JP
		"ドワーフ",
#endif
		"Dwarf",

		{  2, -2,  2, -2,  2, -1 },
		2,  7,  6,  -1,  7,  10, 15,  0,
		11,  125,
		35, 15,
		48,  3, 150, 10,
		46,  3, 120, 10,
		5,
		0x11890005,
	},
	{
#ifdef JP
		"ハーフオーク",
#endif
		"Half-Orc",

		{  2, -1,  0,  0,  1, -2 },
		-3, -3, -2,  -1,  0, 7, 12, -5,
		10,  110,
		11,  4,
		66,  1, 150,  5,
		62,  1, 120,  5,
		3,
		0x1DD8818D,
	},
	{
#ifdef JP
		"ハーフトロル",
#endif
		"Half-Troll",

		{ 4, -4, -1, -3,  3, -3 },
		-5, -8, -5, -2,  -1, 5, 20, -10,
		12,  125,
		20, 10,
		96, 10, 250, 50,
		84,  8, 225, 40,
		3,
		0x00880005,
	},
	{
#ifdef JP
		"アンバライト",
#endif
		"Amberite",

		{  1,  2,  2,  2,  3,  2 },
		4,  5,  3,  2, 3, 13, 15, 10,
		10,  225,
		50, 50,
		82, 5, 190, 20,
		78,  6, 180, 15,
		0,
		0x1FFFF7FF,
	},
	{
#ifdef JP
		"ハイエルフ",
#endif
		"High-Elf",

		{  1,  3,  -1,  3,  1,  3 },
		4,  13, 12,  4,  3, 14, 10, 25,
		10,  200,
		100, 30,
		90, 10, 190, 20,
		82, 10, 180, 15,
		4,
		0x1F77E75B,
	},
	{
#ifdef JP
		"野蛮人",
#endif
		"Barbarian",

		{ 3, -2,  -1,  1,  2, 0 },
		-2, -10, 2,  -1,  1, 7, 12, 10,
		11, 120,
		14, 8,
		82, 5, 200, 20,
		78,  6, 190, 15,
		0,
		0x05C0A09D,
	},
	{
#ifdef JP
		"ハーフオーガ",
#endif
		"Half-Ogre",

		{ 3, -2, 0, -1, 3, -2 },
		-3, -5, -3, -2, -1, 5, 20, 0,
		12,  145,
		40, 10,
		92, 10, 255, 60,
		80,  8, 235, 60,
		3,
		0x10A80407,
	},
	{
#ifdef JP
		"半巨人",
#endif
		"Half-Giant",

		{ 4, -2, -2, -2, 3, -2 },
		-6, -8, -3, -2, -1, 5, 25, 5,
		13, 160,
		40, 10,
		100,10, 255, 65,
		80, 10, 240, 64,
		3,
		0x08880011,
	},
	{
#ifdef JP
		"半タイタン",
#endif
		"Half-Titan",

		{ 5, 1, 2, -2, 3, 1 },
		-5, 5, 1, -2, 1, 8, 25, 0,
		14, 255,
		100,30,
		111, 11, 255, 86,
		99, 11, 250, 86,
		0,
		0x123D4727,
	},
	{
#ifdef JP
		"サイクロプス",
#endif
		"Cyclops",

		{ 4, -3, -2, -3, 4, -3 },
		-4, -5, -3, -2, -2, 5, 20, 12,
		13, 150,
		50, 24,
		92, 10, 255, 60,
		80,  8, 235, 60,
		1,
		0x00888005,
	},
	{
#ifdef JP
		"イーク",
#endif
		"Yeek",

		{ -2, 1, 1, 1, -2, -4 },
		2, 4, 6, 3, 5, 15, -5, -5,
		7, 100,
		14, 3,
		50,  3, 90,  6,
		50,  3, 75,  3,
		2,
		0x1667360F,
	},
	{
#ifdef JP
		"クラッコン",
#endif
		"Klackon",

		{ 2, -1, -1, 1, 2, -1 },
		10, 5, 3, 0, -1, 10, 5, 5,
		12, 190,
		20, 3,
		60,  3, 80,  4,
		54,  3, 70,  4,
		2,
		0x004D8011,
	},
	{
#ifdef JP
		"コボルド",
#endif
		"Kobold",

		{ 1, -1, 0, 1, 0, -2 },
		-2, -3, -1, -1, 1, 8, 10, -8,
		9, 125,
		11,  3,
		60,  1, 130,  5,
		55,  1, 100,  5,
		3,
		0x1444A009,
	},
	{
#ifdef JP
		"ニーベルング",
#endif
		"Nibelung",

		{ 1, -1, 2, 0, 2, -2 },
		3, 5, 6, 1, 5, 10, 9, 0,
		11, 170,
		40, 12,
		43,  3, 92,  6,
		40,  3, 78,  3,
		5,
		0x1569040F,
	},
	{
#ifdef JP
		"ダークエルフ",
#endif
		"Dark-Elf",

		{ -1, 3, 2, 2, -2, 1 },
		5, 10, 12, 3, 8, 12, -5, 10,
		9, 150,
		75, 75,
		60,  4, 100,  6,
		54,  4, 80,  6,
		5,
		0x1E77C7DF,
	},
	{
#ifdef JP
		"ドラコニアン",
#endif
		"Draconian",

		{ 2, 1, 1, 1, 2, -1 },
		-2, 5, 2, 0, 1, 10, 5, 5,
		11, 220,
		75, 33,
		76,  1, 160,  5,
		72,  1, 130,  5,
		2,
		0x07FFE757,
	},
	{
#ifdef JP
		"マインドフレア",
#endif
		"Mindflayer",

		{ -3, 4, 4, 0, -2, -3 },
		10, 15, 9, 2, 5, 12, -10, -5,
		9, 140,
		100, 25,
		68,  6, 142, 15,
		63,  6, 112, 10,
		4,
		0x12334746,
	},
	{
#ifdef JP
		"インプ",
#endif
		"Imp",

		{ 0, -1, -1, 1, 2, -1 },
		-3, 2, -1, 1, -1, 10, 5, -5,
		10, 115,
		13,  4,
		68,  1, 150,  5,
		64,  1, 120,  5,
		3,
		0x1DB537CB,
	},
	{
#ifdef JP
		"ゴーレム",
#endif
		"Golem",

		{ 4, -5, -5, -2, 4, -2 },
		-5, -5, 6, -1, -1, 8, 20, 0,
		12, 200,
		1, 100,
		66,  1, 200,  6,
		62,  1, 180,  6,
		4,
		0x00800001,
	},
	{
#ifdef JP
		"骸骨",
#endif
		"Skeleton",

		{ 0, 1, -2, 0, 1, -2 },
		-5, 0, 3, -1, -1, 8, 10, 0,
		10, 145,
		100, 35,
		72,  6, 50, 5,
		66,  4, 50, 5,
		2,
		0x0234070F,
	},
	{
#ifdef JP
		"ゾンビ",
#endif
		"Zombie",

		{ 2, -6, -6, 1, 4, -3 },
		-5, -5, 5, -1, -1, 5, 15, 0,
		13, 150,
		100, 30,
		72, 6, 100, 25,
		66, 4, 100, 20,
		2,
		0x00800001,
	},
	{
#ifdef JP
		"吸血鬼",
#endif
		"Vampire",

		{ 3, 3, -1, -1, 1, 2 },
		4, 8, 6, 4, 1, 8, 5, 0,
		11, 200,
		100, 30,
		72,  6, 180, 25,
		66,  4, 150, 20,
		5,
		0x067DC7FF,
	},
	{
#ifdef JP
		"幽霊",
#endif
		"Spectre",

		{ -5, 4, -1, 2, 0, -3 },
		10, 15, 12, 5, 5, 14, -15, -5,
		7, 210,
		100, 30,
		72, 6, 100, 25,
		66, 4, 100, 20,
		5,
		0x0631474A,
	},
	{
#ifdef JP
		"妖精",
#endif
		"Sprite",

		{ -4, 3, 3, 3, -2, 2 },
		10, 8, 6, 4, 10, 10, -12, 0,
		7, 145,
		50, 25,
		32,  2, 75,  2,
		29,  2, 65,  2,
		4,
		0x1623F65E,
	},
	{
#ifdef JP
		"獣人",
#endif
		"Beastman",

		{ 2, -2, -1, -1, 2, -2 },
		-5, -2, -1, -1, -1, 5, 12, 5,
		11, 140,
		14, 6,
		65,  6, 150, 20,
		61,  6, 120, 15,
		0,
		0x057887CF,
	},
	{
#ifdef JP
		"エント",
#endif
		"Ent",
		{ 2,  0,  2, -3,  2,  0 },
		 -5,  2,  5,  -1, 0, 9,  15, -5,
		 12, 140,
		120,  60,
		111, 11, 255, 50,
		99, 11, 250, 45,
		  0,
		0x10010005,
	},
	{
#ifdef JP
		"アルコン",
#endif
		"Archon",

		{  2,  0,  4,  1,  2,  3 },
		0,  12,  8,  2, 2, 11, 10, 10,
		11,  235,
		150, 150,
		82, 5, 190, 20,
		78,  6, 180, 15,
		3,
		0x1779F777,
	},
	{
#ifdef JP
		"バルログ",
		"Balrog",
#else
		"Balrog",
#endif

		{  4,  2,  -10,  2,  3,  -5 },
		-3,  12, 15,  -2,  1, 8, 20, 0,
		12,  250,
		255, 255,
		100,10, 255, 65,
		80, 10, 240, 64,
		5,
		0x07EDC4DB,
	},
	{
#ifdef JP
		"ドゥナダン",
#endif
		"Dunadan",

		{  1,  2,  2,  2,  3,  2 },
		4,  5,  3,  2, 3, 13, 15, 10,
		10,  180,
		50, 20,
		82, 5, 190, 20,
		78,  6, 180, 15,
		0,
		0x1FFFF7FF,
	},
	{
#ifdef JP
		"影フェアリー",
#endif
		"Shadow-Fairy",
		{-2,  2,  2,  1, -1,  0 },
		  7,  8,  0,  6, 12, 15, -10, -5,
		  7, 120,
		200, 100,
		 80,  8, 90, 20,
		 73,  8, 80, 15,
		  4,
		0x1E33C7DF,
	},
	{
#ifdef JP
		"クター",
#endif
		"Kutar",

		{  0,  -1,  -1,  1,  2,  3 },
		-2,  5,  5,  5,  -2,  6,  0,  -5,
		11,  140,
		14,  6,
		48,  6, 150, 25,
		44,  4, 130, 20,
		0,
		0x0C18B7AD,
	},
	{
#ifdef JP
		"アンドロイド",
#endif
		"Android",

		{ 4, -5, -5, 0, 4, -2 },
		0, -5, 0, -2, 3, 14, 20, 10,
		13, 200,
		1, 100,
		72, 12, 240, 64,
		66, 12, 220, 64,
		0,
		0x00800001,
	},
	{
#ifdef JP
		"マーフォーク",
#endif
		"Merfolk",

		{ -1,  0,  2,  1,  -1,  1},
		2,  3,  2,  1, 6,  11, -1,  5,
		10,  130,
		24, 16,
		66,  6, 130, 15,
		62,  6, 100, 10,
		2,
		0x1E77E7FF,
	},
};
