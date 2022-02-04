﻿#pragma once

#include <stdint.h>
#include <string>

constexpr std::string_view VARIANT_NAME("Hengband");

/*!
 * @brief セーブファイル上のバージョン定義 / "Savefile Version Number" for Hengband
 * @details v1.1.1以上にのみ適用
 */
#define H_VER_MAJOR  3 //!< ゲームのバージョン定義(メジャー番号)
#define H_VER_MINOR  0 //!< ゲームのバージョン定義(マイナー番号)
#define H_VER_PATCH  0 //!< ゲームのバージョン定義(パッチ番号)
#define H_VER_EXTRA 51 //!< ゲームのバージョン定義(エクストラ番号)

/*!
 * @brief セーブファイルのバージョン(3.0.0から導入)
 */
constexpr uint32_t SAVEFILE_VERSION = 10;

/*!
 * @brief バージョンが開発版が安定版かを返す(廃止予定)
 */
constexpr bool IS_STABLE_VERSION = (H_VER_MINOR % 2 == 0 && H_VER_EXTRA == 0);

enum class VersionStatusType {
	ALPHA,
	BETA,
	RELEASE_CANDIDATE,
	RELEASE,
};

/*!
 * @brief バージョンの立ち位置
 */
constexpr VersionStatusType VERSION_STATUS = VersionStatusType::ALPHA;

void put_version(char *buf);
