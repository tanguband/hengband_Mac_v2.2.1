#pragma once

#include "system/h-type.h"

/**
 * @brief 2次元平面上のベクトルを表すクラス
 */
template <typename T>
struct Vector2D {
    T y{};
    T x{};
    constexpr Vector2D(T y, T x)
        : y(y)
        , x(x)
    {
    }

    constexpr Vector2D &operator+=(const Vector2D &other)
    {
        this->y += other.y;
        this->x += other.x;
        return *this;
    }

    constexpr Vector2D &operator*=(T scalar)
    {
        this->y *= scalar;
        this->x *= scalar;
        return *this;
    }
};

/**
 * @brief 2次元平面上の座標を表すクラス
 */
template <typename T>
struct Point2D {
    T y{};
    T x{};
    constexpr Point2D(T y, T x)
        : y(y)
        , x(x)
    {
    }

    constexpr Point2D &operator+=(const Vector2D<T> &vector)
    {
        this->y += vector.y;
        this->x += vector.x;
        return *this;
    }

    // 座標同士の加算は意味がないので明示的に削除しておく
    constexpr Point2D &operator+=(const Point2D &other) = delete;

    // 座標同士の減算の結果はVector2Dであり、自身から座標を引くことには意味がないので明示的に削除しておく
    constexpr Point2D &operator-=(const Point2D &other) = delete;
};

template <typename T>
constexpr bool operator==(const Point2D<T> &point1, const Point2D<T> &point2)
{
    return (point1.y == point2.y) && (point1.x == point2.x);
}

template <typename T>
constexpr bool operator!=(const Point2D<T> &point1, const Point2D<T> &point2)
{
    return !(point1 == point2);
}

template <typename T>
constexpr Point2D<T> operator+(const Point2D<T> &point, const Vector2D<T> &vector)
{
    auto result = point;
    result += vector;
    return result;
}

template <typename T>
constexpr Point2D<T> operator+(const Vector2D<T> &vector, const Point2D<T> &point)
{
    return operator+(point, vector);
}

template <typename T>
constexpr Vector2D<T> operator-(const Point2D<T> &point1, const Point2D<T> &point2)
{
    return { point1.y - point2.y, point1.x - point2.x };
}

template <typename T>
constexpr Vector2D<T> operator+(const Vector2D<T> &vector1, const Vector2D<T> &vector2)
{
    auto result = vector1;
    result += vector2;
    return result;
}

template <typename T>
constexpr Vector2D<T> operator*(const Vector2D<T> &vector, T scalar)
{
    auto result = vector;
    result *= scalar;
    return result;
}

// 座標同士の加算は意味がないので明示的に削除しておく
template <typename T>
constexpr Point2D<T> operator+(const Point2D<T> &, const Point2D<T> &) = delete;

//! ゲームの平面マップ上の座標位置を表す構造体
using Pos2D = Point2D<POSITION>;

using Pos2DVec = Vector2D<POSITION>;
