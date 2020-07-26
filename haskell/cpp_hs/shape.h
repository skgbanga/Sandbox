#pragma once
#include <cmath>

struct Shape {};

struct Point
{
   float x_;
   float y_;
};

struct Circle : Shape
{
   Point center_;
   float radius_;
};

struct Rectangle : Shape
{
   Point left_;
   Point right_;
};

// area function: Makes sense for both of them
float area(const Circle& circle)
{
   return 3.14 * std::pow(circle.radius_, 2);
}

float area(const Rectangle& r)
{
   const auto h = std::abs(r.right_.y_ - r.left_.y_);
   const auto b = std::abs(r.right_.x_ - r.left_.x_);
   return  h * b;
}

// center makes sense only for circle
Point center(const Circle& circle)
{
   return circle.center_;
}

// diagonal (not diameter) makes sense only for rectangle
float diagonal(const Rectangle& r)
{
   const auto h = std::abs(r.right_.y_ - r.left_.y_);
   const auto b = std::abs(r.right_.x_ - r.left_.x_);
   return std::sqrt( std::pow(h, 2) + std::pow(b, 2) );
}
