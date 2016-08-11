#pragma once

#include <cmath>
#include <ostream>

namespace allscale {
namespace utils {

	using value_t = double;

	struct Point {
		value_t x, y, z;

		Point& operator-=(const Point& other) {
			x -= other.x;
			y -= other.y;
			z -= other.z;
			return *this;
		}

		Point operator-(const Point& other) const {
			return Point(*this) -= other;
		}

		Point operator*(const value_t f) const {
			Point res = *this;
			res.x *= f;
			res.y *= f;
			res.z *= f;
			return res;
		}

		bool operator==(const Point& other) const {
			return x == other.x && y == other.y && z == other.z;
		}

		friend std::ostream& operator<<(std::ostream& out, const Point& p) {
			return out << "[" << p.x << "," << p.y << "," << p.z << "]";
		}
	};

	value_t norm(const Point& a) {
		return sqrt(a.x*a.x + a.y*a.y + a.z*a.z);
	}

	value_t dist(const Point& a, const Point& b) {
		return norm(a - b);
	}

	Point cross(const Point& a, const Point& b) {
		return Point{
				a.y*b.z-a.z*b.y,
				a.z*b.x-a.x*b.z,
				a.x*b.y-a.y*b.x
		};
	}

	value_t area(const Point& a, const Point& b, const Point& c) {
		return norm(cross(a-b,a-c))/2;
	}

	using Vector = Point;

} // end namespace utils
} // end namespace allscale
