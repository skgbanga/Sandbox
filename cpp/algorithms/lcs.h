#pragma once

#include <unordered_map>
#include <vector>

namespace detail {

template <typename Container>
class EugeneMeyers {
 public:
  // Code taken from
  //
  // http://simplygenius.net/Article/DiffTutorial1
  using mapping_t = std::unordered_map<int, int>;
  EugeneMeyers(const Container& A, const Container& B, int d)
      : A_(A), B_(B), max_d_(d) {
    if (max_d_ == std::numeric_limits<int>::max()) {
      max_d_ = int(A_.size() + B_.size());
    }
  }

  bool goDown(int k, int d, const mapping_t& V) const {
    if (k == -d) {
      return true;
    } else if (k == d) {
      return false;
    } else {
      return V.at(k - 1) < V.at(k + 1);
    }
  }

  void generateSnapshots() {
    auto N = (int)A_.size();
    auto M = (int)B_.size();

    mapping_t V;
    V[1] = 0;

    for (int d = 0; d <= max_d_; ++d) {
      for (int k = -d; k <= d; k += 2) {
        bool down = goDown(k, d, V);
        int prev_k = (down) ? k + 1 : k - 1;

        int x_start = V[prev_k];

        int x_next = (down) ? x_start : x_start + 1;
        int y_next = x_next - k;

        // see if we can do down diagonaly
        while ((x_next < N) && (y_next < M)) {
          if (A_[ss(x_next)] == B_[ss(y_next)]) {
            ++x_next;
            ++y_next;
          } else {
            break;
          }
        }

        // record the farthest length we have tranversed while we are still in
        // the grid
        if ((x_next <= N) && (y_next <= M)) {
          farthest_ = std::max(farthest_, Point{x_next, y_next});
        }

        // we have reached the furthest we can do with this value of k
        V[k] = x_next;

        if ((x_next == N) && (y_next == M)) {
          VS_.push_back(V);
          return;
        }
      }
      VS_.push_back(V);
    }

    return;
  }

  template <typename Func>
  void getPath(Func&& func) {
    getPathImpl(VS_.size() - 1, farthest_, std::forward<Func>(func));
  }

  static size_t ss(int i) { return (size_t)i; }

  const Container& A_;
  const Container& B_;
  int max_d_;

  struct Point {
    int x;
    int y;
    friend bool operator<(const Point& lhs, const Point& rhs) {
      auto dist = [](auto p) { return p.x * p.x + p.y * p.y; };
      return dist(lhs) < dist(rhs);
    }
  };
  Point farthest_{0, 0};
  std::vector<mapping_t> VS_{};

  template <typename Func>
  void getPathImpl(size_t d, Point p, Func&& func) {
    if (d == 0) {
      assert(p.x == p.y);
      for (int i = 0; i < p.x; ++i) {
        func.both(A_[ss(i)]);
      }
      return;
    }

    const auto& V = VS_[d];
    int k = (p.x - p.y);

    int xend = V.at(k);
    int yend = xend - k;

    bool down = goDown(k, (int)d, V);
    int prev_k = (down) ? k + 1 : k - 1;

    int xstart = V.at(prev_k);
    int ystart = xstart - prev_k;

    getPathImpl(d - 1, Point{xstart, ystart}, std::forward<Func>(func));

    int xmid = (down) ? xstart : xstart + 1;
    int ymid = xmid - k;

    // print the mid
    if (down) {
      func.b(B_[ss(ymid - 1)]);
    } else {
      func.a(A_[ss(xmid - 1)]);
    }

    // print the snake
    while ((xmid < xend) && (ymid < yend)) {
      ++xmid;
      ++ymid;
      func.both(A_[ss(xmid - 1)]);
    }
  }
};

}

template <typename Container, typename Func>
void eugene_meyers(const Container& A,
                   const Container& B,
                   Func&& func,
                   int d = std::numeric_limits<int>::max()) {
  detail::EugeneMeyers solver(A, B, d);
  solver.generateSnapshots();
  solver.getPath(func);
}
