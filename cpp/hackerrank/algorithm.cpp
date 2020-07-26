#include <iostream>
#include <algorithm>
#include <vector>
#include <numeric>
#include <type_traits>
#include <string>
#include <sstream>
#include <cassert>
#include <iterator>
#include <array>
#include <cctype>

// typedefs
using position_t = std::pair<int, int>;
using grid_t     = std::vector<std::vector<char>>;

namespace util
{
   void print_vec(const std::vector<char>& vec)
   {
      for (const auto& val : vec)
         std::cout << val << ' ';
      std::cout << '\n';
   }

   template <typename T>
   void print_grid(const std::vector<std::vector<T>>& grid)
   {
      std::for_each(grid.cbegin(), grid.cend(), [](const std::vector<T>& row) { print_vec(row); });
   }

   template <std::size_t N>
   void print_positions(const std::array<position_t, N>& positions)
   {
      std::for_each(positions.cbegin(), positions.cend(), [](const position_t& pos)
            {
               std::cout << '(' << pos.first << ", " << pos.second << ')' << std::endl;
            });
   }

   template <std::size_t N>
   void print_graph(const std::array<std::vector<char>, N>& graph)
   {
      int position = 0;
      std::for_each(graph.cbegin(), graph.cend(), [&](const std::vector<char>& vec)
            {
               std::cout << position++ << ": ";
               print_vec(vec);
            });
   }

} // end namespace util

// Chess pieces
enum class ChessPiece : uint8_t
{
   Knight,
   Bishop
};

// stream insertion for ChessPiece
template <typename Stream>
Stream& operator >>(Stream& stream, ChessPiece& piece)
{
   std::string str = "";
   stream >> str;
   if (str == "knight")
      piece = ChessPiece::Knight;
   else if (str == "bishop")
      piece = ChessPiece::Bishop;
   else
   {
      std::cerr << "Only two chess pieces supported: knight or bishop" << std::endl;
      std::exit(EXIT_FAILURE);
   }

   return stream;
}

namespace io
{
   // helper function to avoid default initialiation of chessPiece
   ChessPiece makeChessPiece(std::istream& stream)
   {
      ChessPiece pc;
      stream >> pc;
      return pc;
   }

   // helper function to get type T from istream to avoid initializing them first
   template <typename T, typename std::enable_if<std::is_arithmetic<T>::value, int>::type = 0>
   T get(std::istream& stream)
   {
      T value;
      stream >> value;
      return value;
   }

   // get phone number digits
   // digits are separated by space
   std::vector<char> getDigits(std::istream& stream)
   {
      // https://marcoarena.wordpress.com/2016/03/13/cpp-competitive-programming-io/
      std::string str;
      stream >> std::ws; // consume leading white spaces
      getline(stream, str);

      std::vector<char> digits;
      std::istringstream iss(str);
      std::copy(std::istream_iterator<char>(iss),
                std::istream_iterator<char>(),
                std::back_inserter(digits));

      return digits; // Named return value optimization - no copy
   }

   // rows/cols of the grid will be referred as if they were 0 indexed
   grid_t makeGrid(std::istream& stream)
   {
      auto rows = get<int32_t>(stream);
      auto cols = get<int32_t>(stream);

      grid_t grid;
      for (int32_t i = 0; i < rows; ++i)
      {
         auto vec = getDigits(stream); // give a name so that we can verify we have recieved all the cols
         assert(vec.size() == cols);
         grid.push_back(std::move(vec)); // move it since we don't need it anymore
      }

      return grid;
   }

} // end namespace io

// a utility function to return the positions of all the digits (0..9) in the grid
// if a number if not present, a {-1, -1} is returned
std::array<position_t, 10> digitPositions(const grid_t& grid)
{
   // create default positions for all the numbers
   std::array<position_t, 10> positions {};
   std::fill(positions.begin(), positions.end(), position_t(-1, -1));

   int rows = grid.size();
   assert(rows > 0);
   int cols = grid.front().size();
   assert(cols > 0);

   for (int r = 0; r < rows; ++r)
   {
      for (int c = 0; c < cols; ++c)
      {
         char ph = grid[r][c]; // placeholder
         // if the ph is not a digit, we don't care about it
         if (!std::isdigit(ph))
            continue;

         positions[ph - 48] = std::make_pair(r, c);
      }
   }
   return positions;
};

std::vector<position_t> knight_moves(const grid_t& grid, const position_t& pos)
{
   int rows = grid.size();
   int cols = grid.front().size();

   int r = pos.first;
   int c = pos.second;

   auto not_valid_pos = [=](const position_t& pos)
   {
      bool valid =  (pos.first >= 0) && (pos.second >= 0) &&
                    (pos.first < rows) && (pos.second < cols);
      return !valid;
   };

   std::vector<position_t> possible_moves = { {r-2, c+1},
                                              {r-1, c+2},
                                              {r+1, c+2},
                                              {r+2, c+1},
                                              {r-2, c-1},
                                              {r-1, c-2},
                                              {r+1, c-2},
                                              {r+2, c-1}
                                            };
   possible_moves.erase(std::remove_if(possible_moves.begin(), possible_moves.end(), not_valid_pos),
                        possible_moves.end());

   return possible_moves;
}

std::vector<position_t> bishop_moves(const grid_t& grid, const position_t& pos)
{
   // Avoid this copy paste
   int rows = grid.size();
   int cols = grid.front().size();

   int r = pos.first;
   int c = pos.second;

   auto valid_pos = [=](const position_t& pos)
   {
      bool valid =  (pos.first >= 0) && (pos.second >= 0) &&
                    (pos.first < rows) && (pos.second < cols);
      return valid;
   };

   std::vector<position_t> possible_moves {};

   const auto max_len = 2 * std::max(rows, cols); // conservativ estimate

   // Make this better!
   for (int i = 0; i < max_len; ++i)
   {
      auto pos1 = std::make_pair(r + i, c + i);
      if (valid_pos(pos1))
         possible_moves.push_back(pos1);

      auto pos2 = std::make_pair(r + i, c - i);
      if (valid_pos(pos2))
         possible_moves.push_back(pos2);

      auto pos3 = std::make_pair(r - i, c + i);
      if (valid_pos(pos3))
         possible_moves.push_back(pos3);

      auto pos4 = std::make_pair(r - i, c - i);
      if (valid_pos(pos1))
         possible_moves.push_back(pos4);
   }

   return possible_moves;
}

std::vector<position_t> possible_moves(const grid_t& grid,
                                       const position_t& pos,
                                       const ChessPiece& pc)
{
   switch (pc)
   {
      case ChessPiece::Knight:
         return knight_moves(grid, pos);
      case ChessPiece::Bishop:
         return bishop_moves(grid, pos);
   }
}

// build a "reachability" 'graph' i.e. if our chess piece starts at '0', where all it can go
// depending upon the grid that we have. Since there are 10 digits (0..9), we fix one dimension
// If a number if not present or we can't go anywhere from that number, return an empty vector
std::array<std::vector<char>, 10> makeReachabilityGraph(const grid_t& grid,
      const ChessPiece& pc)
{
   // initialize the graph
   std::array<std::vector<char>, 10> graph {};

   // digit positions
   auto positions = digitPositions(grid);

   for (int d = 0; d < 10; ++d)
   {
      auto pos = positions[d];
      auto moves = possible_moves(grid, pos, pc);
      // check if there are valid digits at the set of possible moves. If there are,
      // add them to out vector
      std::for_each(moves.begin(), moves.end(), [&](const position_t& pos)
            {
               char ph = grid[pos.first][pos.second]; // remember these are valid moves
               if (!std::isdigit(ph))
                  return;

               // remember, there a digit can appear at most only one
               graph[d].push_back(ph);
            });
   }
   return graph;
}

// Calculate possible phone number lengths of length n given a grid
// and a set of starting positions and chess piece
int64_t valid_phone_numbers(const grid_t& grid,
                            const std::vector<char>& starting_digits,
                            int32_t phone_length,
                            ChessPiece pc)
{
   // construct the reachability graph
   auto graph = makeReachabilityGraph(grid, pc);

   // construct a matrix (phone_number_len * 10). Our answer is the sum of the last row
   // of this matrix. Default initialize it (all the values are initially zero)
   //
   // Rationale: Dynamic programming
   // Each [row][col] represents number of possible phone numbers of lengt row + 1
   // ending at number col
   std::vector<std::array<int64_t, 10>> matrix (phone_length);
   // fill the first row from starting digits
   std::for_each(starting_digits.cbegin(), starting_digits.cend(), [&](char c)
         {
            matrix[0][c - 48] = 1;
         });

   for (int r = 1; r < phone_length; ++r)
   {
      const auto& previous = matrix[r - 1];
      for (int d = 0; d < graph.size(); ++d) // constant = 10
      {
         const auto& neighbors = graph[d];
         std::for_each(neighbors.cbegin(), neighbors.cend(), [&](char c)
               {
                  matrix[r][c - 48] += previous[d];
               });
      }
   }

   // sum the last row
   const auto& last = matrix.back();
   int64_t total = 0;
   return std::accumulate(last.cbegin(), last.cend(), total);
};

int main(int argc, char *argv[])
{
   // get the input
   auto chessPc = io::makeChessPiece(std::cin);
   auto phone_length = io::get<int32_t>(std::cin);
   auto valid_starting_digits = io::getDigits(std::cin);
   auto grid = io::makeGrid(std::cin);

   // temporary: make the grid yourself
   // grid_t grid = { {'1', '2', '3'},
   //                 {'4', '5', '6'},
   //                 {'7', '8', '9'},
   //                 {'*', '0', '#'} };

   auto num = valid_phone_numbers(grid, valid_starting_digits, phone_length, chessPc);
   std::cout << num << '\n';
}
