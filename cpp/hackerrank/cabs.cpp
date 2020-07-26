#include <iostream>
#include <string>
#include <vector>
#include <numeric>

enum class time_type
{
   Start,
   End
};

struct Preference
{
   std::string start;
   std::string end;
};

struct cab_time
{
   cab_time(std::string ti, time_type ty)
      : time(std::move(ti)), type(ty)
   {  }

   std::string time;
   time_type type;
};

bool operator<(const cab_time& lhs, const cab_time& rhs)
{
   return std::tie(lhs.time, lhs.type) < std::tie(rhs.time, rhs.type);
}

uint64_t min_cabs(const std::vector<Preference>& times)
{
   // create a vector with all the times
   std::vector<cab_time> points;
   points.reserve(times.size() * 2);
   for (const auto& val : times)
   {
      points.emplace_back(val.start, time_type::Start);
      points.emplace_back(val.end, time_type::End);
   }

   std::sort(points.begin(), points.end());

   uint64_t num_cars = 0;
   uint64_t cabs = std::numeric_limits<uint64_t>::min();
   for (const auto& val : points)
   {
      if (val.type == time_type::Start)
         ++num_cars;
      if (val.type == time_type::End)
         --num_cars;

      cabs = std::max(num_cars, cabs);
   }

   return cabs;
}

int main(int argc, char *argv[])
{
   /* std::vector<Preference> cab_times { {"09:00", "10:00"}, {"10:00", "11:00"} }; */
   std::vector<Preference> cab_times {
      {"01:00", "02:00"},
      {"16:00", "21:30"},
      {"09:30", "13:00"},
      {"21:00", "22:30"},
      {"12:00", "12:30"}
   };
   std::cout << min_cabs(cab_times) << '\n';
   return 0;
}
