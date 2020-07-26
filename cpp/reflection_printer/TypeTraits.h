#pragma once

#include <utility>
#include <boost/hana/accessors.hpp>
#include <boost/hana/second.hpp>

namespace hana = boost::hana;

// quick utility to determine the actual type of something
// Your prgram would not compile, and the compiler would shout out the type (Thanks Scott Meyers!)
template <typename TD>
class TypeDeductor;

// determine the underlying type the accessor is pointing to
template <typename Accessor, typename MsgType>
using accessor_type_t = decltype(hana::second(std::declval<Accessor>())(std::declval<MsgType>()));

// determine whether underlying accessor type is integral or not
template <typename Accessor, typename MsgType>
using is_accessor_integral = std::is_integral<typename std::remove_reference<accessor_type_t<Accessor, MsgType>>::type>;

