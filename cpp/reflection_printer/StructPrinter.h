#pragma once

/// Pretty prints any struct which can be defined as boost hana sequence
// Sequence of { name_of_variable: value_of_variable }

#include "TypeTraits.h"
#include <boost/hana/for_each.hpp>

template <typename MsgType, typename Stream>
struct MsgPrinter
{
   MsgType& msg_;
   Stream& stream_;
   MsgPrinter(MsgType& msg, Stream& stream) : msg_(msg), stream_(stream) {}

   /* // For integral_type */
   /* template <typename Accessor> */
   /*    auto operator()(Accessor elem) -> */
   /*       typename std::enable_if<is_accessor_integral<Accessor, MsgType>::value>::type */
   /*    { */
   /*       stream_ << "Found integer"; */
   /*    } */

   template <typename Accessor>
      auto operator()(Accessor elem) -> void
         /* typename std::enable_if<!is_accessor_integral<Accessor, MsgType>::value>::type */
      {
         stream_ << hana::to<const char*>(hana::first(elem)) << "::";
         stream_ << hana::second(elem)(msg_) << " ";
      }
};

template <typename MsgType, typename Stream>
 // concept: MsgType is a boost sequence
void print_struct(MsgType& msg, Stream& stream)
{
   auto accessors = hana::accessors<MsgType>();
   hana::for_each(accessors, MsgPrinter<MsgType, Stream>(msg, stream));
}
