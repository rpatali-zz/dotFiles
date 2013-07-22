// Copyright (C) 2011, 2012  Strahinja Val Markovic  <val@markovic.io>
//
// This file is part of YouCompleteMe.
//
// YouCompleteMe is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// YouCompleteMe is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with YouCompleteMe.  If not, see <http://www.gnu.org/licenses/>.

#ifndef CONCURRENTSTACK_H_TGI0GOR6
#define CONCURRENTSTACK_H_TGI0GOR6

#include <boost/thread.hpp>
#include <boost/utility.hpp>
#include <stack>

namespace YouCompleteMe {

template <typename T>
class ConcurrentStack : boost::noncopyable {
public:

  void Push( const T &data ) {
    {
      boost::unique_lock< boost::mutex > lock( mutex_ );
      stack_.push( data );
    }

    condition_variable_.notify_one();
  }


  T Pop() {
    boost::unique_lock< boost::mutex > lock( mutex_ );

    while ( stack_.empty() ) {
      condition_variable_.wait( lock );
    }

    T top = stack_.top();
    stack_.pop();
    return top;
  }


  // Gets all the items from the stack and appends them to the input vector.
  // Does not wait for the stack to get items; if the stack is empty, returns
  // false and does not touch the input vector.
  bool PopAllNoWait( std::vector< T > &items ) {
    boost::unique_lock< boost::mutex > lock( mutex_ );

    if ( stack_.empty() )
      return false;

    int num_items = stack_.size();
    items.reserve( num_items + items.size() );

    for ( int i = 0; i < num_items; ++i ) {
      items.push_back( stack_.top() );
      stack_.pop();
    }

    return true;
  }


  bool Empty() {
    boost::unique_lock< boost::mutex > lock( mutex_ );
    return stack_.empty();
  }

private:
  std::stack< T > stack_;
  boost::mutex mutex_;
  boost::condition_variable condition_variable_;

};

} // namespace YouCompleteMe

#endif /* end of include guard: CONCURRENTSTACK_H_TGI0GOR6 */
