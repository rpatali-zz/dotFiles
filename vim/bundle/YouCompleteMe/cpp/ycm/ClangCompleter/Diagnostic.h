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

#ifndef DIAGNOSTIC_H_BZH3BWIZ
#define DIAGNOSTIC_H_BZH3BWIZ

#include "standard.h"
#include <string>

namespace YouCompleteMe {

struct Diagnostic {
  bool operator== ( const Diagnostic &other ) const {
    return
      line_number_ == other.line_number_ &&
      column_number_ == other.column_number_ &&
      kind_ == other.kind_ &&
      text_ == other.text_;
  }

  uint line_number_;
  uint column_number_;

  // Vim's error "kind"
  //  'I' -> informational
  //  'W' -> warning
  //  'E' -> error
  char kind_;

  std::string filename_;

  std::string text_;

  std::string long_formatted_text_;
};

} // namespace YouCompleteMe

#endif /* end of include guard: DIAGNOSTIC_H_BZH3BWIZ */
