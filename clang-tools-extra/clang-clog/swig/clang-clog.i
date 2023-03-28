%module clog

%include "inttypes.i"
%include "std_vector.i"
%include "std_string.i"

namespace std {
  %template(VectorLong) std::vector<long long>;
  %template(VectorVectorLong) std::vector<std::vector<long long>>;
  %template(VectorString) std::vector<std::string>;
}

%{
#include "../ClangClog.h"
%}

%include "../ClangClog.h"
