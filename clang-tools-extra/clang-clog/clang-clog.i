%module clog

%include "std_vector.i"
%include "inttypes.i"
%include "std_string.i"

namespace std {
	  %template(VectorLong) std::vector<int64_t>;
	  %template(VectorVectorLong) std::vector<std::vector<int64_t>>;
	  %template(VectorString) std::vector<std::string>;
}

%{
#include "ClangClog.h"
%}

%include "ClangClog.h"