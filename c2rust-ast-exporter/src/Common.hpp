#ifndef Common_hpp
#define Common_hpp

#include "clang/Basic/Version.h"

#if CLANG_VERSION_MAJOR < 8
#define LLVM8_COMPAT(new_expr, old_expr) old_expr
#else // CLANG_VERSION_MAJOR >= 8
#define LLVM8_COMPAT(new_expr, old_expr) new_expr
#endif // CLANG_VERSION_MAJOR


#endif // Common_hpp
