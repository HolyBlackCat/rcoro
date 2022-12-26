# --- Build modes ---

_proj_commonflags :=
_proj_cxxflags :=
_proj_ldflags :=

$(call NewMode,debug)
$(Mode)COMMON_FLAGS := -g
$(Mode)CXXFLAGS := -D_GLIBCXX_DEBUG

$(call NewMode,debug_soft)
$(Mode)COMMON_FLAGS := -g
$(Mode)CXXFLAGS := -D_GLIBCXX_ASSERTIONS

$(call NewMode,release)
$(Mode)COMMON_FLAGS := -O3
$(Mode)_proj_commonflags := -flto
$(Mode)CXXFLAGS := -DNDEBUG -DIMP_PLATFORM_FLAG_prod=1
$(Mode)LDFLAGS := -s

$(call NewMode,profile)
$(Mode)COMMON_FLAGS := -O3 -pg
$(Mode)CXXFLAGS := -DNDEBUG -DIMP_PLATFORM_FLAG_prod=1

$(call NewMode,sanitize_address)
$(Mode)COMMON_FLAGS := -g -fsanitize=address
$(Mode)CXXFLAGS := -D_GLIBCXX_DEBUG

$(call NewMode,sanitize_ub)
$(Mode)COMMON_FLAGS := -g -fsanitize=undefined
$(Mode)CXXFLAGS := -D_GLIBCXX_DEBUG

$(call NewMode,sanitize_address_ub)
$(Mode)COMMON_FLAGS := -g -fsanitize=address -fsanitize=undefined
$(Mode)CXXFLAGS := -D_GLIBCXX_DEBUG

# --- Project config ---

_proj_cxxflags += -std=c++20 -pedantic-errors -Wall -Wextra -Wdeprecated -Wextra-semi
_proj_cxxflags += -I../macro_sequence_for/include -Iinclude

$(call Project,exe,macoro_test)
$(call ProjectSetting,sources,tests.cpp)
$(call ProjectSetting,common_flags,$(_proj_commonflags))
$(call ProjectSetting,cxxflags,$(_proj_cxxflags))
$(call ProjectSetting,ldflags,$(_proj_ldflags))
