include(FindPackageHandleStandardArgs)
include(ExternalProject)

# get an uppercase version of the build type, for extracting build_type specific flags
if(CMAKE_BUILD_TYPE)
  string(TOUPPER ${CMAKE_BUILD_TYPE} BUILD_TYPE_UC)
endif()

ExternalProject_Add(pbc
  PREFIX            ${CMAKE_CURRENT_BINARY_DIR}/external-pbc
  GIT_REPOSITORY    https://github.com/Vagabond/pbc
  UPDATE_COMMAND    ""
  BUILD_IN_SOURCE   1
  CONFIGURE_COMMAND autoreconf --install > /dev/null 2>&1 || autoreconf &&
                    ./configure
                    --prefix=${CMAKE_CURRENT_BINARY_DIR}
                    --with-pic
                    --disable-shared
                    --enable-optimized
                    --enable-safe-clean
                    $ENV{CONFIGURE_ARGS}
                    CC=${CMAKE_C_COMPILER}
                    CFLAGS=${CMAKE_C_FLAGS_${BUILD_TYPE_UC}}
                    LDFLAGS=-L${CMAKE_CURRENT_BINARY_DIR}/lib
  BUILD_COMMAND     make -j
  BUILD_BYPRODUCTS  ${CMAKE_CURRENT_BINARY_DIR}/lib/libpbc.a
  INSTALL_COMMAND   make install
  )

# Hack to let us declare a not-yet-existing include path below.
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/include/pbc)

add_library(PBC::PBC STATIC IMPORTED)
target_include_directories(PBC::PBC
  INTERFACE
  ${CMAKE_CURRENT_BINARY_DIR}/include
  )
set_target_properties(PBC::PBC
  PROPERTIES
  IMPORTED_LOCATION ${CMAKE_CURRENT_BINARY_DIR}/lib/libpbc.a
  )
target_link_libraries(PBC::PBC
  INTERFACE
  gmp
  )
add_dependencies(PBC::PBC pbc)
