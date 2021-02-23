include(FindPackageHandleStandardArgs)
include(ExternalProject)

# get an uppercase version of the build type, for extracting build_type specific flags
if(CMAKE_BUILD_TYPE)
  string(TOUPPER ${CMAKE_BUILD_TYPE} BUILD_TYPE_UC)
endif()

get_target_property(GMP_LIB_DIR GMP::gmp IMPORTED_DIRECTORY)

ExternalProject_Add(pbc
  PREFIX            ${CMAKE_CURRENT_BINARY_DIR}/external-pbc
  GIT_REPOSITORY    https://github.com/Vagabond/pbc
  GIT_TAG           ddd8ce61b203a692cbf5cb4bc14886b85e828e5f
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
                    CPPFLAGS=-I${GMP_INCLUDE_DIR}
                    LDFLAGS=-L${CMAKE_CURRENT_BINARY_DIR}/lib\ -L${GMP_LIB_DIR}
                    ${APPLE_SDKROOT_ENV}
  BUILD_COMMAND     ${CMAKE_BUILD_TOOL} -j ${APPLE_SDKROOT_ENV}
  BUILD_BYPRODUCTS  ${CMAKE_CURRENT_BINARY_DIR}/lib/libpbc.a
  INSTALL_COMMAND   ${CMAKE_BUILD_TOOL} install
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
  GMP::gmp
  )
add_dependencies(PBC::PBC pbc)
