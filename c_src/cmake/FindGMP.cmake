#[=======================================================================[.rst:
FindGMP
-------

Find the GNU MULTI-Precision Bignum (GMP) library.

This module is derived from [1]. It is modified to remove the GMPxx
requirement.

[1]: https://github.com/dune-project/dune-common/blob/b3ee0b735d1818d34fff16aed57656ab707df628/cmake/modules/FindGMP.cmake

Imported Targets
^^^^^^^^^^^^^^^^

This module provides the following imported targets, if found:

``GMP::gmp``
  Library target of the C library.

Result Variables
^^^^^^^^^^^^^^^^

This will define the following variables:

``GMP_FOUND``
  True if the GMP library and header were found.

Cache Variables
^^^^^^^^^^^^^^^

You may set the following variables to modify the behaviour of
this module:

``GMP_INCLUDE_DIR``
  The directory containing ``gmp.h``.
``GMP_LIB``
  The path to the gmp library.

#]=======================================================================]

# Add a feature summary for this package
include(FeatureSummary)
set_package_properties(GMP PROPERTIES
  DESCRIPTION "GNU multi-precision library"
  URL "https://gmplib.org"
  )

# Try finding the package with pkg-config
find_package(PkgConfig QUIET)
pkg_check_modules(PKG QUIET gmp gmpxx)

# Try to locate the libraries and their headers, using pkg-config hints
find_path(GMP_INCLUDE_DIR gmp.h HINTS ${PKG_gmp_INCLUDEDIR})
find_library(GMP_LIB gmp HINTS ${PKG_gmp_LIBDIR})

# Remove these variables from cache inspector
mark_as_advanced(GMP_INCLUDE_DIR GMP_LIB)

# Report if package was found
include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(GMP
  DEFAULT_MSG
  GMP_INCLUDE_DIR GMP_LIB
  )

# Set targets
if(GMP_FOUND)
  # C library
  if(NOT TARGET GMP::gmp)
    add_library(GMP::gmp UNKNOWN IMPORTED)
    set_target_properties(GMP::gmp PROPERTIES
      IMPORTED_LOCATION ${GMP_LIB}
      INTERFACE_INCLUDE_DIRECTORIES ${GMP_INCLUDE_DIR}
      )
  endif()
endif()
