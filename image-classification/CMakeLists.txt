PROJECT(img2bin)

# Select a default build configuration if none was chosen
IF(NOT CMAKE_BUILD_TYPE)
  SET(CMAKE_BUILD_TYPE "Release" CACHE STRING "Choose the type of build, options are: None (CMAKE_CXX_FLAGS or CMAKE_C_FLAGS used) Debug Release RelWithDebInfo MinSizeRel." FORCE)
ENDIF()

SET(GCC_COVERAGE_COMPILE_FLAGS "-std=c++17")
# SET(GCC_COVERAGE_LINK_FLAGS    "-lgcov")

SET(CMAKE_CXX_FLAGS  "${CMAKE_CXX_FLAGS} ${GCC_COVERAGE_COMPILE_FLAGS}")
# SET(CMAKE_EXE_LINKER_FLAGS  "${CMAKE_EXE_LINKER_FLAGS} ${GCC_COVERAGE_LINK_FLAGS}")

# ADD_EXECUTABLE(img2bin img2bin.cpp)
ADD_EXECUTABLE(optical optical.cpp match.cpp match.hpp constants.hpp matrix.hpp rectangle.hpp otsu.cpp otsu.hpp common.hpp common.cpp)

# FIND_PACKAGE(JPEG REQUIRED)
# IF(JPEG_FOUND)
#   INCLUDE_DIRECTORIES(${JPEG_INCLUDE_DIR})
#   TARGET_LINK_LIBRARIES(img2bin ${JPEG_LIBRARIES})
# ENDIF()
