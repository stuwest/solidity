set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -DSTATICLIB")

aux_source_directory(analysis SRC_LIST)
aux_source_directory(ast SRC_LIST)
aux_source_directory(codegen SRC_LIST)
aux_source_directory(formal SRC_LIST)
aux_source_directory(interface SRC_LIST)
aux_source_directory(parsing SRC_LIST)
aux_source_directory(inlineasm SRC_LIST)
# Until we have a clear separation, libjulia has to be included here
aux_source_directory(../libjulia SRC_LIST)

set(EXECUTABLE solidity)

file(GLOB HEADERS "*/*.h" "../libjulia/backends/evm/*")

include_directories(BEFORE ..)

find_package(Z3 QUIET)
if (${Z3_FOUND})
  include_directories(${Z3_INCLUDE_DIR})
  add_definitions(-DHAVE_Z3)
  message("Z3 SMT solver FOUND.")
else()
  message("Z3 SMT solver NOT found.")
  list(REMOVE_ITEM SRC_LIST "formal/Z3Interface.cpp")
endif()

add_library(${EXECUTABLE} ${SRC_LIST} ${HEADERS})

if (${Z3_FOUND})
  target_link_libraries(${EXECUTABLE} ${Z3_LIBRARY})
endif()

target_link_libraries(${EXECUTABLE})

eth_use(${EXECUTABLE} REQUIRED Dev::soldevcore Solidity::solevmasm)

install( TARGETS ${EXECUTABLE} RUNTIME DESTINATION bin ARCHIVE DESTINATION lib LIBRARY DESTINATION lib )

