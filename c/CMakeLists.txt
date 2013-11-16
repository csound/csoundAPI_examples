cmake_minimum_required(VERSION 2.8.3)
project (csound_api_examples)

# TODO - need to fix library and include path finding

if(APPLE)
  set(CS_LIB CsoundLib64)
else()
  set(CS_LIB csound64)
endif()

find_library(CS_LIB_PATH ${CS_LIB})

add_definitions(-DUSE_DOUBLE=1) 
include_directories(~/Library/Frameworks/CsoundLib64.framework/Headers)

function(make_example name src)
  add_executable(${name} ${src})
  target_link_libraries(${name} ${CS_LIB_PATH})
endfunction(make_example)

make_example(example1 example1.c)
make_example(example2 example2.c)
make_example(example3 example3.c)
make_example(example4 example4.c)
make_example(example5 example5.c)
make_example(example6 example6.c)
make_example(example7 example7.c)
