if (APPLE)
    cmake_minimum_required(VERSION 3.28)
else()
    cmake_minimum_required(VERSION 3.13.4)
endif()

project(APIExamples)

add_executable(example1 example1.c)
add_executable(example2 example2.c)
add_executable(example3 example3.c)
add_executable(example4 example4.c)
add_executable(example5 example5.c)
add_executable(example6 example6.c)
add_executable(example7 example7.c)
add_executable(example8 example8.c)
add_executable(example9 example9.c)
add_executable(example10 example10.c)

if(APPLE) # tested with Vanilla installer
find_library(Csound NAMES CsoundLib64 PATHS "/Applications/Csound" REQUIRED NO_DEFAULT_PATH)
else() # untested
find_library(Csound NAMES csound64 PATHS "/usr/local/lib/")
endif()

target_link_libraries(example1 ${Csound})
target_link_libraries(example2 ${Csound})
target_link_libraries(example3 ${Csound})
target_link_libraries(example4 ${Csound})
target_link_libraries(example5 ${Csound})
target_link_libraries(example6 ${Csound})
target_link_libraries(example7 ${Csound})
target_link_libraries(example8 ${Csound})
target_link_libraries(example9 ${Csound})
target_link_libraries(example10 ${Csound})
