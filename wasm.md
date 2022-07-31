emcmake cmake -S . -B build_wasm -DSOUFFLE_USE_SQLITE=OFF -DSOUFFLE_USE_OPENMP=OFF -DSOUFFLE_USE_ZLIB=OFF -DSOUFFLE_USE_LIBFFI=OFF -DSOUFFLE_USE_CURSES=OFF -DSOUFFLE_ENABLE_TESTING=OFF -DSOUFFLE_TEST_EVALUATION=OFF


-Wno-error

-DCMAKE_BUILD_TYPE=MinSizeRel

Needed to go into src/CMakeLists.txt and remove -Werror from 


Is it actually using my flag?

cd /home/philip/Documents/prolog/souffle/build_wasm/src && /usr/bin/cmake -E cmake_link_script CMakeFiles/souffle.dir/link.txt --verbose=1
/home/philip/Documents/prolog/emsdk/upstream/emscripten/em++  -sMAIN_MODULE=2 -stdlib=libc++ -O3    -fuse-ld=lld -lc++abi @CMakeFiles/souffle.dir/objects1.rsp  -o souffle.js @CMakeFiles/souffle.dir/linklibs.rsp


Todo:
Support dlopen. MAIN_MODULE=2 somehow should work but maybe need -fpic on entire build?


