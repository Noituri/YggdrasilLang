cmake -Thost=x64 -G "Visual Studio 16 2019" -A x64 -DCMAKE_INSTALL_PREFIX=Y:\Development\llvm -DLLVM_BUILD_LLVM_DYLIB=True -DLLVM_LINK_LLVM_DYLIB=True ../llvm