# Dear (Zig) Bindings: Sokol Backend

A port of [sokol_imgui.h](https://github.com/floooh/sokol/blob/master/util/sokol_imgui.h) to Zig, as a backend for [Dear (Zig) Bindings](https://github.com/bcrist/dear_zig_bindings).

Building requires `sokol-shdc` to compile the shaders.  If you already have it installed, you can use that version with `zig build -Dsokol-shdc=path/to/binary`.  Otherwise, `zig build` will attempt to download it from the [sokol-tools-bin](https://github.com/floooh/sokol-tools-bin) repository.