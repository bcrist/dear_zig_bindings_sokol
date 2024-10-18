pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const host_os = b.host.result.os.tag;

    const sokol = b.dependency("sokol", .{
        .target = target,
        .optimize = optimize,
    });

    const dear_zig_bindings = b.dependency("dear_zig_bindings", .{
        .target = target,
        .optimize = optimize,
        .naming = .snake,
        .validate_packed_structs = true,
    });

    var shdc_cmd: []const u8 = b.option([]const u8, "sokol-shdc", "Path or name of sokol-shdc binary.  If not provided, it will be downloaded from github.com/floooh/sokol-tools-bin") orelse "";

    if (shdc_cmd.len == 0) if (b.lazyDependency("sokol_tools_bin", .{})) |dep| {
        const shdc_cache_path = dep.path(
            if (host_os == .windows) "bin/win32/sokol-shdc.exe"
            else if (host_os == .linux) "bin/linux/sokol-shdc"
            else if (host_os.isDarwin() and b.host.result.cpu.arch == .aarch64) "bin/osx_arm64/sokol-shdc"
            else if (host_os.isDarwin()) "bin/osx/sokol-shdc"
            else @panic("Unsupported build host OS!"),
        ).getPath3(b, null);
        shdc_cmd = b.pathResolve(&.{ shdc_cache_path.root_dir.path.?, shdc_cache_path.sub_path });
    };

    if (shdc_cmd.len == 0) {
        shdc_cmd = "sokol-shdc";
    }

    const shdc = b.addSystemCommand(&.{ shdc_cmd, "-i" });
    shdc.addFileArg(b.path("simgui.glsl"));
    shdc.addArg("-o");
    const simgui_shader_zig = shdc.addOutputFileArg("simgui_shader.zig");
    shdc.addArgs(&.{
        "-l", "glsl410:hlsl4:metal_macos:metal_sim:wgsl",
        "-b",
        "-f", "sokol_zig",
    });

    const simgui_shader = b.createModule(.{
        .root_source_file = simgui_shader_zig,
    });
    simgui_shader.addImport("sokol", sokol.module("sokol"));

    const sokol_imgui = b.addModule("sokol_imgui", .{
        .root_source_file = b.path("sokol_imgui.zig"),
    });
    sokol_imgui.addImport("sokol", sokol.module("sokol"));
    sokol_imgui.addImport("ig", dear_zig_bindings.module("ig"));
    sokol_imgui.addImport("simgui_shader", simgui_shader);
}

const std = @import("std");
