pub const Font_Tex_Desc = extern struct {
    min_filter: sg.Filter = .DEFAULT,
    mag_filter: sg.Filter = .DEFAULT,
};

const VS_Params = extern struct {
    disp_size: ig.Vec2 = .zeroes,
    __reserved: [8]u8 = .{ 0 } ** 8,
};

pub const Image_Desc = extern struct {
    image: sg.Image = .{},
    sampler: sg.Sampler = .{},
};

pub const Image = Image_Pool.Slot;
const Image_Pool = Pool(Image_Data);
const Image_Data = struct {
    id: Image = .invalid,
    image: sg.Image = .{},
    sampler: sg.Sampler = .{},
    pip: sg.Pipeline = .{},    // this will either be s.def_pip or s.pip_unfilterable
};

pub const Desc = struct {
    max_vertices: u32 = 65536,
    max_indices: u32 = 65536,
    image_pool_size: u32 = 256,
    color_format: sg.PixelFormat = .DEFAULT,
    depth_format: sg.PixelFormat = .DEFAULT,
    sample_count: i32 = 0,
    ini_filename: [*c]const u8 = null,
    no_default_font: bool = false,
    disable_paste_override: bool = false,
    disable_set_mouse_cursor: bool = false,
    disable_windows_resize_from_edges: bool = false,
    write_alpha_channel: bool = false,
    allocator: std.mem.Allocator,
};

pub const State = struct {
    desc: Desc,
    cur_dpi_scale: f32 = 1,
    vbuf: sg.Buffer = .{},
    ibuf: sg.Buffer = .{},
    font_img: sg.Image = .{},
    font_smp: sg.Sampler = .{},
    default_font: Image = .invalid,
    def_img: sg.Image = .{},       // used as default image for user images
    def_smp: sg.Sampler = .{},     // used as default sampler for user images
    def_shd: sg.Shader = .{},
    def_pip: sg.Pipeline = .{},
    shd_unfilterable: sg.Shader = .{},
    pip_unfilterable: sg.Pipeline = .{},
    vertices: sg.Range = .{},
    indices: sg.Range = .{},
    image_pool: Image_Pool,

    pub fn init(desc: Desc) !State {
        const vertices = try desc.allocator.alloc(ig.Draw_Vert, desc.max_vertices);
        errdefer desc.allocator.free(vertices);

        const indices = try desc.allocator.alloc(ig.Draw_Idx, desc.max_indices);
        errdefer desc.allocator.free(indices);

        var s: State = .{
            .desc = desc,
            .cur_dpi_scale = 1,
            .vertices = sg.asRange(vertices),
            .indices = sg.asRange(indices),
            .image_pool = try Image_Pool.init(desc.allocator, desc.image_pool_size),
        };
        @memset(s.image_pool.items[0..s.image_pool.capacity], .{});

        // initialize Dear ImGui
        _ = ig.create_context(.{});
        ig.style_colors_dark(.{});
        const io = ig.get_io();
        if (!s.desc.no_default_font) {
            _ = io.fonts.?.add_font_default(.{});
        }
        io.ini_filename = s.desc.ini_filename;
        io.config_mac_osxbehaviors = builtin.os.tag.isDarwin();
        io.backend_flags.renderer_has_vtx_offset = true;
        if (!s.desc.disable_set_mouse_cursor) {
            io.backend_flags.has_mouse_cursors = true;
        }
        const pio = ig.get_platform_io();
        pio.platform__set_clipboard_text_fn = set_clipboard;
        pio.platform__get_clipboard_text_fn = get_clipboard;
        io.config_windows_resize_from_edges = !s.desc.disable_windows_resize_from_edges;

        // create sokol-gfx resources
        sg.pushDebugGroup("sokol-imgui");

        // shader object for using the embedded shader source (or bytecode)
        var shd_desc = simgui_shader.simguiShaderDesc(sg.queryBackend());
        shd_desc.label = "sokol-imgui-shader";
        s.def_shd = sg.makeShader(shd_desc);

        // pipeline object for imgui rendering
        var pip_desc: sg.PipelineDesc = .{
            .label = "sokol-imgui-pipeline"
        };
        pip_desc.layout.buffers[0].stride = @sizeOf(ig.Draw_Vert);
        pip_desc.layout.attrs[0] = .{
            .offset = @offsetOf(ig.Draw_Vert, "pos"),
            .format = .FLOAT2,
        };
        pip_desc.layout.attrs[1] = .{
            .offset = @offsetOf(ig.Draw_Vert, "uv"),
            .format = .FLOAT2,
        };
        pip_desc.layout.attrs[2] = .{
            .offset = @offsetOf(ig.Draw_Vert, "col"),
            .format = .UBYTE4N,
        };
        pip_desc.shader = s.def_shd;
        pip_desc.index_type = .UINT16;
        pip_desc.sample_count = s.desc.sample_count;
        pip_desc.depth.pixel_format = s.desc.depth_format;
        pip_desc.colors[0] = .{
            .pixel_format = s.desc.color_format,
            .write_mask = if (s.desc.write_alpha_channel) .RGBA else .RGB,
            .blend = .{
                .enabled = true,
                .src_factor_rgb = .SRC_ALPHA,
                .dst_factor_rgb = .ONE_MINUS_SRC_ALPHA,
                .src_factor_alpha = if (s.desc.write_alpha_channel) .ONE else .DEFAULT,
                .dst_factor_alpha = if (s.desc.write_alpha_channel) .ONE else .DEFAULT,
            },
        };
        s.def_pip = sg.makePipeline(pip_desc);

        // create a unfilterable/nonfiltering variants of the shader and pipeline
        shd_desc.fs.images[0].sample_type = .UNFILTERABLE_FLOAT;
        shd_desc.fs.samplers[0].sampler_type = .NONFILTERING;
        shd_desc.label = "sokol-imgui-shader-unfilterable";
        s.shd_unfilterable = sg.makeShader(shd_desc);
        pip_desc.shader = s.shd_unfilterable;
        pip_desc.label = "sokol-imgui-pipeline-unfilterable";
        s.pip_unfilterable = sg.makePipeline(pip_desc);

        s.vbuf = sg.makeBuffer(.{
            .usage = .STREAM,
            .size = s.vertices.size,
            .label = "sokol-imgui-vertices",
        });

        s.ibuf = sg.makeBuffer(.{
            .type = .INDEXBUFFER,
            .usage = .STREAM,
            .size = s.indices.size,
            .label = "sokol-imgui-indices",
        });

        // a default user-image sampler
        s.def_smp = sg.makeSampler(.{
            .min_filter = .NEAREST,
            .mag_filter = .NEAREST,
            .wrap_u = .CLAMP_TO_EDGE,
            .wrap_v = .CLAMP_TO_EDGE,
            .label = "sokol-imgui-default-sampler",
        });

        // a default user image
        const def_pixels: [64]u32 = .{ 0xFFFF_FFFF } ** 64;
        var def_img_desc: sg.ImageDesc = .{
            .width = 8,
            .height = 8,
            .pixel_format = .RGBA8,
            .label = "sokol-imgui-default-image",
        };
        def_img_desc.data.subimage[0][0] = .{
            .ptr = &def_pixels,
            .size = @sizeOf(@TypeOf(def_pixels)),
        };
        s.def_img = sg.makeImage(def_img_desc);

        // default font texture
        if (!s.desc.no_default_font) {
            s.create_fonts_texture(.{});
        }

        sg.popDebugGroup();

        return s;
    }

    pub const Frame_Desc = extern struct {
        width: i32 = 0,
        height: i32 = 0,
        delta_time: f64 = 0.0,
        dpi_scale: f32 = 1.0,
    };
    pub fn new_frame(s: *State, desc: Frame_Desc) void {
        std.debug.assert(desc.width > 0);
        std.debug.assert(desc.height > 0);
        s.cur_dpi_scale = desc.dpi_scale;
        const io = ig.get_io();
        if (!io.fonts.?.tex_ready) {
            s.destroy_fonts_texture();
            s.create_fonts_texture(.{});
        }
        io.display_size.x = @floatFromInt(desc.width);
        io.display_size.y = @floatFromInt(desc.height);
        io.display_size.x /= s.cur_dpi_scale;
        io.display_size.y /= s.cur_dpi_scale;
        io.delta_time = @floatCast(desc.delta_time);
        if (io.want_text_input and !sapp.keyboardShown()) {
            sapp.showKeyboard(true);
        } else if (!io.want_text_input and sapp.keyboardShown()) {
            sapp.showKeyboard(false);
        }
        if (!s.desc.disable_set_mouse_cursor) {
            switch (ig.get_mouse_cursor()) {
                .arrow => sapp.setMouseCursor(.ARROW),
                .text_input => sapp.setMouseCursor(.IBEAM),
                .resize_all => sapp.setMouseCursor(.RESIZE_ALL),
                .resize_ns => sapp.setMouseCursor(.RESIZE_NS),
                .resize_ew => sapp.setMouseCursor(.RESIZE_EW),
                .resize_nesw => sapp.setMouseCursor(.RESIZE_NESW),
                .resize_nwse => sapp.setMouseCursor(.RESIZE_NWSE),
                .hand => sapp.setMouseCursor(.POINTING_HAND),
                .not_allowed => sapp.setMouseCursor(.NOT_ALLOWED),
                else => {},
            }
        }
        ig.new_frame();
    }

    pub fn render(s: *State) void {
        ig.render();
        const draw_data = ig.get_draw_data() orelse return;
        const io = ig.get_io();
        if (draw_data.cmd_lists_count == 0) return;

        // copy vertices and indices into an intermediate buffer so that
        // they can be updated with a single sg_update_buffer() call each
        // (sg_append_buffer() has performance problems on some GL platforms),
        // also keep track of valid number of command lists in case of a
        // buffer overflow
        var all_vtx_count: usize = 0;
        var all_idx_count: usize = 0;
        for (draw_data.cmd_lists.items()[0..@intCast(draw_data.cmd_lists_count)]) |cl| {
            const vtx_count = cl.vtx_buffer.size;
            const idx_count = cl.idx_buffer.size;

            // check for buffer overflow
            const vtx_bytes = (all_vtx_count + vtx_count) * @sizeOf(ig.Draw_Vert);
            const idx_bytes = (all_idx_count + idx_count) * @sizeOf(ig.Draw_Idx);
            if (vtx_bytes > s.vertices.size or idx_bytes > s.indices.size) break;

            // copy vertices and indices into common buffers
            if (vtx_count > 0) {
                const src = cl.vtx_buffer.items();
                var dst_bytes: [*]u8 = @ptrCast(@constCast(s.vertices.ptr));
                dst_bytes += all_vtx_count * @sizeOf(ig.Draw_Vert);
                const dst: [*]ig.Draw_Vert = @alignCast(@ptrCast(dst_bytes));
                @memcpy(dst, src);
            }
            if (idx_count > 0) {
                const src = cl.idx_buffer.items();
                var dst_bytes: [*]u8 = @ptrCast(@constCast(s.indices.ptr));
                dst_bytes += all_idx_count * @sizeOf(ig.Draw_Idx);
                const dst: [*]ig.Draw_Idx = @alignCast(@ptrCast(dst_bytes));
                @memcpy(dst, src);
            }
            all_vtx_count += vtx_count;
            all_idx_count += idx_count;
        }

        // update the sokol-gfx vertex- and index-buffer
        sg.pushDebugGroup("sokol-imgui");
        if (all_vtx_count > 0) {
            sg.updateBuffer(s.vbuf, .{
                .ptr = s.vertices.ptr,
                .size = all_vtx_count * @sizeOf(ig.Draw_Vert),
            });
        }
        if (all_idx_count > 0) {
            sg.updateBuffer(s.ibuf, .{
                .ptr = s.indices.ptr,
                .size = all_idx_count * @sizeOf(ig.Draw_Idx),
            });
        }

        // render the ImGui command list
        const dpi_scale: f32 = s.cur_dpi_scale;
        const fb_width: i32 = @intFromFloat(io.display_size.x * dpi_scale);
        const fb_height: i32 = @intFromFloat(io.display_size.y * dpi_scale);
        sg.applyViewport(0, 0, fb_width, fb_height, true);
        sg.applyScissorRect(0, 0, fb_width, fb_height, true);

        sg.applyPipeline(s.def_pip);
        var vs_params: VS_Params = .{
            .disp_size = .{
                .x = io.display_size.x,
                .y = io.display_size.y,
            },
        };
        sg.applyUniforms(.VS, 0, sg.asRange(&vs_params));
        var bind: sg.Bindings = .{};
        bind.vertex_buffers[0] = s.vbuf;
        bind.index_buffer = s.ibuf;
        var tex_id = io.fonts.?.tex_id;
        _ = bind_image_sampler(s, &bind, tex_id);
        var vb_offset: i32 = 0;
        var ib_offset: i32 = 0;
        for (draw_data.cmd_lists.items()[0..@intCast(draw_data.cmd_lists_count)]) |cl| {
            bind.vertex_buffer_offsets[0] = vb_offset;
            bind.index_buffer_offset = ib_offset;
            sg.applyBindings(bind);

            var vtx_offset: u32 = 0;
            for (cl.cmd_buffer.items()) |*cmd| {
                if (cmd.user_callback) |callback| {
                    // User callback, registered via ImDrawList::AddCallback()
                    // (ImDrawCallback_ResetRenderState is a special callback value used by the user to request the renderer to reset render state.)
                    if (@intFromPtr(callback) == @intFromPtr(ig.c.ImDrawCallback_ResetRenderState)) continue;
                    callback(cl, cmd);
                    // need to re-apply all state after calling a user callback
                    sg.resetStateCache();
                    sg.applyViewport(0, 0, fb_width, fb_height, true);
                    sg.applyPipeline(s.def_pip);
                    sg.applyUniforms(.VS, 0, sg.asRange(&vs_params));
                    sg.applyBindings(bind);
                } else {
                    if ((tex_id != cmd.texture_id) or (vtx_offset != cmd.vtx_offset)) {
                        tex_id = cmd.texture_id;
                        vtx_offset = cmd.vtx_offset;
                        if (bind_image_sampler(s, &bind, tex_id)) |img| {
                            sg.applyPipeline(img.pip);
                        } else {
                            sg.applyPipeline(s.def_pip);
                        }
                        sg.applyUniforms(.VS, 0, sg.asRange(&vs_params));
                        bind.vertex_buffer_offsets[0] = vb_offset + @as(i32, @intCast(cmd.vtx_offset * @sizeOf(ig.Draw_Vert)));
                        sg.applyBindings(bind);
                    }
                    const scissor_x: i32 = @intFromFloat(cmd.clip_rect.x * dpi_scale);
                    const scissor_y: i32 = @intFromFloat(cmd.clip_rect.y * dpi_scale);
                    const scissor_w: i32 = @intFromFloat((cmd.clip_rect.z - cmd.clip_rect.x) * dpi_scale);
                    const scissor_h: i32 = @intFromFloat((cmd.clip_rect.w - cmd.clip_rect.y) * dpi_scale);
                    sg.applyScissorRect(scissor_x, scissor_y, scissor_w, scissor_h, true);
                    sg.draw(cmd.idx_offset, cmd.elem_count, 1);
                }
            }
            vb_offset += @intCast(cl.vtx_buffer.size_in_bytes());
            ib_offset += @intCast(cl.idx_buffer.size_in_bytes());
        }
        sg.applyViewport(0, 0, fb_width, fb_height, true);
        sg.applyScissorRect(0, 0, fb_width, fb_height, true);
        sg.popDebugGroup();
    }





    fn add_focus_event(focus: bool) void {
        ig.get_io().add_focus_event(focus);
    }

    fn add_mouse_pos_event(x: f32, y: f32) void {
        const io = ig.get_io();
        io.add_mouse_source_event(.mouse);
        io.add_mouse_pos_event(x, y);
    }

    fn add_touch_pos_event(x: f32, y: f32) void {
        const io = ig.get_io();
        io.add_mouse_source_event(.touch_screen);
        io.add_mouse_pos_event(x, y);
    }

    fn add_mouse_button_event(mouse_button: i32, down: bool) void {
        const io = ig.get_io();
        io.add_mouse_source_event(.mouse);
        io.add_mouse_button_event(mouse_button, down);
    }

    fn add_mouse_wheel_event(wheel_x: f32, wheel_y: f32) void {
        const io = ig.get_io();
        io.add_mouse_source_event(.mouse);
        io.add_mouse_wheel_event(wheel_x, wheel_y);
    }

    fn add_key_event(imgui_key: ig.Key, down: bool) void {
        ig.get_io().add_key_event(imgui_key, down);
    }

    fn add_input_character(c: u32) void {
        ig.get_io().add_input_character(c);
    }

    fn add_input_characters_utf8(c: [:0]const u8) void {
        ig.get_io().add_input_characters_utf8(c);
    }

    fn add_touch_button_event(mouse_button: i32, down: bool) void {
        const io = ig.get_io();
        io.add_mouse_source_event(.touch_screen);
        io.add_mouse_button_event(mouse_button, down);
    }

    pub fn handle_event(s: *State, ev: sapp.Event) bool {
        const dpi_scale = s.cur_dpi_scale;
        const io = ig.get_io();
        switch (ev.type) {
            .FOCUSED => {
                add_focus_event(true);
            },
            .UNFOCUSED => {
                add_focus_event(false);
            },
            .MOUSE_DOWN => {
                add_mouse_pos_event(ev.mouse_x / dpi_scale, ev.mouse_y / dpi_scale);
                add_mouse_button_event(@intFromEnum(ev.mouse_button), true);
                update_modifiers(ev.modifiers);
            },
            .MOUSE_UP => {
                add_mouse_pos_event(ev.mouse_x / dpi_scale, ev.mouse_y / dpi_scale);
                add_mouse_button_event(@intFromEnum(ev.mouse_button), false);
                update_modifiers(ev.modifiers);
            },
            .MOUSE_MOVE => {
                add_mouse_pos_event(ev.mouse_x / dpi_scale, ev.mouse_y / dpi_scale);
            },
            .MOUSE_SCROLL => {
                add_mouse_wheel_event(ev.scroll_x, ev.scroll_y);
            },
            .TOUCHES_BEGAN => {
                add_touch_pos_event(ev.touches[0].pos_x / dpi_scale, ev.touches[0].pos_y / dpi_scale);
                add_touch_button_event(0, true);
            },
            .TOUCHES_MOVED => {
                add_touch_pos_event(ev.touches[0].pos_x / dpi_scale, ev.touches[0].pos_y / dpi_scale);
            },
            .TOUCHES_ENDED => {
                add_touch_pos_event(ev.touches[0].pos_x / dpi_scale, ev.touches[0].pos_y / dpi_scale);
                add_touch_button_event(0, false);
            },
            .TOUCHES_CANCELLED => {
                add_touch_button_event(0, false);
            },
            .KEY_DOWN => {
                update_modifiers(ev.modifiers);
                // intercept Ctrl-V, this is handled via EVENTTYPE_CLIPBOARD_PASTED
                if (s.desc.disable_paste_override or !is_ctrl(ev.modifiers) or ev.key_code != .V) {
                    // on web platform, don't forward Ctrl-X, Ctrl-V to the browser
                    if (is_ctrl(ev.modifiers) and (ev.key_code == .X)) {
                        sapp.consumeEvent();
                    }
                    if (is_ctrl(ev.modifiers) and (ev.key_code == .C)) {
                        sapp.consumeEvent();
                    }
                    // it's ok to add ImGuiKey_None key events
                    add_key_event(map_keycode(ev.key_code), true);
                }
            },
            .KEY_UP => {
                update_modifiers(ev.modifiers);
                // intercept Ctrl-V, this is handled via EVENTTYPE_CLIPBOARD_PASTED
                if (s.desc.disable_paste_override or !is_ctrl(ev.modifiers) or ev.key_code != .V) {
                    // on web platform, don't forward Ctrl-X, Ctrl-V to the browser
                    if (is_ctrl(ev.modifiers) and (ev.key_code == .X)) {
                        sapp.consumeEvent();
                    }
                    if (is_ctrl(ev.modifiers) and (ev.key_code == .C)) {
                        sapp.consumeEvent();
                    }
                    // it's ok to add ImGuiKey_None key events
                    add_key_event(map_keycode(ev.key_code), false);
                }
            },
            .CHAR => {
                // on some platforms, special keys may be reported as
                // characters, which may confuse some ImGui widgets,
                // drop those, also don't forward characters if some
                // modifiers have been pressed
                update_modifiers(ev.modifiers);
                if ((ev.char_code >= 32) and
                    (ev.char_code != 127) and
                    (0 == (ev.modifiers & (sapp.modifier_alt|sapp.modifier_ctrl|sapp.modifier_super))))
                {
                    add_input_character(ev.char_code);
                }
            },
            .CLIPBOARD_PASTED => {
                // simulate a Ctrl-V key down/up
                if (!s.desc.disable_paste_override) {
                    add_key_event(copypaste_modifier(), true);
                    add_key_event(.v, true);
                    add_key_event(.v, false);
                    add_key_event(copypaste_modifier(), false);
                }
            },
            else => {},
        }
        return io.want_capture_keyboard or io.want_capture_mouse;
    }

    pub fn deinit(s: *State) void {
        ig.destroy_context(.{});
        // NOTE: it's valid to call the destroy funcs with SG_INVALID_ID
        sg.destroyPipeline(s.pip_unfilterable);
        sg.destroyShader(s.shd_unfilterable);
        sg.destroyPipeline(s.def_pip);
        sg.destroyShader(s.def_shd);
        sg.destroySampler(s.font_smp);
        sg.destroyImage(s.font_img);
        sg.destroySampler(s.def_smp);
        sg.destroyImage(s.def_img);
        sg.destroyBuffer(s.ibuf);
        sg.destroyBuffer(s.vbuf);
        sg.popDebugGroup();
        sg.pushDebugGroup("sokol-imgui");
        s.destroy_all_images();
        s.image_pool.deinit(s.desc.allocator);

        const verts: [*]const ig.Draw_Vert = @alignCast(@ptrCast(s.vertices.ptr.?));
        const indices: [*]const ig.Draw_Idx = @alignCast(@ptrCast(s.indices.ptr.?));

        s.desc.allocator.free(verts[0..(s.vertices.size / @sizeOf(ig.Draw_Vert))]);
        s.desc.allocator.free(indices[0..(s.indices.size / @sizeOf(ig.Draw_Idx))]);
    }

    pub fn create_fonts_texture(s: *State, desc: Font_Tex_Desc) void {
        std.debug.assert(sg.invalid_id == s.font_smp.id);
        std.debug.assert(sg.invalid_id == s.font_img.id);
        std.debug.assert(s.default_font == .invalid);

        const io = ig.get_io();

        // a default font sampler
        s.font_smp = sg.makeSampler(.{
            .wrap_u = .CLAMP_TO_EDGE,
            .wrap_v = .CLAMP_TO_EDGE,
            .min_filter = desc.min_filter,
            .mag_filter = desc.mag_filter,
            .label = "sokol-imgui-font-sampler",
        });

        var font_pixels: [*]u8 = undefined;
        var font_width: i32 = undefined;
        var font_height: i32 = undefined;
        var bytes_per_pixel: i32 = undefined;
        io.fonts.?.get_tex_data_as_rgba32(&font_pixels, &font_width, &font_height, .{ .out_bytes_per_pixel = &bytes_per_pixel });
        var font_img_desc: sg.ImageDesc = .{
            .width = font_width,
            .height = font_height,
            .pixel_format = .RGBA8,
            .label = "sokol-imgui-font-image",
        };
        font_img_desc.data.subimage[0][0] = .{
            .ptr = font_pixels,
            .size = @intCast(font_width * font_height * @as(i32, @sizeOf(u32))),
        };
        s.font_img = sg.makeImage(font_img_desc);

        s.default_font = s.make_image(.{
            .image = s.font_img,
            .sampler = s.font_smp,
        });
        io.fonts.?.tex_id = @ptrFromInt(@intFromEnum(s.default_font));
    }

    pub fn destroy_fonts_texture(s: *State) void {
        sg.destroySampler(s.font_smp);
        sg.destroyImage(s.font_img);
        s.destroy_image(s.default_font);
        s.font_smp.id = sg.invalid_id;
        s.font_img.id = sg.invalid_id;
        s.default_font = .invalid;
    }

    fn bind_image_sampler(s: *State, bindings: *sg.Bindings, tex_id: ig.Texture_ID) ?*Image_Data {
        if (s.lookup_image(image_from_imgui_texture_id(tex_id))) |img| {
            bindings.fs.images[0] = img.image;
            bindings.fs.samplers[0] = img.sampler;
            return img;
        } else {
            bindings.fs.images[0] = s.def_img;
            bindings.fs.samplers[0] = s.def_smp;
            return null;
        }
    }

    pub fn make_image(s: *State, desc: Image_Desc) Image {
        const id = s.image_pool.alloc();
        if (id == .invalid) return id;

        const should_filter = sg.queryPixelformat(sg.queryImageDesc(desc.image).pixel_format).filter;

        s.image_pool.set(id, .{
            .id = id,
            .image = desc.image,
            .sampler = desc.sampler,
            .pip = if (should_filter) s.def_pip else s.pip_unfilterable,
        });

        return id;
    }
    pub fn destroy_image(s: *State, id: Image) void {
        if (id == .invalid) return;
        s.image_pool.set(id, .{});
        s.image_pool.free(id);
    }
    pub fn query_image_desc(s: *State, id: Image) Image_Desc {
        if (s.lookup_image(id)) |img| {
            return .{
                .image = img.image,
                .sampler = img.sampler,
            };
        }
        return .{};
    }

    fn destroy_all_images(s: *State) void {
        for (s.image_pool.items[0..s.image_pool.capacity]) |img| {
            if (img.id != .invalid) s.destroy_image(img.id);
        }
    }
    fn lookup_image(s: *State, id: Image) ?*Image_Data {
        return if (id == .invalid) null else s.image_pool.get_ptr(id);
    }
};

pub fn image_from_imgui_texture_id(im_texture_id: ig.Texture_ID) Image {
    return @enumFromInt(@intFromPtr(im_texture_id));
}
pub fn imgui_texture_id_from_image(id: Image) ig.Texture_ID {
    return @ptrFromInt(@intFromEnum(id));
}

fn is_ctrl(modifiers: u32) bool {
    if (builtin.os.tag.isDarwin()) {
        return 0 != (modifiers & sapp.modifier_super);
    } else {
        return 0 != (modifiers & sapp.modifier_ctrl);
    }
}

fn update_modifiers(mods: u32) void {
    const io = ig.get_io();
    io.add_key_event(ig.Key.none.with_ctrl(), (mods & sapp.modifier_ctrl) != 0);
    io.add_key_event(ig.Key.none.with_shift(), (mods & sapp.modifier_shift) != 0);
    io.add_key_event(ig.Key.none.with_alt(), (mods & sapp.modifier_alt) != 0);
    io.add_key_event(ig.Key.none.with_super(), (mods & sapp.modifier_super) != 0);
}

fn map_keycode(keycode: sapp.Keycode) ig.Key {
    return switch (keycode) {
        .SPACE => .space,
        .APOSTROPHE => .apostrophe,
        .COMMA => .comma,
        .MINUS => .minus,
        .PERIOD => .apostrophe,
        .SLASH => .slash,
        ._0 => .@"0",
        ._1 => .@"1",
        ._2 => .@"2",
        ._3 => .@"3",
        ._4 => .@"4",
        ._5 => .@"5",
        ._6 => .@"6",
        ._7 => .@"7",
        ._8 => .@"8",
        ._9 => .@"9",
        .SEMICOLON => .semicolon,
        .EQUAL => .equal,
        .A => .a,
        .B => .b,
        .C => .c,
        .D => .d,
        .E => .e,
        .F => .f,
        .G => .g,
        .H => .h,
        .I => .i,
        .J => .j,
        .K => .k,
        .L => .l,
        .M => .m,
        .N => .n,
        .O => .o,
        .P => .p,
        .Q => .q,
        .R => .r,
        .S => .s,
        .T => .t,
        .U => .u,
        .V => .v,
        .W => .w,
        .X => .x,
        .Y => .y,
        .Z => .z,
        .LEFT_BRACKET => .left_bracket,
        .BACKSLASH => .backslash,
        .RIGHT_BRACKET => .right_bracket,
        .GRAVE_ACCENT => .grave_accent,
        .ESCAPE => .escape,
        .ENTER => .enter,
        .TAB => .tab,
        .BACKSPACE => .backspace,
        .INSERT => .insert,
        .DELETE => .delete,
        .RIGHT => .right_arrow,
        .LEFT => .left_arrow,
        .DOWN => .down_arrow,
        .UP => .up_arrow,
        .PAGE_UP => .page_up,
        .PAGE_DOWN => .page_down,
        .HOME => .home,
        .END => .end,
        .CAPS_LOCK => .caps_lock,
        .SCROLL_LOCK => .scroll_lock,
        .NUM_LOCK => .num_lock,
        .PRINT_SCREEN => .print_screen,
        .PAUSE => .pause,
        .F1 => .f1,
        .F2 => .f2,
        .F3 => .f3,
        .F4 => .f4,
        .F5 => .f5,
        .F6 => .f6,
        .F7 => .f7,
        .F8 => .f8,
        .F9 => .f9,
        .F10 => .f10,
        .F11 => .f11,
        .F12 => .f12,
        .KP_0 => .keypad0,
        .KP_1 => .keypad1,
        .KP_2 => .keypad2,
        .KP_3 => .keypad3,
        .KP_4 => .keypad4,
        .KP_5 => .keypad5,
        .KP_6 => .keypad6,
        .KP_7 => .keypad7,
        .KP_8 => .keypad8,
        .KP_9 => .keypad9,
        .KP_DECIMAL => .keypad_decimal,
        .KP_DIVIDE => .keypad_divide,
        .KP_MULTIPLY => .keypad_multiply,
        .KP_SUBTRACT => .keypad_subtract,
        .KP_ADD => .keypad_add,
        .KP_ENTER => .keypad_enter,
        .KP_EQUAL => .keypad_equal,
        .LEFT_SHIFT => .left_shift,
        .LEFT_CONTROL => .left_ctrl,
        .LEFT_ALT => .left_alt,
        .LEFT_SUPER => .left_super,
        .RIGHT_SHIFT => .right_shift,
        .RIGHT_CONTROL => .right_ctrl,
        .RIGHT_ALT => .right_alt,
        .RIGHT_SUPER => .right_super,
        .MENU => .menu,
        else => .none,
    };
}

fn copypaste_modifier() ig.Key {
    return if (builtin.os.tag.isDarwin()) ig.Key.none.with_super() else ig.Key.none.with_ctrl();
}

fn set_clipboard(ctx: *ig.Context, text: [*:0]const u8) callconv(.C) void {
    _ = ctx;
    sapp.setClipboardString(std.mem.span(text));
}

fn get_clipboard(ctx: *ig.Context) callconv(.C) [*:0]const u8 {
    _ = ctx;
    return sapp.getClipboardString().ptr;
}

const Pool = @import("pool.zig").Pool;
const ig = @import("ig");
const sg = sokol.gfx;
const sapp = sokol.app;
const simgui_shader = @import("simgui_shader");
const sokol = @import("sokol");
const builtin = @import("builtin");
const std = @import("std");
