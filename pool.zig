pub fn Pool(comptime T: type) type {
    return struct {
        capacity: u32,
        free_count: u32,
        free_stack: [*]Slot,

        // Indexed by Slot:
        items: [*]T,

        pub const Slot = enum (u32) {
            invalid = std.math.maxInt(u32),
            _,
        };

        const Self = @This();

        pub fn init(gpa: std.mem.Allocator, capacity: u32) !Self {
            std.debug.assert(capacity > 0);

            const free_stack = try gpa.alloc(Slot, capacity);
            errdefer gpa.free(free_stack);

            for (1.., free_stack) |i, *slot| slot.* = @enumFromInt(capacity - i);

            const items = try gpa.alloc(T, capacity);

            return .{
                .capacity = capacity,
                .free_count = capacity,
                .free_stack = free_stack.ptr,
                .items = items.ptr,
            };
        }

        pub fn deinit(self: *Self, gpa: std.mem.Allocator) void {
            gpa.free(self.free_stack[0..self.capacity]);
            gpa.free(self.items[0..self.capacity]);
            self.free_stack = undefined;
            self.items = undefined;
            self.capacity = 0;
            self.free_count = 0;
        }

        pub fn alloc(self: *Self) Slot {
            if (self.free_count == 0) return .invalid;

            const stack_index = self.free_count - 1;
            self.free_count = stack_index;

            const id = self.free_stack[stack_index];
            std.debug.assert(@intFromEnum(id) >= 0);
            std.debug.assert(@intFromEnum(id) < self.capacity);
            return id;
        }

        pub fn free(self: *Self, slot: Slot) void {
            std.debug.assert(self.free_count < self.capacity);
            if (builtin.mode == .Debug) {
                // debug check against double-free
                for (self.free_stack[0..self.free_count]) |free_slot| {
                    std.debug.assert(free_slot != slot);
                }
            }
            self.free_stack[self.free_count] = slot;
            self.free_count += 1;
        }

        pub fn get(self: Self, slot: Slot) T {
            std.debug.assert(@intFromEnum(slot) < self.capacity);
            return self.items[@intFromEnum(slot)];
        }

        pub fn set(self: Self, slot: Slot, v: T) void {
            std.debug.assert(@intFromEnum(slot) < self.capacity);
            self.items[@intFromEnum(slot)] = v;
        }

        pub fn get_ptr(self: *Self, slot: Slot) *T {
            std.debug.assert(@intFromEnum(slot) < self.capacity);
            return &self.items[@intFromEnum(slot)];
        }
    };
}

const builtin = @import("builtin");
const std = @import("std");
