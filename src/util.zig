//! General utilities for data-oriented design, etc
const std = @import("std");

pub fn IndexedStore(comptime Value: type, comptime Index: type) type {
    const ii = @typeInfo(Index);
    if (ii != .Enum) {
        @compileError("Index must be an enum type");
    }
    if (ii.Enum.is_exhaustive) {
        @compileError("Index must not be exhaustive");
    }
    if (ii.Enum.fields.len > 1) {
        @compileError(std.fmt.comptimePrint("Index must have zero or one defined fields, has {}", .{ii.Enum.fields.len}));
    }
    if (ii.Enum.fields.len == 1 and ii.Enum.fields[0].value != std.math.maxInt(ii.Enum.tag_type)) {
        @compileError("Index's field must be the max value of its tag");
    }

    return struct {
        items: []const Value,

        const Self = @This();

        pub fn deinit(store: Self, allocator: std.mem.Allocator) void {
            allocator.free(store.items);
        }

        pub fn get(store: Self, index: Index) Value {
            return store.items[@intFromEnum(index)];
        }
        pub inline fn getPtr(store: Self, index: Index) *const Value {
            return &store.items[@intFromEnum(index)];
        }

        pub inline fn count(store: Self) u32 {
            return @intCast(store.items.len);
        }

        pub const Mutable = struct {
            items: std.ArrayListUnmanaged(Value) = .{},

            pub fn deinit(store: *Self.Mutable, allocator: std.mem.Allocator) void {
                store.items.deinit(allocator);
            }

            pub inline fn get(store: Self.Mutable, index: Index) Value {
                return store.items.items[@intFromEnum(index)];
            }
            pub inline fn getPtr(store: Self.Mutable, index: Index) *Value {
                return &store.items.items[@intFromEnum(index)];
            }

            pub inline fn count(store: Self.Mutable) u32 {
                return @intCast(store.items.items.len);
            }

            pub fn append(store: *Self.Mutable, allocator: std.mem.Allocator, value: Value) !Index {
                const i = store.items.items.len;
                try store.items.append(allocator, value);
                return @enumFromInt(i);
            }

            pub fn appendUndefined(store: *Self.Mutable, allocator: std.mem.Allocator) !Index {
                const i = store.items.items.len;
                _ = try store.items.addOne(allocator);
                return @enumFromInt(i);
            }

            pub fn popLast(store: *Self.Mutable) void {
                store.items.shrinkRetainingCapacity(store.count() - 1);
            }

            pub fn resize(store: *Self.Mutable, allocator: std.mem.Allocator, n: u32) !void {
                try store.items.resize(allocator, n);
            }

            pub fn toConst(store: *Self.Mutable, allocator: std.mem.Allocator) !Self {
                return .{ .items = try store.items.toOwnedSlice(allocator) };
            }
        };
    };
}
