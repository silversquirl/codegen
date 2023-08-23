//! General utilities for data-oriented design, etc
const std = @import("std");

pub fn IndexedStore(comptime IndexEnum: type, comptime ValueType: type) type {
    const invalid = checkIndexType("Index", IndexEnum);
    return struct {
        items: []const Value,

        pub const Index = IndexEnum;
        pub const Value = ValueType;

        const Self = @This();

        pub fn deinit(store: Self, allocator: std.mem.Allocator) void {
            allocator.free(store.items);
        }

        pub fn get(store: Self, index: Index) Value {
            checkInvalid(index);
            return store.items[@intFromEnum(index)];
        }
        pub inline fn getPtr(store: Self, index: Index) *const Value {
            checkInvalid(index);
            return &store.items[@intFromEnum(index)];
        }

        pub inline fn count(store: Self) u32 {
            return @intCast(store.items.len);
        }

        inline fn checkInvalid(index: Index) void {
            if (comptime invalid) |v| {
                std.debug.assert(index != v);
            }
        }

        pub const Mutable = struct {
            items: std.ArrayListUnmanaged(Value) = .{},

            pub fn deinit(store: *Self.Mutable, allocator: std.mem.Allocator) void {
                store.items.deinit(allocator);
            }

            pub inline fn get(store: Self.Mutable, index: Index) Value {
                checkInvalid(index);
                return store.items.items[@intFromEnum(index)];
            }
            pub inline fn getPtr(store: Self.Mutable, index: Index) *Value {
                checkInvalid(index);
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

pub fn SectionIndexedStore(comptime BaseIndexEnum: type, comptime OffsetIndexEnum: type, comptime ValueType: type) type {
    const bi = @typeInfo(BaseIndexEnum);
    if (bi != .Enum) {
        @compileError("BaseIndex must be an enum type");
    }
    if (bi.Enum.is_exhaustive) {
        @compileError("BaseIndex must not be exhaustive");
    }
    if (bi.Enum.fields.len > 0) {
        @compileError("BaseIndex must have no fields");
    }

    const invalid = checkIndexType("OffsetIndex", OffsetIndexEnum);

    return struct {
        items: []const Value,

        pub const BaseIndex = BaseIndexEnum;
        pub const OffsetIndex = OffsetIndexEnum;
        pub const Value = ValueType;

        const Self = @This();

        pub fn deinit(store: Self, allocator: std.mem.Allocator) void {
            allocator.free(store.items);
        }

        pub fn get(store: Self, base: BaseIndex, offset: OffsetIndex) Value {
            checkInvalid(offset);
            const index = @intFromEnum(base) + @intFromEnum(offset);
            return store.items[index];
        }
        pub inline fn getPtr(store: Self, base: BaseIndex, offset: OffsetIndex) *const Value {
            checkInvalid(offset);
            const index = @intFromEnum(base) + @intFromEnum(offset);
            return &store.items[index];
        }

        pub inline fn count(store: Self) u32 {
            return @intCast(store.items.len);
        }

        inline fn checkInvalid(offset: OffsetIndex) void {
            if (comptime invalid) |v| {
                std.debug.assert(offset != v);
            }
        }

        pub const Mutable = struct {
            items: std.ArrayListUnmanaged(Value) = .{},

            pub fn deinit(store: *Self.Mutable, allocator: std.mem.Allocator) void {
                store.items.deinit(allocator);
            }

            pub inline fn get(store: Self.Mutable, base: BaseIndex, offset: OffsetIndex) Value {
                checkInvalid(offset);
                const index = @intFromEnum(base) + @intFromEnum(offset);
                return store.items.items[index];
            }
            pub inline fn getPtr(store: Self.Mutable, base: BaseIndex, offset: OffsetIndex) *Value {
                checkInvalid(offset);
                const index = @intFromEnum(base) + @intFromEnum(offset);
                return &store.items.items[index];
            }

            pub inline fn count(store: Self.Mutable) u32 {
                return @intCast(store.items.items.len);
            }

            pub fn append(store: *Self.Mutable, allocator: std.mem.Allocator, base: BaseIndex, value: Value) !OffsetIndex {
                const i = store.items.items.len;
                try store.items.append(allocator, value);
                const offset: OffsetIndex = @enumFromInt(i - @intFromEnum(base));
                checkInvalid(offset);
                return offset;
            }

            pub fn appendUndefined(store: *Self.Mutable, allocator: std.mem.Allocator, base: BaseIndex) !OffsetIndex {
                const i = store.items.items.len;
                _ = try store.items.addOne(allocator);
                const offset: OffsetIndex = @enumFromInt(i - @intFromEnum(base));
                checkInvalid(offset);
                return offset;
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

fn checkIndexType(comptime name: []const u8, comptime Index: type) ?Index {
    const ii = @typeInfo(Index);
    if (ii != .Enum) {
        @compileError(name ++ " must be an enum type");
    }
    if (ii.Enum.is_exhaustive) {
        @compileError(name ++ " must not be exhaustive");
    }
    if (ii.Enum.fields.len > 1) {
        @compileError(std.fmt.comptimePrint(name ++ " must have zero or one defined fields, has {}", .{ii.Enum.fields.len}));
    }
    if (ii.Enum.fields.len == 1 and ii.Enum.fields[0].value != std.math.maxInt(ii.Enum.tag_type)) {
        @compileError(name ++ "'s field must be the max value of its tag");
    }

    if (ii.Enum.fields.len == 0) {
        return null;
    } else {
        return @field(Index, ii.Enum.fields[0].name);
    }
}
