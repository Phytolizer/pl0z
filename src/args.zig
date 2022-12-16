const std = @import("std");

const IndexBuf = std.ArrayList(usize);

const PositionalInfo = struct {
    indices: IndexBuf.Slice,
    num_consumed: usize = 0,
};

pub const Error = error{
    UnknownLongopt,
    UnknownShortopt,
    PositionalArg,
    MissingOptionValue,
    PositionalArgShort,
    MissingOptionValueShort,
    MissingPositionalArg,
} || std.mem.Allocator.Error;

pub const Arg = struct {
    kind: Kind,
    short_name: u8 = 0,
    long_name: []const u8 = "",
    help: []const u8 = "",

    const Kind = union(enum) {
        pos: *[]const u8,
        opt: *?[]const u8,
        flag: *bool,
    };

    const Info = struct {
        args: []const []const u8,
        offset: usize = 0,
    };

    pub fn pos(name: []const u8, help: []const u8, p_pos: *[]const u8) @This() {
        return .{
            .kind = .{ .pos = p_pos },
            .long_name = name,
            .help = help,
        };
    }
};

fn find(
    comptime T: type,
    slice: []const T,
    state: anytype,
    pred: fn (*const T, @TypeOf(state)) bool,
) ?*const T {
    for (slice) |*item| {
        if (pred(item, state)) return item;
    }
    return null;
}

pub const Parser = struct {
    a: std.mem.Allocator,
    args: []const Arg,
    name: []const u8,
    help: []const u8,
    extra: std.ArrayList([]const u8),

    pub fn init(
        a: std.mem.Allocator,
        name: []const u8,
        help: []const u8,
        args: []const Arg,
    ) @This() {
        return .{
            .a = a,
            .args = args,
            .name = name,
            .help = help,
            .extra = std.ArrayList([]const u8).init(a),
        };
    }

    fn parseSingleLongopt(self: *@This(), arg: []const u8, info: Arg.Info) Error!usize {
        const eq_pos = std.mem.indexOfScalar(u8, arg, '=');
        const name = if (eq_pos) |pos|
            arg[0..pos]
        else
            arg;

        const found_arg = find(
            Arg,
            self.args,
            name,
            struct {
                fn longname_eq(a: *const Arg, n: []const u8) bool {
                    return std.mem.eql(u8, a.long_name, n);
                }
            }.longname_eq,
        ) orelse return error.UnknownLongopt;

        switch (found_arg.kind) {
            .pos => return error.PositionalArg,
            .flag => |value| {
                value.* = true;
                return info.offset + 1;
            },
            .opt => |value| {
                if (eq_pos) |pos| {
                    const after_eq_pos = pos + 1;
                    value.* = arg[after_eq_pos..];
                    return info.offset + 1;
                } else if (info.offset + 1 < info.args.len) {
                    value.* = info.args[info.offset + 1];
                    return info.offset + 2;
                } else {
                    return error.MissingOptionValue;
                }
            },
        }
    }

    fn parse_shortopts(
        self: *@This(),
        arg: []const u8,
        info: Arg.Info,
        out_failure: *u8,
    ) Error!usize {
        for (arg) |shortname, i| {
            const found_arg = find(
                Arg,
                self.args,
                shortname,
                struct {
                    fn shortname_eq(a: *const Arg, n: u8) bool {
                        return a.short_name == n;
                    }
                }.shortname_eq,
            ) orelse {
                out_failure.* = shortname;
                return error.UnknownShortopt;
            };

            switch (found_arg.kind) {
                .pos => {
                    out_failure.* = shortname;
                    return error.PositionalArgShort;
                },
                .flag => |value| value.* = true,
                .opt => |value| {
                    if (i + 1 < arg.len) {
                        value.* = arg[i + 1 ..];
                        return info.offset + 1;
                    } else if (info.offset + 1 < info.args.len) {
                        value.* = info.args[info.offset + 1];
                        return info.offset + 2;
                    } else {
                        out_failure.* = shortname;
                        return error.MissingOptionValueShort;
                    }
                },
            }
        }

        return info.offset + 1;
    }

    fn parse_positional(
        self: *@This(),
        arg: []const u8,
        info: Arg.Info,
        positionals: *PositionalInfo,
    ) Error!usize {
        if (positionals.num_consumed >= positionals.indices.len) {
            try self.extra.append(arg);
            return info.offset + 1;
        }

        const index = positionals.indices[positionals.num_consumed];
        positionals.num_consumed += 1;
        self.args[index].kind.pos.* = arg;
        return info.offset + 1;
    }

    fn parse_arg(
        self: *@This(),
        arg: []const u8,
        info: Arg.Info,
        positionals: *PositionalInfo,
        out_err: *?[]u8,
    ) Error!usize {
        // skip empty bare args (empty option values are handled elsewhere)
        if (arg.len == 0) return info.offset + 1;
        errdefer |e| {
            switch (e) {
                error.UnknownLongopt => {
                    out_err.* = std.fmt.allocPrint(
                        self.a,
                        "unknown long option: --{s}",
                        .{arg},
                    ) catch unreachable;
                },
                error.PositionalArg => {
                    out_err.* = std.fmt.allocPrint(
                        self.a,
                        "{s} names a positional arg",
                        .{arg},
                    ) catch unreachable;
                },
                error.MissingOptionValue => {
                    out_err.* = std.fmt.allocPrint(
                        self.a,
                        "option --{s} requires an argument",
                        .{arg},
                    ) catch unreachable;
                },
                else => {},
            }
        }

        if (arg[0] == '-') {
            if (arg.len > 1 and arg[1] == '-') {
                return try self.parseSingleLongopt(arg[2..], info);
            }

            var failure: u8 = 0;
            errdefer |e| {
                switch (e) {
                    error.UnknownShortopt => {
                        out_err.* = std.fmt.allocPrint(
                            self.a,
                            "unknown short option: -{c}",
                            .{failure},
                        ) catch unreachable;
                    },
                    error.PositionalArgShort => {
                        out_err.* = std.fmt.allocPrint(
                            self.a,
                            "{c} names a positional arg",
                            .{failure},
                        ) catch unreachable;
                    },
                    error.MissingOptionValueShort => {
                        out_err.* = std.fmt.allocPrint(
                            self.a,
                            "option -{c} requires a value",
                            .{failure},
                        ) catch unreachable;
                    },
                    else => {},
                }
            }
            return try self.parse_shortopts(arg[1..], info, &failure);
        }

        return try self.parse_positional(arg, info, positionals);
    }

    fn generateMissingPositionalMessage(
        a: std.mem.Allocator,
        args: []const Arg,
        positionals: PositionalInfo,
    ) Error![]u8 {
        std.debug.assert(positionals.indices.len > 0);

        var arena = std.heap.ArenaAllocator.init(a);
        defer arena.deinit();
        var aa = arena.allocator();
        var buf = std.ArrayList([]const u8).init(aa);
        try buf.append("missing positional arguments: ");
        for (positionals.indices[positionals.num_consumed..]) |index, i| {
            const arg = args[index];
            try buf.append(
                try std.fmt.allocPrint(aa, "'{s}'", .{arg.long_name}),
            );
            if (i > 0) try buf.append(", ");
        }
        return try std.mem.concat(a, u8, buf.items);
    }

    pub fn parse(
        self: *@This(),
        args: []const []const u8,
        out_err: *?[]u8,
    ) Error!void {
        var info = Arg.Info{ .args = args };
        var positionals = PositionalInfo{
            .indices = try countPositionals(self.a, self.args),
        };
        defer self.a.free(positionals.indices);

        while (info.offset < args.len) {
            const arg = args[info.offset];
            info.offset = try self.parse_arg(arg, info, &positionals, out_err);
        }

        if (positionals.num_consumed < positionals.indices.len) {
            out_err.* = try generateMissingPositionalMessage(
                self.a,
                self.args,
                positionals,
            );
            return error.MissingPositionalArg;
        }
    }

    pub fn showHelp(
        self: *@This(),
        writer: anytype,
    ) (@TypeOf(writer).Error || Error)!void {
        try writer.print("{s}: {s}\n", .{ self.name, self.help });
        if (self.args.len == 0) return;
        try writer.print("USAGE: {s} [OPTIONS]", .{self.name});
        const indices = try countPositionals(self.a, self.args);
        defer self.a.free(indices);
        for (indices) |i| {
            const arg = self.args[i];
            try writer.print(" <{s}>", .{arg.long_name});
        }
        try writer.writeAll("\nOPTIONS:\n");
        for (self.args) |arg| {
            switch (arg.kind) {
                .pos => try writer.print("  {s}", .{arg.long_name}),
                .flag => {
                    if (arg.short_name != 0) {
                        try writer.print("  -{c}", .{arg.short_name});
                        if (arg.long_name.len > 0) {
                            try writer.writeAll(", ");
                        } else {
                            try writer.writeAll(" ");
                        }
                    } else {
                        try writer.writeAll("      ");
                    }
                    if (arg.long_name.len > 0) {
                        try writer.print("--{s}", .{arg.long_name});
                    }
                },
                .opt => {
                    if (arg.short_name != 0) {
                        try writer.print("  -{c}", .{arg.short_name});
                        if (arg.long_name.len > 0) {
                            try writer.writeAll(", ");
                        } else {
                            try writer.writeAll(" ");
                        }
                    } else {
                        try writer.writeAll("      ");
                    }
                    if (arg.long_name.len > 0) {
                        try writer.print("--{s} ", .{arg.long_name});
                    }
                    try writer.writeAll("<VALUE>");
                },
            }
            try writer.print("    {s}\n", .{arg.help});
        }
    }
};

fn countPositionals(
    a: std.mem.Allocator,
    args: []const Arg,
) Error!IndexBuf.Slice {
    var indices = IndexBuf.init(a);
    for (args) |arg, i| {
        if (arg.kind == .pos) {
            try indices.append(i);
        }
    }
    return try indices.toOwnedSlice();
}
