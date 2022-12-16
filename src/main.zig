const std = @import("std");
const uncamel = @import("uncamel.zig").uncamel;
const parser = @import("parser.zig");

//
// pl0c -- PL/0 compiler.
//
// program      = block "." .
// block        = [ "const" ident "=" number { "," ident "=" number } ";" ]
//                [ "var" ident { "," ident } ";" ]
//                { "procedure" ident ";" block ";" } statement .
// statement    = [ ident ":=" expression
//                | "call" ident
//                | "begin" statement { ";" statement } "end"
//                | "if" condition "then" statement
//                | "while" condition "do" statement ] .
// condition    = "odd" expression
//                | expression ( "=" | "#" | "<" | ">" ) expression .
// expression   = [ "+" | "-" ] term { ( "+" | "-" ) term } .
// term         = factor { ( "*" | "/" ) factor } .
// factor       = ident
//                | number
//                | "(" expression ")" .
//

fn readin(path: []const u8, a: std.mem.Allocator) ![]u8 {
    const file = try std.fs.cwd().openFile(path, .{});
    defer file.close();
    return try file.readToEndAlloc(a, std.math.maxInt(usize));
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.detectLeaks();
    const a = gpa.allocator();

    const args = try std.process.argsAlloc(a);
    defer std.process.argsFree(a, args);

    var stderr = std.io.getStdErr().writer();

    if (args.len < 2) {
        stderr.print(
            "Usage: {s} <file.pl0> [-o <outfile>]\n",
            .{args[0]},
        ) catch unreachable;
        std.process.exit(1);
    }

    const ext = std.fs.path.extension(args[1]);
    if (!std.mem.eql(u8, ext, ".pl0")) {
        stderr.print(
            "error: file extension must be .pl0, not \"{s}\"\n",
            .{ext},
        ) catch unreachable;
        std.process.exit(1);
    }
    const raw = readin(args[1], a) catch |e| {
        const uncameled = uncamel(@errorName(e), a) catch unreachable;
        stderr.print(
            "error reading {s} ({s})\n",
            .{ args[1], uncameled },
        ) catch unreachable;
        a.free(uncameled);
        std.process.exit(1);
    };
    defer a.free(raw);

    var out = getOut: {
        if (args.len > 2 and std.mem.eql(u8, args[2], "-o")) {
            if (args.len < 4) {
                stderr.print(
                    "error: expected output file after -o\n",
                    .{},
                ) catch unreachable;
                std.process.exit(1);
            }
            break :getOut try std.fs.cwd().createFile(args[3], .{});
        } else {
            break :getOut null;
        }
    };

    parser.parse(a, raw, (out orelse std.io.getStdOut()).writer()) catch |e| {
        const uncameled = uncamel(@errorName(e), a) catch unreachable;
        stderr.print(
            "error parsing {s} ({s})\n",
            .{ args[1], uncameled },
        ) catch unreachable;
        a.free(uncameled);
        if (out) |o| {
            o.close();
            try std.fs.cwd().deleteFile(args[3]);
        }
        std.process.exit(1);
    };
    if (out) |o| {
        o.close();
    }
}

test "parsing" {
    const a = std.testing.allocator;

    var dir = try std.fs.cwd().openIterableDir("test", .{});
    defer dir.close();
    var walker = try dir.walk(a);
    defer walker.deinit();
    while (try walker.next()) |ent| {
        const fullDir = try std.fs.path.join(a, &.{ "test", ent.path });
        defer a.free(fullDir);
        std.debug.print("'{s}'... \n", .{fullDir});
        const raw = try readin(fullDir, a);
        defer a.free(raw);

        try parser.parse(a, raw);
    }
}
