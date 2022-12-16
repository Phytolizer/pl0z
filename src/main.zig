const std = @import("std");
const uncamel = @import("uncamel.zig").uncamel;
const parser = @import("parser.zig");
const args = @import("args.zig");

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
    run() catch std.process.exit(1);
}

fn run() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.detectLeaks();
    const a = gpa.allocator();

    const raw_args = try std.process.argsAlloc(a);
    defer std.process.argsFree(a, raw_args);

    var out_path: ?[]const u8 = null;
    var in_path: []const u8 = "";
    const supported_args = [_]args.Arg{
        args.Arg.pos("input", "input file", &in_path),
        .{
            .kind = .{ .opt = &out_path },
            .short_name = 'o',
            .long_name = "output",
            .help = "output file",
        },
    };

    var arg_parser = args.Parser.init(
        a,
        raw_args[0],
        "PL/0 compiler",
        &supported_args,
    );
    var out_err: ?[]u8 = null;
    arg_parser.parse(raw_args[1..], &out_err) catch |err| {
        try arg_parser.showHelp(std.io.getStdErr().writer());
        if (out_err) |e| {
            std.debug.print("error: {s}\n", .{e});
            a.free(e);
        }
        return err;
    };

    var stderr = std.io.getStdErr().writer();

    const ext = std.fs.path.extension(in_path);
    if (!std.mem.eql(u8, ext, ".pl0")) {
        stderr.print(
            "error: file extension must be .pl0, not \"{s}\"\n",
            .{ext},
        ) catch unreachable;
        return error.Usage;
    }
    const raw = readin(in_path, a) catch |e| {
        const uncameled = uncamel(@errorName(e), a) catch unreachable;
        stderr.print(
            "error reading {s} ({s})\n",
            .{ in_path, uncameled },
        ) catch unreachable;
        a.free(uncameled);
        return e;
    };
    defer a.free(raw);

    var out = if (out_path) |op|
        try std.fs.cwd().createFile(op, .{})
    else
        null;

    parser.parse(a, raw, (out orelse std.io.getStdOut()).writer()) catch |e| {
        const uncameled = uncamel(@errorName(e), a) catch unreachable;
        stderr.print(
            "error parsing {s} ({s})\n",
            .{ in_path, uncameled },
        ) catch unreachable;
        a.free(uncameled);
        if (out) |o| {
            o.close();
            try std.fs.cwd().deleteFile(out_path.?);
        }
        return error.Parse;
    };
    if (out) |o| {
        o.close();
    }
}

test "parsing" {
    std.testing.refAllDeclsRecursive(@import("args.zig"));
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
