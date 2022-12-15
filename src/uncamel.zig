const std = @import("std");

pub fn uncamel(s: []const u8, a: std.mem.Allocator) ![]u8 {
    var out = std.ArrayList(u8).init(a);
    for (s) |c, i| {
        if (c >= 'A' and c <= 'Z') {
            if (i > 0)
                try out.append(' ');
            try out.append(c + ('a' - 'A'));
        } else {
            try out.append(c);
        }
    }
    return try out.toOwnedSlice();
}
