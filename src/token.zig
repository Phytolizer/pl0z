const std = @import("std");

pub const TokenKind = enum {
    ident,
    number,
    @"const",
    @"var",
    procedure,
    call,
    begin,
    end,
    @"if",
    then,
    @"while",
    do,
    odd,
    dot,
    equal,
    comma,
    semicolon,
    assign,
    hash,
    less_than,
    greater_than,
    plus,
    minus,
    multiply,
    divide,
    lparen,
    rparen,
    write_int,
    write_char,
    read_int,
    read_char,
    into,

    unknown,
    eof,

    pub fn fromChar(c: u8) @This() {
        return switch (c) {
            '.' => .dot,
            '=' => .equal,
            ',' => .comma,
            ';' => .semicolon,
            '#' => .hash,
            '<' => .less_than,
            '>' => .greater_than,
            '+' => .plus,
            '-' => .minus,
            '*' => .multiply,
            '/' => .divide,
            '(' => .lparen,
            ')' => .rparen,
            else => unreachable,
        };
    }

    pub fn toChar(self: @This()) u8 {
        return switch (self) {
            .dot => '.',
            .equal => '=',
            .comma => ',',
            .semicolon => ';',
            .hash => '#',
            .less_than => '<',
            .greater_than => '>',
            .plus => '+',
            .minus => '-',
            .multiply => '*',
            .divide => '/',
            .lparen => '(',
            .rparen => ')',
            else => unreachable,
        };
    }

    pub fn format(
        self: @This(),
        comptime _: []const u8,
        opts: std.fmt.FormatOptions,
        writer: anytype,
    ) @TypeOf(writer).Error!void {
        const text = std.meta.fieldNames(@This())[@enumToInt(self)];

        return std.fmt.formatBuf(text, opts, writer);
    }
};
