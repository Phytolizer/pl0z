const std = @import("std");
const TokenKind = @import("token.zig").TokenKind;
const version = @import("version.zig").version;

const LexerState = struct {
    a: std.mem.Allocator,
    raw: []const u8,
    offset: usize = 0,
    token: ?[]u8 = null,
    line: usize = 1,

    pub fn init(allocator: std.mem.Allocator, raw: []const u8) @This() {
        return .{
            .a = allocator,
            .raw = raw,
        };
    }

    pub fn get(self: *@This()) ?u8 {
        if (self.offset >= self.raw.len) return null;
        return self.raw[self.offset];
    }

    pub fn next(self: *@This()) ?u8 {
        const ch = self.get();
        self.offset += 1;
        return ch;
    }

    pub fn backUp(self: *@This()) void {
        self.offset -= 1;
    }

    pub fn freeToken(self: *@This()) void {
        if (self.token) |t| {
            self.a.free(t);
            self.token = null;
        }
    }

    pub fn makeToken(self: *@This(), start: usize) !void {
        if (self.token) |t| {
            self.a.free(t);
        }
        var tok = try self.a.dupe(u8, self.raw[start..self.offset]);
        for (tok) |*c| {
            c.* = std.ascii.toLower(c.*);
        }
        self.token = tok;
    }
};

fn comment(s: *LexerState) !void {
    while (s.next()) |ch| {
        switch (ch) {
            '\n' => s.line += 1,
            '}' => return,
            else => {},
        }
    }

    return error.UnterminatedComment;
}

fn str_eq(s1: []const u8, s2: []const u8) bool {
    return std.mem.eql(u8, s1, s2);
}

fn ident(s: *LexerState) !TokenKind {
    const start = s.offset;
    while (s.get()) |c| {
        if (std.ascii.isAlphanumeric(c) or c == '_') {
            _ = s.next();
        } else break;
    }

    try s.makeToken(start);
    s.backUp();

    const tok = s.token.?;
    if (str_eq(tok, "const"))
        return .@"const";
    if (str_eq(tok, "var"))
        return .@"var";
    if (str_eq(tok, "procedure"))
        return .procedure;
    if (str_eq(tok, "call"))
        return .call;
    if (str_eq(tok, "begin"))
        return .begin;
    if (str_eq(tok, "end"))
        return .end;
    if (str_eq(tok, "if"))
        return .@"if";
    if (str_eq(tok, "then"))
        return .then;
    if (str_eq(tok, "while"))
        return .@"while";
    if (str_eq(tok, "do"))
        return .do;
    if (str_eq(tok, "odd"))
        return .odd;

    return .ident;
}

fn number(s: *LexerState) !TokenKind {
    const start = s.offset;
    while (s.get()) |c| {
        if (std.ascii.isDigit(c) or c == '_') {
            _ = s.next();
        } else break;
    }

    s.freeToken();

    var token = try std.ArrayList(u8).initCapacity(s.a, s.offset - start);
    for (s.raw[start..s.offset]) |c| {
        if (c == '_') continue;
        try token.append(c);
    }
    s.token = try token.toOwnedSlice();
    s.backUp();

    _ = try std.fmt.parseInt(i64, s.token.?, 10);

    return .number;
}

fn lex(s: *LexerState) !TokenKind {
    while (true) {
        while (s.get()) |c| {
            if (std.mem.indexOfScalar(u8, " \t\n", c) == null)
                break;
            if (c == '\n')
                s.line += 1;
            _ = s.next();
        }

        if (s.get() == null)
            return .eof;

        const c = s.get().?;
        if (std.ascii.isAlphabetic(c) or c == '_')
            return ident(s);
        if (std.ascii.isDigit(c))
            return number(s);

        switch (c) {
            '{' => try comment(s),
            '.',
            '=',
            ',',
            ';',
            '#',
            '<',
            '>',
            '+',
            '-',
            '*',
            '/',
            '(',
            ')',
            '!',
            => return TokenKind.fromChar(c),
            ':' => {
                _ = s.next();
                const after_colon = s.get();
                if (after_colon == null) {
                    std.debug.print("unknown token: ':'", .{});
                    return error.UnknownToken;
                }
                if (after_colon.? != '=') {
                    std.debug.print("unknown token: ':{c}'", .{after_colon.?});
                    return error.UnknownToken;
                }

                return .assign;
            },
            else => {
                std.debug.print("unknown token: '{c}'", .{c});
                return error.UnknownToken;
            },
        }
    }
}

const ParserState = struct {
    lex: LexerState,
    kind: TokenKind,
    depth: usize,
    stdout: Buf,
    symbols: std.ArrayList(Symbol),
    symbol_arena: std.heap.ArenaAllocator,
    sa: std.mem.Allocator,
    proc: bool = false,

    const Buf = std.io.BufferedWriter(4096, std.fs.File.Writer);

    const Symbol = struct {
        depth: usize,
        kind: TokenKind,
        name: []u8,

        pub fn init(depth: usize, kind: TokenKind, name: []u8) @This() {
            return .{ .depth = depth, .kind = kind, .name = name };
        }
    };

    const CheckKind = enum { lhs, rhs, call };

    const Error = error{
        UnexpectedToken,
        UnknownToken,
        InvalidCharacter,
        UnterminatedComment,
        NestingDepthExceeded,
        InvalidConditional,
        DuplicateSymbol,
        Overflow,
        UndefinedSymbol,
        InvalidCall,
        InvalidLhs,
        InvalidRhs,
    } || std.mem.Allocator.Error;

    pub fn init(a: std.mem.Allocator, raw: []const u8) Error!@This() {
        var result: ParserState = undefined;
        result.depth = 0;
        result.lex = LexerState.init(a, raw);
        result.stdout = std.io.bufferedWriter(std.io.getStdOut().writer());
        result.symbol_arena = std.heap.ArenaAllocator.init(a);
        result.sa = result.symbol_arena.allocator();
        var head = std.ArrayList(Symbol).init(result.sa);
        try head.append(Symbol.init(
            0,
            .procedure,
            try result.sa.dupe(u8, "main"),
        ));
        result.symbols = head;
        return result;
    }

    pub fn deinit(self: *@This()) void {
        self.lex.freeToken();
        self.stdout.flush() catch unreachable;
        self.symbol_arena.deinit();
    }

    fn out(self: *@This(), comptime fmt: []const u8, args: anytype) void {
        self.stdout.writer().print(fmt, args) catch unreachable;
    }

    pub fn next(self: *@This()) Error!void {
        self.kind = lex(&self.lex) catch |e| switch (e) {
            error.UnknownToken => {
                std.debug.print(" (line {d})\n", .{self.lex.line});
                return e;
            },
            else => return e,
        };
        _ = self.lex.next();
    }

    pub fn expect(self: *@This(), match: TokenKind) Error!void {
        if (self.kind != match) {
            std.debug.print(
                "expected <{s}>, got <{s}>",
                .{ match, self.kind },
            );
            return error.UnexpectedToken;
        }
        try self.next();
    }

    pub fn block(self: *@This()) Error!void {
        if (self.depth > 1)
            return error.NestingDepthExceeded;

        self.depth += 1;

        if (self.kind == .@"const") {
            try self.expect(.@"const");
            if (self.kind == .ident) {
                try self.addSymbol(.@"const");
                self.genConst();
            }
            try self.expect(.ident);
            try self.expect(.equal);
            if (self.kind == .number) {
                self.genSymbol();
                self.genSemicolon();
            }
            try self.expect(.number);
            while (self.kind == .comma) {
                try self.expect(.comma);
                if (self.kind == .ident) {
                    try self.addSymbol(.@"const");
                    self.genConst();
                }
                try self.expect(.ident);
                try self.expect(.equal);
                if (self.kind == .number) {
                    self.genSymbol();
                    self.genSemicolon();
                }
                try self.expect(.number);
            }
            try self.expect(.semicolon);
        }
        if (self.kind == .@"var") {
            try self.expect(.@"var");
            if (self.kind == .ident) {
                try self.addSymbol(.@"var");
                self.genVar();
            }
            try self.expect(.ident);
            while (self.kind == .comma) {
                try self.expect(.comma);
                if (self.kind == .ident) {
                    try self.addSymbol(.@"var");
                    self.genVar();
                }
                try self.expect(.ident);
            }
            try self.expect(.semicolon);
            self.genLf();
        }
        while (self.kind == .procedure) {
            self.proc = true;

            try self.expect(.procedure);
            if (self.kind == .ident) {
                try self.addSymbol(.procedure);
                self.genPrologue();
            }
            try self.expect(.ident);
            try self.expect(.semicolon);

            try self.block();

            try self.expect(.semicolon);

            self.proc = false;
        }

        if (!self.proc)
            self.genPrologue();

        try self.statement();

        self.genEpilogue();

        self.depth -= 1;
    }

    fn statement(self: *@This()) Error!void {
        switch (self.kind) {
            .ident => {
                try self.checkSymbol(.lhs);
                self.genSymbol();
                try self.expect(.ident);
                if (self.kind == .assign)
                    self.genSymbol();
                try self.expect(.assign);
                try self.expression();
            },
            .call => {
                try self.expect(.call);
                if (self.kind == .ident) {
                    try self.checkSymbol(.call);
                    self.genCall();
                }
                try self.expect(.ident);
            },
            .begin => {
                self.genSymbol();
                try self.expect(.begin);
                try self.statement();
                while (self.kind == .semicolon) {
                    self.genSemicolon();
                    try self.expect(.semicolon);
                    try self.statement();
                }
                if (self.kind == .end)
                    self.genSymbol();
                try self.expect(.end);
            },
            .@"if" => {
                self.genSymbol();
                try self.expect(.@"if");
                try self.condition();
                if (self.kind == .then)
                    self.genSymbol();
                try self.expect(.then);
                try self.statement();
            },
            .@"while" => {
                self.genSymbol();
                try self.expect(.@"while");
                try self.condition();
                if (self.kind == .do)
                    self.genSymbol();
                try self.expect(.do);
                try self.statement();
            },
            else => {},
        }
    }

    fn condition(self: *@This()) Error!void {
        if (self.kind == .odd) {
            self.genSymbol();
            try self.expect(.odd);
            try self.expression();
            self.genOdd();
        } else {
            try self.expression();

            switch (self.kind) {
                .equal,
                .hash,
                .less_than,
                .greater_than,
                => {
                    self.genSymbol();
                    try self.next();
                },
                else => {
                    std.debug.print("expected condition, got <{s}>\n", .{self.kind});
                    return error.InvalidConditional;
                },
            }

            try self.expression();
        }
    }

    fn factor(self: *@This()) Error!void {
        switch (self.kind) {
            .ident => {
                try self.checkSymbol(.rhs);
                self.genSymbol();
                try self.next();
            },
            .number => {
                self.genSymbol();
                try self.next();
            },
            .lparen => {
                self.genSymbol();
                try self.expect(.lparen);
                try self.expression();
                if (self.kind == .rparen)
                    self.genSymbol();
                try self.expect(.rparen);
            },
            else => {},
        }
    }

    fn term(self: *@This()) Error!void {
        try self.factor();
        while (self.kind == .multiply or self.kind == .divide) {
            self.genSymbol();
            try self.next();
            try self.factor();
        }
    }

    fn expression(self: *@This()) Error!void {
        if (self.kind == .plus or self.kind == .minus) {
            self.genSymbol();
            try self.next();
        }
        try self.term();
        while (self.kind == .plus or self.kind == .minus) {
            self.genSymbol();
            try self.next();
            try self.term();
        }
    }

    pub fn end(self: *@This()) void {
        self.out("/* PL/0 compiler {s} */\n", .{version});
    }

    fn genConst(self: *@This()) void {
        self.out("const long {s}=", .{self.lex.token.?});
    }

    fn genVar(self: *@This()) void {
        self.out("long {s};\n", .{self.lex.token.?});
    }

    fn genSemicolon(self: *@This()) void {
        self.out(";\n", .{});
    }

    fn genLf(self: *@This()) void {
        self.out("\n", .{});
    }

    fn genSymbol(self: *@This()) void {
        switch (self.kind) {
            .ident, .number => {
                self.out("{s}", .{self.lex.token.?});
            },
            .begin => self.out("{{\n", .{}),
            .end => self.out(";\n}}\n", .{}),
            .@"if" => self.out("if(", .{}),
            .then, .do => self.out(")", .{}),
            .odd => self.out("(", .{}),
            .@"while" => self.out("while(", .{}),
            .equal => self.out("==", .{}),
            .comma => self.out(",", .{}),
            .assign => self.out("=", .{}),
            .hash => self.out("!=", .{}),
            .less_than => self.out("<", .{}),
            .greater_than => self.out(">", .{}),
            .plus => self.out("+", .{}),
            .minus => self.out("-", .{}),
            .multiply => self.out("*", .{}),
            .divide => self.out("/", .{}),
            .lparen => self.out("(", .{}),
            .rparen => self.out(")", .{}),
            else => unreachable,
        }
    }

    fn genPrologue(self: *@This()) void {
        if (self.proc) {
            self.out("void {s}(void)\n", .{self.lex.token.?});
        } else {
            self.out("int main(int argc, char** argv)\n", .{});
        }

        self.out("{{\n", .{});
    }

    fn genEpilogue(self: *@This()) void {
        self.out(";", .{});
        if (!self.proc)
            self.out("return 0;", .{});
        self.out("\n}}\n\n", .{});
    }

    fn genCall(self: *@This()) void {
        self.out("{s}();\n", .{self.lex.token.?});
    }

    fn genOdd(self: *@This()) void {
        self.out(")&1", .{});
    }

    fn checkSymbol(self: *@This(), check: CheckKind) Error!void {
        const sym = find: {
            for (self.symbols.items) |sym| {
                if (str_eq(sym.name, self.lex.token.?))
                    break :find sym;
            }
            std.debug.print("undefined symbol \"{s}\"\n", .{self.lex.token.?});
            return error.UndefinedSymbol;
        };

        switch (check) {
            .lhs => {
                if (sym.kind != .@"var") {
                    std.debug.print(
                        "expected variable, got <{s}>\n",
                        .{sym.kind},
                    );
                    return error.InvalidLhs;
                }
            },
            .rhs => {
                if (sym.kind == .procedure) {
                    std.debug.print(
                        "expected variable or constant, got <{s}>\n",
                        .{sym.kind},
                    );
                    return error.InvalidRhs;
                }
            },
            .call => {
                if (sym.kind != .procedure) {
                    std.debug.print(
                        "expected procedure, got <{s}>\n",
                        .{sym.kind},
                    );
                    return error.InvalidCall;
                }
            },
        }
    }

    fn addSymbol(self: *@This(), kind: TokenKind) !void {
        for (self.symbols.items) |sym| {
            if (str_eq(sym.name, self.lex.token.?) and
                sym.depth == self.depth - 1)
            {
                std.debug.print("duplicate symbol \"{s}\"\n", .{sym.name});
                return error.DuplicateSymbol;
            }
        }

        try self.symbols.append(Symbol.init(
            self.depth - 1,
            kind,
            try self.sa.dupe(u8, self.lex.token.?),
        ));
    }

    pub fn parse(a: std.mem.Allocator, raw: []const u8) Error!void {
        var p = try ParserState.init(a, raw);
        defer p.deinit();

        errdefer |e| if (e == error.UnexpectedToken) {
            std.debug.print(" (line {d})\n", .{p.lex.line});
        };

        try p.next();
        try p.block();
        try p.expect(.dot);
        if (p.kind != .eof)
            return error.UnexpectedToken;
        p.end();
    }
};

pub const parse = ParserState.parse;
