const std = @import("std");

const Scanner = @This();

pub const TokenType = enum(usize) {
    // Single-character tokens.
    left_paren,
    right_paren,
    left_brace,
    right_brace,
    comma,
    dot,
    minus,
    plus,
    semicolon,
    slash,
    star,
    // One or two character tokens.
    bang,
    bang_equal,
    equal,
    equal_equal,
    greater,
    greater_equal,
    less,
    less_equal,
    // Literals.
    identifier,
    string,
    number,
    // Keywords.
    and_,
    class,
    else_,
    false_,
    for_,
    fun,
    if_,
    nil,
    or_,
    print,
    return_,
    super,
    this,
    true_,
    var_,
    while_,

    error_,
    eof,
};

pub const Token = struct {
    type: TokenType,
    start: [*]const u8,
    length: usize,
    line: usize,
};

pub const SourceIterator = struct {
    len: usize,
    index: usize,
    start: [*]const u8,
    current: [*]const u8,
    line: usize,

    pub fn init(source: []const u8) SourceIterator {
        return .{
            .len = source.len,
            .index = 0,
            .start = source.ptr,
            .current = source.ptr,
            .line = 1,
        };
    }

    pub fn advance(self: *SourceIterator) u8 {
        defer self.current += 1;
        self.index += 1;
        return self.current[0];
    }

    pub fn skipWhitespace(self: *SourceIterator) void {
        while (true) {
            const c = self.peek();
            switch (c) {
                ' ', '\r', '\t' => _ = self.advance(),
                '\n' => {
                    self.line += 1;
                    _ = self.advance();
                },
                '/' => {
                    if (self.peekNext()) |next| {
                        if (next == '/') {
                            while (self.peek() != '\n' and !self.isAtEnd()) _ = self.advance();
                        }
                        return;
                    }
                    return;
                },
                else => return,
            }
        }
    }

    pub fn peek(self: *SourceIterator) u8 {
        return self.current[0];
    }

    fn isAtEnd(self: *SourceIterator) bool {
        return self.index >= self.len;
    }

    pub fn match(self: *SourceIterator, expected: u8) bool {
        if (self.isAtEnd()) return false;
        if (self.current[0] != expected) return false;
        self.current += 1;
        self.index += 1;
        return true;
    }

    pub fn peekNext(self: *SourceIterator) ?u8 {
        if (self.index + 1 >= self.len) return null;
        return self.current[1];
    }

    pub fn checkKeyword(self: *SourceIterator, start: usize, length: usize, rest: []const u8, ttype: TokenType) TokenType {
        if (@as(usize, @intFromPtr(self.current)) - @as(usize, @intFromPtr(self.start)) == start + length and std.mem.eql(u8, (self.start + start)[0..length], rest)) {
            return ttype;
        }

        return TokenType.identifier;
    }
};

itr: SourceIterator,

pub fn init(source: []const u8) Scanner {
    return .{
        .itr = SourceIterator.init(source),
    };
}

pub fn deinit(self: *Scanner) void {
    _ = self;
}

pub fn scanToken(self: *Scanner) Token {
    self.itr.skipWhitespace();
    self.itr.start = self.itr.current;

    if (self.itr.isAtEnd()) return self.makeToken(.eof);

    const c = self.itr.advance();
    if (std.ascii.isAlphabetic(c)) return self.identifier();
    if (std.ascii.isDigit(c)) return self.number();

    switch (c) {
        '(' => return self.makeToken(.left_paren),
        ')' => return self.makeToken(.right_paren),
        '{' => return self.makeToken(.left_brace),
        '}' => return self.makeToken(.right_brace),
        ';' => return self.makeToken(.semicolon),
        ',' => return self.makeToken(.comma),
        '.' => return self.makeToken(.dot),
        '-' => return self.makeToken(.minus),
        '+' => return self.makeToken(.plus),
        '/' => return self.makeToken(.slash),
        '*' => return self.makeToken(.star),
        '!' => return self.makeToken((if (self.itr.match('=')) TokenType.bang_equal else TokenType.bang)),
        '=' => return self.makeToken((if (self.itr.match('=')) TokenType.equal_equal else TokenType.equal)),
        '<' => return self.makeToken((if (self.itr.match('=')) TokenType.less_equal else TokenType.less)),
        '>' => return self.makeToken((if (self.itr.match('=')) TokenType.greater_equal else TokenType.greater)),
        '"' => return self.string(),
        else => return self.errorToken("Unexpected character."),
    }

    return self.errorToken("Unexpected character.");
}

fn identifier(self: *Scanner) Token {
    while (std.ascii.isAlphanumeric(self.itr.peek())) _ = self.itr.advance();
    return self.makeToken(self.identifierType());
}

fn identifierType(self: *Scanner) TokenType {
    switch (self.itr.start[0]) {
        'a' => return self.itr.checkKeyword(1, 2, "nd", .and_),
        'c' => return self.itr.checkKeyword(1, 4, "lass", .class),
        'e' => return self.itr.checkKeyword(1, 3, "lse", .else_),
        'f' => {
            if (@as(usize, @intFromPtr(self.itr.current)) - @as(usize, @intFromPtr(self.itr.start)) > 1) {
                switch (self.itr.start[1]) {
                    'a' => return self.itr.checkKeyword(2, 3, "lse", .false_),
                    'o' => return self.itr.checkKeyword(2, 1, "r", .for_),
                    'u' => return self.itr.checkKeyword(2, 1, "n", .fun),
                    else => return TokenType.identifier,
                }
            }
        },
        'i' => return self.itr.checkKeyword(1, 1, "f", .if_),
        'n' => return self.itr.checkKeyword(1, 2, "il", .nil),
        'o' => return self.itr.checkKeyword(1, 1, "r", .or_),
        'p' => return self.itr.checkKeyword(1, 4, "rint", .print),
        'r' => return self.itr.checkKeyword(1, 5, "eturn", .return_),
        's' => return self.itr.checkKeyword(1, 4, "uper", .super),
        't' => {
            if (@as(usize, @intFromPtr(self.itr.current)) - @as(usize, @intFromPtr(self.itr.start)) > 1) {
                switch (self.itr.start[1]) {
                    'h' => return self.itr.checkKeyword(2, 2, "is", .this),
                    'r' => return self.itr.checkKeyword(2, 2, "ue", .true_),
                    else => return TokenType.identifier,
                }
            }
        },
        'v' => return self.itr.checkKeyword(1, 2, "ar", .var_),
        'w' => return self.itr.checkKeyword(1, 4, "hile", .while_),
        else => return TokenType.identifier,
    }
    return TokenType.identifier;
}

fn number(self: *Scanner) Token {
    while (std.ascii.isDigit(self.itr.peek())) _ = self.itr.advance();

    if (self.itr.peekNext()) |next| {
        if (self.itr.peek() == '.' and std.ascii.isDigit(next)) {
            _ = self.itr.advance();
            while (std.ascii.isDigit(self.itr.peek())) {
                _ = self.itr.advance();
            }
        }
    }

    return self.makeToken(.number);
}

fn string(self: *Scanner) Token {
    while (self.itr.peek() != '"' and !self.itr.isAtEnd()) {
        if (self.itr.peek() == '$' and (self.itr.peekNext() orelse '0') == '{') {
            // TODO: String interpolation
        }
        if (self.itr.peek() == '\n') self.itr.line += 1;
        _ = self.itr.advance();
    }

    if (self.itr.isAtEnd()) return self.errorToken("Unterminated string.");

    _ = self.itr.advance();
    return self.makeToken(.string);
}

fn makeToken(self: *Scanner, ttype: TokenType) Token {
    const length = if (self.itr.current == self.itr.start) 0 else @as(usize, @intFromPtr(self.itr.current)) - @as(usize, @intFromPtr(self.itr.start));
    return .{
        .type = ttype,
        .start = self.itr.start[0..],
        .length = length,
        .line = self.itr.line,
    };
}

fn errorToken(self: *Scanner, message: []const u8) Token {
    return .{
        .type = .error_,
        .start = message.ptr,
        .length = message.len,
        .line = self.itr.line,
    };
}

test "scanToken" {
    var scanner = Scanner.init(" and \n<= \n== = /");

    var tok = scanner.scanToken();
    try std.testing.expect(std.mem.eql(u8, tok.start[0..tok.length], "and"));
}
