function Reader(tokens) {
    var current = 0;

    if (!tokens || !tokens.length) {
        throw new Error("expected array-like");
    }

    function commaTrim(s) {
        return s && s.replace(/,*/g, '').trim();
    }

    var api = {};

    api.next = function() {
        return commaTrim(tokens[current++]);
    };

    api.peek = function() {
        return commaTrim(tokens[current]);
    };

    api.is_finished = function() {
        return api.peek() === undefined;
    };

    api.tokens = tokens;

    return api;
};

function tokenizer(input) {
    var re = /[\s,]*(~@|[\[\]{}()'`~^@]|"(?:\\.|[^\\"])*"|;.*|[^\s\[\]{}('"`,;)]*)/g;
    return input.trim().match(re).slice(0, -1);
}

function read_symbol(r) {
    return {symbol: r.next().trim()};
}

function read_number(r) {
    return parseInt(r.next(), 10);
}

function read_atom(r) {
    if (isNaN(parseInt(r.peek(), 10))) {
        return read_symbol(r);
    }
    return read_number(r);
}

function read_list(r) {
    var res = [];

    if (r.next() !== '(') {
        throw new Error("thought we were reading a list");
    }

    while (r.peek() && r.peek() !== ')') {
        res.push(read_form(r));
    }

    var finished = r.next();

    if (!finished || finished !== ')') {
        throw new Error("list form not balanced, expected ')'");
    }

    return res;
}

function read_quote(r) {
    if (r.next() !== "'") {
        throw new Error("though we were reading a quoted form");
    }
    return {quote: read_form(r)};
}

function read_quasiquote(r) {
    if (r.next() !== "`") {
        throw new Error("though we were reading a quasiquoted form");
    }
    return {quasi: read_form(r)};
}

function read_unquote(r) {
    if (r.next() !== "~") {
        throw new Error("though we were reading a unquoted form");
    }
    return {unquote: read_form(r)};
}

function read_spliceunquote(r) {
    if (r.next() !== "~@") {
        throw new Error("though we were reading a splice-unquoted form");
    }
    return {splice_unquote: read_form(r)};
}

function read_form(r) {
    switch (r.peek()) {
    case '(':    return read_list(r);
    case "'":    return read_quote(r);
    case '`':    return read_quasiquote(r);
    case '~':    return read_unquote(r);
    case '~@':   return read_spliceunquote(r);
    default:     return read_atom(r);
    }
}

function read_str(input) {
    var tokens = tokenizer(input);
    var reader = Reader(tokens);
    var result = read_form(reader);

    // if (!reader.is_finished()) {
    //     console.log('WARNING: string was not fully consumed while reading form, next token:', reader.peek());
    // }

    return result;
}

exports.read_str = read_str;
