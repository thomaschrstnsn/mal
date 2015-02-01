function Reader(tokens) {
    var current = 0;

    if (!tokens || !tokens.length) {
        throw new Error("expected array-like");
    }

    var api = {};

    api.next = function() {
        return tokens[current++];
    };

    api.peek = function() {
        return tokens[current];
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

    if (r.next().trim() !== '(') {
        throw new Error("thought we were reading a list");
    }

    while (r.peek() && r.peek().trim() !== ')') {
        res.push(read_form(r));
    }

    var finished = r.next();

    if (!finished || finished.trim() !== ')') {
        throw new Error("list form not balanced, expected ')'");
    }

    return res;
}

function read_form(r) {
    if (r.peek().trim() === '(') {
        return read_list(r);
    }
    return read_atom(r);
}

function read_str(input) {
    var tokens = tokenizer(input);
    var reader = Reader(tokens);
    var result = read_form(reader);

    if (!reader.is_finished()) {
        console.log('WARNING: string was not fully consumed while reading form, next token:', reader.peek());
    }

    return result;
}

exports.read_str = read_str;
exports.tokenizer = tokenizer;
