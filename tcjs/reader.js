function BlankException(msg) {
}

function Reader(tokens) {
    var current = 0;

    if (!Array.isArray(tokens)) {
        throw new Error("expected array-like");
    }

    if (tokens.length === 0) {
        throw new BlankException();
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

function tokenizer(str) {
    var re = /[\s,]*(~@|[\[\]{}()'`~^@]|"(?:\\.|[^\\"])*"|;.*|[^\s\[\]{}('"`,;)]*)/g;
    var results = [];
    while ((match = re.exec(str)[1]) != '') {
        if (match[0] === ';') { continue; }
        results.push(match);
    }
    return results;
}

function read_symbol(r) {
    return {symbol: r.next()};
}

function read_string(r) {
    var s = r.next();
    if (s[0] !== '"' || s[s.length - 1] !== '"') {
        throw new Error("problem reading string: " + s);
    }
    return s.substr(1, s.length - 2);
}

function read_keyword(r) {
    var kw = r.next().substr(1);

    if (kw === '') {
        throw new Error('Could not read keyword');
    }

    return {keyword: kw};
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

function assignType(obj, type) {
    return Object.defineProperty(obj, type,
                                 {enumerable: false,
                                  writable: false,
                                  configurable: false,
                                  value: true});
}

function read_vector(r) {
    var res = [];
    assignType(res, 'malVector');

    if (r.next() !== '[') {
        throw new Error("thought we were reading a vector");
    }

    while (r.peek() && r.peek() !== ']') {
        res.push(read_form(r));
    }

    var finished = r.next();

    if (!finished || finished !== ']') {
        throw new Error("vector form not balanced, expected ')'");
    }

    return res;
}

function read_map(r) {
    var res = {};
    assignType(res, 'malMap');

    if (r.next() !== '{') {
        throw new Error("thought we were reading a map");
    }

    while (r.peek() && r.peek() !== '}') {
        var key = read_form(r);

        if (!key.keyword) {
            throw new Error("only keywords supported as keys");
        }

        if (r.peek() === '}') {
            throw new Error("unpaired key reading map form");
        }

        var value = read_form(r);
        res[key.keyword] = value;
    }

    var finished = r.next();

    if (!finished || finished !== '}') {
        throw new Error("map form not balanced, expected '}'");
    }

    return res;
}

function read_quotes(prefix, type, field, r) {
    if (r.next() !== prefix) {
        throw new Error("though we were reading a " + type + " form");
    }
    var res = {};
    res[field] = read_form(r);
    return res;
}

function read_quote(r) {
    return read_quotes("'", 'quoted', 'quote', r);
}

function read_quasiquote(r) {
    return read_quotes('`', 'quasiquoted', 'quasi', r);
}

function read_unquote(r) {
    return read_quotes('~', 'unquoted', 'unquote', r);
}

function read_spliceunquote(r) {
    return read_quotes('~@', 'splice-unquoted', 'splice_unquote', r);
}

function read_form(r) {
    switch (r.peek()[0]) {
    case ':': return read_keyword(r);
    case '"': return read_string(r);
    case ';': return null;
    }

    switch (r.peek()) {
    case '(':    return read_list(r);
    case '[':    return read_vector(r);
    case '{':    return read_map(r);
    case "'":    return read_quote(r);
    case '`':    return read_quasiquote(r);
    case '~':    return read_unquote(r);
    case '~@':   return read_spliceunquote(r);
    case 'true': return true;
    case 'false':return false;
    case 'nil':  return null;
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
exports.BlankException = BlankException;
