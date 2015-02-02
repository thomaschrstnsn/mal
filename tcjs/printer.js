function pr_quoted(symbol, form) {
    return '(' + symbol + ' ' + pr_str(form) + ')';
}

function pr_str(x) {
    if (x === null) {
        return 'nil';
    }
    if (typeof x === 'string') {
        return '"' + x + '"';
    }
    if (Array.isArray(x)) {
        return (x.malVector ? '[' : '(') +
            x.map(pr_str).join(' ') +
            (x.malVector ? ']' : ')');
    }
    if (x.malMap) {
        return '{' + Object.keys(x).map(function (k) {
            return ':'+k + ' ' + pr_str(x[k]);
        }).join(', ') + '}';
    }
    if (!isNaN(new Number(x))) {
        return x.toString();
    }
    if (x.symbol) {
        return x.symbol;
    }
    if (x.quote) {
        return pr_quoted('quote', x.quote);
    }
    if (x.quasi) {
        return pr_quoted('quasiquote', x.quasi);
    }
    if (x.unquote) {
        return pr_quoted('unquote', x.unquote);
    }
    if (x.splice_unquote) {
        return pr_quoted('splice-unquote', x.splice_unquote);
    }
    if (x.keyword) {
        return ':' + x.keyword;
    }

    throw new Error("Unhandled thing");
}

exports.pr_str = pr_str;
