function pr_str(x) {
    if (Array.isArray(x)) {
        return '(' + x.map(pr_str).join(' ') + ')';
    }
    if (!isNaN(new Number(x))) {
        return x.toString();
    }
    if (x.symbol) {
        return x.symbol;
    }
    if (x.quote) {
        return '(quote ' + pr_str(x.quote) + ')';
    }
    if (x.quasi) {
        return '(quasiquote ' + pr_str(x.quasi) + ')';
    }
    if (x.unquote) {
        return '(unquote ' + pr_str(x.unquote) + ')';
    }
    if (x.splice_unquote) {
        return '(splice-unquote ' + pr_str(x.splice_unquote) + ')';
    }

    throw new Error("Unhandled thing: " + x.toString());
}

exports.pr_str = pr_str;
