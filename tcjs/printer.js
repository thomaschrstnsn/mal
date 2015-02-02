var types = require('./types');
var keywordMarker = require('./types').keywordMarker;

function pr_seq(x, prefix, postfix) {
    return prefix + x.map(pr_str).join(' ') + postfix;
}

function pr_str(x) {
    if (x === null) {
        return 'nil';
    }

    if (types.isKeyword(x)) {
        return ':' + types.nameOf(x);
    }
    if (typeof x === 'string') {
        return '"' + x + '"';
    }
    if (types.isVector(x)) {
        return pr_seq(x, '[', ']');
    }
    if (types.isList(x)) {
        return pr_seq(x, '(', ')');
    }
    if (types.isMap(x)) {
        return '{' + Object.keys(x).map(function (k) {
            return pr_str(k) + ' ' + pr_str(x[k]);
        }).join(', ') + '}';
    }
    if (!isNaN(new Number(x))) {
        return x.toString();
    }
    if (types.isSymbol(x)) {
        return types.nameOf(x);
    }
    if (types.isQuoted(x)) {
        return '(' + types.quoteType(x) + ' ' + pr_str(types.getQuoted(x)) + ')';
    }

    throw new Error("Unhandled thing");
}

exports.pr_str = pr_str;
