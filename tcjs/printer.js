var types = require('./types');
var logger = require('./logger');

function pr_seq(x, prefix, postfix, printer) {
    return prefix + x.map(printer).join(' ') + postfix;
}

function escape_string(x) {
    return x.replace(/\\/g, '\\\\')
        .replace(/"/g, '\\"')
        .replace(/\n/g, '\\n');
}

function pr_str(x, print_readably) {
    if (x === null) {
        return 'nil';
    }

    if (types.isKeyword(x)) {
        return ':' + types.nameOf(x);
    }
    if (typeof x === 'string') {
        if (print_readably) {
            return '"' + escape_string(x) + '"';
        }
        return x;
    }
    var printer = function (a) {
        return pr_str(a, print_readably);
    };
    if (types.isVector(x)) {
        return pr_seq(x, '[', ']', printer);
    }
    if (types.isList(x)) {
        return pr_seq(x, '(', ')', printer);
    }
    if (types.isMap(x)) {
        return '{' + Object.keys(x).map(function (k) {
            return pr_str(k, print_readably) + ' ' +
                pr_str(x[k], print_readably);
        }).join(', ') + '}';
    }
    if (!isNaN(new Number(x))) {
        return x.toString();
    }
    if (types.isSymbol(x)) {
        return types.nameOf(x);
    }
    if (typeof x === 'function') {
        return '#native func"'+ x.toString() + '"';
    }
    if (x.fn && x.ast && x.params && x.env) {
        var type = types.isMacro(x) ? 'macro' : 'func';
        return '#' + type + '"(fn* (' + x.params.join(" ") + ') '+ pr_str(x.ast) + ')"';
    }

    logger.debug("do not know how to print:", x);
    throw new Error("Unhandled thing");
}

exports.pr_str = pr_str;
