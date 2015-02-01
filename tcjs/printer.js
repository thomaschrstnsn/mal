function pr_number(x) {
    return x.toString();
}

function pr_list(x) {
    return '(' + x.map(pr_str).join(' ') + ')';
};

function pr_str(x) {
    if (Array.isArray(x)) {
        return pr_list(x);
    }
    if (!isNaN(new Number(x))) {
        return pr_number(x);
    }
    if (x.symbol) {
        return x.symbol;
    }

    throw new Error("Unhandled thing: " + x.toString());
}

exports.pr_str = pr_str;
