var _ = require('lodash');
var types = require('./types');
var printer = require('./printer');

var floorIt = Math.floor;

function reduceFunc(fn, args) {
    return _.chain(args).values().reduce(fn).value();
}

function add(a, b) {
    return floorIt(a+b);
}

function plus() {
    return reduceFunc(add, arguments);
}

function subtract(a, b) {
    return floorIt(a-b);
}

function minus() {
    return reduceFunc(subtract, arguments);
}

function divide(a ,b) {
    return floorIt(a/b);
}

function slash() {
    return reduceFunc(divide, arguments);
}

function multiply(a, b) {
    return floorIt(a*b);
}

function star() {
    return reduceFunc(multiply, arguments);
}

function list() {
    var l = _.values(arguments);
    types.toList(l);
    return l;
}

function empty(x) {
    return x.length === 0;
}

function count(x) {
    if (x === null) {
        return 0;
    }
    return x.length;
}

function seq_eq(a, b) {
    return a.length === b.length &&
        _.chain(a).zip(b).every(function (vs) {
            return eq(vs[0], vs[1]);
        }).value();
}

function eq(a, b) {
    function bothAre(pred) {
        return pred(a) && pred(b);
    };
    if (a === null || b === null) {
        return a === b;
    }
    if ((types.isList(a) || types.isVector(a)) &&
        (types.isList(b) || types.isVector(b))) {
        return seq_eq(a, b);
    }
    if (bothAre(types.isKeyword)) {
        return types.nameOf(a) === types.nameOf(b);
    }
    if (bothAre(types.isSymbol)) {
        return types.nameOf(a) === types.nameOf(b);
    }
    if (bothAre(types.isQuoted)) {
        return types.quoteType(a) === types.quoteType(b) &&
            eq(types.getQuoted(a), types.getQuoted(b));
    }
    if (bothAre(types.isString)) {
        return a === b;
    }

    return a === b;
}

function lt(a, b) {
    return a < b;
}

function lte(a,b) {
    return a <= b;
}

function gt(a,b) {
    return a > b;
}

function gte(a,b) {
    return a >= b;
}

function print_joined(args, print_readably, joiner) {
    if (args.length === 0) {
        return '';
    }
    return _.chain(args)
        .values()
        .map(function (x) {return printer.pr_str(x, print_readably);})
        .reduce(function (a,b) { return a + joiner + b;})
        .value();
}

function pr_str() {
    return print_joined(arguments, true, ' ');
}

function str() {
    return print_joined(arguments, false, '');
}

function prn() {
    var toPrint = print_joined(arguments, true, ' ');
    console.log(toPrint);
    return null;
}

function println() {
    var toPrint = print_joined(arguments, false, ' ');
    console.log(toPrint);
    return null;
}

var read_str = require('./reader').read_str;

function read_string(s) {
    return read_str(s);
}

var fs = require('fs');

function slurp(filename) {
    return fs.readFileSync(filename, {encoding: 'utf8'});
}

function concatTwo(xs, ys) {
    return xs.concat(ys);
}

function concat() {
    var res = _.chain(arguments).values().reduce(concatTwo, []).value();
    types.toList(res);
    return res;
}

function cons(x, xs) {
    return concat([x], xs);
}

module.exports = {'+': plus,
                  '-': minus,
                  '/': slash,
                  '*': star,
                  'list': list,
                  'list?': types.isList,
                  'empty?': empty,
                  'count': count,
                  '=': eq,
                  '<': lt,
                  '<=': lte,
                  '>': gt,
                  '>=': gte,
                  'pr-str': pr_str,
                  'str': str,
                  'prn': prn,
                  'println': println,
                  'read-string': read_string,
                  'slurp': slurp,
                  'cons': cons,
                  'concat': concat
                 };
