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

function nilQ(x) {
    return x === null;
}

function trueQ(x) {
    return x === true;
}

function falseQ(x) {
    return x === false;
}

function list() {
    var l = _.values(arguments);
    return types.toList(l);
}

function vector() {
    var v = _.values(arguments);
    return types.toVector(v);
}

function hash_map() {
    var map = {};
    _.chain(arguments)
        .values()
        .chunk(2)
        .forEach(function(pair) {
            var key = pair[0];
            var val = pair[1];

            if (val === undefined) {
                throw new Error('missing value in hash-map');
            }
            if (typeof (key) !== 'string') {
                throw new Error('expected key to be string or keyword');
            }
            map[key] = val;
        })
        .value();
    return types.toMap(map);
}

function assoc() {
    var map = arguments[0];
    if (!types.isMap(map)) {
        throw new Error("assoc expected map as first argument");
    }

    var keyValues = _.chain(arguments).values().rest().value();

    if ((keyValues.length % 2 !== 0)) {
        throw new Error("expected keys and values to have same count");
    }

    var c = types.clone(map);

    for (var i = 0; i < keyValues.length; i++) {
        var key = keyValues[i++];
        var val = keyValues[i];

        if (typeof key !== 'string') {
            throw new Error("only string/keyword keys are allowed on maps");
        }
        c[key] = val;
    }

    return c;
}

function dissoc() {
    var map = arguments[0];
    var keys = _.chain(arguments).values().rest().value();

    if (!types.isMap(map)) {
        throw new Error("dissoc expected map as first argument");
    }

    if (!_.every(keys, function(x) {return typeof x === 'string';})) {
        throw new Error("only string/keyword keys are allowed in maps");
    }

    var c = types.clone(map);

    for (var i = 0; i < keys.length; i++) {
        delete c[keys[i]];
    }
    return c;
}

function get(map, key, notFound) {
    if (map === null) {
        return null;
    }

    if (!types.isMap(map)) {
        throw new Error("get expected map as first argument");
    }

    if (typeof key !== 'string') {
        throw new Error("only string/keyword keys allowed in maps");
    }

    if (map.hasOwnProperty(key)) {
        return map[key];
    }

    return notFound === undefined ? null : notFound;
}

function containsQ(map, key) {
    var notFound = {notFound: key};
    return get(map, key, notFound) !== notFound;
}

function keys(x) {
    if (!types.isMap(x)) {
        throw new Error("keys called on non-map");
    }
    return types.toList(_.keys(x));
}

function vals(x) {
    if (!types.isMap(x)) {
        throw new Error("vals called on non-map");
    }
    return types.toList(_.values(x));
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

function sequentialQ(x) {
    return x !== null && (types.isList(x) || types.isVector(x));
}

function apply() {
    var f = arguments[0];

    var argVals = _.chain(arguments).values();

    var numNonSeqArgs = arguments.length - 2;
    var nonSeqArgs = argVals.rest().take(numNonSeqArgs);

    var seqArgs = argVals.last().value();

    var args = nonSeqArgs.concat(seqArgs).value();

    return f.apply(undefined, args);
}

function map() {
    var f = arguments[0];

    var func = typeof f === 'function' ? f : f.fn;

    var cols = _.chain(arguments).values().rest().value();

    if (!_.every(cols, sequentialQ)){
        throw new Error("non-sequence as collection in map");
    }

    var length = _.chain(cols).map(count).min().value();

    var res = [];
    for (var i = 0; i < length; i++) {
        function at(s) {
            return s[i];
        };

        var args = _.map(cols, at);

        var val = func.apply(undefined, args);
        res.push(val);
    }

    return types.toList(res);
}

function conj() {
    var coll = arguments[0] || types.toList([]);
    if (!sequentialQ(coll)) {
        throw new Error("expected sequential as first argument to conj");
    }

    var xs = _.chain(arguments).values().rest().value();

    if (types.isVector(coll)) {
        return types.toVector(xs.concat(coll));
    }
    return types.toList(coll.concat(xs));
}

function range() {
    var start = 0;
    var end = 0;
    var step = 1;

    switch (arguments.length) {
    case 0: throw new Error("range needs atleast one argument");
    default: throw new Error("range takes at most three arguments");
    case 3:
        step  = arguments[2];
        end   = arguments[1];
        start = arguments[0];
        break;
    case 2:
        start = arguments[0];
        end   = arguments[1];
        break;
    case 1:
        end = arguments[0];
        break;
    }

    return types.toList(_.range(start, end, step));
}

function cons(x, xs) {
    return concat([x], xs);
}

function nth(xs, n) {
    if (xs === null) return null;

    if (types.isList(xs) || types.isVector(xs)) {
        if (n < xs.length && n >= 0) {
            return xs[n];
        }
        throw new Error("invalid index as second parameter to nth");
    }

    throw new Error("non sequence as first parameter to nth");
}

function first(xs) {
    if (xs === null) return null;

    if (types.isList(xs) || types.isVector(xs)) {
        if (empty(xs)) {
            return null;
        }
        return xs[0];
    }

    throw new Error("non sequence as parameter to first");
}

function rest(xs) {
    if (xs === null) return null;

    if (types.isList(xs) || types.isVector(xs)) {
        return types.toList(xs.slice(1));
    }

    throw new Error("non sequence as parameter to rest");
}

function meta(obj) {
    var m = types.getMeta(obj);
    return m !== undefined ? m : null;
}

module.exports = {'+': plus,
                  '-': minus,
                  '/': slash,
                  '*': star,
                  'nil?': nilQ,
                  'true?': trueQ,
                  'false?': falseQ,
                  'symbol': types.str2symbol,
                  'symbol?': types.isSymbol,
                  'keyword': types.str2keyword,
                  'keyword?': types.isKeyword,
                  'list': list,
                  'list?': types.isList,
                  'hash-map': hash_map,
                  'map?': types.isMap,
                  'assoc': assoc,
                  'dissoc': dissoc,
                  'get': get,
                  'contains?': containsQ,
                  'vector': vector,
                  'vector?': types.isVector,
                  'vals': vals,
                  'keys': keys,
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
                  'sequential?': sequentialQ,
                  'apply': apply,
                  'map': map,
                  'conj': conj,
                  'range': range,
                  'cons': cons,
                  'concat': concat,
                  'nth': nth,
                  'first': first,
                  'rest': rest,
                  'with-meta': types.withMeta,
                  'meta': meta
                 };
