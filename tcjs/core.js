var _ = require('lodash');
var types = require('./types');

var floorIt = Math.floor;

function reduceFunc(fn, args) {
    return _.chain(args).values().reduce(fn);
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
    return x.length;
}

function eq(a, b) {
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
                  '>=': gte
                 };
