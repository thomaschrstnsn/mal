var _ = require('lodash');

var TYPE_KEY = '__malType';
var META_KEY = '__malMeta';

var LIST_TYPE = 'malList';
var VECTOR_TYPE = 'malVector';
var MAP_TYPE = 'malMap';
var MACRO_TYPE = 'malMacro';
var ATOM_TYPE = 'malAtom';

function staticPropDef(value) {
    return {enumerable: false,
            writable: false,
            configurable: false,
            value: value};
}

function getType(obj) {
    return obj[TYPE_KEY];
}

function assignType(obj, type) {
    return Object.defineProperty(obj, TYPE_KEY, staticPropDef(type));
}

function getMeta(obj) {
    return obj[META_KEY];
}

function assignMeta(obj, meta) {
    return Object.defineProperty(obj, META_KEY, staticPropDef(meta));
}

function withMeta(obj, meta) {
    return assignMeta(clone(obj), meta);
}

function clone(obj) {
    return assignType(_.clone(obj), getType(obj));
}

function toList(obj) {
    return assignType(obj, LIST_TYPE);
}

function isList(obj) {
    return getType(obj) === LIST_TYPE;
}

function toVector(obj) {
    return assignType(obj, VECTOR_TYPE);
}

function isVector(obj) {
    return getType(obj) === VECTOR_TYPE;
}

function toMap(obj) {
    return assignType(obj, MAP_TYPE);
}

function isMap(obj) {
    return getType(obj) === MAP_TYPE;
}

function toMacro(obj) {
    return assignType(obj, MACRO_TYPE);
}

function isMacro(obj) {
    return getType(obj) === MACRO_TYPE;
}

function str2symbol(str) {
    return {symbol: str};
}

function isSymbol(obj) {
    if (obj === undefined || obj === null) {
        return false;
    }
    return !!obj.symbol;
}

function isThisSymbol(obj, expSymbol) {
    if (obj === undefined || obj === null) {
        return false;
    }
    return isSymbol(obj) && expSymbol === nameOf(obj);
}

var keywordMarker = '\u200B';

function str2keyword(str) {
    return keywordMarker + str;
}

function isKeyword(obj) {
    return obj[0] === keywordMarker;
}

function nameOf(obj) {
    if (isSymbol(obj)) {
        return obj.symbol;
    }
    if (isKeyword(obj)) {
        return obj.substr(1);
    }
    throw new Error("unhandled object in nameOf");
}

function isString(obj) {
    return typeof obj === 'string' && !isKeyword(obj);
}

function wrapInAtom(obj) {
    return assignType({atomValue: obj}, ATOM_TYPE);
}

function isAtom(obj) {
    return getType(obj) === ATOM_TYPE;
}

function valueOfAtom(obj) {
    if (!isAtom(obj)) {
        throw new Error("derefing non atom");
    }
    return obj.atomValue;
}

function setAtom(atom, newValue) {
    if (!isAtom(atom)) {
        throw new Error("expected atom");
    }
    atom.atomValue = newValue;
}

function toError(val) {
    return {malError: val};
}

function isError(obj) {
    return obj.malError !== undefined;
}

function valueOfError(obj) {
    return obj.malError;
}

module.exports = {withMeta: withMeta,
                  getMeta: getMeta,
                  clone: clone,
                  toList: toList,
                  isList: isList,
                  toVector: toVector,
                  isVector: isVector,
                  toMap: toMap,
                  isMap: isMap,
                  toMacro: toMacro,
                  isMacro: isMacro,
                  str2symbol: str2symbol,
                  isSymbol: isSymbol,
                  isThisSymbol: isThisSymbol,
                  str2keyword: str2keyword,
                  isKeyword: isKeyword,
                  nameOf: nameOf,
                  isString: isString,
                  wrapInAtom: wrapInAtom,
                  valueOfAtom: valueOfAtom,
                  isAtom: isAtom,
                  setAtom: setAtom,
                  toError: toError,
                  isError: isError,
                  valueOfError: valueOfError
                 };
