function assignType(obj, type, value) {
    return Object.defineProperty(obj, type,
                                 {enumerable: false,
                                  writable: false,
                                  configurable: false,
                                  value: value || true});
}

function toList(obj) {
    return assignType(obj, 'malList');
}

function isList(obj) {
    return !!obj.malList;
}

function toVector(obj) {
    return assignType(obj, 'malVector');
}

function isVector(obj) {
    return !!obj.malVector;
}

function toMap(obj) {
    return assignType(obj, 'malMap');
}

function isMap(obj) {
    return !!obj.malMap;
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

module.exports = {toList: toList,
                  isList: isList,
                  toVector: toVector,
                  isVector: isVector,
                  toMap: toMap,
                  isMap: isMap,
                  str2symbol: str2symbol,
                  isSymbol: isSymbol,
                  isThisSymbol: isThisSymbol,
                  str2keyword: str2keyword,
                  isKeyword: isKeyword,
                  nameOf: nameOf,
                  isString: isString
                 };
