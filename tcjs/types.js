function assignType(obj, type, value) {
    return Object.defineProperty(obj, type,
                                 {enumerable: false,
                                  writable: false,
                                  configurable: false,
                                  value: value || true});
}

function inverseStringMap(map) {
    var inv = {};
    for (var key in map) {
        var value = map[key];
        inv[value] = key;
    }
    return inv;
}

var quoteTypes = {quote: '\'',
                  quasiquote: '`',
                  unquote: '~',
                  'splice-unquote': '~@'};

var quoteShortHands = inverseStringMap(quoteTypes);

function quotedForm(form, quoteType) {
    if (!quoteTypes[quoteType]) {
        throw new Error("unknown quotetype: " + quoteType);
    }
    var q = {quoted: form};
    assignType(q, 'malQuoted', quoteType);
    return q;
}

function isQuoted(obj, quoteType) {
    if (obj.malQuoted) {
        if (quoteType) {
            return obj.malQuoted === quoteType;
        }
        return true;
    }
    return false;
}

function quoteType(obj) {
    if (isQuoted(obj)) {
        return obj.malQuoted;
    }
    throw new Error('quoteType called with non-quote');
}

function getQuoted(obj) {
    if (isQuoted(obj)) {
        return obj.quoted;
    }
    throw new Error('unhandled object in getQuoted');
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
    return !!obj.symbol;
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

module.exports = {quoteTypes: quoteTypes,
                  quoteShortHands: quoteShortHands,
                  getQuoted: getQuoted,
                  quotedForm: quotedForm,
                  isQuoted: isQuoted,
                  quoteType: quoteType,
                  toList: toList,
                  isList: isList,
                  toVector: toVector,
                  isVector: isVector,
                  toMap: toMap,
                  isMap: isMap,
                  str2symbol: str2symbol,
                  isSymbol: isSymbol,
                  str2keyword: str2keyword,
                  isKeyword: isKeyword,
                  nameOf: nameOf,
                  isString: isString
                 };
