var types = require('./types');
var _ = require('lodash');
var logger = require('./logger');

function Env(outer, binds, exprs) {
    var data = {};

    var api = {};

    api.set = function(key, val) {
        data[key] = val;
    };

    api.find = function(key) {
        var found = data[key];
        if (found !== undefined) {
            return found;
        }

        if (outer) {
            return outer.find(key);
        }

        return undefined;
    };

    api.get = function(key) {
        var found = api.find(key);
        if (found !== undefined) {
            return found;
        }
        logger.debug('unable to get:', key, 'in:', data);
        throw new Error('could not find key: ' + key + ' in environment');
    };

    api.keys = function() {
        var outerKeys = (outer && outer.keys()) || [];
        return [Object.keys(data)].concat(outerKeys);
    };

    if (binds) {
        for (var i = 0; i < binds.length; i++) {
            var key = binds[i];
            if (key === '&') {
                var restSym = binds[i+1];
                var restExprs = _.values(exprs).slice(i);
                types.toList(restExprs);
                api.set(restSym, restExprs);
                break;
            }
            var val = exprs[i];
            api.set(key, val);
        }
    }

    return api;
}

exports.Env = Env;
