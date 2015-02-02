function Env(outer) {
    var data = {};

    var api = {};

    api.set = function(key, val) {
        data[key] = val;
    };

    api.find = function(key) {
        var found = data[key];
        if (found) {
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

        throw new Error('could not find key: ' + key + ' in env');
    };

    api.keys = function() {
        return Object.keys(data);
    };

    return api;
}

exports.Env = Env;
