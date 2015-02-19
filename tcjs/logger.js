var _ = require('lodash');

function Logger() {
    var LEVELS = {SILENT: 0, INFO: 1, DEBUG: 2};
    var level = 'INFO';

    function logIfLevelAtLeast(minLevel, args) {
        if (LEVELS[level] >= minLevel) {
            return console.log.apply(console, args);
        }
        return undefined;
    }

    function debug() {
        return logIfLevelAtLeast(LEVELS.DEBUG, arguments);
    }

    function info() {
        return logIfLevelAtLeast(LEVELS.INFO, arguments);
    }

    function exception(exc) {
        switch(level) {
        case 'SILENT': break;
        case 'INFO': return info(exc);
        case 'DEBUG':
            if (exc.stack) { return debug(exc.stack);}
            else           { return debug(exc); }
        }
        return undefined;
    }

    function setLevel(l) {
        if (LEVELS[l] !== undefined) {
            level = l;
        } else throw new Error('unknown level: ' + l);
    }

    function getLevel() {
        return level;
    }

    return {debug: debug,
            info: info,
            exception: exception,
            LEVELS: _.chain(LEVELS).keys().value(),
            setLevel: setLevel,
            getLevel: getLevel
           };
};

module.exports = Logger();
