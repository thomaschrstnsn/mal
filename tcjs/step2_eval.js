var _ = require('lodash');
var reader = require('./reader.js'),
    read_str = reader.read_str;

var types = require('./types');

function READ(a) {
    return read_str(a);
}

function eval_ast(ast, env) {
    if (types.isSymbol(ast)) {
        var name = types.nameOf(ast);
        var el = env[name];

        if (!el) {
            throw new Error("error looking up symbol: " + name);
        }
        return el;
    }
    if (types.isList(ast) || types.isVector(ast)) {
        var res = ast.map(function (x) {
            return EVAL(x, env);
        });
        if (types.isList(ast)) {
            types.toList(res);
        }
        if (types.isVector(ast)) {
            types.toVector(res);
        }
        return res;
    }
    if (types.isMap(ast)) {
        var res = {};
        _.forEach(ast, function (val, key) {
            var eKey = EVAL(key, env);
            var eVal = EVAL(val, env);
            res[eKey] = eVal;
        });
        types.toMap(res);
        return res;
    }
    return ast;
}

function EVAL(ast, env) {
    if (types.isList(ast)) {
        var evaled = eval_ast(ast, env);
        var func = evaled.shift();
        return func.apply(undefined, evaled);
    }
    return eval_ast(ast, env);
}

var pr_str = require('./printer.js').pr_str;

function PRINT(a) {
    return pr_str(a, true);
}

function DEBUG_rep(a) {
    var r = READ(a);
    console.log("read:  ", r);

    var e = EVAL(r);
    console.log("eval:  ", e);

    var p = PRINT(e);
    console.log("print: ", p);

    return p;
}

var floorIt = Math.floor;

var repl_env = {'+': function (a, b) { return floorIt(a + b);},
                '-': function (a, b) { return floorIt(a - b);},
                '*': function (a, b) { return floorIt(a * b);},
                '/': function (a, b) { return floorIt(a / b);}
               };

function rep(a) {
    return PRINT(EVAL(READ(a), repl_env));
}

var readline = require('./node_readline.js');

if (process.argv[2] === 'debug') {
    console.log("debugging REPL");
    rep = DEBUG_rep;
}

// repl loop
if (typeof require !== 'undefined' && require.main === module) {
    // Synchronous node.js commandline mode
    while (true) {
        var line = readline.readline("mal-user> ");
        if (line === null) { break; }
        try {
            if (line) { console.log(rep(line)); }
        } catch (exc) {
            if (exc instanceof reader.BlankException) { continue; }
            if (exc.stack) { console.log(exc.stack); }
            else           { console.log(exc); }
        }
    }
}
