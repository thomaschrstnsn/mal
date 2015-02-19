var _ = require('lodash');

var reader = require('./reader'),
    read_str = reader.read_str;

var types = require('./types');
var logger = require('./logger');

function READ(a) {
    return read_str(a);
}

function eval_ast(ast, env) {
    if (ast === null) { return null;}

    if (types.isSymbol(ast)) {
        var symbol = types.nameOf(ast);
        return env.get(symbol);
    }
    if (types.isList(ast) || types.isVector(ast)) {
        var res = ast.map(function (x) {
            return EVAL(x, env);
        });
        if (types.isList(ast)) {
            types.toList(res);
        }
        else {
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

function define_form(ast, env) {
    if (ast.length !== 3) {
        throw new Error('expected 3 elements in define form');
    }

    if (!types.isSymbol(ast[1])) {
        throw new Error('expected symbol as key in define form');
    }

    var key = types.nameOf(ast[1]);
    var val = EVAL(ast[2], env);
    env.set(key, val);

    return val;
}

function let_form_tco(ast, env) {
    if (ast.length !== 3) {
        throw new Error('expected 3 elements in let* form');
    }
    var bindings = ast[1];

    if (!types.isList(bindings) && !types.isVector(bindings)) {
        throw new Error('expected second element in let* form to be list or vector');
    }

    if (!(types.length % 2) === 0) {
        throw new Error('expected even number of elements in bindings to let form');
    }

    var letEnv = Env(env);
    for (var i = 0; i < bindings.length; i++) {
        var sym = bindings[i++];
        var unevalVal = bindings[i];

        if (!types.isSymbol(sym)) {
            throw new Error('expected even element in bindings to let form to be symbol');
        }

        var val = EVAL(unevalVal, letEnv);

        letEnv.set(types.nameOf(sym), val);
    }

    var form = ast[2];
    return [form, letEnv];
}

function if_form_tco(ast, env) {
    if (ast.length !== 3 && ast.length !== 4) {
        throw new Error('expected 3 or 4 elements in if form');
    }
    var condition = EVAL(ast[1], env);
    var thenUneval = ast[2];
    var elseUneval = ast[3];

    if (condition !== false && condition !== null) {
        return [thenUneval, env];
    }

    if (elseUneval !== undefined) {
        return [elseUneval, env];
    }

    return [null, env];
}

function do_form_tco(ast, env) {
    ast.slice(1, -1).map(function (step) { return EVAL(step, env);});
    return [ast.slice(-1)[0], env];
}

function fn_form_tco(ast, env) {
    if (ast.length !== 3) {
        throw new Error('expected 3 elements in fn form');
    }

    var argList = ast[1];

    if (!types.isList(argList) && !types.isVector(argList)) {
        throw new Error('expected list/vector of argument bindings');
    }

    if (!argList.map(types.isSymbol).reduce(function(prev, cur) {
        return prev && cur;
    }, true)) {
        throw new Error('expected only symbols in argument bindings');
    }

    var body = ast[2];
    var argSyms = argList.map(types.nameOf);

    return [{fn: function () {return EVAL(body, Env(env, argSyms, arguments));},
            ast: body,
            params: argSyms,
            env: env
            }, env];
}

function EVAL(ast, env) {
    while (true) {
        if (ast !== null && ast !== undefined && types.isList(ast)) {
            var first = ast[0];
            if (types.isSymbol(first)) {
                if (types.nameOf(first) === 'def!') {
                    return define_form(ast, env);
                }

                var tcoAstEnv = null;
                switch (types.nameOf(first)) {
                case 'let*':
                    tcoAstEnv = let_form_tco(ast, env);
                    break;
                case 'do':
                    tcoAstEnv = do_form_tco(ast, env);
                    break;
                case 'if':
                    tcoAstEnv = if_form_tco(ast, env);
                    break;
                case 'fn*':
                    tcoAstEnv = fn_form_tco(ast, env);
                    break;
                }

                if (tcoAstEnv) {
                    ast = tcoAstEnv[0];
                    env = tcoAstEnv[1];
                    continue;
                }
            }

            var evaled = eval_ast(ast, env);
            var func = evaled[0];
            if (typeof func === 'function') {
                return func.apply(undefined, evaled.slice(1));
            }
            if (func.ast && func.fn && func.params && func.env) {
                ast = func.ast;
                env = Env(func.env, func.params, evaled.slice(1));
                continue;
            }
            logger.debug('found func:', func);
            throw new Error('expected callable thing as first thing in list being evaled');
        }

        return eval_ast(ast, env);
    }
}

var pr_str = require('./printer').pr_str;

function PRINT(a) {
    return pr_str(a, true);
}

var Env = require('./env').Env;

var repl_env = Env(null);

function import_ns(ns, env) {
    _.forEach(ns, function (val, sym) {
        env.set(sym, val);
    });
}

var core = require('./core');

import_ns(core, repl_env);

function rep(a) {
    return PRINT(EVAL(READ(a), repl_env));
}

rep("(def! not (fn* (a) (if a false true)))");

function DEBUG_rep(a) {
    logger.debug('env: ', repl_env.keys());

    var r = READ(a);
    logger.debug("read:  ", r);

    var e = EVAL(r, repl_env);
    logger.debug("eval:  ", e);

    var p = PRINT(e);
    logger.debug("print: ", p);

    return p;
}

var readline = require('./node_readline');

if (process.argv[2] === 'debug') {
    logger.setLevel('DEBUG');
    logger.debug("debugging REPL");
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
            logger.exception(exc);
        }
    }
}
