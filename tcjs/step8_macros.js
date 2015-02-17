var _ = require('lodash');

var reader = require('./reader.js'),
    read_str = reader.read_str;
var types = require('./types');
var pr_str = require('./printer.js').pr_str;

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

function define_macro_form(ast, env) {
    if (ast.length !== 3) {
        throw new Error('expected 3 elements in define form');
    }

    if (!types.isSymbol(ast[1])) {
        throw new Error('expected symbol as key in define form');
    }

    var key = types.nameOf(ast[1]);
    var val = EVAL(ast[2], env);
    env.set(key, types.toMacro(val));

    return val;
}

function get_macro_call(ast, env) {
    if (ast !== undefined && ast !== null && types.isList(ast)) {
        var first = ast[0];
        if (types.isSymbol(first)) {
            var lookedUp = env.find(types.nameOf(first));
            if (lookedUp !== undefined && types.isMacro(lookedUp)) {
                return lookedUp;
            };
        }
    }
    return undefined;
}

function macroexpand(ast, env) {
    var macro = undefined;
    while ((macro = get_macro_call(ast, env)) !== undefined) {
        ast = macro.fn.apply(undefined, ast.slice(1));
    }
    return ast;
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

function is_pair(x) {
    return (types.isList(x) || types.isVector(x)) && x.length > 0;
}

function quasiquote(qAst) {
    if (!is_pair(qAst)) {
        return types.toList([types.str2symbol('quote'), qAst]);
    } else if (types.isThisSymbol(qAst[0], 'unquote')){
        return qAst[1];
    } else if (is_pair(qAst[0]) &&
               types.isThisSymbol(qAst[0][0], 'splice-unquote')) {
        return types.toList([types.str2symbol('concat'),
                             qAst[0][1],
                             types.toVector(qAst.slice(1))]);
    }
    return types.toList([types.str2symbol('cons'),
                         quasiquote(qAst[0]),
                         quasiquote(types.toList(qAst.slice(1)))]);
}

function EVAL(ast, env) {
    while (true) {
        if (ast !== null && ast !== undefined && types.isList(ast)) {
            ast = macroexpand(ast, env);

            if (ast === null || !types.isList(ast)) {
                return ast;
            }

            var first = ast[0];
            if (types.isSymbol(first)) {
                switch (types.nameOf(first)) {
                case 'def!': return define_form(ast, env);
                case 'defmacro!': return define_macro_form(ast, env);
                case 'macroexpand': return macroexpand(ast[1], env);
                case 'quote': return ast[1];
                case 'quasiquote':
                    ast = quasiquote(ast[1]);
                    continue;
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
            console.log('found func:', func);
            throw new Error('expected callable thing as first thing in list being evaled');
        }

        return eval_ast(ast, env);
    }
}

function PRINT(a) {
    return pr_str(a, true);
}

var Env = require('./env').Env;

var repl_env = Env(null);

repl_env.set('eval', function(ast) {
    return EVAL(ast, repl_env);
});

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
rep("(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f)\")\")))))");

rep("(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))");

rep("(defmacro! or (fn* (& xs) (if (empty? xs) nil (if (= 1 (count xs)) (first xs) `(let* (or_FIXME ~(first xs)) (if or_FIXME or_FIXME (or ~@(rest xs))))))))");

function DEBUG_rep(a) {
    console.log('env: ', repl_env.keys());

    var r = READ(a);
    console.log("read:  ", r);

    var e = EVAL(r, repl_env);
    console.log("eval:  ", e);

    var p = PRINT(e);
    console.log("print: ", p);

    return p;
}

var malArgv = process.argv.slice(3);
types.toList(malArgv);

repl_env.set('*ARGV*', malArgv);

function repl() {
    var readline = require('./node_readline.js');
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
}

if (process.argv[2] === 'debug') {
    console.log("debugging REPL");
    rep = DEBUG_rep;
    repl();
} else if (process.argv.length > 2) {
    var fileToLoad = process.argv[2];
    rep('(load-file "' + fileToLoad + '")');
} else {
    repl();
}
