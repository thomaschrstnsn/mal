var reader = require('./reader.js'),
    read_str = reader.read_str;

var types = require('./types');

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

function let_form(ast, env) {
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
    while (bindings.length > 0) {
        var sym = bindings.shift();
        var unevalVal = bindings.shift();

        if (!types.isSymbol(sym)) {
            throw new Error('expected even element in bindings to let form to be symbol');
        }

        var val = EVAL(unevalVal, letEnv);

        letEnv.set(types.nameOf(sym), val);
    }

    var form = ast[2];
    return EVAL(form, letEnv);
}

function if_form(ast, env) {
    if (ast.length !== 3 && ast.length !== 4) {
        throw new Error('expected 3 or 4 elements in if form');
    }
    var condition = EVAL(ast[1], env);
    var thenUneval = ast[2];
    var elseUneval = ast[3];

    if (condition !== false && condition !== null) {
        return EVAL(thenUneval, env);
    }

    if (elseUneval !== undefined) {
        return EVAL(elseUneval, env);
    }

    return null;
}

function do_form(ast, env) {
    return ast.slice(1).map(function (step) { return EVAL(step, env);}).pop();
}

function fn_form(ast, env) {
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

    return function () {
        return EVAL(body, Env(env, argSyms, arguments));
    };
}

function EVAL(ast, env) {
    if (ast !== null && types.isList(ast)) {
        var first = ast[0];
        if (types.isSymbol(first)) {
            switch (types.nameOf(first)) {
            case 'def!': return define_form(ast, env);
            case 'let*': return let_form(ast, env);
            case 'if':   return if_form(ast, env);
            case 'do':   return do_form(ast, env);
            case 'fn*':  return fn_form(ast, env);
            }
        }

        var evaled = eval_ast(ast, env);
        var func = evaled.shift();
        return func.apply(undefined, evaled);
    }

    return eval_ast(ast, env);
}

var pr_str = require('./printer.js').pr_str;

function PRINT(a) {
    return pr_str(a);
}

var Env = require('./env').Env;

var floorIt = Math.floor;

var repl_env = Env(null);
repl_env.set('+', function (a, b) { return floorIt(a + b);});
repl_env.set('-', function (a, b) { return floorIt(a - b);});
repl_env.set('*', function (a, b) { return floorIt(a * b);});
repl_env.set('/', function (a, b) { return floorIt(a / b);});

function rep(a) {
    return PRINT(EVAL(READ(a), repl_env));
}

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
