package org.rascalmpl.library.lang.javascript.m3;

import org.rascalmpl.library.Prelude;

import io.usethesource.vallang.ISourceLocation;
import io.usethesource.vallang.IString;
import io.usethesource.vallang.IValueFactory;
import jdk.nashorn.internal.runtime.Context;
import jdk.nashorn.internal.runtime.ErrorManager;
import jdk.nashorn.internal.runtime.ScriptRuntime;
import jdk.nashorn.internal.runtime.options.Options;

public class JavascriptParser {
    // TODO: use babylon to parse the file instead of nashhorn?
    String program(String file) {
        return "var fs = require('fs');"
             + "var util = require('util');"
             + "var babylon = require('babylon');"
             + "fs.readFile(file+'.js', 'utf8', function(err, code) {"
             + "var AST = babylon.parse(code);"
             + "alert(AST);"
             +  "console.log(util.inspect("
             +    "AST,"
             + "    false, null"
             + "));"
             + "  fs.writeFile(file+'.json', JSON.stringify(AST),"
             +   "function(err) {"
             +       "if (err) throw err;"
             +       "});"
             + "});"
            ;
    }

    protected final IValueFactory values;

    public JavascriptParser(IValueFactory values){
        super();		
        this.values = values;
    }

    private String _parse(String iname, String code) { 
        Options options = new Options("nashorn");
        ErrorManager errors = new ErrorManager();
        Context contextm = new Context(options, errors, Thread.currentThread().getContextClassLoader());
        Context.setGlobal(contextm.createGlobal());
        return ScriptRuntime.parse(code, iname, true);
    }

    public IString _parse(IString iname, IString code) { 
        return values.string(_parse(iname.getValue(), code.getValue()));
    }
    
    public IString _parse(ISourceLocation loc) { 
        return values.string(_parse(loc.getURI().toASCIIString(), ((IString) new Prelude(values).readFile(loc)).getValue()));
    }
}

