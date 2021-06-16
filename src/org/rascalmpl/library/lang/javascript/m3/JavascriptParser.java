/*
 * Copyright (c) 2017, Bert Lisser - Centrum Wiskunde & Informatica (CWI)
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
 * 1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.  
 */
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

    public IString _parse(ISourceLocation loc, IString code) { 
        return values.string(_parse(loc.getURI().toASCIIString(), code.getValue()));
    }
    
    public IString _parse(ISourceLocation loc) { 
    	
        return values.string(_parse(loc.getURI().toASCIIString(), Prelude.readFile(values, false, loc).getValue()));
    }
}

