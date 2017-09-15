package org.rascalmpl.library.lang.javascript.m3;

import io.usethesource.vallang.IString;
import io.usethesource.vallang.IValueFactory;
import jdk.nashorn.internal.runtime.Context;
import jdk.nashorn.internal.runtime.ErrorManager;
import jdk.nashorn.internal.runtime.ScriptRuntime;
import jdk.nashorn.internal.runtime.options.Options;

public class AST {
	String program(String file) {return "var fs = require('fs');"
	+"var util = require('util');"
	+"var babylon = require('babylon');"
    +"fs.readFile(file+'.js', 'utf8', function(err, code) {"
	+"var AST = babylon.parse(code);"
    +"alert(AST);"
	+ "console.log(util.inspect("
	+    "AST,"
	+"    false, null"
	+ "));"
	+ "  fs.writeFile(file+'.json', JSON.stringify(AST),"
	+   "function(err) {"
	+       "if (err) throw err;"
	+       "});"
	+"});"
	;
	}
	
	protected final IValueFactory values;
	
	public AST(IValueFactory values){
		super();		
		this.values = values;
	}

public String _parse(String iname, String code) { 
    		  // values.string(ScriptUtils.parse(code.getValue(), "<unknown>", false));
	    Options options = new Options("nashorn");
	    ErrorManager errors = new ErrorManager();
	    Context contextm = new Context(options, errors, Thread.currentThread().getContextClassLoader());
	    Context.setGlobal(contextm.createGlobal());
		return ScriptRuntime.parse(code, iname, true);
}

public IString _parse(IString iname, IString code) { 
	return values.string(_parse(iname.getValue(), code.getValue()));
}

/*
public String _parse(String file) { 
	  // values.string(ScriptUtils.parse(code.getValue(), "<unknown>", false));
	System.err.println(program(file));
	// System.out.println(program(file.getValue()));
	ScriptEngine engine = new ScriptEngineManager().getEngineByName("nashorn");
	try {
		engine.eval(program(file));
	} catch (ScriptException e) {
		// TODO Auto-generated catch block
		e.printStackTrace();
	}
//return values.string(file.getValue()+".json");
return "aap";
}
*/



static public void main(String[] argv) {
	  AST ast = new AST(null);
	  System.out.println(ast._parse("/noot/aap", ast.program("aap")));
}
}
