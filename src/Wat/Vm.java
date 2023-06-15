package Wat;

import static List.Parser.parse;
import static Wat.Utility.$;
import static Wat.Utility.apply;
import static Wat.Utility.binOp;
import static Wat.Utility.eIf;
import static Wat.Utility.eIfnull;
import static Wat.Utility.getClasses;
import static Wat.Utility.getExecutable;
import static Wat.Utility.getField;
import static Wat.Utility.headAdd;
import static Wat.Utility.ifnull;
import static Wat.Utility.isInstance;
import static Wat.Utility.or;
import static Wat.Utility.read;
import static Wat.Utility.reorg;
import static Wat.Utility.stackDeep;
import static Wat.Utility.uncked;
import static Wat.Utility.Binop.And;
import static Wat.Utility.Binop.Dvd;
import static Wat.Utility.Binop.Ge;
import static Wat.Utility.Binop.Gt;
import static Wat.Utility.Binop.Le;
import static Wat.Utility.Binop.Ls;
import static Wat.Utility.Binop.Mns;
import static Wat.Utility.Binop.Or;
import static Wat.Utility.Binop.Pls;
import static Wat.Utility.Binop.Pwr;
import static Wat.Utility.Binop.Rst;
import static Wat.Utility.Binop.Sl;
import static Wat.Utility.Binop.Sr;
import static Wat.Utility.Binop.Sr0;
import static Wat.Utility.Binop.Xor;
import static java.lang.Runtime.version;
import static java.lang.System.currentTimeMillis;
import static java.lang.System.out;
import static java.util.Arrays.stream;
import static java.util.stream.Collectors.joining;
import static java.util.stream.Stream.of;
import static javax.tools.ToolProvider.getSystemJavaCompiler;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.lang.reflect.Array;
import java.lang.reflect.Constructor;
import java.lang.reflect.Executable;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.URI;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.regex.Pattern;

import javax.tools.DiagnosticCollector;
import javax.tools.JavaFileObject;
import javax.tools.SimpleJavaFileObject;

// java.exe -cp bin --enable-preview Wat.Vm

/* Abbreviations:
	c: cons
	x, exp, expt, cln: expression, expexted, cleaner
	xs: expressions
	op: operator
	o, os: operands
	o0, o1, ..: operand 0, 1, ..
	cmb: combiner
	opv: operative combiner
	apv: applicative combiner
	apv0, tnk: 0 args applicative combiner, thunk
	apv1, hdl: 1 arg applicative combiner, handler
	p: parameter
	pt: parameters tree
	arg: argument
	args: arguments
	e: environment
	de: environment
	eo: environment operand
	ep: environment parameter
	xe: extended environment
	k, nxt: continuation
	s: sospension
	r: resumption
	f: function
	s: supplier
	err: error
	exc: exception
	id: identifier
	num: number
	str: string
	stx: syntax
	sym: symbol
	key: keyword
	cmt: comment
	dbg: debugging information
	err: error
	v, val: value
	res: result
	thw: throwable
	prp: prompt
	hdl: handler
 */

public class Vm {

	static {
	    for (File file: new File("bin/Ext").listFiles()) file.delete();
	}

	boolean dotco = true; // do tco
	boolean doasrt = true; // do assert
	boolean ctapv = false; // applicative catch & throw
	boolean instr = false; // intern string
	boolean prstk = false; // print stack
	boolean prwrn = false; // print warning
	boolean aquote = false; // auto quote list
	
	int prtrc = 0; // print trace: 0:none, 1:load, 2:eval root, 3:eval all, 4:return, 5:combine, 6:bind/lookup
	int ttrue = 0; // type true: 0:true, 1:!false, 2:!(or false null), 3:!(or false null inert), 4:!(or false null inert zero)
	int prenv = 3; // print environment
	int bndres = 0; // bind result: 0:inert 1:rhs 2:prev
	private Object bndres(Object o) {
		return switch (o) {
			case null-> bndres;
			case Inert i-> 0; 
			case Keyword k-> switch (k.name) {
				case "rhs"-> 1;
				case "prv"-> 2;
				default-> typeError("invalid keyword: {datum}", k, parseBytecode("or", ":rhs", ":prv"));
			};
			default-> typeError("not a #null, #inert, :rhs or :prv: {datum}", o, parseBytecode("or", "Null", "Inert", ":rhs", ":prv") );
		};
	}
	
	
	// Continuations
	class Continuation {
		Function<Resumption, Object> f; Continuation nxt; Dbg dbg;
		Continuation(Dbg dbg, Function<Resumption, Object> f, Continuation next) {
			this.f = f; this.nxt = next; this.dbg = dbg;
		}
		public String toString() { return "{Continuation %s}".formatted(dbg); }
		Object apply(Env e, Apv apv0) { return apply(()-> combine(e, apv0, null)); }
		Object apply(Supplier s) { return f.apply(new Resumption(nxt, s));}
	}
	class Resumption {
		Continuation k; Supplier<Object> s;
		Resumption(Continuation k, Supplier<Object> s) { this.k=k; this.s=s; }
		public String toString() { return "{Resumption %s %s}".formatted(s, k); }
		<T> T resume() { var k = this.k; this.k = k.nxt; return getTco(k.f.apply(this)); }
	};
	class Suspension {
		Object prp; Combinable hdl; Continuation k;
		Suspension(Object prp, Combinable hdl) { this.prp = prp; this.hdl = hdl; }
		public String toString() { return "{Suspension %s %s %s}".formatted(prp, hdl, k); }
		Suspension suspend(Dbg dbg, Function<Resumption, Object> f) { 
			k = new Continuation(dbg, f, k); return this;
		}
	}
	Object pipe(Dbg dbg, Supplier before, Function ... after) {
		return pipe(null, dbg, before, after);
	}
	Object pipe(Resumption r, Dbg dbg, Supplier before, Function ... after) {
		var res = r != null ? r.resume() : before.get();
		//if (res instanceof Tco) out.println /*throw new Error*/("do getTco 1"); 
		return res instanceof Suspension s ? s.suspend(dbg, rr-> pipe(rr, dbg, before, after)) : pipe(null, 0, dbg, res, after);
	}
	Object pipe(Resumption r, int i, Dbg dbg, Object res, Function ... after) {
		for (var first=true; i<after.length; i+=1) { // only one resume for suspension
			res = first && r != null && !(first = false) ? r.resume() : after[i].apply(res);
			//if (res instanceof Tco && i < after.length-1) out.println /*throw new Error*/ ("do getTco 2"); 
			if (res instanceof Suspension s) { var ii=i; var rres=res; return s.suspend(dbg, rr-> pipe(rr, ii, dbg, rres, after)); }
		}
		return res;
	}
	Object map(Function f, List todo) {
		return map(null, null, f, todo);
	}
	Object map(Resumption r, List done, Function f, List todo) {
		for (var first=true;;) { // only one resume for suspension
			if (todo == null) return reverse(done); 
			var res = first && r != null && !(first = false) ? r.resume() : f.apply(todo.car());
			//if (res instanceof Tco) out.println /*throw new Error*/("do getTco 3"); 
			if (res instanceof Suspension s) { List td=todo, dn=done; return s.suspend(dbg(null, "map", todo.car), rr-> map(rr, dn, f, td)); }
			todo = todo.cdr(); done = cons(res, done);
		}
	}
	class Dbg {
		Env e; Object op; Object[] os;
		Dbg (Env e, Object op, Object ... os) {
			this.e = e; this.op = op; this.os = os; 
		}
		public String toString() {
			var s = op instanceof List l && l.car instanceof Begin b && b.root ? "(%Begin* ...)" : Vm.this.toString(op);
			for (var o: os) s += " " + Vm.this.toString(o);
			return s;
		} 
	};
	Dbg dbg (Env e, Object op, Object ... os) { return new Dbg(e, op, os); }
	
	
	// Basic Forms
	class Inert { public String toString() { return "#inert"; }};
	public Inert inert = new Inert();
	Inert inert(Object ... values) { return inert; }
	
	class Ignore { public String toString() { return "#ignore"; }};
	public Ignore ignore = new Ignore();
	
	
	// Tail Call Optimization
	interface Tco extends Supplier {};
	Object tco(Tco tco) { return dotco ? tco : tco.get(); }
	/* utile per debug!
	class Tco implements Supplier {
		Supplier tco;
		Tco(Supplier tco) { this.tco = tco; }
		@Override public Object get() { return tco.get(); }
		@Override public String toString() { return "Tco"; }
	};
	Object tco(Supplier tco) { return dotco ? new Tco(tco) : tco.get(); }
	*/
	<T> T getTco(Object o) { while (o instanceof Tco tco) o = tco.get(); return (T) o; }
	
	
	// Trace Log
	int level=0, start=0; String indent = "|  ";
	String indent() { return indent.repeat(level-start) + "|" + stackDeep() + ":  " ; }
	
	
	// Evaluation Core
	<T> T evaluate(Env e, Object o) {
		if (prtrc >= 3) print("evaluate: ", indent(), o, "   ", e);
		Object v; try {
			level += 1;
			v = switch (o) {
				case Symbol s-> tco(()-> pipe(dbg(e, o), ()-> e.lookup(s)));
				case List l-> tco(()-> pipe(dbg(e, o), ()-> getTco(evaluate(e, l.car)), op-> tco(()-> combine(e, op, l.cdr()))));
				case null, default-> o;
			};
		}
		finally {
			level -= 1;
		}
		if (prtrc >= 4) print("  return: ", indent(), v=getTco(v)); 
		return (T) v;
	}
	
	abstract class Intern {
		String name;
		Intern(String name) { this.name = name; }
		public String toString() { return getClass() == Keyword.class ? ":" + name : name; }
		public int hashCode() { return Objects.hashCode(name); }
		public boolean equals(Object o) {
			return this == o || getClass().isInstance(o) && name.equals(((Intern) o).name);
		}		
	}
	class Keyword extends Intern { Keyword(String name) { super(name); }}
	Keyword keyword(String name) { return intern(name.startsWith(":") ? name : ":" + name); }
	class Symbol extends Intern { Symbol(String name) { super(name); }}
	Symbol symbol(String name) { return intern(name.startsWith(":") ? name.substring(1) : name); }
	
	class Cons {
		Object car, cdr;
		Cons(Object car, Object cdr) { this.car = car; this.cdr = cdr; }
		public String toString() { return "(" + toString(this) + ")"; }
		private String toString(Cons c) {
			var car = c.car == null ? "()" : Vm.this.toString(true, c.car);
			if (c.cdr == null) return car;
			if (c.cdr instanceof Cons cdr) return car + " " + toString(cdr);
			return car + " . " + Vm.this.toString(true, c.cdr);
		}
		public boolean equals(Object o) {
			return this == o || o instanceof Cons c && Vm.this.equals(this.car,  c.car) && Vm.this.equals(this.cdr,  c.cdr);
		}
		public <T> T car() { return (T) car; }
		public <T> T cdr() { return (T) cdr; }
		public <T> T car(int i) { Cons o=this; for (; i>0 && o.cdr instanceof Cons c; i-=1, o=c); return i==0 ? o.car() : typeError("not a cons: {datum}", o.cdr, symbol("Cons")); }
		public <T> T cdr(int i) { Cons o=this; for (; i>0 && o.cdr instanceof Cons c; i-=1, o=c); return i==0 ? o.cdr() : typeError("not a cons: {datum}", o.cdr, symbol("Cons")); }
		Object setCar(Object car) { return this.car = car; }
		Object setCdr(Object cdr) { return this.cdr = cdr; }
	}
	public class List extends Cons {
		List(Object car, List cdr) { super(car, cdr); }
		@Override public List cdr() { return (List) cdr; }
		List setCdr(List cdr) { return (List)(this.cdr = cdr); }
		@Override Object setCdr(Object cdr) { return cdr == null || cdr instanceof List ? this.cdr = cdr : typeError("not a list: {datum}", cdr, symbol("List") ); }
	}
	public int len(List o) { int i=0; for (; o != null; i+=1, o=o.cdr()); return i; }
	<T extends Cons> T cons(Object car, Object cdr) {
		return (T)(cdr == null || cdr instanceof List ? new List(car, (List) cdr) : new Cons(car, cdr));
	}
	
	
	// Environment
	class Env<K> {
		Env parent; Map<K,Object> map = new LinkedHashMap(); 
		Env(Env parent) { this.parent = parent; }
		record Lookup(boolean isBound, Object value) {}
		Lookup get(K name) {
			for (var env=this; env != null; env=env.parent) {
				Object res = env.map.get(name);
				if (res != null || env.map.containsKey(name)) return new Lookup(true, res);
			}
			return new Lookup(false, null);
		};
		Object set(K key, Object value) {
			for (var env=this; env != null; env=env.parent) {
				if (!env.map.containsKey(key)) continue;
				if (prtrc >= 6) print("     set: ", key, "=", value, " in: ", env);
				return env.def(key, value);
			}
			return error("unbound symbol: {symbol}", "type", symbol("unboundSymbol"), "symbol", key, "environment", this);
		};
		Object def(K key, Object value) {
			if (prtrc >= 6) print("    bind: ", key, "=", value, " in: ", this);
			return map.put(key, value);
		}
		public String toString() {
			var deep = deep();
			var prefix = switch(deep) { case 1-> "Vm"; case 2-> "The"; default-> ""; };
			return "{" + prefix + "Env[" + map.size() + "]" + eIf(deep < prenv, ()-> reverseMap(map)) + eIfnull(parent, ()-> " " + parent) + "}";
		}
		Object lookup(K key) {
			var lookup = get(key);
			if (!lookup.isBound) return error("unbound symbol: {symbol}", "type", symbol("unboundSymbol"), "symbol", key, "environment", this);
			if (prtrc >= 6) print("  lookup: ", lookup.value); return lookup.value;
		}
		boolean isParent(Env other) {
			for (var env=this; env != null; env=env.parent) if (other == env) return true; 
			return false;
		};
		int deep() { int i=0; for (var env=this; env != null; env=env.parent) i+=1; return i; }
	}
	Env env(Env parent) { return new Env<Symbol>(parent); }
	
	
	// Classes and Objects
	public class Box implements ArgsList {
		Object value;
		public Box (Object val) { this.value = val; }
		@Override public Object apply(List o) {
 			var chk = checkR(this, o, 0, 2);
			if (chk instanceof Suspension s) return s;
			if (!(chk instanceof Integer len)) return typeError("not an integer: {datum}", chk, symbol("Integer"));
			return switch (len) {
				case 0-> value;
				default-> switch (bndres(len != 2 ? null : o.car())) {
					case Suspension s-> s;
					case Integer i-> switch(i) {
						case 0-> inert(value = o.car(len-1));
						case 1-> value = o.car(len-1);
						case 2-> { var v = value; value = o.car(len-1); yield v; }
						default-> typeError("invalid bndret value: {datum}", i, parseBytecode("or", 0, 1, 2));
					};
					case Object obj-> typeError("not an integer: {datum}", obj, symbol("Integer"));
			    }; 
			};
		}
		public String toString() { return "{" + getClass().getSimpleName() + " " + Vm.this.toString(value) + "}"; }
	}
	public class Obj extends RuntimeException implements ArgsList {
		private static final long serialVersionUID = 1L;
		Map<String,Object> map = new LinkedHashMap();
		public Obj(String msg, Object ... objs) { super(msg); put(objs); }
		public Obj(String msg, Throwable t, Object ... objs) { super(msg, t); put(objs); }
		public Obj(Throwable t, Object ... objs) { super(t); put(objs); }
		public Obj(Object ... objs) { put(objs); }
		public Object get(Object key) { return map.get(toKey(key)); }
		boolean isBound(Object key) { return map.containsKey(toKey(key)); }
		public Object put(Object ... objs) {
			if (objs == null) return null;
			Object last = null;
			for (int i=0, e=objs.length-1; i<=e; i+=1) {
				var key = toKey(objs[i]);
	        	if (i == e) throw new Error("one value expected at end of: " + list(objs));
	        	last = map.put(key, objs[i+=1]);
	        }
			return last;
		}
		public String toKey(Object key) {
			return switch (key) {
				case Intern i-> i.name;
				case Object o-> Utility.apply(s-> s.startsWith(":") ? s.substring(1) : s, o instanceof String s ? s : Vm.this.toString(o));
			};
		}
		@Override public String toString() {
			var field = map.toString().substring(1);
			return "{&" + getClass().getCanonicalName() // or .getSimpleName()?
				+ eIfnull(getMessage(), m-> " " + Vm.this.toString(true, m))
				+ eIfnull(getCause(), t-> " " + t.getClass().getSimpleName())
				+ (field.length() == 1 ? field : " " + field) ;
		}
		@Override public Object apply(List o) {
			var chk = checkR(this, o, 1, 3, or(Keyword.class,Symbol.class,String.class)); // (key) | (key value) | (key :key value)
			if (chk instanceof Suspension s) return s;
			if (!(chk instanceof Integer len)) return typeError("not an integer: {datum}", chk, symbol("Integer"));
			var key = toKey(o.car());
			return switch (len) { 
				case 1-> Utility.apply(v-> v != null || map.containsKey(key) ? v : error("slot non found: {slotName} in: {object}", "type", symbol("unboundSlot"), "slotName", o.car(), "object", this), map.get(key));
				default-> switch (bndres(len != 3 ? null : o.car(1))) {
					case Suspension s-> s;
					case Integer i-> switch(i) {
						case 0-> inert(put(key, o.car(len-1)));
						case 1-> { var v = o.car(len-1); put(key, v); yield v; }
						case 2-> put(key, o.car(len-1));
						default-> typeError("invalid bndret value: {datum}", i, parseBytecode("or", 0, 1, 2));
					};
					case Object obj-> typeError("not an integer: {datum}", obj, symbol("Integer"));
			    }; 
			};
		}
		private static final Pattern keyword = Pattern.compile("\\{(.+?)\\}");
		@Override public String getMessage() {
			var msg = super.getMessage(); if (msg == null) return null;
			var matcher = keyword.matcher(msg); if (!matcher.find()) return msg;
			var sb = new StringBuffer(); do {
				String s = matcher.group(1), k = toKey(s); var v = map.get(k);
				matcher.appendReplacement(sb, Vm.this.toString(v != null || map.containsKey(k) ? v :  "{"+ s +"}" ).replace("\\", "\\\\"));
			} while(matcher.find());
			matcher.appendTail(sb);
			return sb.toString();
		}
	}
	public class Condition extends Obj {
		private static final long serialVersionUID = 1L;
		public Condition(Object ... objs) { super(objs); }
		public Condition(Throwable cause, Object ... objs) { super(cause, objs); }
		public Condition(String message, Object ... objs) { super(message, objs); }
		public Condition(String message, Throwable cause, Object ... objs) { super(message, cause, objs); }
	}
	Class extend(Symbol className, Class superClass) {
		try {
			if (!className.name.matches("[A-Z][a-zA-Z_0-9]*")) return typeError("invalid class name: {datum}", className, parseBytecode("regex", "[A-Z][a-zA-Z_0-9]*"));
			if (superClass != null && !of(Obj.class, Box.class).anyMatch(rc-> rc.isAssignableFrom(superClass))) return typeError("invalid class: {datum}", superClass, parseBytecode("or", Obj.class, Box.class));
			var c = Class.forName("Ext." + className);
			if (prwrn) out.println("Warning: class " + className + " is already defined!");
			return c;
		}
		catch (ClassNotFoundException e) {
			try {
				class JavaStringFile extends SimpleJavaFileObject {
				    final String code;
				    JavaStringFile(String name, String code) {
				        super(URI.create("string:///" + name.replace('.','/') + Kind.SOURCE.extension), Kind.SOURCE); this.code = code;
				    }
				    @Override public CharSequence getCharContent(boolean ignoreEncodingErrors) { return code; }
				}
			    var diagnostics = new DiagnosticCollector<JavaFileObject>();
			    var isObj = superClass == null || superClass == Obj.class;
			    var isBox = superClass != null && Box.class.isAssignableFrom(superClass);
			    var task = getSystemJavaCompiler().getTask(
			    	null, null, diagnostics,
			    	java.util.List.of("-d", "bin", "--enable-preview", "-source", ""+version().feature(), "-Xlint:unchecked" ),
			    	null,
			    	java.util.List.of(
				    	new JavaStringFile("Ext." + className, ( """
							package Ext;
							import Wat.Vm;
							public class %s extends %s {
							"""
							+  eIf(isBox, """
								public %1$s(Vm vm, String s, %3$s o) { %4$ss, o); }
				    			public %1$s(Vm vm, Throwable t, %3$s o) { %4$st, o); }
				    			public %1$s(Vm vm, String s, Throwable t, %3$s o) {	%4$ss, t, o); }
							""" )
							/* + eIf(!isBox || Condition.class.isAssignableFrom(superClass), """
				    	 	 	public %1$s(Vm vm, %3$s o) { %4$so); }
				    	 	""" ) */
							+ """
				    	 	 	 public %1$s(Vm vm, %3$s o) { %4$so); }
							}
							""").formatted(
								className,
								isObj ? "Vm.Obj" : superClass.getCanonicalName(),
								isBox ? "Object" : "Object ... ", 
								isObj || Utility.equals(superClass, Condition.class, Error.class, Box.class) ? "vm.super(" : "super(vm, "
							)
						)
			    	)
			    );
			    if (!task.call()) {
			    	System.out.println(diagnostics.getDiagnostics());
			    	return error("defining class " + className);
			    }
				new File("bin/Ext/" + className + ".class").deleteOnExit();
				return Class.forName("Ext." + className);
			}
			catch (Throwable thw) {
				return error("defining class: " + className, thw);
			}
		}
	}
	
	
	// Methods
	Map<Class, Map<Symbol,Object>> methods = new LinkedHashMap<>();
	Object addMethod(Class cls, Symbol name, Object method) {
		return methods.computeIfAbsent(cls, k-> new LinkedHashMap<>()).put(name, method);
	}
	Object getMethod(Class cls, Symbol name) {
		var c = cls; do {
			var ms = methods.get(c); if (ms == null) continue;
			var m = ms.get(name); if (m != null) return m;
		} while (c != null && (c = c.getSuperclass()) != null);
		return error("method: {methodName} not found for: {class}!", "type", symbol("unboundMethod"), "methodName", name, "class", cls);
	}	
	
	
	// Bind
	Object bind(Dbg dbg, Env e, Object lhs, Object rhs) {
		return bind(dbg, true, bndres, e, lhs, rhs);
	}
	Object bind(Dbg dbg, boolean def, int bndret, Env e, Object lhs, Object rhs) {
		try {
		  var v = bind(def, bndret, e, lhs, rhs);
		  return bndret == 0 ? inert : v;
		}
		catch (RuntimeException rte) {
			return error(
				rte.getMessage() + " " + (def ? "bind" : "sett") + "ing: " + toString(lhs)
				+ eIfnull(dbg, ()-> " of: " + (dbg.op instanceof Opv ? dbg.op : cons(dbg.op, dbg.os[0])))
				+ " with: " + rhs,
				"type", symbol("match")
			);
		}
	}
	Object bind(boolean def, int bndret, Env e, Object lhs, Object rhs) {
		return switch (lhs) {
			case Ignore i-> rhs;
			case Symbol s-> { var v = def ? e.def(s, rhs) : e.set(s, rhs); yield bndret == 2 ? v : rhs; }  
			case Keyword k-> {
				if (k.equals(rhs)) yield rhs;
				throw new RuntimeException("not found keyword: " + k);
			}
			case null-> {
				if (rhs == null) yield null;
				throw new RuntimeException("too many arguments" /*+ ", none expected,"*/ + ", found: " + toString(rhs));
			}
			case Cons lc-> {
				if (lc.car instanceof Symbol s && Utility.equals(s.name, "%quote", "quote")) {
					if (equals((Object) lc.car(1), rhs)) yield null;
					throw new RuntimeException("not found literal: " + lc.car(1));
				}
				if (!(rhs instanceof Cons rc)) throw new RuntimeException("too few arguments" /*+ ", more expected,"*/ + " found: " + toString(rhs));
				var v = bind(def, bndret, e, lc.car, rc.car);
				yield lc.cdr == null && rc.cdr == null ? v : bind(def, bndret, e, lc.cdr, rc.cdr);
			}
			default-> {
				if (equals(lhs, rhs)) yield null;
				throw new RuntimeException("not found literal: " + toString(lhs));
			}
		};
	}
	
	
	// Operative & Applicative Combiners
	interface Combinable { <T> T combine(Env e, List o); }
	
	Object combine(Env e, Object op, List o) {
		if (prtrc >= 5) print(" combine: ", indent(), op, " ", o /*, "   ", e*/);
		if (op instanceof Combinable cmb) return cmb.combine(e, o);
		// per default le jFun nude sono considerate applicative
		if (isjFun(op)) return new Apv(new JFun(op)).combine(e, o);
		return aquote ? cons(op, o) : typeError("not a combiner: {datum} in: " + cons(op, o), op, symbol("Combinable"));
	}
	
	class Opv implements Combinable  {
		Object pt, ep; List x; Env e;
		Opv(Object pt, Object ep, List x, Env e) { this.pt = pt; this.ep = ep; this.e = e;
			this.x = x != null && x.cdr != null && x.car instanceof String ? x.cdr() : x;
		}
		public Object combine(Env e, List o) {
			var xe = env(this.e);
			var dbg = dbg(e, this, o);
			return tco(()-> pipe(dbg, ()-> bind(dbg, xe, pt, o), $-> bind(dbg, xe, ep, e), $$-> tco(()-> begin.combine(xe, x))));
			/* TODO in alternativa al precedente
			return pipe(dbg, ()-> bind(xe, dbg, p, o), $-> bind(xe, dbg, ep, e),
				$$-> x == null ? inert : tco(()-> x.cdr == null ? evaluate(e, x.car) : begin.begin(null, xe, x))
			);
			//*/
		}
		public String toString() { return "{%Opv " + ifnull(pt, "()", Vm.this::toString) + " " + Vm.this.toString(ep) + " " + apply(s-> s.substring(1, s.length()-1), Vm.this.toString(x)) + /*" " + e +*/ "}"; }
	}
	class Apv implements Combinable  {
		Combinable cmb;
		Apv(Combinable cmb) { this.cmb = cmb; }
		public Object combine(Env e, List o) {
			return tco(()-> pipe(dbg(e, this, o), ()-> map(car-> getTco(evaluate(e, car)), o), args-> tco(()-> cmb.combine(e, (List) args)))); 
		}
		public String toString() {
			return "{%Apv " + Vm.this.toString(cmb) + "}";
		}
		Combinable unwrap() { return cmb; }
	}
	Object wrap(Object arg) {
		return arg instanceof Apv || isjFun(arg) ? arg 
		: arg instanceof Combinable cmb ? new Apv(cmb)
		: typeError("cannot wrap: {datum}", arg, symbol("Combinable"));
	}
	Object unwrap(Object arg) {
		return arg instanceof Apv apv ? apv.cmb
		: isjFun(arg) ? new JFun(arg)
		: typeError("cannot unwrap: {datum}", arg, symbol("Apv"));
	}
	Apv lambda(Env e, Object pt, List body) { return new Apv(new Opv(pt, ignore, body, e)); }
	
	
	// Built-in Combiners
	class Vau implements Combinable  {
		public Object combine(Env e, List o) {
			var chk = checkM(this, o, 2); // o = (pt ep) | (pt ep x ...)
			if (chk instanceof Suspension s) return s;
			if (!(chk instanceof Integer len)) return typeError("not an integer: {datum}", chk, symbol("Integer"));
			var pt = o.car();
			var ep = o.car(1);
			if (ep == null) return typeError("not #ignore or a symbol: {datum} of: " + cons(this, o), ep, parseBytecode("or", "Ignore","Symbol"));
			var msg = checkPt(cons(this, o), pt, ep); if (msg != null) return msg;
			return new Opv(pt, ep, o.cdr(1), e);
		}
		public String toString() { return "%Vau"; }
	};
	class Def implements Combinable  {
		boolean def;
		Def(boolean def) {this.def = def; }
		public Object combine(Env e, List o) {
			var chk = checkR(this, o, 2, 3);  // o = (pt arg) | (pt key arg)
			if (chk instanceof Suspension s) return s;
			if (!(chk instanceof Integer len)) return typeError("not an integer: {datum}", chk, symbol("Integer"));
			var pt = o.car();
			if (!(pt instanceof Symbol)) {
				if (!(pt instanceof Cons)) return typeError("not a symbol or parameter tree: " + ifnull(pt, "()") + " of: " + cons(this, o), pt, parseBytecode("or", "Symbol", "Cons"));
				var msg = checkPt(cons(this, o), pt); if (msg != null) return msg;
			}
			var dbg = dbg(e, this, o);
			return pipe(dbg, ()-> getTco(evaluate(e, o.car(len-1))), res-> 
				switch (bndres(len != 3 ? null : o.car(1))) {
					case Suspension s-> s;
					case Integer i-> i >= 0 && i <= 2 ? bind(dbg, def, i, e, pt, res) : typeError("invalid bndret value: {datum}", i, parseBytecode("or", 0, 1, 2));
					case Object obj-> typeError("not an integer: {datum}", obj, symbol("Integer"));
				}
			);
		}
		public String toString() { return def ? "%Def" : "%Set!"; }
	};
	/* TODO non più necessario
	class DefStar implements Combinable  {
		public Object combine(Env e, List o) {
			var chk = checkM(this, o, 1); // o = (pt arg ...)
			if (chk instanceof Suspension s) return s;
			if (!(chk instanceof Integer len)) return typeError("not an integer: {datum}", chk, symbol("Integer"));
			var pt = o.car();
			if (!(pt instanceof Symbol)) {
				if (!(pt instanceof Cons)) return typeError("not a symbol or parameter tree: {datum} in: " + cons(this, o), pt, parseBytecode("or","Symbol","Cons"));
				var msg = checkPt(cons(this, o), pt); if (msg != null) return error(msg + " of: " + cons(this, o));
			}
			var dbg = dbg(e, this, o);
			return pipe(dbg, ()-> map(car-> getTco(evaluate(e, car)), o.cdr()), res-> bind(dbg, e, pt, res));
		}
		public String toString() { return "%Def*"; }
	};
	*/
	class Eval implements Combinable  {
		public Object combine(Env e, List o) {
			var chk = checkN(this, o, 2, null, Env.class); // o = (x eo)
			if (chk instanceof Suspension s) return s;
			if (!(chk instanceof Integer len)) return typeError("not an integer: {datum}", chk, symbol("Integer"));
			return evaluate(o.car(1), o.car());
		}
		public String toString() { return "%Eval"; }
	}
	
	
	// First-order Control
	class Begin implements Combinable  {
		boolean root;
		Begin() {}; Begin(boolean root) { this.root = root; } 
		public Object combine(Env e, List o) {
			// o = () | (x ...)
			return o == null ? inert : tco(()-> begin(null, e, o));
		}
		Object begin(Resumption r, Env e, List list) {
			for (var first = true;;) { // only one resume for suspension
				if (prtrc >= 3 && root && r == null) print("\n--------");
				var car = list.car;
				if (prtrc == 2 && root && r == null) print("evaluate: ", car);
				if (list.cdr == null) { return evaluate(e, car); } 
				var res = first && r != null && !(first = false) ? r.resume() : getTco(evaluate(e, car));
				if (res instanceof Suspension s) { var l = list; return s.suspend(dbg(e, this, list.car), rr-> begin(rr, e, l)); }
				list = list.cdr();
			}
		}
		public String toString() { return "%Begin" + eIf(!root, "*"); }
	}
	Begin begin = new Begin();
	class If implements Combinable  {
		public Object combine(Env e, List o) {
			var chk = checkR(this, o, 2, 3); // o = (test then) | (test then else) 
			if (chk instanceof Suspension s) return s;
			if (!(chk instanceof Integer len)) return typeError("not an integer: {datum}", chk, symbol("Integer"));
			var test = o.car();
			return tco(()-> pipe(dbg(e, this, o), ()-> getTco(evaluate(e, test)), res->
				switch (istrue(res)) {
					case Suspension s-> s;
					case Boolean b-> b
						? evaluate(e, o.car(1))
						: o.cdr(1) == null ? inert : evaluate(e, o.car(2));
						//: o.cdr(1) == null ? inert : ()-> begin.combine(e, o.cdr(1))
					case Object obj-> typeError("not a boolean: {datum}", obj, symbol("Boolean"));
				}
			));
		}
		public String toString() { return "%If"; }
	}
	Object istrue(Object res) {
		return switch (ttrue) {
			case 0-> res instanceof Boolean b ? b : typeError("not a boolean: {datum}", res, symbol("Boolean")); // Kernel Wat Lispx
			case 1-> !Utility.equals(res, false); // Scheme Racket Guile
			case 2-> !Utility.equals(res, false, null);
			case 3-> !Utility.equals(res, false, null, ignore);
			case 4-> !Utility.equals(res, false, null, ignore, 0);
			default-> res instanceof Boolean b ? b : typeError("not a boolean: {datum}", res, symbol("Boolean")); // Kernel Wat Lispx
		};
	}
	class Loop implements Combinable  {
		public Object combine(Env e, List o) {
			var chk = checkM(this, o, 1); // o = (x ...)
			if (chk instanceof Suspension s) return s;
			if (!(chk instanceof Integer len)) return typeError("not an integer: {datum}", chk, symbol("Integer"));
			return combine(null, e, o);
		}
		public Object combine(Resumption r, Env e, List o) {
			for (var first = true;;) { // only one resume for suspension
				var res = first && r != null && !(first = false) ? r.resume() : getTco(begin.combine(e, o));
				if (res instanceof Suspension s) { return s.suspend(dbg(e, this, o), rr-> combine(rr, e, o)); }
			}
		}
		public String toString() { return "%Loop"; }
	}
	class Catch implements Combinable {
		public Object combine(Env e, List o) {
			var chk = checkR(this, o, 2, 3); // o = (tag x) | (tag x hdl) -> (tag x apv1)
			if (chk instanceof Suspension s) return s;
			if (!(chk instanceof Integer len)) return typeError("not an integer: {datum}", chk, symbol("Integer"));
			var tag = o.car();
			var x = o.car(1);
			var hdl = len == 2 ? null : o.car(2);
			return combine(null, e, tag, x, hdl);
		}
		private Object combine(Resumption r, Env e, Object tag, Object x, Object hdl) {
			Object res = null;
			try {
				if (ctapv && !(x instanceof Apv apv && args(apv) == 0)) return typeError("not a zero args applicative combiner: {datum}", x, symbol("Apv"));
				res = r != null ? r.resume() : getTco(!ctapv ? evaluate(e, x) : Vm.this.combine(e, x, null));
			}
			catch (Throwable thw) {
				if (tag != ignore && thw instanceof Value val && val.tag != tag) throw thw; 
				res = pipe(dbg(e, this, hdl, thw), ()-> {
						if (hdl == null) {
							if (thw instanceof Value val) return val.value;
							throw thw instanceof Error err ? err : new Error(thw);
						}
						return (ctapv ? hdl : getTco(evaluate(e, hdl))) instanceof Apv apv && args(apv) == 1
							? getTco(Vm.this.combine(e, unwrap(apv), list(thw instanceof Value val ? val.value : thw)))
							: typeError("not a one arg applicative combiner: {datum}", hdl, symbol("Apv"))
						; 
					}
				);
			}
			return res instanceof Suspension s ? s.suspend(dbg(e, this, tag, x, hdl), rr-> combine(rr, e, tag, x, hdl)) : res;
		}
		public String toString() { return "%Catch"; }
	}
	class Throw implements Combinable {
		public Object combine(Env e, List o) {
			var chk = checkR(this, o, 1, 2); // o = (tag) | (tag value)
			if (chk instanceof Suspension s) return s;
			if (!(chk instanceof Integer len)) return typeError("not an integer: {datum}", chk, symbol("Integer"));
			var tag = o.car();
			var value = len == 1 ? inert : o.car(1);
			throw new Value(tag, ctapv ? value : pipe(dbg(e, this, tag, value), ()-> getTco(evaluate(e, value))));
		}
		public String toString() { return "%Throw"; }
	}
	class Finally implements Combinable {
		public Object combine(Env e, List o) { return combine(null, e, o); }
		public Object combine(Resumption r, Env e, List o) {
			var chk = checkM(this, o, 1); // o = (x cln)
			if (chk instanceof Suspension s) return s;
			if (!(chk instanceof Integer len)) return typeError("not an integer: {datum}", chk, symbol("Integer"));
			var x = o.car();
			var cln = o.cdr();
			try {
				var res = r != null ? r.resume() : pipe(dbg(e, this, x, cln), ()-> getTco(evaluate(e, x)), v-> cleanup(cln, e, true, v));
				return res instanceof Suspension s ? s.suspend(dbg(e, this, o), rr-> combine(rr, e, o)) : res;
			}
			catch (Throwable thw) {
				return cleanup(cln, e, false, thw);
			}
		}
		Object cleanup(List cln, Env e, boolean success, Object res) {
			return pipe(dbg(e, this, cln, success, res), ()-> getTco(begin.combine(e, cln)), $-> {
					if (success) return res;
					throw res instanceof Value val ? val : res instanceof Error err ? err : new Error("cleanup error:", (Throwable) res);
				}
			);
		}
		public String toString() { return "%Finally"; }
	}
	
	// Delimited Control
	class TakeSubcont implements Combinable  {
		public Object combine(Env e, List o) {
			var chk = checkM(this, o, 2, null, or(Ignore.class, Symbol.class)); // o = (prp (or ignore symbol) . body)
			if (chk instanceof Suspension s) return s;
			if (!(chk instanceof Integer len)) return typeError("not an integer: {datum}", chk, symbol("Integer"));
			return pipe(dbg(e, this, o), ()-> getTco(evaluate(e, o.car())),
				prp-> new Suspension(prp, lambda(e, list(o.<Object>car(1)), o.cdr(1)))
					.suspend(dbg(e, this, o), rr-> Vm.this.combine(e, rr.s, null)))
			;
		}
		public String toString() { return "%TakeSubcont"; }
	}
	class PushPrompt implements Combinable  {
		public Object combine(Env e, List o) {
			var chk = checkN(this, o, 2); // o = (prp apv0) | (prp x) | (prp (begin ...))
			if (chk instanceof Suspension s) return s;
			if (!(chk instanceof Integer len)) return typeError("not an integer: {datum}", chk, symbol("Integer"));
			var prp = o.car();
			var x = o.car(1);
			return pushPrompt(null, e, dbg(e, this, o), prp, ()-> x instanceof Apv apv && args(apv) == 0 ? Vm.this.combine(e, apv, null) : evaluate(e, x));
		}
		public String toString() { return "%PushPrompt"; }
	}
	class PushPromptSubcont implements Combinable  {
		public Object combine(Env e, List o) {
			var chk = checkN(this, o, 3); // o = (prp k apv0)
			if (chk instanceof Suspension s) return s;
			if (!(chk instanceof Integer len)) return typeError("not an integer: {datum}", chk, symbol("Integer"));
			var prp = o.car();
			var o1 = o.car(1); if (!(o1 instanceof Continuation k)) return typeError("not a continuation: {datum}", o1, symbol("Continuation")); 
			var o2 = o.car(2); if (!(o2 instanceof Apv apv0 && args(apv0) == 0)) return typeError("not a zero args applicative combiner: {datum}", o2, symbol("Apv"));
			return pushPrompt(null, e, dbg(e, this, o), prp, ()-> k.apply(e, apv0));
		}
		public String toString() { return "%PushPromptSubcont"; }
	}
	Object pushPrompt(Resumption r, Env e, Dbg dbg, Object prp, Supplier action) {
		var res = r != null ? r.resume() : getTco(action.get());
		if (!(res instanceof Suspension s)) return res;
		return prp == ignore || !equals(s.prp, prp)
			? s.suspend(dbg, rr-> pushPrompt(rr, e, dbg, prp, action))
			: combine(e, s.hdl, cons(s.k, null))
		;
	}
	Object pushSubcontBarrier(Resumption r, Env e, Object x) {
		var res = r != null ? r.resume() : getTco(evaluate(e, x));
		if (!(res instanceof Suspension s)) return res;
		return s.suspend(dbg(e, "pushSubcontBarrier", x), rr-> pushSubcontBarrier(rr, e, x)).k.apply(()-> error("prompt not found: {prompt}", "type", symbol("prompt"), "prompt", s.prp));
	}
	
	
	// Dynamic Variables
	class DVar extends Box { DVar(Object val) { super(val); }}
	class DDef implements Combinable {
		public Object combine(Env e, List o) {
			var chk = checkN(this, o, 2, Symbol.class); // o = (var val)
			if (chk instanceof Suspension s) return s;
			if (!(chk instanceof Integer len)) return typeError("not an integer: {datum}", chk, symbol("Integer"));
			var var = o.car();
			var lookup = e.get(var);
			if (lookup.isBound && !(lookup.value instanceof DVar)) return error("not a dinamic variable: " + var);
			DVar dVar = (DVar) lookup.value;
			return pipe(dbg(e, this, o), ()-> getTco(evaluate(e, o.car(1))), val-> {
					if (dVar != null) dVar.value = val; else bind(null, e, var, new DVar(val));
					return inert;
				}
			);
		}
		public String toString() { return "%DDef"; }
	}
	class DDefStar implements Combinable {
		public Object combine(Env e, List o) {
			var chk = checkM(this, o, 2); // o = ((var ...) vals ...)
			if (chk instanceof Suspension s) return s;
			if (!(chk instanceof Integer len)) return typeError("not an integer: {datum}", chk, symbol("Integer"));
			var vars = array(o.car(), Symbol.class);
			var dVars = new DVar[vars.length];
			for (int i=0; i<vars.length; i+=1) {
				var var = vars[i];
				var lookup = e.get(var);
				if (!lookup.isBound) continue;
				if (!(lookup.value instanceof DVar dVar)) return typeError("not a dinamic variable: {datum}", var, symbol("DVar"));
				dVars[i] = dVar;
			}
			return pipe(dbg(e, this, o), ()-> map(car-> getTco(evaluate(e, car)), o.cdr()), args-> {
					var vals = array((List) args);
					if (vars.length != vals.length) return error("not same length: " + vars + " and " + vals);
					for (int i=0; i<dVars.length; i+=1) {
						var dVar = dVars[i]; if (dVar != null) dVar.value = vals[i]; else bind(dbg(e, this, o), e, vars[i], new DVar(vals[i]));
					}
					return inert;
				}
			);
		}
		public String toString() { return "%DDef*"; }
	}
	class DLet implements Combinable {
		public Object combine(Env e, List o) { return combine(null, e, o); }
		public Object combine(Resumption r, Env e, List o) {
			var chk = checkM(this, o, 2); // o = (((var ...) val ...) x ...)
			if (chk instanceof Suspension s) return s;
			if (!(chk instanceof Integer len)) return typeError("not an integer: {datum}", chk, symbol("Integer"));
			List car = o.car();
			var vars = array(car.car());
			var vals = array(car.cdr());
			if (vars.length != vals.length) return error("not same length: " + vars + " and " + vals);
			var olds = new Object[vals.length];
			for (int i=0; i<vars.length; i+=1) {
				if (!(vars[i] instanceof DVar dvar)) return typeError("not a dinamic variable: {datum}", vars[i], symbol("DVar"));
				olds[i] = dvar.value;
				dvar.value = vals[i];
			}
			try {
				Object res = r != null ? r.resume() : getTco(len == 2 && o.car(1) instanceof Apv apv && args(apv) == 0 ? Vm.this.combine(e, apv, null) : begin.combine(e, o.cdr()));
				return res instanceof Suspension s ? s.suspend(dbg(e, this, o), rr-> combine(rr, e, o)) : res;
			}
			finally {
				for (int i=0; i<vars.length; i+=1) ((DVar) vars[i]).value = olds[i];
			}
		}
		public String toString() { return "%DLet"; }
	}
	class DLambda implements Combinable {
		public Object combine(Env e, List o) { return combine(null, e, o); }
		public Object combine(Resumption r, Env e, List o) {
			var chk = checkM(this, o, 1, list(Symbol.class)); // o = ((var ...) x ...)
			if (chk instanceof Suspension s) return s;
			if (!(chk instanceof Integer)) return typeError("not an integer: {datum}", chk, symbol("Integer"));
			var vars = array(o.car());
			var len = vars.length;
			var body = o.cdr();
			return new Apv( new Combinable() {
				public Object combine(Env de, List o) { return combine(null, de, o); }
				public Object combine(Resumption r, Env de, List o) {
					var vals = o == null ? new Object[len] : array(o);
					if (vals.length != len) return error("not same length: " + list(vars) + " and " + o);
					var olds = new Object[len];
					var ndvs = new Object[len];
					for (int i=0; i<len; i+=1) {
						var lookup = de.get(vars[i]);
						if (body == null && !lookup.isBound) continue;
						if ((ndvs[i] = lookup.value) instanceof DVar dvar) { olds[i] = dvar.value; continue; }
						return typeError("not a dinamic variable: {datum}", vars[i], symbol("DVar"));
					}
					for (int i=0; i<len; i+=1) {
						if (ndvs[i] instanceof DVar dvar) dvar.value = vals[i]; else de.def(vars[i], new DVar(vals[i]));
					}
					if (body == null) return switch (bndres) { case 0-> inert; case 1-> vals[len-1]; case 2-> olds[len-1]; default-> 1; };  
					try {
						Object res = r != null ? r.resume() : getTco(begin.combine(e, body));
						return res instanceof Suspension s ? s.suspend(dbg(e, this, o), rr-> combine(rr, e, o)) : res;
					}
					finally {
						for (int i=0; i<len; i+=1) if (ndvs[i] instanceof DVar dvar) dvar.value = olds[i];
					}
				}
				@Override public String toString() { return "{%DOpv " + o.car() + eIfnull(body, ()-> " " + apply(s-> s.substring(1, s.length()-1), Vm.this.toString(body))) + "}"; }
			});
		}
		public String toString() { return "%D\\"; }
	}
	
	
	// Java Native Interface
	interface ArgsList extends Function<List,Object> {}
	interface ChkList extends BiFunction<String,List,Object> {}
	interface LenList extends BiFunction<Integer,List,Object> {}
	class JFun implements Combinable {
		String name; ArgsList jfun; ChkList check;
		JFun(String name, ChkList check, LenList jfun) { this(name, jfun); this.check = check; };
		JFun(String name, Object jfun) { this(jfun); this.name = name; };
		JFun(Object jfun) {
			this.jfun = switch (jfun) {
				case ArgsList al-> al;  
				case LenList ll-> o-> switch(check.apply(name, o)) { case Suspension s-> s; case Integer len-> ll.apply(len, o); case Object chk-> typeError("not an integer: {datum}", chk, symbol("Integer")); };  
				case Supplier sp-> o-> switch (checkN(name, o, 0)) { case Suspension s-> s; case Integer len-> sp.get(); case Object chk-> typeError("not an integer: {datum}", chk, symbol("Integer")); };  
				case Function f-> o-> switch (checkN(name, o, 1)) {case Suspension s-> s; case Integer len-> f.apply(o.car()); case Object chk-> typeError("not an integer: {datum}", chk, symbol("Integer")); };  
				case BiFunction f-> o-> switch (checkN(name, o, 2)) {case Suspension s-> s; case Integer len-> f.apply(o.car(), o.car(1)); case Object chk-> typeError("not an integer: {datum}", chk, symbol("Integer")); };
				case Field f-> o-> uncked(()-> switch (checkR(name, o, 1, 2)) {case Suspension s-> s; case Integer len-> { if (len == 1) yield f.get(o.car()); f.set(o.car(), o.car(1)); yield inert; } case Object chk-> typeError("not an integer: {datum}", chk, symbol("Integer")); });
				case Method mt-> o-> {
					var pc = mt.getParameterCount();
					return switch (!mt.isVarArgs() ? checkN(name, o, pc+1) : checkM(name, o, pc)) {
						case Suspension s-> s;
						case Integer len-> uncked(()-> mt.invoke(o.car(), reorg(mt, array(o.cdr()))));
						case Object chk-> typeError("not an integer: {datum}", chk, symbol("Integer"));
					};
				};
				case Constructor c-> o-> switch (checkN(name, o, c.getParameterCount())) {
					case Suspension s-> s;
					case Integer len-> uncked(()-> c.newInstance(reorg(c, array(o))));
					case Object chk-> typeError("not an integer: {datum}", chk, symbol("Integer"));
				};
				default -> typeError("not a combine: {datum}", ifnull(name, jfun), parseBytecode("or", "ArgList", "LenList", "Supplier", "Function", "Bifunction", "Field", "Method", "Constructor"));
			};
		}
		public Object combine(Env e, List o) {
			return pipe(dbg(e, this, o), ()-> {
					try {
						return jfun.apply(o);
					}
					catch (Throwable thw) {
						switch (thw) {
							case Value val: throw val;
							case Error err: return error(err);
							default: return error("error combining: " + this + " with: " + o, thw);
						}
					}
				}
			);
		}
		public String toString() {
			if (name != null) return name;
			var intefaces = stream(jfun.getClass().getInterfaces()).map(i-> Vm.this.toString(i)).collect(joining(" "));
			return "{JFun" + eIf(intefaces.isEmpty(), ()-> " " + intefaces) + " " + jfun + "}";
		}
	}
	boolean isjFun(Object obj) {
		return isInstance(obj, Supplier.class, Function.class, BiFunction.class, Executable.class, Field.class);
	}
	Object jInvoke(String name) {
		if (name == null) return typeError("method name is null", name, symbol("String"));
		return new ArgsList() {
			@Override public Object apply(List o) {
				var chk = checkM("jGetSet", o, 1, Object.class);
				if (chk instanceof Suspension s) return s;
				if (!(chk instanceof Integer len)) return typeError("not an integer: {datum}", chk, symbol("Integer"));
				Object o0 = o.car();
				Object[] args = array(o, 1);
				// (@new class . objects)            -> class.getConstructor(getClasses(objects)).newInstance(objects) -> constructor.newInstance(objects)
				// (@<name> object . objects)        -> object.getClass().getMethod(name, getClasses(objects)).invocke(object, objects) -> method.invoke(object, objects)
				// (@getConstructor class . classes) -> class.getConstructor(classes) -> constructor
				// (@getMethod class name . classes) -> class.getMethod(name, classes) -> method
				// (@getField class name)            -> class.getField(name, classes) -> field
				if (name.equals("new") && o0 instanceof Class c && of(Obj.class, Box.class).anyMatch(rc-> rc.isAssignableFrom(c)) && (args.length == 0 || args[0].getClass() != Vm.class)) {
					// (@new Error "assert error!") -> (@new Error vm "assert error!")
					args = headAdd(args, Vm.this);
				}
				var classes = getClasses(args);
				Executable executable = getExecutable(o0, name, classes);
				if (executable == null) return error("not found {executable} of: {object}", "type", symbol("unboundExecutable"), "executable", Vm.this.toString(name, classes), "object", Vm.this.toString(o0));
				try {
					//executable.setAccessible(true);
					args = reorg(executable, args);
					return switch (executable) { 
						case Method m-> m.invoke(o0, args);
						case Constructor c-> c.newInstance(args);
					};
				}
				catch (Throwable thw) {
					if (thw instanceof InvocationTargetException ite) {
						switch (ite.getTargetException()) {
							case Value v: throw v;
							case Error e: return error(e);
							default: // in errore senza!
						}
					}
					return error("error executing " + Vm.this.toString(name, args) + " of: " + Vm.this.toString(o0) + " with: " + Vm.this.toString(list(args)), thw);
				}
			}
			@Override public String toString() { return "@" + name; }
		};
	}
	Object jGetSet(String name) {
		if (name == null) return typeError("field name is null", name, symbol("String"));
		return new ArgsList() {
			@Override public Object apply(List o) {
				var chk = checkR("jGetSet", o, 1, 2);
				if (chk instanceof Suspension s) return s;
				if (!(chk instanceof Integer len)) return typeError("not an integer: {datum}", chk, symbol("Integer"));
				var o0 = o.car();
				// (.<name> object)       -> object.getclass().getField(name).get(object) -> field.get(object) 
				// (.<name> object value) -> object.getClass().getField(name).set(object,value) -> field.set(object, value) 
				Field field = getField(o0 instanceof Class ? (Class) o0 : o0.getClass(), name);
				if (field == null) return error("not found slot: {slotName} of: {object}", "type", symbol("unboundSlot"), "slotName", name, "object", Vm.this.toString(o0));
				try {
					if (len == 1) return field.get(o0);
					field.set(o0, o.car(1)); return inert;
				}
				catch (Throwable thw) {
					return error("can't " + (len==1 ? "get" : "set") + " " + name + " of " + Vm.this.toString(o0) + eIf(len == 1, ()-> " with " + Vm.this.toString(o.car(1))));
				}
			}
			@Override public String toString() { return "." + name; }
		};
	}
	
	
	// Error handling
	Object rootPrompt = new Object() { public String toString() { return "%RootPrompt"; }};
	Object pushRootPrompt(Object x) { return list(new PushPrompt(), rootPrompt, x); }
	<T> T error(String msg, Object ... objs) { return error(new Error(msg, objs)); }
	<T> T error(Throwable cause, Object ... objs) { return error(new Error(cause, objs)); }
	<T> T error(String msg, Throwable cause, Object ... objs) { return error(new Error(msg, cause, objs)); }
	<T> T error(Error err) {
		var userBreak = theEnv.get(symbol("userBreak")).value;
		if (userBreak == null) throw err;
		return (T) pipe(dbg(theEnv, "userBreak", err), ()-> getTco(evaluate(theEnv, list(userBreak, err)))); 
	}
	<T> T typeError(String msg, Object datum, Object expectedType) { return error(new Error(msg, "type", symbol("type"), "datum", datum, "expectedType", expectedType)); }
	<T> T unboundSymbolError(String msg, String name, Env env) { return error(new Error(msg, "type", symbol("unboundSymbol"), "symbol", name, "env", env)); }
	<T> T unboundSlotError(String msg, String name, Object object) { return error(new Error(msg, "type", symbol("unboundSlot"), "name", name, "object", object)); }
	<T> T unboundMethodError(String msg, String name, Object object) { return error(new Error(msg, "type", symbol("unboundMethod"), "name", name, "object", object)); }
	public class Error extends Condition {
		private static final long serialVersionUID = 1L;
		public Error(Object ... objs) { super(objs); }
		public Error(Throwable cause, Object ... objs) { super(cause, objs); }
		public Error(String message, Object ... objs) { super(message, objs); }
		public Error(String message, Throwable cause, Object ... objs) { super(message, cause, objs); }
	}
	class Value extends RuntimeException {
		private static final long serialVersionUID = 1L;
		Object tag, value;
		Value(Object tag, Object value) {
			super(Vm.this.toString(tag) + " " + Vm.this.toString(value)); this.tag = tag; this.value = value;
		}
	}
	class PTree {
		private Object exp, pt, ep;
		private Set syms = new HashSet();
		PTree(Object exp, Object pt, Object ep) { this.exp=exp; this.pt = pt; this.ep = ep; }
		Object check() { 
			if (!((pt == null || pt == ignore || pt instanceof Symbol) && syms.add(pt))) {
				if (!(pt instanceof Cons)) return typeError("not a #null, #ignore, symbol or parameters tree: {datum} of: " + exp, pt, parseBytecode("or", "Null", "Ignore", "Symbol", "Cons") );
				var msg = check(pt); if (msg != null) return msg;
			}
			if (ep == null /* %def! && %set! */ || ep == ignore) return syms.size() > 0 ? null : typeError("no one #null #ignore or symbol in: {datum} of: " + exp, pt, parseBytecode("or", "Null", "Ignore", "Symbol"));
			if (!(ep instanceof Symbol sym)) return typeError("not a #ignore or symbol: {datum} of: " + exp, ep, parseBytecode("or", "Ignore", "Symbol"));
			return !syms.contains(sym) ? null : error("not a unique symbol: {datum} in: {expr}", "datum", ep, "expr", exp);
		}
		private Object check(Object p) {
			if (p == null || p == ignore) syms.add(p);
			if (p instanceof Symbol) { return syms.add(p) ? null : error("not a unique symbol: {datum} in: " + pt + " of: {expr}", "datum", p, "expr", exp); }
			if (!(p instanceof Cons c)) return null;
			var msg = check(c.car); if (msg != null) return msg;
			return c.cdr == null ? null : check(c.cdr);
		}
	}
	Object checkPt(Object exp, Object pt) { return checkPt(exp, pt, null); }
	Object checkPt(Object exp, Object pt, Object ep) { return new PTree(exp, pt, ep).check(); }
	int args(Apv apv) {
		return switch(apv.cmb) {
			case Opv opv-> opv.pt == null ? 0 : opv.pt instanceof Cons c && c.cdr == null && (c.car == ignore || c.car instanceof Symbol) ? 1 : Integer.MAX_VALUE;
			case JFun jFun-> jFun.jfun instanceof Supplier ? 0 : jFun.jfun instanceof Function ? 1 : Integer.MAX_VALUE;
			default-> Integer.MAX_VALUE;
		};
	}
	Object checkN(Object op, List o, int expt, Object ... cls) {
		return checkR(op, o, expt, expt, cls);
	}
	Object checkM(Object op, List o, int min, Object ... cls) {
		return checkR(op, o, min, Integer.MAX_VALUE, cls);
	}
	Object checkR(Object op, List o, int min, int max, Object ... cls) {
		var chk = cls.length == 0 ? len(o) : checkT(op, o, min, cls);
		if (chk instanceof Suspension s) return s;
		if (!(chk instanceof Integer len)) return typeError("not an integer: {datum}", chk, symbol("Integer"));
		if (len >= min && len <= max) return len; 
		return error((len < min ? "less then " + min : "more then " + max) + " operands to combine: " + op + " with: " + o);
	}
	Object checkT(Object op, List o, int min, Object ... chks) {
		if (o == null) return 0;
		int len=chks.length, i=0;
		for (var oo=o; oo != null; i+=1, oo=oo.cdr()) {
			if (len == 0) continue;
			var chk = checkT2(op, o, oo.car, i-min, i < len && i < min ? chks[i] : len <= min ?  null : chks[len-1]);
			if (chk instanceof Suspension s) return s;
			if (chk != null) return typeError("not an null: {datum}", chk, symbol("Null"));
		}
		return i;
	}
	Object checkT2(Object op, Object o, Object on, int i, Object chk) {
		switch (chk) {
			case null: return null;
			case Class cl when on instanceof Class cl2 && cl.isAssignableFrom(cl2) || cl.isInstance(on): return null;
			case List l:
				if (!(on instanceof List onl)) break;
				for (; onl != null; onl = onl.cdr()) {
					var chkl = checkT2(op, o, onl.car, i, l.car);
					if (chkl instanceof Suspension s) return s;
					if (chkl != null) return typeError("not an null: {datum}", chkl, symbol("Null"));
				}
				return null;
			case Object[] chks:
				if (i > 0) return checkT2(op, o, on, i, chks[i % chks.length]);
				for (Object chk2: chks) {
					if (Utility.equals(on, chk2) || chk2 instanceof Class cl && (cl.isInstance(on) || on instanceof Class cl2 && cl.isAssignableFrom(cl2))) return null;
				}
			default: break;
		}
		return typeError(
			"not " + (chk instanceof Object[] objs ? "one of " : "a ") + "{expectedType}: {datum} to combine: " + toString(op) + " with: " + toString(o),
			on, chk == null ? symbol("Null")
			: chk instanceof Class cl ? symbol(cl.getSimpleName())
			: cons(symbol("or"), list(stream((Object[]) chk).map(obj-> symbol(obj == null ? "Null" : obj instanceof Class cl ? cl.getSimpleName() : Vm.this.toString(obj))).toArray() ))
		);
	}
	
	
	// Utilities
	List list(Object ... args) {
		return list(true, args);
	}
	<T> T listStar(Object ... args) {
		return list(false, args);
	}
	<T> T list(boolean b, Object ... args) {
		var len = args.length-1;
		var c = b || len < 0 ? null : args[len];
		for (var i=len-(b?0:1); i>=0; i-=1) c = cons(args[i], c);
		return (T) c;
	}
	Object[] array(List c) {
		return array(c, 0);
	}
	Object[] array(List c, int i) {
		return array(c, i, Object.class);
	}
	<T> T[] array(List c, Class<T> cl) {
		return (T[]) array(c, 0, cl);
	}
	<T> T[] array(List o, int i, Class<T> cl) {
		var res = new ArrayList<T>();
		for (; o != null; o = o.cdr()) if (i-- <= 0) res.add(o.car());
		return res.toArray((T[]) Array.newInstance(cl, 0));
	}
	List reverse(List list) {
		List res = null;
		for (; list != null; list = list.cdr()) res = cons(list.car, res);
		return res;
	}
	Object append(List h, Object o) {
		if (h == null) return o;
		return cons(h.car(), append(h.cdr(), o));
	}
	Object listStar(List h) {
		return h == null ? null : listStar1(h);
	}
	Object listStar1(List h) {
		var car = h.car;
		var cdr = h.cdr();
		return cdr == null ? car : cons(car, listStar(cdr));
	}
	<T> T print(Object ... os) {
		for (var o: os) out.print(toString(o)); out.println();
		return (T)(os.length == 0 ? inert : os[os.length - 1]);
	}
	<T> T write(Object ... os) {
		for (var o: os) out.print(toString(true, o)); out.println();
		return (T)(os.length == 0 ? inert : os[os.length - 1]);
	}
	<T> T log(Object ... os) {
		int i=0; for (var o: os) out.print(eIf(i++ == 0, " ") + toString(o)); out.println();
		return (T)(os.length == 0 ? inert : os[0]);
	}
	boolean equals(Object a, Object b) {
		if (a instanceof Object[] aa) return equals(aa, b);
		if (a instanceof Cons ca) return ca.equals(b);
		if (a instanceof Object) return a.equals(b);
		return a == b; // only for null
	}
	boolean equals(Object[] a, Object o) {
		if (!(o instanceof Object[] b) || a.length != b.length) return false;
		for (int i=0; i<a.length; i+=1) if (!equals(a[i], b[i])) return false;
		return true;
	}
	Object vmAssert(String str, Object objs) throws Exception {
		List exp = cons(begin, parseBytecode(parse(str)));
		return vmAssert.combine(theEnv,  objs instanceof Throwable ? exp : cons(exp, parseBytecode(objs))); 
	}
	class Assert implements Combinable {
		public Object combine(Env env, List o) {
			if (!doasrt) return true;
			var chk = checkR(this, o, 1, 2); // o = (x) | (x v)
			if (!(chk instanceof Integer len)) return chk;
			return test.combine(env, cons(null, o));
		}
		public String toString() { return "Assert"; }
	}
	Assert vmAssert = new Assert();
	class Test implements Combinable {
		public Object combine(Env env, List o) {
			if (!doasrt) return true;
			var chk = checkR(this, o, 2, 3); // o = (name x) | (name x v)
			if (!(chk instanceof Integer len)) return chk;
			var name = eIfnull(o.car(), n-> "test "+ n + ": ");
			var exp = o.car(1);
			try {
				env = env(env);
				var val = pushSubcontBarrier(null, env, pushRootPrompt(exp));
				if (len == 2) print(name, exp, " should throw but is ", val);
				else {
					var expt = o.car(2);
					if (Vm.this.equals(val, pushSubcontBarrier(null, env, pushRootPrompt(expt)))) return true;
					print(name, exp, " should be ", expt, " but is ", val);
				}
			}
			catch (Throwable thw) {
				if (len == 2) return true;
				if (prstk) thw.printStackTrace(out);
				else print(name, exp, " throw ", "{" + thw.getClass().getSimpleName() + " " + thw.getMessage() + "}");
			}
			return false;
		}
		public String toString() { return "Test"; }
	}
	Test test = new Test();
	
	
	// Bytecode parser
	Map<String,Intern> interns = new LinkedHashMap<>();
	<T extends Intern> T intern(String name) {
		return (T) interns.computeIfAbsent(name, n-> n.startsWith(":") && n.length() > 1 ? new Keyword(n.substring(1)) : new Symbol(n));
	}
	
	Object parseBytecode(Object o) {
		if (o instanceof String s) return switch(s) { case "#inert"-> inert; case "#ignore", "#_" -> ignore; default-> intern(instr ? s.intern() : s); };
		if (o instanceof Object[] a) return parseBytecode(a);
		return o;
	}
	Object parseBytecode(Object ... objs) {
		if (objs.length == 0) return null;
		if (objs.length == 2 && objs[0] != null && objs[0].equals("wat-string")) return instr ? ((String) objs[1]).intern() : objs[1];
		int i = objs.length - 1;
		Object tail = null; 
		if (i > 1 && objs[i-1] != null && objs[i-1].equals(".")) { tail = parseBytecode(objs[i]); i-=2; }
		for (; i>=0; i-=1) {
			var obj = objs[i];
			if (obj != null && obj.equals("."))	throw new Error(". not is the penultimate element in " + objs);
			tail = cons(parseBytecode(obj), tail);
		}
		return tail;
	}
	
	
	// Stringification
	String toString(Object o) { return toString(false, o); }
	String toString(boolean t, Object o) {
		return switch (o) {
			case null-> "#null"; // () in cons  altrove #null
			case Boolean b-> b ? "#t" : "#f";
			case Class cl-> "&" + Utility.toSource(cl);
			case String s-> !t ? s : '"' + Utility.toSource(s) + '"';
			case Object[] a-> {
				var s = new StringBuilder();
				for (var e: a) s.append(eIf(s.isEmpty(), ", ") + toString(true, e));
				yield "[" + s.toString() + "]";
			}
			default-> o.toString();
		};
	}
	public static String reverseMap(Map map) {
		var sb = new StringBuilder(); map.entrySet().forEach(e-> sb.insert(0, " " + e)); return sb.toString();
	}
	private String toString(String name, Object[] args) {
		return (name.equals("new") ? "constructor: " : "method: " + name) + ifnull(args, "()", a-> toString(list(a)));
	}
	
	
	// Bootstrap
	Env vmEnv=env(null), theEnv=env(vmEnv); {
		bind(null, vmEnv, symbol("%def"), new Def(true));
		bind(null, vmEnv, symbol("%begin"), begin);
		getTco(evaluate(vmEnv, parseBytecode(
				$("%begin",
					// Basics
					$("%def", "%vau", new Vau()),
					$("%def", "%set!", new Def(false)),
					//$("%def", "%def*", new DefStar()), // TODO non più necessario
					$("%def", "%eval", wrap(new Eval())),
					$("%def", "%makeEnv", wrap(new JFun("%MakeEnv", (n,o)-> checkR(n, o, 0, 1, or(null, Env.class)), (l,o)-> env(l == 0 ? null : o.car()) ))) ,
					$("%def", "%wrap", wrap(new JFun("%Wrap", (Function) this::wrap))),
					$("%def", "%unwrap", wrap(new JFun("%Unwrap", (Function) this::unwrap))),
					$("%def", "%value", wrap(new JFun("%Value", (n,o)-> checkN(n, o, 2, Symbol.class, Env.class), (l,o)-> o.<Env>car(1).get(o.car()).value) )),
					$("%def", "%bound?", wrap(new JFun("%Bound?", (n,o)-> checkN(n, o, 2, Symbol.class, Env.class), (l,o)-> o.<Env>car(1).get(o.car()).isBound) )),
					$("%def", "%bind?", wrap(new JFun("%Bind?", (n,o)-> checkN(n, o, 3, Env.class), (l,o)-> { try { bind(true, 0, o.<Env>car(), o.car(1), o.car(2)); return true; } catch (RuntimeException rte) { return false; }} ))),
					$("%def", "%apply", wrap(new JFun("%Apply", (ArgsList) o-> combine(o.cdr(1) == null ? env(null) : o.car(2), unwrap(o.car()), o.car(1)) ))),
					$("%def", "%apply*", wrap(new JFun("%Apply*", (ArgsList) o-> combine(env(null), unwrap(o.car()), o.cdr()) ))),
					$("%def", "%resetEnv", wrap(new JFun("%ResetEnv", (Supplier) ()-> { theEnv.map.clear(); return theEnv; } ))),
					$("%def", "%pushEnv", wrap(new JFun("%PushEnv", (Supplier) ()-> theEnv = env(theEnv)))),
					$("%def", "%popEnv", wrap(new JFun("%PopEnv", (Supplier) ()-> theEnv = theEnv.parent))),
					// Values
					$("%def", "%car", wrap(new JFun("%Car", (n,o)-> checkN(n, o, 1, Cons.class), (l,o)-> o.<Cons>car().car() ))),
					$("%def", "%cdr", wrap(new JFun("%Car", (n,o)-> checkN(n, o, 1, Cons.class), (l,o)-> o.<Cons>car().cdr() ))),
					$("%def", "%cadr", wrap(new JFun("%Cadr", (n,o)-> checkN(n, o, 1, Cons.class), (l,o)-> o.<Cons>car().car(1) ))),
					$("%def", "%cons", wrap(new JFun("%Cons", (BiFunction) (car,cdr)-> cons(car,cdr) ))),
					$("%def", "%cons?", wrap(new JFun("%Cons?", (Function<Object, Boolean>) obj-> obj instanceof Cons))),
					$("%def", "%list?", wrap(new JFun("%List?", (Function<Object, Boolean>) obj-> obj instanceof List))),
					$("%def", "%null?", wrap(new JFun("%Null?", (Function<Object, Boolean>) obj-> obj == null))),
					$("%def", "%symbol", wrap(new JFun("%Symbol", (n,o)-> checkN(n, o, 1, String.class), (l,o)-> symbol(o.car()) ))),
					$("%def", "%keyword", wrap(new JFun("%Keyword", (n,o)-> checkN(n, o, 1, String.class), (l,o)-> keyword(o.car()) ))),
					$("%def", "%symbol?", wrap(new JFun("%Symbol?", (Function<Object, Boolean>) obj-> obj instanceof Symbol ))),
					$("%def", "%keyword?", wrap(new JFun("%Keyword?", (Function<Object, Boolean>) obj-> obj instanceof Keyword ))),
					$("%def", "%intern", wrap(new JFun("%Intern", (n,o)-> checkN(n, o, 1, String.class), (l,o)-> intern(o.car()) ))),
					$("%def", "%internName", wrap(new JFun("%InternName", (n,o)-> checkN(n, o, 1, Intern.class), (l,o)-> o.<Intern>car().name ))),
					// First-order Control
					$("%def", "%if", new If()),
					$("%def", "%loop", new Loop()),
					$("%def", "%catch", apply(c-> !ctapv ? c : wrap(c), new Catch())),
					$("%def", "%throw", apply(t-> !ctapv ? t : wrap(t), new Throw())),
					$("%def", "%finally", new Finally()),
					// Delimited Control
					$("%def", "%takeSubcont", new TakeSubcont()),
					$("%def", "%pushPrompt", new PushPrompt()),
					$("%def", "%pushPromptSubcont", wrap(new PushPromptSubcont())),
					$("%def", "%pushSubcontBarrier", wrap(new JFun("%PushSubcontBarrier", (n,o)-> checkM(n, o, 2, Env.class), (l,o)-> pushSubcontBarrier(null, o.car(), cons(begin, o.cdr())) ))),
					// Dynamically-scoped Variables
					$("%def", "%box", wrap(new JFun("%Box", (Function<Object,Box>) Box::new))),
					$("%def", "%dVar", wrap(new JFun("%DVar", (Function<Object,DVar>) DVar::new))),
					$("%def", "%dVal", wrap(new JFun("%DVal", (n,o)-> checkR(n, o, 1, 2, DVar.class), (l,o)-> apply(dv-> l == 1 ? dv.value : (dv.value=o.car(1)), o.<DVar>car()) ))),
					$("%def", "%dDef", new DDef()),
					$("%def", "%dDef*", new DDefStar()),
					$("%def", "%dLet", new DLet()),
					$("%def", "%d\\", new DLambda()),
					// Errors
					$("%def", "%rootPrompt", rootPrompt),
					$("%def", "%error", wrap(new JFun("%Error", (ArgsList) o-> ((ArgsList) jInvoke("error")).apply(listStar(this, o))))),
					// Java Interface
					$("%def", "%jFun?", wrap(new JFun("%JFun?", (Function<Object,Boolean>) this::isjFun))),
					$("%def", "%jInvoke", wrap(new JFun("%JInvoke", (Function<String,Object>) this::jInvoke))),
					$("%def", "%jGetSet", wrap(new JFun("%JGetSet", (Function<String,Object>) this::jGetSet))),
					$("%def", "%instanceof?", wrap(new JFun("%Instanceof?", (n,o)-> checkN(n, o, 2, null, Class.class), (l,o)-> o.<Class>car(1).isInstance(o.car()) ))),
					// Object System
					$("%def", "%addMethod", wrap(new JFun("%AddMethod", (n,o)-> checkN(n, o, 3, or(null, Class.class), Symbol.class, Apv.class), (l,o)-> addMethod(o.car(), o.car(1), o.car(2)) ))),
					$("%def", "%getMethod", wrap(new JFun("%GetMethod", (n,o)-> checkN(n, o, 2, or(null, Class.class), Symbol.class), (l,o)-> getMethod(o.car(), o.car(1)) ))),
					$("%def", "%obj", wrap(new JFun("%Obj", (n,o)-> checkM(n, o, 1, or(Box.class, Obj.class), or(Symbol.class, Keyword.class), null), (l,o)-> ((ArgsList) jInvoke("new")).apply(listStar(o.car(), Vm.this, o.cdr)) ))),
					$("%def", "%class", wrap(new JFun("%Class", (n,o)-> checkR(n, o, 1, 2, Symbol.class, or(Box.class, Obj.class)), (l,o)-> extend(o.car(), apply(cdr-> cdr == null ? null : cdr.car(), o.cdr())) ))),
					$("%def", "%subClass?", wrap(new JFun("%SubClass?", (n,o)-> checkN(n, o, 2, Class.class, Class.class), (l,o)-> o.<Class>car(1).isAssignableFrom(o.car()) ))),
					$("%def", "%type?",  wrap(new JFun("%Type?", (n,o)-> checkN(n, o, 2, null, or(null, Class.class)), (l,o)-> apply((o1, c)-> c == null ? o1 == null /*: o1 == null ? !c.isPrimitive()*/ : o1 != null && c.isAssignableFrom(o1.getClass()), o.car(), o.<Class>car(1)) ))),
					$("%def", "%classOf", wrap(new JFun("%ClassOf", (n,o)-> checkN(n, o, 1), (l,o)-> apply(o1-> o1 == null ? null : o1.getClass(), o.car()) ))),
					// Utilities
					$("%def", "%list", wrap(new JFun("%List", (ArgsList) o-> o))),
					$("%def", "%list*", wrap(new JFun("%List*", (ArgsList) this::listStar))),
					$("%def", "%len", wrap(new JFun("%Len", (n,o)-> checkM(n, o, 1, or(null, List.class)), (l,o)-> len(o.car()) ))),
					$("%def", "%list->array", wrap(new JFun("%List->array", (n,o)-> checkM(n, o, 1, List.class), (l,o)-> array(o) ))),
					$("%def", "%array->list", wrap(new JFun("%Array->list", (n,o)-> checkM(n, o, 1, Boolean.class, Object[].class), (l,o)-> list(o.car(), o.car(1)) ))),
					$("%def", "%reverse", wrap(new JFun("%Reverse", (n,o)-> checkM(n, o, 1, or(null, List.class)), (l,o)-> reverse(o.car()) ))),
					$("%def", "%append", wrap(new JFun("%Append", (n,o)-> checkM(n, o, 2, or(null, List.class)), (l,o)-> append(o.car(),o.car(1)) ))),
					// 
					$("%def", "%$", wrap(new JFun("%$", (BiFunction<Object,Object,String>) (a,b)-> Vm.this.toString(a) + Vm.this.toString(b)))),
					$("%def", "%+", wrap(new JFun("%+", (n,o)-> checkN(n, o, 2, Number.class, Number.class), (l,o)-> binOp(Pls, o.car(), o.car(1)) ))),
					$("%def", "%*", wrap(new JFun("%*", (n,o)-> checkN(n, o, 2, Number.class, Number.class), (l,o)-> binOp(Pwr, o.car(), o.car(1)) ))),
					$("%def", "%-", wrap(new JFun("%-", (n,o)-> checkN(n, o, 2, Number.class, Number.class), (l,o)-> binOp(Mns, o.car(), o.car(1)) ))),
					$("%def", "%/", wrap(new JFun("%/", (n,o)-> checkN(n, o, 2, Number.class, Number.class), (l,o)-> binOp(Dvd, o.car(), o.car(1)) ))),
					$("%def", "%%", wrap(new JFun("%%", (n,o)-> checkN(n, o, 2, Number.class, Number.class), (l,o)-> binOp(Rst, o.car(), o.car(1)) ))),
					//
					$("%def", "%!", wrap(new JFun("%!", (Function) a-> switch (istrue(a)) { case Suspension s-> s; case Boolean b-> !b; case Object obj-> typeError("not a boolean: {datum}", obj, symbol("Boolean")); }))),
					$("%def", "%!!", wrap(new JFun("%!!", (Function) a-> switch (istrue(a)) { case Suspension s-> s; case Boolean b-> b; case Object obj-> typeError("not a boolean: {datum}", obj, symbol("Boolean")); }))),
					$("%def", "%<", wrap(new JFun("%<", (n,o)-> checkN(n, o, 2, Number.class, Number.class), (l,o)-> binOp(Ls, o.car(), o.car(1)) ))),
					$("%def", "%>", wrap(new JFun("%>", (n,o)-> checkN(n, o, 2, Number.class, Number.class), (l,o)-> binOp(Gt, o.car(), o.car(1)) ))),
					$("%def", "%<=", wrap(new JFun("%<=", (n,o)-> checkN(n, o, 2, Number.class, Number.class), (l,o)-> binOp(Le, o.car(), o.car(1)) ))),
					$("%def", "%>=", wrap(new JFun("%>=", (n,o)-> checkN(n, o, 2, Number.class, Number.class), (l,o)-> binOp(Ge, o.car(), o.car(1)) ))),
					//
					$("%def", "%~", wrap(new JFun("%~", (n,o)-> checkN(n, o, 1, Integer.class), (l,o)-> ~o.<Integer>car() ))),
					$("%def", "%&", wrap(new JFun("%&", (n,o)-> checkN(n, o, 2, Number.class, Number.class), (l,o)-> binOp(And, o.car(), o.car(1)) ))),
					$("%def", "%|", wrap(new JFun("%|", (n,o)-> checkN(n, o, 2, Number.class, Number.class), (l,o)-> binOp(Or, o.car(), o.car(1)) ))),
					$("%def", "%^", wrap(new JFun("%^", (n,o)-> checkN(n, o, 2, Number.class, Number.class), (l,o)-> binOp(Xor, o.car(), o.car(1)) ))),
					$("%def", "%<<", wrap(new JFun("%<<", (n,o)-> checkN(n, o, 2, Number.class, Number.class), (l,o)-> binOp(Sl, o.car(), o.car(1)) ))),
					$("%def", "%>>", wrap(new JFun("%>>", (n,o)-> checkN(n, o, 2, Number.class, Number.class), (l,o)-> binOp(Sr, o.car(), o.car(1)) ))),
					$("%def", "%>>>", wrap(new JFun("%>>>", (n,o)-> checkN(n, o, 2, Number.class, Number.class), (l,o)-> binOp(Sr0, o.car(), o.car(1)) ))),
					//
					$("%def", "%==", wrap(new JFun("%==", (BiFunction<Object,Object,Boolean>) (a,b)-> a instanceof Number ? a.equals(b) : a == b ))),
					$("%def", "%!=", wrap(new JFun("%!=", (BiFunction<Object,Object,Boolean>) (a,b)-> a instanceof Number ? !a.equals(b) : a != b ))),
					$("%def", "%eq?", wrap(new JFun("%Eq?", (BiFunction<Object,Object,Boolean>) (a,b)-> Vm.this.equals(a, b) ))),
					//
					$("%def", "%quote", $("%vau", $("arg"), ignore, "arg")),
					$("%def", "%theEnv", $("%vau", null, "env", "env")),
					$("%def", "%\\", $("%vau", $("formals", ".", "body"), "env", $("%wrap", $("%eval", $("%list*", "%vau", "formals", ignore, "body"), "env")) )),
					//
					$("%def", "vm", this),
					$("%def", "%test", test),
					$("%def", "%assert", vmAssert),
					$("%def", "toString", wrap(new JFun("ToString", (Function<Object,String>) obj-> toString(obj)))),
					$("%def", "log", wrap(new JFun("Log", (ArgsList) o-> log(array(o))))),
					$("%def", "print", wrap(new JFun("Print", (ArgsList) o-> print(array(o))))),
					$("%def", "write", wrap(new JFun("Write", (ArgsList) o-> write(array(o))))),
					$("%def", "load", wrap(new JFun("Load", (Function<String, Object>) nf-> uncked(()-> loadText(nf))))),
					$("%def", "read", wrap(new JFun("Read", (n,o)-> checkR(n, o, 0, 1, Integer.class), (l,o)-> cons(begin, parseBytecode(uncked(()-> parse(read(l == 0 ? 0 : o.<Integer>car())))))))),
					$("%def", "dotco", wrap(new JFun("Dotco", (n,o)-> checkR(n, o, 0, 1, Boolean.class), (l,o)-> l == 0 ? dotco : inert(dotco=o.car()) ))),
					$("%def", "doasrt",  wrap(new JFun("Doasrt", (n,o)-> checkR(n, o, 0, 1, Boolean.class), (l,o)-> l == 0 ? doasrt : inert(doasrt=o.car()) ))),
					$("%def", "ctapv", wrap(new JFun("Ctapv",
						(n,o)-> checkR(n, o, 0, 1, Boolean.class),
						(l,o)-> l == 0 ? ctapv
								: inert(ctapv = o.car(),
									bind(null, theEnv,
										list(symbol("%catch"), symbol("%throw")),
										list((Object) apply(c-> !ctapv ? c : wrap(c), new Catch()),
											 (Object) apply(t-> !ctapv ? t : wrap(t), new Throw())))) ))),
					$("%def", "prtrc", wrap(new JFun("Prtrc", (n,o)-> checkR(n, o, 0, 1, or(0, 1, 2, 3, 4, 5, 6)), (l,o)-> l == 0 ? prtrc : inert(start=level-(dotco ? 0 : 3), prtrc=o.car()) ))),
					$("%def", "ttrue", wrap(new JFun("Ttrue", (n,o)-> checkR(n, o, 0, 1, or(0, 1, 2, 3, 4)), (l,o)-> l == 0 ? ttrue : inert(ttrue=o.car()) ))),
					$("%def", "bndres", wrap(new JFun("Bndres", (n,o)-> checkR(n, o, 0, 1, or(0, 1, 2)), (l,o)-> l == 0 ? bndres : inert(bndres=o.car()) ))),
					$("%def", "prenv", wrap(new JFun("Prenv", (n,o)-> checkR(n, o, 0, 1, Integer.class), (l,o)-> l == 0 ? prenv : inert(prenv=o.car()) ))),
					$("%def", "prstk", wrap(new JFun("Prstk", (n,o)-> checkR(n, o, 0, 1, Boolean.class), (l,o)-> l == 0 ? prstk : inert(prstk=o.car()) ))),
					$("%def", "prwrn", wrap(new JFun("Prwrn", (n,o)-> checkR(n, o, 0, 1, Boolean.class), (l,o)-> l == 0 ? prwrn : inert(prwrn=o.car()) )))
				)
		)));
	}
	
	
	// API
	public Object exec(Object bytecode) {
		var wrapped = pushRootPrompt(cons(new Begin(true), parseBytecode(bytecode)));
		return pushSubcontBarrier(null, theEnv, wrapped);
	}
	public Object call(String funName, Object ... args) {
		return exec(list(symbol(funName), parseBytecode(args)));
	}
	public Object get(String varName) {
		return exec(symbol(varName));
	}
	public Object eval(String exp) throws Exception {
		return exec(parse(exp));
	}
	public void compile(String fileName) throws Exception {
		try (var oos = new ObjectOutputStream(new FileOutputStream("build/" + fileName))) {
			oos.writeObject(parse(readText(fileName)));
		}
	}
	public String readText(String fileName) throws IOException {
		return Files.readString(Paths.get(fileName), Charset.forName("cp1252"));
	}
	public Object readBytecode(String fileName) throws Exception {
		try (var ois = new ObjectInputStream(new FileInputStream("build/" + fileName))) {
			return ois.readObject();
		}
	}
	public Object loadText(String fileName) throws Exception {
		if (prtrc >= 1) print("\n--------: " + fileName);
		var v = eval(readText(fileName));
		if (prtrc > 1) print("--------: " + fileName + " end");
		return v;
	}
	public Object loadBytecode(String fileName) throws Exception {
		if (prtrc >= 1) print("\n--------: " + fileName);
		var v = exec(readBytecode(fileName));
		if (prtrc > 1) print("--------: "  + fileName + " end");
		return v;
	}
	public void repl() throws Exception {
		loop: for (;;) {
			switch (read()) {
				case "" : break loop;
				case "\n": break;
				case String exp: try {
					print(exec(parse(exp)));
				}
				catch (Throwable thw) {
					if (prstk) thw.printStackTrace(out);
					else out.println(/*thw instanceof Obj ? thw : */ "{" + thw.getClass().getSimpleName() + " " + thw.getMessage() + "}");
				}
			}
		}
		print("\nfinito");
	}
	
	
	// Test
	public static void main(String[] args) throws Exception {
		new Vm().main();
	}
	public void main() throws Exception {
		//*
		var milli = currentTimeMillis();
		loadText("lispx/vm.lispx");
		print("start time: " + (currentTimeMillis() - milli));
		repl();
		//*/
		//extend2(symbol("Obj2"), null);
		//print(parseBytecode("or","#null","List"));
	}
}
