package Wat;

import static List.Parser.toByteCode;
import static Wat.Utility.$;
import static Wat.Utility.apply;
import static Wat.Utility.binOp;
import static Wat.Utility.eIf;
import static Wat.Utility.eIfnull;
import static Wat.Utility.first;
import static Wat.Utility.getClasses;
import static Wat.Utility.getExecutable;
import static Wat.Utility.getField;
import static Wat.Utility.headAdd;
import static Wat.Utility.ifnull;
import static Wat.Utility.isInstance;
import static Wat.Utility.member;
import static Wat.Utility.more;
import static Wat.Utility.or;
import static Wat.Utility.read;
import static Wat.Utility.reorg;
import static Wat.Utility.stackDeep;
import static Wat.Utility.toSource;
import static Wat.Utility.uncked;
import static Wat.Utility.BinOp.And;
import static Wat.Utility.BinOp.Dvd;
import static Wat.Utility.BinOp.Mns;
import static Wat.Utility.BinOp.Or;
import static Wat.Utility.BinOp.Pls;
import static Wat.Utility.BinOp.Pwr;
import static Wat.Utility.BinOp.Rst;
import static Wat.Utility.BinOp.Sl;
import static Wat.Utility.BinOp.Sr;
import static Wat.Utility.BinOp.Sr0;
import static Wat.Utility.BinOp.Xor;
import static java.lang.Runtime.version;
import static java.lang.System.currentTimeMillis;
import static java.lang.System.out;
import static java.util.Arrays.fill;
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
import java.util.Map.Entry;
import java.util.Objects;
import java.util.Set;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.regex.Pattern;

import javax.tools.DiagnosticCollector;
import javax.tools.JavaFileObject;
import javax.tools.SimpleJavaFileObject;

import List.ParseException;

// java.exe -cp bin --enable-preview Wat.Vm

/* Abbreviations:
	c: cons
	x, exp, expt, cln: expression, expected, cleaner
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
	dt: definiens tree
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
	lst: list
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
	dft: default 
 */

public class Vm {
	
	static {
		for (File file: new File("bin/Ext").listFiles()) file.delete();
	}
	
	boolean doTco = true; // do tco
	boolean doAsrt = true; // do assert
	boolean ctApv = false; // applicative catch & throw
	boolean intStr = false; // intern string
	boolean prStk = false; // print stack
	boolean prWrn = false; // print warning
	boolean prMap = false; // print error map entry
	boolean aQuote = true; // auto quote list
	boolean else1 = false; // else multiple expressions
	boolean hdlAny = true; // any value for catch hadler
	
	Object boxDft = null; // box/dinamic default: null, inert, ...
	
	int prTrc = 0; // print trace: 0:none, 1:load, 2:eval root, 3:eval all, 4:return, 5:combine, 6:bind/lookup
	int typeT = 0; // type true: 0:true, 1:!false, 2:!(or false null), 3:!(or false null inert), 4:!(or false null inert zero)
	int prEnv = 10; // print environment
	int bndRes = 0; // bind result: 0:inert 1:rhs 2:prv
	private Object bndRes(Object o, boolean obj) {
		switch (o) {
			case Inert i: return 0; 
			case Ignore i: return bndRes;
			case Keyword k: switch (k.name) {
				case "rhs": return 1;
				case "prv": return 2;
				case "obj": if (obj) return 3;
			};
			default: return typeError("cannot determine bndRes, not {expected}: {datum}", o,
				toChk(obj
					? or(inert, ignore, keyword(":rhs"), keyword(":prv"), keyword(":obj"))
					: or(inert, ignore, keyword(":rhs"), keyword(":prv"))
				)
			);
		}
	}
	
	
	// Continuations
	class Continuation {
		Function<Resumption, Object> f; Continuation nxt; Dbg dbg;
		Continuation(Dbg dbg, Function<Resumption, Object> f, Continuation next) {
			this.f = f; this.nxt = next; this.dbg = dbg;
		}
		public String toString() { return "{Continuation %s}".formatted(dbg); }
		Object apply(Env e, List lst) { return apply(()-> getTco(begin.combine(e, lst))); }
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
		return res instanceof Suspension s ? s.suspend(dbg, rr-> pipe(rr, dbg, before, after)) : pipe(null, dbg, res, 0, after);
	}
	Object pipe(Resumption r, Dbg dbg, Object res, int i, Function ... after) {
		for (var first=true; i<after.length; i+=1) { // only one resume for suspension
			res = first && r != null && !(first = false) ? r.resume() : after[i].apply(res);
			//if (res instanceof Tco && i < after.length-1) out.println /*throw new Error*/ ("do getTco 2"); 
			if (res instanceof Suspension s) { var ii=i; var rres=res; return s.suspend(dbg, rr-> pipe(rr, dbg, rres, ii, after)); }
		}
		return res;
	}
	Object map(Function f, List todo) {
		return map(null, f, todo, null);
	}
	Object map(Resumption r, Function f, List todo, List done) {
		for (var first=true;;) { // only one resume for suspension
			if (todo == null) return reverse(done); 
			var res = first && r != null && !(first = false) ? r.resume() : f.apply(todo.car);
			//if (res instanceof Tco) out.println /*throw new Error*/("do getTco 3"); 
			if (res instanceof Suspension s) { List td=todo, dn=done; return s.suspend(dbg(null, "map", todo.car), rr-> map(rr, f, td, dn)); }
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
	
	class SheBang { public String toString() { return "#!"; }};
	public SheBang sheBang = new SheBang();
	
	// Tail Call Optimization
	class Tco implements Supplier {
		Supplier tco;
		Tco(Supplier tco) { this.tco = tco; }
		@Override public Object get() { return tco.get(); }
		@Override public String toString() { return "Tco"; }
	};
	Object tco(Supplier tco) { return doTco ? new Tco(tco) : tco.get(); }
	<T> T getTco(Object o) { while (o instanceof Tco tco) o = tco.get(); return (T) o; }
	
	
	// Trace Logging
	int level=0, start=0; String indent = "|  ";
	String indent() { return indent.repeat(level-start) + "|" + stackDeep() + ":  " ; }
	
	
	// Evaluation Core
	<T> T evaluate(Env e, Object o) {
		if (prTrc >= 3) print("evaluate: ", indent(), o, "   ", e);
		Object v; try {
			level += 1;
			v = switch (o) {
				case Symbol s-> tco(()-> pipe(dbg(e, o), ()-> e.get(s)));
				case List l-> tco(()-> pipe(dbg(e, o), ()-> getTco(evaluate(e, l.car)), op-> tco(()-> combine(e, op, l.cdr()))));
				case null, default-> o;
			};
		}
		finally {
			level -= 1;
		}
		if (prTrc >= 4) print("  return: ", indent(), v=getTco(v)); 
		return (T) v;
	}
	
	// Basic Object
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
		Object car; private Object cdr;
		Cons(Object car, Object cdr) { this.car = car; this.cdr = cdr; }
		public String toString() {
			if (car instanceof Symbol s) switch (s.name) {
				case "%,": return "," + Vm.this.toString(car(1));
				case "%,@": return ",@" + Vm.this.toString(car(1));
				case "%`": return "`" + Vm.this.toString(car(1));
				case "%'", "quote": return "'" + Vm.this.toString(car(1));
			}
			return "(" + toString(this) + ")";
		}
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
		public <T> T car(int i) { Cons o=this; for (; i>0 && o.cdr instanceof Cons c; i-=1, o=c); return i==0 ? o.car() : error("cannot get car", "type", symbol("outOfBounds")); }
		public <T> T cdr(int i) { Cons o=this; for (; i>0 && o.cdr instanceof Cons c; i-=1, o=c); return i==0 ? o.cdr() : error("cannot get cdr", "type", symbol("outOfBounds")); }
		<T extends Cons> T setCar(Object car) { this.car = car; return (T) this; }
		<T extends Cons> T setCdr(Object cdr) { this.cdr = cdr; return (T) this; }
	}
	public class List extends Cons {
		List(Object car, List cdr) { super(car, cdr); }
		@Override public List cdr() { return super.cdr(); }
		List setCdr(List cdr) { return super.setCdr(cdr); }
		@Override List setCdr(Object cdr) { return cdr == null || cdr instanceof List ? setCdr(cdr) : typeError("cannot set cdr, not a {expected}: {datum}", cdr, symbol("List") ); }
	}
	public int len(List o) { int i=0; for (; o != null; i+=1, o=o.cdr()); return i; }
	public int len(Object o) { int i=0; for (; o instanceof Cons c; i+=1, o=c.cdr()); return i /*o == null ? i : i + .5*/; }
	//public Object arity(Object o) { int i=0; for (; o instanceof Cons c; i+=1, o=c.cdr()); return o == null ? i : list(symbol(">="), i); }
	<T extends Cons> T cons(Object car) { return (T) new List(car, null); }
	<T extends Cons> T cons(Object car, Object cdr) {
		return (T)(cdr == null || cdr instanceof List ? new List(car, (List) cdr) : new Cons(car, cdr));
	}
	
	
	// Environment
	interface ObjEnv {
		Object value(Object key);
		boolean isBound(Object key);
		boolean remove(Object key);
		List keys();
		Object get(Object key);
	}
	class Env implements ArgsList, ObjEnv {
		Env parent; LinkedHashMap<String,Object> map = new LinkedHashMap();
		Env(Env parent, Obj obj) {
			this.parent = parent;
			map = obj.map;
		}
		Env(Env parent, Object ... objs) {
			this.parent = parent;
			if (objs == null) return;
			for (int i=0, e=objs.length; i<e; i+=1) map.put(toKey(objs[i]), objs[i+=1]);
		}
		record Lookup(boolean isBound, Object value) {}
		Lookup lookup(Object obj) {
			var key = toKey(obj);
			for (var env=this; env != null; env=env.parent) {
				Object res = env.map.get(key);
				if (res != null || env.map.containsKey(key)) return new Lookup(true, res);
			}
			return new Lookup(false, null);
		};
		public Object value(Object obj) {  return lookup(obj).value; }
		public boolean isBound(Object obj) { return lookup(obj).isBound; }
		public Object get(Object obj) {
			var lookup = lookup(obj);
			if (!lookup.isBound) return unboundSymbolError("unbound symbol: {symbol}", obj, this);
			if (prTrc >= 6) print("  lookup: ", lookup.value); return lookup.value;
		}
		Object def(Object obj, Object value) {
			var key = toKey(obj);
			if (prTrc >= 6) print("    bind: ", key, "=", value, " in: ", this);
			return map.put(key, value);
		}
		Object set(Object obj, Object value) {
			var key = toKey(obj);
			for (var env=this; env != null; env=env.parent) {
				if (!env.map.containsKey(key)) continue;
				if (prTrc >= 6) print("     set: ", key, "=", value, " in: ", env);
				return env.map.put(key, value);
			}
			return unboundSymbolError("unbound symbol: {symbol}", obj, this);
		};
		boolean isParent(Env other) {
			for (var env=this; env != null; env=env.parent) if (other == env) return true; 
			return false;
		};
		int deep() { int i=0; for (var env=this; env != null; env=env.parent) i+=1; return i; }
		public String toString() {
			return "{" + (this==theEnv ? "The" : this==vmEnv ? "Vm" : "") + "Env" + (map.size() > prEnv ? "[" + map.size() + "] ..." : toStringSet(map.reversed().entrySet())) + eIfnull(parent, ()-> " " + parent) + "}";
		}
		public boolean remove(Object obj) {
			var key = toKey(obj);
			for (var env=this; env != null; env=env.parent) {
				if (!env.map.containsKey(key)) continue;
				env.map.remove(key);
				return true;
			}
			return false;
		};
		public List keys() {
			var keys = map.keySet();
			for (var env=parent; env != null; env=env.parent) keys.addAll(env.map.keySet());
			return list(keys.toArray());
		}
		@Override public Object apply(List o) {
			var len = len(o);
			return switch (len(o)) {
				case 0-> this;
				case 1-> get(o.car);
				default-> {
					BiFunction f =	o.car == keyword(":def") ? (k,v)-> def(k,v) : o.car == keyword(":set!") ? (k,v)-> set(k,v) : null;
					if (f == null) f = (BiFunction) (k,v)-> def(k,v); else { len-=1; o=o.cdr();  }
					var bndRes = len % 2 == 0 ? ignore : first(o.car, len-=1, o=o.cdr());
					for (; len>2; len-=2, o=o.cdr(1)) f.apply(toKey(o.car), o.car(1));
					var key = toKey(o.car);
					yield switch (bndRes(bndRes, true)) {
						case Suspension s-> s;
						case Integer i-> switch(i) {
							case 0-> inert(f.apply(key, o.car(1)));
							case 1-> { var v = o.car(1); f.apply(key, v); yield v; }
							case 2-> f.apply(key, o.car(1));
							case 3-> { f.apply(key, o.car(1)); yield this; }
							default-> typeError("cannot def/set! env, invalid bndRes value, not {expected}: {datum}", i, toChk(or(0, 1, 2, 3)));
						};
						case Object obj-> resumeError(obj, symbol("Integer"));
					};
				}
			};
		}
	}
	Env env() { return env(null); }
	Env env(Env env, Obj obj) { return new Env(env, obj); }
	Env env(Env env, Object ... objs) { return new Env(env, objs); }
	
	
	// Box, Obj, Condition, Error
	public class Box implements ArgsList {
		Object value;
		public Box (Object val) { this.value = val; }
		@Override public Object apply(List o) { // () | (value) | (:key value)
			var chk = checkR(this, o, 0, 2);
			if (chk instanceof Suspension s) return s;
			if (!(chk instanceof Integer len)) return resumeError(chk, symbol("Integer"));
			return switch (len) {
				case 0-> value;
				default-> switch (bndRes(len != 2 ? ignore : o.car, true)) {
					case Suspension s-> s;
					case Integer i-> switch(i) {
						case 0-> inert(value = o.car(len-1));
						case 1-> value = o.car(len-1);
						case 2-> { var v = value; value = o.car(len-1); yield v; }
						case 3-> { value = o.car(len-1); yield this; }
						default-> typeError("cannot set box, invalid bndRes value, not {expected}: {datum}", i, toChk(or(0, 1, 2, 3)));
					};
					case Object obj-> resumeError(obj, symbol("Integer"));
				}; 
			};
		}
		public String toString() { return "{&" + getClass().getSimpleName() + " " + Vm.this.toString(value) + "}"; }
	}
	public class Obj extends RuntimeException implements ArgsList, ObjEnv {
		private static final long serialVersionUID = 1L;
		LinkedHashMap<String,Object> map = new LinkedHashMap();
		public Obj(Object ... objs) { puts(objs); }
		public Obj(String msg, Object ... objs) { super(msg); puts(objs); }
		public Obj(Throwable t, Object ... objs) { super(t); puts(objs); }
		public Obj(String msg, Throwable t, Object ... objs) { super(msg, t); puts(objs); }
		public Object value(Object key) { return map.get(toKey(key)); }
		public boolean isBound(Object key) { return map.containsKey(toKey(key)); }
		public boolean remove(Object key) { key=toKey(key); if (!map.containsKey(key)) return false; map.remove(key); return true; }
		public List keys() { return list(map.keySet().toArray()); }
		public Object get(Object obj) {
			var key = toKey(obj);
			var val = map.get(key);
			return val != null || map.containsKey(key) ? val : unboundFieldError("slot: {field} not found in: {object}", key, this);
		}
		Object puts(Object ... objs) {
			if (objs == null) return null;
			Object last = null;
			for (int i=0, e=objs.length; i<e; i+=1) last = map.put(toKey(objs[i]), objs[i+=1]);
			return last;
		}
		@SuppressWarnings("unused")
		@Override public String toString() {
			var s = "";
			for (Throwable thw = this; thw != null; thw = thw.getCause()) {
				if (thw instanceof Exception && !(thw instanceof RuntimeException)) break;
				s += (s.length() == 0 ? "" : "\n") + (thw instanceof Obj obj ? toString(obj) : "{&" + thw.getClass().getSimpleName() + " " + thw.getMessage() + "}");
				break; // TODO per il momento solo il primo 
			}
			return s;
		}
		public String toString(Obj obj) {
			return "{&" + obj.getClass().getCanonicalName() // or .getSimpleName()?
				+ eIfnull(obj.getMessage(), m-> " " + Vm.this.toString(true, m))
				//+ eIfnull(getCause(), t-> " " + symbol(t.getClass().getSimpleName()))
				+ (map.size() > 10 ? " [" + map.size() + "]" + " ..." : toStringSet(obj.map.entrySet()))
				+ "}"
			;
		}
		@Override public Object apply(List o) {
			var len = len(o);
			if (len == 0) return this;
			if (len == 1) return get(o.car);
			var bndRes = len % 2 == 0 ? ignore : first(o.car, len-=1, o=o.cdr());
			for (; len>2; len-=2, o=o.cdr(1)) map.put(toKey(o.car), o.car(1));
			var key = toKey(o.car);
			return switch (bndRes(bndRes, true)) {
				case Suspension s-> s;
				case Integer i-> switch(i) {
					case 0-> inert(map.put(key, o.car(1)));
					case 1-> { var v = o.car(1); map.put(key, v); yield v; }
					case 2-> map.put(key, o.car(1));
					case 3-> { map.put(key, o.car(1)); yield this; }
					default-> typeError("cannot set obj, invalid bndRes value, not {expected}: {datum}", i, toChk(or(0, 1, 2, 3)));
				};
				case Object obj-> resumeError(obj, symbol("Integer"));
			};
		}
		private static final Pattern keyword = Pattern.compile("\\{(.+?)\\}");
		@Override public String getMessage() {
			var msg = super.getMessage(); if (msg == null) return null;
			var matcher = keyword.matcher(msg); if (!matcher.find()) return msg;
			var sb = new StringBuffer(); do {
				var s = matcher.group(1);
				var i = s.indexOf(","); 
				var k = toKey(i == -1 ? s : s.substring(0, i));
				var v = value(k);
				matcher.appendReplacement(sb, (v != null || isBound(k) ? (i == -1 ? Vm.this.toString(true, v) : s.substring(i+1).formatted(v)) : "{"+ s +"}" ).replace("\\", "\\\\").replace("$", "\\$"));
			} while(matcher.find());
			matcher.appendTail(sb);
			return sb.toString();
		}
	}
	public class Condition extends Obj {
		private static final long serialVersionUID = 1L;
		public Condition(Object ... objs) { super(objs); }
		public Condition(String message, Object ... objs) { super(message, objs); }
		public Condition(Throwable cause, Object ... objs) { super(cause, objs); }
		public Condition(String message, Throwable cause, Object ... objs) { super(message, cause, objs); }
	}
	public class Error extends Condition {
		private static final long serialVersionUID = 1L;
		public Error(Object ... objs) { super(objs); }
		public Error(String message, Object ... objs) { super(message, objs); }
		public Error(Throwable cause, Object ... objs) { super(cause, objs); }
		public Error(String message, Throwable cause, Object ... objs) { super(message, cause, objs); }
	}
	
	
	// Class
	Object newClass(Symbol className, Class superClass) {
		try {
			if (!className.name.matches("[A-Z][a-zA-Z_0-9]*")) return typeError("invalid class name, not {expected}: {datum}", className, toLispList("regex", "[A-Z][a-zA-Z_0-9]*"));
			if (superClass != null && !of(Obj.class, Box.class).anyMatch(rc-> rc.isAssignableFrom(superClass))) return typeError("invalid superclass, not {expected}: {datum}", superClass, toChk(or(Obj.class, Box.class)));
			var c = Class.forName("Ext." + className);
			if (prWrn) out.println("Warning: class " + className + " is already defined!");
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
							+ """
								public %1$s(Vm vm, %3$s o) { %4$so); }
							}
							""").formatted(
								className,
								isObj ? "Vm.Obj" : superClass.getCanonicalName(),
								isBox ? "Object" : "Object ... ", 
								isObj || member(superClass, Condition.class, Error.class, Box.class) ? "vm.super(" : "super(vm, "
							)
						)
					)
				);
				if (!task.call()) {
					System.out.println(diagnostics.getDiagnostics());
					return javaError("error defining class {member}", null, className, null, null);
				}
				new File("bin/Ext/" + className + ".class").deleteOnExit();
				return Class.forName("Ext." + className);
			}
			catch (Throwable thw) {
				return javaError("error defining class {member}", thw, className, null, null);
			}
		}
	}
	
	
	// Methods
	Map<Class, Map<Symbol,Object>> methods = new LinkedHashMap<>();
	Object addMethod(Class cls, Symbol name, Object method) {
		return methods.computeIfAbsent(cls, k-> new LinkedHashMap<>()).put(name, method);
	}
	Object getMethod(Class cls, Symbol symbol) {
		var c = cls; do {
			var ms = methods.get(c); if (ms == null) continue;
			var m = ms.get(symbol); if (m != null) return m;
		} while (c != null && (c = c.getSuperclass()) != null);
		return unboundExecutableError("method: {executable} not found in: {class}", symbol, cls);
	}	
	
	
	// Bind
	public class InnerException extends RuntimeException {
		private static final long serialVersionUID = 1L;
		Object[] objs;
		public InnerException(String message, Object ... objs) { super(message); this.objs = objs; }		
	}
	public class MatchException extends InnerException {
		private static final long serialVersionUID = 1L;
		public MatchException(String message, Object datum, int operands) { super(message, "datum", datum, "operands#", operands); }
	}
	public class TypeException extends InnerException {
		private static final long serialVersionUID = 1L;
		public TypeException(String message, Object datum, Object expected) { super(message, "datum", datum, "expected", expected); }
	}
	<T> T matchError(String msg, Object ... objs) {
		return error(msg, headAdd(objs, "type", symbol("match")));
	}
	Object bind(Dbg dbg, Env e, Object lhs, Object rhs) {
		return bind(dbg, true, bndRes, e, lhs, rhs);
	}
	Object bind(Dbg dbg, boolean def, int bndRes, Env e, Object lhs, Object rhs) {
		return bind(null, dbg, def, bndRes, e, lhs, rhs); 
	}
	Object bind(Resumption r, Dbg dbg, boolean def, int bndRes, Env e, Object lhs, Object rhs) {
		try {
			var res = bind(def, bndRes, e, lhs, rhs);
			return bndRes == 0 ? inert : res;
		}
		catch (InnerException ie) {
			return error(
				(def ? "bind" : "sett") + "ing: " + toString(lhs)
				+ eIfnull(dbg, ()-> " of: " + (dbg.op instanceof Opv opv ? opv : cons(dbg.op, dbg.os[0])))
				+ " with: " + rhs,
				ie
			);
		}
	}
	Object bind(boolean def, int bndRes, Env e, Object lhs, Object rhs) {
		return switch (lhs) {
			case Ignore i-> rhs;
			case Symbol s-> { var v = def ? e.def(s, rhs) : e.set(s, rhs); yield bndRes == 2 ? v : rhs; }  
			case Keyword k-> {
				if (k.equals(rhs)) yield rhs;
				throw new TypeException("expected keyword: {expected}, found: {datum}", rhs, k);
			}
			case null-> {
				if (rhs == null) yield null;
				throw new MatchException("expected {operands#,%+d} operands, found: {datum}", rhs, -len(rhs));
			}
			case Cons lc-> {
				if (member(lc.car, quotes)) {
					if (equals(lc.<Object>car(1), rhs)) yield null; // or rhs?
					throw new TypeException("expected literal: {expected}, found: {datum}", rhs, lc.car(1));
				}
				else if (lc.car == sheBang) {
					getTco(evaluate(e, list(symbol("%checkO"), rhs, lc.car(1))));
					yield bind(def, bndRes, e, lc.car(2), rhs); 
				}
				else if (!(rhs instanceof Cons rc)) {
					throw new MatchException("expected {operands#,%+d} operands, found: {datum}", rhs, len(lc));
				}
				else {
					var res = bind(def, bndRes, e, lc.car, rc.car);
					yield lc.cdr() == null && rc.cdr() == null ? res : bind(def, bndRes, e, lc.cdr(), rc.cdr());
				}
			}
			default-> {
				if (equals(lhs, rhs)) yield null; // or rhs?
				throw new TypeException("expected literal: {expected}, found: {datum}", rhs, lhs);
			}
		};
	}
	
	
	// Operative & Applicative Combiners
	interface Combinable { <T> T combine(Env e, List o); }
	
	Object combine(Env e, Object op, List o) {
		if (prTrc >= 5) print(" combine: ", indent(), op, " ", o /*, "   ", e*/);
		if (op instanceof Combinable cmb) return cmb.combine(e, o);
		// per default le jFun nude sono considerate applicative
		if (isjFun(op)) return new Apv(new JFun(op)).combine(e, o);
		return aQuote ? cons(op, o) : typeError("cannot combine, not a {expected}: {datum} in: " + cons(op, o), op, symbol("Combinable"));
	}
	
	class Opv implements Combinable  {
		Env e; Object pt, ep; List x;
		Opv(Env e, Object pt, Object ep, List x) {
			this.e = e; this.pt = pt; this.ep = ep;
			this.x = x != null && x.cdr() != null && x.car instanceof String ? x.cdr() : x;
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
		public String toString() { return "{%Opv " + ifnull(pt, "()", Vm.this::toString) + " " + Vm.this.toString(ep) + eIfnull(x, ()-> " " + apply(s-> !(x instanceof Cons) ? s : s.substring(1, s.length()-1), Vm.this.toString(x))) + /*" " + e +*/ "}"; }
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
		: typeError("cannot wrap, not a {expected}: {datum}", arg, symbol("Combinable"));
	}
	Object unwrap(Object arg) {
		return arg instanceof Apv apv ? apv.cmb
		: isjFun(arg) ? new JFun(arg)
		: arg instanceof Combinable ? arg
		: typeError("cannot unwrap, not a {expected}: {datum}", arg, symbol("Apv"));
	}
	Opv opv(Env e, Object pt, Object pe, List body) { return new Opv(e, pt, pe, body); } 
	Apv lambda(Env e, Object pt, List body) { return new Apv(opv(e, pt, ignore, body)); }
	Apv apv1(Env e, Symbol sym, List body) { return lambda(e, list(sym), body); }
	Apv apv0(Env e, List body) { return lambda(e, null, body); }
	
	
	// Built-in Combiners
	class Vau implements Combinable  {
		public Object combine(Env e, List o) {
			var chk = checkM(this, o, 2); // o = (pt ep . forms)
			if (chk instanceof Suspension s) return s;
			if (!(chk instanceof Integer len)) return resumeError(chk, symbol("Integer"));
			var pt = o.car;
			var ep = o.car(1);
			var msg = checkPt(cons(this, o), pt, ep); if (msg != null) return msg;
			return new Opv(e, pt, ep, o.cdr(1));
		}
		public String toString() { return "%Vau"; }
	};
	class Def implements Combinable  {
		boolean def;
		Def(boolean def) { this.def = def; }
		public Object combine(Env e, List o) {
			var chk = checkR(this, o, 2, 3); // o = (dt val) | (dt (or #ignore #inert :rhs :prv) val)
			if (chk instanceof Suspension s) return s;
			if (!(chk instanceof Integer len)) return resumeError(chk, symbol("Integer"));
			var dt = o.car;
			var msg = checkDt(cons(this, o), dt); if (msg != null) return msg;
			var dbg = dbg(e, this, o);
			return pipe(dbg, ()-> getTco(evaluate(e, o.car(len-1))), res-> 
				switch (bndRes(len != 3 ? ignore : o.car(1), false)) {
					case Suspension s-> s;
					case Integer i-> i >= 0 && i <= 2
						? bind(dbg, def, i, e, dt, res)
						: typeError("cannot " + (def ? "def" : "set!") + ", invalid bndRes value, not {expected}: {datum}", i, toChk(or(0, 1, 2)));
					case Object obj-> resumeError(chk, symbol("Integer"));
				}
			);
		}
		public String toString() { return def ? "%Def" : "%Set!"; }
	};
	class Eval implements Combinable  {
		public Object combine(Env e, List o) {
			var chk = checkR(this, o, 1, 2, Any.class, Env.class); // o = (x) | (x eo)
			if (chk instanceof Suspension s) return s;
			if (!(chk instanceof Integer len)) return resumeError(chk, symbol("Integer"));
			return evaluate(len == 1 ? e : o.car(1), o.car);
		}
		public String toString() { return "%Eval"; }
	}
	
	
	// First-Order Control
	class Begin implements Combinable  {
		boolean root;
		Begin() {}; Begin(boolean root) { this.root = root; } 
		public Object combine(Env e, List o) {
			// o = () | (form . forms)
			return o == null ? inert : tco(()-> combine(null, e, o));
		}
		Object combine(Resumption r, Env e, List lst) {
			for (var first = true;;) { // only one resume for suspension
				if (prTrc >= 3 && root && r == null) print("\n--------");
				var car = lst.car;
				if (prTrc == 2 && root && r == null) print("evaluate: ", car);
				if (lst.cdr() == null) { return evaluate(e, car); } 
				var res = first && r != null && !(first = false) ? r.resume() : getTco(evaluate(e, car));
				if (res instanceof Suspension s) { var l = lst; return s.suspend(dbg(e, this, lst.car), rr-> combine(rr, e, l)); }
				lst = lst.cdr();
			}
		}
		public String toString() { return "%Begin" + eIf(!root, "*"); }
	}
	Begin begin = new Begin();
	class If implements Combinable  {
		public Object combine(Env e, List o) {
			return combine(null, e, o);
		}
		Object combine(Resumption r, Env e, List o) {
			var chk = checkR(this, o, 2, else1 ? 3 : more); // o = (test then . else) 
			if (chk instanceof Suspension s) return s;
			if (!(chk instanceof Integer len)) return resumeError(chk, symbol("Integer"));
			var car = o.car;
			return tco(()-> pipe(dbg(e, this, o), ()-> getTco(evaluate(e, car)), test->
				switch (istrue(test)) {
					case Suspension s-> s;
					case Boolean b-> b ? tco(()-> evaluate(e, o.car(1)))
						: o.cdr(1) == null ? inert : else1 ? tco(()-> evaluate(e, o.car(2))) : tco(()-> begin.combine(e, o.cdr(1)));
					case Object obj-> resumeError(obj, symbol("Boolean"));
				}
			));
		}
		public String toString() { return "%If"; }
	}
	Object istrue(Object res) {
		return switch (typeT) {
			case 0-> res instanceof Boolean b ? b : typeError("not a {expected}: {datum}", res, symbol("Boolean")); // Kernel, Wat, Lispx
			case 1-> !member(res, false); // Scheme, Guile, Racket
			case 2-> !member(res, false, null); // Clojure
			case 3-> !member(res, false, null, ignore);
			case 4-> !member(res, false, null, ignore, 0); // Javascript
			default-> res instanceof Boolean b ? b : typeError("not a {expected}: {datum}", res, symbol("Boolean")); // Kernel, Wat, Lispx
		};
	}
	class IfStar implements Combinable  {
		public Object combine(Env e, List o) {
			var chk = checkR(this, o, 2, more); // o = (test then . else) 
			if (chk instanceof Suspension s) return s;
			if (!(chk instanceof Integer len)) return resumeError(chk, symbol("Integer"));
			return combine(null, e, o);
		}
		Object combine(Resumption r, Env e, List o) {
			if (o == null) return inert;
			var car = o.car;
			if (o.cdr() == null) return tco(()-> evaluate(e, car)); 
			return tco(()-> pipe(dbg(e, this, o), ()-> getTco(evaluate(e, car)), test->
				switch (istrue(test)) {
					case Suspension s-> s;
					case Boolean b-> b ? tco(()-> evaluate(e, o.car(1))) : tco(()-> combine(null, e, o.cdr(1)));  
					case Object obj-> resumeError(obj, symbol("Boolean"));
				}
			));
		}
		public String toString() { return "%If*"; }
	}
	class Loop implements Combinable  {
		public Object combine(Env e, List o) {
			var chk = checkM(this, o, 1); // o = (form . forms)
			if (chk instanceof Suspension s) return s;
			if (!(chk instanceof Integer len)) return resumeError(chk, symbol("Integer"));
			return combine(null, e, o);
		}
		public Object combine(Resumption r, Env e, List o) {
			for (var first = true;;) { // only one resume for suspension
				var res = first && r != null && !(first = false) ? r.resume() : getTco(begin.combine(e, o));
				if (res instanceof Suspension s) return s.suspend(dbg(e, this, o), rr-> combine(rr, e, o));
			}
		}
		public String toString() { return "%Loop"; }
	}
	class CatchTagWth implements Combinable {
		public Object combine(Env e, List o) {
			// (catch . forms)               -> (%CatchTagWth #_   #_ . forms)
			// (catchTag tag . forms)        -> (%CatchTagWth tag  #_ . forms)
			// (catchWth hdl . forms)        -> (%CatchTagWth #_  hdl . forms)
			// (catchTagWth tag hdl . forms) -> (%CatchTagWth tag hdl . forms)
			var chk = !ctApv ? checkM(this, o, 2) : checkN(this, o, 3, Any.class, hdlAny ? Any.class : or(ignore, Apv1.class), Apv0.class);
			if (chk instanceof Suspension s) return s;
			if (!(chk instanceof Integer len)) return resumeError(chk, symbol("Integer"));
			return pipe(dbg(e, this, o), ()-> ctApv ? o.car : getTco(evaluate(e, o.car)), tag-> combine(null, e, tag, o.car(1), o.cdr(1)) ); 
		}
		private Object combine(Resumption r, Env e, Object tag, Object hdl, List xs) {
			try {
				var res = r != null ? r.resume() : getTco(!ctApv ? begin.combine(e, xs) : Vm.this.combine(e, xs.car, null));
				return res instanceof Suspension s ? s.suspend(dbg(e, this, tag, xs, hdl), rr-> combine(rr, e, tag, hdl, xs)) : res;
			}
			catch (Throwable thw) {
				if (tag != ignore && thw instanceof Value val && val.tag != tag) throw thw; 
				if (hdl != ignore) return combine2(null, e, tag, hdl, thw);
				if (thw instanceof Value val) return val.value;
				throw thw instanceof Error err ? err : new Error("catch exception: " + Vm.this.toString(thw.getClass()), thw);
			}
		}
		public Object combine2(Resumption r, Env e, Object tag, Object hdl, Throwable thw) {
			Object res = r != null ? r.resume() : ctApv ? hdl : getTco(evaluate(e, hdl));
			if (res instanceof Suspension s) return s.suspend(dbg(e, this, tag, hdl), rr-> combine2(rr, e, tag, hdl, thw));
			return res instanceof Apv apv && args(apv) == 1
				? getTco(Vm.this.combine(e, unwrap(apv), list(thw instanceof Value val ? val.value : thw)))
				: hdlAny ? res : typeError("cannot apply handler, not a {expected}: {datum}", res, symbol("Apv1"))
			;
		}
		public String toString() { return "%CatchTagWth"; }
	}
	class ThrowTag implements Combinable {
		public Object combine(Env e, List o) {
			var chk = checkR(this, o, 1, 2); // o = (tag) | (tag value)
			if (chk instanceof Suspension s) return s;
			if (!(chk instanceof Integer len)) return resumeError(chk, symbol("Integer"));
			var value = len == 1 ? inert : o.car(1);
			var dbg = dbg(e, this, o);
			return pipe(dbg, ()-> ctApv ? o.car : getTco(evaluate(e, o.car)),
				tag->{ throw new Value(tag, ctApv ? value : pipe(dbg, ()-> getTco(evaluate(e, value)))); }
			);		
		}
		public String toString() { return "%ThrowTag"; }
	}
	class AtEnd implements Combinable {
		public Object combine(Env e, List o) { return combine(null, e, o); }
		public Object combine(Resumption r, Env e, List o) {
			var chk = checkM(this, o, 2); // o = (cln form . forms)
			if (chk instanceof Suspension s) return s;
			if (!(chk instanceof Integer len)) return resumeError(chk, symbol("Integer"));
			var cln = o.car;
			try {
				var res = r != null ? r.resume() : pipe(dbg(e, this, o), ()-> getTco(begin.combine(e, o.cdr())), v-> cleanup(cln, e, true, v));
				return res instanceof Suspension s ? s.suspend(dbg(e, this, o), rr-> combine(rr, e, o)) : res;
			}
			catch (Throwable thw) {
				return cleanup(cln, e, false, thw);
			}
		}
		Object cleanup(Object cln, Env e, boolean success, Object res) {
			return pipe(dbg(e, this, cln, success, res), ()-> getTco(evaluate(e, cln)), $-> {
					if (success) return res;
					throw res instanceof Value val ? val : res instanceof Error err ? err : new Error("cleanup error!", (Throwable) res);
				}
			);
		}
		public String toString() { return "%AtEnd"; }
	}
	
	
	// Delimited Control
	class TakeSubcont implements Combinable  {
		public Object combine(Env e, List o) {
			var chk = checkM(this, o, 2, Any.class, or(ignore, Symbol.class)); // o = (prp (or #ignore symbol) . forms)
			if (chk instanceof Suspension s) return s;
			if (!(chk instanceof Integer len)) return resumeError(chk, symbol("Integer"));
			return pipe(dbg(e, this, o), ()-> getTco(evaluate(e, o.car)),
				prp-> new Suspension(prp, lambda(e, list(o.<Object>car(1)), o.cdr(1)))
					.suspend(dbg(e, this, o), rr-> Vm.this.combine(e, rr.s, null)))
			;
		}
		public String toString() { return "%TakeSubcont"; }
	}
	class PushPrompt implements Combinable  {
		public Object combine(Env e, List o) {
			var chk = checkM(this, o, 1); // o = (prp . forms)
			if (chk instanceof Suspension s) return s;
			if (!(chk instanceof Integer len)) return resumeError(chk, symbol("Integer"));
			var dbg = dbg(e, this, o);
			return pipe(dbg, ()-> getTco(evaluate(e, o.car)), prp-> pushPrompt(null, e, dbg, prp, ()-> getTco(begin.combine(e, o.cdr()))));
		}
		public String toString() { return "%PushPrompt"; }
	}
	class PushDelimSubcont implements Combinable  {
		public Object combine(Env e, List o) {
			var chk = checkM(this, o, 2); // o = (prp k . forms)
			if (chk instanceof Suspension s) return s;
			if (!(chk instanceof Integer len)) return resumeError(chk, symbol("Integer"));
			var dbg = dbg(e, this, o);
			return pipe(dbg,
				()-> getTco(evaluate(e, o.car)),
				prp-> cons(prp, getTco(evaluate(e, o.car(1)))),
				a-> apply(
					c->	pushPrompt(null, e, dbg, c.car,
						()-> c.cdr() instanceof Continuation cont
						? cont.apply(e, o.<List>cdr(1))
						: typeError("cannot apply continuation, not a {expected}: {datum}", c.cdr(), Continuation.class)
					),
					(Cons) a
				)
			);
		}
		public String toString() { return "%PushDelimSubcont"; }
	}
	Object pushPrompt(Resumption r, Env e, Dbg dbg, Object prp, Supplier action) {
		var res = r != null ? r.resume() : getTco(action.get());
		if (!(res instanceof Suspension s)) return res;
		return prp == ignore || !equals(s.prp, prp)
			? s.suspend(dbg, rr-> pushPrompt(rr, e, dbg, prp, action))
			: combine(e, s.hdl, cons(s.k, null))
		;
	}
	Combinable pushSubcontBarrier = new Combinable() {
		public Object combine(Env e, List o) {
			return combine(null, e, o);
		}
		Object combine(Resumption r, Env e, List o) {
			var res = r != null ? r.resume() : getTco(begin.combine(e, o));
			if (!(res instanceof Suspension s)) return res;
			return s.suspend(dbg(e, "pushSubcontBarrier", o), rr-> combine(rr, e, o)).k.apply(()-> unboundPromptError("prompt not found: {prompt}", s.prp));
		}
		public String toString() { return "%PushSubcontBarrier"; }
	};
	
	
	// Dynamic Variables
	class DVar extends Box { DVar(Object val) { super(val); }}
	class DLambda implements Combinable {
		public Object combine(Env e, List o) { return combine(null, e, o); }
		public Object combine(Resumption r, Env e, List o) {
			var chk = checkM(this, o, 1, list(Symbol.class)); // o = ((symbol . symbols) . forms)
			if (chk instanceof Suspension s) return s;
			if (!(chk instanceof Integer)) return resumeError(chk, symbol("Integer"));
			var vars = array(o.car());
			var len = vars.length;
			var body = o.cdr();
			return new Apv(
				new Combinable() {
					public Object combine(Env de, List o) { return combine(null, de, o); }
					public Object combine(Resumption r, Env de, List o) {
						var vals = o == null ? apply(a-> { if (boxDft != null) fill(a, boxDft); return a; }, new Object[len]) : array(o);
						if (vals.length != len)	return matchError(
							"expected {operands,%+d} operand, found: {datum}"
							+ (body != null ? " bind" : " sett") + "ing: " + list(vars)
							+ (body == null ? "" : " of: " + this)
							+ " with: " + o,
							"operands", len-vals.length, "datum", len > vals.length ? null : o.cdr(vals.length-len)
						);
						var olds = new Object[len];
						var ndvs = new Object[len];
						for (int i=0; i<len; i+=1) {
							var lookup = de.lookup(vars[i]);
							if (body == null && !lookup.isBound) continue;
							// TODO in alternativa al precedente
							//if (body == null && lookup.value == null) continue;
							if ((ndvs[i] = lookup.value) instanceof DVar dvar) { olds[i] = dvar.value; continue; }
							return typeError("cannot get value, not a {expected}: {datum}", vars[i], symbol("DVar"));
						}
						for (int i=0; i<len; i+=1) {
							if (ndvs[i] instanceof DVar dvar) dvar.value = vals[i]; else de.def(vars[i], new DVar(vals[i]));
						}
						if (body == null) return switch (bndRes) { case 0-> inert; case 1-> vals[len-1]; case 2-> olds[len-1]; default-> 1; };  
						try {
							Object res = r != null ? r.resume() : getTco(begin.combine(e, body));
							return res instanceof Suspension s ? s.suspend(dbg(e, this, o), rr-> combine(rr, e, o)) : res;
						}
						finally {
							for (int i=0; i<len; i+=1) if (ndvs[i] instanceof DVar dvar) dvar.value = olds[i];
						}
					}
					@Override public String toString() { return "{%DOpv " + o.car + eIfnull(body, ()-> " " + apply(s-> s.substring(1, s.length()-1), Vm.this.toString(body))) + "}"; }
				}
			);
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
				case LenList ll-> o-> pipe(null, ()-> check.apply(name, o),
					obj-> obj instanceof Integer len ? ll.apply(len, o)	: resumeError(obj, symbol("Integer")));   
				case Supplier sp-> o-> pipe(null, ()-> checkN(name, o, 0),
					obj-> obj instanceof Integer len? sp.get() : resumeError(obj, symbol("Integer")));  
				case Function f-> o-> pipe(null, ()-> checkN(name, o, 1),
					obj-> obj instanceof Integer len ? f.apply(o.car) : resumeError(obj, symbol("Integer")));  
				case BiFunction f-> o-> pipe(null, ()-> checkN(name, o, 2),
					obj-> obj instanceof Integer len ? f.apply(o.car, o.car(1)) : resumeError(obj, symbol("Integer")));
				case Field f-> o-> pipe(null, ()-> checkR(name, o, 1, 2), obj->{
						if (!(obj instanceof Integer len)) return resumeError(obj, symbol("Integer"));
						if (len == 1) return uncked(()-> f.get(o.car));
						return uncked(()->{ f.set(o.car, o.car(1)); return inert; });
					}
				);
				case Method mt-> o->{
					var pc = mt.getParameterCount();
					return pipe(null,
						()-> !mt.isVarArgs() ? checkN(name, o, pc+1) : checkM(name, o, pc),
						obj-> obj instanceof Integer len
							? uncked(()-> mt.invoke(o.car, reorg(mt, array(o.cdr()))))
							: resumeError(obj, symbol("Integer"))
					);
				};
				case Constructor c-> o->
					pipe(null,
						()-> checkN(name, o, c.getParameterCount()),
						obj-> obj instanceof Integer len 
							? uncked(()-> c.newInstance(reorg(c, array(o))))
							: resumeError(obj, symbol("Integer"))
					);
				default -> typeError("cannot invoke, not a {expected}: {datum}", this, toChk(or(ArgsList.class, LenList.class, Supplier.class, Function.class, BiFunction.class, Field.class, Executable.class)));
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
							case Error err: throw err;
							case InnerException ie when name.equals("%CheckO"): throw ie; 
							default: return javaError("error executing: {member} with: {args}", thw, symbol(name), o, null);
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
	interface AtDot extends ArgsList {}
	interface At extends AtDot {}
	Object at(String name) {
		if (name == null) return typeError("method name is null, not a {expected}", name, symbol("String"));
		return new At() {
			@Override public Object apply(List o) {
				var chk = checkM("At", o, 1, Object.class);
				if (chk instanceof Suspension s) return s;
				if (!(chk instanceof Integer len)) return resumeError(chk, symbol("Integer"));
				Object o0 = o.car;
				Object[] args = array(o, 1);
				// (@new class . objects)            -> class.getConstructor(getClasses(objects)).newInstance(objects) -> constructor.newInstance(objects)
				// (@<name> object . objects)        -> object.getClass().getMethod(name, getClasses(objects)).invocke(object, objects) -> method.invoke(object, objects)
				// (@getConstructor class . classes) -> class.getConstructor(classes) -> constructor
				// (@getMethod class name . classes) -> class.getMethod(name, classes) -> method
				// (@getField class name)            -> class.getField(name, classes) -> field
				if (name.equals("new") && o0 instanceof Class c && isInstance(c, Obj.class, Box.class) && (args.length == 0 || args[0].getClass() != Vm.class)) {
					// (@new Error "assert error!") -> (@new Error vm "assert error!")
					args = headAdd(args, Vm.this);
				}
				var classes = getClasses(args);
				Executable executable = getExecutable(o0, name, classes);
				if (executable == null) return unboundExecutableError(Vm.this.toString(name, classes) + " not found in: {class}", symbol(name), list((Object[])classes), o0 instanceof Class cl ? cl : o0.getClass());
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
						switch (thw = ite.getTargetException()) {
							case Value v: throw v;
							case Error e: throw e;
							default: // in errore senza!
						}
					}
					return javaError("error executing " + Vm.this.toString(name, classes) + " of: {object} with: {args}", thw, symbol(name), list(args), o0);
				}
			}
			@Override public String toString() { return "@" + name; }
		};
	}
	interface Dot extends AtDot {};
	Object dot(String name) {
		if (name == null) return typeError("field name is null, not a {expected}", name, symbol("String"));
		return new Dot() {
			@Override public Object apply(List o) {
				var chk = checkR("Dot", o, 1, 2);
				if (chk instanceof Suspension s) return s;
				if (!(chk instanceof Integer len)) return resumeError(chk, symbol("Integer"));
				var o0 = o.car;
				// (.<name> object)       -> object.getclass().getField(name).get(object) -> field.get(object) 
				// (.<name> object value) -> object.getClass().getField(name).set(object,value) -> field.set(object, value) 
				Field field = getField(o0 instanceof Class ? (Class) o0 : o0.getClass(), name);
				if (field == null) return unboundFieldError("field: {field} not found in: {object}", name, o0 instanceof Class cl ? cl : o0.getClass());
				try {
					if (len == 1) return field.get(o0);
					field.set(o0, o.car(1)); return inert;
				}
				catch (Throwable thw) {
					return len==1
						? javaError("error getting field {member} of: {object}", thw, symbol(name), null, o0)
						: javaError("error setting field {member} of: {object} with: {args}", thw, symbol(name), o.cdr(), o0)
					;
				}
			}
			@Override public String toString() { return "." + name; }
		};
	}
	
	
	// Error Handling
	Object rootPrompt = new Object() { public String toString() { return "%RootPrompt"; }};
	List pushRootPrompt(List lst) { return cons(listStar(new PushPrompt(), rootPrompt, lst)); }
	<T> T error(Error err) {
		var userBreak = theEnv.lookup(symbol("userBreak")).value;
		if (userBreak == null) throw err; // TODO or new Value(ignore, err) come eventualmente la userBreak?
		return (T) pipe(dbg(theEnv, "userBreak", err), ()-> getTco(evaluate(theEnv, list(userBreak, err)))); 
	}
	<T> T error(String msg, Object ... objs) { return error(new Error(msg, objs)); }
	<T> T error(Throwable thw, Object ... objs) { return error(null, thw, objs); }
	<T> T error(String msg, Throwable thw, Object ... objs) {
		if (thw instanceof InnerException ie) {
			var error = new Error(ie.getMessage() + eIfnull(msg, ()->" " + msg), "type", symbol(ie.getClass().getSimpleName().replace("Exception", "").toLowerCase()));
			error.puts(objs);
			error.puts(ie.objs);
			return error(error);
		}
		return error(new Error(msg, thw, objs));
	}
	<T> T typeError(String msg, Object datum, Object expected) { return error(msg, "type", symbol("type"), "datum", datum, "expected", expected); }
	<T> T typeError(String msg, Object datum, Object expected, Object expr) { return error(msg, "type", symbol("type"), "datum", datum, "expected", expected, "expr", expr); }
	<T> T syntaxError(String msg, Object datum, Object expr) { return error(msg, "type", symbol("syntax"), "datum", datum, "expr", expr); }
	<T> T resumeError(Object datum, Object expected) { return error("invalid resume value, not a {expected}: {datum}", "type", symbol("resume"), "datum", datum, "expected", expected); }
	<T> T unboundSymbolError(String msg, Object name, Env env) { return error(msg, "type", symbol("unboundSymbol"), "symbol", name, "environment", env); }
	<T> T unboundFieldError(String msg, String field, Object object) { return error(msg, "type", symbol("unboundField"), "field", field, "object", object); }
	<T> T unboundExecutableError(String msg, Symbol executable, Object object) { return error(msg, "type", symbol("unboundExecutable"), "executable", executable, "class", object); }
	<T> T unboundExecutableError(String msg, Symbol executable, List classes, Object object) { return error(msg, "type", symbol("unboundExecutable"), "executable", executable, "classes", classes, "class", object); }
	<T> T unboundPromptError(String msg, Object prompt) { return error(msg, "type", symbol("unboundPrompt"), "prompt", prompt); }
	<T> T javaError(String msg, Throwable cause, Symbol member, List args, Object object) { return error(msg, cause, "type", symbol("java"), "member", member, "object", object, "args", args); }
	class Value extends RuntimeException {
		private static final long serialVersionUID = 1L;
		Object tag, value;
		Value(Object tag, Object value) {
			super(Vm.this.toString(tag) + " " + Vm.this.toString(value), value instanceof Throwable thw ? thw : null); this.tag = tag; this.value = value;
		}
	}
	
	
	// Check Definitions/Parameters Tree
	class PTree {
		private Object exp, pt, ep; // ep == null per Def ovvero %def || %set!, ep == (or #ignore Symbol) per Vau 
		private Set syms = new HashSet();
		PTree(Object exp, Object pt, Object ep) { this.exp=exp; this.pt = pt; this.ep = ep; }
		Object check() { 
			if (!((pt instanceof Symbol || (pt == null || pt == ignore) && ep != null) && syms.add(pt))) {
				if (!(pt instanceof Cons)) return typeError("invalid parameter tree, not {expected}: {datum} of: {expr}", pt, toChk(ep == null ? or(Symbol.class, Cons.class) : or(null, ignore, Symbol.class, Cons.class)), exp );
				var msg = check(pt); if (msg != null) return msg;
			}
			if (ep == null || ep == ignore) return syms.size() > 0 ? null : typeError("invalid parameter tree syntax, not one {expected} in: {datum} of: {expr}", pt, toChk(ep == null ? Symbol.class : or(null, ignore, Symbol.class)), exp);
			if (!(ep instanceof Symbol sym)) return typeError("invalid parameter tree, not {expected}: {datum} of: {expr}", ep, toChk(or(ignore, Symbol.class)), exp);
			return !syms.contains(sym) ? null : syntaxError("invalid parameter tree syntax, not a unique symbol: {datum} in: {expr}", ep, exp);
		}
		private Object check(Object p) {
			if (p == null || p == ignore) { if (ep != null) syms.add(p); return null; }
			if (p instanceof Symbol) { return syms.add(p) ? null : syntaxError("invalid parameter tree syntax, not a unique symbol: {datum} in: " + pt + " of: {expr}", p, exp); }
			if (!(p instanceof Cons c)) return null;
			if (c.car instanceof Symbol sym && member(sym.name, "%'", "quote")) return len(c) == 2 ? null : syntaxError("invalid parameter tree #' syntax: {datum} in: {expr}", c, exp);
			if (c.car == sheBang) return len(c) == 3 && c.car(2) instanceof Symbol? check(c.car(2)) : syntaxError("invalid parameter tree #! syntax: {datum} in: {expr}", c, exp);
			var msg = check(c.car); if (msg != null) return msg;
			return c.cdr() == null ? null : check(c.cdr());
		}
	}
	Object checkDt(Object exp, Object dt) { return checkPt(exp, dt, null); }
	Object checkPt(Object exp, Object pt, Object ep) { return new PTree(exp, pt, ep).check(); }
	int args(Apv apv) {
		return switch(apv.cmb) {
			case Opv opv-> opv.pt == null ? 0 : opv.pt instanceof Cons c && c.cdr() == null && (c.car == ignore || c.car instanceof Symbol) ? 1 : more;
			case JFun jFun-> jFun.jfun instanceof Supplier ? 0 : jFun.jfun instanceof Function ? 1 : more;
			default-> more;
		};
	}
	
	
	// Check Parameters Value
	class Any {}
	Object checkN(Object op, List o, int expt, Object ... chks) {
		return checkR(op, o, expt, expt, chks);
	}
	Object checkM(Object op, List o, int min, Object ... chks) {
		return checkR(op, o, min, more, chks);
	}
	Object checkR(Object op, List o, int min, int max, Object ... chks) {
		try {
			return checkL(min, max, o, chks);
		}
		catch (InnerException ie) {
			return innerError(ie, op, o, chks);	
		}
	}
	int checkL(int min, int max, List o, Object ... chks) {
		var len = chks.length == 0 ? len(o) : checkT(min, max, o, chks);
		var rst = max != more || chks.length <= min ? 0 : (len - min) % (chks.length - min); 
		if (len >= min && len <= max && rst == 0) return len;
		throw rst != 0
			? new MatchException("expected {operands#,%+d} operands at end of: {datum}", o, rst)
			: new MatchException("expected {operands#,%+d} operands in {datum}", o, len<min ? min-len : max-len)
		;
	}
	int checkT(int min, int max, List o, Object ... chks) {
		var len=chks.length;
		int i=0; for (; o != null; i+=1, o=o.cdr()) {
			if (len == 0 || i >= max) continue;
			var cksn = i < len && i < min ? chks[i] : len <= min ? Any.class : chks[min + (i-min) % (len-min)];
			if (cksn instanceof Object[] cksna && cksna[0] instanceof List) {
				var res = checkO(o, cksn);
				if (res > 0) return i+res;
			}
			else {
				checkO(o.car, cksn);
			}
		}
		return i;
	}
	public int checkO(Object o, Object chk) {
		if (chk instanceof Object[] chks) {
			for (int i=0; i<chks.length; i+=1) {
				try {
					if (chks[i] instanceof List) return checkO(o, chks[i]);
					checkO(o, chks[i]);
					return 0;
				}
				catch (Throwable thw) {
				}
			}
		}
		else if (chk instanceof List chkl) {
			if (chkl.car instanceof Object[] chks && chkl.cdr() == null) return checkO(o, chks);
			if (member(chkl.car, comparators)) {
				switch (getTco(combine(env(), chkl.car(1), list(o, chkl.car(2))))) {
					case Boolean b when b: return 0;
					case Object obj: throw new TypeException("not a {expected}: {datum}", o, toChk(chkl));
				}
			}
			if (member(chkl.car, symbol("and"))) {
				for (var chka = chkl.cdr(); chka != null; chka = chka.cdr()) {
					try {
						checkO(o, chka.car);
					}
					catch (Throwable thw) {
						throw new TypeException("not a {expected}: {datum}", o, toChk(chkl));
					}
				}
				return 0;
			}
			if (o != null && !(o instanceof List)) throw new TypeException("not a {expected}: {datum}", o, toChk(or(null, List.class)));
			var ol = (List) o;
			int min=0, max=more;
			if (chkl.car instanceof Integer mn) { min = max = mn; chkl = chkl.cdr(); }
			if (chkl != null && chkl.car instanceof Integer mx) { max = mx; chkl = chkl.cdr(); }
			return checkL(min, max, ol, array(chkl));
		}
		else if (chk instanceof Class chkc) {
			if (checkC(o, chkc)) return 0;
		}
		else if (member(o, chk))  {
			return 0;
		}
		throw new TypeException("not a {expected}: {datum}", o, toChk(chk));
	}
	public Object toChk(Object chk) {
		return chk instanceof Class cl ? symbol(cl.getSimpleName())
			: chk instanceof Integer i && i == more ? symbol("oo")
			: chk instanceof Object[] a ? cons(symbol("or"), list(stream(a).map(o-> toChk(o)).toArray()))
			: chk instanceof List l ? map(o-> toChk(o), member(l.car, comparators) ? cons(l.car, l.cdr(1)) : l )
			: chk
		;
	}
	class Apv0 extends Apv { Apv0(Combinable cmb) { super(cmb); }}
	class Apv1 extends Apv { Apv1(Combinable cmb) { super(cmb); }}
	public boolean checkC(Object obj, Class cl) {
		return cl == Any.class
			|| cl.isInstance(obj)
			|| obj instanceof Class cl2 && cl.isAssignableFrom(cl2)
			|| cl == Apv0.class && obj instanceof Apv apv && args(apv) == 0
			|| cl == Apv1.class && obj instanceof Apv apv && args(apv) == 1
			// TODO valutare
			//|| cl == DTree.class && checkDt(obj, obj) == null
			//|| cl == PTree.class && checkPt(obj, obj, obj) == null
		;
	}
	public Object check(Object op, Object o, Object chk) {
		try {
			return checkO(o, chk);
		}
		catch (InnerException ie) {
			return innerError(ie, op, o, chk);	
		}
	}
	private Object innerError(InnerException ie, Object op, Object o, Object chk) {
		return !member(op, "check", "the+")
			? error("combining {operator} with {operands}", ie, "operator", op, "operands", o)
			: ie.objs[1] != o /*&& ie.objs[3].equals(toChk(chk))*/
			? error("checking {operands} with {check}", ie, "operands", o, "check", toChk(chk))
			: error(ie)
		;
	}
	
	
	// Utilities
	List list(Object ... objs) {
		return list(true, objs);
	}
	<T> T listStar(Object ... objs) {
		return list(false, objs);
	}
	<T> T list(boolean b, Object ... objs) {
		var len = objs.length-1;
		var c = b || len < 0 ? null : objs[len];
		for (var i=len-(b?0:1); i>=0; i-=1) c = cons(objs[i], c);
		return (T) c;
	}
	//List toList(Object obj) { return obj == null ? (List) null : obj instanceof List l ? l : list(obj); } 
	Object[] array(List l) {
		return array(l, 0);
	}
	Object[] array(List l, int i) {
		return array(l, i, Object.class);
	}
	<T> T[] array(List l, Class<T> cls) {
		return (T[]) array(l, 0, cls);
	}
	<T> T[] array(List l, int i, Class<T> cls) {
		var res = new ArrayList<T>();
		for (; l != null; l = l.cdr()) if (i-- <= 0) res.add(l.car());
		return res.toArray((T[]) Array.newInstance(cls, 0));
	}
	List reverse(List l) {
		List h = null;
		for (; l != null; l = l.cdr()) h = cons(l.car, h);
		return h;
	}
	Object append(List l, Object t) {
		if (l == null) return t;
		return cons(l.car, append(l.cdr(), t));
	}
	Object listStar(List l) {
		return l == null ? null : listStarN(l);
	}
	Object listStarN(List l) {
		var cdr = l.cdr();
		return cdr == null ? l.car : cons(l.car, listStarN(cdr));
	}
	Object listMinus(List l) {
		return l == null ? null : listMinusN(l);
	}
	Object listMinusN(List l) {
		var cdr = l.cdr();
		return cdr == null ? (l.car == null ? null : l) : cons(l.car, listMinusN(cdr));
	}
	<T> T print(Object ... objs) {
		for (var obj: objs) out.print(toString(obj)); out.println();
		return (T)(objs.length == 0 ? inert : objs[objs.length - 1]);
	}
	<T> T write(Object ... objs) {
		for (var obj: objs) out.print(toString(true, obj)); out.println();
		return (T)(objs.length == 0 ? inert : objs[objs.length - 1]);
	}
	<T> T log(Object ... objs) {
		int i=0; for (var obj: objs) out.print(eIf(i++ == 0, " ") + toString(obj)); out.println();
		return (T)(objs.length == 0 ? inert : objs[0]);
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
		List exp = cons(begin, toLispList(str));
		return vmAssert.combine(theEnv,  objs instanceof Throwable ? exp : cons(exp, toLispExpr(objs))); 
	}
	Combinable vmAssert = new Combinable() {
		public Object combine(Env env, List o) {
			if (!doAsrt) return true;
			var chk = checkM(this, o, 1); // o = (x . v)
			if (!(chk instanceof Integer len)) return chk;
			return test.combine(env, cons(null, o));
		}
		public String toString() { return "%Assert"; }
	};
	Combinable test = new Combinable() {
		public Object combine(Env e, List o) {
			if (!doAsrt) return true;
			var chk = checkM(this, o, 2); // o = (name x . v)
			if (!(chk instanceof Integer len)) return chk;
			var env = env(e);
			var name = eIfnull(o.car, n-> "test "+ n + ": ");
			var exp = o.car(1);
			try {
				var val = pushSubcontBarrier.combine(env, pushRootPrompt(cons(exp)));
				switch (len) {
					case 2-> 
						print(name, exp, " should throw but is ", val);
					case 3-> {
						var expt = o.<List>cdr(1);
						if (Vm.this.equals(val, pushSubcontBarrier.combine(env, pushRootPrompt(expt)))) return true;
						print(name, exp, " should be ", o.car(2), " but is ", val);
					}
					default-> {
						var expt = (List) map(x-> pushSubcontBarrier.combine(env, pushRootPrompt(cons(x))), o.cdr(1)); 
						if (expt.car instanceof Class && matchObj(val, expt)) return true;
						print(name, exp, " should be ", expt, " but is ", val);
					}
				}
			}
			catch (Throwable thw) {
				if (len == 2) return true;
				if (prStk)
					thw.printStackTrace(out);
				else {
					var val = thw instanceof Value v ? v.value : thw;
					var expt = (List) map(x-> pushSubcontBarrier.combine(env, pushRootPrompt(cons(x))), o.cdr(1));
					if (expt.car instanceof Class && matchObj(val, expt)) return true;
					print(name, exp, " should be ", expt, " but is ", val);
				}
			}
			return false;
		}
		public String toString() { return "%Test"; }
	};
	boolean isType(Object object, Class classe) {
		return classe == null ? object == null : object != null && classe.isAssignableFrom(object.getClass());
	}
	private boolean matchObj(Object object, List expt) {
		//if (!(expt.car() instanceof Class cls && isType(object, cls))) return false;
		if (!isType(object, expt.car())) return false;
		for (var l=expt.cdr(); l != null; l = l.cdr(1)) {
			var car = l.car;
			if (car instanceof AtDot ad) {
				if (!Vm.this.equals(ad.apply(list(object)), l.car(1))) return false;
			}
			else if (object instanceof Obj obj) {
				if (!Vm.this.equals(obj.value(car), l.car(1))) return false;
			}
			else {
				return false;
			}
		}
		return true;
	}
	
	
	// Bytecode Parser
	Map<String,Intern> interns = new LinkedHashMap<>();
	<T extends Intern> T intern(String name) {
		return (T) interns.computeIfAbsent(name, n-> n.startsWith(":") && n.length() > 1 ? new Keyword(n.substring(1)) : new Symbol(n));
	}
	Object[] quotes = $(symbol("%'"), symbol("quote"));
	Object[] comparators = $(symbol("<"), symbol("<="), symbol(">="), symbol(">"));
	
	Object toLispExpr(Object o) {
		if (o instanceof String s) return switch(s) { case "#inert"-> inert; case "#ignore", "#_"-> ignore; case "#!"-> sheBang; default-> intern(intStr ? s.intern() : s); };
		if (o instanceof Object[] objs) {
			if (objs.length == 0) return null;
			if (objs.length == 2 && objs[0] != null && objs[0].equals("wat-string")) return intStr ? ((String) objs[1]).intern() : objs[1];
			return toLispList(objs);
		}
		return o;
	}
	<T extends Cons> T toLispList(Object ... objs) {
		int i = objs.length - 1;
		Object tail = null;
		if (i > 1 && objs[i-1] != null && objs[i-1].equals(".")) { tail = toLispExpr(objs[i]); i-=2; }
		for (; i>=0; i-=1) {
			var obj = objs[i];
			if (obj != null && obj.equals("."))	throw new Error(". not is the penultimate element in " + objs);
			tail = cons(toLispExpr(obj), tail);
		}
		return (T) tail;
	}
	<T extends Cons> T toLispList(String s) throws Exception {
		return (T) toLispList(toByteCode(s));
	}
	
	
	// Stringification
	public String toString() { return "Vm"; }
	String toString(Object o) { return toString(false, o); }
	String toString(boolean t, Object o) {
		return switch (o) {
			case null-> "#null"; // () in cons altrove #null
			case Boolean b-> b ? "#t" : "#f";
			case Class cl-> "&" + toSource(cl);
			case String s-> !t ? s : '"' + toSource(s) + '"';
			case Object[] a-> {
				var s = new StringBuilder();
				for (var e: a) s.append(eIf(s.isEmpty(), ", ") + toString(true, e));
				yield "[" + s.toString() + "]";
			}
			default-> o.toString();
		};
	}
	public String toStringSet(Set<Entry<String,Object>> set) {
		return set.isEmpty() ? "" : " " + set.stream().map(e-> e.getKey() + " " + toString(true, e.getValue())).collect(joining(" "));
	}
	private String toString(String name, Class[] classes) {
		return (name.equals("new") ? "constructor: " : "method: ") + name
			+ "(" + stream(classes).map(Class::getSimpleName).collect(joining(", ")) + ")"
		;
	}
	public String toKey(Object key) {
		return switch (key) {
			case Intern i-> i.name;
			case Object o-> Utility.apply(s-> s.startsWith(":") ? s.substring(1) : s, o instanceof String s ? s : toString(o));
		};
	}
	
	
	// Bootstrap
	Env vmEnv=vmEnv(), theEnv=env(vmEnv);
	Env vmEnv() {
		Env vmEnv = env();
		return (Env) vmEnv.apply(
			list(keyword(":obj"),
				// Basics
				"%begin", begin,
				"%vau", new Vau(),
				"%def", new Def(true),
				"%set!", new Def(false),
				"%eval", wrap(new Eval()),
				"%wrap", wrap(new JFun("%Wrap", (Function) this::wrap)),
				"%unwrap", wrap(new JFun("%Unwrap", (Function) this::unwrap)),
				"%apply", wrap(new JFun("%Apply", (n,o)-> checkR(n, o, 2, 3, Combinable.class, Any.class, Env.class), (l,o)-> combine(l == 2 ? env() : o.car(2), unwrap(o.car), o.car(1)) )),
				"%apply*", wrap(new JFun("%Apply*", (ArgsList) o-> combine(env(), unwrap(o.car), o.cdr()) )),
				"%apply**", wrap(new JFun("%Apply**", (ArgsList) o-> combine(env(), unwrap(o.car), (List) listStar(o.cdr())) )),
				// Env
				"%vmEnv", wrap(new JFun("%VmEnv", (n,o)-> checkN(n, o, 0), (l,o)-> vmEnv() )),
				"%newEnv", wrap(new JFun("%NewEnv", (n,o)-> check(n, o, or(null, list(2, or(null, Env.class), Obj.class), list(1, more, or(null, Env.class), or(Symbol.class, Keyword.class, String.class), Any.class))), (l,o)-> l==0 ? env() : l==2 ? env(o.car(), o.<Obj>car(1)) : env(o.car(), array(o.cdr())) )),
				"%bind", wrap(new JFun("%Bind", (n,o)-> checkN(n, o, 3, Env.class), (l,o)-> bind(true, 0, o.<Env>car(), o.car(1), o.car(2)) )),
				"%bind?", wrap(new JFun("%Bind?", (n,o)-> checkN(n, o, 3, Env.class), (l,o)-> { try { bind(true, 0, o.<Env>car(), o.car(1), o.car(2)); return true; } catch (InnerException ie) { return false; }} )),
				"%resetEnv", wrap(new JFun("%ResetEnv", (Supplier) ()-> { theEnv.map.clear(); return theEnv; } )),
				"%pushEnv", wrap(new JFun("%PushEnv", (Supplier) ()-> theEnv = env(theEnv))),
				"%popEnv", wrap(new JFun("%PopEnv", (Supplier) ()-> theEnv = theEnv.parent)),
				// Env & Obj
				"%value", wrap(new JFun("%Value", (n,o)-> checkN(n, o, 2, or(Symbol.class, Keyword.class, String.class), ObjEnv.class), (l,o)-> o.<ObjEnv>car(1).value(o.car)) ),
				"%bound?", wrap(new JFun("%Bound?", (n,o)-> checkN(n, o, 2, or(Symbol.class, Keyword.class, String.class), ObjEnv.class), (l,o)-> o.<ObjEnv>car(1).isBound(o.car)) ),
				"%remove!", wrap(new JFun("%remove!", (n,o)-> checkN(n, o, 2, or(Symbol.class, Keyword.class, String.class), ObjEnv.class), (l,o)-> o.<ObjEnv>car(1).remove(o.car)) ),				
				// Values
				"%car", wrap(new JFun("%Car", (n,o)-> checkR(n, o, 1, 2, Cons.class, Integer.class), (l,o)-> apply(c-> l == 1 ? c.car() : c.car(o.<Integer>car(1)), o.<Cons>car()) )),
				"%cdr", wrap(new JFun("%Car", (n,o)-> checkR(n, o, 1, 2, Cons.class, Integer.class), (l,o)-> apply(c-> l == 1 ? c.cdr() : c.cdr(o.<Integer>car(1)), o.<Cons>car()) )),
				"%cadr", wrap(new JFun("%Cadr", (n,o)-> checkN(n, o, 1, Cons.class), (l,o)-> o.<Cons>car().car(1) )),
				"%cddr", wrap(new JFun("%Cddr", (n,o)-> checkN(n, o, 1, Cons.class), (l,o)-> o.<Cons>car().cdr(1) )),
				"%cons", wrap(new JFun("%Cons", (n,o)-> checkR(n, o, 1, 2), (l,o)-> cons(o.car, l == 1 ? null : o.car(1)) )),
				"%null?", wrap(new JFun("%Null?", (Function<Object, Boolean>) obj-> obj == null)),
				"%!null?", wrap(new JFun("%!Null?", (Function<Object, Boolean>) obj-> obj != null)),
				"%cons?", wrap(new JFun("%Cons?", (Function<Object, Boolean>) obj-> obj instanceof Cons)),
				"%list?", wrap(new JFun("%List?", (Function<Object, Boolean>) obj-> obj instanceof List)),
				"%symbol", wrap(new JFun("%Symbol", (n,o)-> checkN(n, o, 1, String.class), (l,o)-> symbol(o.car()) )),
				"%symbol?", wrap(new JFun("%Symbol?", (Function<Object, Boolean>) obj-> obj instanceof Symbol )),
				"%keyword", wrap(new JFun("%Keyword", (n,o)-> checkN(n, o, 1, String.class), (l,o)-> keyword(o.car()) )),
				"%keyword?", wrap(new JFun("%Keyword?", (Function<Object, Boolean>) obj-> obj instanceof Keyword )),
				"%intern", wrap(new JFun("%Intern", (n,o)-> checkN(n, o, 1, String.class), (l,o)-> intern(o.car()) )),
				"%intern?", wrap(new JFun("%Intern?", (Function<Object, Boolean>) obj-> obj instanceof Intern )),
				"%internName", wrap(new JFun("%InternName", (n,o)-> checkN(n, o, 1, Intern.class), (l,o)-> o.<Intern>car().name )),
				// First-Order Control
				"%if", new If(),
				"%if*", new IfStar(),
				"%loop", new Loop(),
				"%atEnd", new AtEnd(),
				"%throwTag", apply(t-> !ctApv ? t : wrap(t), new ThrowTag()),
				"%catchTagWth", apply(c-> !ctApv ? c : wrap(c), new CatchTagWth()),
				// Delimited Control
				"%takeSubcont", new TakeSubcont(),
				"%pushPrompt", new PushPrompt(),
				"%pushDelimSubcont", new PushDelimSubcont(),
				"%pushSubcontBarrier", pushSubcontBarrier,
				// Box
				"%newBox", wrap(new JFun("%NewBox", (n,o)-> checkR(n, o, 0, 1), (l,o)-> new Box(l == 0 ? boxDft : o.car))),
				// Dynamically-Scoped Variables
				"%newDVar", wrap(new JFun("%NewDVar", (n,o)-> checkR(n, o, 0, 1), (l,o)-> new DVar(l == 0 ? boxDft : o.car))),
				"%dVal", wrap(new JFun("%DVal", (n,o)-> checkR(n, o, 1, 2, DVar.class), (l,o)-> apply(dv-> l == 1 ? dv.value : (dv.value=o.car(1)), o.<DVar>car()) )),
				"%d\\", new DLambda(),
				// Errors
				"%rootPrompt", rootPrompt,
				"%error", wrap(new JFun("%Error", (ArgsList) o-> ((ArgsList) at("error")).apply(cons(this, o)))),
				// Java Interface
				"%jFun?", wrap(new JFun("%JFun?", (Function<Object,Boolean>) this::isjFun)),
				"%at", wrap(new JFun("%At", (Function<String,Object>) this::at)),
				"%dot", wrap(new JFun("%Dot", (Function<String,Object>) this::dot)),
				"%instanceOf?", wrap(new JFun("%InstanceOf?", (n,o)-> checkN(n, o, 2, Any.class, Class.class), (l,o)-> o.<Class>car(1).isInstance(o.car) )),
				// Object System
				"%new", wrap(new JFun("%New",
					(n,o)-> checkM(n, o, 1,
						or( list(2, Box.class),
							list(1, more, Obj.class,
								or( list(or(Symbol.class, Keyword.class, String.class), Any.class),
									list(1, more, Throwable.class, or(Symbol.class, Keyword.class, String.class), Any.class),
									list(1, more, String.class,
										or(	list(or(Symbol.class, Keyword.class, String.class), Any.class),
											list(1, more, Throwable.class, or(Symbol.class, Keyword.class, String.class), Any.class) )))))),
					(l,o)-> ((ArgsList) at("new")).apply(listStar(o.car, Vm.this, o.cdr())) )),
				"%newClass", wrap(new JFun("%NewClass", (n,o)-> checkR(n, o, 1, 2, Symbol.class, or(Box.class, Obj.class)), (l,o)-> newClass(o.car(), apply(cdr-> cdr == null ? null : cdr.car(), o.cdr())) )),
				"%subClass?", wrap(new JFun("%SubClass?", (n,o)-> checkN(n, o, 2, Class.class, Class.class), (l,o)-> o.<Class>car(1).isAssignableFrom(o.car()) )),
				"%type?",  wrap(new JFun("%Type?", (n,o)-> checkN(n, o, 2, Any.class, or(null, Class.class)), (l,o)-> isType(o.car, o.car(1)) )),
				"%classOf", wrap(new JFun("%ClassOf", (n,o)-> checkN(n, o, 1), (l,o)-> apply(o1-> o1 == null ? null : o1.getClass(), o.car) )),
				"%addMethod", wrap(new JFun("%AddMethod", (n,o)-> checkN(n, o, 3, or(null, Class.class), Symbol.class, Apv.class), (l,o)-> addMethod(o.car(), o.car(1), o.car(2)) )),
				"%getMethod", wrap(new JFun("%GetMethod", (n,o)-> checkN(n, o, 2, or(null, Class.class), Symbol.class), (l,o)-> getMethod(o.car(), o.car(1)) )),
				"%the", wrap(new JFun("%The", (n,o)-> checkN(n, o, 2, Class.class), (l,o)-> o.<Class>car().isInstance(o.car(1)) ? o.car(1) : typeError("not a {expected}: {datum}", o.car(1), symbol(o.<Class>car().getSimpleName())) )),
				"%the+", wrap(new JFun("%The+", (n,o)-> checkN(n, o, 2), (l,o)-> check("the+", o.car(1), o.car) )),
				"%check", wrap(new JFun("%Check", (n,o)-> checkN(n, o, 2), (l,o)-> check("check", o.car, o.car(1)) )),
				"%checkO", wrap(new JFun("%CheckO", (n,o)-> checkN(n, o, 2), (l,o)-> checkO(o.car, o.car(1)) )),
				"%matchObj?", wrap(new JFun("%MatchObj?", (n,o)-> checkN(n, o, 2, Any.class, list(1, more, Class.class, or(At.class, Dot.class, Symbol.class, Keyword.class, String.class), Any.class)), (l,o)-> matchObj(o.car, o.car(1)) )),
				// List & Array
				"%list", wrap(new JFun("%List", (ArgsList) o-> o)),
				"%list*", wrap(new JFun("%List*", (ArgsList) this::listStar)),
				"%list-", wrap(new JFun("%List-", (ArgsList) this::listMinus)),
				"%len", wrap(new JFun("%Len", (n,o)-> checkM(n, o, 1, or(null, List.class)), (l,o)-> len(o.car()) )),
				"%list->array", wrap(new JFun("%List->Array", (n,o)-> checkM(n, o, 1, List.class), (l,o)-> array(o.car()) )),
				"%array->list", wrap(new JFun("%Array->List", (n,o)-> checkM(n, o, 2, Boolean.class, Object[].class), (l,o)-> list(o.<Boolean>car(), o.<Object[]>car(1)) )),
				"%reverse", wrap(new JFun("%Reverse", (n,o)-> checkM(n, o, 1, or(null, List.class)), (l,o)-> reverse(o.car()) )),
				"%append", wrap(new JFun("%Append", (n,o)-> checkM(n, o, 2, or(null, List.class)), (l,o)-> append(o.car(),o.car(1)) )),
				// String
				"%string?", wrap(new JFun("%String?", (Function<Object, Boolean>) obj-> obj instanceof String )),
				"%$", wrap(new JFun("%$", (BiFunction<Object,Object,String>) (a,b)-> Vm.this.toString(a) + Vm.this.toString(b))),
				// Math
				"%number?", wrap(new JFun("%Number?", (Function<Object, Boolean>) obj-> obj instanceof Number )),
				"%+", wrap(new JFun("%+", (n,o)-> checkN(n, o, 2, Number.class, Number.class), (l,o)-> binOp(Pls, o.car(), o.car(1)) )),
				"%*", wrap(new JFun("%*", (n,o)-> checkN(n, o, 2, Number.class, Number.class), (l,o)-> binOp(Pwr, o.car(), o.car(1)) )),
				"%-", wrap(new JFun("%-", (n,o)-> checkN(n, o, 2, Number.class, Number.class), (l,o)-> binOp(Mns, o.car(), o.car(1)) )),
				"%/", wrap(new JFun("%/", (n,o)-> checkN(n, o, 2, Number.class, Number.class), (l,o)-> binOp(Dvd, o.car(), o.car(1)) )),
				"%%", wrap(new JFun("%%", (n,o)-> checkN(n, o, 2, Number.class, Number.class), (l,o)-> binOp(Rst, o.car(), o.car(1)) )),
				// Not
				"%!", wrap(new JFun("%!", (Function) a-> switch (istrue(a)) { case Suspension s-> s; case Boolean b-> !b; case Object obj-> typeError("not a {expected}: {datum}", obj, symbol("Boolean")); } )),
				"%!!", wrap(new JFun("%!!", (Function) a-> switch (istrue(a)) { case Suspension s-> s; case Boolean b-> b; case Object obj-> typeError("not a {expected}: {datum}", obj, symbol("Boolean")); } )),
				// Comparator
				"%<", wrap(new JFun("%<", (n,o)-> checkN(n, o, 2, Comparable.class, Comparable.class), (l,o)-> o.<Comparable>car().compareTo(o.car(1)) < 0 )),
				"%>", wrap(new JFun("%>", (n,o)-> checkN(n, o, 2, Comparable.class, Comparable.class), (l,o)-> o.<Comparable>car().compareTo(o.car(1)) > 0 )),
				"%<=", wrap(new JFun("%<=", (n,o)-> checkN(n, o, 2, Comparable.class, Comparable.class), (l,o)-> o.<Comparable>car().compareTo(o.car(1)) <= 0 )),
				"%>=", wrap(new JFun("%>=", (n,o)-> checkN(n, o, 2, Comparable.class, Comparable.class), (l,o)-> o.<Comparable>car().compareTo(o.car(1)) >= 0 )),
				// Bit
				"%~", wrap(new JFun("%~", (n,o)-> checkN(n, o, 1, Integer.class), (l,o)-> ~o.<Integer>car() )),
				"%&", wrap(new JFun("%&", (n,o)-> checkN(n, o, 2, Number.class, Number.class), (l,o)-> binOp(And, o.car(), o.car(1)) )),
				"%|", wrap(new JFun("%|", (n,o)-> checkN(n, o, 2, Number.class, Number.class), (l,o)-> binOp(Or, o.car(), o.car(1)) )),
				"%^", wrap(new JFun("%^", (n,o)-> checkN(n, o, 2, Number.class, Number.class), (l,o)-> binOp(Xor, o.car(), o.car(1)) )),
				"%<<", wrap(new JFun("%<<", (n,o)-> checkN(n, o, 2, Number.class, Number.class), (l,o)-> binOp(Sl, o.car(), o.car(1)) )),
				"%>>", wrap(new JFun("%>>", (n,o)-> checkN(n, o, 2, Number.class, Number.class), (l,o)-> binOp(Sr, o.car(), o.car(1)) )),
				"%>>>", wrap(new JFun("%>>>", (n,o)-> checkN(n, o, 2, Number.class, Number.class), (l,o)-> binOp(Sr0, o.car(), o.car(1)) )),
				// Equals
				"%==", wrap(new JFun("%==", (BiFunction<Object,Object,Boolean>) (a,b)-> a instanceof Number ? a.equals(b) : a == b )),
				"%!=", wrap(new JFun("%!=", (BiFunction<Object,Object,Boolean>) (a,b)-> a instanceof Number ? !a.equals(b) : a != b )),
				"%eq?", wrap(new JFun("%Eq?", (BiFunction<Object,Object,Boolean>) Vm.this::equals )),
				// Derivated
				"%theEnv", opv(vmEnv, null, symbol("env"), list(symbol("env"))),
				"%'", opv(vmEnv, list(symbol("arg")), ignore, list(symbol("arg"))),
				"%\\", opv(vmEnv,
					cons(symbol("formals"), symbol("body")),
					symbol("env"),
					uncked(()-> toLispList("(%wrap (%eval (%list* %vau formals #ignore body) env))")) ),
				// Extra
				"vm", this,
				"%test", test,
				"%assert", vmAssert,
				"toString", wrap(new JFun("ToString", (Function<Object,String>) Vm.this::toString )),
				// Input/Output
				"log", wrap(new JFun("Log", (ArgsList) o-> log(array(o)) )),
				"print", wrap(new JFun("Print", (ArgsList) o-> print(array(o)) )),
				"write", wrap(new JFun("Write", (ArgsList) o-> write(array(o)) )),
				"load", wrap(new JFun("Load", (Function<String, Object>) nf-> uncked(()-> loadText(nf)) )),
				"read", wrap(new JFun("Read", (n,o)-> checkR(n, o, 0, 1, Integer.class), (l,o)-> cons(begin, uncked(()-> toLispList(read(l == 0 ? 0 : o.<Integer>car())))) )),
				// Config
				"doTco", wrap(new JFun("DoTco", (n,o)-> checkR(n, o, 0, 1, Boolean.class), (l,o)-> l == 0 ? doTco : inert(doTco=o.car()) )),
				"doAsrt", wrap(new JFun("DoAsrt", (n,o)-> checkR(n, o, 0, 1, Boolean.class), (l,o)-> l == 0 ? doAsrt : inert(doAsrt=o.car()) )),
				"ctApv", wrap(new JFun("CtApv",
					(n,o)-> checkR(n, o, 0, 1, Boolean.class),
					(l,o)-> l == 0 ? ctApv
						: inert(ctApv = o.car(),
							bind(null, theEnv,
								list(symbol("%catch"), symbol("%throw")),
								list((Object) apply(c-> !ctApv ? c : wrap(c), new CatchTagWth()),
									(Object) apply(t-> !ctApv ? t : wrap(t), new ThrowTag())))) )),
				"prTrc", wrap(new JFun("PrTrc", (n,o)-> checkR(n, o, 0, 1, or(0, 1, 2, 3, 4, 5, 6)), (l,o)-> l == 0 ? prTrc : inert(start=level-(doTco ? 0 : 3), prTrc=o.car()) )),
				"typeT", wrap(new JFun("TypeT", (n,o)-> checkR(n, o, 0, 1, or(0, 1, 2, 3, 4)), (l,o)-> l == 0 ? typeT : inert(typeT=o.car()) )),
				"bndRes", wrap(new JFun("BndRes",
					(n,o)-> checkR(n, o, 0, 1, or(inert, keyword("rhs"), keyword("prv"))),
					(l,o)-> l == 0
						? switch (bndRes) {case 1-> keyword("rhs"); case 2-> keyword("prv"); default-> inert; }
						: inert(bndRes=(int) bndRes(o.car, false)) )),
				"prEnv", wrap(new JFun("PrEnv", (n,o)-> checkR(n, o, 0, 1, Integer.class), (l,o)-> l == 0 ? prEnv : inert(prEnv=o.car()) )),
				"else1", wrap(new JFun("Else1", (n,o)-> checkR(n, o, 0, 1, Boolean.class), (l,o)-> l == 0 ? else1 : inert(else1=o.car()) )),
				"boxDft", wrap(new JFun("BoxDft", (n,o)-> checkR(n, o, 0, 1), (l,o)-> l == 0 ? boxDft : inert(boxDft=o.car) )),
				"aQuote", wrap(new JFun("AQuote", (n,o)-> checkR(n, o, 0, 1, Boolean.class), (l,o)-> l == 0 ? aQuote : inert(aQuote=o.car()) )),
				"prStk", wrap(new JFun("PrStk", (n,o)-> checkR(n, o, 0, 1, Boolean.class), (l,o)-> l == 0 ? prStk : inert(prStk=o.car()) )),
				"prWrn", wrap(new JFun("PrWrn", (n,o)-> checkR(n, o, 0, 1, Boolean.class), (l,o)-> l == 0 ? prWrn : inert(prWrn=o.car()) )),
				"prMap", wrap(new JFun("PrMap", (n,o)-> checkR(n, o, 0, 1, Boolean.class), (l,o)-> l == 0 ? prMap : inert(prMap=o.car()) )),
				"hdlAny", wrap(new JFun("HdlAny", (n,o)-> checkR(n, o, 0, 1, Boolean.class), (l,o)-> l == 0 ? hdlAny : inert(hdlAny=o.car()) ))
			)
		);
	}
	
	
	// API
	public Object exec(Object bytecode) {
		return pushSubcontBarrier.combine(theEnv, pushRootPrompt(cons(cons(new Begin(true), toLispExpr(bytecode)))));
	}
	public Object call(String funName, Object ... args) {
		return exec($(funName, ".", $(args)));
	}
	public Object get(String varName) {
		return exec(symbol(varName));
	}
	public Object eval(String exp) throws Exception {
		return exec(toByteCode(exp));
	}
	public void compile(String fileName) throws Exception {
		try (var oos = new ObjectOutputStream(new FileOutputStream("build/" + fileName))) {
			oos.writeObject(toByteCode(readText(fileName)));
		}
	}
	public String readText(String fileName) throws IOException {
		return Files.readString(Paths.get(fileName), Charset.forName("UTF-8"));
	}
	public Object readList(String fileName) throws Exception {
		return toLispList(toByteCode(readText(fileName)));
	}
	public Object readBytecode(String fileName) throws Exception {
		try (var ois = new ObjectInputStream(new FileInputStream("build/" + fileName))) {
			return ois.readObject();
		}
	}
	public Object loadText(String fileName) throws Exception {
		if (prTrc >= 1) print("\n--------: " + fileName);
		var v = eval(readText(fileName));
		if (prTrc > 1) print("--------: " + fileName + " end");
		return v;
	}
	public Object loadBytecode(String fileName) throws Exception {
		if (prTrc >= 1) print("\n--------: " + fileName);
		var v = exec(readBytecode(fileName));
		if (prTrc > 1) print("--------: "  + fileName + " end");
		return v;
	}
	public void repl() throws Exception {
		loop: for (;;) {
			switch (read()) {
				case "":
					break loop;
				case String exp: try {
					print(exec(toByteCode(exp)));
				}
				catch (Throwable thw) {
					if (prStk)
						thw.printStackTrace(out);
					/* TODO sostituito dal seguente, eliminare
					else {
						print(thw instanceof Value v ? ifnull(v.getCause(), thw) : thw);
					}
					*/
					else if (thw instanceof Value v)  {
						if (v.tag != ignore) print("uncatched throw tag: " + v.tag);
						print(ifnull(v.getCause(), thw));
					}
					else {
						print(thw);
					}
				}
			}
		}
		print("finito");
	}
	private void print(Throwable thw) {
		do out.println(
			thw instanceof ParseException pe
			? "{&" + Utility.getMessage(pe) + "}"
			: thw instanceof Obj o 
			?  ifnull(o.getMessage(), "&" + o.getClass().getCanonicalName()) + (prMap ? toStringSet(o.map.entrySet()) : "")
			: "&" + thw.getClass().getCanonicalName() + eIfnull(thw.getMessage(), msg-> ": " + msg)
		);
		while ((thw = thw.getCause()) != null);
	}
	
	
	// Test
	public static void main(String[] args) throws Exception {
		//out.println(Charset.defaultCharset());
		//out.println(getProperty("stdout.encoding"));
		new Vm().main("boot.lsp");
	}
	public void main(String file) throws Exception {
		if (new File(file).exists()) {
			var milli = currentTimeMillis();
			var res = loadText(file);
			print("start time: " + (currentTimeMillis() - milli));
			if (res != ignore) print(res);
		}
		repl();
	}
}
