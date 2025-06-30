package Wat;

import static List.Parser.str2bc;
import static List.Parser.string;
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
import static Wat.Utility.system;
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
import static Wat.Utility.PrimitiveWrapper.isWrapper;
import static java.lang.Character.isISOControl;
import static java.lang.Integer.toHexString;
import static java.lang.Runtime.version;
import static java.lang.System.currentTimeMillis;
import static java.lang.System.out;
import static java.lang.reflect.Array.getLength;
import static java.util.Arrays.copyOfRange;
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
import java.util.function.BiConsumer;
import java.util.function.BiFunction;
import java.util.function.Consumer;
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
	val, v: value
	res: result
	thw: throwable
	prp: prompt
	dft: default
 */

public class Vm {
	
	static {
		for (File file: new File("bin/Ext").listFiles()) file.delete();
		//new File("bin/Ext").delete();
	}
	
	boolean doTco = true; // do tco
	boolean doAsrt = true; // do assert
	boolean intStr = false; // intern string
	boolean prStk = false; // print stack
	boolean prWrn = false; // print warning
	boolean prAttr = false; // print error attribute
	boolean prInert = false; // print inert
	boolean aQuote = true; // auto quote list
	boolean hdlAny = true; // any value for catch handler
	boolean bndSmt = false; // bind simil match type
	boolean thwErr = true; // error throw err (or val returned from userBreak)
	boolean setErr = true; // error if set don't find the bind
	
	Boolean pstki = null; // push/take prompt #ignore: null:s.prp==prp, true:prp==#ignore; false:s.prp==#ignore
	Object boxDft = null; // box/dinamic default: null, inert, ...
	
	int prTrc = 0; // print trace: 0:none, 1:load, 2:eval root, 3:eval all, 4:return, 5:combine, 6:bind/lookup
	int typeT = 0; // type true: 0:true, 1:!false, 2:!(or false null), 3:!(or false null inert), 4:!(or false null inert zero)
	int prEnv = 10; // print environment
	int bndRes = 0; // bind result: 0:inert 1:rhs 2:prv 3:cnt
	private Object bndRes(Object o) {
		switch (o) {
			case Inert _: return 0;
			case Ignore _: return bndRes;
			case Keyword k: switch (k.name) {
				case "rhs": return 1;
				case "prv": return 2;
				case "cnt": return 3;
			};
			default: return typeError("cannot determine bndRes, not {expected}: {datum}", o,
				toChk(or(inert, ignore, keyword(":rhs"), keyword(":prv"), keyword(":cnt")))
			);
		}
	}
	
	
	// Continuations
	class Continuation {
		Dbg dbg; Function<Resumption, Object> f; Continuation nxt;
		Continuation(Dbg dbg, Function<Resumption, Object> f, Continuation next) {
			this.dbg = dbg; this.f = f; this.nxt = next;
		}
		public String toString() { return "{Continuation %s}".formatted(dbg); }
		Object apply(Supplier s) { return f.apply(new Resumption(nxt, s)); }
		int length() { int i=0; for (Continuation c=this; c != null; i+=1, c=c.nxt); return i; }
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
		Suspension suspend(Dbg dbg, Function<Resumption, Object> f) { k = new Continuation(dbg, f, k); return this; }
	}
	
	/*TODO non più necessario eliminare
	Object pipe(Dbg dbg, Supplier before, Function ... after) {
		return pipe(null, dbg, before, after);
	}
	Object pipe(Resumption r, Dbg dbg, Supplier before, Function ... after) {
		var res = r != null ? r.resume() : before.get();
		return res instanceof Suspension s ? s.suspend(dbg, rr-> pipe(rr, dbg, before, after)) : pipe(null, dbg, res, 0, after);
	}
	Object pipe(Resumption r, Dbg dbg, Object res, int i, Function ... after) {
		for (var first=true; i<after.length; i+=1) { // only one resume for suspension
			res = first && r != null && !(first = false) ? r.resume() : after[i].apply(res);
			if (!(res instanceof Suspension s)) continue;
			int ii=i; var rres=res;
			return s.suspend(dbg, rr-> pipe(rr, dbg, rres, ii, after));
		}
		return res;
	}
	*/
	
	Object pipe(Dbg dbg, Supplier ... supplier) {
		return pipe(null, dbg, 0, supplier);
	}
	Object pipe(Resumption r, Dbg dbg, int i, Supplier ... supplier) {
		Object res = null;
		for (var first=true; i<supplier.length; i+=1) { // only one resume for suspension
			res = first && r != null && !(first = false) ? r.resume() : supplier[i].get();
			if (!(res instanceof Suspension s)) continue;
			int ii=i; return s.suspend(dbg, rr-> pipe(rr, dbg, ii, supplier));
		}
		return res;
	}
	
	Object pipe(Dbg dbg, Supplier before, Function after) {
		return pipe(null, dbg, before, after);
	}
	Object pipe(Resumption r, Dbg dbg, Supplier before, Function after) {
		var res = r != null ? r.resume() : before.get();
		return res instanceof Suspension s ? s.suspend(dbg, rr-> pipe(rr, dbg, before, after)) : pipe(null, dbg, res, after);
	}
	Object pipe(Resumption r, Dbg dbg, Object res0, Function after) {
		var res1 = r != null ? r.resume() : after.apply(res0);
		return res1 instanceof Suspension s ? s.suspend(dbg, rr-> pipe(rr, dbg, res0, after)) : res1;
	}
	
	Object pipe(Dbg dbg, Supplier before, Function after, BiFunction atend) {
		return pipe(null, dbg, before, after, atend);
	}
	Object pipe(Resumption r, Dbg dbg, Supplier before, Function after, BiFunction atend) {
		var res0 = r != null ? r.resume() : before.get();
		return res0 instanceof Suspension s ? s.suspend(dbg, rr-> pipe(rr, dbg, before, after, atend)) : pipe(null, dbg, res0, after, atend);
	}
	Object pipe(Resumption r, Dbg dbg, Object res0, Function after, BiFunction atend) {
		var res1 = r != null ? r.resume() : after.apply(res0);
		return res1 instanceof Suspension s ? s.suspend(dbg, rr-> pipe(rr, dbg, res0, after, atend)) : pipe(null, dbg, res0, res1, atend);
	}
	Object pipe(Resumption r, Dbg dbg, Object res0, Object res1, BiFunction atend) {
		var res2 = r != null ? r.resume() : atend.apply(res0, res1);
		return res2 instanceof Suspension s ? s.suspend(dbg, rr-> pipe(rr, dbg, res0, res1, atend)) : res2;
	}
	
	Object map(Object op, Function f, List todo) {
		return map(op, f, -1, todo);
	}
	Object map(Object op, Function f, int i, List todo) {
		return map(null, op, f, i, todo, null);
	}
	Object map(Resumption r, Object op, Function f, int i, List todo, List done) {
		Object res = null;
		for (var first=true;; i-=1, todo=todo.cdr(), done=cons(res, done)) { // only one resume for suspension
			if (i == 0 || todo == null) return reverse(done, todo);
			res = first && r != null && !(first = false) ? r.resume() : f.apply(todo.car);
			if (!(res instanceof Suspension s)) continue;
			int ii=i; List td=todo, dn=done;
			return s.suspend(dbg(null, op, todo.car), rr-> map(rr, op, f, ii, td, dn));
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
	Dbg dbg(Env e, Object op, Object ... os) { return new Dbg(e, op, os); }
	
	
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
		Object val; try {
			level += 1;
			val = switch (o) {
				case Symbol s-> tco(()-> e.get(s));
				case List l-> tco(()-> pipe(dbg(e, o), ()-> getTco(evaluate(e, l.car)), op-> tco(()-> combine(e, op, l.cdr()))));
				case null, default-> o;
			};
		}
		finally {
			level -= 1;
		}
		if (prTrc >= 4) print("  return: ", indent(), val=getTco(val));
		return (T) val;
	}
	
	
	// Basic Object
	class Inert { public String toString() { return "#inert"; }};
	public Inert inert = new Inert();
	Inert inert(Object ... values) { return inert; }
	
	class Ignore { public String toString() { return "#ignore"; }};
	public Ignore ignore = new Ignore();
	
	class SharpColon { public String toString() { return "#:"; }};
	public SharpColon sharpColon = new SharpColon();
	
	class Intern implements Comparable<Intern> {
		String name;
		Intern(String name) { this.name = name; }
		public String toString() { return getClass() == Keyword.class ? ":" + name : name; }
		public int hashCode() { return Objects.hashCode(name); }
		public boolean equals(Object o) {
			return this == o || getClass().isInstance(o) && name.equals(((Intern) o).name);
		}
		public int compareTo(Intern o) {
			if (getClass().isInstance(o)) return name.compareTo(((Intern) o).name);
			throw new ClassCastException("class &" + o.getClass().getName()  + " cannot be cast to class &" + getClass().getName());
		}
	}
	Map<String, Intern> interns = new LinkedHashMap<>();
	<T extends Intern> T intern(String name) {
		return (T) interns.computeIfAbsent(name, n-> n.startsWith(":") && n.length() > 1 ? new Keyword(n.substring(1)) : new Symbol(n));
	}
	class Keyword extends Intern { Keyword(String name) { super(name); }}
	Keyword keyword(String name) { return intern(name.startsWith(":") && name.length() > 1 ? name : ":" + name); }
	class Symbol extends Intern { Symbol(String name) { super(name); }}
	Symbol symbol(String name) { return intern(name.startsWith(":") && name.length() > 1 ? name.substring(1) : name); }
	Object[] quotes = $(symbol("%'"), symbol("quote"));
	Object[] atdots = $(symbol("%at"), symbol("%dot"));
	
	class Cons {
		Object car; private Object cdr;
		Cons(Object car, Object cdr) { this.car = car; this.cdr = cdr; }
		public String toString() {
			if (car instanceof Symbol s && len(cdr) == 1) switch (s.name) {
				case "quote": return "'" + Vm.this.toString(car(1));
				case "%'", "%`", "%,", "%,@", "%\u00b4": return s.name.substring(1) + Vm.this.toString(car(1));
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
		public <T> T car(int i) {
			Cons o=this; int i0 = i; for (; i>0 && o.cdr() instanceof Cons c; i-=1, o=c);
			return i==0 ? o.car() : error("cannot get {a/d} of {datum}", "type", symbol("outOfBounds"), "a/d", symbol("c" + "a" + "d".repeat(i0) + "r"),  "datum", this);
		}
		public <T> T cdr(int i) {
			Cons o=this; int i0 = i; for (; i>0 && o.cdr() instanceof Cons c; i-=1, o=c);
			return i==0 ? o.cdr() : error("cannot get {a/d} of {datum}", "type", symbol("outOfBounds"), "a/d", symbol("c" + "d" + "d".repeat(i0) + "r"),  "datum", this);
		}
		Object setCar(Object car) { this.car = car; return this; }
		Object setCdr(Object cdr) { this.cdr = cdr; return this; }
		public <T> T cxr(int i0) {
			Object o=this;
			int i=i0; for (; i>1 && o instanceof Cons c; o = (i&1) == 1 ? c.car() : c.cdr(), i >>= 1);
			if (i==1) return (T) o;
			String n=""; for(i=i0; i>1; n = "da".charAt(i&1) + n, i >>= 1);
			return error("cannot get {a/d} of {datum}", "type", symbol("outOfBounds"), "a/d", symbol("c" + n + "r"), "datum", this);
		}
		public <T> T cxr(String n) {
			Object o=this; int i = n.length()-1;
			for(; i>=0 && o instanceof Cons c; i-=1) {
				//char ch = n.charAt(i); o = ch == 'a' ? c.car() : ch == 'd' ? c.cdr() : typeError("invalid ca..dr index, not {expected}: {datum}", symbol(n), symbol("[ad]+"));
				o = switch (n.charAt(i)) { case 'a'-> c.car(); case 'd'-> c.cdr(); default-> typeError("invalid a/d index, not {expected}: {datum}", symbol(n), symbol("[ad]+")); };
			}
			return i==-1 ? (T) o : error("cannot get {a/d} of {datum}", "type", symbol("outOfBounds"), "a/d", symbol("c" + n + "r"), "datum", this);
		}
	}
	public class List extends Cons {
		List(Object car, List cdr) { super(car, cdr); }
		@Override public List cdr() { return super.cdr(); }
		@Override Object setCdr(Object cdr) { return cdr == null || cdr instanceof List ? super.setCdr(cdr) : typeError("cannot set cdr, not a {expected}: {datum}", cdr, symbol("List") ); }
	}
	//public int len(List o) { int i=0; for (; o != null; i+=1, o=o.cdr()); return i; }
	public int len(Object o) { int i=0; for (; o instanceof Cons c; i+=1, o=c.cdr()); return i /*o == null ? i : i + .5*/; }
	public Object arity(Object o) { int i=0; for (; o instanceof Cons c; i+=1, o=c.cdr()); return o == null ? i : list(symbol(">="), i); }
	//public Object arity(Object o) { int i=0; for (; o instanceof Cons c; i+=1, o=c.cdr()); return list(symbol(o == null ? "==" : ">="), i); }
	<T extends Cons> T cons(Object car) { return (T) new List(car, null); }
	<T extends Cons> T cons(Object car, Object cdr) {
		return (T)(cdr == null || cdr instanceof List ? new List(car, (List) cdr) : new Cons(car, cdr));
	}
	
	
	// Environment
	interface ObjEnv {
		<T> T value(Object key);
		boolean isBound(Object key);
		<T> T get(Object key);
		Lookup lookup(Object obj);
		List keys();
		List symbols();
		List keywords();
		boolean remove(Object key);
	}
	record Lookup(boolean isBound, Object value) {}
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
		public Lookup lookup(Object obj) {
			var key = toKey(obj);
			for (var env=this; env != null; env=env.parent) {
				Object res = env.map.get(key);
				if (res != null || env.map.containsKey(key)) return new Lookup(true, res);
			}
			return new Lookup(false, null);
		};
		public <T> T value(Object obj) {  return (T) lookup(obj).value; }
		public boolean isBound(Object obj) { return lookup(obj).isBound; }
		public Object get(Object obj) {
			var lookup = lookup(obj);
			if (!lookup.isBound) return unboundSymbolError(obj, this);
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
			if (setErr) return unboundSymbolError(obj, this);
			if (prTrc >= 6) print("    bind: ", key, "=", value, " in: ", this);
			return map.put(key, value);
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
		public Set<String> keySet() {
			Set<String> keySet = new HashSet();
			keySet.addAll(map.keySet());
			for (var env=parent; env != null; env=env.parent) keySet.addAll(env.map.keySet());
			return keySet;
		}
		public List keys() { return list(keySet().toArray()); }
		public List symbols() { return list(keySet().stream().map(Vm.this::symbol).toArray()); }
		public List keywords() { return list(keySet().stream().map(Vm.this::keyword).toArray()); }
		@Override public Object apply(List o) { // () | (value) | (key value ...) | ((or #inert #ignore :rhs :prv :cnt) key value ...)
			var len = len(o);
			return switch (len) {
				case 0-> this;
				case 1-> get(o.car);
				default-> {
					BiFunction f =	o.car == keyword(":def") ? (k,v)-> def(k,v) : o.car == keyword(":set!") ? (k,v)-> set(k,v) : null;
					if (f == null) f = (BiFunction) (k,v)-> def(k,v); else { len-=1; o=o.cdr(); }
					var bndRes = len % 2 == 0 ? ignore : first(o.car, len-=1, o=o.cdr());
					for (; len>2; len-=2, o=o.cdr(1)) f.apply(toKey(o.car), o.car(1));
					var key = toKey(o.car);
					yield switch (bndRes(bndRes)) {
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
		public Box () { this.value = boxDft; }
		public Box (Object val) { this.value = val; }
		@Override public Object apply(List o) { // () | (value) | ((or #inert #ignore :rhs :prv :cnt) value)
			var chk = checkR(this, o, 0, 2);
			if (chk instanceof Suspension s) return s;
			if (!(chk instanceof Integer len)) return resumeError(chk, symbol("Integer"));
			return switch (len) {
				case 0-> value;
				default-> switch (bndRes(len == 2 ? o.car : ignore)) {
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
		public String toString() { return "{" + toSource(getClass()) + " " + Vm.this.toString(value) + "}"; }
	}
	public class Obj extends RuntimeException implements ArgsList, ObjEnv {
		private static final long serialVersionUID = 1L;
		LinkedHashMap<String,Object> map = new LinkedHashMap();
		public Obj(Env env) { map = env.map; }
		public Obj(Object ... objs) { puts(objs); }
		public Obj(String msg, Object ... objs) { super(msg); puts(objs); }
		public Obj(Throwable t, Object ... objs) { super(t); puts(objs); }
		public Obj(String msg, Throwable t, Object ... objs) { super(msg, t); puts(objs); }
		public Object value(Object key) { return map.get(toKey(key)); }
		public boolean isBound(Object key) { return map.containsKey(toKey(key)); }
		public boolean remove(Object key) { key=toKey(key); if (!map.containsKey(key)) return false; map.remove(key); return true; }
		public Set<String> keySet() { return map.keySet(); }
		public List keys() { return list(keySet().toArray()); }
		public List symbols() { return list(keySet().stream().map(Vm.this::symbol).toArray()); }
		public List keywords() { return list(keySet().stream().map(Vm.this::keyword).toArray()); }
		public Object get(Object obj) {
			var key = toKey(obj);
			var val = map.get(key);
			return val != null || map.containsKey(key) ? val : unboundSlotError(key, this);
		}
		public Lookup lookup(Object obj) {
			var key = toKey(obj);
			Object res = map.get(key);
			return new Lookup(res != null || map.containsKey(key), res);
		};
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
			return "{" + toSource(obj.getClass()) // or .getSimpleName()?
				+ eIfnull(obj.getMessage(), m-> " " + Vm.this.toString(true, m))
				//+ eIfnull(getCause(), t-> " " + symbol(t.getClass().getSimpleName()))
				+ (map.size() > 10 ? " [" + map.size() + "]" + " ..." : toStringSet(obj.map.entrySet()))
				+ "}"
			;
		}
		@Override public Object apply(List o) { // () | (value) | (key value ...) | ((or #inert #ignore :rhs :prv :cnt) key value ...)
			var len = len(o);
			if (len == 0) return this;
			if (len == 1) return get(o.car);
			var bndRes = len % 2 == 0 ? ignore : first(o.car, len-=1, o=o.cdr());
			for (; len>2; len-=2, o=o.cdr(1)) map.put(toKey(o.car), o.car(1));
			var key = toKey(o.car);
			return switch (bndRes(bndRes)) {
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
				var v = lookup(k);
				matcher.appendReplacement(sb, (v.isBound ? (i == -1 ? Vm.this.toString(true, v.value) : s.substring(i+1).formatted(v.value)) : "{"+ s +"}" ).replace("\\", "\\\\").replace("$", "\\$"));
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
			if (!className.name.matches("[A-Z][a-zA-Z_0-9]*")) return typeError("invalid class name, not {expected}: {datum}", className, bc2lst("regex", "[A-Z][a-zA-Z_0-9]*"));
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
		return methods.computeIfAbsent(cls, _-> new LinkedHashMap<>()).put(name, method);
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
	<T> T matchError(String msg, Object datum, int operands) {
		return error(msg, "type", symbol("match"), "datum", datum, "operands#", operands);
	}
	<T> T matchError(String msg, Object datum, int operands, List expr) {
		return error(msg, "type", symbol("match"), "datum", datum, "operands#", operands, "expr", expr);
	}
	<T> T matchError(String msg, Object datum, int operands, Object subex, List expr) {
		return error(msg, "type", symbol("match"), "datum", datum, "operands#", operands, "subex", subex, "expr", expr);
	}
	Object bind(Dbg dbg, Env e, Object lhs, Object rhs) {
		return bind(dbg, true, bndRes, e, lhs, rhs);
	}
	Object bind(Dbg dbg, boolean def, int bndRes, Env e, Object lhs, Object rhs) {
		return bind(null, dbg, def, bndRes, e, lhs, rhs);
	}
	Object bind(Resumption r, Dbg dbg, boolean def, int bndRes, Env e, Object lhs, Object rhs) {
		try {
			return bind(def, bndRes, e, lhs, rhs);
		}
		catch (InnerException ie) {
			return error(
				(def ? "bind" : "sett") + "ing: " + toString(lhs)
				//+ eIfnull(dbg, ()-> " of: " + (dbg.op instanceof Opv opv ? opv : cons(dbg.op, dbg.os[0])))
				+ eIfnull(dbg, ()-> " of: " + (dbg.op instanceof Opv opv ? opv : cons(dbg.op, switch(dbg.os.length) { case 0-> null; case 1-> dbg.os[0] instanceof Cons c ? c : cons(dbg.os[0]); default-> list(dbg.os); })))
				//+ eIfnull(dbg, ()-> " of: " + (dbg.op instanceof Opv opv ? opv : cons(dbg.op, switch(dbg.os.length) { case 0-> null; case 1-> switch(dbg.os[0]) { case Cons c-> c; case Object obj-> cons(obj); }; default-> list(dbg.os); })))
				+ " with: " + rhs,
				ie
			);
		}
	}
	Object bind(boolean def, int bndRes, Env e, Object lhs, Object rhs) {
		var res = bind0(def, bndRes, e, lhs, rhs);
		return switch (bndRes) { case 0-> inert; case 3-> e; default-> res; };
	}
	Object bind0(boolean def, int bndRes, Env e, Object lhs, Object rhs) {
		return switch (lhs) {
			case Ignore _-> rhs;
			case Symbol s-> { var val = def ? e.def(s, rhs) : e.set(s, rhs); yield bndRes == 2 ? val : rhs; }
			//case Symbol s-> apply(val-> bndRes == 2 ? val : rhs, def ? e.def(s, rhs) : e.set(s, rhs));
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
				else if (lc.car == sharpColon) {
					checkO(rhs, getTco(evalChk.combine(e, cons(lc.car(1)))));
					yield bind(def, bndRes, e, lc.car(2), rhs);
				}
				else if (rhs instanceof Cons rc) {
					var res = bind(def, bndRes, e, lc.car, rc.car);
					yield lc.cdr() == null && rc.cdr() == null ? res : bind(def, bndRes, e, lc.cdr(), rc.cdr());
				}
				else if (rhs != null) {
				//se if (rhs instanceof Object) {
					Object res=null, head=lc; int i=0;
					//if (bndSmt && !(rhs instanceof Number) && !(rhs instanceof String)) {
					//if (bndSmt && !(rhs instanceof Number || rhs instanceof String)) {
					//if (bndSmt && !isInstance(rhs, Number.class, String.class)) {
					//if (!bndSmt && !isWrapper(rhs.getClass()) && !(rhs instanceof String) && lc.cdr() != null) {
					if (bndSmt && lc.cdr() != null && !(isWrapper(rhs.getClass()) || rhs instanceof String)) {
						res = getTco(evaluate(e, lc.car));
						if (!(res instanceof Class cls)) throw new TypeException("expected an {expected}, found: {datum}", res, symbol("Class"));
						if (!isType(rhs, cls)) yield false;
						head = lc.cdr();
					}
					var objEnv = rhs instanceof ObjEnv oe ? oe : null; var isObjEnv = objEnv != null;
					var array = rhs instanceof Object[] oa ? oa : null; var isArray = array != null;
					for (; head instanceof Cons cons; i+=1, head=cons.cdr()) {
						var car = cons.car;
						if (car == sharpColon) break;
						if (car instanceof List lst && lst.car instanceof List lst2 && lst2.car.equals(symbol("%at")) /*&& len(lst2) == 2*/) {
							res = getTco(evaluate(e, lst2));
							if (!(res instanceof At at)) throw new TypeException("expected an {expected}, found: {datum}", res, symbol("At"));
							res = map("evalArgs", arg-> getTco(evaluate(e, arg)), lst.cdr());
							if (!(res instanceof List list)) throw new TypeException("expected a {expected}, found: {datum}", res, symbol("List"));
							res = bind(def, bndRes, e, symbol(at.name), at.apply(cons(rhs, list)));
						}
						else if (car instanceof List lst && member(lst.car, atdots) /*&& len(lst) == 2*/) {
							res = getTco(evaluate(e, lst));
							if (!(res instanceof AtDot ad)) throw new TypeException("expected an {expected}, found: {datum}", res, symbol("AtDot"));
							res = bind(def, bndRes, e, symbol(ad.name), ad.apply(cons(rhs)));
						}
						else if (isObjEnv) {
							res = bind(def, bndRes, e, car, objEnv.get(car instanceof Cons car2 && car2.car == sharpColon ? car2.car(2) : car));
						}
						else if (isArray) {
							//f (rhs.getClass().isArray()) { ... Array.get(rhs, i) ... Array.getLength(rhs) ...
							res = bind(def, bndRes, e, car, array[i]);
						}
						else {
							throw new MatchException("expected {operands#,%+d} operands, found: {datum}", rhs, len(lc));
						}
					}
					yield head == null ? res : bind(def, bndRes, e, head, isArray ? copyOfRange(array, i, array.length) : rhs);
				}
				else {
					throw new MatchException("expected {operands#,%+d} operands, found: {datum}", rhs, len(lc));
				}
			}
			default-> {
				if (equals(lhs, rhs)) yield null; // or rhs?
				throw new TypeException("expected literal: {expected}, found: {datum}", rhs, lhs);
			}
		};
	}
	
	
	// Operative & Applicative Combiners
	abstract class Combinable {
		abstract <T> T combine(Env e, List o);
		Object arity;
	}
	Object ge(int n) { return list(symbol(">="), n); }
	
	Object combine(Env e, Object op, List o) {
		if (prTrc >= 5) print(" combine: ", indent(), op, " ", o /*, "   ", e*/);
		if (op instanceof Combinable cmb) return cmb.combine(e, o);
		// per default le jFun nude sono considerate applicative
		if (isjFun(op)) return new Apv(new JFun(op)).combine(e, o);
		return aQuote ? cons(op, o) : typeError("cannot combine, not a {expected}: {datum} in: {expr}", op, symbol("Combinable"), cons(op, o));
	}
	
	class Opv extends Combinable {
		Env e; Object pt, ep; List xs;
		Opv(Env e, Object pt, Object ep, List xs) {
			this.e = e; this.pt = pt; this.ep = ep; //this.x = x
			this.xs = xs == null || xs.car != sharpColon ? xs
				: xs.cdr() != null ? cons(cons(new Colon(this), xs.cdr()))
				: matchError("invalid return value check, expected {operands#,%+d} operands, found: {datum} in: {expr}", null, 1, xs) ;
		}
		public Object combine(Env e, List o) {
			var xe = env(this.e);
			var dbg = dbg(e, this, o);
			return tco(()-> pipe(dbg, ()-> bind(dbg, xe, pt, o), ()-> bind(dbg, xe, ep, e), ()-> tco(()-> begin.combine(xe, xs))));
		}
		public String toString() {
			return "{%Opv " + ifnull(pt, "()", Vm.this::toString) + " " + Vm.this.toString(ep) + eIfnull(xs, ()-> " " + stream(array(xs)).map(Vm.this::toString).collect(joining(" "))) + /*" " + e +*/ "}";
		}
	}
	class Colon extends Combinable {
		Object op; boolean isCheck;
		Colon(Object op) { this.op = op; isCheck = op.equals("check"); arity = isCheck ? 2 : ge(2); }
		//*
		public Object combine(Env e, List o) {
			var chk = isCheck ? checkN(this, o, 2) : checkM(this, o, 2); // o = (form check) | (check form . forms)
			if (chk instanceof Suspension s) return s;
			if (!(chk instanceof Integer len) || (isCheck ? len != 2 : len < 2)) return resumeError(chk, and("Integer " + "(" + (isCheck ? "==" : ">=") + " 2)"));
			return pipe(dbg(e, this, o),
				()-> getTco(isCheck ? evaluate(e, o.car) : begin.combine(e, o.cdr())),
				(_)-> getTco(evalChk.combine(e, cons(o.car(isCheck ? 1 : 0)))),
				(val, eck)-> apply(len2-> len2 instanceof Suspension s ? s : isCheck ? len2 : val, check(op, val, eck))
			);
		}
		/*/
		public Object combine(Env e, List o) {
			var dbg = dbg(e, this, o);
			return pipe(dbg, ()-> isCheck ? checkN(this, o, 2) : checkM(this, o, 2), // o = (form check) | (check form . forms)
				chk-> {
					if (!(chk instanceof Integer len) || (isCheck ? len != 2 : len < 2)) return resumeError(chk, and("Integer " + "(" + (isCheck ? "==" : ">=") + " 2)"));
					return pipe(dbg(e, this, o),
						()-> getTco(isCheck ? evaluate(e, o.car) : begin.combine(e, o.cdr())),
						(_)-> getTco(evalChk.combine(e, cons(o.car(isCheck ? 1 : 0)))),
						(val, eck)-> pipe(dbg(e, "check", val, eck), ()-> check(op, val, eck), len2-> isCheck ? len2 : val)
					);
				}
			);
		}
		//*/
		public String toString() { return isCheck ? "%check" : "%:"; }
	};
	class Apv extends Combinable {
		Combinable cmb;
		Apv(Combinable cmb) { this.cmb = cmb; }
		public Object combine(Env e, List o) {
			return tco(()-> pipe(dbg(e, this, o), ()-> map("evalArgs", car-> getTco(evaluate(e, car)), o), args-> tco(()-> cmb.combine(e, (List) args))));
		}
		public String toString() {
			return "{%Apv " + Vm.this.toString(cmb) + "}";
		}
		Combinable unwrap() { return cmb;}
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
	Opv opv(Env e, Object pt, Object pe, List xs) { return new Opv(e, pt, pe, xs); }
	Opv opv(Env e, String s) { List lst = uncked(()-> str2lst(s)); return opv(e, lst.car(), lst.car(1), lst.cdr(1)); }
	Apv lambda(Env e, Object pt, List xs) { return new Apv(opv(e, pt, ignore, xs)); }
	Apv lambda(Env e, String s) { List lst = uncked(()-> str2lst(s)); return lambda(e, lst.car(), lst.cdr()); }
	Apv apv1(Env e, Symbol sym, List xs) { return lambda(e, cons(sym), xs); }
	Apv apv0(Env e, List xs) { return lambda(e, null, xs); }
	
	
	// Built-in Combiners
	class Vau extends Combinable {
		{ arity = ge(2); }
		public Object combine(Env e, List o) {
			var chk = checkM(this, o, 2); // o = (pt ep . forms)
			if (chk instanceof Suspension s) return s;
			if (!(chk instanceof Integer /*len*/)) return resumeError(chk, symbol("Integer"));
			var pt = o.car;
			var ep = o.car(1);
			var err = checkPt(cons(this, o), pt, ep); if (err != null) return err;
			return new Opv(e, pt, ep, o.cdr(1));
		}
		public String toString() { return "%Vau"; }
	};
	class DefSet extends Combinable {
		{ arity = list(2, 3); }
		boolean def;
		DefSet(boolean def) { this.def = def; }
		//*
		public Object combine(Env e, List o) {
			var chk = checkR(this, o, 2, 3); // o = (dt val) | (dt (or #ignore #inert :rhs :prv :cnt) val)
			if (chk instanceof Suspension s) return s;
			if (!(chk instanceof Integer len)) return resumeError(chk, symbol("Integer"));
			var dt = o.car;
			var err = checkDt(cons(this, o), dt); if (err != null) return err;
			var dbg = dbg(e, this, o);
			return switch (bndRes(len != 3 ? ignore : o.car(1))) {
				case Suspension s-> s;
				case Integer i-> i >= 0 && i <= 3
					? pipe(dbg, ()-> getTco(evaluate(e, o.car(len-1))), res-> bind(dbg, def, i, e, dt, res))
					: typeError("cannot " + (def ? "def" : "set!") + ", invalid bndRes value, not {expected}: {datum}", i, toChk(or(0, 1, 2, 3)));
				case Object obj-> resumeError(obj, symbol("Integer"));
			};
		}
		/*/
		public Object combine(Env e, List o) {
			var chk = checkR(this, o, 2, 3); // o = (dt val) | (dt (or #ignore #inert :rhs :prv :cnt) val)
			if (chk instanceof Suspension s) return s;
			var len = (int) chk;
			var dt = o.car;
			var err = checkDt(cons(this, o), dt); if (err != null) return err;
			var obj = bndRes(len != 3 ? ignore : o.car(1));
			if (obj instanceof Suspension s) return s;
			var bndRes = (int) obj;
			var dbg = dbg(e, this, o);
			return bndRes >= 0 && bndRes <= 3
				? pipe(dbg, ()-> getTco(evaluate(e, o.car(len-1))), res-> bind(dbg, def, bndRes, e, dt, res))
				: typeError("cannot " + (def ? "def" : "set!") + ", invalid bndRes value, not {expected}: {datum}", bndRes, toChk(or(0, 1, 2, 3)))
			;
		/* /
		public Object combine(Env e, List o) {
			return switch(checkR(this, o, 2, 3)) { // o = (dt val) | (dt (or #ignore #inert :rhs :prv :cnt) val)
				case Integer len-> {
					var dt = o.car;
					yield switch (checkDt(cons(this, o), dt)) {
						case null-> switch (bndRes(len != 3 ? ignore : o.car(1))) {
							case Integer bndRes-> {
								var dbg = dbg(e, this, o);
								yield bndRes >= 0 && bndRes <= 3
									? pipe(dbg, ()-> getTco(evaluate(e, o.car(len-1))), res-> bind(dbg, def, bndRes, e, dt, res))
									: typeError("cannot " + (def ? "def" : "set!") + ", invalid bndRes value, not {expected}: {datum}", bndRes, toChk(or(0, 1, 2, 3)));
							}
							case Object s-> s;
						};
						case Object err-> err;
					};
				}
				case Object s-> s;
			};
		}
		//*/
		public String toString() { return def ? "%Def" : "%Set!"; }
	};
	//*/
	class Eval extends Combinable {
		{ arity = list(1, 2); }
		public Object combine(Env e, List o) {
			var chk = checkR(this, o, 1, 2, Any.class, Env.class); // o = (x) | (x eo)
			if (chk instanceof Suspension s) return s;
			if (!(chk instanceof Integer len)) return resumeError(chk, symbol("Integer"));
			return evaluate(len == 1 ? e : o.car(1), o.car);
		}
		public String toString() { return "%Eval"; }
	}
	
	
	// First-Order Control
	class Begin extends Combinable {
		{ arity = ge(0); }
		boolean root;
		Begin() {}; Begin(boolean root) { this.root = root; }
		public Object combine(Env e, List o) {
			// o = () | (form . forms)
			return o == null ? inert : tco(()-> combine(null, e, o));
		}
		Object combine(Resumption r, Env e, List lst) {
			for (var first = true;; lst = lst.cdr()) { // only one resume for suspension
				if (prTrc >= 3 && root && r == null) print("\n--------");
				var car = lst.car;
				if (prTrc == 2 && root && r == null) print("evaluate: ", car);
				if (lst.cdr() == null) return evaluate(e, car);
				var res = first && r != null && !(first = false) ? r.resume() : getTco(evaluate(e, car));
				if (!(res instanceof Suspension s)) continue;
				var l = lst;
				return s.suspend(dbg(e, this, car), rr-> combine(rr, e, l));
			}
		}
		public String toString() { return "%Begin" + eIf(!root, "*"); }
	}
	Begin begin = new Begin();
	class If extends Combinable {
		{ arity = ge(2); }
		public Object combine(Env e, List o) {
			var chk = checkR(this, o, 2, more); // o = (test then . else)
			if (chk instanceof Suspension s) return s;
			if (!(chk instanceof Integer /*len*/)) return resumeError(chk, symbol("Integer"));
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
			/* TODO in alternativa alla precedente
			return tco(()-> pipe(dbg(e, this, o), ()-> getTco(evaluate(e, car)), test-> istrue(test),
				(_, b)-> tco((Boolean) b ? ()-> evaluate(e, o.car(1)) : ()-> combine(null, e, o.cdr(1)))
			*/
			));
		}
		public String toString() { return "%If*"; }
	}
	Object istrue(Object res) {
		return switch (typeT) {
			case 0-> res instanceof Boolean b ? b : typeError("not a {expected}: {datum}", res, symbol("Boolean")); // Kernel, Wat, Lispx
			case 1-> !member(res, false); // Scheme, Guile, Racket
			case 2-> !member(res, false, null); // Clojure
			case 3-> !member(res, false, null, inert);
			case 4-> !member(res, false, null, inert, 0); // Javascript
			default-> res instanceof Boolean b ? b : typeError("not a {expected}: {datum}", res, symbol("Boolean")); // Kernel, Wat, Lispx
		};
	}
	class Loop extends Combinable {
		{ arity = ge(1); }
		public Object combine(Env e, List o) {
			var chk = checkM(this, o, 1); // o = (form . forms)
			if (chk instanceof Suspension s) return s;
			if (!(chk instanceof Integer /*len*/)) return resumeError(chk, symbol("Integer"));
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
	class CatchTagWth extends Combinable {
		{ arity = ge(2); }
		public Object combine(Env e, List o) {
			// (catch . forms)               -> (%CatchTagWth #_   #_ . forms)
			// (catchTag tag . forms)        -> (%CatchTagWth tag  #_ . forms)
			// (catchWth hdl . forms)        -> (%CatchTagWth #_  hdl . forms)
			// (catchTagWth tag hdl . forms) -> (%CatchTagWth tag hdl . forms)
			var chk = checkM(this, o, 2);
			if (chk instanceof Suspension s) return s;
			if (!(chk instanceof Integer /*len*/)) return resumeError(chk, symbol("Integer"));
			return pipe(dbg(e, this, o), ()-> getTco(evaluate(e, o.car)), tag-> combine(null, e, tag, o.car(1), o.cdr(1)) );
		}
		private Object combine(Resumption r, Env e, Object tag, Object hdl, List xs) {
			try {
				var res = r != null ? r.resume() : getTco(begin.combine(e, xs));
				return res instanceof Suspension s ? s.suspend(dbg(e, this, tag, xs, hdl), rr-> combine(rr, e, tag, hdl, xs)) : res;
			}
			catch (Throwable thw) {
				if (tag != ignore && thw instanceof Value val && val.tag != tag) throw thw;
				if (hdl != ignore) return combine2(null, e, tag, hdl, thw);
				if (thw instanceof Value val) return val.value;
				throw thw instanceof Condition cnd ? cnd : new Error("catch exception: " + Vm.this.toString(thw.getClass()), thw);
			}
		}
		public Object combine2(Resumption r, Env e, Object tag, Object hdl, Throwable thw) {
			Object res = r != null ? r.resume() : getTco(evaluate(e, hdl));
			if (res instanceof Suspension s) return s.suspend(dbg(e, this, tag, hdl), rr-> combine2(rr, e, tag, hdl, thw));
			return res instanceof Apv apv && args(apv) == 1
				? getTco(Vm.this.combine(e, unwrap(apv), cons(thw instanceof Value val ? val.value : thw)))
				: hdlAny ? res : typeError("cannot apply handler, not a {expected}: {datum}", res, symbol("Apv1"))
			;
		}
		public String toString() { return "%CatchTagWth"; }
	}
	class ThrowTag extends Combinable {
		{ arity = ge(1); }
		public Object combine(Env e, List o) {
			return switch (checkM(this, o, 1)) { // o = (tag . forms)
				case Suspension s-> s;
				case Integer len when len >= 1-> pipe(dbg(e, this, o),
						()-> getTco(evaluate(e, o.car)),
						(_)-> getTco(begin.combine(e, o.cdr())),
						(tag, val)->{ throw new Value(tag, val); }
					);
				case Object obj-> resumeError(obj, and("Integer (>= 1)"));
			};
		}
		public String toString() { return "%ThrowTag"; }
	}
	class AtEnd extends Combinable {
		{ arity = ge(2); }
		public Object combine(Env e, List o) { return combine(null, e, o); }
		public Object combine(Resumption r, Env e, List o) {
			var chk = checkM(this, o, 2); // o = (cln form . forms)
			if (chk instanceof Suspension s) return s;
			if (!(chk instanceof Integer /*len*/)) return resumeError(chk, symbol("Integer"));
			var cln = o.car;
			try {
				var res = r != null ? r.resume() : getTco(begin.combine(e, o.cdr()));
				return res instanceof Suspension s ? s.suspend(dbg(e, this, o), rr-> combine(rr, e, o)) : cleanup(cln, e, true, res);
			}
			catch (Throwable thw) {
				return cleanup(cln, e, false, thw);
			}
		}
		Object cleanup(Object cln, Env e, boolean success, Object res) {
			return tco(()-> pipe(dbg(e, this, cln, success, res), ()-> getTco(evaluate(e, cln)), ()-> {
					if (success) return res;
					throw res instanceof Value val ? val
						: res instanceof Condition cnd ? cnd
						: new Error("cleanup error!", (Throwable) res)
					;
				}
			));
		}
		public String toString() { return "%AtEnd"; }
	}
	
	
	// Delimited Control
	class PushPrompt extends Combinable {
		{ arity = ge(1); }
		public Object combine(Env e, List o) {
			var chk = checkM(this, o, 1); // o = (prp . forms)
			if (chk instanceof Suspension s) return s;
			if (!(chk instanceof Integer /*len*/)) return resumeError(chk, symbol("Integer"));
			var dbg = dbg(e, this, o);
			return pipe(dbg, ()-> getTco(evaluate(e, o.car)), prp-> pushPrompt(null, e, dbg, prp, ()-> begin.combine(e, o.cdr())));
		}
		public String toString() { return "%PushPrompt"; }
	}
	PushPrompt pushPrompt = new PushPrompt();
	class TakeSubcont extends Combinable {
		{ arity = ge(2); }
		public Object combine(Env e, List o) {
			var chk = checkM(this, o, 2, Any.class, or(ignore, Symbol.class)); // o = (prp (or #ignore symbol) . forms)
			if (chk instanceof Suspension s) return s;
			if (!(chk instanceof Integer /*len*/)) return resumeError(chk, symbol("Integer"));
			var hdl = lambda(e, cons(o.car(1)), o.cdr(1));
			return pipe(dbg(e, this, o), ()-> getTco(evaluate(e, o.car)),
				prp-> tco(()-> new Suspension(prp, hdl).suspend(dbg(e, this, prp, hdl), rr-> Vm.this.combine(e, rr.s, null))))
			;
		}
		public String toString() { return "%TakeSubcont"; }
	}
	class PushSubcont extends Combinable {
		{ arity = ge(1); }
		public Object combine(Env e, List o) {
			var chk = checkM(this, o, 1, or(Symbol.class, List.class)); // o = (k . forms)
			if (chk instanceof Suspension s) return s;
			if (!(chk instanceof Integer /*len*/)) return resumeError(chk, symbol("Integer"));
			var dbg = dbg(e, this, o);
			return pipe(dbg,
				()-> getTco(evaluate(e, o.car)),
				(val)-> switch (val) {
					case Continuation k-> k.apply(()-> begin.combine(e, o.cdr()));
					case Object object-> typeError("cannot push subcont, not a {expected}: {datum}", object, symbol("Continuation"));
				}
			);
		}
		public String toString() { return "%PushSubcont"; }
	}
	class PushDelimSubcont extends Combinable {
		{ arity = ge(2); }
		public Object combine(Env e, List o) {
			var chk = checkM(this, o, 2, Any.class, or(Symbol.class, List.class)); // o = (prp k . forms)
			if (chk instanceof Suspension s) return s;
			if (!(chk instanceof Integer /*len*/)) return resumeError(chk, symbol("Integer"));
			var dbg = dbg(e, this, o);
			return pipe(dbg,
				()-> getTco(evaluate(e, o.car)),
				(_)-> getTco(evaluate(e, o.car(1))),
				(prp, val)-> switch (val) {
					case Continuation k-> pushPrompt(null, e, dbg, prp, ()-> k.apply(()-> begin.combine(e, o.cdr(1))));
					case Object object-> typeError("cannot push delimited subcont, not a {expected}: {datum}", object, symbol("Continuation"));
				}
			);
		}
		public String toString() { return "%PushDelimSubcont"; }
	}
	Object pushPrompt(Resumption r, Env e, Dbg dbg, Object prp, Supplier action) {
		return tco(()-> {
				var res = r != null ? r.resume() : getTco(action.get());
				if (!(res instanceof Suspension s)) return res;
				//   prp == ignore: (pushPrompt #ignore (+ 1 (takeSubcont 'a k (pushSubcont k 1)))) -> 2, prompt #ignore catch any take
				// s.prp == ignore: (pushPrompt 'a (+ 1 (takeSubcont #ignore k (pushSubcont k 1)))) -> 2, take #ignore get previus prompt
				return pstki != null && ((pstki ? prp : s.prp) == ignore) || equals(s.prp, prp)
					? combine(e, s.hdl, cons(s.k))
					: s.suspend(dbg, rr-> pushPrompt(rr, e, dbg, prp, action))
				;
			}
		);
	}
	//*
	Combinable pushSubcontBarrier = new Combinable() {
		{ arity = ge(0); }
		public Object combine(Env e, List o) {
			return combine(null, e, o);
		}
		Object combine(Resumption r, Env e, List o) {
			var res = r != null ? r.resume() : getTco(begin.combine(e, o));
			if (!(res instanceof Suspension s)) return res;
			return tco(()-> new Continuation(dbg(e, "pushSubcontBarrier", o), rr-> combine(rr, e, o), s.k).apply(()-> unboundPromptError(s.prp)));
		}
		public String toString() { return "%PushSubcontBarrier"; }
	};
	/*/ // TODO in alternativa al precedente
	class PushSubcontBarrier implements Combinable  {
		public Object combine(Env e, List o) {
			return combine(null, e, o);
		}
		Object combine(Resumption r, Env e, List o) {
			var res = r != null ? r.resume() : getTco(begin.combine(e, o));
			if (!(res instanceof Suspension s)) return res;
			return s.suspend(dbg(e, "pushSubcontBarrier", o), rr-> combine(rr, e, o)).k.apply(()-> unboundPromptError(s.prp));
		}
		public String toString() { return "%PushSubcontBarrier"; }
	};
	PushSubcontBarrier pushSubcontBarrier = new PushSubcontBarrier();
	//*/
	
	
	// Dynamic Variables
	class DVar extends Box { DVar(Object val) { super(val); }}
	class DVLambda extends Combinable {
		{ arity = ge(1); }
		public Object combine(Env e, List o) { return combine(null, e, o); }
		public Object combine(Resumption r, Env e, List o) {
			var chk = checkM(this, o, 1, cons(Symbol.class)); // o = ((symbol . symbols) . forms)
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
							"expected {operands#,%+d} operands, found: {datum}"
							+ (body == null ? " sett" : " bind") + "ing: " + list(vars)
							+ (body == null ? "" : " of: " + this)
							+ " with: " + o,
							len > vals.length ? null : o.cdr(len-1), len-vals.length
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
						if (body == null) return switch (bndRes) { case 1-> vals[len-1]; case 2-> olds[len-1]; case 3-> de; default-> inert; };
						try {
							Object res = r != null ? r.resume() : getTco(begin.combine(e, body));
							return res instanceof Suspension s ? s.suspend(dbg(de, this, o), rr-> combine(rr, de, o)) : res;
						}
						finally {
							for (int i=0; i<len; i+=1) if (ndvs[i] instanceof DVar dvar) dvar.value = olds[i];
						}
					}
					@Override public String toString() { return "{%DOpv " + o.car + eIfnull(body, ()-> " " + apply(s-> s.substring(1, s.length()-1), Vm.this.toString(body))) + "}"; }
				}
			);
		}
		public String toString() { return "%DV\\"; }
	}
	
	
	// Java Native Interface
	interface ArgsList extends Function<List, Object> {}
	interface ChkList extends BiFunction<Symbol, List, Object> {}
	interface LenList extends BiFunction<Integer, List, Object> {}
	
	class JFun extends Combinable {
		Symbol op; ArgsList jfun; ChkList check;
		JFun(String name, Object arity, ChkList check, LenList jfun) { this(name, check, jfun); this.arity=arity; };
		JFun(String name, ChkList check, LenList jfun) { this(name, jfun); this.check = check; };
		JFun(String name, Object jfun) { this(jfun); this.op = symbol(name); };
		JFun(Object jfun) {
			if (arity == null) arity = switch (jfun) {
				case Supplier _ -> 0;
				case BiConsumer _, BiFunction _ -> 2;
				case Consumer _, Function _ -> 1;
				case Field _ -> list(1, 2);
				case Executable ex-> apply(pc-> ex.isVarArgs() ? list(keyword(">="), pc) : pc , ex.getParameterCount());
				default -> null;
			};
			this.jfun = switch (jfun) {
				case ArgsList al-> al;
				case LenList ll-> o-> pipe(dbg(null, "%" + op, o), ()-> check.apply(op, o),
					obj-> obj instanceof Integer len ? ll.apply(len, o)	: resumeError(obj, symbol("Integer")));
				case Supplier s-> o-> pipe(dbg(null, "%" + op, o), ()-> checkN(op, o, 0),
					obj-> obj instanceof Integer /*len*/ ? s.get() : resumeError(obj, symbol("Integer")));
				case Consumer c-> o-> pipe(dbg(null, "%" + op, o), ()-> checkN(op, o, 1),
					obj->{
						if (! (obj instanceof Integer /*len*/)) return resumeError(obj, symbol("Integer"));
						c.accept(o.car);
						return inert;
					}
				);
				case Function f-> o-> pipe(dbg(null, "%" + op, o), ()-> checkN(op, o, 1),
					obj-> obj instanceof Integer /*len*/ ? f.apply(o.car) : resumeError(obj, symbol("Integer")));
				case BiConsumer bc-> o-> pipe(dbg(null, "%" + op, o), ()-> checkN(op, o, 2),
					obj->{
						if (! (obj instanceof Integer /*len*/)) return resumeError(obj, symbol("Integer"));
						bc.accept(o.car, o.car(1));
						return inert;
					}
				);
				case BiFunction bf-> o-> pipe(dbg(null, "%" + op, o), ()-> checkN(op, o, 2),
					obj-> obj instanceof Integer /*len*/ ? bf.apply(o.car, o.car(1)) : resumeError(obj, symbol("Integer")));
				case Field f-> o-> pipe(null, ()-> checkR(op, o, 1, 2), obj->{
						if (!(obj instanceof Integer len)) return resumeError(obj, symbol("Integer"));
						if (len == 1) return uncked(()-> f.get(o.car));
						return uncked(()->{ f.set(o.car, o.car(1)); return inert; });
					}
				);
				case Method m-> o->{
					var pc = m.getParameterCount();
					return pipe(dbg(null, "%" + op, o),
						()-> m.isVarArgs() ? checkM(op, o, pc) : checkN(op, o, pc+1),
						obj-> obj instanceof Integer /*len*/
							? uncked(()-> m.invoke(o.car, reorg(m, array(o.cdr()))))
							: resumeError(obj, symbol("Integer"))
					);
				};
				case Constructor c-> o->{
					var pc = c.getParameterCount();
					return pipe(dbg(null, "%" + op, o),
						()-> c.isVarArgs() ? checkM(op, o, pc-1) : checkN(op, o, pc),
						obj-> obj instanceof Integer /*len*/
							? uncked(()-> c.newInstance(reorg(c, array(o))))
							: resumeError(obj, symbol("Integer"))
					);
				};
				default -> typeError("cannot build jfun, not a {expected}: {datum}", this, toChk(or(ArgsList.class, LenList.class, Supplier.class, Function.class, BiFunction.class, Field.class, Executable.class)));
			};
		}
		/*
		public Object combine(Env e, List o) {
			return pipe(dbg(e, this, o), ()-> {
					try {
						return jfun.apply(o);
					}
					catch (Throwable thw) {
						switch (thw) {
							case Value val: throw val;
							case Condition cnd: throw cnd;
							case InnerException ie /*when name.equals("%CheckO")* /: throw ie;
							case RuntimeException rte when rte.getCause() instanceof InvocationTargetException ite: {
								switch (ite.getTargetException()) {
									case Value val: throw val;
									case Condition cnd: throw cnd;
									case InnerException ie: throw ie;
									default: break;
								}
							}
							default: break;
						}
						return javaError("error executing: {member} with: {args}", thw, op, o, null);
					}
				}
			);
		}
		/*/
		public Object combine(Env e, List o) {
			return pipe(dbg(e, this, o), ()-> {
				try {
					return jfun.apply(o);
				}
				catch (Throwable thw) {
					if (thw instanceof RuntimeException rte
					&& rte.getCause() instanceof InvocationTargetException ite) {
						thw = ite.getTargetException();
					}
					switch (thw) {
						case Value val:	throw val;
						case Condition cnd: throw cnd;
						case InnerException ie /*when name.equals("%CheckO")*/: throw ie;
						default: return javaError("error executing: {member} with: {args}", thw, op, o, null);
					}					
				}
			});
		}
		//*/
		
		public String toString() {
			if (op != null) return op.name;
			var intefaces = stream(jfun.getClass().getInterfaces()).map(Vm.this::toString).collect(joining(" "));
			return "{JFun" + eIf(intefaces.isEmpty(), ()-> " " + intefaces) + " " + jfun + "}";
		}
	}
	boolean isjFun(Object obj) {
		return isInstance(obj, Supplier.class, Consumer.class, Function.class, BiConsumer.class, BiFunction.class, Executable.class, Field.class);
	}
	
	abstract class AtDot implements ArgsList { String name; }
	
	class At extends AtDot {
		At(String name) { super.name = name; }
		public Object apply(List o) {
			var chk = checkM("At", o, 1, Object.class);
			if (chk instanceof Suspension s) return s;
			if (!(chk instanceof Integer /*len*/)) return resumeError(chk, symbol("Integer"));
			Object o0 = o.car;
			Object[] args = array(o, 1);
			// (@new class . objects)            -> class.getConstructor(getClasses(objects)).newInstance(objects) -> constructor.newInstance(objects)
			// (@<name> object . objects)        -> object.getClass().getMethod(name, getClasses(objects)).invocke(object, objects) -> method.invoke(object, objects)
			// (@getConstructor class . classes) -> class.getConstructor(classes) -> constructor
			// (@getMethod class name . classes) -> class.getMethod(name, classes) -> method
			// (@getField class name)            -> class.getField(name) -> field
			if (name.equals("new") && o0 instanceof Class c && c.getDeclaringClass() == Vm.class && (args.length == 0 || args[0].getClass() != Vm.class)) {
				// (@new Error "assert error!") -> (@new Error vm "assert error!")
				args = headAdd(args, Vm.this);
			}
			var classes = getClasses(args);
			Executable executable = getExecutable(o0, name, classes);
			if (executable == null) return unboundExecutableError(Vm.this.toString(name, classes) + " not found in: {class}", symbol(name), list((Object[]) classes), o0 instanceof Class cl ? cl : o0.getClass());
			try {
				executable.setAccessible(true);
				args = reorg(executable, args);
				return switch (executable) {
					case Method m-> m.invoke(o0, args);
					case Constructor c-> c.newInstance(args);
				};
			}
			catch (Throwable thw) {
				//*
				if (thw instanceof InvocationTargetException ite) {
					switch (thw = ite.getTargetException()) {
						case Value val: throw val;
						case Condition cnd: throw cnd;
						case InnerException ie: throw ie;
						default: // in errore senza!
					}
				}
				return javaError("error executing " + Vm.this.toString(name, classes) + " of: {object} with: {args}", thw, symbol(name), list(args), o0);
				/*/
				if (thw instanceof InvocationTargetException ite) {
					thw = ite.getTargetException();
				}
				switch (thw) {
					case Value val:	throw val;
					case Condition cnd: throw cnd;
					case InnerException ie: throw ie;
					default: return javaError(
						"error executing "
						+ Vm.this.toString(name, classes)
						+ " of: {object} with: {args}", thw, symbol(name), list(args), o0
					);
				}					
				//*/
			}
		}
		public String toString() { return "@" + name; }
	}
	Object at(String name) {
		if (name == null) return typeError("method name is {name}, not a {expected}", name, symbol("String"));
		return new At(name);
	}
	
	class Dot extends AtDot {
		Dot(String name) { super.name = name; }
		public Object apply(List o) {
			var chk = checkR("Dot", o, 1, 2);
			if (chk instanceof Suspension s) return s;
			if (!(chk instanceof Integer len)) return resumeError(chk, symbol("Integer"));
			var o0 = o.car;
			if ("length".equals(name) && len == 1 && o0.getClass().isArray()) return getLength(o0);
			// (.<name> object)       -> object.getclass().getField(name).get(object) -> field.get(object)
			// (.<name> object value) -> object.getClass().getField(name).set(object,value) -> field.set(object, value)
			//TODO (.<name> object (or #ignore #inert :rhs :prv :cnt) value)
			Field field = getField(o0 instanceof Class ? (Class) o0 : o0.getClass(), name);
			if (field == null) return unboundFieldError(name, o0 instanceof Class cl ? cl : o0.getClass());
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
		public String toString() { return "." + name; }
	}
	Object dot(String name) {
		if (name == null) return typeError("field name is null, not a {expected}", name, symbol("String"));
		return new Dot(name);
	}
	
	
	// Error Handling
	Object rootPrompt = new Object() { public String toString() { return "%RootPrompt"; }};
	List pushRootPrompt(List lst) { return cons(listStar(pushPrompt, rootPrompt, lst)); }
	<T> T error(Error err) {
		var userBreak = theEnv.lookup(symbol("userBreak")).value;
		if (userBreak == null) throw err;
		return (T) pipe(dbg(theEnv, "userBreak", err), ()-> getTco(evaluate(theEnv, list(userBreak, err)))
			/* TODO da problemi al debuger
			, val->{ throw thwErr ? err : new Value(ignore, val); }
			*/
		);
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
	<T> T notUniqueError(String msg, Object datum, Object expr) { return error(msg, "type", symbol("syntax"), "datum", datum, "expr", expr); }
	<T> T resumeError(Object datum, Object expected) { return typeError("invalid resume value, not a {expected}: {datum}", datum, expected); }
	<T> T unboundSymbolError(Object name, Env env) { return error("unbound symbol: {symbol} in: {env}", "type", symbol("unboundSymbol"), "symbol", name, "env", env); }
	<T> T unboundSlotError(String slot, Obj obj) { return error("unbound slot: {slot} in: {obj}", "type", symbol("unboundSlot"), "slot", slot, "obj", obj); }
	<T> T unboundFieldError(String field, Object object) { return error("unbound field: {field} in: {object}", "type", symbol("unboundField"), "field", field, "object", object); }
	<T> T unboundExecutableError(String msg, Symbol executable, Object object) { return error(msg, "type", symbol("unboundExecutable"), "executable", executable, "class", object); }
	<T> T unboundExecutableError(String msg, Symbol executable, List classes, Object object) { return error(msg, "type", symbol("unboundExecutable"), "executable", executable, "classes", classes, "class", object); }
	<T> T unboundPromptError(Object prompt) { return error("prompt not found: {prompt}", "type", symbol("unboundPrompt"), "prompt", prompt); }
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
		private List expr;
		private Object pt, ep; // ep == null per Def ovvero %def || %set!, ep == (or #ignore Symbol) per Vau
		private Set syms = new HashSet();
		PTree(List expr, Object pt, Object ep) { this.expr=expr; this.pt=pt; this.ep=ep; }
		Object check() {
			if (!((pt instanceof Symbol || (pt == null || pt == ignore) && ep != null) && syms.add(pt))) {
				if (!(pt instanceof Cons)) return typeError("invalid parameter tree, not {expected}: {datum} in: {expr}", pt, toChk(ep == null ? or(Symbol.class, Cons.class) : or(null, ignore, Symbol.class, Cons.class)), expr);
				var msg = check(pt); if (msg != null) return msg;
			}
			if (ep == null || ep == ignore) return syms.size() > 0 ? null : typeError("invalid parameter tree syntax, not one {expected} in: {datum} of: {expr}", pt, toChk(ep == null ? Symbol.class : or(null, ignore, Symbol.class)), expr);
			if (!(ep instanceof Symbol sym)) return typeError("invalid environment parameter, not {expected}: {datum} in: {expr}", ep, toChk(or(ignore, Symbol.class)), expr);
			return !syms.contains(sym) ? null : notUniqueError("invalid environment parameter, not a unique symbol: {datum} in: {expr}", ep, expr);
		}
		private Object check(Object p) {
			if (p == null || p == ignore) { if (ep != null) syms.add(p); return null; }
			if (p instanceof Symbol) { return syms.add(p) || member(p, atdots) ? null : notUniqueError("invalid parameter tree, not a unique symbol: {datum} in: " + pt + " of: {expr}", p, expr); }
			if (!(p instanceof Cons c)) return null;
			var len = len(c);
			if (member(c.car, quotes))
				return len == 2
					? null
					: matchError("invalid parameter tree, expected {operands#,%+d} operands, found: {datum} in: {subex} of: {expr}", 2 > len ? null : c.cdr(len-2), 2-len, c, expr)
				;
			if (c.car == sharpColon)
				return len != 3
					? matchError("invalid parameter tree, expected {operands#,%+d} operands, found: {datum} in: {subex} of: {expr}", 3 > len ? null : c.cdr(len-2), 3-len, c, expr)
					: !apply(v-> v instanceof Symbol || v == ignore, c.car(2))
					? typeError("invalid parameter tree, not a {expected}: {datum} in: " + c + " of: {expr}", c.car(2), toChk(or(Symbol.class, ignore)), expr)
					: check(c.car(2))
				;
			var msg = check(c.car); if (msg != null) return msg;
			return c.cdr() == null ? null : check(c.cdr());
		}
	}
	Object checkDt(List expr, Object dt) { return new PTree(expr, dt, null).check(); }
	Object checkPt(List expr, Object pt, Object ep) {
		return ep != null
			? new PTree(expr, pt, ep).check()
			: typeError("invalid environment parameter, not {expected}: {datum} in: {expr}", ep, toChk(or(ignore, Symbol.class)), expr)
		;
	}
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
			return innerError(ie, op, o, listStar(min, max, list(chks)));
		}
	}
	int checkL(int min, int max, List o, Object ... chks) {
		var len = chks.length == 0 ? len(o) : checkT(min, max, o, chks);
		var rst = max != more || chks.length <= min ? 0 : (len - min) % (chks.length - min);
		if (len >= min && len <= max && rst == 0) return len;
		throw rst != 0
			? new MatchException("expected {operands#,%+d} operands at end of: {datum}", o, rst)
			: new MatchException("expected {operands#,%+d} operands in: {datum}", o, len<min ? min-len : max-len)
		;
	}
	/*
	int checkT(int min, int max, List o, Object ... chks) {
		int i=0, len=chks.length, lst=len-1, mdl=len-min;
		if (min >= len) max = len;
		for (; o != null; i+=1, o=o.cdr()) {
			if (i >= max) continue;
			var cksi = i < len ? chks[i] : chks[min + (i-min) % mdl];
			if (i == lst // i is the last index
			&& cksi instanceof Object[] cksor // is an or
			&& (cksor[0] instanceof List // and the first is a list
			||  cksor.length > 1 && cksor[0] == null && cksor[1] instanceof List) // or length > 1 and the first is null and the second is a list
			) {
				// check the rest of o!
				return i + checkO(o, cksi);
			}
			checkO(o.car, cksi);
		}
		return i;
	}
	/*/
	static private Object none = new Object();
	int checkT(int min, int max, List o, Object ... chks) {
		int i=0, len=chks.length, mdl=len-min;
		Object chkl = chks[len-1] instanceof Object[] cksor // is an or
				&& (cksor[0] instanceof List // and the first is a list
				||  cksor.length > 1 && cksor[0] == null && cksor[1] instanceof List) // or len > 1 and the first is null and the second is a list
			? cksor
			: none
		;
		if (min >= len) max = len;
		for (; o != null; i+=1, o=o.cdr()) {
			if (i >= max) continue;
			var cksi = i < len ? chks[i] : chks[min + (i-min) % mdl];
			if (cksi == chkl) return i + checkO(o, cksi); // check the rest of o!
			checkO(o.car, cksi);
		}
		return i;
	}
	//*/
	public int checkO(Object o, Object chk) {
		if (equals(o, chk)) return len(o);
		if (chk instanceof Object[] chks) {
			for (int i=0; i<chks.length; i+=1) {
				try {
					//*// TODO probabilmente è sufficiente!
					return checkO(o, chks[i]);
					/*/
					if (chks[i] instanceof List) return checkO(o, chks[i]);
					checkO(o, chks[i]);
					return 0;
					//*/
				}
				catch (Throwable thw) {
				}
			}
		}
		else if (chk instanceof List chkl) {
			if (chkl.car instanceof Object[] chks && chkl.cdr() == null) return checkO(o, chks);
			if (chkl.car == symbol("and")) {
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
			if (chkl.car instanceof Apv) {
				try {
					switch (getTco(combine(env(), chkl.car, cons(o, chkl.cdr(1))))) {
						case Boolean b when b: return 0;
						case Object _: throw new TypeException("not a {expected}: {datum}", o, toChk(chkl.cdr()) /*true*/);
						/* TODO in alternativa del precedente
						case Boolean _: throw new TypeException("not {expected}: {datum}", chkl.cdr(), toChk(true));
						case Object _: throw new TypeException("not a {expected}: {datum}", cons(chkl.car(1), cons(o, chkl.cdr(1))), toChk(Boolean.class));
						*/
					}
				}
				catch (Throwable thw) {
					if (thw instanceof TypeException) throw thw;
					throw new TypeException("not a {expected}: {datum}", o, toChk(chkl));
				}
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
		throw new TypeException("not a {expected}: {datum}", o, toChk(chk));
	}
	public Object toChk(Object chk) {
		return chk instanceof Class cl ? symbol(cl.getSimpleName())
			: chk instanceof Integer i && i == more ? symbol("oo")
			: chk instanceof Object[] a ? cons(symbol("or"), list(stream(a).map(this::toChk).toArray()))
			: chk instanceof List l ? map("toChk", this::toChk, l.car instanceof Apv ? l.cdr() : l )
			: chk
		;
	}
	class Apv0 extends Apv { Apv0(Combinable cmb) { super(cmb); }}
	class Apv1 extends Apv { Apv1(Combinable cmb) { super(cmb); }}
	public boolean checkC(Object obj, Class cl) {
		return cl == Any.class
			|| cl.isInstance(obj)
			|| obj instanceof Class cl2 && cl.isAssignableFrom(cl2)
			|| cl == Apv.class && (obj instanceof Apv || isjFun(obj))
			|| cl == Combinable.class && (obj instanceof Combinable || isjFun(obj))
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
		return op instanceof Opv
			? error("returning from {opv}", ie, "opv", op)
			: !member(op, ":", "check")
			? error("combining {operator} with {operands}", ie, "operator", op, "operands", o)
			// con : e check quando va in errore un check in profondità, es. (: (Integer) (#t))
			// ovvero quando il valore in errore (ie.objs[1]) è diverso dall'intero valore da controllare (o)
			// ovvero quando il check in errore (ie.objs[3]) è diverso dall'intero check da effettuare (chk)
			: ie.objs[1] != o // || !ie.objs[3].equals(toChk(chk))
			? error("checking {operands} with {check}", ie, "operands", o, "check", toChk(chk))
			: error(ie)
		;
	}
	Object and(String s) {
		return uncked(()-> str2lst("and " + s));
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
		return reverse(l, null);
	}
	List reverse(List l, List t) {
		for (; l != null; l = l.cdr()) t = cons(l.car, t);
		return t;
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
		List exp = cons(begin, str2lst(str));
		return vmAssert.combine(theEnv,  objs instanceof Throwable ? exp : cons(exp, bc2exp(objs)));
	}
	Combinable vmAssert = new Combinable() {
		public Object combine(Env env, List o) {
			if (!doAsrt) return true;
			var chk = checkM(this, o, 1); // o = (x . v)
			if (!(chk instanceof Integer /*len*/)) return chk;
			return test.combine(env, cons(null, o));
		}
		public String toString() { return "%Assert"; }
	};
	Combinable test = new Combinable() {
		{ arity = ge(2); }
		public Object combine(Env e, List o) {
			if (!doAsrt) return true;
			var chk = checkM(this, o, 2); // o = (name x . v)
			if (!(chk instanceof Integer len)) return chk;
			var env = env(e);
			var name = eIfnull(o.car, n-> "test " + n + ": ");
			var exp = o.car(1);
			try {
				var val = getTco(pushSubcontBarrier.combine(env, pushRootPrompt(cons(exp))));
				switch (len) {
					case 2:
						print(name, exp, " should throw but is ", val);
						break;
					case 3: {
						if (! (val instanceof Box)) {
							var expt = o.<List>cdr(1);
							if (Vm.this.equals(val, getTco(pushSubcontBarrier.combine(env, pushRootPrompt(expt))))) return true;
							print(name, exp, " should be ", o.car(2), " but is ", val);
							break;
						}
					}
					default: {
						var expt = (List) map("evalExpt", x-> getTco(pushSubcontBarrier.combine(env, pushRootPrompt(cons(x)))), o.cdr(1));
						if (expt.car instanceof Class && matchType(val, expt)) return true;
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
					var expt = (List) map("evalExpt", x-> getTco(pushSubcontBarrier.combine(env, pushRootPrompt(cons(x)))), o.cdr(1));
					if (expt.car instanceof Class && matchType(val, expt)) return true;
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
	private boolean matchType(Object object, List chk) {
		//if (!(expt.car() instanceof Class cls && isType(object, cls))) return false;
		if (!isType(object, chk.car())) return false;
		// TODO manca l'errore se la lista per Box ha più di un argomento ma comunque ritorna false!
		if (object instanceof Box box) return chk.cdr() == null || chk.cdr(1) == null && equals(box.value, chk.car(1));
		for (var l=chk.cdr(); l != null; l = l.cdr(1)) {
			var key = l.car;
			var expt = l.car(1);
			if (key instanceof AtDot ad) {
				var val = ad.apply(cons(object));
				try { checkO(val, expt); } catch (InnerException ie) { return false; }
			}
			else if (object instanceof ObjEnv obj) {
				var lookup = obj.lookup(key);
				if (!lookup.isBound) return false;
				try { checkO(lookup.value, expt); } catch (InnerException ie) { return false; }
			}
			else {
				return false;
			}
		}
		return true;
	}
	
	
	// Bytecode Parser
	Object bc2exp(Object o) {
		return switch (o) {
			case String s-> switch(s) { case "#inert"-> inert; case "#_", "#ignore"-> ignore; case "#:"-> sharpColon; default-> intern(intStr ? s.intern() : s); };
			case Object[] objs-> objs.length == 2 && objs[0] == string ? intStr ? ((String) objs[1]).intern() : objs[1] : bc2lst(objs);
			case null, default-> o;
		};
	}
	<T extends Cons> T bc2lst(Object ... objs) {
		Object head = null;
		int i = objs.length - 1;
		if (i > 1 && ".".equals(objs[i-1])) { head = bc2exp(objs[i]); i-=2; }
		for (; i>=0; i-=1) head = cons(bc2exp(objs[i]), head);
		return (T) head;
	}
	<T extends Cons> T str2lst(String s) throws Exception {
		return (T) bc2lst(str2bc(s));
	}
	
	
	// Stringification
	public String toString() { return "Vm"; }
	String toString(Object o) { return toString(false, o); }
	String toString(boolean t, Object o) {
		return switch (o) {
			case null-> "#null"; // () in cons altrove #null
			case Boolean b-> b ? "#t" : "#f";
			case Character c-> !t ? c.toString() : "#\\" + (isISOControl(c) ? "x"+ toHexString(c) : c);
			case Class cl-> toSource(cl);
			case String s-> !t ? s : '"' + toSource(s) + '"';
			case Object[] a-> {
				var s = new StringBuilder();
				for (var e: a) s.append(eIf(s.isEmpty(), ", ") + toString(true, e));
				yield "[" + s.toString() + "]";
			}
			default-> o.toString();
		};
	}
	String toStringSet(Set<Entry<String,Object>> set) {
		return set.isEmpty() ? "" : " " + set.stream().map(e-> ":" + e.getKey() + " " + toString(true, e.getValue())).collect(joining(" "));
	}
	String toString(String name, Class[] classes) {
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
	Opv evalChk = vmEnv.value("%evalChk");
	Env vmEnv() {
		Env vmEnv = env();
		return (Env) vmEnv.apply(
			list(keyword(":cnt"),
				// Basics
				"%begin", begin,
				"%vau", new Vau(),
				"%def", new DefSet(true),
				"%set!", new DefSet(false),
				"%eval", wrap(new Eval()),
				"%wrap", wrap(new JFun("%Wrap", (Function) this::wrap)),
				"%unwrap", wrap(new JFun("%Unwrap", (Function) this::unwrap)),
				//"%opv?", wrap(new JFun("%Opv?", (Function<Object, Boolean>) obj-> obj instanceof Opv )),
				//"%apv?", wrap(new JFun("%Apv?", (Function<Object, Boolean>) obj-> obj instanceof Apv || isjFun(obj) )),
				"%apply", wrap(new JFun("%Apply", list(2, 3), (n,o)-> checkR(n, o, 2, 3, Combinable.class, Any.class, Env.class), (l,o)-> combine(l == 2 ? env() : o.car(2), unwrap(o.car), o.car(1)) )),
				"%apply*", wrap(new JFun("%Apply*", (ArgsList) o-> combine(env(), unwrap(o.car), o.cdr()) )),
				"%apply**", wrap(new JFun("%Apply**", (ArgsList) o-> combine(env(), unwrap(o.car), (List) listStar(o.cdr())) )),
				// Env
				//"%env?", wrap(new JFun("%Env?", (Function<Object, Boolean>) obj-> obj instanceof Env )),
				"%vmEnv", wrap(new JFun("%VmEnv", 0, (n,o)-> checkN(n, o, 0), (_,_)-> vmEnv() )),
				"%newEnv", wrap(new JFun("%NewEnv", ge(0),
					(n,o)-> checkR(n, o, 0, more,
						or( null,
							list(2, or(null, Env.class), Obj.class),
							list(1, more, or(null, Env.class), or(Symbol.class, Keyword.class, String.class), Any.class))),
					(l,o)-> l==0 ? env() : l==2 ? env(o.car(), o.<Obj>car(1)) : env(o.car(), array(o.cdr())) )),
				"%bind", wrap(new JFun("%Bind", 3, (n,o)-> checkN(n, o, 3, Env.class), (_,o)-> bind(true, 3, o.<Env>car(), o.car(1), o.car(2)) )),
				"%bind?", wrap(new JFun("%Bind?", 3, (n,o)-> checkN(n, o, 3, Env.class), (_,o)-> { try { bind(true, 0, o.<Env>car(), o.car(1), o.car(2)); return true; } catch (InnerException ie) { return false; }} )),
				"%resetEnv", wrap(new JFun("%ResetEnv", (Supplier) ()-> { theEnv.map.clear(); return theEnv; } )),
				"%pushEnv", wrap(new JFun("%PushEnv", (Supplier) ()-> theEnv = env(theEnv))),
				"%popEnv", wrap(new JFun("%PopEnv", (Supplier) ()-> theEnv = theEnv.parent)),
				// Obj
				//"%obj?", wrap(new JFun("%Obj?", (Function<Object, Boolean>) obj-> obj instanceof Obj )),
				"%new", wrap(new JFun("%New", ge(1),
					(n,o)-> checkM(n, o, 1,
						or( list(1, 2, Box.class),
							list(1, more, Obj.class,
								or( list(1, Env.class),
									list(or(Symbol.class, Keyword.class, String.class), Any.class),
									list(1, more, Throwable.class,
										or(Symbol.class, Keyword.class, String.class), Any.class),
									list(1, more, String.class,
										or(	list(or(Symbol.class, Keyword.class, String.class), Any.class),
											list(1, more, Throwable.class,
												or(Symbol.class, Keyword.class, String.class), Any.class) )))))),
					(_,o)-> ((ArgsList) at("new")).apply(listStar(o.car, Vm.this, o.cdr()))
				)),
				// Env & Obj
				//"%objEnv?", wrap(new JFun("%ObjEnv?", (Function<Object, Boolean>) obj-> obj instanceof ObjEnv )),
				"%get", wrap(new JFun("%Get", 2, (n,o)-> checkN(n, o, 2, or(Symbol.class, Keyword.class, String.class), ObjEnv.class), (_,o)-> o.<ObjEnv>car(1).get(o.car)) ),
				"%value", wrap(new JFun("%Value", 2, (n,o)-> checkN(n, o, 2, or(Symbol.class, Keyword.class, String.class), ObjEnv.class), (_,o)-> o.<ObjEnv>car(1).value(o.car)) ),
				"%bound?", wrap(new JFun("%Bound?", 2, (n,o)-> checkN(n, o, 2, or(Symbol.class, Keyword.class, String.class), ObjEnv.class), (_,o)-> o.<ObjEnv>car(1).isBound(o.car)) ),
				"%remove!", wrap(new JFun("%remove!", 2, (n,o)-> checkN(n, o, 2, or(Symbol.class, Keyword.class, String.class), ObjEnv.class), (_,o)-> o.<ObjEnv>car(1).remove(o.car)) ),
				"%keys", wrap(new JFun("%Keys", 1, (n,o)-> checkN(n, o, 1, or(ObjEnv.class)), (_,o)-> o.<ObjEnv>car().keys()) ),
				"%symbols", wrap(new JFun("%Symbols", 1, (n,o)-> checkN(n, o, 1, ObjEnv.class), (_,o)-> o.<ObjEnv>car().symbols()) ),
				"%keywords", wrap(new JFun("%Keywords", 1, (n,o)-> checkN(n, o, 1, ObjEnv.class), (_,o)-> o.<ObjEnv>car().keywords()) ),
				// Cons
				"%car", wrap(new JFun("%Car", list(1, 2), (n,o)-> checkR(n, o, 1, 2, Cons.class, Integer.class), (l,o)-> apply(c-> l == 1 ? c.car() : c.car(o.<Integer>car(1)), o.<Cons>car()) )),
				"%cdr", wrap(new JFun("%Car", list(1, 2), (n,o)-> checkR(n, o, 1, 2, Cons.class, Integer.class), (l,o)-> apply(c-> l == 1 ? c.cdr() : c.cdr(o.<Integer>car(1)), o.<Cons>car()) )),
				"%cnr", wrap(new JFun("%Cnr", 2, (n,o)-> checkN(n, o, 2, Cons.class, Integer.class), (_,o)-> o.<Cons>car().cxr(o.<Integer>car(1)) )),
				"%csr", wrap(new JFun("%Csr", 2, (n,o)-> checkN(n, o, 2, Cons.class, or(Symbol.class, Keyword.class)), (_,o)-> o.<Cons>car().cxr(o.<Intern>car(1).name) )),
				"%csr", wrap(new JFun("%Csr", 2, (n,o)-> checkN(n, o, 2, Cons.class, or(Symbol.class, Keyword.class, String.class)), (_,o)-> o.<Cons>car().cxr(apply(a-> a instanceof Intern i ? i.name : (String) a, o.car(1))) )),		
				"%cxr", wrap(new JFun("%Cxr", 2, (n,o)-> checkN(n, o, 2, Cons.class, or(Symbol.class, Keyword.class, String.class, Integer.class)), (_,o)-> ((ArgsList) at("cxr")).apply(list(o.car(), apply(a-> a instanceof Intern i ? i.name : a, o.car(1)) )) )),
				"%cadr", wrap(new JFun("%Cadr", 1, (n,o)-> checkN(n, o, 1, Cons.class), (_,o)-> o.<Cons>car().car(1) )),
				"%cddr", wrap(new JFun("%Cddr", 1, (n,o)-> checkN(n, o, 1, Cons.class), (_,o)-> o.<Cons>car().cdr(1) )),
				"%cons", wrap(new JFun("%Cons", list(1, 2), (n,o)-> checkR(n, o, 1, 2), (l,o)-> cons(o.car, l == 1 ? null : o.car(1)) )),
				"%setCar", wrap(new JFun("%SetCar",
					list(2, 3),
					(n,o)-> checkR(n, o, 2, 3, Cons.class),
					(l,o)->	switch (bndRes(l == 2 ? ignore : o.car(1))) {
						case Suspension s-> s;
						case Integer i-> switch(i) {
							case 0-> inert((o.<Cons>car()).setCar(o.car(l-1)));
							case 1-> { var v = o.car(l-1); (o.<Cons>car()).setCar(v); yield v; }
							case 2-> { var cons = o.<Cons>car(); var v = cons.car; cons.setCar(o.car(l-1)); yield v; }
							case 3-> (o.<Cons>car()).setCar(o.car(l-1));
							default-> typeError("cannot set car, invalid bndRes value, not {expected}: {datum}", i, toChk(or(0, 1, 2, 3)));
						};
						case Object obj-> resumeError(obj, symbol("Integer"));
					}
				)),
				"%setCdr", wrap(new JFun("%SetCdr",
					list(2, 3),
					(n,o)-> checkR(n, o, 2, 3, Cons.class),
					(l,o)->	switch (bndRes(l == 2 ? ignore : o.car(1))) {
						case Suspension s-> s;
						case Integer i-> switch(i) {
							case 0-> inert((o.<Cons>car()).setCdr(o.car(l-1)));
							case 1-> { var v = o.car(l-1); (o.<Cons>car()).setCdr(v); yield v; }
							case 2-> { var cons = o.<Cons>car(); var v = cons.cdr; cons.setCdr(o.car(l-1)); yield v; }
							case 3-> (o.<Cons>car()).setCdr(o.car(l-1));
							default-> typeError("cannot set cdr, invalid bndRes value, not {expected}: {datum}", i, toChk(or(0, 1, 2, 3)));
						};
						case Object obj-> resumeError(obj, symbol("Integer"));
					}
				)),
				"%null?", wrap(new JFun("%Null?", (Function<Object, Boolean>) obj-> obj == null)),
				//"%!null?", wrap(new JFun("%!Null?", (Function<Object, Boolean>) obj-> obj != null)),
				"%cons?", wrap(new JFun("%Cons?", (Function<Object, Boolean>) obj-> obj instanceof Cons)),
				//"%atom?", wrap(new JFun("%Cons?", (Function<Object, Boolean>) obj-> !(obj instanceof Cons))),
				// List
				"%list?", wrap(new JFun("%List?", (Function<Object, Boolean>) obj-> obj instanceof List)),
				"%list", wrap(new JFun("%List", (ArgsList) o-> o)),
				"%list*", wrap(new JFun("%List*", (ArgsList) this::listStar)),
				"%list-", wrap(new JFun("%List-", (ArgsList) this::listMinus)),
				"%append", wrap(new JFun("%Append", (n,o)-> checkN(n, o, 2, or(null, List.class)), (_,o)-> append(o.car(),o.car(1)) )),
				"%len", wrap(new JFun("%Len", 1, (n,o)-> checkN(n, o, 1 /*, or(null, List.class)*/), (_,o)-> len(o.car()) )),
				//"%arity", wrap(new JFun("%Arity", 1, (n,o)-> checkN(n, o, 1), (_,o)-> arity(o.car()) )),
				"%arity", wrap(new JFun("%Arity", 1, (n,o)-> checkN(n, o, 1), (_,o)->{
					var obj = o.car;
					if (obj instanceof Apv apv) obj = apv.cmb;
					if (obj instanceof JFun jFun) {
						if (jFun.arity != null) return jFun.arity; 
						obj = jFun.jfun;
					}
					return switch (obj) {
						case Opv opv-> opv.xs != null && opv.xs.car == keyword("caseVau")
							? map(null, c-> arity(((List) c).car), (List) opv.e.get("clauses"))
							: arity(opv.pt);
						case Supplier _-> 0;
						case BiConsumer _, BiFunction _ -> 2;
						case Consumer _, Function _ -> 1;
						case Field _ -> list(1, 2);
						case Executable ex-> apply(pc-> ex.isVarArgs() ? list(keyword(">="), pc) : pc , ex.getParameterCount());
						case Combinable cmb-> cmb.arity;
						default -> inert;
					};
				})),
				"%reverse", wrap(new JFun("%Reverse", 1, (n,o)-> checkN(n, o, 1, or(null, List.class)), (_,o)-> reverse(o.car()) )),
				// Symbol Keyword
				"%symbol", wrap(new JFun("%Symbol", 1, (n,o)-> checkN(n, o, 1, String.class), (_,o)-> symbol(o.car()) )),
				"%symbol?", wrap(new JFun("%Symbol?", (Function<Object, Boolean>) obj-> obj instanceof Symbol )),
				"%keyword", wrap(new JFun("%Keyword", 1, (n,o)-> checkN(n, o, 1, String.class), (_,o)-> keyword(o.car()) )),
				"%keyword?", wrap(new JFun("%Keyword?", (Function<Object, Boolean>) obj-> obj instanceof Keyword )),
				"%intern", wrap(new JFun("%Intern", 1, (n,o)-> checkN(n, o, 1, String.class), (_,o)-> intern(o.car()) )),
				"%intern?", wrap(new JFun("%Intern?", (Function<Object, Boolean>) obj-> obj instanceof Intern )),
				"%name", wrap(new JFun("%Name", 1, (n,o)-> checkN(n, o, 1, Intern.class), (_,o)-> o.<Intern>car().name )),
				// Equals
				"%==", wrap(new JFun("%==", (BiFunction<Object,Object,Boolean>) (a,b)-> a instanceof Number ? a.equals(b) : a == b )),
				"%!=", wrap(new JFun("%!=", (BiFunction<Object,Object,Boolean>) (a,b)-> a instanceof Number ? !a.equals(b) : a != b )),
				"%eq?", wrap(new JFun("%Eq?", (BiFunction<Object,Object,Boolean>) Vm.this::equals )),
				// Boolean
				//"%boolean?", wrap(new JFun("%Boolean?", (Function<Object, Boolean>) obj-> obj instanceof Boolean )),
				//"%t?", wrap(new JFun("%T?", (Function<Object, Boolean>) obj-> { try { return switch (istrue(obj)) { case Suspension s-> s; case Boolean b-> b; default-> false; }; } catch (Throwable t) { return false; }})),
				"%!", wrap(new JFun("%!", (Function) a-> switch (istrue(a)) { case Suspension s-> s; case Boolean b-> !b; case Object obj-> typeError("not a {expected}: {datum}", obj, symbol("Boolean")); } )),
				"%!!", wrap(new JFun("%!!", (Function) a-> switch (istrue(a)) { case Suspension s-> s; case Boolean b-> b; case Object obj-> typeError("not a {expected}: {datum}", obj, symbol("Boolean")); } )),
				// Number
				"%number?", wrap(new JFun("%Number?", (Function<Object, Boolean>) obj-> obj instanceof Number )),
				"%+", wrap(new JFun("%+", 2, (n,o)-> checkN(n, o, 2, Number.class, Number.class), (_,o)-> binOp(Pls, o.car(), o.car(1)) )),
				"%*", wrap(new JFun("%*", 2, (n,o)-> checkN(n, o, 2, Number.class, Number.class), (_,o)-> binOp(Pwr, o.car(), o.car(1)) )),
				"%-", wrap(new JFun("%-", 2, (n,o)-> checkN(n, o, 2, Number.class, Number.class), (_,o)-> binOp(Mns, o.car(), o.car(1)) )),
				"%/", wrap(new JFun("%/", 2, (n,o)-> checkN(n, o, 2, Number.class, Number.class), (_,o)-> binOp(Dvd, o.car(), o.car(1)) )),
				"%%", wrap(new JFun("%%", 2, (n,o)-> checkN(n, o, 2, Number.class, Number.class), (_,o)-> binOp(Rst, o.car(), o.car(1)) )),
				// String
				"%string?", wrap(new JFun("%String?", (Function<Object, Boolean>) obj-> obj instanceof String )),
				"%$", wrap(new JFun("%$", (BiFunction<Object,Object,String>) (a,b)-> Vm.this.toString(a) + Vm.this.toString(b))),
				// Comparator
				//"%comparator?", wrap(new JFun("%Comparator?", (Function<Object, Boolean>) obj-> obj instanceof Comparator )),
				"%<", wrap(new JFun("%<", 2, (n,o)-> checkN(n, o, 2, Comparable.class, Comparable.class), (_,o)-> o.<Comparable>car().compareTo(o.car(1)) < 0 )),
				"%>", wrap(new JFun("%>", 2, (n,o)-> checkN(n, o, 2, Comparable.class, Comparable.class), (_,o)-> o.<Comparable>car().compareTo(o.car(1)) > 0 )),
				"%<=", wrap(new JFun("%<=", 2, (n,o)-> checkN(n, o, 2, Comparable.class, Comparable.class), (_,o)-> o.<Comparable>car().compareTo(o.car(1)) <= 0 )),
				"%>=", wrap(new JFun("%>=", 2, (n,o)-> checkN(n, o, 2, Comparable.class, Comparable.class), (_,o)-> o.<Comparable>car().compareTo(o.car(1)) >= 0 )),
				// Bit
				//"%integer?", wrap(new JFun("%Integer?", (Function<Object, Boolean>) obj-> obj instanceof Integer )),
				"%~", wrap(new JFun("%~", 1, (n,o)-> checkN(n, o, 1, Integer.class), (_,o)-> ~o.<Integer>car() )),
				"%&", wrap(new JFun("%&", 2, (n,o)-> checkN(n, o, 2, Number.class, Number.class), (_,o)-> binOp(And, o.car(), o.car(1)) )),
				"%|", wrap(new JFun("%|", 2, (n,o)-> checkN(n, o, 2, Number.class, Number.class), (_,o)-> binOp(Or, o.car(), o.car(1)) )),
				"%^", wrap(new JFun("%^", 2, (n,o)-> checkN(n, o, 2, Number.class, Number.class), (_,o)-> binOp(Xor, o.car(), o.car(1)) )),
				"%<<", wrap(new JFun("%<<", 2, (n,o)-> checkN(n, o, 2, Number.class, Number.class), (_,o)-> binOp(Sl, o.car(), o.car(1)) )),
				"%>>", wrap(new JFun("%>>", 2, (n,o)-> checkN(n, o, 2, Number.class, Number.class), (_,o)-> binOp(Sr, o.car(), o.car(1)) )),
				"%>>>", wrap(new JFun("%>>>", 2, (n,o)-> checkN(n, o, 2, Number.class, Number.class), (_,o)-> binOp(Sr0, o.car(), o.car(1)) )),
				// First-Order Control
				"%if", new If(),
				"%loop", new Loop(),
				"%atEnd", new AtEnd(),
				"%throwTag", new ThrowTag(),
				"%catchTagWth", new CatchTagWth(),
				// Delimited Control
				"%pushPrompt", pushPrompt,
				"%takeSubcont", new TakeSubcont(),
				"%pushSubcont", new PushSubcont(),
				"%pushDelimSubcont", new PushDelimSubcont(),
				"%pushSubcontBarrier", pushSubcontBarrier,
				// Box
				//"%box?", wrap(new JFun("%Box?", (Function<Object, Boolean>) obj-> obj instanceof Box )),
				"%newBox", wrap(new JFun("%NewBox", list(0, 1), (n,o)-> checkR(n, o, 0, 1), (l,o)-> new Box(l == 0 ? boxDft : o.car))),
				// Dynamically-Scoped Variables
				//"%dVar?", wrap(new JFun("%DVar?", (Function<Object, Boolean>) obj-> obj instanceof DVar )),
				"%newDVar", wrap(new JFun("%NewDVar", list(0, 1), (n,o)-> checkR(n, o, 0, 1), (l,o)-> new DVar(l == 0 ? boxDft : o.car))),
				"%dVal", wrap(new JFun("%DVal", list(1, 2), (n,o)-> checkR(n, o, 1, 2, DVar.class), (l,o)-> apply(dv-> l == 1 ? dv.value : (dv.value=o.car(1)), o.<DVar>car()) )),
				"%dv\\", new DVLambda(),
				// Errors
				"%rootPrompt", rootPrompt,
				"%error", wrap(new JFun("%Error", (ArgsList) o-> ((ArgsList) at("error")).apply(cons(this, o)))),
				// Java Interface
				"%jFun?", wrap(new JFun("%JFun?", (Function<Object,Boolean>) this::isjFun)),
				"%at", wrap(new JFun("%At", (Function<String,Object>) this::at)),
				"%dot", wrap(new JFun("%Dot", (Function<String,Object>) this::dot)),
				"%supplier", new Combinable() {
					{ arity = 0; }
					@Override public final <T> T combine(Env e, List o) {
						var chk = checkM(this, o, 1, (Object) null); // o = (() . forms)
						if (chk instanceof Suspension s) return (T) s;
						if (!(chk instanceof Integer /*len*/)) return resumeError(chk, symbol("Integer"));
						var opv = opv(e, o.car, ignore, o.cdr());
						return (T) new Supplier() {
							@Override public Object get() {
								return getTco(opv.combine(e, null));
							};
							@Override public String toString() {
								return "{Supplier " + stream(array(o)).map(Vm.this::toString).collect(joining(" ")) + "}";
							}
						};
					}
					@Override public String toString() { return "%Supplier"; }
				},
				"%consumer", new Combinable() {
					{ arity = 1; }
					@Override public final <T> T combine(Env e, List o) {
						var chk = checkM(this, o, 1, list(1, Symbol.class)); // o = ((symbol) . forms)
						if (chk instanceof Suspension s) return (T) s;
						if (!(chk instanceof Integer /*len*/)) return resumeError(chk, symbol("Integer"));
						var opv = opv(e, o.car, ignore, o.cdr());
						return (T) new Consumer() {
							@Override public void accept(Object t) {
								getTco(opv.combine(e, cons(t)));
							};
							@Override public String toString() {
								return "{Consumer " + stream(array(o)).map(Vm.this::toString).collect(joining(" ")) + "}";
							}
						};
					}
					@Override public String toString() { return "%Consumer"; }
				},
				"%function", new Combinable() {
					{ arity = 1; }
					@Override public final <T> T combine(Env e, List o) {
						var chk = checkM(this, o, 1, list(1, Symbol.class)); // o = ((symbol) . forms)
						if (chk instanceof Suspension s) return (T) s;
						if (!(chk instanceof Integer /*len*/)) return resumeError(chk, symbol("Integer"));
						var opv = opv(e, o.car, ignore, o.cdr());
						return (T) new Function() {
							@Override public Object apply(Object t) {
								return getTco(opv.combine(e, cons(t)));
							};
							@Override public String toString() {
								return "{Function " + stream(array(o)).map(Vm.this::toString).collect(joining(" ")) + "}";
							};
						};
					}
					@Override public String toString() { return "%Function"; }
				},
				"%biConsumer", new Combinable() {
					{ arity = 2; }
					@Override public final <T> T combine(Env e, List o) {
						var chk = checkM(this, o, 1, list(2, Symbol.class)); // o = ((symbol symbol) . forms)
						if (chk instanceof Suspension s) return (T) s;
						if (!(chk instanceof Integer /*len*/)) return resumeError(chk, symbol("Integer"));
						var opv = opv(e, o.car, ignore, o.cdr());
						return (T) new BiConsumer() {
							@Override public void accept(Object t, Object u) {
								getTco(opv.combine(e, list(t, u)));
							};
							@Override public String toString() {
								return "{BiConsumer " + stream(array(o)).map(Vm.this::toString).collect(joining(" ")) + "}";
							}
						};
					}
					@Override public String toString() { return "%BiConsumer"; }
				},
				"%biFunction", new Combinable() {
					{ arity = 2; }
					@Override public final <T> T combine(Env e, List o) {
						var chk = checkM(this, o, 1, list(2, Symbol.class)); // o = ((symbol) . forms)
						if (chk instanceof Suspension s) return (T) s;
						if (!(chk instanceof Integer /*len*/)) return resumeError(chk, symbol("Integer"));
						var opv = opv(e, o.car, ignore, o.cdr());
						return (T) new BiFunction() {
							@Override public Object apply(Object t, Object u) {
								return getTco(opv.combine(e, list(t, u)));
							};
							@Override public String toString() {
								return "{Function " + stream(array(o)).map(Vm.this::toString).collect(joining(" ")) + "}";
							};
						};
					}
					@Override public String toString() { return "%BiFunction"; }
				},
				"%instanceOf?", wrap(new JFun("%InstanceOf?", (n,o)-> checkN(n, o, 2, Any.class, Class.class), (_,o)-> o.<Class>car(1).isInstance(o.car) )),
				// Class
				//"%class?", wrap(new JFun("%Class?", (Function<Object, Boolean>) obj-> obj instanceof Class )),
				"%newClass", wrap(new JFun("%NewClass", list(1, 2), (n,o)-> checkR(n, o, 1, 2, Symbol.class, or(Box.class, Obj.class)), (_,o)-> newClass(o.car(), apply(cdr-> cdr == null ? null : cdr.car(), o.cdr())) )),
				"%subClass?", wrap(new JFun("%SubClass?", 2, (n,o)-> checkN(n, o, 2, Class.class, Class.class), (_,o)-> o.<Class>car(1).isAssignableFrom(o.car()) )),
				"%type?",  wrap(new JFun("%Type?", 2, (n,o)-> checkN(n, o, 2, Any.class, or(null, Class.class)), (_,o)-> isType(o.car, o.car(1)) )),
				"%classOf", wrap(new JFun("%ClassOf", 2, (n,o)-> checkN(n, o, 1), (_,o)-> apply(o1-> o1 == null ? null : o1.getClass(), o.car) )),
				// Method
				"%addMethod", wrap(new JFun("%AddMethod", 3, (n,o)-> checkN(n, o, 3, or(null, Class.class), Symbol.class, Apv.class), (_,o)-> addMethod(o.car(), o.car(1), o.car(2)) )),
				"%getMethod", wrap(new JFun("%GetMethod", 2, (n,o)-> checkN(n, o, 2, or(null, Class.class), Symbol.class), (_,o)-> getMethod(o.car(), o.car(1)) )),
				// Check
				"%matchType?", wrap(new JFun("%MatchType?",
					(n,o)-> checkN(n, o, 2,	Any.class,
						or( list(1, 2, Box.class, Any.class),
							list(1, more, Any.class,
								list(1, more, Class.class,
									or(At.class, Dot.class, Symbol.class, Keyword.class, String.class), Any.class)))),
					(_,o)-> matchType(o.car, o.car(1)) )),
				"%:", new Colon(":"),
				"%check", new Colon("check"),
				"%evalChk", opv(vmEnv,"""
					(ck) env
					(%def %=*
					  (%vau (key . lst) env
					    (%def key (%eval key env))
					    ( (%def loop :rhs (%\\ (lst) (%if (%null? lst) #f (%== (%car lst) key) #t (loop (%cdr lst)) ) )) lst) ))
					(%def evl
					  (%\\ (ck)
					    (%if
					      (%== ck 'oo) (.MAX_VALUE &java.lang.Integer)
					      (%! (%cons? ck)) (%eval ck env)
					      ( (%\\ (ckcar)
					          (%if
					            (%== ckcar 'or) (%list->array (evm (%cdr ck)))
					            (%== ckcar 'and) (%cons 'and (evm (%cdr ck)))
					            (%=* ckcar %' quote) (%cadr ck)
					            ( (%\\ (evckcar)
					                (%if (%type? evckcar &Wat.Vm$Apv)
					                  (%cons evckcar (%cons ckcar (%eval (%list* '%list (%cdr ck)) env)))
					                  (%cons (evl ckcar) (evm (%cdr ck))) ))
					              (%eval ckcar env) ) ))
					        (%car ck) ) )))
					(%def evm (%\\ (lst) (%if (%null? lst) #null (%cons (evl (%car lst)) (evm (%cdr lst))))))
					(evl ck)
					"""
				),
				// Array
				//"%array?", wrap(new JFun("%Array?", (Function<Object, Boolean>) obj-> obj.getClass().isArray() )),
				"%list->array", wrap(new JFun("%List->Array", ge(1), (n,o)-> checkM(n, o, 1, List.class), (_,o)-> array(o.car()) )),
				"%array->list", wrap(new JFun("%Array->List", ge(2), (n,o)-> checkM(n, o, 2, Boolean.class, Object[].class), (_,o)-> list(o.<Boolean>car(), o.<Object[]>car(1)) )),
				// Derivated
				"%theEnv", opv(vmEnv, "() env env"),
				"%'", opv(vmEnv, "(arg) #ignore arg"),
				"%`", opv(vmEnv, "(arg) #ignore arg"),
				"%´", opv(vmEnv, "(arg) #ignore arg"),
				"%\\", opv(vmEnv, "(formals . forms) env (%wrap (%eval (%list* %vau formals #ignore forms) env))"),
				// Extra
				"vm", this,
				"%test", test,
				"%assert", vmAssert,
				"toString", wrap(new JFun("ToString", (Function<Object,String>) Vm.this::toString )),
				// Input/Output
				"log", wrap(new JFun("Log", (ArgsList) o-> log(array(o)) )),
				"print", wrap(new JFun("Print", (ArgsList) o-> print(array(o)) )),
				"write", wrap(new JFun("Write", (ArgsList) o-> write(array(o)) )),
				"load", wrap(new Combinable() {
					{ arity = list(1, 2); }
					@Override public final <T> T combine(Env e, List o) {
						var chk = checkR(this, o, 1, 2, String.class, Env.class); // o = (string env)
						if (chk instanceof Suspension s) return (T) s;
						if (!(chk instanceof Integer len)) return resumeError(chk, symbol("Integer"));
						return (T) uncked(()-> loadText(len==1 ? e : o.<Env>car(1), o.<String>car()));
					}
					@Override public String toString() { return "%Load"; }
				}),
				"read", wrap(new JFun("Read", (n,o)-> checkR(n, o, 0, 1, Integer.class), (l,o)-> uncked(()-> str2lst(read(l == 0 ? 0 : o.<Integer>car())).car) )),
				//"eof", new JFun("eof", (n,o)-> checkN(n, o, 0), (l,o)-> List.Parser.eof),
				//"eof?", wrap(new JFun("eof?", (n,o)-> checkN(n, o, 1), (l,o)-> List.Parser.eof.equals(o.car))),
				"readString", wrap(new JFun("ReadString", 1, (n,o)-> checkN(n, o, 1), (_,o)-> uncked(()-> str2lst(o.toString())) )),
				"system", wrap(new JFun("System", (n,o)-> checkR(n, o, 1, 2, String.class, Boolean.class), (l,o)-> uncked(()-> system(l==1 ? false : o.<Boolean>car(1),  "cmd.exe", "/e:on", "/c", o.<String>car())) )),
				// Config
				"doTco", wrap(new JFun("DoTco", list(0, 1), (n,o)-> checkR(n, o, 0, 1, Boolean.class), (l,o)-> l == 0 ? doTco : inert(doTco=o.car()) )),
				"doAsrt", wrap(new JFun("DoAsrt", list(0, 1), (n,o)-> checkR(n, o, 0, 1, Boolean.class), (l,o)-> l == 0 ? doAsrt : inert(doAsrt=o.car()) )),
				"intStr", wrap(new JFun("IntStr", 0, (n,o)-> checkN(n, o, 0), (_,_)-> intStr )),
				"prTrc", wrap(new JFun("PrTrc", list(0, 1), (n,o)-> checkR(n, o, 0, 1, or(0, 1, 2, 3, 4, 5, 6)), (l,o)-> l == 0 ? prTrc : inert(start=level-(doTco ? 0 : 3), prTrc=o.car()) )),
				"typeT", wrap(new JFun("TypeT", list(0, 1), (n,o)-> checkR(n, o, 0, 1, or(0, 1, 2, 3, 4)), (l,o)-> l == 0 ? typeT : inert(typeT=o.car()) )),
				"bndRes", wrap(new JFun("BndRes", list(0, 1),
					(n,o)-> checkR(n, o, 0, 1, or(inert, keyword("rhs"), keyword("prv"), keyword("cnt"))),
					(l,o)-> l == 0
						? switch (bndRes) {case 1-> keyword("rhs"); case 2-> keyword("prv"); case 3-> keyword("cnt"); default-> inert; }
						: inert(bndRes=(int) bndRes(o.car))
				)),
				"prEnv", wrap(new JFun("PrEnv", list(0, 1), (n,o)-> checkR(n, o, 0, 1, Integer.class), (l,o)-> l == 0 ? prEnv : inert(prEnv=o.car()) )),
				"boxDft", wrap(new JFun("BoxDft", list(0, 1), (n,o)-> checkR(n, o, 0, 1), (l,o)-> l == 0 ? boxDft : inert(boxDft=o.car) )),
				"aQuote", wrap(new JFun("AQuote", list(0, 1), (n,o)-> checkR(n, o, 0, 1, Boolean.class), (l,o)-> l == 0 ? aQuote : inert(aQuote=o.car()) )),
				"prStk", wrap(new JFun("PrStk", list(0, 1), (n,o)-> checkR(n, o, 0, 1, Boolean.class), (l,o)-> l == 0 ? prStk : inert(prStk=o.car()) )),
				"prWrn", wrap(new JFun("PrWrn", list(0, 1), (n,o)-> checkR(n, o, 0, 1, Boolean.class), (l,o)-> l == 0 ? prWrn : inert(prWrn=o.car()) )),
				"prAttr", wrap(new JFun("PrAttr", list(0, 1), (n,o)-> checkR(n, o, 0, 1, Boolean.class), (l,o)-> l == 0 ? prAttr : inert(prAttr=o.car()) )),
				"prInert", wrap(new JFun("PrInert", list(0, 1), (n,o)-> checkR(n, o, 0, 1, Boolean.class), (l,o)-> l == 0 ? prInert : inert(prInert=o.car()) )),
				//"bndSmt", wrap(new JFun("BndSmt", 0, (n,o)-> checkN(n, o, 0), (l,o)-> bndSmt )),
				"bndSmt", wrap(new JFun("BndSmt", list(0, 1), (n,o)-> checkR(n, o, 0, 1, Boolean.class), (l,o)-> l == 0 ? bndSmt : inert(bndSmt=o.car()) )),
				"thwErr", wrap(new JFun("ThwErr", list(0, 1), (n,o)-> checkR(n, o, 0, 1, Boolean.class), (l,o)-> l == 0 ? thwErr : inert(thwErr=o.car()) )),
				"hdlAny", wrap(new JFun("HdlAny", list(0, 1), (n,o)-> checkR(n, o, 0, 1, Boolean.class), (l,o)-> l == 0 ? hdlAny : inert(hdlAny=o.car()) ))
			)
		);
	}
	
	
	// API
	public Object exec(Env env, Object bytecode) {
		return getTco(pushSubcontBarrier.combine(env, pushRootPrompt(cons(cons(new Begin(true), bc2exp(bytecode))))));
	}
	public Object call(String funName, Object ... args) {
		return exec(theEnv, $(funName, ".", $(args)));
	}
	public Object get(String varName) {
		return exec(theEnv, symbol(varName));
	}
	public Object eval(Env env, String exp) throws Exception {
		return exec(env, str2bc(exp));
	}
	public String readText(String fileName) throws IOException {
		return Files.readString(Paths.get(fileName), Charset.forName("UTF-8"));
	}
	public List readList(String fileName) throws Exception {
		return str2lst(readText(fileName));
	}
	public void writeByteCode(String fileName) throws Exception {
		try (var oos = new ObjectOutputStream(new FileOutputStream("build/" + fileName))) {
			oos.writeObject(str2bc(readText(fileName)));
		}
	}
	public Object readBytecode(String fileName) throws Exception {
		try (var ois = new ObjectInputStream(new FileInputStream("build/" + fileName))) {
			return ois.readObject();
		}
	}
	public Object loadText(Env env, String fileName) throws Exception {
		if (prTrc >= 1) print("\n--------: " + fileName);
		var v = eval(env, readText(fileName));
		if (prTrc > 1) print("--------: " + fileName + " end");
		return v;
	}
	public Object loadBytecode(String fileName) throws Exception {
		if (prTrc >= 1) print("\n--------: " + fileName);
		var v = exec(theEnv, readBytecode(fileName));
		if (prTrc > 1) print("--------: " + fileName + " end");
		return v;
	}
	public void repl() throws Exception {
		loop: for (;;) {
			switch (read()) {
				case "":
					break loop;
				case String exp: try {
					var val = eval(theEnv, exp);
					if (!prInert && val == inert) break;
					print(val);
				}
				catch (Throwable thw) {
					if (prStk)
						thw.printStackTrace(out);
					else if (thw instanceof Value v) {
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
			? ifnull(o.getMessage(), toSource(o.getClass())) + (prAttr ? toStringSet(o.map.entrySet()) : "")
			: toSource(thw.getClass()) + eIfnull(thw.getMessage(), msg-> ": " + msg)
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
			var res = loadText(theEnv, file);
			print("start time: " + (currentTimeMillis() - milli));
			if (res != ignore) print(res);
		}
		repl();
	}
}
