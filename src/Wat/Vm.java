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
import static Wat.Utility.isInstance;
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
import static java.lang.System.currentTimeMillis;
import static java.lang.System.out;
import static java.util.Arrays.stream;
import static java.util.stream.Collectors.joining;
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

import javax.tools.DiagnosticCollector;
import javax.tools.JavaFileObject;
import javax.tools.SimpleJavaFileObject;

// java.exe -cp bin --enable-preview Wat.Vm

/* Abbreviations:
	c: cons
	x, cln: expression
	op: operator
	o, os: operands
	o0, o1, ..: operand 0, 1, ..
	cmb: combiner
	opv: operative combiner
	apv: applicative combiner
	apv0, tnk: 0 args applicative combiner
	apv1, hdl: 1 arg applicative combiner
	p: parameter
	pt: parameters tree
	arg: argument
	args: arguments
	e: environment
	eo: environment operand
	ep: environment parameter
	xe: extended environment
	k, nxt: continuation
	s: sospension
	r: resumption
	f: function
	s: supplier
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
 */

public class Vm {

	static {
	    for (File file: new File("bin/Ext").listFiles()) file.delete();
	}

	boolean dotco = true; // do tco
	boolean doasrt = true; // do assert
	boolean ctapv = false; // catch & throw applicative
	boolean instr = false; // intern string
	
	int prtrc = 0; // print trace: 0:none, 1:load, 2:eval root, 3:eval all, 4:return, 5:combine, 6:bind/lookup
	int prenv = 3; // print environment
	boolean prstk = false; // print stack
	
	
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
		Object prt; Combinable hdl; Continuation k;
		Suspension(Object prt, Combinable handler) { this.prt = prt; this.hdl = handler; }
		public String toString() { return "{Suspension %s %s %s}".formatted(prt, hdl, k); }
		Suspension suspend(Dbg dbg, Function<Resumption, Object> f) { 
			k = new Continuation(dbg, f, k); return this;
		}
	}
	Object pipe(Dbg dbg, Supplier before, Function ... after) {
		return pipe(null, dbg, before, after);
	}
	Object pipe(Resumption r, Dbg dbg, Supplier before, Function ... after) {
		var res = r != null ? r.resume() : before.get();
		return res instanceof Suspension s ? s.suspend(dbg, rr-> pipe(rr, dbg, before, after)) : pipe(null, 0, dbg, res, after);
	}
	Object pipe(Resumption r, int i, Dbg dbg, Object res, Function ... after) {
		for (var first=true; i<after.length; i+=1) { // only one resume for suspension
			res = first && r != null && !(first = false) ? r.resume() : after[i].apply(res);
			if (res instanceof Suspension s) { var ii=i; var rres=res; return s.suspend(dbg, rr-> pipe(rr, ii, dbg, rres, after)); }
		}
		return res;
	}
	Object mapCar(Function f, List todo) {
		return mapCar(null, null, f, todo);
	}
	Object mapCar(Resumption r, List done, Function f, List todo) {
		for (var first=true;;) { // only one resume for suspension
			if (todo == null) return reverse(done); 
			var res = first && r != null && !(first = false) ? r.resume() : f.apply(todo.car());
			if (res instanceof Suspension s) { List td=todo, dn=done; return s.suspend(dbg(null, "mapCar", todo.car), rr-> mapCar(rr, dn, f, td)); }
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
	Inert inert(Object value) { return inert; }
	
	class Ignore { public String toString() { return "#ignore"; }};
	public Ignore ignore = new Ignore();
	
	
	// Tail Call Optimization
	interface Tco extends Supplier {};
	Object tco(Tco tco) { return dotco ? tco : tco.get(); }
	<T> T getTco(Object o) { while (o instanceof Tco tco) o = tco.get(); return (T) o; }
	
	
	// Trace Log
	int level=0, start=0; String indent = "|  ";
	String indent() { return indent.repeat((level-start)) + "|" + stackDeep() + ":  " ; 	}
	
	
	// Evaluation Core
	@SuppressWarnings("preview")
	<T> T evaluate(Env e, Object o) {
		if (prtrc >= 3) print("evaluate: ", indent(), o, "   ", e);
		T v; try {
			level += 1;
			v = getTco(
				switch (o) {
					case null, default-> o;
					case Symbol s-> pipe(dbg(e, o), ()-> e.lookup(s));
					case List l-> {
						var ee=e; yield pipe(dbg(ee, o), ()-> evaluate(ee, l.car), op-> combine(ee, op, l.cdr()));
					}
					case Cons c-> error("not a proper list: " + c);
				}
			);
		}
		finally {
			level -= 1;
		}
		if (prtrc >= 4) print("  return: ", indent(), v); 
		return v;
	}
	
	abstract class Intern {
		String name;
		Intern(String name) { this.name = name; }
		public String toString() { return name; }
		public int hashCode() { return Objects.hashCode(name); }
		public boolean equals(Object o) {
			return this == o || this.getClass().isInstance(o) && name.equals(((Intern) o).name);
		}		
	}
	class Keyword extends Intern { Keyword(String name) { super(name); }}
	Keyword keyword(String name) { return new Keyword(name); }
	class Symbol extends Intern { Symbol(String name) { super(name); }}
	Symbol symbol(String name) { return new Symbol(name); }
	
	class Cons {
		Object car, cdr;
		Cons(Object car, Object cdr) { this.car = car; this.cdr = cdr; }
		public String toString() { return "(" + toString(this) + ")"; }
		private String toString(Cons c) {
			var car = Vm.this.toString(true, c.car);
			if (c.cdr == null) return car;
			if (c.cdr instanceof Cons cdr) return car + " " + toString(cdr);
			return car + " . " + Vm.this.toString(true, c.cdr);
		}
		public boolean equals(Object o) {
			return this == o || o instanceof Cons c && Vm.this.equals(this.car,  c.car) && Vm.this.equals(this.cdr,  c.cdr);
		}
		public <T> T car() { return (T) car; }
		public <T> T cdr() { return (T) cdr; }
		public <T> T car(int i) { Cons o=this; for (; i>0 && o.cdr instanceof Cons c; i-=1, o=c); return i==0 ? o.car() : error("not a cons: " + o); }
		public <T> T cdr(int i) { Cons o=this; for (; i>0 && o.cdr instanceof Cons c; i-=1, o=c); return i==0 ? o.cdr() : error("not a cons: " + o); }
		Object setCar(Object car) { return this.car = car; }
		Object setCdr(Object cdr) { return this.cdr = cdr; }
	}
	public class List extends Cons {
		List(Object car, List cdr) { super(car, cdr); }
		@Override public List cdr() { return (List) cdr; }
		List setCdr(List cdr) { return (List)(this.cdr = cdr); }
		@Override Object setCdr(Object cdr) { return cdr == null || cdr instanceof List ? this.cdr = cdr : error("not a list: " + cdr); }
	}
	public int len(List o) { int i=0; for (; o != null; i+=1, o=o.cdr()); return i; }
	<T> T cons(Object car, Object cdr) {
		return (T)(cdr == null || cdr instanceof List ? new List(car, (List) cdr) : new Cons(car, cdr));
	}
	
	
	// Environment
	class Env<K> /*extends LinkedHashMap<K,Object>*/ {
		Env parent; Map<K,Object> map = new LinkedHashMap(); 
		Env(Env parent) { this.parent = parent; }
		record Lookup(boolean isBound, Object value) {}
		Lookup get(K name) {
			/* TODO sostituito dal seguente
			Env env = this; do {
				Object res = env.map.get(name);
				if (res != null || env.map.containsKey(name)) return new Lookup(true, res);
			} while ((env = env.parent) != null);
			*/
			for (var env=this; env!=null; env=env.parent) {
				Object res = env.map.get(name);
				if (res != null || env.map.containsKey(name)) return new Lookup(true, res);
			}
			return new Lookup(false, null);
		};
		boolean set(K name, Object value) {
			/* TODO sostituito dal seguente
			Env env = this; do {
				if (env.map.containsKey(name)) { env.put(name, value); return true; }
			} while ((env = env.parent) != null);
			*/
			for (var env=this; env!=null; env=env.parent)
				if (env.map.containsKey(name)) { env.put(name, value); return true; }
			return false; // TODO or error("!") ok new Lookup(false, null)
		};
		Object put(K name, Object value) {
			if (prtrc >= 6) print("    bind: ", name, "=", value, " in: ", this); map.put(name, value); return null; 
		}
		public String toString() {
			var deep = deep();
			var prefix = switch(deep) { case 1-> "Vm-"; case 2-> "The-"; default-> ""; };
			return "{" + prefix + "Env[" + map.size() + "]" + eIf(deep < prenv, ()-> reverseMap(map)) + eIf(parent == null, ()-> " " + parent) + "}";
		}
		Object lookup(K name) {
			var lookup = get(name); if (!lookup.isBound) return error("unbound: " + name);
			if (prtrc >= 6) print("  lookup: ", lookup.value); return lookup.value;
		}
		boolean isParent(Env other) {
			/* TODO sostituito dal seguente
			Env env = this; do if (env == other) return true; while ((env = env.parent) != null);
			*/
			for (var env=this; env!=null; env=env.parent) if (other == env) return true; 
			return false;
		};
		int deep() { int i=0; for (var env=this; env!=null; env=env.parent) i+=1; return i; }
	}
	Env env(Env parent) { return new Env<Symbol>(parent); }
	
	
	// Classes and Objects
	public class StdObj extends LinkedHashMap<Keyword, Object> {
		private static final long serialVersionUID = 1L;
		public StdObj(List list) {
	        for (var l=list; l!=null; l=l.cdr()) {
	        	var car = l.car; if (!(car instanceof Keyword key)) throw new Error("not a keyword: " + car + " in: " + list); 
	        	l = l.cdr(); if (l == null) throw new Error("a value expected in: " + list);
	        	put(key, l.car);
	        }
		}
	}
	Class extend(Symbol className, Class superClass) {
		try {
			if (!className.name.matches("[a-zA-Z$_.]+")) return error("invalid class name: " + className);
			if (superClass != null && !StdObj.class.isAssignableFrom(superClass)) return error("invalid extendible " + superClass);
			var c = Class.forName("Ext." + className);
			out.println("Warning: class " + className + " just defined!");
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
			    var isStdObj = superClass == null || superClass == StdObj.class;
			    var task = getSystemJavaCompiler().getTask(
			    	null, null, diagnostics,
			    	java.util.List.of("-d", "bin", "--enable-preview", "-source", "19", "-Xlint:unchecked" ),
			    	null,
			    	java.util.List.of(
				    	new JavaStringFile("Ext." + className, """
							package Ext;
							import Wat.Vm;
							public class %1$s extends %2$s {
								public %1$s(Vm vm, Vm.List l) { %3$s; }
							}
							""".formatted(className, isStdObj ? "Vm.StdObj" : superClass.getCanonicalName(), isStdObj ? "vm.super(l)" : "super(vm, l)")
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
			catch (Throwable t) {
				return error("defining class " + className, t);
			}
		}
	}
	
	
	// Methods
	Map<Class, Map<Symbol,Object>> methods = new LinkedHashMap<>();
	Object addMethod(Class cls, Symbol name, Object method) {
		return methods.computeIfAbsent(cls, k-> new LinkedHashMap<>()).put(name, method);
	}
	Object getMethod(Class cls, Symbol name) {
		do {
			var ms = methods.get(cls); if (ms == null) continue;
			var m = ms.get(name); if (m != null) return m;
		} while ((cls = cls.getSuperclass()) != null);
		return error("method " + name + " not found!");
	}	
	
	
	// Bind
	Object bind(Env e, Dbg dbg, Object lhs, Object rhs) {
		var msg = bind(e, lhs, rhs); if (msg == null) return inert;
		return error(msg + " for bind: " + toString(lhs) + eIfnull(dbg,()-> " of: " + cons(dbg.op, list(dbg.os))) + " with: " + rhs);
	}
	@SuppressWarnings("preview")
	Object bind(Env e, Object lhs, Object rhs) {
		return switch (lhs) {
			case Ignore i-> null;
			case Symbol s-> e.put(s, rhs);  
			case Keyword k-> k.equals(rhs) ? null : "not found keyword: " + k;  
			case null-> rhs == null ? null : "too many operands" /*+ ", none expected, but got: " + toString(rhs)*/;
			case Cons lc-> {
				if (!(rhs instanceof Cons rc)) yield "too few operands" /*+ ", more expected, but got: " + toString(rhs)*/;
				var msg = bind(e, lc.car, rc.car); if (msg != null) yield msg;
				yield bind(e, lc.cdr, rc.cdr);
			}
			default-> error("cannot bind: " + lhs);
		};
	}
	
	
	// Operative & Applicative Combiners
	interface Combinable { <T> T combine(Env e, List o); }
	
	<T> T combine(Env e, Object op, List o) {
		if (prtrc >= 5) print(" combine: ", indent(), op, " ", o /*, "   ", e*/);
		if (op instanceof Combinable cmb) return cmb.combine(e, o);
		// per default le jFun nude sono considerate applicative
		if (isjFun(op)) return (T) new Apv(new JFun(op)).combine(e, o);
		return error("not a combiner: " + toString(op) + " in: " + cons(op, o));
	}
	
	class Opv implements Combinable  {
		Object p, ep; List x; Env e;
		Opv(Object p, Object ep, List x, Env e) { this.p = p; this.ep = ep; this.x = x; this.e = e; }
		public Object combine(Env e, List o) {
			var xe = env(this.e);
			var dbg = dbg(e, this, o);
			return pipe(dbg, ()-> bind(xe, dbg, p, o), $-> bind(xe, dbg, ep, e), $$-> tco(()-> begin.combine(xe, x)));
			/* TODO in alternativa al precedente
			return pipe(dbg, ()-> bind(xe, dbg, p, o), $-> bind(xe, dbg, ep, e),
				$$-> x == null ? inert : tco(()-> x.cdr == null ? evaluate(e, x.car) : begin.begin(null, xe, x))
			);
			//*/
		}
		public String toString() { return "{Opv " + Vm.this.toString(p) + " " + Vm.this.toString(ep) + " " + Vm.this.toString(x) + /*" " + e +*/ "}"; }
	}
	class Apv implements Combinable  {
		Combinable cmb;
		Apv(Combinable cmb) { this.cmb = cmb; }
		public Object combine(Env e, List o) {
			return pipe(dbg(e, this, o), ()-> mapCar(car-> evaluate(e, car), o), args-> tco(()-> cmb.combine(e, (List) args))); 
		}
		public String toString() {
			return "{Apv " + Vm.this.toString(cmb) + "}";
		}
		Combinable unwrap() { return cmb; }
	}
	Object wrap(Object arg) {
		return arg instanceof Apv || isjFun(arg) ? arg 
		: arg instanceof Combinable cmb ? new Apv(cmb)
		: error("cannot wrap: " + arg);
	}
	<T> T unwrap(Object arg) {
		return arg instanceof Apv apv ? (T) apv.cmb
		: isjFun(arg) ? (T) new JFun(arg)
		: error("cannot unwrap: " + arg);
	}
	//Apv lambda(Object p, Object e, List b) { return new Apv(new Opv(p, ignore, b, evaluate(theEnv, e))); }
	
	
	// Built-in Combiners
	class Vau implements Combinable  {
		public Object combine(Env e, List o) {
			checkO(this, o, 2, -1); // o = (pt ep) | (pt ep x ...)
			var pt = o.car();
			var ep = o.car(1);
			var msg = checkPt(pt, ep); if (msg != null) return error(msg + " of: " + cons(this, o));
			return new Opv(pt, ep, o.cdr(1), e);
		}
		public String toString() { return "%Vau"; }
	};
	class Def implements Combinable  {
		public Object combine(Env e, List o) {
			checkO(this, o, 2); // o = (pt arg)
			var pt = o.car();
			if (!(pt instanceof Symbol)) {
				if (!(pt instanceof Cons)) return error("not a symbol or parameter tree: " + pt + " in: " + cons(this, o));
				var msg = checkPt(pt); if (msg != null) return error(msg + " of: " + cons(this, o));
			}
			var dbg = dbg(e, this, o);
			return pipe(dbg, ()-> evaluate(e, o.car(1)), res-> bind(e, dbg, pt, res));
		}
		public String toString() { return "%Def"; }
	};
	class DefStar implements Combinable  {
		public Object combine(Env e, List o) {
			checkO(this, o, 1, -1); // o = (pt arg ...)
			var pt = o.car();
			if (!(pt instanceof Symbol)) {
				if (!(pt instanceof Cons)) return error("not a symbol or parameter tree: " + pt + " in: " + cons(this, o));
				var msg = checkPt(pt); if (msg != null) return error(msg + " of: " + cons(this, o));
			}
			var dbg = dbg(e, this, o);
			return pipe(dbg, ()-> mapCar(car-> evaluate(e, car), o.cdr()), res-> bind(e, dbg, pt, res));
		}
		public String toString() { return "%Def*"; }
	};
	class Eval implements Combinable  {
		public Object combine(Env e, List o) {
			checkO(this, o, 2); // o = (x eo)
			var x = o.car();
			var o1 = o.car(1);
			if (!(o1 instanceof Env eo)) return error("not an Env: " + o1);
			return evaluate(eo, x);
		}
		public String toString() { return "%Eval"; }
	}
	
	
	// First-order Control
	class Begin implements Combinable  {
		boolean root;
		Begin() {}; Begin(boolean root) { this.root = root; } 
		public Object combine(Env e, List o) {
			// o = () | (x ...)
			return o == null ? inert : begin(null, e, o);
		}
		Object begin(Resumption r, Env e, List list) {
			for (var first = true;;) { // only one resume for suspension
				if (prtrc >= 3 && root && r == null) print("\n--------");
				var car = list.car;
				if (prtrc == 2 && root && r == null) print("evaluate: ", car);
				if (list.cdr == null) { return tco(()-> evaluate(e, car)); } 
				var res = first && r != null && !(first = false) ? r.resume() : evaluate(e, car);
				if (res instanceof Suspension s) { var l = list; return s.suspend(dbg(e, "evalBegin", list.car), rr-> begin(rr, e, l)); }
				list = list.cdr();
			}
		}
		public String toString() { return "%Begin" + eIf(!root, "*"); }
	}
	Begin begin = new Begin();
	class If implements Combinable  {
		public Object combine(Env e, List o) {
			checkO(this, o, 2, 3); // o = (test then) | (test then else) 
			var test = o.car();
			return pipe(dbg(e, this, o), ()-> evaluate(e, test), res-> istrue(res)
				? tco(()-> evaluate(e, o.car(1)))
				: o.cdr(1) == null ? inert : tco(()-> evaluate(e, o.car(2)))
				//: o.cdr(1) == null ? inert : tco(()-> begin.combine(e, o.cdr(1)))
			);
		}
		public String toString() { return "%If"; }
	}
	boolean istrue(Object res) {
		return res instanceof Boolean b ? b : error("not a boolean.");
		//return res instanceof Boolean b ? b : res != null; // or #inert or 0 or ""  or [] or ... !?
		// ((rec (for . l) (when l (print (car l)) (apply for (cdr l)))) 1 2 3 4)
	}
	class Loop implements Combinable  {
		public Object combine(Env e, List o) {
			checkO(this, o, 1, -1); // o = (x ...)
			for (;;) getTco(begin.combine(e, o));
		}
		public String toString() { return "%Loop"; }
	}
	class Catch implements Combinable {
		public Object combine(Env e, List o) {
			var l = checkO(this, o, 2, 3); // o = (tag x) | (tag x hdl) -> (tag x (lambda (exc) ... )) 
			var tag = o.car();
			var x = o.car(1);
			var hdl = l == 2 ? null : o.car(2);
			return combine(null, e, tag, x, hdl);
		}
		private Object combine(Resumption r, Env e, Object tag, Object x, Object hdl) {
			Object res = null;
			try {
				if (ctapv && !(x instanceof Apv apv0 && args(apv0) == 0)) return error("not a zero args applicative combiner: " + x);
				res = r != null ? r.resume() : !ctapv ? evaluate(e, x) : getTco(Vm.this.combine(e, x, null));
			}
			catch (Throwable thw) {
				if (tag != ignore && thw instanceof Value val && val.tag != tag) throw thw; 
				res = pipe(dbg(e, this, hdl, thw), ()-> {
						if (hdl == null) {
							if (thw instanceof Value val) return val.value;
							throw thw instanceof Error err ? err : new Error(thw);
						}
						return (ctapv ? hdl : evaluate(e, hdl)) instanceof Apv apv1 && args(apv1) == 1
							? getTco(Vm.this.combine(e, unwrap(apv1), list(thw instanceof Value val ? val.value : thw)))
							: error("not a one arg applicative combiner: " + hdl)
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
			var l = checkO(this, o, 1, 2); // o = (tag) | (tag value)
			var tag = o.car();
			var value = l == 1 ? inert : o.car(1);
			throw new Value(tag, ctapv ? value : pipe(dbg(e, this, tag, value), ()-> evaluate(e, value)));
		}
		public String toString() { return "%Throw"; }
	}
	class Finally implements Combinable {
		public Object combine(Env e, List o) {
			checkO(this, o, 2); // o = (x cln)
			var x = o.car();
			var cln = o.car(1);
			try {
				return pipe(dbg(e, this, x, cln), ()-> evaluate(e, x), res-> cleanup(cln, e, true, res));
			}
			catch (Throwable thw) {
				return cleanup(cln, e, false, thw);
			}
		}
		Object cleanup(Object cln, Env e, boolean success, Object res) {
			return pipe(dbg(e, this, cln, success, res), ()-> evaluate(e, cln), $-> {
					if (success) return res;
					throw res instanceof Value val ? val : res instanceof Error err ? err : new Error((Throwable) res);
				}
			);
		}
		public String toString() { return "%Finally"; }
	}
	
	// Delimited Control
	class TakeSubcont implements Combinable  {
		public Object combine(Env e, List o) {
			checkO(this, o, 2); // o = (prt hdl) -> (prt (lambda (k) ...))
			var prt = o.car();
			var hdl = o.car(1); 
			if (!(hdl instanceof Apv apv1 && args(apv1) == 1)) return error("not a one arg applicative combiner: " + hdl); 
			//return new Suspension(prt, apv1).suspend(rr-> evaluate(e, cons(rr.s, null)), dbg(e, this, o)); // for tco?
			return new Suspension(prt, apv1).suspend(dbg(e, this, o), rr-> Vm.this.combine(e, rr.s, null));
		}
		public String toString() { return "%TakeSubcont"; }
	}
	class PushPrompt implements Combinable  {
		public Object combine(Env e, List o) {
			checkO(this, o, 2); // o = (prt x) | (prt (begin ...))
			var prt = o.car();
			var x = o.car(1);
			return pushPrompt(null, e, dbg(e, this, o), prt, ()-> evaluate(e, x));
		}
		public String toString() { return "%PushPrompt"; }
	}
	class PushPromptSubcont implements Combinable  {
		public Object combine(Env e, List o) {
			checkO(this, o, 3); // o = (prt k apv0) -> (prt k (lambda () ...)
			var prt = o.car();
			var o1 = o.car(1); if (!(o1 instanceof Continuation k)) return error("not a continuation: " + o1); 
			var o2 = o.car(2); if (!(o2 instanceof Apv apv0 && args(apv0) == 0)) return error("not a zero args applicative combiner: " + o2);
			return pushPrompt(null, e, dbg(e, this, o), prt, ()-> k.apply(e, apv0));
		}
		public String toString() { return "%PushPromptSubcont"; }
	}
	Object pushPrompt(Resumption r, Env e, Dbg dbg, Object prt, Supplier action) {
		var res = r != null ? r.resume() : action.get();
		if (!(res instanceof Suspension s)) return res;
		return prt == ignore || !equals(s.prt, prt)
			? s.suspend(dbg, rr-> pushPrompt(rr, e, dbg, prt, action))
			: combine(e, s.hdl, cons(s.k, null))
		;
	}
	Object pushSubcontBarrier(Resumption r, Env e, Object x) {
		var res = r != null ? r.resume() : evaluate(e, x);
		if (!(res instanceof Suspension s)) return res;
		return s.suspend(dbg(e, "pushSubcontBarrier", x), rr-> pushSubcontBarrier(rr, e, x)).k.apply(()-> error("prompt not found: " + s.prt));
	}
	
	
	// Dynamic Variables
	class Box implements ArgsList {
		Object value;
		Box (Object val) { this.value = val; }
		public Object apply(List o) { return checkO(this, o, 0, 1) == 0 ? value : inert(value = o.car()); }
		public String toString() { return "{" + getClass().getSimpleName() + " " + value + "}"; }
	}
	class DVar extends Box { DVar(Object val) { super(val); }}
	class DDef implements Combinable {
		public Object combine(Env e, List o) {
			checkO(this, o, 2, Symbol.class, null); // o = (var val)
			var var = o.car();
			var lookup = e.get(var);
			if (lookup.isBound && !(lookup.value instanceof DVar)) return error("not a dinamic variable: " + var);
			DVar dVar = (DVar) lookup.value;
			return pipe(dbg(e, this, o), ()-> evaluate(e, o.car(1)), val-> {
					if (dVar != null) dVar.value = val; else bind(e, null, var, new DVar(val));
					return inert;
				}
			);
		}
		public String toString() { return "%DDef"; }
	}
	class DDefStar implements Combinable {
		public Object combine(Env e, List o) {
			checkO(this, o, 2, -1); // o = ((var ...) vals ...)
			var vars = array(o.car(), Symbol.class);
			var dVars = new DVar[vars.length];
			for (int i=0; i<vars.length; i+=1) {
				var var = vars[i];
				var lookup = e.get(var);
				if (!lookup.isBound) continue;
				if (!(lookup.value instanceof DVar dVar)) return error("not a dinamic variable: " + var);
				dVars[i] = dVar;
			}
			return pipe(dbg(e, this, o), ()-> mapCar(car-> evaluate(e, car), o.cdr()), args-> {
					var vals = array((List) args);
					if (vars.length != vals.length) return error("not same length: " + vars + " and " + vals);
					for (int i=0; i<dVars.length; i+=1) dSet(dbg(e, this, o), dVars[i], vars[i], vals[i]);
					return inert;
				}
			);
		}
		private void dSet(Dbg dbg, DVar dvar, Symbol var, Object val) {
			if (dvar != null) dvar.value = val; else bind(dbg.e, dbg, var, new DVar(val));
		}
		public String toString() { return "%DDef*"; }
	}
	class DLet implements Combinable {
		public Object combine(Env e, List o) {
			checkO(this, o, 2, -1); // o = (((var ...) val ...) x ...)
			List car = o.car();
			var vars = array(car.car());
			var vals = array(car.cdr());
			if (vars.length != vals.length) return error("not same length: " + vars + " and " + vals);
			var olds = new Object[vals.length];
			for (int i=0; i<vars.length; i+=1) {
				if (!(vars[i] instanceof DVar dvar)) return error("not a dinamic variable: " + vars[i]);
				olds[i] = dvar.value;
				dvar.value = vals[i];
			}
			try {
				List x = o.cdr(); return pipe(dbg(e, this, x), ()-> getTco(begin.combine(e, x)));
			}
			finally {
				for (int i=0; i<vars.length; i+=1) ((DVar) vars[i]).value = olds[i];
			}
		}
		public String toString() { return "%DLet"; }
	}
	
	
	// Java Native Interface
	interface ArgsList extends Function<List,Object> {}
	@SuppressWarnings("preview")
	class JFun implements Combinable {
		String name; ArgsList jfun; 
		JFun(String name, Object jfun) { this(jfun); this.name = name; };
		JFun(Object jfun) {
			this.jfun = switch (jfun) {
				case Supplier s-> o-> { checkO(jfun, o, 0); return s.get(); };  
				case ArgsList a-> a;  
				case Function f-> o-> { checkO(jfun, o, 1); return f.apply(o.car()); };  
				case BiFunction f-> o-> { checkO(jfun, o, 2); return f.apply(o.car(), o.car(1)); };
				case Field f-> o-> uncked(()-> { if (checkO(jfun, o, 1, 2) == 1) return f.get(o.car()); f.set(o.car(), o.car(1)); return inert; });
				case Method mt-> o-> {
					var pc = mt.getParameterCount();
					if (!mt.isVarArgs()) checkO(jfun, o, pc+1); else checkO(jfun, o, pc, -1);
					return uncked(()-> mt.invoke(o.car(), reorg(mt, array(o.cdr()))));
				};
				case Constructor c-> o-> {
					checkO(jfun, o, c.getParameterCount());
					return uncked(()-> c.newInstance(reorg(c, array(o))));
				};
				default -> error("not a combine " + jfun);
			};
		}
		public Object combine(Env e, List o) {
			return pipe(dbg(e, this, o), ()-> {
					try {
						return jfun.apply(o);
					}
					catch (Value | Error exc) {
						throw exc;
					}
					catch (Throwable exc) {
						return error("jfun error: " + exc.getMessage(), exc);
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
	@SuppressWarnings("preview")
	Object jInvoke(String name) {
		if (name == null) return error("method name is null");
		return (ArgsList) o-> {
			if (!(o instanceof List)) return error("no operands for executing: " + name) ;  
			Object o0 = o.car();
			if (o0 == null) return error("receiver is null");
			Object[] args = array(o, 1);
			// (@new class . objects)            -> class.getConstructor(getClasses(objects)).newInstance(objects) -> constructor.newInstance(objects)
			// (@<name> object . objects)        -> object.getClass().getMethod(name, getClasses(objects)).invocke(object, objects) -> method.invoke(object, objects)
			// (@getConstructor class . classes) -> class.getConstructor(classes) -> constructor
			// (@getMethod class name . classes) -> class.getMethod(name, classes) -> method
			// (@getField class name)            -> class.getField(name, classes) -> field
			var classes = getClasses(args);
			Executable executable = getExecutable(name.startsWith("new") ? (Class) o0 : o0.getClass(), name, classes);
			if (executable == null) return error("not found " + executable(name, classes) + " of: " + toString(o0));
			try {
				//executable.setAccessible(true);
				args = reorg(executable, args);
				return switch (executable) { 
					case Method m-> m.invoke(o0, args);
					case Constructor c-> c.newInstance(args);
				};
				/* TODO in alternativa al precedente da verificare
				return apply(v-> isjFun(v) ? wrap(new JFun(name, v)) : v,
					switch (executable) { 
						case Method m-> m.invoke(o0, args);
						case Constructor c-> c.newInstance(args);
					}
				);
				//*/
				/* TODO in alternativa al precedente da verificare
				Object v = switch (executable) { 
					case Method m-> m.invoke(o0, args);
					case Constructor c-> c.newInstance(args);
				};
				return isjFun(v) ? wrap(new JFun(name, v)) : v;
				//*/
			}
			catch (Exception exc) {
				return error("error executing " + executable(name, args) + " of: " + toString(o0) + " with: " + toString(list(args)), exc);
			}
		};
	}
	Object jGetSet(String name) {
		if (name == null) return error("field name is null");
		return (ArgsList) o-> {
			var len = checkO("jGetSet", o, 1, 2); 
			var o0 = o.car();
			// (.<name> object)       -> object.getclass().getField(name).get(object) -> field.get(object) 
			// (.<name> object value) -> object.getClass().getField(name).set(object,value) -> field.set(object, value) 
			Field field = getField(o0 instanceof Class ? (Class) o0 : o0.getClass(), name);
			if (field == null) return error("not found field: " + name + " in: " + toString(o0));
			try {
				if (len == 1) return field.get(o0);
				field.set(o0, o.car(1)); return inert;
			}
			catch (Exception e) {
				return error("can't " + (len==1 ? "get" : "set") + " " + name + " of " + toString(o0) + eIf(len == 1, ()-> " with " + toString(o.car(1))));
			}
		};
	}
	
	
	// Error handling
	Object rootPrompt = new Object() { public String toString() { return "%RootPrompt"; }};
	Object pushRootPrompt(Object x) { return list(new PushPrompt(), rootPrompt, x); }
	<T> T error(String msg) {
		return error(msg, null);
	}
	<T> T error(String msg, Throwable cause) {
		var exc = new Error(msg, cause); 
		var userBreak = theEnv.get("userBreak").value;
		if (userBreak != null) {
			// con l'attuale userBbreak se stack is true viene tornata una sospension per il takeSubcont (o un tco)
			var res = evaluate(theEnv, list(userBreak, exc));
			//var res = pipe(dbg(theEnv, userBreak, exc), ()-> evaluate(theEnv, list(userBreak, exc))); // for ?
			// per l'esecuzione della throw anche se userbreak non lo facesse
			if (res instanceof Suspension s) return (T) s.suspend(dbg(theEnv, "throw", exc), rr-> { throw exc; });
		}
		throw exc;
	}
	static class Error extends RuntimeException {
		private static final long serialVersionUID = 1L;
		public Error(Throwable cause) { super(cause); }
		public Error(String message) { super(message); }
		public Error(String message, Throwable cause) { super(message, cause); }
	}
	class Value extends RuntimeException {
		private static final long serialVersionUID = 1L;
		Object tag, value;
		Value(Object tag, Object value) {
			super(Vm.this.toString(tag) + " " + Vm.this.toString(value)); this.tag = tag; this.value = value;
		}
	}
	class PTree {
		private Object pt, ep;
		private Set syms = new HashSet<Symbol>();
		PTree(Object pt, Object ep) { this.pt = pt; this.ep = ep; }
		Object check() { 
			if (pt != null && pt != ignore) { var msg = check(pt); if (msg != null) return msg; }
			if (ep == null) return syms.size() > 0 ? null : "no one symbol in: " + pt;
			if (ep == ignore) return null;
			if (!(ep instanceof Symbol sym)) return "not a #ignore or symbol: " + ep;
			return !syms.contains(sym) ? null : "not a unique symbol: " + ep;
		}
		private Object check(Object p) {
			if (p == ignore) return null;
			if (p instanceof Keyword) return null;
			if (p instanceof Symbol) { return syms.add(p) ? null : "not a unique symbol: " + p + eIf(p == pt, ()-> " in: " + pt); }
			if (!(p instanceof Cons c)) return "not a #ignore, keyword or symbol: " + p + eIf(p == pt, ()-> " in: " + pt);
			var msg = check(c.car); if (msg != null) return msg;
			return c.cdr == null ? null : check(c.cdr);
		}
	}
	Object checkPt(Object pt) { return checkPt(pt, null); }
	Object checkPt(Object pt, Object ep) { return new PTree(pt, ep).check(); }
	@SuppressWarnings("preview")
	int args(Apv apv) {
		return switch(apv.cmb) {
			case Opv opv-> opv.p == null ? 0 : opv.p instanceof Cons c && c.cdr == null && (c.car == ignore || c.car instanceof Symbol) ? 1 : Integer.MAX_VALUE;
			case JFun jFun-> jFun.jfun instanceof Supplier ? 0 : jFun.jfun instanceof Function ? 1 : Integer.MAX_VALUE;
			default-> Integer.MAX_VALUE;
		};
	}
	int checkO(Object op, List o, int expt, Class ... cls) {
		return checkO(op, o, expt, expt, cls);
	}
	int checkO(Object op, List o, int min, int max, Class ... cls) {
		var len = len(o); if (len >= min && (max == -1 || len <= max)) return cls.length == 0 ? len : checkO(op, o, cls); 
		return error((len < min ? "less then " + min : "more then " + max) + " operands to combine: " + op + " with: " + o);
	}
	int checkO(Object op, List o, Class ... cls) {
		if (o == null) return 0;
		int len=cls.length-1, i=0; for (var oo=o; o != null; i+=1, o=o.cdr()) {
			if (len == -1) continue;
			var cl = cls[i<=len ? i : len];
			var o0 = o.car; if (cl == null || cl.isInstance(o0)) continue;
			return error("not a " + toString(cls[i]) + ": " + o0 + " to combine: " + op + " with: " + oo);
		}
		return i;
	}
	
	
	// Utilities
	<T,R extends Cons> R list(T ... args) {
		return list(true, args);
	}
	<T,R extends Cons> R listStar(T ... args) {
		return list(false, args);
	}
	<T,R extends Cons> R list(boolean b, T ... args) {
		var len = args.length-1;
		var c = b || len < 0 ? null : args[len];
		for (var i=len-(b?0:1); i>=0; i-=1) c = cons(args[i], c);
		return (R) c;
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
	boolean vmAssert(String str, Object objs) throws Exception {
		List expr = cons(begin, parseBytecode(parse(str)));
		return vmAssert.combine(theEnv,  objs instanceof Throwable ? expr : cons(expr, parseBytecode(objs))); 
	}
	class Assert implements Combinable {
		public Boolean combine(Env env, List o) {
			if (!doasrt) return true;
			checkO(this, o, 1, 2); // o = (x)|(x v)
			return test.combine(env, cons(null, o));
		}
		public String toString() { return "Assert"; }
	}
	Assert vmAssert = new Assert();
	class Test implements Combinable {
		public Boolean combine(Env env, List o) {
			if (!doasrt) return true;
			var len = checkO(this, o, 2, 3); // o = (name x)|(name x v)
			var name = eIfnull(o.car(), n-> "test "+ n + ": ");
			var expr = o.car(1);
			try {
				env = env(env);
				var val = pushSubcontBarrier(null, env, pushRootPrompt(expr));
				if (len == 2) print(name, expr, " should throw but is ", val);
				else {
					var expt = o.car(2);
					if (Vm.this.equals(val, pushSubcontBarrier(null, env, pushRootPrompt(expt)))) return true;
					print(name, expr, " should be ", expt, " but is ", val);
				}
			}
			catch (Throwable t) {
				if (len == 2) return true;
				if (prstk) t.printStackTrace(out);
				else print(name, expr, " throw ", t);
			}
			return false;
		}
		public String toString() { return "Test"; }
	}
	Test test = new Test();
	
	
	// Bytecode parser
	Map<String,Intern> interns = new LinkedHashMap<>();
	Intern intern(String name) {
		return interns.computeIfAbsent(name, n-> n.startsWith(":") && n.length() > 1 ? keyword(n) : symbol(n));
	}
	
	Object parseBytecode(Object o) {
		if (o instanceof String s) return switch(s) { case "#inert"-> inert; case "#ignore"-> ignore; default-> intern(instr ? s.intern() : s); };
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
	@SuppressWarnings("preview")
	String toString(boolean t, Object o) {
		return switch (o) {
			case null-> "()"; // () in cons  altrove #null
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
	private String executable(String name, Object[] args) {
		return (name.equals("new") ? "constructor" : "method: " + name) + toString(list(args));
	}
	
	
	// Bootstrap
	Env theEnv=env(null); {
		bind(theEnv, null, symbol("%def"), new Def());
		bind(theEnv, null, symbol("%begin"), begin);
		evaluate(theEnv,
			parseBytecode(
				$("%begin",
					// Basics
					$("%def", "%vau", new Vau()),
					$("%def", "%def*", new DefStar()),
					$("%def", "%eval", wrap(new Eval())),
					$("%def", "%makeEnv", wrap(new JFun("%MakeEnv", (ArgsList) o-> env(checkO("env", o, 0, 1, Env.class) == 0 ? null : o.car())))),
					$("%def", "%wrap", wrap(new JFun("%Wrap", (Function<Object, Object>) t-> wrap(t)))),
					$("%def", "%unwrap", wrap(new JFun("%Unwrap", (Function<Object, Object>) t-> unwrap(t)))),
					$("%def", "%bound?", wrap(new JFun("%Bound?", (BiFunction<Symbol,Env,Boolean>) (s, e)-> e.get(s).isBound))),
					$("%def", "%bind", wrap(new JFun("%Bind", (ArgsList) o-> bind((Env) o.car(), o.car(1), o.car(2)) == null))),
					$("%def", "%apply", wrap(new JFun("%Apply", (ArgsList) o-> combine(o.cdr(1) != null ? o.car(2) : env(null), unwrap(o.car()), o.car(1))))),
					$("%def", "%apply*", wrap(new JFun("%Apply*", (ArgsList) o-> combine(env(null), unwrap(o.car()), o.cdr())))),
					$("%def", "%resetEnv", wrap(new JFun("%ResetEnv", (Supplier) ()-> { theEnv.map.clear(); return theEnv; }))),
					$("%def", "%pushEnv", wrap(new JFun("%PushEnv", (Supplier) ()-> theEnv = env(theEnv)))),
					$("%def", "%popEnv", wrap(new JFun("%PopEnv", (Supplier) ()-> theEnv = theEnv.parent))),
					// Values
					$("%def", "%car", wrap(new JFun("%Car", (Function<Cons, Object>) Cons::car))),
					$("%def", "%cdr", wrap(new JFun("%Car", (Function<Cons, Object>) Cons::cdr))),
					$("%def", "%cadr", wrap(new JFun("%Car", (Function<Cons, Object>) c-> c.car(1)))),
					$("%def", "%cons", wrap(new JFun("%Cons", (BiFunction<Object, Object, Object>) (car,cdr)-> cons(car,cdr)))),
					$("%def", "%cons?", wrap(new JFun("%Cons?", (Function<Object, Boolean>) obj-> obj instanceof Cons))),
					$("%def", "%null?", wrap(new JFun("%Null?", (Function<Object, Boolean>) obj-> obj == null))),
					$("%def", "%string->symbol", wrap(new JFun("%String->symbol", (Function<String, Symbol>) this::symbol))),
					$("%def", "%symbol?", wrap(new JFun("%Symbol?", (Function<Object, Boolean>) obj-> obj instanceof Symbol))),
					$("%def", "%keyword?", wrap(new JFun("%Keyword?", (Function<Object, Boolean>) obj-> obj instanceof Keyword))),
					$("%def", "%intern", wrap(new JFun("%Intern", (Function<String, Intern>) s-> intern(s)))),
					$("%def", "%internName", wrap(new JFun("%InternName", (Function<Intern, String>) i-> i.name))),
					// First-order Control
					$("%def", "%if", new If()),
					$("%def", "%loop", new Loop()),
					$("%def", "%catch", apply(c-> !ctapv ? c : wrap(c), new Catch())),
					$("%def", "%throw", apply(t-> !ctapv ? t : wrap(t), new Throw())),
					$("%def", "%finally", new Finally()),
					// Delimited Control
					$("%def", "%takeSubcont", wrap(new TakeSubcont())),
					$("%def", "%pushPrompt", new PushPrompt()),
					$("%def", "%pushPromptSubcont", wrap(new PushPromptSubcont())),
					$("%def", "%pushSubcontBarrier", wrap(new JFun("%PushSubcontBarrier", (BiFunction<Object,Env,Object>) (o, e)-> pushSubcontBarrier(null, e, o)))),
					// Dynamically-scoped Variables
					$("%def", "%box", wrap(new JFun("%Box", (Function<Object,Box>) Box::new))),
					$("%def", "%dVar", wrap(new JFun("%DVar", (Function<Object,DVar>) DVar::new))),
					$("%def", "%dVal", wrap(new JFun("%DVal", (ArgsList) o-> { DVar dv = o.car(); return checkO("%DVal", o, 1, 2) == 1 ? dv.value : (dv.value=o.car(1)); }))),
					$("%def", "%dDef", new DDef()),
					$("%def", "%dDef*", new DDefStar()),
					$("%def", "%dLet", new DLet()),
					// Errors
					$("%def", "%rootPrompt", rootPrompt),
					$("%def", "%error", wrap(new JFun("%Error", (Function<String, Object>) this::error))),
					// Java Interface
					$("%def", "%jFun?", wrap(new JFun("%JFun?", (Function<Object,Boolean>) this::isjFun))),
					$("%def", "%jInvoke", wrap(new JFun("%JInvoke", (Function<String,Object>) this::jInvoke))),
					$("%def", "%jGetSet", wrap(new JFun("%JGetSet", (Function<String,Object>) this::jGetSet))),
					$("%def", "%instanceof?", wrap(new JFun("%Instanceof?", (BiFunction<Object,Class,Boolean>) (o,c)-> c.isInstance(o)))),
					// Object System
					$("%def", "%addMethod", wrap(new JFun("%AddMethod", (ArgsList) o-> addMethod(o.car(), o.car(1), o.car(2))))),
					$("%def", "%getMethod", wrap(new JFun("%GetMethod", (BiFunction<Class,Symbol,Object>) this::getMethod))),
					$("%def", "%obj", wrap(new JFun("%Obj", (ArgsList) o-> uncked(()-> ((Class<? extends StdObj>) o.car()).getDeclaredConstructor(List.class).newInstance(o.cdr()))))),
					$("%def", "%obj", wrap(new JFun("%Obj", (ArgsList) o-> uncked(()-> ((Class<? extends StdObj>) o.car()).getDeclaredConstructor(Vm.class, List.class).newInstance(Vm.this, o.cdr()))))),
					$("%def", "%class", wrap(new JFun("%Class", (ArgsList) o-> extend(o.car(), apply(cdr-> cdr == null ? null : cdr.car(), o.cdr()))))),
					$("%def", "%subClass?", wrap(new JFun("%SubClass", (BiFunction<Class,Class,Boolean>) (cl,sc)-> sc.isAssignableFrom(cl)))),
					$("%def", "%type?",  wrap(new JFun("%Type?", (BiFunction<Object,Class,Boolean>) (o,c)-> o == null ? c == null : c.isAssignableFrom(o.getClass())))),
					$("%def", "%classOf", wrap(new JFun("%ClassOf", (Function<Object,Class>) Object::getClass))),
					// Utilities
					$("%def", "%list", wrap(new JFun("%List", (ArgsList) o-> o))),
					$("%def", "%list*", wrap(new JFun("%List*", (ArgsList) this::listStar))),
					$("%def", "%len", wrap(new JFun("%Len", (Function<List,Integer>) this::len))),
					$("%def", "%list->array", wrap(new JFun("%List->array", (Function<List,Object[]>) this::array))),
					$("%def", "%array->list", wrap(new JFun("%Array->list", (BiFunction<Boolean,Object[],List>) this::list))),
					$("%def", "%reverse", wrap(new JFun("%Reverse", (Function<List,List>) this::reverse))),
					$("%def", "%append", wrap(new JFun("%Append", (BiFunction<List,Object,Object>) this::append))),
					// 
					$("%def", "%$", wrap(new JFun("%$", (BiFunction<Object,Object,String>) (a,b)-> Vm.this.toString(a) + Vm.this.toString(b)))),
					$("%def", "%+", wrap(new JFun("%+", (BiFunction<Number,Number,Object>) (a,b)-> binOp(Pls, a, b)))),
					$("%def", "%*", wrap(new JFun("%*", (BiFunction<Number,Number,Object>) (a,b)-> binOp(Pwr, a, b)))),
					$("%def", "%-", wrap(new JFun("%-", (BiFunction<Number,Number,Object>) (a,b)-> binOp(Mns, a, b)))),
					$("%def", "%/", wrap(new JFun("%/", (BiFunction<Number,Number,Object>) (a,b)-> binOp(Dvd, a, b)))),
					$("%def", "%%", wrap(new JFun("%%", (BiFunction<Number,Number,Object>) (a,b)-> binOp(Rst, a, b)))),
					//
					$("%def", "%!", wrap(new JFun("%!", (Function<Boolean,Boolean>) a-> !a))),
					//$("%def", "%!", wrap(new JFun("%!", (Function<Object,Boolean>) a-> !istrue(a)))),
					$("%def", "%<", wrap(new JFun("%<", (BiFunction<Number,Number,Object>) (a,b)-> binOp(Ls, a, b)))),
					$("%def", "%>", wrap(new JFun("%>", (BiFunction<Number,Number,Object>) (a,b)-> binOp(Gt, a, b)))),
					$("%def", "%<=", wrap(new JFun("%<=", (BiFunction<Number,Number,Object>) (a,b)-> binOp(Le, a, b)))),
					$("%def", "%>=", wrap(new JFun("%>=", (BiFunction<Number,Number,Object>) (a,b)-> binOp(Ge, a, b)))),
					//
					$("%def", "%~", wrap(new JFun("%~", (Function<Integer,Integer>) a-> ~a))),
					$("%def", "%&", wrap(new JFun("%&", (BiFunction<Number,Number,Object>) (a,b)-> binOp(And, a, b)))),
					$("%def", "%|", wrap(new JFun("%|", (BiFunction<Number,Number,Object>) (a,b)-> binOp(Or, a, b)))),
					$("%def", "%^", wrap(new JFun("%^", (BiFunction<Number,Number,Object>) (a,b)-> binOp(Xor, a, b)))),
					$("%def", "%<<", wrap(new JFun("%<<", (BiFunction<Number,Number,Object>) (a,b)-> binOp(Sl, a, b)))),
					$("%def", "%>>", wrap(new JFun("%>>", (BiFunction<Number,Number,Object>) (a,b)-> binOp(Sr, a, b)))),
					$("%def", "%>>>", wrap(new JFun("%>>>", (BiFunction<Number,Number,Object>) (a,b)-> binOp(Sr0, a, b)))),
					//
					$("%def", "%==", wrap(new JFun("%==", (BiFunction<Object,Object,Boolean>) (a,b)-> a == b))),
					$("%def", "%!=", wrap(new JFun("%!=", (BiFunction<Object,Object,Boolean>) (a,b)-> a != b))),
					$("%def", "%eq?", wrap(new JFun("%Eq?", (BiFunction<Object,Object,Boolean>) (a,b)-> Vm.this.equals(a, b)))),
					//
					$("%def", "%quote", $("%vau", $("arg"), ignore, "arg")),
					$("%def", "%theEnv", $("%vau", null, "env", "env")),
					$("%def", "%lambda", $("%vau", $("formals", ".", "body"), "env",
						$("%wrap", $("%eval", $("%list*", "%vau", "formals", ignore, "body"), "env")))),
					//$("%def", "%jambda", jFun((ArgsList) o-> lambda(o.car(), o.car(1), o.cdr(1)))),
					//
					$("%def", "vm", this),
					$("%def", "test", test),
					$("%def", "assert", vmAssert),
					$("%def", "toString", wrap(new JFun("ToString", (Function<Object,String>) obj-> toString(obj)))),
					$("%def", "log", wrap(new JFun("Log", (ArgsList) o-> log(array(o))))),
					$("%def", "print", wrap(new JFun("Print", (ArgsList) o-> print(array(o))))),
					$("%def", "write", wrap(new JFun("Write", (ArgsList) o-> write(array(o))))),
					$("%def", "load", wrap(new JFun("Load", (Function<String, Object>) nf-> uncked(()-> loadText(nf))))),
					$("%def", "dotco", wrap(new JFun("Dotco", (ArgsList) o-> { return checkO("dotco", o, 0, 1, Boolean.class) == 0 ? dotco : inert(dotco=o.car()); }))),
					$("%def", "doasrt",  wrap(new JFun("Doasrt", (ArgsList) o-> { return checkO("doasrt", o, 0, 1, Boolean.class) == 0 ? doasrt : inert(doasrt=o.car()); }))),
					$("%def", "prtrc", wrap(new JFun("Prtrc", (ArgsList) o-> { if (checkO("prtrc", o, 0, 1, Integer.class) == 0) return prtrc; start=level-3; return inert(prtrc=o.car()); }))),
					$("%def", "prenv", wrap(new JFun("Prenv", (ArgsList) o-> { return checkO("prenv", o, 0, 1, Integer.class) == 0 ? prenv : inert(prenv=o.car()); }))),
					$("%def", "prstk", wrap(new JFun("Prstk", (ArgsList) o-> { return checkO("prstk", o, 0, 1, Boolean.class) == 0 ? prstk : inert(prstk=o.car()); })))
				)
			)
		);
		theEnv = env(theEnv);
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
	@SuppressWarnings("preview")
	public void repl() throws Exception {
		loop: for (;;) {
			switch (read()) {
				case "" : break loop;
				case String exp: try {
					print(exec(parse(exp)));
				}
				catch (Throwable t) {
					if (prstk) t.printStackTrace(out);
					else out.println("{" + t.getClass().getSimpleName() + " " + t.getMessage() + "}");
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
		///*
		var milli = currentTimeMillis();
		loadText("testVm.lsp");
		loadText("boot.lsp");
		loadText("test.lsp");
		loadText("testJni.lsp");
		loadText("testOos.lsp");
		print("start time: " + (currentTimeMillis() - milli));
		repl();
		//*/
		//extend2(symbol("StdObj2"), null);
	}
}
