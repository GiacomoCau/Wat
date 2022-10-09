package Wat;

import static List.Parser.parse;
import static Wat.Utility.$;
import static Wat.Utility.binOp;
import static Wat.Utility.eIf;
import static Wat.Utility.eIfnull;
import static Wat.Utility.getClasses;
import static Wat.Utility.getExecutable;
import static Wat.Utility.getField;
import static Wat.Utility.isInstance;
import static Wat.Utility.reorg;
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
import static java.lang.System.in;
import static java.lang.System.out;
import static java.lang.reflect.Array.newInstance;
import static java.util.Arrays.stream;
import static java.util.stream.Collectors.joining;

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
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.function.BiFunction;
import java.util.function.BinaryOperator;
import java.util.function.Function;
import java.util.function.Supplier;

/* Abbreviations:
	c: cons
	x: expression
	xs: expressions
	op: operator
	o, os: operands
	o0, o1, ..: operand 0, 1, .. 
	cmb: combiner
	opv: operative combiner
	apv: applicative combiner
	apv0, thunk: zero args applicative combiner
	apv1, handler: one arg applicative combiner
	p: parameter
	ps: parameters
	pt: parameters tree
	arg: argument
	args: arguments
	e: environment
	eo: environment operand 
	ep: environment parameter
	xe: extended environment
	k, next: continuation
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
	cmt: comment
	dbg: debugging information
 */

public class Vm {
	
	boolean dotco = true;
	boolean jfopv = true;
	
	boolean trace = false;
	boolean stack = false;
	boolean prenv = false;
	
	interface Combinable { <T> T combine(Env e, List o); }
	
	
	// Continuations
	class Continuation {
		Function<Resumption, Object> f; Continuation next; Object dbg;
		Continuation(Dbg dbg, Function<Resumption, Object> f, Continuation next) {
			this.f = f; this.next = next; this.dbg = dbg;
		}
		public String toString() { return "{Continuation %s}".formatted(dbg); }
		//Object apply(Env e, Apv apv0) { return apply(()-> evaluate(e, cons(apv0, null))); } // for tco?
		Object apply(Env e, Apv apv0) { return apply(()-> combine(e, apv0, null)); }
		Object apply(Supplier s) { return f.apply(new Resumption(next, s));}
	}
	class Resumption {
		Continuation k; Supplier<Object> s;
		Resumption(Continuation k, Supplier<Object> s) { this.k=k; this.s=s; }
		public String toString() { return "{Resumption %s %s}".formatted(s, k); }
		<T> T resume() { var k = this.k; this.k = k.next; return getTco(k.f.apply(this)); }
	};
	class Suspension {
		Object prompt; Combinable handler; Continuation k;
		Suspension(Object prompt, Combinable handler) { this.prompt = prompt; this.handler = handler; }
		public String toString() { return "{Suspension %s %s %s}".formatted(prompt, handler, k); }
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
		for (; i < after.length; i+=1) {
			res = r != null ? r.resume() : after[i].apply(res);
			if (res instanceof Suspension s) { var ii=i; var rres=res; return s.suspend(dbg, rr-> pipe(rr, ii, dbg, rres, after)); }
		}
		return res;
	}
	Object mapCar(Function f, List todo) {
		return mapCar(null, null, f, todo);
	}
	Object mapCar(Resumption r, List done, Function f, List todo) {
		var first = true;
		for (;;) {
			if (todo == null) return reverseList(done); 
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
	
	class Ignore { public String toString() { return "#ignore"; }};
	public Ignore ignore = new Ignore();
	
	
	// Tail Call Optimization
	interface Tco extends Supplier {};
	Object tco(Tco tco) { return dotco ? tco : tco.get(); }
	<T> T getTco(Object o) { while (o instanceof Tco tco) o = tco.get(); return (T) o; }
	
	
	// Evaluation Core
	@SuppressWarnings("preview")
	<T> T evaluate(Env e, Object o) {
		if (trace) print("evaluate: ", o);
		return getTco(
			switch (o) {
				case null, default-> o;
				case Symbol s-> pipe(dbg(e, o), ()-> e.lookup(s.name));
				case List c-> {
					var ee=e; yield pipe(dbg(e, o), ()-> evaluate(ee, c.car), op-> combine(ee, op, c.cdr()));
				}
			}
		);
	}
	
	class Keyword {
		String name;
		Keyword(String name) { this.name = name; }
		public String toString() { return name; }
		public int hashCode() { return Objects.hashCode(name); }
		public boolean equals(Object o) {
			return this == o || o instanceof Keyword sym && name.equals(sym.name);
		}		
	}
	Keyword keyword(String name) { return new Keyword(name); }
	
	class Symbol {
		String name;
		Symbol(String name) { this.name = name; }
		public String toString() { return name; }
		public int hashCode() { return Objects.hashCode(name); }
		public boolean equals(Object o) {
			return this == o || o instanceof Symbol sym && name.equals(sym.name);
		}
	}
	Symbol symbol(String name) { return new Symbol(name); }
	
	class Cons {
		Object car, cdr;
		Cons(Object car, Object cdr) { this.car = car; this.cdr = cdr; }
		public String toString() { return "(" + toString(this) + ")"; }
		private String toString(Cons c) {
			if (c.cdr == null) return null2brkt(c.car);
			if (c.cdr instanceof Cons cc) return null2brkt(c.car) + " " + toString(cc);
			return null2brkt(c.car) + " . " + Vm.this.toString(true, c.cdr);
		}
		private String null2brkt(Object o) { return o == null ? "()" : Vm.this.toString(true, o); }
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
	class List extends Cons {
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
	class Env {
		Map<String,Object> map = new LinkedHashMap(); Env parent;
		Env(Env parent) { this.parent = parent; }
		record Lookup(boolean isBound, Object value) {}
		Lookup get(String name) {
			Env env = this;	do {
				Object res = env.map.get(name);
				if (res != null || env.map.containsKey(name)) return new Lookup(true, res);
			} while ((env = env.parent) != null);
			return new Lookup(false, null);
		};
		Object bind(String name, Object rhs) {
			if (trace) print("    bind: ", name, "=", rhs, " in: ", this); map.put(name, rhs); return null; 
		}
		public String toString() {
			var isThenv = this == theEnvironment;
			return "[" + eIf(!isThenv, "The-") + "Env" + eIf(isThenv && !prenv, ()-> mapReverse()) + eIf(parent == null, ()-> " " + parent) + "]";
		}
		String mapReverse() {
			var sb = new StringBuilder(); map.entrySet().forEach(e-> sb.insert(0, " " + e)); return sb.toString();
		}
		Object lookup(String name) {
			var lookup = get(name); if (!lookup.isBound) return error("unbound: " + name);
			if (trace) print("  lookup: ", lookup.value); return lookup.value;
		}
		boolean isBound(String name) { return get(name).isBound; }
	}
	Env env(Env parent) { return new Env(parent); }
	
	
	// Bind
	Object bind(Env e, Dbg dbg, Object lhs, Object rhs) {
		var msg = bind(e, lhs, rhs); if (msg == null) return inert;
		return error(msg + " for bind: " + toString(lhs) + eIfnull(dbg,()-> " of: " + cons(dbg.op, dbg.os)) + " with: " + rhs);
	}
	@SuppressWarnings("preview")
	Object bind(Env e, Object lhs, Object rhs) {
		return switch (lhs) {
			case Ignore i-> null;
			case Symbol s-> e.bind(s.name, rhs);  
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
	<T> T combine(Env e, Object op, List o) {
		if (trace) print(" combine: ", op, " ", o);
		if (op instanceof Combinable cmb) return cmb.combine(e, o);
		// per default le jFun dovrebbero essere operative e non applicative
		if (isjFun(op)) return jfopv
			? ((Combinable) jFun(op)).combine(e, o) // jfun x default operative
			: ((Combinable) jWrap(op)).combine(e, o) // jfun x default applicative
		;
		return error("not a combiner: " + toString(op) + " in: " + cons(op, o));
	}
	
	class Opv implements Combinable  {
		Object p, ep; List x; Env e;
		Opv(Object p, Object ep, List x, Env e) { this.p = p; this.ep = ep; this.x = x; this.e = e; }
		public Object combine(Env e, List o) {
			var xe = env(this.e);
			var dbg = dbg(e, this, o);
			return pipe(dbg, ()-> bind(xe, dbg, p, o), $-> bind(xe, dbg, ep, e), $$-> tco(()-> begin.combine(xe, x)));
		}
		public String toString() { return "{Opv " + Vm.this.toString(p) + " " + Vm.this.toString(ep) + " " + Vm.this.toString(x) + "}"; }
	}
	class Apv implements Combinable  {
		Combinable cmb;
		Apv(Combinable cmb) { this.cmb = cmb; }
		public Object combine(Env e, List o) {
			return pipe(dbg(e, this, o), ()-> mapCar(car-> evaluate(e, car), o),args-> tco(()-> cmb.combine(e, (List) args))); 
		}
		public String toString() { return "{Apv " + Vm.this.toString(cmb) + "}"; }
		Combinable unwrap() { return cmb; }
	}
	Apv wrap(Object arg) { return arg instanceof Apv apv ? apv : arg instanceof Combinable cmb ? new Apv(cmb) : error("cannot wrap: " + arg); }
	<T> T unwrap(Object arg) { return arg instanceof Apv apv ? (T) apv.cmb : error("cannot unwrap: " + arg); }
	Apv lambda(Object p, Object e, List b) { return new Apv(new Opv(p, ignore, b, evaluate(theEnvironment, e))); }
	
	
	// Built-in Combiners
	class Vau implements Combinable  {
		public Object combine(Env e, List o) {
			checkO(this, o, 2, -1); // o = (pt ep x ...)
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
				if (!(pt instanceof Cons)) return error("not a symbol: " + pt + " in: " + cons(this, o));
				var msg = checkPt(pt); if (msg != null) return error(msg + " of: " + cons(this, o));
			}
			var arg = o.car(1);
			var dbg = dbg(e, this, o);
			return pipe(dbg, ()-> evaluate(e, arg), res-> bind(e, dbg, pt, res));
		}
		public String toString() { return "%Def"; }
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
			// o = (... xs)
			return o != null ? begin(null, e, o) : inert;
		}
		Object begin(Resumption r, Env e, List list) {
			if (trace && root && r == null) print("\n--------");
			var first = true;
			for (;;) {
				if (!(list.cdr instanceof List cdr)) { var l = list; return tco(()-> evaluate(e, l.car)); } 
				var res = first && r != null && !(first = false) ? r.resume() : evaluate(e, list.car);
				if (res instanceof Suspension s) { var l = list; return s.suspend(dbg(e, "evalBegin", list.car), rr-> begin(rr, e, l)); }
				list = cdr;
			}
		}
		public String toString() { return "%Begin" + eIf(!root, "*"); }
	}
	Begin begin = new Begin();
	class If implements Combinable  {
		public Object combine(Env e, List o) {
			checkO(this, o, 2, 3); // o = (test then else) 
			var test = o.car();
			return pipe(dbg(e, this, o), ()-> evaluate(e, test), res-> istrue(res)
				? tco(()-> evaluate(e, o.car(1)))
				: o.cdr(1) == null ? inert : tco(()-> evaluate(e, o.car(2))))
			;
		}
		private boolean istrue(Object res) {
			return res != null && res instanceof Boolean b && b;
		}
		public String toString() { return "%If"; }
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
			var l = checkO(this, o, 2, 3); // o = (tag x) (tag x handler)
			var tag = o.car();
			var x = o.car(1);
			var handler = l == 3 ? o.car(2) : null;
			return combine(null, e, tag, x, handler);
		}
		private Object combine(Resumption r, Env e, Object tag, Object x, Object handler) {
			Object res = null;
			try {
				res = r != null ? r.resume() : evaluate(e, x);
			}
			catch (Value exc) {
				if (tag != ignore && !Vm.this.equals(exc.tag, tag)) throw exc; 
				res = getValue(e, handler, exc);
			}
			return res instanceof Suspension s ? s.suspend(dbg(e, this, tag, x, handler), rr-> combine(rr, e, tag, x, handler)) : res;
		}
		private Object getValue(Env e, Object handler, Value exc) {
			if (handler == null) return exc.value;
			handler = evaluate(e, handler);
			if (!(handler instanceof Apv apv1 && args(apv1) == 1)) return error("not a one arg applicative combiner: " + handler); 
			// unwrap handler to prevent eval if exc is sym or cons
			return evaluate(e, list(unwrap(apv1), exc.value));
		}
		public String toString() { return "%Catch"; }
	}
	class Throw implements Combinable {
		public Object combine(Env e, List o) {
			var l = checkO(this, o, 1, 2); // o = (tag) (tag value)
			var tag = o.car();
			var value = l == 1 ? inert : o.car(1);
			return pipe(dbg(e, this, o), ()-> evaluate(e, value), res-> { throw new Value(tag, res); });
		}
		public String toString() { return "%Throw"; }
	}
	class Finally implements Combinable {
		public Object combine(Env e, List o) {
			checkO(this, o, 2); // o = (x cleanup)
			var x = o.car();
			var cleanup = o.car(1);
			try {
				return pipe(dbg(e, this, o), ()-> evaluate(e, x), res-> cleanup(null, cleanup, e, res));
			}
			catch (Throwable t) {
				return cleanup(null, cleanup, e, t);
			}
		}
		Object cleanup(Resumption r, Object cleanup, Env e, Object value) {
			return pipe(dbg(e, this, value), ()-> evaluate(e, cleanup),
				$-> {
					if (!(value instanceof Throwable t)) return value;
					throw t instanceof Value v ? v : t instanceof Error err ? err : new Error(t);
				}
			);
		}
		public String toString() { return "%Finally"; }
	}
	
	// Delimited Control
	class TakeSubcont implements Combinable  {
		public Object combine(Env e, List o) {
			checkO(this, o, 2); // o = (prompt handler) ((prompt (lambda (k) ...))
			var prompt = o.car();
			var handler = o.car(1); 
			if (!(handler instanceof Apv apv1 && args(apv1) == 1)) return error("not a one arg applicative combiner: " + handler); 
			//return new Suspension(prompt, apv1).suspend(rr-> evaluate(e, cons(rr.s, null)), dbg(e, this, o)); // for tco?
			return new Suspension(prompt, apv1).suspend(dbg(e, this, o), rr-> Vm.this.combine(e, rr.s, null));
		}
		public String toString() { return "%TakeSubcont"; }
	}
	class PushPrompt implements Combinable  {
		public Object combine(Env e, List o) {
			checkO(this, o, 2); // o = (prompt x) (prompt (begin ...))
			var prompt = o.car();
			var x = o.car(1);
			return pushPrompt(null, e, dbg(e, this, o), prompt, ()-> evaluate(e, x));
		}
		public String toString() { return "%PushPrompt"; }
	}
	class PushPromptSubcont implements Combinable  {
		public Object combine(Env e, List o) {
			checkO(this, o, 3); // o = (prompt k apv0) (prompt k (lambda () ...)
			var prompt = o.car();
			var o1 = o.car(1); if (!(o1 instanceof Continuation k)) return error("not a continuation: " + o1); 
			var o2 = o.car(2); if (!(o2 instanceof Apv apv0 && args(apv0) == 0)) return error("not a zero args applicative combiner: " + o2);
			//return pushPrompt(null, e, dbg(e, this, o), prompt, ()-> k.apply(()-> evaluate(e, cons(apv0, null)))); // for tco?
			//return pushPrompt(null, e, dbg(e, this, o), prompt, ()-> k.apply(()-> Vm.this.combine(e, apv0, null)));
			return pushPrompt(null, e, dbg(e, this, o), prompt, ()-> k.apply(e, apv0));
		}
		public String toString() { return "%PushPromptSubcont"; }
	}
	Object pushPrompt(Resumption r, Env e, Dbg dbg, Object prompt, Supplier action) {
		var res = r != null ? r.resume() : action.get();
		if (!(res instanceof Suspension s)) return res;
		return prompt == ignore || !equals(s.prompt, prompt)
			? s.suspend(dbg, rr-> pushPrompt(rr, e, dbg, prompt, action))
			: combine(e, s.handler, cons(s.k, null))
		;
	}
	Object pushSubcontBarrier(Resumption r, Env e, Object x) {
		var res = r != null ? r.resume() : evaluate(e, x);
		if (!(res instanceof Suspension s)) return res;
		return s.suspend(dbg(e, "pushSubcontBarrier", x), rr-> pushSubcontBarrier(rr, e, x)).k.apply(()-> error("prompt not found: " + s.prompt));
	}
	
	
	// Dynamic Variables
	class DVar {
		Object val;
		DVar(Object val) { this.val = val; }
		public String toString() { return "{DVar " + val + "}"; }
	}
	class DDef implements Combinable {
		public Object combine(Env e, List o) {
			checkO(this, o, 2, -1); // o = (vars ... vals)
			var vars = listToArray(o.car(), Symbol.class);
			var dVars = new DVar[vars.length];
			for (int i=0; i<vars.length; i+=1) {
				var var = vars[i];
				var lookup = e.get(var.name);
				if (!lookup.isBound) continue;
				if (!(lookup.value instanceof DVar dVar)) return error("not a dinamic variable: " + var);
				dVars[i] = dVar;
			}
			return pipe(dbg(e, this, o), ()-> mapCar(car-> evaluate(e, car), o.cdr()), args-> {
					var vals = listToArray((List) args);
					if (vars.length != vals.length) return error("not same length: " + vars + " and " + vals);
					for (int i=0; i<dVars.length; i+=1) dSet(dbg(e, this, o), dVars[i], vars[i], vals[i]);
					return inert;
				}
			);
		}
		private void dSet(Dbg dbg, DVar dvar, Symbol var, Object val) {
			if (dvar != null) dvar.val = val; else bind(dbg.e, dbg, var, new DVar(val));
		}
		public String toString() { return "%DDef"; }
	}
	class DLet implements Combinable {
		public Object combine(Env e, List o) {
			checkO(this, o, 3, -1); // o = (vars vals ... x)
			var vars = listToArray(o.car());
			var vals = listToArray(o.car(1));
			if (vars.length != vals.length) return error("not same length: " + vars + " and " + vals);
			var olds = new Object[vals.length];
			for (int i=0; i<vars.length; i+=1) {
				if (!(vars[i] instanceof DVar dvar)) return error("not a dinamic variable: " + vars[i]);
				olds[i] = dvar.val;
				dvar.val = vals[i];
			}
			try {
				List x = o.cdr(1); return pipe(dbg(e, this, x), ()-> getTco(begin.combine(e, x)));
			}
			finally {
				for (int i=0; i<vars.length; i+=1) ((DVar) vars[i]).val = olds[i];
			}
		}
		public String toString() { return "%DLet"; }
	}
	
	
	// Error handling
	Object rootPrompt = new Object() { public String toString() { return "%rootPrompt"; }};
	Object pushRootPrompt(Object x) { return list(new PushPrompt(), rootPrompt, x); }
	<T> T error(String msg) {
		return error(msg, null);
	}
	<T> T error(String msg, Throwable cause) {
		var exc = new Error(msg, cause); 
		var userBreak = theEnvironment.get("userBreak").value;
		if (userBreak != null) {
			// con l'attuale userBbreak se stack is true viene tornata una sospension per il takeSubcont (o un tco)
			var res = evaluate(theEnvironment, list(userBreak, exc));
			//var res = pipe(dbg(theEnvironment, userBreak, exc), ()-> evaluate(theEnvironment, list(userBreak, exc))); // for ?
			// per l'esecuzione della throw anche se userbreak non lo facesse
			if (res instanceof Suspension s) return (T) s.suspend(dbg(theEnvironment, "throw", exc), rr-> { throw exc; });
		}
		throw exc;
	}
	class Error extends RuntimeException {
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
			if (p instanceof Symbol) { return syms.add(p) ? null : "not a unique symbol: " + p + eIf(p == pt, ()-> " in: " + pt); }
			if (!(p instanceof Cons c)) return "not a #ignore or symbol: " + p + eIf(p == pt, ()-> " in: " + pt);
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
	<T extends Cons> T list(Object ... args) {
		return arrayToList(true, args);
	}
	<T extends Cons> T listStar(Object ... args) {
		return arrayToList(false, args);
	}
	<T extends Cons> T arrayToList(boolean b, Object ... args) {
		var len = args.length-1;
		var c = b || len < 0 ? null : args[len];
		for (var i=len-(b?0:1); i>=0; i-=1) c = cons(args[i], c);
		return (T) c;
	}
	Object[] listToArray(List c) {
		return listToArray(c, 0);
	}
	Object[] listToArray(List c, int i) {
		return listToArray(c, i, Object.class);
	}
	<T> T[] listToArray(List c, Class<T> cl) {
		return (T[]) listToArray(c, 0, cl);
	}
	<T> T[] listToArray(List o, int i, Class<T> cl) {
		var res = new ArrayList<T>();
		for (; o != null; o = o.cdr()) if (i-- <= 0) res.add(o.car());
		return res.toArray((T[]) Array.newInstance(cl, 0));
	}
	List reverseList(List list) {
		List res = null;
		for (; list != null; list = list.cdr()) res = cons(list.car, res);
		return res;
	}
	Object listToListStar(List h) {
		if (h.cdr == null) return h.car;
		return cons(h.car(), listToListStar(h.cdr()));
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
		var expr = cons(begin, parseBytecode(parse(str)));
		return vmAssert(theEnvironment, objs instanceof Throwable ? $(expr) : $(expr, parseBytecode(objs))); 
	}
	boolean vmAssert(Env env, Object ... objs) {
		return vmAssert(env, (String) null, objs[0], objs.length == 1 ? $() : $(objs[1])); 
	}
	boolean vmAssert(Env env, String name, Object expr, Object ... objs) {
		name = eIfnull(name, n-> "test "+ n + ": ");
		try {
			env = env(env);
			var val = pushSubcontBarrier(null, env, pushRootPrompt(expr));
			if (objs.length == 0) print(name, expr, " should throw but is ", val);
			else {
				var expt = objs[0];
				if (equals(val, pushSubcontBarrier(null, env, pushRootPrompt(expt)))) return true;
				print(name, expr, " should be ", expt, " but is ", val);
			}
		}
		catch (Throwable t) {
			if (objs.length == 0) return true;
			if (stack) t.printStackTrace(out);
			else print(name, expr, " throw ", t);
		}
		return false;
	}
	
	
	// Bytecode parser
	Object parseBytecode(Object o) {
		if (o instanceof String s) return switch(s) { case "#inert"-> inert; case "#ignore"-> ignore; default-> s.startsWith(":") ? keyword(s) : symbol(s); };
		if (o instanceof Object[] a) return parseBytecode(a);
		return o;
	}
	Object parseBytecode(Object ... objs) {
		if (objs.length == 0) return null;
		if (objs.length == 2 && objs[0] != null && objs[0].equals("wat-string")) return objs[1];
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
	
	
	// JNI
	interface ArgsList extends Function<List,Object> {}
	interface EnvArgsList extends BiFunction<Env,List,Object> {}
	class JFun implements Combinable {
		Object jfun;
		JFun(Object jfun) { this.jfun = jfun; };
		@SuppressWarnings("preview")
		public Object combine(Env e, List o) {
			return pipe(dbg(e, this, o), ()-> {
					try {
						return switch (jfun) {
							case Supplier s-> { checkO(jfun, o, 0); yield s.get(); }  
							case ArgsList a-> a.apply(o);  
							case Function f-> { checkO(jfun, o, 1); yield f.apply(o.car()); }  
							case EnvArgsList f-> { yield f.apply(e, o); }
							case BiFunction f-> { checkO(jfun, o, 2); yield f.apply(o.car(), o.car(1)); }
							case Field f-> { checkO(jfun, o, 1, 2); if (len(o) <= 1) yield f.get(o.car()); f.set(o.car(), o.car(1)); yield inert; }
							case Method mt-> {
								var pc = mt.getParameterCount();
								if (!mt.isVarArgs()) checkO(jfun, o, 1+pc); else checkO(jfun, o, pc, -1);
								yield mt.invoke(o.car(), listToArray(o.cdr()));
							}
							case Constructor c-> { checkO(jfun, o, c.getParameterCount()); yield c.newInstance(listToArray(o)); }
							default -> error("not a combine " + jfun);
						};
					}
					catch (Value | Error exc) {
						throw exc;
					}
					catch (Throwable exc) {
						return error("jfun error: ", exc);
					}
				}
			);
		}
		public String toString() {
			var intefaces = Arrays.stream(jfun.getClass().getInterfaces()).map(i-> Vm.this.toString(i)).collect(joining(" "));
			return "{JFun" + eIf(intefaces.isEmpty(), ()-> " " + intefaces) + " " + jfun + "}"; }
	}
	boolean isjFun(Object jfun) {
		return isInstance(jfun, Supplier.class, ArgsList.class, Function.class, EnvArgsList.class, BiFunction.class, Executable.class, Field.class);
	}
	JFun jFun(Object jFun) {
		return jFun instanceof JFun jfun ? jfun : isjFun(jFun) ? new JFun(jFun) : error("no a jFun: " + jFun);
	}
	Apv jWrap(Object jfun) {
		return wrap(jFun(jfun));
	}
	
	@SuppressWarnings("preview")
	Object jInvoke(String name) {
		if (name == null) return error("method name is null");
		return jWrap(
			(ArgsList) o-> {
				if (!(o instanceof List)) return error("no operands for executing: " + name) ;  
				Object o0 = o.car();
				if (o0 == null) return error("receiver is null");
				try {
					switch (name) {
						case "getField": {
							checkO(o0, o, 2, Class.class, String.class);
							var cl = (Class) o0;
							String fName = o.car(1);
							return jWrap(cl.getField(fName));
						}
						case "getMethod": {
							checkO(o0, o, 2, -1, Class.class, String.class, Class.class);
							var cl = (Class) o0;
							String mName = o.car(1);
							return jWrap(cl.getMethod(mName, listToArray(o, 2, Class.class)));
						}
						case "invoke": {
							if (o0 instanceof Apv apv) o0 = apv.cmb;
							if (o0 instanceof JFun f) o0 = f.jfun; 
							if (!(o0 instanceof Method mt)) return error("not a Method " + toString(o0));
							var pc = mt.getParameterCount(); if (!mt.isVarArgs()) checkO(o0, o, 2+pc); else checkO(o0, o, 1+pc, -1); 
							return mt.invoke(o.car(1), listToArray(o, 2));
						}
						case "getConstructor": {
							checkO(o0, o, 1, -1, Class.class);
							var cl = (Class) o0;
							return jWrap(cl.getConstructor(listToArray(o, 1, Class.class)));
						}
						case "newInstance": {
							if (o0 == Array.class) {
								checkO(o0, o, 2, -1, Class.class, Class.class, Integer.class);
								Class cl = o.car(1);
								return newInstance(cl, stream(listToArray(o, 2, Integer.class)).mapToInt(i-> i).toArray() );
							}
							else {
								if (o0 instanceof Apv apv) o0 = apv.cmb;
								if (o0 instanceof JFun f) o0 = f.jfun; 
								if (!(o0 instanceof Constructor c)) return error("not a Constructor " + toString(o0));
								var pc = c.getParameterCount(); if (!c.isVarArgs()) checkO(o0, o, 1+pc); else checkO(o0, o, pc, -1); 
								return c.newInstance(listToArray(o, 1));
							}
						}
					}
				}
				catch (Exception exc) {
					return error("error executing: " + name + " in: " + toString(o), exc);
				}
				Object[] args = listToArray(o, 1);
				//Executable executable = getExecutable(name.equals("new") ? (Class) o0 : o0.getClass(), name,  getClasses(args));
				// (@new class classes)   -> class.getConstructor(classes) -> constructor
				// (@new class objects)   -> class.getConstructor(classes).newInstance(objects) -> constructor.newInstance(objects)
				// (@name class classes)  -> class.getMethod(name, classes) -> method
				// (@name object objects) -> object.getClass().getMethod(name, getClasses(objects)).invocke(object, objects) -> method.invoke(object, objects)
				Executable executable = getExecutable(o0 instanceof Class cl ? cl : o0.getClass(), name, getClasses(args));
				if (executable == null) return error("not found " + executable(name, args) + " of: " + toString(o0));
				if (o0 instanceof Class && stream(args).allMatch(a-> a instanceof Class)) return jWrap(executable);
				try {
					if (executable.isVarArgs()) args = reorg(executable.getParameterTypes(), args);
					return switch (executable) {
						case Method m-> m.invoke(o0, args);
						case Constructor c-> c.newInstance(args);
					};
				}
				catch (Exception exc) {
					return error("error executing " + executable(name, args) + "of: " + toString(o0) + " with: " + toString(args), exc);
				}
			}
		);
	}
	private String executable(String name, Object[] args) {
		return (name.equals("new") ? "constructor" : "method: " + name) + toString(list((Object[])getClasses(args)));
	}
	Object jGetSet(String name) {
		if (name == null) return error("field name is null");
		return jWrap(
			(ArgsList) o-> {
				var len = checkO("jGetSet", o, 1, 2); 
				var o0 = o.car();
				// (.name class)        -> class.getField(name) -> field
				// (.name object)       -> object.getclass().getField(name).get(object) -> field.get(object) 
				// (.name object value) -> object.getClass().getField(name).set(object,value) -> field.set(object, value) 
				Field field = getField(o0 instanceof Class cl ? cl : o0.getClass(), name);
				if (field == null) return error("not found field: " + name + " in: " + toString(o0));
				if (o0 instanceof Class) return jWrap(field);
				try {
					if (len == 1) return field.get(o0);
					field.set(o0, o.car(1)); return inert;
				}
				catch (Exception e) {
					return error("can't " + (len==1 ? "get" : "set") + " " + name + " of " + toString(o0) + eIf(len == 1, ()-> " with " + toString(o.car(1))));
				}
			}
		);
	}
	
	
	// Stringification
	String toString(Object o) { return toString(false, o); }
	@SuppressWarnings("preview")
	String toString(boolean t, Object o) {
		return switch (o) {
			case null-> "#null";
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
	
	
	// Bootstrap
	Env theEnvironment = env(null); {
		bind(theEnvironment, null, symbol("%def"), new Def());
		bind(theEnvironment, null, symbol("%begin"), begin);
		evaluate(theEnvironment,
			parseBytecode(
				$("%begin",
					// Basics
					$("%def", "%vau", new Vau()),
					$("%def", "%eval", wrap(new Eval())),
					$("%def", "%makeEnvironment", jWrap((ArgsList) o-> env(checkO("env", o, 0, 1, Env.class) == 0 ? null : o.car()))),
					$("%def", "%wrap", jWrap((Function<Object, Object>) this::wrap)),
					$("%def", "%unwrap", jWrap((Function<Object, Object>) this::unwrap)),
					// Values
					$("%def", "%cons", jWrap((BiFunction<Object, Object, Object>) this::cons)),
					$("%def", "%cons?", jWrap((Function<Object, Boolean>) obj-> obj instanceof Cons)),
					$("%def", "%nil?", jWrap((Function<Object, Boolean>) obj-> obj == null)),
					$("%def", "%string->symbol", jWrap((Function<String, Symbol>) this::symbol)),
					$("%def", "%symbol?", jWrap((Function<Object, Boolean>) obj-> obj instanceof Symbol)),
					$("%def", "%symbolName", jWrap((Function<Symbol, String>) sym-> sym.name)),
					// First-order Control
					$("%def", "%if", new If()),
					$("%def", "%loop", new Loop()),
					$("%def", "%catch", new Catch()),
					$("%def", "%throw", new Throw()),
					$("%def", "%finally", new Finally()),
					// Delimited Control
					$("%def", "%takeSubcont", wrap(new TakeSubcont())),
					$("%def", "%pushPrompt", new PushPrompt()),
					$("%def", "%pushPromptSubcont", wrap(new PushPromptSubcont())),
					// Dynamically-scoped Variables
					$("%def", "%dNew", jWrap((Function<Object,DVar>) DVar::new)),
					$("%def", "%dVal", jWrap((ArgsList) o-> { DVar dv = o.car(); return checkO("%dVal", o, 1, 2) == 1 ? dv.val : (dv.val=o.car(1)); })),
					$("%def", "%dDef", new DDef()),
					$("%def", "%dLet", new DLet()),
					// Errors
					$("%def", "%rootPrompt", rootPrompt),
					$("%def", "%error", jWrap((Function<String, Object>) this::error)),
					// Java Interface
					$("%def", "%jinvoke", jWrap((Function<String,Object>) this::jInvoke)),
					$("%def", "%jgetset", jWrap((Function<String,Object>) this::jGetSet)),
					$("%def", "instanceof", jWrap((BiFunction<Object,Class,Boolean>) (o,c)-> c.isInstance(o))),
					// Utilities
					$("%def", "%list", jWrap((ArgsList) o-> o)),
					$("%def", "%list*", jWrap((ArgsList) this::listToListStar)),
					$("%def", "%list->array", jWrap((Function<List,Object[]>) this::listToArray)),
					$("%def", "%array->list", jWrap((BiFunction<Boolean,Object[],Object>) this::arrayToList)),
					$("%def", "%reverseList", jWrap((Function<List,List>) this::reverseList)),
					// 
					$("%def", "+", jWrap((BinaryOperator) (a,b)-> a instanceof Number n1 && b instanceof Number n2 ? binOp(Pls, n1, n2) : toString(a) + toString(b))),
					$("%def", "*", jWrap((BiFunction<Number,Number,Object>) (a,b)-> binOp(Pwr, a, b))),
					$("%def", "-", jWrap((BiFunction<Number,Number,Object>) (a,b)-> binOp(Mns, a, b))),
					$("%def", "/", jWrap((BiFunction<Number,Number,Object>) (a,b)-> binOp(Dvd, a, b))),
					$("%def", "%", jWrap((BiFunction<Number,Number,Object>) (a,b)-> binOp(Rst, a, b))),
					//
					$("%def", "!", jWrap((Function<Boolean,Boolean>) a-> !a)),
					$("%def", "<", jWrap((BiFunction<Number,Number,Object>) (a,b)-> binOp(Ls, a, b))),
					$("%def", ">", jWrap((BiFunction<Number,Number,Object>) (a,b)-> binOp(Gt, a, b))),
					$("%def", "<=", jWrap((BiFunction<Number,Number,Object>) (a,b)-> binOp(Le, a, b))),
					$("%def", ">=", jWrap((BiFunction<Number,Number,Object>) (a,b)-> binOp(Ge, a, b))),
					//
					$("%def", "~", jWrap((Function<Integer,Integer>) a-> ~a)),
					$("%def", "&", jWrap((BiFunction<Number,Number,Object>) (a,b)-> binOp(And, a, b))),
					$("%def", "|", jWrap((BiFunction<Number,Number,Object>) (a,b)-> binOp(Or, a, b))),
					$("%def", "^", jWrap((BiFunction<Number,Number,Object>) (a,b)-> binOp(Xor, a, b))),
					$("%def", "<<", jWrap((BiFunction<Number,Number,Object>) (a,b)-> binOp(Sl, a, b))),
					$("%def", ">>", jWrap((BiFunction<Number,Number,Object>) (a,b)-> binOp(Sr, a, b))),
					$("%def", ">>>", jWrap((BiFunction<Number,Number,Object>) (a,b)-> binOp(Sr0, a, b))),
					//
					$("%def", "%quote", $("%vau", $("arg"), ignore, "arg")),
					$("%def", "%theEnvironment", $("%vau", null, "env", "env")),
					$("%def", "%lambda", $("%vau", $("formals", ".", "body"), "env",
						$("%wrap", $("%eval", $("%list*", "%vau", "formals", ignore, "body"), "env")))),
					$("%def", "%jambda", jFun((ArgsList) o-> lambda(o.car(), o.car(1), o.cdr(1)))),
					
					$("%def", "==", jWrap((BiFunction<Object,Object,Boolean>) (a,b)-> a == b)),
					$("%def", "!=", jWrap((BiFunction<Object,Object,Boolean>) (a,b)-> a != b)),
					$("%def", "eq?", jWrap((BiFunction<Object,Object,Boolean>) (a,b)-> equals(a, b))),
					$("%def", "assert", jFun((EnvArgsList) (e,o)-> { checkO("assert", o, 1, 2); return vmAssert(e,listToArray(o)); } )),
					$("%def", "test", jFun((EnvArgsList) (e,o)-> { checkO("test", o, 2, 3); return vmAssert(e,toString(o.car()), o.car(1), listToArray(o.cdr(1))); } )),
					$("%def", "toString", jWrap((Function<Object,String>) obj-> toString(obj))),
					$("%def", "log", jWrap((ArgsList) o-> log(listToArray(o)))),
					$("%def", "print", jWrap((ArgsList) o-> print(listToArray(o)))),
					$("%def", "write", jWrap((ArgsList) o-> write(listToArray(o)))),
					$("%def", "load", jWrap((Function<String, Object>) nf-> uncked(()-> eval(readString(nf))))),
					$("%def", "dotco", jWrap((ArgsList) o-> { if (checkO("dotco", o, 0, 1, Boolean.class) == 0) return dotco; dotco=o.car(); return inert; })),
					$("%def", "trace", jWrap((ArgsList) o-> { if (checkO("trace", o, 0, 1, Boolean.class) == 0) return trace; trace=o.car(); return inert; })),
					$("%def", "stack", jWrap((ArgsList) o-> { if (checkO("stack", o, 0, 1, Boolean.class) == 0) return stack; stack=o.car(); return inert; })),
					$("%def", "prenv", jWrap((ArgsList) o-> { if (checkO("prenv", o, 0, 1, Boolean.class) == 0) return prenv; prenv=o.car(); return inert; }))
				)
			)
		);
	}
	
	
	// API
	public Object exec(Object bytecode) {
		var wrapped = pushRootPrompt(cons(new Begin(true), parseBytecode(bytecode)));
		return pushSubcontBarrier(null, theEnvironment, wrapped);
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
			oos.writeObject(parse(readString(fileName)));
		}
	}
	public String readString(String fileName) throws IOException {
		if (trace) print("\n--------:  " + fileName);
		return Files.readString(Paths.get(fileName), Charset.forName("cp1252"));
	} 
	public Object exec(String fileName) throws Exception {
		return exec(readBytecode(fileName));
	}
	public Object readBytecode(String fileName) throws Exception {
		if (trace) print("\n--------:  " + fileName);
		try (var ois = new ObjectInputStream(new FileInputStream("build/" + fileName))) {
			return ois.readObject();
		}
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
					if (stack) t.printStackTrace(out);
					else out.println(t.getClass().getSimpleName() + ": " + t.getMessage());
				}
			}
		}
		print("finito");
	}
	public String read() throws IOException {
		var s = new StringBuilder();
		int open = 0, close = 0;
		boolean inEscape = false, inString = false, inUString = false, inComment=false, sMlComment=false, inMlComment=false, eMlComment=false;
		do {
			var oc = close-open;
			out.print(oc==0 ? ">" : "%+d%s>".formatted(oc, oc>0 ? "(" : ")"));
			for (int c; (c = in.read()) != '\n' || inString || inUString || inMlComment;) {
				if (inEscape) {
					inEscape = false;
				}
				else if (inString) switch (c) {
					case '\\'-> inEscape = true;
					case '"'-> inString = false;
				}
				else if (inUString) switch (c) {
					case '|'-> inUString = false;
				}
				else if (inComment) switch (c) {
					case '"'-> inString = true;
					case '\n'-> inComment = false;
				}
				else if (eMlComment) switch (c) {
					case '|'-> inMlComment = eMlComment = false;
				}
				else if (inMlComment) switch (c) {
					case '"'-> inString = true;
					case ';'-> inComment = true;
					case '#'-> eMlComment = true;
				}
				else if (sMlComment) switch (c) {
					case '#'-> inMlComment = !(sMlComment = false);
					default -> inUString = !(sMlComment = false);
				}
				else switch (c) {
					case '"'-> inString = true;
					case ';'-> inComment = true;
					case '|'-> sMlComment = true;
					case '('-> open += 1;
					case ')'-> close += 1;
				}
				if (c >= 32 || inUString) s.append((char) c);
			}
			if (inComment) { s.append('\n'); inComment = false; }
		} while (open > close);
		return s.toString();
	}
	
	
	// Test
	public static void main(String[] args) throws Exception {
		new Vm().main();
	}
	public void main() throws Exception {
		/*
		print(parseBytecode("a"));
		print(parseBytecode("#ignore"));
		print(parseBytecode($()));
		print(parseBytecode($("a")));
		print(parseBytecode($("a", "b")));
		print(parseBytecode($("a", "b", ".", $())));
		print(parseBytecode($("a", "b", ".", "c")));
		print(parseBytecode($("a", $("b"), ".", "c")));
		print(parseBytecode($("wat-string", "string")));
		//*/
		/*
		print(list());
		print(list("a"));
		print(list("a", "b"));
		print(arrayToList(true));
		print(arrayToList(true, "a"));
		print(arrayToList(true, "a", "b"));
		print(listStar());
		print(listStar("a"));
		print(listStar("a", "b"));
		print(arrayToList(false));
		print(arrayToList(false, "a"));
		print(arrayToList(false, "a", "b"));
		//*/
		/*
		print(listToListStar(1)); // -> 1 
		print(listToListStar(nil)); // -> ()
		print(listToListStar(list())); // -> ()
		print(listToListStar(list(1, 2))); // -> (1 . 2)
		print(listToListStar(list(1, 2, 3))); // -> (1 2 . 3)
		//print(listToListStar(listStar(1, 2))); // -> not a proper list
		//print(listToListStar(listStar(1, 2, 3))); // -> not a proper list
		//print(listToListStar(listStar(1, 2, 3, 4))); // -> not a proper list
		//*/
		//print(equals(listToArray(list(1,2,3,4)), new Object[] {1,2,3,4})); // -> true
		//print(parseBytecode(parse("(%cons 1 2)"))); // -> ((%cons 1 2))
		/*
		var Throw = new Throwable();
		vmAssert("1", 1);
		vmAssert("1 2 3", 3);
		vmAssert("(%cons 1 2)", $(1,".",2));
		vmAssert("(%list 1 2 3)", $("%list",1,2,3));
		vmAssert("(%list* 1 2 3 4)", $("%list*",1,2,3,4));
		vmAssert("(%array->list #t (%list->array (%list 1 2 3 4)))", $("%list",1,2,3,4));
		vmAssert("(%array->list #f (%list->array (%list 1 2 3 4)))", $("%list*",1,2,3,4));
		vmAssert("(%reverseList (%list 1 2 3 4))", $("%list",4,3,2,1));
		
		vmAssert("(+ 1 2)", 3);
		vmAssert("(* 3 2)", 6);
		vmAssert("(* (* 3 2) 2)", 12);
		
		vmAssert("((%vau a #ignore a) 1 2)", $("%list",1,2));
		vmAssert("((%vau (a b) #ignore b) 1 2)", 2);
		vmAssert("((%vau (a . b) #ignore b) 1 2 3)", $("%list",2,3));
		vmAssert("(%def x (%list 1)) x", $("%list",1));
		vmAssert("(%def x 1) ((%wrap(%vau (a) #ignore a)) x)", 1);
		
		vmAssert("(%def () 1) a", Throw);
		vmAssert("(%def a (%list 1)) a", $("%list", 1));
		vmAssert("(%def (a) (%list 1)) a", 1);
		vmAssert("(%def a (%list 1 2 3)) a", $("%list",1,2,3));
		vmAssert("(%def (a) 1 2 3) a", Throw);
		vmAssert("(%def 2 1)", Throw);
		vmAssert("(%def (a) (%list 1)) a", 1);
		vmAssert("(%def (a b) (%list 1 2)) a b", 2);
		vmAssert("(%def (a . b) (%list 1 2 3)) a b", $("%list",2,3)); 
		
		vmAssert("(%vau (a . 12) #ignore 0)", Throw);
		vmAssert("(%def (a b 12) 1)", Throw);
		
		vmAssert("(%quote (a b c))", $("%quote", $("a","b","c")));
		
		vmAssert("(%def 2 1)", Throw);
		vmAssert("(%def a 1) a", 1);
		vmAssert("(%def (a . b) (%list 1 2 3)) a b", $("%list",2,3));
		vmAssert("(%def define %def) (define a 1) a", 1);
		vmAssert("(%def $define! %def) ($define! a 1) a", 1);
		vmAssert("(%def $define! %def) $define!", theEnvironment.get("%def").value);
		vmAssert("(%if #t 1 2)", 1);
		vmAssert("(%if #f 1 2)", 2);
		vmAssert("(%if #null 1 2)", 2);
		//*/
		/*
		eval("""
			(assert (%def a 1) #inert)
			(assert (%def a)         ) ;throw
		""");
		//*/
		
		//exec(parse(readString("boot.wat")));
		//compile("boot.wat");
		//exec(readBytecode("boot.wat"));
		//*
		var milli = currentTimeMillis();
		eval(readString("testVm.lsp"));
		//eval(readString("wat/boot.wat"));
		//eval(readString("wat/test.wat"));
		eval(readString("boot.lsp"));
		eval(readString("test.lsp"));
		eval(readString("testJni.lsp"));
		print("start time: " + (currentTimeMillis() - milli));
		//stack = true;
		repl();
		//*/
	}
}
