package Wat;

import static List.Parser.parse;
import static Wat.Utility.$;
import static Wat.Utility.apply;
import static Wat.Utility.eIf;
import static Wat.Utility.eIfnull;
import static Wat.Utility.getClasses;
import static Wat.Utility.getExecutable;
import static Wat.Utility.getField;
import static Wat.Utility.isInstance;
import static Wat.Utility.reorg;
import static Wat.Utility.uncked;
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
	o: operands
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
	
	boolean tco = true;
	boolean trace = false;
	boolean stack = false;
	boolean thenv = false;
	boolean jfapv = false;
	
	interface Combinable { Object combine(Env e, Object o); }
	
	
	// Continuations
	class Continuation {
		Function<Resumption, Object> f; Continuation next; Object dbg; Env e;
		Continuation(Function<Resumption, Object> f, Continuation next, Object dbg, Env e) {
			this.f = f; this.next = next; this.dbg = dbg; this.e = e;
		}
		public String toString() { return "{Continuation %s %s}".formatted(dbg, e); }
		Object apply(Env e, Apv apv0) { return apply(()-> combine(e, apv0, nil)); }
		Object apply(Supplier s) { return f.apply(new Resumption(next, s));}
	}
	class Resumption {
		Continuation k; Supplier<Object> s;
		Resumption(Continuation k, Supplier<Object> s) { this.k=k; this.s=s; }
		public String toString() { return "{Resumption %s %s}".formatted(s, k); }
		<T> T resume() { var k = this.k; this.k = k.next; return (T) k.f.apply(this); }
	};
	class Suspension {
		Object prompt; Combinable handler; Continuation k;
		Suspension(Object prompt, Combinable handler) { this.prompt = prompt; this.handler = handler; }
		public String toString() { return "{Suspension %s %s %s}".formatted(prompt, handler, k); }
		Suspension suspend(Function<Resumption, Object> f, Object dbg, Env e) { 
			k = new Continuation(f, k, dbg, e); return this;
		}
	}
	<T, R> Object pipe(Supplier<T> before, Function<T, R> after, Object dbg, Env e) {
		return pipe(null, before, after, dbg, e);
	}
	<T, R> Object pipe(Resumption r, Supplier<T> before, Function<T, R> after, Object dbg, Env e) {
		T ris = r != null ? r.resume() : before.get();
		return ris instanceof Suspension s ? s.suspend(rr-> pipe(rr, before, after, dbg, e), dbg, e) : after.apply(ris);
	}
	<T, R> Object pipe1(Resumption r, boolean first, Supplier<T> before, Function<T, R> after, Object dbg, Env e) {
		T ris = first && r != null && !(first=false) ? r.resume() : before.get();
		var ff = first; return ris instanceof Suspension s ? s.suspend(rr-> pipe1(rr, ff, before, after, dbg, e), dbg, e) : after.apply(ris);
	}
	
	
	// Basic Forms
	class Nil { public String toString() { return "()"; }};
	public Nil nil = new Nil();
	
	class Inert { public String toString() { return "#inert"; }};
	public Inert inert = new Inert();
	
	class Ignore { public String toString() { return "#ignore"; }};
	public Ignore ignore = new Ignore();
	
	
	// Tail Call Optimization
	record Tco (Env e, Object o) {}
	Object tco (Env e, Object o) { return tco ? new Tco(e, o) : evaluate(e, o); }
	
	
	// Evaluation Core
	@SuppressWarnings("preview")
	Object evaluate(Env e, Object o) {
		if (trace) print("evaluate: ", o);
		for (;;) {
			Object v = switch (o) {
				case null, default-> o;
				case Symbol s-> e.lookup(s.name);
				case Cons c-> {
					var ee=e; yield pipe1(null, true,  ()-> evaluate(ee, c.car), op-> combine(ee, op, c.cdr), o, e);
				}
			};
			if (!(v instanceof Tco tco)) return v;
			e = tco.e; o = tco.o;
		}
	}
	
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
			if (c.cdr == nil) return Vm.this.toString(true, c.car);
			if (c.cdr instanceof Cons cc) return Vm.this.toString(true, c.car) + " " + toString(cc);
			return Vm.this.toString(true, c.car) + " . " + Vm.this.toString(true, c.cdr);
		}
		public boolean equals(Object o) {
			return this == o || o instanceof Cons c && Vm.this.equals(this.car,  c.car) && Vm.this.equals(this.cdr,  c.cdr);
		}
	}
	Cons cons(Object car, Object cdr) { return new Cons(car, cdr); };
	<T> T car(Object o) { return o instanceof Cons c ? (T) c.car : error("not a cons: " + toString(o)); }
	<T> T cdr(Object o) { return o instanceof Cons c ? (T) c.cdr : error("not a cons: " + toString(o)); }
	<T> T car(Object o, int i) { for (; i>0; i-=1) o=cdr(o); return car(o); }
	<T> T cdr(Object o, int i) { for (; i>0; i-=1) o=cdr(o); return cdr(o); }
	int len(Object o) { int i=0; for (; o instanceof Cons c; o=c.cdr) i+=1; return i; }
	<T> T setCar(Object o, Object v) { return o instanceof Cons c ? (T) (c.car=v) : error("not a cons: " + toString(o)); }
	<T> T setCdr(Object o, Object v) { return o instanceof Cons c ? (T) (c.cdr=v) : error("not a cons: " + toString(o)); }
	
	
	// Environment
	class Env {
		Map<String,Object> map = new LinkedHashMap(); Env parent;
		Env(Env parent) { this.parent = parent; }
		public Object get(String name) { return map.get(name); };
		public Object bind(String name, Object rhs) {
			if (trace) print("    bind: ", name, "=", rhs, " in: ", this); map.put(name, rhs); return null; 
		}
		public String toString() {
			var isThenv = this == theEnvironment;
			return "[" + eIf(!isThenv, "The-") + "Env" + eIf(isThenv && !thenv, ()-> mapReverse()) + eIf(parent == null, ()-> " " + parent) + "]";
		}
		String mapReverse() {
			var sb = new StringBuilder(); map.entrySet().forEach(e-> sb.insert(0, " " + e)); return sb.toString();
		}
		Object lookup(String name) {
			if (!map.containsKey(name)) return parent != null ? parent.lookup(name) : error("unbound: " + name);
			Object value = map.get(name); if (trace) print("  lookup: ", value); return value;
			/* TODO in alternativa al precedente
			Object value = map.get(name);
			if (value != null || map.containsKey(name)) { if (trace) print("  lookup: ", value); return value; }
			return parent != null ? parent.lookup(name) : error("unbound: " + name);
			//*/
			/* TODO idem
			Object value = map.get(name);
			if (value == null && !map.containsKey(name)) return parent != null ? parent.lookup(name) : error("unbound: " + name);
			if (trace) print("  lookup: ", value); return value;
			//*/
		}
	}
	Env env(Env parent) { return new Env(parent); }
	
	
	// Bind
	Object bind(Env e, Object lhs, Object rhs, Object exp) {
		var msg = bind(e, lhs, rhs); if (msg == null) return inert;
		return error(msg + " for bind: " + lhs + eIf(exp == null, ()-> " of: " + exp) + " with: " + rhs);
	}
	@SuppressWarnings("preview")
	Object bind(Env e, Object lhs, Object rhs) {
		return switch (lhs) {
			case Ignore i-> null;
			case Symbol s-> e.bind(s.name, rhs);  
			case Nil n-> rhs == nil ? null : "too many arguments" /*+ ", none expected, but got: " + toString(rhs)*/;
			case Cons lc-> {
				if (!(rhs instanceof Cons rc)) yield "too few arguments" /*+ ", more expected, but got: " + toString(rhs)*/;
				var msg = bind(e, lc.car, rc.car); if (msg != null) yield msg;
				yield bind(e, lc.cdr, rc.cdr);
			}
			default-> error("cannot bind: " + lhs);
		};
	}
	
	
	// Operative & Applicative Combiners
	Object combine(Env e, Object op, Object o) {
		if (trace) print(" combine: ", op, " ", o);
		if (op instanceof Combinable cmb) return cmb.combine(e, o);
		// per default le jFun dovrebbero essere operative e non applicative
		if (isjFun(op)) return jfapv
			? ((Combinable) jWrap(op)).combine(e, o) // jfun x default applicative
			: ((Combinable) jFun(op)).combine(e, o) // jfun x default operative
		;
		return error("not a combiner: " + toString(op) + " in: " + cons(op, o));
	}
	
	class Opv implements Combinable  {
		Object p, ep, x; Env e;
		Opv(Object p, Object ep, Object x, Env e) { this.p = p; this.ep = ep; this.x = x; this.e = e; }
		public Object combine(Env e, Object o) {
			var xe = env(this.e); bind(xe, p, o, this); bind(xe, ep, e, this); return begin.combine(xe, x);
		}
		public String toString() { return "{Opv " + Vm.this.toString(p) + " " + Vm.this.toString(ep) + " " + Vm.this.toString(x) + "}"; }
	}
	class Apv implements Combinable  {
		Combinable cmb;
		Apv(Combinable cmb) { this.cmb = cmb; }
		public Object combine(Env e, Object o) {
			return pipe(()-> evalArgs(null, e, o, nil), args-> cmb.combine(e, args), o, e);
		}
		Object evalArgs(Resumption r, Env e, Object todo, Object done) {
			if (todo == nil) return reverseList(done);
			var arg = car(todo);
			return pipe(()-> evaluate(e, arg), res-> evalArgs(null, e, cdr(todo), cons(res, done)), arg, e);  
		}
		public String toString() { return "{Apv " + Vm.this.toString(cmb) + "}"; }
	}
	Apv wrap(Object arg) { return /*arg instanceof Apv apv ? apv :*/ arg instanceof Combinable cmb ? new Apv(cmb) : error("cannot wrap: " + arg); }
	Combinable unwrap(Object arg) { return arg instanceof Apv apv ? apv.cmb : error("cannot unwrap: " + arg); }
	Apv lambda(Object p, Object e, Object b) { return new Apv(new Opv(p, ignore, b, (Env) evaluate(theEnvironment, e))); }
	
	
	// Built-in Combiners
	class Vau implements Combinable  {
		public Object combine(Env e, Object o) {
			checkO(this, o, 3, -1); // o = (pt ep x ...)
			var pt = car(o);
			var ep = car(o, 1);
			var msg = checkPt(pt, ep); if (msg != null) return error(msg + " of: " + cons(this, o));
			return new Opv(pt, ep, cdr(o, 1), e);
		}
		public String toString() { return "vmVau"; }
	};
	class Def implements Combinable  {
		public Object combine(Env e, Object o) {
			checkO(this, o, 2); // o = (pt arg)
			var pt = car(o);
			if (!(pt instanceof Symbol)) {
				if (!(pt instanceof Cons)) return error("not a symbol: " + pt + " in: " + cons(this, o));
				var msg = checkPt(pt); if (msg != null) return error(msg + " of: " + cons(this, o));
			}
			var arg = car(o, 1);
			return pipe(()-> evaluate(e, arg), res-> bind(e, pt, res, cons(this, o)), arg, e);
		}
		public String toString() { return "vmDef"; }
	};
	class Eval implements Combinable  {
		public Object combine(Env e, Object o) {
			checkO(this, o, 2); // o = (x eo)
			var x = car(o);
			var o1 = car(o, 1);
			if (!(o1 instanceof Env eo)) return error("not an Env: " + o1);
			return evaluate(eo, x);
		}
		public String toString() { return "vmEval"; }
	}
	
	
	// First-order Control
	class Begin implements Combinable  {
		boolean root;
		Begin() {}; Begin(boolean root) { this.root = root; } 
		public Object combine(Env e, Object o) {
			// o = (... xs)
			return o instanceof Cons c ? begin(null, e, c) : inert;
		}
		Object begin(Resumption r, Env e, Cons c) {
			if (trace && root && r == null) print("\n--------");
			var first = true;
			for (;;) {
				if (!(c.cdr instanceof Cons cc)) return tco(e, c.car); 
				var res = first && r != null && !(first = false) ? r.resume() : evaluate(e, c.car);
				var ccc = c; if (res instanceof Suspension s) return s.suspend(rr-> begin(rr, e, ccc), c.car, e);
				c = cc;
			}
		}
		public String toString() { return "vmBegin" + eIf(!root, "*"); }
	}
	Begin begin = new Begin();
	class If implements Combinable  {
		public Object combine(Env e, Object o) {
			checkO(this, o, 2, 3); // o = (test then else) 
			var test = car(o);
			return pipe(()-> evaluate(e, test), res-> istrue(res) ? tco(e, car(o, 1)) : cdr(o, 1) instanceof Cons c ? tco(e, c.car) : inert, test, e);
		}
		private boolean istrue(Object res) {
			return res != null && res != nil && res instanceof Boolean b && b;
		}
		public String toString() { return "vmIf"; }
	}
	class Loop implements Combinable  {
		public Object combine(Env e, Object o) {
			checkO(this, o, 1, -1); // o = (x ...)
			for (;;) evaluate(e, begin.combine(e, o));
		}
		public String toString() { return "vmLoop"; }
	}
	class Catch implements Combinable  {
		public Object combine(Env e, Object o) {
			var l = checkO(this, o, 1, 2); // o = (x handler)
			var x = car(o);
			var handler = l == 1 ? null : car(o, 1);
			return combine(null, e, x, handler);
		}
		private Object combine(Resumption r, Env e, Object x, Object handler) {
			Object res = null;
			try {
				res = r != null ? r.resume() : evaluate(e, x);
			}
			catch (Error | Value exc) {
				res = exc instanceof Value v ? v.value : exc;
				if (handler != null) {
					handler = evaluate(e, handler);
					if (!(handler instanceof Apv apv1 && args(apv1) == 1)) return error("not a one arg applicative combiner: " + handler); 
					// unwrap handler to prevent eval if exc is sym or cons
					res = Vm.this.combine(e, unwrap(apv1), list(res));
				}
			}
			var hh = handler;
			return res instanceof Suspension s ? s.suspend(rr-> combine(rr, e, x, hh), x, e) : res;
		}
		public String toString() { return "vmCatch"; }
	}
	class Finally implements Combinable {
		public Object combine(Env e, Object o) {
			checkO(this, o, 2); // o = (x cleanup)
			var x = car(o);
			var cleanup = car(o, 1);
			try {
				return pipe(()-> evaluate(e, x), res-> doCleanup(null, cleanup, res, e), x, e);
			}
			catch (Throwable t) {
				return doCleanup(null, cleanup, t, e);
			}
		}
		Object doCleanup(Resumption r, Object cleanup, Object value, Env e) {
			return pipe(()-> evaluate(e, cleanup),
				res->{
					if (!(value instanceof Throwable t)) return value;
					throw t instanceof RuntimeException rte ? rte : new Error(t);
				},
				value, e
			);
		}
		public String toString() { return "vmFinally"; }
	}
	class CatchTag implements Combinable {
		public Object combine(Env e, Object o) {
			checkO(this, o, 2); // o = (tag x)
			var tag = car(o);
			var x = car(o, 1);
			try {
				return pipe(()-> evaluate(e, x), res-> res, tag, e);
			}
			catch (ValueTag exc) {
				if (Vm.this.equals(exc.tag, tag)) return exc.value;
				throw exc;
			}
		}
		public String toString() { return "vmCatchTag"; }
	}
	class ThrowTag implements Combinable {
		public Object combine(Env e, Object o) {
			var l = checkO(this, o, 1, 2); // o = (tag value)
			var tag = car(o);
			var value = l == 2 ? car(o, 1) : inert;
			return pipe(()-> evaluate(e, value), res->{ throw new ValueTag(tag, res); }, tag, e);
		}
		public String toString() { return "vmThrowTag"; }
	}
	class ValueTag extends RuntimeException {
		private static final long serialVersionUID = 1L;
		Object tag, value;
		ValueTag(Object tag, Object value) {
			super(Vm.this.toString(tag) + " " + Vm.this.toString(value)); this.tag = tag; this.value = value;
		}
	}
	
	// TODO valutare
	class CatchAll implements Combinable {
		public Object combine(Env e, Object o) {
			var l = checkO(this, o, 1, 3); // o = (tag x handler)
			var tag = l < 2 ? null : car(o);
			var x = car(o, l == 1 ? 0 : 1);
			var handler = l == 3 ? car(o, 2) : null;
			return combine(null, e, tag, x, handler);
		}
		private Object combine(Resumption r, Env e, Object tag, Object x, Object handler) {
			Object res = null;
			try {
				res = r != null ? r.resume() : evaluate(e, x);
			}
			catch (ValueTag exc) {
				if (tag != null && !Vm.this.equals(exc.tag, tag)) throw exc; 
				res = getValue(exc, e, handler);
			}
			return res instanceof Suspension s ? s.suspend(rr-> combine(rr, e, tag, x, handler), tag, e) : res;
		}		
		private Object getValue(ValueTag exc, Env e, Object handler) {
			if (handler == null) return exc.value;
			handler = evaluate(e, handler);
			if (!(handler instanceof Apv apv1 && args(apv1) == 1)) return error("not a one arg applicative combiner: " + handler); 
			// unwrap handler to prevent eval if exc is sym or cons
			return Vm.this.combine(e, unwrap(apv1), list(exc.value));
		}
		public String toString() { return "CatchAll"; }
	}
	class ThrowAll implements Combinable {
		public Object combine(Env e, Object o) {
			var l = checkO(this, o, 1, 2); // o = (tag value)
			var tag = l == 2 ? car(o) : null;
			var value = l > 0 ? car(o, l-1) : null;
			return pipe(()-> evaluate(e, value), res->{ throw new ValueTag(tag, res); }, tag, e);
		}
		public String toString() { return "ThrowAll"; }
	}
	
	// Delimited Control
	class PushPrompt implements Combinable  {
		public Object combine(Env e, Object o) {
			checkO(this, o, 2); // o = (prompt x)
			var prompt = car(o);
			var x = car(o, 1);
			return pushPrompt(null, e, cons(this, o), prompt, ()-> evaluate(e, x));
		}
		public String toString() { return "vmPushPrompt"; }
	}
	class TakeSubcont implements Combinable  {
		public Object combine(Env e, Object o) {
			checkO(this, o, 2); // o = (prompt handler)
			var prompt = car(o);
			var handler = car(o, 1); 
			if (!(handler instanceof Apv apv1 && args(apv1) == 1)) return error("not a one arg applicative combiner: " + handler); 
			return new Suspension(prompt, apv1).suspend(rr-> evaluate(e, cons(rr.s, nil)), cons(this, o), e);
		}
		public String toString() { return "vmTakeSubcont"; }
	}
	class PushPromptSubcont implements Combinable  {
		public Object combine(Env e, Object o) {
			checkO(this, o, 3); // o = (prompt k apv0)
			var prompt = car(o);
			var o1 = car(o, 1); if (!(o1 instanceof Continuation k)) return error("not a continuation: " + o1); 
			var o2 = car(o, 2); if (!(o2 instanceof Apv apv0 && args(apv0) == 0)) return error("not a zero args applicative combiner: " + o2);
			//return pushPrompt(r, e, cons(this, o), prompt, ()-> k.apply(e, ()-> Vm.this.combine(null, e, apv0, nil)));
			return pushPrompt(null, e, cons(this, o), prompt, ()-> k.apply(e, apv0));
		}
		public String toString() { return "vmPushPromptSubcont"; }
	}
	Object pushPrompt(Resumption r, Env e, Object x, Object prompt, Supplier action) {
		var res = r != null ? r.resume() : action.get();
		if (!(res instanceof Suspension s)) return res;
		return prompt == ignore || !equals(s.prompt, prompt)
			? s.suspend(rr-> pushPrompt(rr, e, x, prompt, action), x, e)
			: combine(e, s.handler, cons(s.k, nil))
		;
	}
	Object pushSubcontBarrier(Resumption r, Env e, Object x, Supplier action) {
		var res = r != null ? r.resume() : action.get();
		if (!(res instanceof Suspension s)) return res;
		s.suspend(rr-> pushSubcontBarrier(rr, e, x, action), x, e);
		return s.k.apply(()-> error("prompt not found: " + s.prompt));
	}
	
	
	// Dynamic Variables
	class DV {
		Object val;
		DV(Object val) { this.val = val; }
		public String toString() { return "[DV " + val + "]"; }
	}
	class DNew implements Combinable  {
		public Object combine(Env e, Object o) {
			checkO(this, o, 1); // o = (x)
			return new DV(car(o));
		}
		public String toString() { return "vmDNew"; }
	}
	class DRef implements Combinable  {
		public Object combine(Env e, Object o) {
			checkO(this, o, 1); // o = (x)
			var x = car(o);
			return x instanceof DV dv ? dv.val : error("not a dinamic variable: " + x);
		}
		public String toString() { return "vmDRef"; }
	}
	class DLet implements Combinable  {
		public Object combine(Env e, Object o) {
			checkO(this, o, 3); // o = (dv val x)
			var o0 = car(o);
			if (!(o0 instanceof DV dv)) return error("not a dinamic variable: " + o0);
			var val = car(o, 1);
			var x = car(o, 2);
			var oldVal = dv.val;
			try {
				dv.val = val;
				return pipe(()-> evaluate(e, x), res->res, x, e);
			}
			finally {
				dv.val = oldVal;
			}
		}
		public String toString() { return "vmDLet"; }
	}
	
	
	// Error handling
	Object rootPrompt = new Object() { public String toString() { return "rootPrompt"; }};
	Object pushRootPrompt(Object x) { return list(new PushPrompt(), rootPrompt, x); }
	<T> T error(String msg) {
		return error(msg, null);
	}
	<T> T error(String msg, Throwable cause) {
		var userBreak = theEnvironment.get("user-break");
		var exc = new Error(msg, cause); 
		if (userBreak == null) throw exc;
		return (T) evaluate(theEnvironment, cons(userBreak, list(exc)));
	}
	class Error extends RuntimeException {
		private static final long serialVersionUID = 1L;
		public Error(Throwable cause) { super(cause); }
		public Error(String message) { super(message); }
		public Error(String message, Throwable cause) { super(message, cause); }
	}
	class Value extends RuntimeException {
		private static final long serialVersionUID = 1L;
		Object value; Value(Object value) { super(Vm.this.toString(value)); this.value = value; }
	}
	class PTree {
		private Object pt, ep;
		private Set syms = new HashSet<Symbol>();
		PTree(Object pt, Object ep) { this.pt = pt; this.ep = ep; }
		Object check() { 
			if (pt != nil && pt != ignore) {	var msg = check(pt); if (msg != null) return msg; }
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
			return c.cdr == nil ? null : check(c.cdr);
		}
	}
	Object checkPt(Object pt) { return checkPt(pt, null); }
	Object checkPt(Object pt, Object ep) { return new PTree(pt, ep).check(); }
	@SuppressWarnings("preview")
	int args(Apv apv) {
		return switch(apv.cmb) {
			case Opv opv-> opv.p == nil ? 0 : opv.p instanceof Cons c && c.cdr == nil && (c.car == ignore || c.car instanceof Symbol) ? 1 : Integer.MAX_VALUE;
			case JFun jFun-> jFun.jfun instanceof Supplier ? 0 : jFun.jfun instanceof Function ? 1 : Integer.MAX_VALUE;
			default-> Integer.MAX_VALUE;
		};
	}
	int checkO(Object op, Object o, int expt, Class ... cls) {
		var len=len(o); if (len == expt) { checkO(op, o, cls); return len; }
		return error((len < expt ? "less" : "more") + " then " + expt + " operands to combine: " + op + " with: " + o);
	}
	int checkO(Object op, Object o, int min, int max, Class ... cls) {
		var len=len(o); if (len >= min && (max == -1 || len <= max)) { checkO(op, o, cls); return len; } 
		return error((len < min ? "less then " + min : "more then " + max) + " operands to combine: " + op + " with: " + o);
	}
	int checkO(Object op, Object o, Class ... cls) {
		if (o == nil) return 0;
		int i=0; for (var oo=o; i<cls.length && o instanceof Cons c; i+=1, o=c.cdr) {
			var o0 = c.car; var cl=cls[i]; if (cl == null || cl.isInstance(o0)) continue;
			return error("not a " + toString(cls[i]) + ": " + o0 + " to combine: " + op + " with: " + oo);
		}
		return i;
	}
	
	
	// Utilities
	<T> Object list(T ... args) {
		return arrayToList(true, args);
	}
	<T> Object listStar(T ... args) {
		return arrayToList(false, args);
	}
	<T> Object arrayToList(boolean b, T ... args) {
		var len = args.length-1;
		var c = b || len < 0 ? nil : args[len];
		for (var i=len-(b?0:1); i>=0; i-=1) c = cons(args[i], c);
		return c;
	}
	Object[] listToArray(Object c) {
		return listToArray(c, 0);
	}
	Object[] listToArray(Object c, int i) {
		return listToArray(c, i, Object.class);
	}
	<T> T[] listToArray(Object c, Class<T> cl) {
		return (T[]) listToArray(c, 0, cl);
	}
	<T> T[] listToArray(Object o, int i, Class<T> cl) {
		var res = new ArrayList<T>();
		for (; o instanceof Cons c; o = c.cdr) if (i-- <= 0) res.add(car(c));
		return res.toArray((T[]) Array.newInstance(cl, 0));
	}
	Object reverseList(Object list) {
		Object res = nil;
		for (; list != nil; list = cdr(list)) res = cons(car(list), res);
		return res;
	}
	Object listToListStar(Object h) {
		if (!(h instanceof Cons hc)) return h;
		if (hc.cdr == nil) return hc.car;
		if (hc.cdr instanceof Cons c1) {
			if (c1.cdr == nil) { hc.cdr = c1.car; return hc; }
			if (c1.cdr instanceof Cons c2) {
				while (c2.cdr != nil && c2.cdr instanceof Cons c3) { c1=c2; c2=c3; }
				if (c2.cdr == nil) { c1.cdr = c2.car;  return hc; }
			}
		}
		return error("not a proper list: " + toString(hc));
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
		return objs instanceof Throwable ? vmAssert(expr) : vmAssert(expr, parseBytecode(objs)); 
	}
	boolean vmAssert(Object ... objs) {
		return vmAssert((String) null, objs[0], objs.length == 1 ? new Object[] {} : new Object[] { objs[1] }); 
	}
	boolean vmAssert(String name, Object expr, Object ... objs) {
		try {
			var env = env(theEnvironment);
			var val = pushSubcontBarrier(null, env, expr, ()-> evaluate(env, expr));
			if (objs.length == 0) print(eIfnull(name, ()-> "test "+ name + ": "), " ", expr, " should throw but is ", val);
			else {
				var expt = objs[0];
				if (equals(val, expt)) return true;
				print(eIfnull(name, ()-> "test "+ name + ": "), expr, " should be ", expt, " but is ", val);
			}
		}
		catch (Throwable t) {
			if (objs.length == 0) return true;
			if (stack) t.printStackTrace(out);
			else print(expr, " throw ", t);
		}
		return false;
	}
	
	
	// Bytecode parser
	Object parseBytecode(Object o) {
		if (o instanceof String s) return switch(s) { case "#inert"-> inert; case "#ignore"-> ignore; default-> symbol(s); };
		if (o instanceof Object[] a) return parseBytecode(a);
		return o;
	}
	Object parseBytecode(Object ... objs) {
		if (objs.length == 0) return nil;
		if (objs.length == 2 && objs[0].equals("wat-string")) return objs[1];
		Object head = cons(parseBytecode(objs[0]), nil), cons = head;
		for (int i=1; i<objs.length; i+=1) {
			var obj = objs[i];
			if (obj == null || !obj.equals(".")) {
				cons = ((Cons) cons).cdr = cons(parseBytecode(objs[i]), nil);
				continue;
			}
			if (i != objs.length-2) throw new Error(". not is the penultimate element in " + objs);
			((Cons) cons).cdr = parseBytecode(objs[i+1]);
			return head;
		}
		return head;
	}
	
	
	// JNI
	interface ArgsList extends Function {}
	class JFun implements Combinable {
		Object jfun;
		JFun(Object jfun) { this.jfun = jfun; };
		@SuppressWarnings("preview")
		public Object combine(Env e, Object o) {
			try {
				return switch (jfun) {
					case Supplier s-> { checkO(jfun, o, 0); yield s.get(); }  
					case ArgsList a-> a.apply(o);  
					case Function f-> { checkO(jfun, o, 1); yield f.apply(car(o)); }  
					case BiFunction f-> { checkO(jfun, o, 2); yield f.apply(car(o), car(o, 1)); }
					case Field f-> { checkO(jfun, o, 1, 2); if (len(o) <= 1) yield f.get(car(o)); f.set(car(o), car(o, 1)); yield inert; }
					case Method mt-> {
						var pc = mt.getParameterCount();
						if (!mt.isVarArgs()) checkO(jfun, o, 1+pc); else checkO(jfun, o, pc, -1);
						yield mt.invoke(car(o), listToArray(cdr(o)));
					}
					case Constructor c-> { checkO(jfun, o, c.getParameterCount()); yield c.newInstance(listToArray(o)); }
					default -> error("not a combine " + jfun);
				};
			}
			catch (Exception exc) {
				throw exc instanceof RuntimeException rte ? rte : new RuntimeException(exc);
			}
		}
		public String toString() {
			var intefaces = Arrays.stream(jfun.getClass().getInterfaces()).map(i-> Vm.this.toString(i)).collect(joining(" "));
			return "{JFun" + eIf(intefaces.isEmpty(), ()-> " " + intefaces) + " " + jfun + "}"; }
	}
	boolean isjFun(Object jfun) {
		return isInstance(jfun, Supplier.class, ArgsList.class, Function.class, BiFunction.class, Executable.class, Field.class);
	}
	JFun jFun(Object jFun) {
		return /*jfun instanceof JFun ? jfun :*/ isjFun(jFun) ? new JFun(jFun) : error("no a jFun: " + jFun);
	}
	Apv jWrap(Object jfun) {
		return wrap(jFun(jfun));
	}
	
	@SuppressWarnings("preview")
	Object jInvoke(String name) {
		if (name == null) return error("method name is null");
		return jWrap(
			(ArgsList) o-> {
				if (!(o instanceof Cons)) return error("no operands for executing: " + name) ;  
				Object o0 = car(o);
				if (o0 == null) return error("receiver is null");
				try {
					switch (name) {
						case "getField": {
							checkO(o0, o, 2, Class.class, String.class);
							var cl = (Class) o0;
							var fName = (String) car(o, 1);
							return jWrap(cl.getField(fName));
						}
						case "getMethod": {
							checkO(o0, o, 2, -1, Class.class, String.class);
							var cl = (Class) o0;
							var mName = (String) car(o, 1);
							return jWrap(cl.getMethod(mName, listToArray(o, 2, Class.class)));
						}
						case "invoke": {
							if (o0 instanceof Apv apv) o0 = apv.cmb;
							if (o0 instanceof JFun f) o0 = f.jfun; 
							if (!(o0 instanceof Method mt)) return error("not a Method " + toString(o0));
							var pc = mt.getParameterCount(); if (!mt.isVarArgs()) checkO(o0, o, 2+pc); else checkO(o0, o, 1+pc, -1); 
							return mt.invoke(car(o, 1), listToArray(o, 2));
						}
						case "getConstructor": {
							checkO(o0, o, 1, -1, Class.class);
							var cl = (Class) o0;
							return jWrap(cl.getConstructor(listToArray(o, 1, Class.class)));
						}
						case "newInstance": {
							if (o0 == Array.class) {
								checkO(o0, o, 2, -1, Class.class, Class.class);
								var cl = (Class) car(o, 1);
								return newInstance(cl, stream(listToArray(o, 2, Integer.class)).mapToInt(i->i).toArray() );
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
		return (name.equals("new") ? "constructor" : "method: " + name) + toString(list(getClasses(args)));
	}
	Object jGetSet(String name) {
		if (name == null) return error("field name is null");
		return jWrap(
			(ArgsList) o-> {
				var len = checkO("jGetSet", o, 1, 2); 
				var o0 = car(o);
				// (.name class)        -> class.getField(name) -> field
				// (.name object)       -> object.getclass().getField(name).get(object) -> field.get(object) 
				// (.name object value) -> object.getClass().getField(name).set(object,value) -> field.set(object, value) 
				Field field = getField(o0 instanceof Class cl ? cl : o0.getClass(), name);
				if (field == null) return error("not found field: " + name + " in: " + toString(o0));
				if (o0 instanceof Class) return jWrap(field);
				try {
					if (len == 1) return field.get(o0);
					field.set(o0, car(o, 1)); return inert;
				}
				catch (Exception e) {
					return error("can't " + (len==1 ? "get" : "set") + " " + name + " of " + toString(o0) + eIf(len == 1, ()-> " with " + toString(car(o, 1))));
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
		bind(theEnvironment, symbol("vm-def"), new Def(), null);
		bind(theEnvironment, symbol("vm-begin"), begin, null);
		evaluate(theEnvironment,
			parseBytecode(
				$("vm-begin",
					// Basics
					$("vm-def", "vm-vau", new Vau()),
					$("vm-def", "vm-eval", wrap(new Eval())),
					$("vm-def", "vm-make-environment", jWrap((ArgsList) o-> env(checkO("env", o, 0, 1, Env.class) == 0 ? null : car(o)))),
					$("vm-def", "vm-wrap", jWrap((Function<Object, Object>) this::wrap)),
					$("vm-def", "vm-unwrap", jWrap((Function<Object, Object>) this::unwrap)),
					// Values
					$("vm-def", "vm-cons", jWrap((BiFunction<Object, Object, Object>) this::cons)),
					$("vm-def", "vm-cons?", jWrap((Function<Object, Boolean>) obj-> obj instanceof Cons)),
					$("vm-def", "vm-nil?", jWrap((Function<Object, Boolean>) obj-> obj == nil)),
					$("vm-def", "vm-string-to-symbol", jWrap((Function<String, Symbol>) this::symbol)),
					$("vm-def", "vm-symbol?", jWrap((Function<Object, Boolean>) obj-> obj instanceof Symbol)),
					$("vm-def", "vm-symbol-name", jWrap((Function<Symbol, String>) sym-> sym.name)),
					// First-order Control
					$("vm-def", "vm-if", new If()),
					$("vm-def", "vm-loop", new Loop()),
					$("vm-def", "vm-catch", new Catch()),
					$("vm-def", "vm-throw", jWrap((ArgsList) o-> { throw checkO("throw", o, 0, 1) == 0 ? new Value(inert) : apply(v-> v instanceof Error err ? err : new Value(v), car(o)); })),
					$("vm-def", "vm-finally", new Finally()),
					$("vm-def", "vm-catch-tag", new CatchTag()),
					$("vm-def", "vm-throw-tag", new ThrowTag()),
					$("vm-def", "catchAll", new CatchAll()),
					$("vm-def", "throwAll", new ThrowAll()),
					// Delimited Control
					$("vm-def", "vm-push-prompt", new PushPrompt()),
					$("vm-def", "vm-take-subcont", wrap(new TakeSubcont())),
					$("vm-def", "vm-push-prompt-subcont", wrap(new PushPromptSubcont())),
					// Dynamically-scoped Variables
					$("vm-def", "vm-dnew", wrap(new DNew())),
					$("vm-def", "vm-dlet", new DLet()),
					$("vm-def", "vm-dref", wrap(new DRef())),
					// Errors
					$("vm-def", "vm-root-prompt", rootPrompt),
					$("vm-def", "vm-error", jWrap((Function<String, Object>) this::error)),
					// JS Interface
					$("vm-def", "vm-jinvoke", jWrap((Function<String,Object>) this::jInvoke)),
					$("vm-def", "vm-jgetset", jWrap((Function<String,Object>) this::jGetSet)),
					$("vm-def", "instanceof", jWrap((BiFunction<Object,Class,Boolean>) (o,c)-> c.isInstance(o))),
					// Utilities
					$("vm-def", "vm-list", jWrap((ArgsList) o-> o)),
					$("vm-def", "vm-list*", jWrap((ArgsList) this::listToListStar)),
					$("vm-def", "vm-list-to-array", jWrap((Function<Object,Object[]>) this::listToArray)),
					$("vm-def", "vm-array-to-list", jWrap((BiFunction<Boolean,Object[],Object>) this::arrayToList)),
					$("vm-def", "vm-reverse-list", jWrap((Function) this::reverseList)),
					// 
					$("vm-def", "+", jWrap((BinaryOperator<Object>)	(a,b)->	a instanceof Integer && b instanceof Integer ? ((Integer)a) + ((Integer)b) : toString(a) + toString(b))),
					$("vm-def", "*", jWrap((BinaryOperator<Integer>) (a,b)-> a * b)),
					$("vm-def", "-", jWrap((BinaryOperator<Integer>) (a,b)-> a - b)),
					$("vm-def", "/", jWrap((BinaryOperator<Integer>) (a,b)-> a / b)),
					//
					$("vm-def", "!", jWrap((Function<Boolean,Boolean>) a-> !a)),
					$("vm-def", "not", jWrap((Function<Boolean,Boolean>) a-> !a)),
					$("vm-def", "<", jWrap((BiFunction<Integer,Integer,Boolean>) (a,b)-> a < b)),
					$("vm-def", ">", jWrap((BiFunction<Integer,Integer,Boolean>) (a,b)-> a > b)),
					$("vm-def", "<=", jWrap((BiFunction<Integer,Integer,Boolean>) (a,b)-> a <= b)),
					$("vm-def", ">=", jWrap((BiFunction<Integer,Integer,Boolean>) (a,b)-> a >= b)),
					//
					$("vm-def", "vm-quote", $("vm-vau", $("a"), ignore, "a")),
					$("vm-def", "vm-lambda", $("vm-vau", $("formals", ".", "body"), "env",
						$("vm-wrap", $("vm-eval", $("vm-list*", "vm-vau", "formals", "#ignore", "body"), "env")))),
					$("vm-def", "vm-jambda", jFun((ArgsList) o-> lambda(car(o), car(o,1), cdr(o,1)))),
					
					$("vm-def", "==", jWrap((BiFunction<Object,Object,Boolean>) (a,b)-> a == b)),
					$("vm-def", "!=", jWrap((BiFunction<Object,Object,Boolean>) (a,b)-> a != b)),
					$("vm-def", "eq?", jWrap((BiFunction<Object,Object,Boolean>) (a,b)-> equals(a, b))),
					$("vm-def", "assert", jFun((ArgsList) o-> { checkO("assert", o, 1, 2); return vmAssert(listToArray(o)); } )),
					$("vm-def", "test", jFun((ArgsList) o-> { checkO("test", o, 2, 3); return vmAssert(toString(car(o)), car(o,1), listToArray(cdr(o,1))); } )),
					$("vm-def", "toString", jWrap((Function<Object,String>) obj-> toString(obj))),
					$("vm-def", "log", jWrap((ArgsList) o-> log(listToArray(o)))),
					$("vm-def", "print", jWrap((ArgsList) o-> print(listToArray(o)))),
					$("vm-def", "write", jWrap((ArgsList) o-> write(listToArray(o)))),
					$("vm-def", "load", jWrap((Function<String, Object>) nf-> uncked(()-> eval(readString(nf))))),
					$("vm-def", "trace", jWrap((ArgsList) o-> { if (checkO("trace", o, 0, 1, Boolean.class) == 0) return trace; trace=car(o); return inert; })),
					$("vm-def", "stack", jWrap((ArgsList) o-> { if (checkO("stack", o, 0, 1, Boolean.class) == 0) return stack; stack=car(o); return inert; })),
					$("vm-def", "thenv", jWrap((ArgsList) o-> { if (checkO("thenv", o, 0, 1, Boolean.class) == 0) return thenv; thenv=car(o); return inert; })),
					$("vm-def", "tco", jWrap((ArgsList) o-> { if (checkO("tco", o, 0, 1, Boolean.class) == 0) return tco; tco=car(o); return inert; }))
				)
			)
		);
	}
	
	
	// API
	public Object exec(Object bytecode) {
		var wrapped = pushRootPrompt(cons(new Begin(true), parseBytecode(bytecode)));
		return pushSubcontBarrier(null, theEnvironment, wrapped, ()-> evaluate(theEnvironment, wrapped));
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
		try (ObjectOutputStream oos = new ObjectOutputStream(new FileOutputStream("build/" + fileName))) {
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
		try (ObjectInputStream ois = new ObjectInputStream(new FileInputStream("build/" + fileName))) {
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
		boolean inEscape = false, inString = false, inComment=false, sMlComment=false, inMlComment=false, eMlComment=false;
		do {
			var oc = close-open;
			out.print(oc==0 ? ">" : "%+d%s>".formatted(oc, oc>0 ? "(" : ")"));
			for (int c; (c = in.read()) != '\n';) {
				if (inEscape) {
					inEscape = false;
				}
				else if (inString) switch (c) {
					case '\\'-> inEscape = true;
					case '"'-> inString = false;
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
					case '#': inMlComment = true;
					default : sMlComment = false;
				}
				else switch (c) {
					case '"'-> inString = true;
					case ';'-> inComment = true;
					case '|'-> sMlComment = true;
					case '('-> open += 1;
					case ')'-> close += 1;
				}
				if (c >= 32) s.append((char) c);
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
		//print(parseBytecode(parse("(vm-cons 1 2)"))); // -> ((vm-cons 1 2))
		/*
		var Throw = new Throwable();
		vmAssert("1", 1);
		vmAssert("1 2 3", 3);
		vmAssert("(vm-cons 1 2)", $(1,".",2));
		vmAssert("(vm-list 1 2 3)", $(1,2,3));
		vmAssert("(vm-list* 1 2 3 4)", $(1,2,3,".",4));
		vmAssert("(vm-array-to-list #t (vm-list-to-array (vm-list 1 2 3 4)))", $(1,2,3,4));
		vmAssert("(vm-array-to-list #f (vm-list-to-array (vm-list 1 2 3 4)))", $(1,2,3,".",4));
		vmAssert("(vm-reverse-list (vm-list 1 2 3 4))", $(4,3,2,1));
		
		vmAssert("(+ 1 2)", 3);
		vmAssert("(* 3 2)", 6);
		vmAssert("(* (* 3 2) 2)", 12);
		
		vmAssert("((vm-vau a #ignore a) 1 2)", $(1,2));
		vmAssert("((vm-vau (a b) #ignore b) 1 2)", 2);
		vmAssert("((vm-vau (a . b) #ignore b) 1 2 3)", $(2,3));
		vmAssert("(vm-def x (vm-list 1)) x", $(1));
		vmAssert("(vm-def x 1)((vm-wrap(vm-vau (a) #ignore a)) x)", 1);
		
		vmAssert("(vm-def () 1) a", Throw);
		vmAssert("(vm-def a (vm-list 1)) a", $(1));
		vmAssert("(vm-def (a) (vm-list 1)) a", 1);
		vmAssert("(vm-def a (vm-list 1 2 3)) a", $(1,2,3));
		vmAssert("(vm-def (a) 1 2 3) a", Throw);
		vmAssert("(vm-def 2 1)", Throw);
		vmAssert("(vm-def (a) (vm-list 1)) a", 1);
		vmAssert("(vm-def (a b) (vm-list 1 2)) a b", 2);
		vmAssert("(vm-def (a . b) (vm-list 1 2 3)) a b", $(2,3)); 
		
		vmAssert("(vm-vau (a . 12) #ignore 0)", Throw);
		vmAssert("(vm-def (a b 12) 1)", Throw);
		
		vmAssert("(vm-quote (a b c))", $("a","b","c"));
		
		vmAssert("(vm-def 2 1)", Throw);
		vmAssert("(vm-def a 1) a", 1);
		vmAssert("(vm-def (a . b) (vm-list 1 2 3)) a b", $(2,3));
		vmAssert("(vm-def define vm-def) (define a 1) a", 1);
		vmAssert("(vm-def $define! vm-def) ($define! a 1) a", 1);
		vmAssert("(vm-def $define! vm-def) $define!", theEnvironment.get("vm-def"));
		vmAssert("(vm-if #t 1 2)", 1);
		vmAssert("(vm-if #f 1 2)", 2);
		vmAssert("(vm-if #null 1 2)", 2);
		//*/
		/*
		eval("""
			(assert (vm-def a 1) #inert)
			(assert (vm-def a)         ) ;throw
		""");
		//*/
		
		//exec(parse(readString("boot.wat")));
		//compile("boot.wat");
		//exec(readBytecode("boot.wat"));
		var milli = currentTimeMillis();
		eval(readString("boot.wat"));
		eval(readString("test.wat"));
		eval(readString("testJni.wat"));
		print("start time: " + (currentTimeMillis() - milli)); 
		repl();
	}
}
