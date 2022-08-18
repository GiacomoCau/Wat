package Wat;

import static List.Parser.parse;
import static Wat.Utility.$;
import static Wat.Utility.eIf;
import static Wat.Utility.getClasses;
import static Wat.Utility.getExecutable;
import static Wat.Utility.getField;
import static Wat.Utility.isInstance;
import static Wat.Utility.reorg;
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
import java.util.function.Consumer;
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
	apv0: zero args applicative combiner
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
	
	boolean trace = false;
	boolean stack = false;
	boolean thenv = false;
	boolean jfapv = false;
	
	interface Combinable { Object combine(Resumption r, Env e, Object o); }
	
	
	// Continuations
	class Continuation {
		Function<Resumption, Object> f; Continuation next; Object dbg; Env e;
		Continuation(Function<Resumption, Object> f, Continuation next, Object dbg, Env e) {
			this.f = f; this.next = next; this.dbg = dbg; this.e = e;
		}
		public String toString() { return "[Continuation %s %s %s]".formatted(f, dbg, e); }
		Object apply(Env e, Apv apv0) {
			return f.apply(new Resumption(next, ()-> combine(null, e, apv0, nil)));
		}
	}
	class Resumption {
		Continuation k; Supplier<Object> s;
		Resumption(Continuation k, Supplier<Object> s) { this.k=k; this.s=s; }
		public String toString() { return "[Resumption %s %s]".formatted(s, k); }
        Object resume() {
        	var k = this.k; this.k = k.next; return k.f.apply(this);
        }
	};
	class Suspension {
		Object prompt; Combinable handler; Continuation k;
		Suspension(Object prompt, Combinable handler) { this.prompt = prompt; this.handler = handler; }
		public String toString() { return "[Suspension %s %s %s]".formatted(prompt, handler, k); }
		Suspension suspend(Function<Resumption, Object> f, Object dbg, Env e) {
			k = new Continuation(f, k, dbg, e); return this;
		}
	}
	
	
	// Forms
	class Nil { public String toString() { return "()"; }};
	public Nil nil = new Nil();
	
	class Inert { public String toString() { return "#inert"; }};
	public Inert inert = new Inert();
	
	class Ignore { public String toString() { return "#ignore"; }};
	public Ignore ignore = new Ignore();
	
	
	// Evaluation Core
	@SuppressWarnings("preview")
	Object evaluate(Resumption r, Env e, Object o) {
		if (trace) print("evaluate: ", o);
		return switch (o) {
			case null, default-> o;
			case Symbol s-> e.lookup(s.name);
			case Cons c-> {
				Object op = r != null ? r.resume() : evaluate(null, e, c.car);
				yield op instanceof Suspension s ? s.suspend(rr-> evaluate(rr, e, o), o, e) : combine(null, e, op, c.cdr);
			}
		};
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
		var	msg = bind2(e, lhs, rhs); if (msg == null) return inert;
		return error(msg + " for bind: " + lhs + eIf(exp == null, ()-> " of: " + exp) + " with: " + rhs);
	}
	@SuppressWarnings("preview")
	Object bind2(Env e, Object lhs, Object rhs) {
		return switch (lhs) {
			case Ignore i-> null;
			case Symbol s-> e.bind(s.name, rhs);  
			case Nil n-> rhs == nil ? null : "too many arguments";
			case Cons lc-> {
				if (!(rhs instanceof Cons rc)) yield "too few arguments";
				var msg = bind2(e, lc.car, rc.car);
				if (msg != null) yield msg;
				yield bind2(e, lc.cdr, rc.cdr);
			}
			default-> error("cannot bind: " + lhs);
		};
    }
	
	
	// Operative & Applicative Combiners
	Object combine(Resumption r, Env e, Object op, Object o) {
		if (trace) print(" combine: ", op, " ", o);
		if (op instanceof Combinable cmb) return cmb.combine(r, e, o);
		// per default le jFun dovrebbero essere operative e non applicative
		if (isjFun(op)) return jfapv
			? ((Combinable) jWrap(op)).combine(r, e, o) // jfun x default applicative
			: ((Combinable) jFun(op)).combine(r, e, o); // jfun x default operative
		return error("not a combiner: " + toString(op) + " in: " + cons(op, o));
	}
	
	class Opv implements Combinable  {
		Object p, ep, x; Env e;
		Opv(Object p, Object ep, Object x, Env e) { this.p = p; this.ep = ep; this.x = x; this.e = e; }
		public Object combine(Resumption r, Env e, Object o) {
			var xe = env(this.e); bind(xe, p, o, this); bind(xe, ep, e, this); return evaluate(null, xe, x);
		}
		public String toString() { return "[Opv " + Vm.this.toString(p) + " " + Vm.this.toString(ep) + " " + Vm.this.toString(x) + "]"; }
	}
	class Apv implements Combinable  {
		Combinable cmb;
		Apv(Combinable cmb) { this.cmb = cmb; }
		public Object combine(Resumption r, Env e, Object o) {
			var args = r != null ? r.resume() : evalArgs(null, e, o, nil);
			return args instanceof Suspension s ? s.suspend(rr-> combine(rr, e, o), o, e) : cmb.combine(null, e, args);
		}
		Object evalArgs(Resumption r, Env e, Object todo, Object done) {
			if (todo == nil) return reverseList(done);
			var arg = car(todo);
			var res = r != null ? r.resume() : evaluate(null, e, arg);
			return res instanceof Suspension s ? s.suspend(rr-> evalArgs(rr, e, todo, done), arg, e) : evalArgs(null, e, cdr(todo), cons(res, done));
		}
		public String toString() { return "[Apv " + Vm.this.toString(cmb) + "]"; }
	}
	Object wrap(Object arg) { return arg instanceof Combinable cmb ? new Apv(cmb) : error("cannot wrap: " + arg); } // type check
	Object unwrap(Object arg) { return arg instanceof Apv apv ? apv.cmb : error("cannot unwrap: " + arg); } // type check
	
	
	// Built-in Combiners
	class Vau implements Combinable  {
		public Object combine(Resumption r, Env e, Object o) {
			checkO(this, o, 3); // o = (pt ep x)
			var pt = car(o);
			var ep = car(o, 1);
			var msg = checkPt(pt, ep); if (msg != null) return error(msg + " of: " + cons(this, o));
			return new Opv(pt, ep, car(o, 2), e);
		}
		public String toString() { return "vmVau"; }
	};
	class Def implements Combinable  {
		public Object combine(Resumption r, Env e, Object o) {
			checkO(this, o, 2); // o = (pt arg)
			var pt = car(o);
			if (!(pt instanceof Symbol)) {
				if (!(pt instanceof Cons)) return error("not a symbol: " + pt + " in: " + cons(this, o));
				var msg = checkPt(pt); if (msg != null) return error(msg + " of: " + cons(this, o));
			}
			var arg = car(o, 1);
			var res = r != null ? r.resume() : evaluate(null, e, arg);
			return res instanceof Suspension s ? s.suspend(rr-> combine(rr, e, o), arg, e) : bind(e, pt, res, cons(this, o));
		}
		public String toString() { return "vmDef"; }
	};
	class Eval implements Combinable  {
		public Object combine(Resumption r, Env e, Object o) {
			checkO(this, o, 2); // o = (x eo)
			var x = car(o);
			var o1 = car(o, 1);
			if (!(o1 instanceof Env eo)) return error("not an Env: " + o1);
			return evaluate(r, eo, x);
		}
		public String toString() { return "vmEval"; }
	}
	
	
	// First-order Control
	class Begin implements Combinable  {
		boolean root;
		Begin() { } 
		Begin(boolean root) { this.root = root; } 
		public Object combine(Resumption r, Env e, Object o) {
			// o = (... xs)
			return o == nil ? null : begin(r, e, o);
		}
		Object begin(Resumption r, Env e, Object xs) {
			if (trace && root && r == null) print("\n--------");
			var o0 = car(xs);
			var res = r != null ? r.resume() : evaluate(null, e, o0);
			return res instanceof Suspension s ? s.suspend(rr-> begin(rr, e, xs), o0, e)
				: ((Function) cdr-> cdr == nil ? res : begin(null, e, cdr)).apply(cdr(xs))
			;
		}
		public String toString() { return "vmBegin" + eIf(!root, "*"); }
	}
	class If implements Combinable  {
		public Object combine(Resumption r, Env e, Object o) {
			checkO(this, o, 3); // o = (test then else) 
			var test = car(o);
			var res = r != null ? r.resume() : evaluate(null, e, test);
			return res instanceof Suspension s ? s.suspend(rr-> combine(rr, e, o), test, e)
				: evaluate(null, e, car(o, res != null && res != nil && res instanceof Boolean b && b ? 1 : 2))
			;
		}
		public String toString() { return "vmIf"; }
	}
	class Loop implements Combinable  {
		public Object combine(Resumption r, Env e, Object o) {
			checkO(this, o, 1); // o = (x)
			var first = true; // only resume once
			while (true) {
				var o0 = car(o);
				var res = first && r != null ? r.resume() : evaluate(null, e, o0);
				first = false;
				if (res instanceof Suspension s) return s.suspend(rr-> combine(rr, e, o), o0, e);
			}
		}
		public String toString() { return "vmLoop"; }
	}
	class Catch implements Combinable  {
		public Object combine(Resumption r, Env e, Object o) {
			checkO(this, o, 2); // o = (x handler)
			var x = car(o);
			var handler = car(o, 1);
			if (!(handler instanceof Apv apv1 && args(apv1) == 1)) return error("not a one arg applicative combiner: " + handler); 
			Object res = null;
			try {
				res = r != null ? r.resume() : evaluate(null, e, x);
			}
			catch (Error | Value exc) {
				// unwrap handler to prevent eval if exc is sym or cons
				res = Vm.this.combine(null, e, unwrap(apv1), list(exc instanceof Value v ? v.value : exc));
			}
			if (res instanceof Suspension s) s.suspend(rr-> combine(rr, e, o), x, e);
			return res;
		}
		public String toString() { return "vmCatch"; }
	}
	class Finally implements Combinable  {
		@SuppressWarnings("finally")
		public Object combine(Resumption r, Env e, Object o) {
			checkO(this, o, 2); // o = (prot cleanup)
			var prot = car(o);
			var cleanup = car(o, 1);
			Object res = null;
			try {
				res = r != null ? r.resume() : evaluate(null, e, prot);
				if (res instanceof Suspension s) s.suspend(rr-> combine(rr, e, o), prot, e);
			}
			finally {
				return res instanceof Suspension s ? s : doCleanup(null, e, cleanup, res);
			}
		}
		Object doCleanup(Resumption r, Env e, Object cleanup, Object res) {
			var fres = r != null ? r.resume() : evaluate(null, e, cleanup);
			if (fres instanceof Suspension s) s.suspend(rr-> doCleanup(rr, e, cleanup, res), cleanup, e);
			return fres;
		}
		public String toString() { return "vmFinaly"; }
	}
	
	
	// Delimited Control
	class PushPrompt implements Combinable  {
		public Object combine(Resumption r, Env e, Object o) {
			checkO(this, o, 2); // o = (prompt x)
			var prompt = car(o);
			var x = car(o, 1);
			var res = r != null ? r.resume() : evaluate(null, e, x);
			if (!(res instanceof Suspension s)) return res;
			if (s.prompt != prompt) return s.suspend(rr-> combine(rr, e, o), x, e);
			return Vm.this.combine(null, e, s.handler, cons(s.k, nil));
		}
		public String toString() { return "vmPushPrompt"; }
	}
	class TakeSubcont implements Combinable  {
		public Object combine(Resumption r, Env e, Object o) {
			checkO(this, o, 2); // o = (prompt handler)
			var prompt = car(o);
			var handler = car(o, 1);
			if (!(handler instanceof Apv apv1 && args(apv1) == 1)) return error("not a one arg applicative combiner: " + handler); 
			return new Suspension(prompt, apv1).suspend(rr-> Vm.this.combine(null, e, rr.s, nil), this, e);
		}
		public String toString() { return "vmTakeSubcont"; }
	}
	class PushSubcont implements Combinable  {
		public Object combine(Resumption r, Env e, Object o) {
			checkO(this, o, 2); // o = (k apv0)
			var o0 = car(o);
			if (!(o0 instanceof Continuation k)) return error("not a continuation: " + o0); 
			var o1 = car(o, 1);
			if (!(o1 instanceof Apv apv0 && args(apv0) == 0)) return error("not a zero args applicative combiner: " + o1);
			//var res = r != null ? r.resume() : new Resumption(k, ()-> Vm.this.combine(null, e, apv0, nil)).resume(); 
			//var res = r != null ? r.resume() : k.f.apply(new Resumption(k.next, ()-> Vm.this.combine(null, e, apv0, nil)));
			var res = r != null ? r.resume() : k.apply(e, apv0);
			if (res instanceof Suspension s) s.suspend(rr-> combine(rr, e, o), apv0, e);
			return res;
		}
		public String toString() { return "vmPushSubcont"; }
	}
	class PushPromptSubcont implements Combinable  {
		public Object combine(Resumption r, Env e, Object o) {
			checkO(this, o, 2); // o = (prompt k apv0)
			var prompt = car(o);
			var o1 = car(o, 1);
			if (!(o1 instanceof Continuation k)) return error("not a continuation: " + o1); 
			var o2 = car(o, 2);
			if (!(o2 instanceof Apv apv0 && args(apv0) == 0)) return error("not a zero args applicative combiner: " + o2);
			//var res = r != null ? r.resume() : new Resumption(k, ()-> Vm.this.combine(null, e, apv0, nil)).resume();
			//var res = r != null ? r.resume() : k.f.apply(new Resumption(k.next, ()-> Vm.this.combine(null, e, apv0, nil)));
			var res = r != null ? r.resume() : k.apply(e, apv0);
			if (!(res instanceof Suspension s)) return res;
			if (s.prompt != prompt) return s.suspend(rr-> combine(rr, e, o), apv0, e);
			return Vm.this.combine(null, e, s.handler, cons(s.k, nil));
		}
		public String toString() { return "vmPushPromptSubcont"; }
	}
	
	
	// Dynamic Variables
	class DV {
		Object val;
		DV(Object val) { this.val = val; }
		public String toString() { return "[DV " + val + "]"; }
	}
	class DNew implements Combinable  {
		public Object combine(Resumption r, Env e, Object o) {
			checkO(this, o, 1); // o = (x)
			return new DV(car(o));
		}
		public String toString() { return "vmDNew"; }
	}
	class DRef implements Combinable  {
		public Object combine(Resumption r, Env e, Object o) {
			checkO(this, o, 1); // o = (x)
			var x = car(o);
			return x instanceof DV dv ? dv.val : error("not a dinamic variable: " + x);
		}
		public String toString() { return "vmDRef"; }
	}
	class DLet implements Combinable  {
		public Object combine(Resumption r, Env e, Object o) {
			checkO(this, o, 3); // o = (dv val x)
			var o0 = car(o);
			if (!(o0 instanceof DV dv)) return error("not a dinamic variable: " + o0);
			var val = car(o, 1);
			var x = car(o, 2);
			var oldVal = dv.val;
			try {
				dv.val = val;
				var res = r != null ? r.resume() : evaluate(null, e, x);
				if (res instanceof Suspension s) s.suspend(rr-> combine(rr, e, o), x, e);
				return res;
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
		// var exc = t == null ? new Error(msg) : new Error(msg, t); // this throw
		var exc = new Error(msg, cause); 
		if (userBreak == null) throw exc;
		return (T) combine(null, theEnvironment, userBreak, list(exc));
	}
	class Error extends RuntimeException {
		private static final long serialVersionUID = 1L;
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
		return error("not " + expt + " operands for combine: " + op + " with: " + o);
	}
	int checkO(Object op, Object o, int min, int max, Class ... cls) {
		var len=len(o); if (len >= min && (max == -1 || len <= max)) { checkO(op, o, cls); return len; } 
		return error((len < min ? "less then " + min : eIf(max == -1, ()-> " or more then " + max)) + " operands for combine: " + op + " with: " + o);
	}
	int checkO(Object op, Object o, Class ... cls) {
		if (o == nil) return 0;
		int i=0; for (var oo=o; i<cls.length && o instanceof Cons c; i+=1, o=c.cdr) {
			var o0 = c.car; var cl=cls[i]; if (cl == null || cl.isInstance(o0)) continue;
			return error("not a " + toString(cls[i]) + ": " + o0 + " for combine: " + op + " with: " + oo);
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
		return (T) os[os.length - 1];
	}
	<T> T write(Object ... os) {
		for (var o: os) out.print(toString(true, o)); out.println();
		return (T) os[os.length - 1];
	}
	<T> T log(Object ... os) {
		int i=0; for (var o: os) out.print(eIf(i++ == 0, " ") + toString(o)); out.println();
		return (T) os[0];
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
	Void vmAssert(Object ... objs) {
		var expr = objs[0];
		try {
			var val = evaluate(null, env(theEnvironment), expr);
			if (objs.length == 1) print(expr, " should throw but is ", val);
			else {
				var expt = objs[1];
				if (equals(val, expt)) return null;
				print(expr, " should be ", expt, " but is ", val);
			}
		}
		catch (Throwable t) {
			if (objs.length == 1) return null;
			if (stack) t.printStackTrace(out);
			else print(expr, " throw ", t);
		}
		return null;
	}
	Void vmAssert(String str, Object objs) throws Exception {
		var expr = cons(new Begin(), parseBytecode(parse(str)));
		return objs instanceof Throwable ? vmAssert(expr) : vmAssert(expr, parseBytecode(objs)); 
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
	class JFun implements Combinable  {
		Object jfun;
		JFun(Object jfun) { this.jfun = jfun; };
		@SuppressWarnings("preview")
		public Object combine(Resumption r, Env e, Object o) {
			try {
				return switch (jfun) {
					case Supplier s-> { checkO(jfun, o, 0); yield s.get(); }  
					case ArgsList a-> a.apply(o);  
					case Function f-> { checkO(jfun, o, 1); yield f.apply(car(o)); }  
					case Consumer c-> { checkO(jfun, o, 1); c.accept(car(o)); yield inert; }
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
			return "[JFun" + eIf(intefaces.isEmpty(), ()-> " " + intefaces) + " " + jfun + "]"; }
	}
	boolean isjFun(Object jfun) {
		return isInstance(jfun, Supplier.class, ArgsList.class, Function.class, BiFunction.class, Consumer.class, Executable.class, Field.class);
	}
	Object jFun(Object jFun) {
		return /*jfun instanceof JFun ? jfun :*/ isjFun(jFun) ? new JFun(jFun) : error("no a jFun: " + jFun);
	}
	Object jWrap(Object jfun) {
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
		bind(theEnvironment, symbol("vm-begin"), new Begin(), null);
		evaluate(null, theEnvironment,
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
					$("vm-def", "vm-throw", jWrap((Consumer<Object>) obj-> { if (obj instanceof Error e) throw e; else throw new Value(obj); })),
					$("vm-def", "vm-catch", new Catch()),
					$("vm-def", "vm-finally", new Finally()), //,
					// Delimited Control
					$("vm-def", "vm-push-prompt", new PushPrompt()),
					$("vm-def", "vm-take-subcont", wrap(new TakeSubcont())),
					$("vm-def", "vm-push-subcont", wrap(new PushSubcont())),
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
					$("vm-def", "+", jWrap((BiFunction<Integer,Integer,Integer>) (a,b)-> a + b)),
					$("vm-def", "*", jWrap((BiFunction<Integer,Integer,Integer>) (a,b)-> a * b)),
					$("vm-def", "-", jWrap((BiFunction<Integer,Integer,Integer>) (a,b)-> a - b)),
					$("vm-def", "/", jWrap((BiFunction<Integer,Integer,Integer>) (a,b)-> a / b)),
					//
					$("vm-def", "!", jWrap((Function<Boolean,Boolean>) a-> !a)),
					$("vm-def", "not", jWrap((Function<Boolean,Boolean>) a-> !a)),
					$("vm-def", "<", jWrap((BiFunction<Integer,Integer,Boolean>) (a,b)-> a < b)),
					$("vm-def", ">", jWrap((BiFunction<Integer,Integer,Boolean>) (a,b)-> a > b)),
					$("vm-def", "<=", jWrap((BiFunction<Integer,Integer,Boolean>) (a,b)-> a <= b)),
					$("vm-def", ">=", jWrap((BiFunction<Integer,Integer,Boolean>) (a,b)-> a >= b)),
					//
					$("vm-def", "vm-quote", $("vm-vau", $("a"), ignore, "a")),
					$("vm-def", "==", jWrap((BiFunction<Object,Object,Boolean>) (a,b)-> a == b)),
					$("vm-def", "!=", jWrap((BiFunction<Object,Object,Boolean>) (a,b)-> a != b)),
					$("vm-def", "eq?", jWrap((BiFunction<Object,Object,Boolean>) (a,b)-> equals(a, b))),
					$("vm-def", "assert", jFun((ArgsList) o-> { checkO("assert", o, 1, 2); return vmAssert(listToArray(o)); } )),
					$("vm-def", "toString", jWrap((Consumer) obj-> toString(obj))),
					$("vm-def", "print", jWrap((ArgsList) o-> print(listToArray(o)))),
					$("vm-def", "write", jWrap((ArgsList) o-> write(listToArray(o)))),
					$("vm-def", "log", jWrap((ArgsList) o-> log(listToArray(o)))),
					$("vm-def", "trace", jWrap((ArgsList) o-> { if (checkO("trace", o, 0, 1, Boolean.class) == 0) return trace; trace=car(o); return inert; })),
					$("vm-def", "stack", jWrap((ArgsList) o-> { if (checkO("stack", o, 0, 1, Boolean.class) == 0) return stack; stack=car(o); return inert; })),
					$("vm-def", "thenv", jWrap((ArgsList) o-> { if (checkO("thenv", o, 0, 1, Boolean.class) == 0) return thenv; thenv=car(o); return inert; }))
				)
			)
		);
	}
	
	
	// API
	public Object exec(Object bytecode) {
		var wrapped = pushRootPrompt(cons(new Begin(true), parseBytecode(bytecode)));
		var res = evaluate(null, theEnvironment, wrapped);
		if (res instanceof Suspension s) throw new Error("prompt not found: " + s.prompt);
		return res;
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
		vmAssert("(vm-def $define! vm-def) $define!", TheEnvironment.get("vm-def"));
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
		eval(readString("boot.wat"));
		eval(readString("test.wat"));
		eval(readString("testJni.wat"));
		repl();
	}
}
