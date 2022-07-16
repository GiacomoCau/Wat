package Wat;

import static List.Parser.parse;
import static Wat.Utility.$;
import static Wat.Utility.getClasses;
import static Wat.Utility.getExecutable;
import static Wat.Utility.getField;
import static Wat.Utility.isInstance;
import static Wat.Utility.reorg;
import static java.lang.System.in;
import static java.lang.System.out;

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
	apv0: applicative combiner with 0 arguments
	apv1, handler: applicative combiner with 1 argument
	p: parameter
	ps: parameters
	pt: parameters tree
	arg: argument
	args: arguments
	e: environment
	eo: environment operand 
	ep: environment parameter
	xe: extended environment
	s: sospension
	r: resumption
	f: function
	s: supplier
	k, next, continuation: stackframe
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
	
	interface Evaluable { Object eval(Resumption r, Env e); }
	interface Bindable { Object bind(Env e, Object rhs); }
	interface Combinable { Object combine(Resumption r, Env e, Object o); int param=0; }
	
	
	/* Continuations */
	record StackFrame(Function<Resumption, Object> f, StackFrame next, Object dbg, Env e) {
		public String toString() { return "[StackFrame %s %s %s]".formatted(f, dbg, e); }
	}
	
	class Resumption {
		StackFrame k; Supplier<Object> s;
		Resumption(StackFrame k, Supplier<Object> s) { this.k = k; this.s = s; }
		public String toString() { return "[Resumption %s %s]".formatted(s, k); }
	};
	Object resumeFrame(Resumption r) { return resumeFrame(r.k, r.s); }
	Object resumeFrame(StackFrame k, Supplier<Object> s) { 	return k.f.apply(new Resumption(k.next, s)); }
	
	class Suspension {
		Object prompt; Combinable handler; StackFrame continuation;
		Suspension(Object prompt, Combinable handler) { this.prompt = prompt; this.handler = handler; }
		public String toString() { return "[Suspension %s %s %s]".formatted(prompt, handler, continuation); }
	}
	Suspension suspendFrame(Suspension suspension, Function<Resumption, Object> f) {
		return suspendFrame(suspension, f, null, null);
	}
	Suspension suspendFrame(Suspension suspension, Function<Resumption, Object> f, Object dbg, Env e) {
		suspension.continuation = new StackFrame(f, suspension.continuation, dbg, e);
		return suspension;
	}
	
	
	/* Forms */
	class Nil implements Bindable {
		public Object bind(Env e, Object rhs) { return rhs == nil ? null : "too many arguments"; /* + " NIL expected, but got: " + rhs.toString();*/ }
		public String toString() { return "()"; }
	};
	public Nil nil = new Nil();
	
	static class Ign implements Bindable {
		public Object bind(Env e, Object rhs) { return null; } 
		public String toString() { return "#ignore"; }
	};
	public static Ign ign = new Ign();
	
	
	/* Evaluation Core */
	Object evaluate(Resumption r, Env e, Object o) {
		if (trace) print("evaluate: ", o);
		return o instanceof Evaluable x ? x.eval(r, e) : o;
	}
	
	class Sym implements Evaluable, Bindable {
		String name;
		Sym(String name) { this.name = name; }
		public Object eval(Resumption r, Env e) { return e.lookup(name); }
		public Object bind(Env e, Object rhs) { return e.bind(name, rhs); }	
		public int hashCode() { return Objects.hashCode(name); }
		public boolean equals(Object o) {
			return this == o || o instanceof Sym sym && name.equals(sym.name);
		}
		public String toString() { return name; }
	}
	Sym sym(String name) { return new Sym(name); }
	String sym_name(Sym sym) { return sym.name; }
	
	class Cons implements Evaluable, Bindable {
		Object car, cdr;
		Cons(Object car, Object cdr) { this.car = car; this.cdr = cdr; }
		public Object eval(Resumption r, Env e) {
			Object op = r != null ? resumeFrame(r) : evaluate(null, e, car);
			return op instanceof Suspension s ? suspendFrame(s, rr-> eval(rr, e)) : combine(null, e, op, cdr);
		}
		public Object bind(Env e, Object rhs) {
			if (!(car instanceof Bindable mcar)) return "cannot bind: " + car;
			if (!(cdr instanceof Bindable mcdr)) return "cannot bind: " + cdr; 
			mcar.bind(e, car(rhs)); return mcdr.bind(e, cdr(rhs));
		}
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
	Object car(Object o) { return o instanceof Cons c ? c.car : error("not a cons: " + o.toString()); }
	Object cdr(Object o) { return o instanceof Cons c ? c.cdr : error("not a cons: " + o.toString()); }
	Object elt(Object o, int i) { for (; i>0; i-=1) o=cdr(o); return car(o); }
	Object rst(Object o, int i) { for (; i>0; i-=1) o=cdr(o); return cdr(o); }
	int len(Object o) { int i=0; for (; o instanceof Cons c; o=c.cdr) i+=1; return i; }
	
	
	/* Environment */
	class Env {
		Map<String,Object> map = new LinkedHashMap();
		Env parent;
		Env(Env parent) { this.parent = parent; }
		public Object get(String name) { return map.get(name); };
		public Object bind(String name, Object rhs) {
			map.put(name, rhs); if (trace) print("bind ", name, "=", rhs, " in: ", this); return null; 
		}
		public String toString() { return this == TheEnvironment ? "[The-Env]" : "[Env " + map + " " + parent + "]"; }
		Object lookup(String name) {
			if (!map.containsKey(name)) return parent != null ? parent.lookup(name) : error("unbound: " + name);
			Object value = map.get(name); if (trace) print("lookup: ", /*name, ": ",*/ value); return value;
			
		}
	}
	Env env(Env parent) { return new Env(parent); }
	
	
	/* Bind */
	Object bind(Env e, Object lhs, Object rhs, Object exp) {
		if (!(lhs instanceof Bindable bindable)) return error("cannot bind: " + lhs);
		Object msg; try {
			msg = bindable.bind(e, rhs); if (msg == null) return ign;
		}
		catch (Error exc) { // only error in car() or cdr()
			msg = "to few arguments"; // + " because " + exc.getMessage();
		}
		return error(msg + " in bind: " + lhs + (exp == null ? "" : " of: " + exp) + " with: " + rhs);
	}
	
	
	/* Operative & Applicative Combiners */
	Object combine(Resumption r, Env e, Object op, Object o) {
		if (trace) print("combine: ", cons(op, o));
		if (op instanceof Combinable cmb) return cmb.combine(r, e, o);
		// per default le Function non wrapped dovrebbero essere operative e non applicative
		if (isjFun(op))
			//return ((Combinable) jswrap(op)).combine(m, e, o); // jsfun x default applicative
			return ((Combinable) jFun(op)).combine(r, e, o); // jsfun x default operative
		return error("not a combiner: " + op.toString() + " in: " + cons(op, o));
	}
	
	class Opv implements Combinable  {
		Object p; Object ep; Object x; Env e;
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
			var args = r != null ? resumeFrame(r) : evalArgs(null, e, o, nil);
			return args instanceof Suspension s ? suspendFrame(s, rr-> combine(rr, e, o)) : cmb.combine(null, e, args);
		}
		Object evalArgs(Resumption r, Env e, Object todo, Object done) {
			if (todo == nil) return reverseList(done);
			var arg = r != null ? resumeFrame(r) : evaluate(null, e, car(todo));
			return arg instanceof Suspension s ? suspendFrame(s, rr-> evalArgs(rr, e, todo, done)) : evalArgs(null, e, cdr(todo), cons(arg, done));
		}
		public String toString() { return "[Apv " + Vm.this.toString(cmb) + "]"; }
	}
	Object wrap(Object arg) { return arg instanceof Combinable cmb ? new Apv(cmb) : error("cannot wrap: " + arg); } // type check
	Object unwrap(Object arg) { return arg instanceof Apv apv ? apv.cmb : error("cannot unwrap: " + arg); } // type check
	
	
	/* Built-in Combiners */
	class Vau implements Combinable  {
		public Object combine(Resumption r, Env e, Object o) {
			checkOperand(this, o, 3); // o = (pt ep expr)
			var pt = elt(o, 0);
			var ep = elt(o, 1);
			var msg = new PTree(pt, ep).check(); if (msg != null) return error(msg + " of: " + cons(this, o));
			return new Opv(pt, ep, elt(o, 2), e);
		}
		public String toString() { return "vm-vau"; }
	};
	class Def implements Combinable  {
		public Object combine(Resumption r, Env e, Object o) {
			checkOperand(this, o, 2); // o = (pt arg)
			var pt = elt(o, 0);
			if (!(pt instanceof Sym)) {
				if (!(pt instanceof Cons)) return error("not a symbol: " + pt + " in: " + cons(this, o));
				var msg = new PTree(pt).check(); if (msg != null) return error(msg + " of: " + cons(this, o));
			}
			var arg = elt(o, 1);
			var val = r != null ? resumeFrame(r) : evaluate(null, e, arg);
			return val instanceof Suspension s ? suspendFrame(s, rr-> combine(rr, e, o)) : bind(e, pt, val, cons(this, o));
		}
		public String toString() { return "vm-def"; }
	};
	class PTree {
		private Object pt, ep;
		private Set syms = new HashSet<Sym>();
		PTree(Object pt) { this.pt = pt; }
		PTree(Object pt, Object ep) { this(pt); this.ep = ep; }
		Object check() { 
			if (pt != nil && pt != ign) {	var msg = check(pt); if (msg != null) return msg; }
			if (ep == null) return syms.size() > 0 ? null : "no one symbol in: " + pt;
			if (ep == ign) return null;
			if (!(ep instanceof Sym sym)) return "not a #ignore or symbol: " + ep;
			return !syms.contains(sym) ? null : "not a unique symbol: " + ep;
		}
		private Object check(Object p) {
			if (p == ign) return null;
			if (p instanceof Sym) { return syms.add(p) ? null : "not a unique symbol: " + p + (p == pt ? "" : " in: " + pt); }
			if (!(p instanceof Cons c)) return "not a #ignore or symbol: " + p + (p == pt ? "" : " in: " + pt);
			var msg = check(c.car); if (msg != null) return msg;
			return c.cdr == nil ? null : check(c.cdr);
		}
	}
	class Eval implements Combinable  {
		public Object combine(Resumption r, Env e, Object o) {
			checkOperand(this, o, 2); // o = (x eo)
			var x = elt(o, 0);
			var eo = elt(o, 1);
			return evaluate(r, (Env) eo, x);
		}		
		public String toString() { return "vm-eval"; }
	}
	
	
	/* First-order Control */
	class Begin implements Combinable  {
		public Object combine(Resumption r, Env e, Object o) {
			// o = (... xs)
			return o == nil ? null : begin(r, e, o);
		}
		Object begin(Resumption r, Env e, Object xs) {
			var res = r != null ? resumeFrame(r) : evaluate(null, e, car(xs));
			return res instanceof Suspension s ? suspendFrame(s, rr-> begin(rr, e, xs))
				: ((Function) kdr-> kdr == nil ? res : begin(null, e, kdr)).apply(cdr(xs))
			;
		}
		public String toString() { return "vm-begin"; }
	};
	class If implements Combinable  {
		public Object combine(Resumption r, Env e, Object o) {
			checkOperand(this, o, 3); // o = (test then else) 
			var test = r != null ? resumeFrame(r) : evaluate(null, e, elt(o, 0));
			return test instanceof Suspension s ? suspendFrame(s, rr-> combine(rr, e, o))
				: evaluate(null, e, test != null && test != nil && test instanceof Boolean b && b ? elt(o, 1) : elt(o, 2))
			;
		}
		public String toString() { return "vm-if"; }
	}
	class Loop implements Combinable  {
		public Object combine(Resumption r, Env e, Object o) {
			checkOperand(this, o, 1); // o = (x)
			var first = true; // only resume once
			while (true) {
				var res = first && r != null ? resumeFrame(r) : evaluate(null, e, elt(o, 0));
				first = false;
				if (res instanceof Suspension s) return suspendFrame(s, rr-> combine(rr, e, o), elt(o, 0), e);
			}
		}
		public String toString() { return "vm-loop"; }
	}
	class Catch implements Combinable  {
		public Object combine(Resumption r, Env e, Object o) {
			checkOperand(this, o, 2); // o = (x handler)
			var x = elt(o, 0);
			var handler = elt(o, 1);
			if (!(handler instanceof Apv apv1)) return error("not a one arg applicative combiner: " + handler); 
			Object res = null;
			try {
				res = r != null ? resumeFrame(r) : evaluate(null, e, x);
			}
			catch (Value exc) {
				// unwrap handler to prevent eval if exc is sym or cons
				res = Vm.this.combine(null, e, unwrap(apv1), list(exc.value));
			}
			if (res instanceof Suspension s) suspendFrame(s, rr-> combine(rr, e, o), x, e);
			return res;
		}
		public String toString() { return "vm-catch"; }
	}
	class Value extends RuntimeException {
		private static final long serialVersionUID = 1L;
		Object value; Value(Object value) { super(Vm.this.toString(value)); this.value = value;	}
	}
	class Finally implements Combinable  {
		@SuppressWarnings("finally")
		public Object combine(Resumption r, Env e, Object o) {
			checkOperand(this, o, 2); // o = (prot cleanup)
			var prot = elt(o, 0);
			var cleanup = elt(o, 1);
			Object res = null;
			try {
				res = r != null ? resumeFrame(r) : evaluate(null, e, prot);
				if (res instanceof Suspension s) suspendFrame(s, rr-> combine(rr, e, o), prot, e);
			}
			finally {
				return res instanceof Suspension s ? s : doCleanup(null, e, cleanup, res);
			}
		}
		Object doCleanup(Resumption r, Env e, Object cleanup, Object res) {
			var fres = r != null ? resumeFrame(r) : evaluate(null, e, cleanup);
			if (fres instanceof Suspension s) suspendFrame(s, rr-> doCleanup(rr, e, cleanup, res), cleanup, e);
			return fres;
		}
		public String toString() { return "vm-finaly"; }
	}
	
	
	/* Delimited Control */
	class PushPrompt implements Combinable  {
		public Object combine(Resumption r, Env e, Object o) {
			checkOperand(this, o, 2); // o = (prompt exp)
			var prompt = elt(o, 0);
			var x = elt(o, 1);
			var res = r != null ? resumeFrame(r) : evaluate(null, e, x);	
			if (!(res instanceof Suspension s)) return res;
			if (s.prompt != prompt) return suspendFrame(s, rr-> combine(rr, e, o), x, e);
			return Vm.this.combine(null, e, s.handler, cons(s.continuation, nil));
		}
		public String toString() { return "vm-pushp-rompt"; }
	}
	class TakeSubcont implements Combinable  {
		public Object combine(Resumption r, Env e, Object o) {
			checkOperand(this, o, 2); // o = (prompt handler)
			var prompt = elt(o, 0);
			var handler = elt(o, 1);
			if (!(handler instanceof Apv apv1)) return error("not a one arg applicative combiner: " + handler); 
			return suspendFrame(new Suspension(prompt, apv1), rr-> Vm.this.combine(null, e, rr.s, nil), this, e);
		}
		public String toString() { return "vm-take-subcont"; }
	}
	class PushSubcont implements Combinable  {
		public Object combine(Resumption r, Env e, Object o) {
			checkOperand(this, o, 2); // o = (k apv0)
			var o0 = elt(o, 0);
			if (!(o0 instanceof StackFrame k)) return error("not a stackframe: " + o0); 
			var o1 = elt(o, 1);
			if (!(o1 instanceof Apv apv0)) return error("not a zero args applicative combiner: " + o1);
			var res = r != null ? resumeFrame(r) : resumeFrame(k, ()-> Vm.this.combine(null, e, apv0, nil));
			if (res instanceof Suspension s) suspendFrame(s, rr-> combine(rr, e, o), apv0, e);
			return res;
		}
		public String toString() { return "vm-push-subcont"; }
	}
	class PushPromptSubcont implements Combinable  {
		public Object combine(Resumption r, Env e, Object o) {
			checkOperand(this, o, 2); // o = (prompt k apv0)
			var prompt = elt(o, 0);
			var o1 = elt(o, 1);
			if (!(o1 instanceof StackFrame k)) return error("not a stackframe: " + o1); 
			var o2 = elt(o, 2);
			if (!(o2 instanceof Apv apv0)) return error("not a zero args applicative combiner: " + o2); 
			var res = r != null ? resumeFrame(r) : resumeFrame(k, ()-> Vm.this.combine(null, e, apv0, nil));
			if (!(res instanceof Suspension s)) return res;
			if (s.prompt != prompt) return suspendFrame(s, rr-> combine(rr, e, o), apv0, e);
			return Vm.this.combine(null, e, s.handler, cons(s.continuation, nil));
		}
		public String toString() { return "vm-push-prompt-subcont"; }
	}
	
	
	/* Dynamic Variables */
	class DV {
		Object val;
		DV(Object val) { this.val = val; }
		public String toString() {	return "[vm-dv " + val + "]"; }
	}
	class DNew implements Combinable  {
		public Object combine(Resumption r, Env e, Object o) { checkOperand(this, o, 1); return new DV(elt(o, 0));	}
		public String toString() {	return "vm-dref"; }
	}
	class DRef implements Combinable  {
		public Object combine(Resumption r, Env e, Object o) {
			checkOperand(this, o, 1); var x = elt(o, 0); return x instanceof DV dv ? dv.val : error("not a dinamic variable: " + x);
		}
		public String toString() {	return "vm-dnew"; }
	}
	class DLet implements Combinable  {
		public Object combine(Resumption r, Env e, Object o) {
			checkOperand(this, o, 3); // o = (xdv val x)
			var xdv = elt(o, 0);
			if (!(xdv instanceof DV dv)) return error("not a dinamic variable: " + xdv);
			var val = elt(o, 1);
			var x = elt(o, 2);
			var oldVal = dv.val;
			try {
				dv.val = val;
				var res = r != null ? resumeFrame(r) : evaluate(null, e, x);
				if (res instanceof Suspension s) suspendFrame(s, rr-> combine(rr, e, o), x, e);
				return res;
			}
			finally {
				dv.val = oldVal;
			}
		}
		public String toString() {	return "vm-dlet"; }
	}
	
	
	/* Error handling */
	Object RootPrompt = new Object() { public String toString() { return "RoorPrompt"; } };
	Object pushRootPrompt(Object x) { return list(new PushPrompt(), RootPrompt, x); }
	Object error(String err) {
		//console.log(err)
		var user_break = TheEnvironment.get("user-break");
		if (user_break == null) throw new Error(err);
		return combine(null, TheEnvironment, user_break, list(err));
	}
	class Error extends RuntimeException {
		private static final long serialVersionUID = 1L;
		public Error(String message) {	super(message); }
		public Error(String message, Throwable cause) { super(message, cause); }
	}
	Object checkOperand(Object op, Object o, int expt) {
		var len=len(o); if (len == expt) return true;
		return error("not " + expt + " operands in: " + cons(op, o));
	}
	
	
	/* Utilities */
	Object list(Object ... args) {
		return arrayToList(true, args);
	}
	Object listStar(Object ... args) {
		return arrayToList(false, args);
	}
	Object arrayToList(boolean b, Object ... args) {
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
	<T> T[] listToArray(Object c, int i, Class<T> cl) {
		var res = new ArrayList();
		for (; c != nil; c = cdr(c)) if (i-- <= 0) res.add(car(c));
		return (T[]) res.toArray((T[]) Array.newInstance(cl, 0));
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
	<T> T print(T ... os) {
		for (var o: os) out.print(toString(o));
		out.println();
		return os[os.length - 1];
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
	Void assertVm(Object ... objs) {
		var expr = objs[0];
		try {
			var val = expr instanceof String s ? eval(s) : evaluate(null, env(TheEnvironment), expr);
			if (objs.length == 1) print(expr, " should be throw but is ", val);
			else {
				var expt = objs[1]; 
				if (equals(val, expt) || val.equals(parseBytecode(expt))) return null;
				print(expr, "should be ", expt, " but is ", val);
			}
		}
		catch (Throwable t) {
			if (objs.length == 1) return null;
			if (stack) t.printStackTrace(out);
			else print(expr, " throw ", t);
		}
		return null;
	}
	
	
	/* Bytecode parser */
	Object parseBytecode(Object o) {
		if (o instanceof String s) return s.equals("#ignore") ? ign : sym(s);
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
			if (i != objs.length-2) throw new RuntimeException(". not is the penultimate element in " + objs);
			((Cons) cons).cdr = parseBytecode(objs[i+1]);
			return head;
		}
		return head;
	}
	
	
	/* JNI */
	interface ArgsList extends Function {}
	class JFun implements Combinable  {
		Object jfun;
		JFun(Object jfun) { this.jfun = jfun; };
		@SuppressWarnings("preview")
		public Object combine(Resumption r, Env e, Object o) {
			try {
				// TODO andrebbe fatto il controllo sugli operandi
				/* warning preview
				return switch (jsfun) {
					case Supplier s -> s.get();  
					case ArgsList f -> f.apply(o);  
					case Function f -> f.apply(car(o));  
					case BiFunction f -> f.apply(elt(o,0), elt(o,1));
					case Consumer c -> { c.accept(car(o)); yield null; }
					case Method mt-> mt.invoke(car(o), list_to_array(cdr(o)));
					case Constructor c-> c.newInstance(list_to_array(o));
					default -> error("not a combine " + jsfun);
				};
				*/
				if (jfun instanceof Supplier s) return s.get();  
				if (jfun instanceof ArgsList f) return f.apply(o);  
				if (jfun instanceof Function f) return f.apply(car(o));  
				if (jfun instanceof BiFunction f) return f.apply(car(o), elt(o,1));
				if (jfun instanceof Consumer c) { c.accept(car(o)); return ign; }
				if (jfun instanceof Field f) { if (len(o) < 2) return f.get(car(o)); f.set(car(o), elt(o,1)); return ign; };
				if (jfun instanceof Method mt)	return mt.invoke(car(o), listToArray(cdr(o)));
				if (jfun instanceof Constructor c) return c.newInstance(listToArray(o));
				return error("not a combine " + jfun);
			}
			catch (Exception exp) {
				throw exp instanceof RuntimeException rte ? rte : new RuntimeException(exp);
			}
		}
		public String toString() {return "[JSFun " + jfun + "]"; }
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
	
	boolean jInstanceOf(Object o, Class c) {
		return c.isInstance(o);
	}
	
	@SuppressWarnings("preview")
	Object jInvoker(String name) {
		if (name == null) return error("method name is null");
		return jWrap(
			(ArgsList) x-> {
				Object obj = elt(x, 0);
				if (obj == null) return error("receiver is null");
				if (obj instanceof Apv apv) obj = apv.cmb;
				if (obj instanceof JFun f) obj = f.jfun; 
				try {
					switch (name) {
						case "getField":
							return jWrap(((Class) obj).getField((String) elt(x,1)));
						case "getMethod":
							return jWrap(((Class) obj).getMethod((String) elt(x,1), listToArray(x, 2, Class.class)));
						case "invoke":
							return ((Method) obj).invoke(elt(x,1), listToArray(x, 2, Object.class));
						case "getConstructor":
							return jWrap(((Class) obj).getConstructor(listToArray(x, 1, Class.class)));
						case "newInstance":
							return obj == Array.class
								? Array.newInstance((Class) elt(x, 1), Arrays.stream(listToArray(x, 2, Integer.class)).mapToInt(i->i).toArray() )
								: ((Constructor) obj).newInstance(listToArray(x, 1));
					}
				}
				catch (Exception exp) {
					throw new RuntimeException("error executing method: " + name + " in: " + x, exp);
				}
				Object[] args = listToArray(x, 1);
				Executable executable = getExecutable(name.equals("new") ? (Class) obj : obj.getClass(), name,  getClasses(args));
				if (executable != null)	try {
					if (executable.isVarArgs()) args = reorg(executable.getParameterTypes(), args);
					/* warning preview
					return switch (executable) {
						case Method m-> m.invoke(obj, args);
						case Constructor c-> c.newInstance(args);
					};
					*/
					if (executable instanceof Method m) return m.invoke(obj, args);
					if (executable instanceof Constructor c) return c.newInstance(args);
					throw new RuntimeException("no method no constructor: " + executable);   
				}
				catch (Exception exp) {
					throw new RuntimeException("error executing " + (name.equals("new") ? "constructor" : "method: " + name) + " in: " + x, exp);
				}
				throw new RuntimeException("not found " + (name.equals("new") ? "constructor" : "method: " + name) + toString(list(getClasses(args))) + " in: " + obj);
			}
		);
	}
	Object jGetSetter(String name) {
		if (name == null) return error(name + " getter called with wrong args");
		return jWrap(
			(ArgsList) x-> {
				var len = len(x);
				if (len > 2) return error("too many operands in: " + cons(this, x));			
				var obj = elt(x, 0);
				try {
					Field field = getField(obj instanceof Class c ? c : obj.getClass(), name);
					if (len == 1) return field.get(obj);
					field.set(obj, elt(x, 1));
					return ign;
				}
				catch (Exception e) {
					return error("can't " + (len==1 ? "get" : "set") +" " + name + " of " + toString(obj) + (len == 1 ? "" : " with " + elt(x, 1)));
				}
			}
		);
	}
	
	
	/* Stringification */
	String toString(Object o) {
		return toString(false, o);
	}
	@SuppressWarnings("preview")
	String toString(boolean b, Object o) {
		var r = new StringBuilder();
		/* warning preview
		r.append(
			switch (o) {
				case null-> "#null";
				case Object[] a-> {
					var s = new StringBuilder();
					for (var e: a) s.append((s.isEmpty() ? "" : ", ") + toString(true, e));
					yield "[" + s.toString() + "]";
				}
				case String s-> !b ? s : toSource(s);
				case Class cl-> "&" + cl.getName();
				default-> o.toString();
			}
		);
		*/
		if (o == null)
			r.append("#null");
		else if (o instanceof Object[] a) {
			var s = new StringBuilder();
			for (var e: a) s.append((s.isEmpty() ? "" : ", ") + toString(true, e));
			r.append("[" + s.toString() + "]");
		}
		else if (o instanceof String s)
			r.append(!b ? s : toSource(s));
		else if (o instanceof Class cl)
			r.append("&" + cl.getName());
		else
			r.append(o.toString());
		return r.toString();
	}
	public static String toSource(String s) {
		s = s
			.replaceAll("\"", "\\\\\"")
			.replaceAll("\n", "\\\\n")
			.replaceAll("\t", "\\\\t")
			.replaceAll("\r", "\\\\r")
			.replaceAll("\b", "\\\\b")
			.replaceAll("\f", "\\\\f")
		;
		return '"' + s + '"';
	}
	
	
	/* Bootstrap */
	Object builtinBytecode =
		$("vm-begin",
			// Basics
			$("vm-def", "vm-vau", new Vau()),
			$("vm-def", "vm-eval", wrap(new Eval())),
			$("vm-def", "vm-make-environment", jWrap((ArgsList) args-> env(args == nil ? null : (Env) car(args)))),
			$("vm-def", "vm-wrap", jWrap((Function<Object, Object>) this::wrap)),
			$("vm-def", "vm-unwrap", jWrap((Function<Object, Object>) this::unwrap)),
			// Values
			$("vm-def", "vm-cons", jWrap((BiFunction<Object, Object, Object>) this::cons)),
			$("vm-def", "vm-cons?", jWrap((Function<Object, Boolean>) obj-> obj instanceof Cons)),
			$("vm-def", "vm-nil?", jWrap((Function<Object, Boolean>) obj-> obj == nil)),
			$("vm-def", "vm-string-to-symbol", jWrap((Function<String, Sym>) this::sym)),
			$("vm-def", "vm-symbol?", jWrap((Function<Object, Boolean>) obj-> obj instanceof Sym)),
			$("vm-def", "vm-symbol-name", jWrap((Function<Sym, String>) this::sym_name)),
			// First-order Control
			$("vm-def", "vm-if", new If()),
			$("vm-def", "vm-loop", new Loop()),
			$("vm-def", "vm-throw", jWrap((Consumer<Object>) obj-> { throw new Value(obj); })),
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
			$("vm-def", "vm-root-prompt", RootPrompt),
			$("vm-def", "vm-error", jWrap((Function<String, Object>) this::error)),
			// JS Interface
			$("vm-def", "vm-js-invoker", jWrap((Function<String,Object>) this::jInvoker)),
			$("vm-def", "vm-js-getter", jWrap((Function<String,Object>) this::jGetSetter)),
			$("vm-def", "instanceof", jWrap((BiFunction<Object,Class,Boolean>) this::jInstanceOf)),
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
			$("vm-def", "<=", jWrap((BiFunction<Integer,Integer,Boolean>) (a,b)-> a <= b)),
			$("vm-def", ">", jWrap((BiFunction<Integer,Integer,Boolean>) (a,b)-> a > b)),
			$("vm-def", ">=", jWrap((BiFunction<Integer,Integer,Boolean>) (a,b)-> a >= b)),
			//
			$("vm-def", "vm-quote", $("vm-vau", $("a"), ign, "a")),
			$("vm-def", "==", jWrap((BiFunction<Object,Object,Boolean>) (a,b)-> a == b)),
			$("vm-def", "!=", jWrap((BiFunction<Object,Object,Boolean>) (a,b)-> a != b)),
			$("vm-def", "eq?", jWrap((BiFunction<Object,Object,Boolean>) (a,b)-> equals(a, b))),
			$("vm-def", "assert", jFun((ArgsList) l-> assertVm(listToArray(l)))),
			$("vm-def", "toString", jWrap((ArgsList) l-> this.toString(listToArray(l)))),
			$("vm-def", "print", jWrap((ArgsList) l-> this.print(listToArray(l)))),
			$("vm-def", "trace", jWrap((Consumer<Boolean>) b-> trace=b)),
			$("vm-def", "stack", jWrap((Consumer<Boolean>) b-> stack=b)),
			$("vm-def", "root-prompt", RootPrompt)
		)
	;
	Env TheEnvironment = env(null); {
		bind(TheEnvironment, sym("vm-def"), new Def(), null);
		bind(TheEnvironment, sym("vm-begin"), new Begin(), null);
		evaluate(null, TheEnvironment, parseBytecode(builtinBytecode));
	}
	
	
	/* API */
	public Object exec(Object bytecode) {
		var wrapped = pushRootPrompt(cons(new Begin(), parseBytecode(bytecode)));
		var res = evaluate(null, TheEnvironment, wrapped);
		if (res instanceof Suspension s) throw new RuntimeException("prompt not found: " + s.prompt);
		return res;
	}
	public Object call(String funName, Object ... args) {
		return exec(list(sym(funName), parseBytecode(args)));
	}
	public Object get(String varName) {
		return exec(sym(varName));
	}
	
	
	/* Wat */
	public Object eval(String sexpr) throws Exception {
		return exec(parse(sexpr));
	}
	public void compile(String fileName) throws Exception {
		try (
			ObjectOutputStream oos = new ObjectOutputStream(new FileOutputStream("build/" + fileName))
		) {
			oos.writeObject(parse(readFile(fileName)));
		}
	}
	public static String readFile(String filename) throws IOException {
		return Files.readString(Paths.get(filename), Charset.forName("cp1252"));
	} 
	public Object exec(String fileName) throws Exception {
		return exec(readBytecode(fileName));
	}
	public Object readBytecode(String fileName) throws Exception {
		try (
			ObjectInputStream ois = new ObjectInputStream(new FileInputStream("build/" + fileName));
		) {
			return ois.readObject();
		}
	}
	
	@SuppressWarnings("preview")
	public void repl() throws Exception {
		loop: for (;;) {
			//switch (read())
			String line; switch (line = read()) {
				case "": break loop;
				//case String line: try { // warning preview
				default: try {
					//out.println(line);
					//out.println(to_string(parse(line)));
					print(exec(parse(line)));
				}
				catch (Throwable t) {
					if (stack)
						t.printStackTrace(out);
					else
						out.println(t.getClass().getSimpleName() + ":" + t.getMessage());
				}
			}
		}
		print("finito");
	}
	private String read() throws IOException {
		var s = new StringBuilder();
		int open = 0, close = 0;
		boolean inEscape = false, inString = false, inComment=false, sMlComment=false, inMlcomment=false, eMlComment=false;
		do {
			var oc = close-open;
			out.print((oc==0 ? "" : "%+d".formatted(oc)) + "> ");
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
					case '/'-> inMlcomment = eMlComment = false;
				}
				else if (inMlcomment) switch (c) {
					case '"'-> inString = true;
					case ';'-> inComment = true;
					case '*'-> eMlComment = true;
				}
				else if (sMlComment) switch (c) {
					case '*': inMlcomment = true;
					default : sMlComment = false;
				}
				else switch (c) {
					case '"'-> inString = true;
					case ';'-> inComment = true;
					case '/'-> sMlComment = true;
					case '('-> open += 1;
					case ')'-> close += 1;
				}
				if (c >= 32) s.append((char) c);
			}
			if (inComment) { s.append('\n'); inComment = false; }
		} while (open != close);
		return s.toString();
	}
	
	
	/* Test */
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
		print(parseBytecode(parse("(vm-cons 1 2)"))); // -> ((vm-cons 1 2))
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
		/*
		assertVm("1", 1);
		assertVm("(vm-begin 1 2 3)", 3);
		assertVm("(vm-cons 1 2)", $(1,".",2));
		assertVm("(vm-list 1 2 3)", $(1,2,3));
		assertVm("(vm-list* 1 2 3 4)", $(1,2,3,".",4));
		assertVm("(vm-list-to-array (vm-list 1 2 3 4))", new Object[] {1,2,3,4});
		assertVm("(vm-array-to-list #t (vm-list-to-array (vm-list 1 2 3 4)))", $(1,2,3,4));
		assertVm("(vm-array-to-list #f (vm-list-to-array (vm-list 1 2 3 4)))", $(1,2,3,".",4));
		assertVm("(vm-reverse-list (vm-list 1 2 3 4))", $(4,3,2,1));
		
		assertVm("(+ 1 2)", 3);
		assertVm("(* 3 2)", 6);
		assertVm("(* (* 3 2) 2)", 12);
		
		assertVm("((vm-vau a #ignore a) 1 2)", $(1,2));
		assertVm("((vm-vau (a b) #ignore b) 1 2)", 2);
		assertVm("((vm-vau (a . b) #ignore b) 1 2 3)", $(2,3));
		assertVm("(vm-begin (vm-def x (vm-list 1)) x)", $(1));
		assertVm("(vm-def x 1)((vm-wrap(vm-vau (a) #ignore a)) x)", 1);
		
		assertVm("(vm-def () 1) a");
		assertVm("(vm-def a (vm-list 1)) a", $(1));
		assertVm("(vm-def (a) (vm-list 1)) a", 1);
		assertVm("(vm-def a (vm-list 1 2 3)) a", $(1,2,3));
		assertVm("(vm-def (a) 1 2 3) a");
		assertVm("(vm-def 2 1)");
		assertVm("(vm-def (a) (vm-list 1)) a", 1);
		assertVm("(vm-def (a b) (vm-list 1 2)) a b", 2);
		assertVm("(vm-def (a . b) (vm-list 1 2 3)) a b", $(2,3)); 
				
		assertVm("(vm-vau (a . 12) #ignore 0)");
		assertVm("(vm-def (a b 12) 1)");
		
		assertVm("(vm-qot a b c)");

		assertVm("(vm-def 2 1)");
		assertVm("(vm-def a 1) a", 1);
		assertVm("(vm-def (a . b) (vm-list 1 2 3)) a b", $(2,3));
		assertVm("(vm-def define vm-def) (define a 1) a", 1);
		assertVm("(vm-def $define! vm-def) ($define! a 1) a", 1);
		assertVm("(vm-def $define! vm-def) $define!", TheEnvironment.get("vm-def"));
		assertVm("a", 1);
		assertVm("b", $(2,3));
		assertVm("(vm-if #t 1 2)", 1);
		assertVm("(vm-if #f 1 2)", 2);
		assertVm("(vm-if #null 1 2)", 2);
		//*/
		/*
		eval("""
			(assert (vm-def a 1) #ignore)
			(assert (vm-def a)          ) ;throw
		""");
		//*/
		
		//*
		//exec(parse(readFile("boot.wat")));
		//compile("boot.wat");
		//exec(readBytecode("boot.wat"));
		eval(readFile("boot.wat"));
		//eval(readFile("boot2.wat"));
		eval(readFile("test.wat"));		
		repl();
		//*/
	}
}
