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
	o0, o1, ..: operand 0, 1 .. 
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
	abstract class Combinable { abstract Object combine(Resumption r, Env e, Object o); }
	
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
		if (trace) print("eval: ", o);
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
		public String toString() {	return "(" + toString(this) + ")"; }
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
		public Object bind(String name, Object rhs) {
			map.put(name, rhs); if (trace) print("bind: ", name, "=", rhs, " ", this); return null; 
		}
		public String toString() { return this == TheEnvironment ? "[The-Env]" : "[Env " + map + " " + parent + "]"; }
		Object lookup(String name) {
			if (!map.containsKey(name)) return parent != null ? parent.lookup(name) : error("unbound: " + name);
			Object value = map.get(name); if (trace) print("lookup: ", name, ":", value); return value;
			
		}
	}
	Env env(Env parent) { return new Env(parent); }
	/*
	Object lookup(Env e, String name) {
		for(;;) {
			if (e.has(name)) {Object value = e.get(name); if (trace) print("lookup: ", name, ":", value); return value; }
			if (e.parent == null) return error("unbound: " + name);
			e = e.parent;
		}
	}
	*/
	
	
	/* Bind */
	Object bind(Env e, Object lhs, Object rhs, Object exp) {
		if (!(lhs instanceof Bindable bindable)) return error("cannot bind: " + lhs);
		Object msg; try {
			msg = bindable.bind(e, rhs); if (msg == null) return ign;
		}
		catch (ErrorException exc) { // only error in car() or cdr()
			msg = "insufficient arguments"; // + " because " + exc.getMessage();
		}
		return error(msg + " in bind: " + lhs + (exp == null ? "" : " of: " + exp) + " with: " + rhs);
	}
	
	
	/* Operative & Applicative Combiners */
	Object combine(Resumption r, Env e, Object op, Object o) {
		if (trace) print("combine: ", cons(op, o));
		if (op instanceof Combinable cmb) return cmb.combine(r, e, o);
		// per default le Function non wrapped dovrebbero essere operative e non applicative
		if (isjsfun(op))
			//return ((Combinable) jswrap(op)).combine(m, e, o); // jsfun x default applicative
			return ((Combinable) jsfun(op)).combine(r, e, o); // jsfun x default operative
		return error("not a combiner: " + op.toString() + " in: " + cons(op, o));
	}
	
	class Opv extends Combinable {
		Object p; Object ep; Object x; Env e;
		Opv(Object p, Object ep, Object x, Env e) { this.p = p; this.ep = ep; this.x = x; this.e = e; }
		public Object combine(Resumption r, Env e, Object o) {
			var xe = env(this.e); bind(xe, p, o, this); bind(xe, ep, e, this); return evaluate(null, xe, x);
		}
		public String toString() { return "[Opv " + Vm.this.toString(p) + " " + Vm.this.toString(ep) + " " + Vm.this.toString(x) + "]"; }
	}
	
	class Apv extends Combinable {
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
	class Vau extends Combinable {
		Object combine(Resumption r, Env e, Object o) {
			// o = (pt ep expr)
			if (len(o) > 3) return error("too many operands in: " + cons(this, o));
			var pt = elt(o, 0);
			var ep = elt(o, 1);
			var msg = new PTree(pt, ep).check(); if (msg != null) return error(msg + " of: " + cons(this, o));
			return new Opv(pt, ep, elt(o, 2), e);
		}
		public String toString() { return "vm-vau"; }
	};
	class Def extends Combinable {
		Object combine(Resumption r, Env e, Object o) {
			// o = (pt arg)
			if (len(o) > 2) return error("too many operands in: " + cons(this, o));
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
	class Eval extends Combinable {
		Object combine(Resumption r, Env e, Object o) {
			// o = (x eo)
			var x = elt(o, 0);
			var eo = elt(o, 1);
			return evaluate(r, (Env) eo, x);
		}		
		public String toString() { return "vm-eval"; }
	}
	
	
	/* First-order Control */
	class Begin extends Combinable {
		Object combine(Resumption r, Env e, Object o) {
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
	class If extends Combinable {
		Object combine(Resumption r, Env e, Object o) {
			// o = (test then else) 
			if (len(o) > 3) return error("too many operands in: " + cons(this, o));
			var test = r != null ? resumeFrame(r) : evaluate(null, e, elt(o, 0));
			return test instanceof Suspension s ? suspendFrame(s, rr-> combine(rr, e, o))
				: evaluate(null, e, test != null && test != nil && test instanceof Boolean b && b ? elt(o, 1) : elt(o, 2))
			;
		}
		public String toString() { return "vm-if"; }
	}
	class Loop extends Combinable {
		Object combine(Resumption r, Env e, Object o) {
			// o = (x)
			var first = true; // only resume once
			while (true) {
				var res = first && r != null ? resumeFrame(r) : evaluate(null, e, elt(o, 0));
				first = false;
				if (res instanceof Suspension s) return suspendFrame(s, rr-> combine(rr, e, o), elt(o, 0), e);
			}
		}
		public String toString() { return "vm-loop"; }
	}
	class Catch extends Combinable {
		Object combine(Resumption r, Env e, Object o) {
			//o = (x handler)
			var x = elt(o, 0);
			var handler = elt(o, 1);
			Object res = null;
			try {
				res = r != null ? resumeFrame(r) : evaluate(null, e, x);
			}
			catch (ValueException exc) {
				// unwrap handler to prevent eval if exc is sym or cons
				res = Vm.this.combine(null, e, unwrap(handler), list(exc.value));
			}
			if (res instanceof Suspension s) suspendFrame(s, rr-> combine(rr, e, o), x, e);
			return res;
		}
		public String toString() { return "vm-catch"; }
	}
	class ValueException extends RuntimeException {
		private static final long serialVersionUID = 1L;
		Object value;
		ValueException(Object value) {
			super(Vm.this.toString(value));
			this.value = value;
		}
	}
	class Finally extends Combinable {
		@SuppressWarnings("finally")
		Object combine(Resumption r, Env e, Object o) {
			// o = (prot cleanup)
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
	class PushPrompt extends Combinable {
		Object combine(Resumption r, Env e, Object o) {
			// o = (prompt exp)
			if (len(o) > 2) return error("too many operands in: " + cons(this, o));
			var prompt = elt(o, 0);
			var x = elt(o, 1);
			var res = r != null ? resumeFrame(r) : evaluate(null, e, x);	
			if (!(res instanceof Suspension s)) return res;
			if (s.prompt != prompt) return suspendFrame(s, rr-> combine(rr, e, o), x, e);
			return Vm.this.combine(null, e, s.handler, cons(s.continuation, nil));
		}
		public String toString() { return "vm-pushp-rompt"; }
	}
	class TakeSubcont extends Combinable {
		Object combine(Resumption r, Env e, Object o) {
			// o = (prompt handler)
			if (len(o) > 2) return error("too many operands in: " + cons(this, o));
			var prompt = elt(o, 0);
			var handler = elt(o, 1);
			if (!(handler instanceof Apv apv1)) return error("not a one arg applicative combiner: " + handler); 
			var cap = new Suspension(prompt, apv1);
			return suspendFrame(cap, rr-> Vm.this.combine(null, e, rr.s, nil), this, e);
		}
		public String toString() { return "vm-take-subcont"; }
	}
	class PushSubcont extends Combinable {
		Object combine(Resumption r, Env e, Object o) {
			// o = (k apv0)
			if (len(o) > 2) return error("too many operands in: " + cons(this, o));
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
	class PushPromptSubcont extends Combinable {
		Object combine(Resumption r, Env e, Object o) {
			// o = (prompt k apv0)
			if (len(o) > 2) return error("too many operands in: " + cons(this, o));
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
	class DNew extends Combinable {
		Object combine(Resumption r, Env e, Object o) { return new DV(elt(o, 0));	}
		public String toString() {	return "vm-dref"; }
	}
	class DRef extends Combinable {
		Object combine(Resumption r, Env e, Object o) {
			var x = elt(o, 0);
			return x instanceof DV dv ? dv.val : error("not dinamic variable: " + x);
		}
		public String toString() {	return "vm-dnew"; }
	}
	class DLet extends Combinable {
		Object combine(Resumption r, Env e, Object o) {
			var xdv = elt(o, 0);
			if (!(xdv instanceof DV dv)) return error("not dinamic variable: " + xdv);
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
	Object RootPrompt = new Object() {
		public String toString() { return "RoorPrompt"; }
	};
	Object pushRootPrompt(Object x) { return list(new PushPrompt(), RootPrompt, x); }
	Object error(String err) {
		//console.log(err)
		var user_break = TheEnvironment.lookup("user-break");
		if (user_break == null) throw new ErrorException(err);
		return combine(null, TheEnvironment, user_break, list(err));
	}
	class ErrorException extends RuntimeException {
		private static final long serialVersionUID = 1L;
		public ErrorException(String message) {	super(message); }
		public ErrorException(String message, Throwable cause) { super(message, cause); }
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
	/* sostituito dal seguente
	Object[] list_to_array(Object c) {
		var res = new ArrayList();
		for (; c != nil; c = cdr(c)) res.add(car(c));
		return res.toArray();
	}
	 */
	<T> T[] listToArray(Object c) {
		return listToArray(c, 0);
	}
	<T> T[] listToArray(Object c, Class<T> cl) {
		return (T[]) listToArray(c, 0, cl);
	}
	<T> T[] listToArray(Object c, int i) {
		return (T[]) listToArray(c, i, Object.class);
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
		for(var o: os) out.print(toString(o));
		out.println();
		return os[os.length - 1];
	}
	boolean equals(Object a, Object b) {
		if (a instanceof Object[] aa) return equals(aa, b);
		if (a instanceof Cons ca) return ca.equals(b);
		if (a instanceof Object) return a.equals(b);
		return a == b;
	}
	boolean equals(Object[] a, Object o) {
		if (!(o instanceof Object[] b) || a.length != b.length) return false;
		for (int i=0; i<a.length; i+=1) if (!equals(a[i], b[i])) return false;
		return true;
	}
	/*
	Void assertEq(Object ... objs) {
		var x = objs[0];
		try {
			var v = evaluate(null, env(the_environment), x);
			if (objs.length == 1)
				print(x, "should be throw but is", v);
			else {
				var ex = objs[1]; 
				if (!equals(v, ex))
					print(x, "should be", ex, "but is", v);
			}
		}
		catch (Throwable t) {
			if (objs.length > 1) print(x, "throw", t);
		}
		return null;
	}
	void assertEq(String expr, Object expected) {
		try {
			var value = eval(expr);
			if (equals(value, expected)) return;
			else if (value.equals(parse_bytecode(expected))) return;
			err.println(expr + " should be " + toString(expected) + " but is " + value);
		}
		catch (Throwable t) {
			if (!(expected instanceof Throwable)) 
				err.println(t.getMessage());
				//t.printStackTrace();
		}
	}
	//*/
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
	
	/* JSNI */
	interface ArgsList extends Function {}
	class JSFun extends Combinable {
		Object jsfun;
		JSFun(Object jsfun) { this.jsfun = jsfun; }
		@SuppressWarnings("preview")
		Object combine(Resumption r, Env e, Object o) {
			try {
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
				if (jsfun instanceof Supplier s) return s.get();  
				if (jsfun instanceof ArgsList f) return f.apply(o);  
				if (jsfun instanceof Function f) return f.apply(car(o));  
				if (jsfun instanceof BiFunction f) return f.apply(elt(o,0), elt(o,1));
				if (jsfun instanceof Consumer c) { c.accept(car(o)); return ign; }
				if (jsfun instanceof Method mt)	return mt.invoke(car(o), listToArray(cdr(o)));
				if (jsfun instanceof Constructor c) return c.newInstance(listToArray(o));
				return error("not a combine " + jsfun);
			}
			catch (Exception exp) {
				throw exp instanceof RuntimeException rte ? rte : new RuntimeException(exp);
			}
		}
		public String toString() {return "[JSFun " + jsfun /*.getClass().getSimpleName()*/ + "]"; }
	}
	boolean isjsfun(Object jsfun) {
		return isInstance(jsfun, Supplier.class, ArgsList.class, Function.class, BiFunction.class, Consumer.class, Executable.class);
	}
	Object jsfun(Object jsfun) {
		return /*jsfun instanceof JSFun ? jsfun :*/ isjsfun(jsfun) ? new JSFun(jsfun) : error("no a jsfun: " + jsfun);
	}
	Object jswrap(Object jsfun) {
		return wrap(jsfun(jsfun));
	}
	
	boolean jsInstanceOf(Object o, Class c) {
		return c.isInstance(o);
	}
	
	@SuppressWarnings("preview")
	Object jsInvoker(String name) {
		if (name == null) return error("method name is null");
		return jswrap(
			(ArgsList) x-> {
				Object obj = elt(x, 0);
				if (obj == null) return error("receiver is null");
				if (obj instanceof Apv apv) obj = apv.cmb;
				if (obj instanceof JSFun f) obj = f.jsfun; 
				try {
					switch (name) {
						case "getMethod":
							return jswrap(((Class) obj).getMethod((String) elt(x,1), listToArray(x, 2, Class.class)));
						case "invoke":
							return ((Method) obj).invoke(elt(x,1), listToArray(x, 2, Object.class));
						case "getConstructor":
							return jswrap(((Class) obj).getConstructor(listToArray(x, 1, Class.class)));
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
				throw new RuntimeException("not found " + (name.equals("new") ? "constructor" : "method: " + name) + " in: " + obj);
			}
		);
	}
	/*
	@SuppressWarnings("preview")
	Object jsInvoker(String name) {
		if (name == null) return error("method name is null");
		return jswrap(
			(ArgsList) x-> {
				Object obj = elt(x, 0);
				if (obj == null) return error("receiver is null");
				if (obj instanceof Apv apv) obj = apv.cmb;
				if (obj instanceof JSFun f) obj = f.jsfun; 
				//else error("not a JSFun: " + obj);
				
				Object[] args = list_to_array(cdr(x));
				Executable executable = getExecutable(name.equals("new") ? (Class) obj : obj.getClass(), name,  getClasses(args));
				if (executable != null)	try {
					if (executable.isVarArgs()) args = reorg(executable.getParameterTypes(), args);
					/* warning preview
					return switch (executable) {
						case Method m-> m.invoke(obj, args);
						case Constructor c-> c.newInstance(args);
					};
					* /
					if (executable instanceof Method m) return jswrap(m.invoke(obj, args));
					if (executable instanceof Constructor c) return c.newInstance(args);
					throw new RuntimeException("no method no constructor");   
				}
				catch (Exception exp) {
					throw new RuntimeException("error executing " + (name.equals("new") ? "constructor" : "method: " + name) + " in: " + x, exp);
				}
				else try {
					switch (name) {
						case "getMethod":
							//return jswrap(((Class) obj).getMethod((String) elt(x,1), Arrays.stream((list_to_array(cdr(cdr(x))))).toArray(Class[]::new)));
							//return jswrap(((Class) obj).getMethod((String) elt(x,1), toClassArray(list_to_array(cdr(cdr(x))))));
							return jswrap(((Class) obj).getMethod((String) elt(x,1), list_to_array(x, 2, Class.class)));
						case  "invoke":
							return ((Method) obj).invoke(elt(x,1), list_to_array(x, 2, Object.class));
						case "getConstructor":
							//return jswrap(((Class) obj).getConstructor(Arrays.stream(args).toArray(Class[]::new)));
							return jswrap(((Class) obj).getConstructor(toClassArray(args)));
						case "newInstance":
							return obj == Array.class
								? Array.newInstance((Class) elt(x, 1), Arrays.stream(list_to_array(x, 2, Integer.class)).mapToInt(i->i).toArray() )
								: ((Constructor) obj).newInstance(args);
					}
				}
				catch (Exception exp) {
					throw new RuntimeException("error executing method: " + name + " in: " + x, exp);
				}
				throw new RuntimeException("not found " + (name.equals("new") ? "constructor" : "method: " + name) + " in: " + x);
			}
		);
	}
	*/
	Object jsGetSetter(String name) {
		if (name == null) return error(name + " getter called with wrong args");
		return jswrap(
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
					return error("can't " + (len==1 ? "get" : "set") +" " + name + " of " + obj + (len == 1 ? "" : " with " + elt(x, 1)));
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
			$("vm-def", "vm-make-environment", jswrap((ArgsList) args-> env(args == nil ? null : (Env) car(args)))),
			$("vm-def", "vm-wrap", jswrap((Function<Object, Object>) this::wrap)),
			$("vm-def", "vm-unwrap", jswrap((Function<Object, Object>) this::unwrap)),
			// Values
			$("vm-def", "vm-cons", jswrap((BiFunction<Object, Object, Object>) this::cons)),
			$("vm-def", "vm-cons?", jswrap((Function<Object, Boolean>) obj-> obj instanceof Cons)),
			$("vm-def", "vm-nil?", jswrap((Function<Object, Boolean>) obj-> obj == nil)),
			$("vm-def", "vm-string-to-symbol", jswrap((Function<String, Sym>) this::sym)),
			$("vm-def", "vm-symbol?", jswrap((Function<Object, Boolean>) obj-> obj instanceof Sym)),
			$("vm-def", "vm-symbol-name", jswrap((Function<Sym, String>) this::sym_name)),
			// First-order Control
			$("vm-def", "vm-if", new If()),
			$("vm-def", "vm-loop", new Loop()),
			$("vm-def", "vm-throw", jswrap((Consumer<Object>) obj-> { throw new ValueException(obj); })),
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
			$("vm-def", "vm-error", jswrap((Function<String, Object>) this::error)),
			// JS Interface
			//$("vm-def", "vm-js-wrap", jswrap(jswrap)),
			//$("vm-def", "vm-js-unop", jswrap(js_unop)),
			//$("vm-def", "vm-js-binop", jswrap(js_binop)),
			$("vm-def", "vm-js-getter", jswrap((Function<String,Object>) this::jsGetSetter)),
			//$("vm-def", "vm-js-setter", jswrap(js_setter)),
			$("vm-def", "vm-js-invoker", jswrap((Function<String,Object>) this::jsInvoker)),
			//$("vm-def", "vm-js-function", jswrap(js_function)),
			//$("vm-def", "vm-js-global", JS_GLOBAL),
			//$("vm-def", "vm-js-make-object", jswrap(Object::new)),
			//$("vm-def", "vm-js-make-prototype", jswrap(make_prototype)),
			//$("vm-def", "vm-js-new", jswrap(jsnew)),
			//$("vm-def", "vm-type?", jswrap(is_type)),
			// Setters
			//$("vm-def", "vm-setter", SETTER),
			// Utilities
			$("vm-def", "vm-list", jswrap((ArgsList) o-> o)),
			$("vm-def", "vm-list*", jswrap((ArgsList) this::listToListStar)),
			$("vm-def", "vm-list-to-array", jswrap((Function<Object,Object[]>) this::listToArray)),
			$("vm-def", "vm-array-to-list", jswrap((BiFunction<Boolean,Object[],Object>) this::arrayToList)),
			$("vm-def", "vm-reverse-list", jswrap((Function) this::reverseList)),
			// 
			$("vm-def", "+", jswrap((BiFunction<Integer,Integer,Integer>) (a,b)-> a + b)),
			$("vm-def", "*", jswrap((BiFunction<Integer,Integer,Integer>) (a,b)-> a * b)),
			$("vm-def", "-", jswrap((BiFunction<Integer,Integer,Integer>) (a,b)-> a - b)),
			$("vm-def", "/", jswrap((BiFunction<Integer,Integer,Integer>) (a,b)-> a / b)),
			//
			$("vm-def", "!", jswrap((Function<Boolean,Boolean>) a-> !a)),
			$("vm-def", "not", jswrap((Function<Boolean,Boolean>) a-> !a)),
			$("vm-def", "<", jswrap((BiFunction<Integer,Integer,Boolean>) (a,b)-> a < b)),
			$("vm-def", "<=", jswrap((BiFunction<Integer,Integer,Boolean>) (a,b)-> a <= b)),
			$("vm-def", ">", jswrap((BiFunction<Integer,Integer,Boolean>) (a,b)-> a > b)),
			$("vm-def", ">=", jswrap((BiFunction<Integer,Integer,Boolean>) (a,b)-> a >= b)),
			//
			$("vm-def", "vm-quote", $("vm-vau", $("a"), ign, "a")),
			$("vm-def", "==", jswrap((BiFunction<Object,Object,Boolean>) (a,b)-> a == b)),
			$("vm-def", "!=", jswrap((BiFunction<Object,Object,Boolean>) (a,b)-> a != b)),
			$("vm-def", "eq?", jswrap((BiFunction<Object,Object,Boolean>) (a,b)-> equals(a, b))),
			$("vm-def", "assert", jsfun((ArgsList) l-> assertVm(listToArray(l)))),
			$("vm-def", "toString", jswrap((ArgsList) l-> this.toString(listToArray(l)))),
			$("vm-def", "print", jswrap((ArgsList) l-> this.print(listToArray(l)))),
			$("vm-def", "instanceof", jswrap((BiFunction<Object,Class,Boolean>)this::jsInstanceOf)),
			$("vm-def", "trace", jswrap((Consumer<Boolean>) b-> trace=b)),
			$("vm-def", "stack", jswrap((Consumer<Boolean>) b-> stack=b)),
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
	public Object exec(String fileName) throws Exception {
		return exec(readBytecode(fileName));
	}
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
		boolean inescape = false, instring = false, incomment=false, smlcomment=false, inmlcomment=false, emlcomment=false;
		do {
			out.print("> ");
			for (int c; (c = in.read()) != '\n';) {
				if (inescape) {
					inescape = false;
				}
				else if (instring) switch (c) {
					case '\\'-> inescape = true;
					case '"'-> instring = false;
				}
				else if (incomment) switch (c) {
					case '"'-> instring = true;
					case '\n'-> incomment = false;
				}
				/*else if (emlcomment) {
					if (c == '/') inmlcomment = emlcomment = false;
				}*/
				else if (emlcomment) switch (c) {
					case '/'-> inmlcomment = emlcomment = false;
				}
				else if (inmlcomment) switch (c) {
					case '"'-> instring = true;
					case ';'-> incomment = true;
					case '*'-> emlcomment = true;
				}
				/*else if (smlcomment) {
					if (c == '*') inmlcomment = true;
					smlcomment = false;
				}*/
				else if (smlcomment) switch (c) {
					case '*': inmlcomment = true;
					default : smlcomment = false;
				}
				else switch (c) {
					case '"'-> instring = true;
					case ';'-> incomment = true;
					case '/'-> smlcomment = true;
					case '('-> open += 1;
					case ')'-> close += 1;
				}
				if (c >= 32) s.append((char) c);
			}
			if (incomment) { s.append('\n'); incomment = false; }
		} while (open != close);
		return s.toString();
	}
	
	
	/* Test */
	public static void main(String[] args) throws Exception {
		new Vm().main();
	}
	
	public void main() throws Exception {
		//*
		//exec(parse(readFile("boot.wat")));
		//compile("boot.wat");
		//exec(readBytecode("boot.wat"));
		eval(readFile("boot.wat"));
		//eval(readFile("boot2.wat"));
		eval(readFile("test.wat"));
		//*/
		
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
		assertVm("$define!", TheEnvironment.get("vm-def"));
		assertVm("a", 1);
		assertVm("b", $(2,3));
		assertVm("(vm-if #t 1 2)", 1);
		assertVm("(vm-if #f 1 2)", 2);
		assertVm("(vm-if #null 1 2)", 2);
		assertVm("(($vau (x) #ignore 1 2 3 4 5 x) 6)", 6);
		//*/
		/*
		eval("""
			(assert (vm-def a 1) #ignore)
			(assert (vm-def a)          ) ;throw
		""");
		//*/		
		repl();
	}
}
