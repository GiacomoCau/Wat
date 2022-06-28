package Wat;

import static Expr.Parser.parse;
import static Wat.Utility.$;
import static Wat.Utility.isInstance;
import static java.lang.System.err;
import static java.lang.System.in;
import static java.lang.System.out;

import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
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

public class Vm {
	
	class Mark {}
	interface Evaluable { Object eval(Mark m, Env e); }
	interface Matchable { Object match(Env e, Object rhs); }
	abstract class Combinable { abstract Object combine(Mark m, Env e, Object o); }
	
	
	// Continuations
	record StackFrame(Function<Mark, Object> fun, StackFrame next, Object dbg, Env e) {}
	
	class Resumption extends Mark {
		StackFrame k; Function<Resumption, Object> f;
		Resumption (StackFrame k, Function<Resumption, Object> f) { this.k = k; this.f = f; }
		public String toString() { return "resumption"; }
	};
	Object resumeFrame(Resumption m) {
		return m.k.fun.apply(new Resumption(m.k.next, m.f));
	}
	
	class Suspension extends Mark {
		Suspension prompt; Combinable handler; StackFrame continuation;
		Suspension(Suspension prompt, Combinable handler) { this.prompt = prompt; this.handler = handler; }
		public String toString() { return "suspension"; }
	}
	Suspension suspendFrame(Suspension suspension, Function<Mark, Object> fun, Object dbg, Env e) {
		suspension.continuation = new StackFrame(fun, suspension.continuation, dbg, e);
		return suspension;
	}
	
	Object monadic(Mark m, Supplier<Object> a, Function<Object, Object>  b) {
		Object res = m instanceof Resumption r ? resumeFrame(r) : a.get();
		if (res instanceof Suspension s) return suspendFrame(s, mm-> monadic(mm, a, b), null, null);
		return b.apply(res);
	}
	
	
	/* Forms */
	class Nil implements Matchable {
		public Object match(Env e, Object rhs) { return rhs == NIL ? null : error("NIL expected, but got: " + rhs.toString()); }
		public String toString() { return "()"; }
	};
	public Nil NIL = new Nil();
	
	static class Ign implements Matchable {
		public Object match(Env e, Object rhs) { return null; } 
		public String toString() { return "#ignore"; }
	};
	public static Ign IGN = new Ign();
	
	boolean trace = false;
	
	/* Evaluation Core */
	Object evaluate(Mark m, Env e, Object o) {
		if (trace) print("eval:", o);
		return o instanceof Evaluable x ? x.eval(m, e) : o;
	}
	
	class Sym implements Evaluable, Matchable {
		String name;
		Sym(String name) { this.name = name; }
		public Object eval(Mark m, Env e) { return lookup(e, this); }
		public Object match(Env e, Object rhs) { e.put(this, rhs); if (trace) print("bind:", this.name, rhs, e); return IGN; }	
		public int hashCode() { return Objects.hashCode(name); }
		public boolean equals(Object obj) {
			if (this == obj) return true;
			if (obj == null || getClass() != obj.getClass()) return false;
			final Sym other = (Sym) obj;
			if (!name.equals(other.name)) return false;
			return true;
		}
		public String toString() { return name; }
	}
	Sym sym(String name) { return new Sym(name); }
	String sym_name(Sym sym) { return sym.name; }
	
	class Cons implements Evaluable, Matchable {
		Object car, cdr;
		Cons(Object car, Object cdr) { this.car = car; this.cdr = cdr; }
		public Object eval(Mark m, Env e) {
			return monadic(
				null,
				()-> evaluate(null, e, car),
				op-> combine(null, e, op, cdr)
			);
		}
		public Object match(Env e, Object rhs) {
			if (!(car instanceof Matchable mcar)) return error("cannot match against: " + car + " in: " + this);
			if (!(cdr instanceof Matchable mcdr)) return error("cannot match against: " + cdr + " in: " + this); 
			return monadic(
				null,
				()-> mcar.match(e, car(rhs)),
				__-> mcdr.match(e, cdr(rhs))
			);
		}
		public String toString() {	return "(" + toString(this) + ")"; }
		private String toString(Cons c) {
			if (c.cdr == NIL) return Vm.this.toString(c.car);
			if (c.cdr instanceof Cons cons) return Vm.this.toString(c.car) + " " + toString(cons);
			return Vm.this.toString(c.car) + " . " + Vm.this.toString(c.cdr);
		}
		public boolean equals(Object o) {
			if (!(o instanceof Cons c)) return false;
			if (!(car.equals(c.car))) return false;
			if (!(cdr.equals(c.cdr))) return false;
			return true;
		}
	}
	Cons cons(Object car, Object cdr) {
		return new Cons(car, cdr);
	};
	Object car(Object o) { // tc
		return o instanceof Cons cons ? cons.car : error("not a cons: " + o.toString());
	}
	Object cdr(Object o) { // tc
		return o instanceof Cons cons ? cons.cdr : error("not a cons: " + o.toString());
	}
	Object elt(Object o, int i) { for (; i>0; i-=1) o=cdr(o); return car(o); }
	int len(Object o) { int i=0; for (; o instanceof Cons c; o=c.cdr) i+=1; return i; }
	
	
	// Operative & Applicative Combiners
	Object combine(Mark m, Env e, Object cmb, Object o) {
		if (trace) print("combine:", cons(cmb, o));
		if (cmb instanceof Combinable combinable) return combinable.combine(m, e, o);
		if (isInstance(cmb, Function.class, BiFunction.class, ArgsList.class))
			return ((Combinable) jswrap(cmb)).combine(m, e, o);
		return error("not a combiner: " + cmb.toString() + " in: " + cons(cmb, o));
	}
	
	class Opv extends Combinable {
		Object p; Object ep; Object x; Env e;
		Opv(Object p, Object ep, Object x, Env e) { this.p = p; this.ep = ep; this.x = x; this.e = e; }
		public Object combine(Mark m, Env e, Object o) {
			var xe = env(this.e);
			return monadic(
				null,
				()-> bind(xe, p, o),
				__-> monadic(
					null,
					()-> bind(xe, ep, e),
					___-> evaluate(null, xe, x)
				)
			);
		}
		public String toString() {
			return "[Opv " + Vm.this.toString(p) + " " + Vm.this.toString(ep) + " " + Vm.this.toString(x) + "]";
		}
	}
	
	class Apv extends Combinable {
		Combinable cmb;
		Apv (Combinable cmb) { this.cmb = cmb; }
		public Object combine(Mark m, Env e, Object o) {
			return monadic(
				null,
				()-> evalArgs(null, e, o, NIL),
				args-> cmb.combine(null, e, args)
			);
		}
		Object evalArgs(Mark m, Env e, Object todo, Object done) {
			if (todo == NIL) return reverse_list(done);
			return monadic(
				null,
				()-> evaluate(null, e, car(todo)),
				arg-> evalArgs(null, e, cdr(todo), cons(arg, done))
			);
		}
		public String toString() { return "[Apv " + Vm.this.toString(cmb) + "]"; }
	}
	Object wrap(Object sExpr) { return sExpr instanceof Combinable cmb ? new Apv(cmb) : error("cannot wrap: " + sExpr); } // type check
	Object unwrap(Object sExpr) { return sExpr instanceof Apv apv ? apv.cmb : error("cannot unwrap: " + sExpr); } // type check
	
	
	/* Built-in Combiners */
	class Vau extends Combinable {
		Object combine(Mark m, Env e, Object o) {
			// o = (ptree envp expr)
			if (len(o) > 3) return error("too many operands in: " + cons(this, o));
			var ptree = elt(o, 0);
			var envp = elt(o, 1);
			var msg = new PTree(ptree, envp).check(); if (msg != null) return error(msg + " in " + cons(this, o));
			return new Opv(ptree, envp, elt(o, 2), e);
		}
		public String toString() { return "vm-vau"; }
	};
	class Def extends Combinable {
		Object combine(Mark m, Env e, Object o) {
			if (len(o) > 2) return error("too many operands in: " + cons(this, o));
			var lhs = elt(o, 0);
			var msg = new PTree(lhs).check(); if (msg != null) return error(msg + " in " + cons(this, o));
			var rhs = elt(o, 1);
			return monadic(
				null,
				()-> evaluate(null, e, rhs),
				val-> bind(e, lhs, val)
			);
		}
		public String toString() { return "vm-def"; }
	};
	class PTree {
		private Object ptree, envp;
		private Set symbols = new HashSet<Sym>();
		PTree(Object ptree) { this.ptree = ptree; }
		PTree(Object ptree, Object envp) { this(ptree); this.envp = envp; }
		Object check() { 
			if (envp != null && envp != IGN && !(envp instanceof Sym)) return "not #ignore or a symbol: " + envp;
			var msg = check(ptree); if (msg != null) return msg;
			return envp == null || envp == IGN || !symbols.contains(envp) ? null : "not a unique symbol: " + envp;
		}
		private Object check(Object ptree) {
			if (ptree == NIL || ptree == IGN) return null;
			if (ptree instanceof Sym) { return symbols.add(ptree) ? null : "not a unique symbol: " + ptree; }
			if (ptree instanceof Cons cons) { var msg = check(cons.car); return msg != null ? msg : check(cons.cdr); }
			return "not a symbol: " + ptree;
		}
	}
	class Eval extends Combinable {
		Object combine(Mark m, Env e, Object o) {
			var x = elt(o, 0);
			var eo = elt(o, 1);
			return evaluate(m, (Env) eo, x);
		}		
		public String toString() { return "vm-eval"; }
	}
	
	
	/* First-order Control */
	class Begin extends Combinable {
		Object combine(Mark m, Env e, Object o) {
			return o == NIL ? null : begin(m, e, o);
		}
		Object begin(Mark m, Env e, Object xs) {
			return monadic(
				null,
				()-> evaluate(null, e, car(xs)),
				res-> {
					var kdr = cdr(xs);
					return kdr == NIL ? res : begin(null, e, kdr);
				}
			);
		}
		public String toString() { return "vm-begin"; }
	};
	class If extends Combinable {
		Object combine(Mark m, Env e, Object o) {
			if (len(o) > 3) return error("too many operands in: " + cons(this, o));
			return monadic(
				null,
				()-> evaluate(null, e, elt(o, 0)),
				test-> evaluate(null, e, test != null && test != NIL && test instanceof Boolean b && b ? elt(o, 1) : elt(o, 2))
			);
			
		}
		public String toString() { return "vm-if"; }
	}
	class Loop extends Combinable {
		Object combine(Mark m, Env e, Object o) {
			var first = true; // only resume once
			while (true) {
				var res = first && m instanceof Resumption r ? resumeFrame(r) : evaluate(null, e, elt(o, 0));
				first = false;
				if (res instanceof Suspension s) return suspendFrame(s, mm-> combine(mm, e, o), elt(o, 0), e);
			}
		}
		public String toString() { return "vm-loop"; }
	}
	class Catch extends Combinable {
		Object combine(Mark m, Env e, Object o) {
			var x = elt(o, 0);
			var handler = elt(o, 1);
			Object res = null;
			try {
				res = m instanceof Resumption r ? resumeFrame(r) : evaluate(null, e, x);
			}
			catch (Throwable exc) {
				// unwrap handler to prevent eval if exc is sym or cons
				res = Vm.this.combine(null, e, unwrap(handler), list(exc));
			}
			if (res instanceof Suspension s) suspendFrame(s, mm-> combine(mm, e, o), x, e);
			return res;
		}
		public String toString() { return "vm-catch"; }
	}
	class Finally extends Combinable {
		@SuppressWarnings("finally")
		Object combine(Mark m, Env e, Object o) {
			var prot = elt(o, 0);
			var cleanup = elt(o, 1);
			Object res = null;
			try {
				res = m instanceof Resumption r ? resumeFrame(r) : evaluate(null, e, prot);
				if (res instanceof Suspension s) suspendFrame(s, mm-> combine(mm, e, o), prot, e);
			}
			finally {
				return res instanceof Suspension s ? s : doCleanup(null, e, cleanup, res);
			}
		}
		Object doCleanup(Mark m, Env e, Object cleanup, Object res) {
			var fres = m instanceof Resumption r ? resumeFrame(r) : evaluate(null, e, cleanup);
			if (fres instanceof Suspension s) suspendFrame(s, mm-> doCleanup(mm, e, cleanup, res), cleanup, e);
			return fres;
		}
		public String toString() { return "vm-finaly"; }
	}
	
	
	/* Delimited Control */
	class PushPrompt extends Combinable {
		Object combine(Mark m, Env e, Object o) {
			var prompt = elt(o, 0);
			var x = elt(o, 1);
			var res = m instanceof Resumption r ? resumeFrame(r) : evaluate(null, e, x);	
			if (!(res instanceof Suspension s)) return res;
			if (s.prompt != prompt) return suspendFrame(s, mm-> combine(mm, e, o), x, e);
			return Vm.this.combine(null, e, s.handler, cons(s.continuation, NIL));
		}
		public String toString() { return "vm-pushp-rompt"; }
	}

	class TakeSubcont extends Combinable {
		Object combine(Mark m, Env e, Object o) {
			var prompt = elt(o, 0);
			if (!(prompt instanceof Suspension s)) return error("not a suspend " + prompt); 
			var handler = elt(o, 1);
			if (!(handler instanceof Combinable c)) return error("not a combine " + handler); 
			var cap = new Suspension(s, c);
			return suspendFrame(cap, mm-> Vm.this.combine(null, e, ((Resumption) mm).f, NIL), this, e);
		}
		public String toString() { return "vm-take-subcont"; }
	}
	
	class PushSubcont extends Combinable {
		Object combine(Mark m, Env e, Object o) {
			var thek = elt(o, 0);
			if (!(thek instanceof StackFrame k)) return error("not a stackframe " + thek); 
			var thef = elt(o, 1);
			if (!(thef instanceof Function f)) return error("not a fun " + thef); 
			var res = m instanceof Resumption r ? resumeFrame(r) : resumeFrame(new Resumption(k, f));
			if (res instanceof Suspension s) suspendFrame(s, mm-> combine(mm, e, o), thef, e);
			return res;
		}
		public String toString() { return "vm-push-subcont"; }
	}
	
	class PushPromptSubcont extends Combinable {
		Object combine(Mark m, Env e, Object o) {
			var prompt = elt(o, 0);
			var thek = elt(o, 1);
			if (!(thek instanceof StackFrame k)) return error("not a stackframe " + thek); 
			var thef = elt(o, 2);
			if (!(thef instanceof Function f)) return error("not a fun " + thef); 
			var res = m instanceof Resumption r ? resumeFrame(r) : resumeFrame(new Resumption(k, f));
			if (!(res instanceof Suspension s)) return res;
			if (s.prompt != prompt) return suspendFrame(s, mm-> combine(mm, e, o), thef, e);
			return Vm.this.combine(null, e, s.handler, cons(s.continuation, NIL));
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
		Object combine(Mark m, Env e, Object o) { return new DV(elt(o, 0));	}
		public String toString() {	return "vm-dref"; }
	}
	class DRef extends Combinable {
		Object combine(Mark m, Env e, Object o) {
			var x = elt(o, 0);
			return x instanceof DV dv ? dv.val : error("not dinamic variable: " + x);
		}
		public String toString() {	return "vm-dnew"; }
	}
	class DLet extends Combinable {
		Object combine(Mark m, Env e, Object o) {
			var xdv = elt(o, 0);
			if (!(xdv instanceof DV dv)) return error("not dinamic variable: " + xdv);
			var val = elt(o, 1);
			var x = elt(o, 2);
			var oldVal = dv.val;
			try {
				dv.val = val;
				var res = m instanceof Resumption r ? resumeFrame(r) : evaluate(null, e, x);
				if (res instanceof Suspension s) suspendFrame(s, mm-> combine(m, e, o), x, e);
				return res;
			}
			finally {
				dv.val = oldVal;
			}
			
		}
		public String toString() {	return "vm-dlet"; }
	}
	
	/* Environment */
	class Env {
		Map<String,Object> map = new LinkedHashMap();
		Env parent;
		Env() { this(null); };
		Env(Env parent) { this.parent = parent; }
		public Object get(Sym sym) { return get(sym.name); };
		public Object get(String name) { return map.get(name); };
		public Object put(Sym sym, Object rhs) { return put(sym.name, rhs); }
		public Object put(String name, Object rhs) { return map.put(name, rhs); }
		public String toString() {	return "[Env " + map + " " + parent + "]"; }
	}
	Env env() { return new Env(null); }
	Env env(Env parent) { return new Env(parent); }
	//Object lookup(Env e, Sym sym) {	Object sExpr = e.get(sym); return sExpr != null ? sExpr : e.parent != null ? lookup(e.parent, sym) : error("unbound: " + sym); };
	Object lookup(Env e, Sym sym) {
		for(;;) {
			Object value = e.get(sym); 
			if (value != null) { if (trace) print("lookup:", value); return value; }
			if (e.parent == null) return error("unbound: " + sym);
			e = e.parent;
		}
	}
	
	Object bind(Env e, Object lhs, Object rhs) {
		if (lhs instanceof Matchable matchable) { matchable.match(e, rhs); return IGN; }
		return error("cannot match against: " + lhs);
	}
	
	
	/* Error handling */
	Object ROOT_PROMPT = new Object() {
		public String toString() { return "ROOT_PROMPT"; }
	};
	Object push_root_prompt(Object x) { return list(new PushPrompt(), ROOT_PROMPT, x); }
	Object error(String err) {
		//console.log(err)
		var user_break = the_environment.get(sym("user-break"));
		if (user_break == null) throw new RuntimeException(err);
		return combine(null, the_environment, user_break, list(err));
	}
	
	
	/* Utilities */
	Object list(Object ... args) {
		return array_to_list(true, args);
	}
	Object list_star(Object ... args) {
		return array_to_list(false, args);
	}
	Object array_to_list(boolean b, Object ... args) {
		var len = args.length-1;
		var c = b || len < 0 ? NIL : args[len];
		for (var i=len-(b?0:1); i>=0; i-=1) c = cons(args[i], c);
		return c;
	}
	Object[] list_to_array(Object c) {
		var res = new ArrayList();
		for (; c != NIL; c = cdr(c)) res.add(car(c));
		return res.toArray();
	}
	Object reverse_list(Object list) {
		Object res = NIL;
		for (; list != NIL; list = cdr(list)) res = cons(car(list), res);
		return res;
	}
	Object list_to_list_star(Object h) {
		if (!(h instanceof Cons)) return h;
		var o = cdr(h); 
		if (o == NIL) return car(h);
		if (!(o instanceof Cons)) return error("not a proper list: " + toString(h));
		var o2 = cdr(o);
		if (o2 == NIL) 
			((Cons) h).cdr = ((Cons) o).car;
		else if (!(o2 instanceof Cons))
			return error("not a proper list: " + toString(h));
		else {
			for (Object o3; (o3=cdr(o2)) != NIL; o=o2, o2=o3) {
				if (!(o3 instanceof Cons))	return error("not a proper list: " + toString(h));
			}
			((Cons) o).cdr = ((Cons) o2).car;
		}
		return h;
	}
	<T> T print(T ... o) {
		out.println(toString(o));
		return o[o.length - 1];
	}
	boolean eq(Object a, Object b) {
		if (a instanceof Object[] aa && b instanceof Object[] ab) return Arrays.deepEquals(aa, ab);
		if (a instanceof Cons ca && b instanceof Cons cb) return eq(ca.car, cb.car) && eq(ca.cdr, cb.cdr);
		return a == b;
	}
	void assertEq(String expr, Object expected) {
		try {
			var value = eval(expr);
			if (expected instanceof Object[] ea && value instanceof Object[] va && Arrays.deepEquals(ea, va))
					return;
			else if (parse_bytecode(expected).equals(value))
					return;
			//if (eq(value, parse_bytecode(expected))) return;
			err.println(expr + " should be " + toString(expected) + " but is " + value);
		}
		catch (Throwable t) {
			if (!(expected instanceof Throwable)) 
				err.println(t.getMessage());
				//t.printStackTrace();
		}
	}
	Void assertEq(Object ... os) {
		try {
			var a = os[0];
			var v = evaluate(null, env(the_environment), a);
			if (os.length == 1)
				print(a, "should be throw but is", v);
			else {
				var b = os[1];
				if (!eq(v, b)) print(a, "should be", b, "but is", v);
			}
		}
		catch (Throwable t) {
			if (os.length > 1)
				print(os[0], "throw", t);
				//err.println(t.getMessage());
				//t.printStackTrace();
		}
		return null;
	}
	
	/* Bytecode parser */
	Object parse_bytecode(Object o) {
		if (o instanceof String s) return s.equals("#ignore") ? IGN : sym(s);
		if (o instanceof Object[] a) return parse_bytecode(a);
		return o;
	}
	/*
	Object parse_bytecode(Object ... objs) {
		if (objs.length == 0) return NIL;
		if (objs.length == 2 && objs[0].equals("wat-string")) return objs[1];
		Object head = cons(parse_bytecode(objs[0]), NIL), cons = head;
		for (int i=1; i<objs.length; i+=1) {
			if (objs[i].equals(".")) {
				if (i != objs.length-2) throw new RuntimeException(". not is the penultimate element in " + objs);
				((Cons) cons).cdr = parse_bytecode(objs[i+1]);
				return head;
			}
			var x = ((Cons) cons).cdr = cons(parse_bytecode(objs[i]), NIL);
			cons = x;
		}
		return head;
	}
	//*/
	Object parse_bytecode(Object ... objs) {
		if (objs.length == 0) return NIL;
		if (objs.length == 2 && objs[0].equals("wat-string")) return objs[1];
		Object head = cons(parse_bytecode(objs[0]), NIL), cons = head;
		for (int i=1; i<objs.length; i+=1) {
			var obj = objs[i];
			if (obj == null || !obj.equals(".")) {
				cons = ((Cons) cons).cdr = cons(parse_bytecode(objs[i]), NIL);
				continue;
			}
			if (i != objs.length-2) throw new RuntimeException(". not is the penultimate element in " + objs);
			((Cons) cons).cdr = parse_bytecode(objs[i+1]);
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
		Object combine(Mark m, Env e, Object o) {
			return switch (jsfun) {
				case ArgsList f -> f.apply(o);  
				case Function f -> f.apply(car(o));  
				case BiFunction f -> f.apply(elt(o,0), elt(o,1));
				default -> error("not a combine " + jsfun);
			};
		}
		public String toString() {return "JSFun" /*+jsfun.getClass().getSimpleName()*/; }
	}
	Object jswrap(Object jsfun) {
		if (!isInstance(jsfun, ArgsList.class, Function.class, BiFunction.class, Consumer.class)) return error("no fun:" + jsfun);
		return wrap(new JSFun(jsfun));
	}
	
	
	/* Stringification */
	String toString(Object ... os) {
		var r = "";
		for (var o: os) {
			if (!r.isEmpty()) r+= " ";
			if (o == null) r+= "null";
			else if (!(o instanceof Object[] a)) r+= o.toString();
			else {
				var s =""; for (var e: a) s+= (s.isEmpty() ? "" : " ") + toString(e); r+= "(" + s + ")";
			}
		}
		return r;
	}
	
	/* Bootstrap */
	Object builtin_bytecode =
		$("vm-begin",
			// Basics
			$("vm-def", "vm-vau", new Vau()),
			$("vm-def", "vm-eval", wrap(new Eval())),
			$("vm-def", "vm-make-environment", jswrap((Function<Env, Env>) parent-> env(parent))),
			$("vm-def", "vm-wrap", jswrap((Function<Object, Object>) this::wrap)),
			$("vm-def", "vm-unwrap", jswrap((Function<Object, Object>) this::unwrap)),
			// Values
			$("vm-def", "vm-cons", jswrap((BiFunction<Object, Object, Object>) this::cons)),
			$("vm-def", "vm-cons?", jswrap((Function<Object, Boolean>) obj-> obj instanceof Cons)),
			$("vm-def", "vm-nil?", jswrap((Function<Object, Boolean>) obj-> obj == NIL)),
			$("vm-def", "vm-string-to-symbol", jswrap((Function<String, Sym>) this::sym)),
			$("vm-def", "vm-symbol?", jswrap((Function<Object, Boolean>) obj-> obj instanceof Sym)),
			$("vm-def", "vm-symbol-name", jswrap((Function<Sym, String>) this::sym_name)),
			// First-order Control
			$("vm-def", "vm-if", new If()),
			$("vm-def", "vm-loop", new Loop()),
			$("vm-def", "vm-throw", jswrap((Consumer<String>) err-> { throw new RuntimeException(err); })),
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
			$("vm-def", "vm-root-prompt", ROOT_PROMPT),
			$("vm-def", "vm-error", jswrap((Function<String, Object>) this::error)),
			// JS Interface
			$("vm-def", "vm-js-wrap", jswrap((Function) this::jswrap)),
			//$("vm-def", "vm-js-unop", jswrap(js_unop)),
			//$("vm-def", "vm-js-binop", jswrap(js_binop)),
			//$("vm-def", "vm-js-getter", jswrap(js_getter)),
			//$("vm-def", "vm-js-setter", jswrap(js_setter)),
			//$("vm-def", "vm-js-invoker", jswrap(js_invoker)),
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
			$("vm-def", "vm-list*", jswrap((ArgsList) this::list_to_list_star)),
			$("vm-def", "vm-list-to-array", jswrap((Function<Object,Object[]>) this::list_to_array)),
			$("vm-def", "vm-array-to-list", jswrap((BiFunction<Boolean,Object[],Object>) this::array_to_list)),
			$("vm-def", "vm-reverse-list", jswrap((Function) this::reverse_list)),
			// 
			$("vm-def", "+", jswrap((BiFunction<Integer,Integer,Integer>) (a,b)-> a + b)),
			$("vm-def", "*", jswrap((BiFunction<Integer,Integer,Integer>) (a,b)-> a * b)),
			//
			$("vm-def", "vm-qot", $("vm-vau", $("a"), IGN, "a")),
			$("vm-def", "==", jswrap((BiFunction<Object,Object,Boolean>) (a,b)-> a == b)),
			$("vm-def", "eq", jswrap((BiFunction<Object,Object,Boolean>) (a,b)-> eq(a,b))),
			$("vm-def", "assert", new JSFun((ArgsList) l-> assertEq(list_to_array(l))))
		)
	;
	Env the_environment = env(); {
		bind(the_environment, sym("vm-def"), new Def());
		bind(the_environment, sym("vm-begin"), new Begin());
		evaluate(null, the_environment, parse_bytecode(builtin_bytecode));
	}
	
	
	/* API */
	public Object exec(Object bytecode) {
		var wrapped = push_root_prompt(cons(new Begin(), parse_bytecode(bytecode)));
		var res = evaluate(null, the_environment, wrapped);
		if (res instanceof Suspension s) throw new RuntimeException("prompt not found: " + s.prompt);
		return res;
	}
	public Object call(String fun_name, Object ... args) {
		return exec(list(sym(fun_name), parse_bytecode(args)));
	}
	public Object get(String var_name) {
		return exec(sym(var_name));
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
			oos.writeObject(parse(readString(fileName)));
		}
	}
    public static String readString(String filename) throws IOException {
        return Files.readString(Paths.get(filename));
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
			switch (readLine()) {
				case "": break loop;
				case String line: try {
					//out.println(line);
					//out.println(to_string(parse(line)));
					print(toString(exec(parse(line))));
				}
				catch (Throwable t) {
					t.printStackTrace();
				}
			}
		}
		print("finito");
	}
	private String readLine() throws IOException {
		var s = new StringBuilder();
		int open = 0, close = 0;
		do {
			out.print("> ");
			for (int c; (c = in.read()) != '\n';) {
				switch (c) { case '('-> open+=1; case ')'-> close+=1; }
				if (c >= 32) s.append((char)c);
			}
		} while (open != close);
		return s.toString();
	}
	
	
	/* Test */
	public static void main(String[] args) throws Exception {
		new Vm().main();
	}
	
	public void main() throws Exception {
		//*
		//exec(parse(readString("boot.wat")));
		//eval(readString("boot.wat"));
		//compile("boot.wat");
		//exec(readBytecode("boot.wat"));
		//*/
					
		/*
		print(parse_bytecode("a"));
		print(parse_bytecode("#ignore"));
		print(parse_bytecode($()));
		print(parse_bytecode($("a")));
		print(parse_bytecode($("a", "b")));
		print(parse_bytecode($("a", "b", ".", $())));
		print(parse_bytecode($("a", "b", ".", "c")));
		print(parse_bytecode($("a", $("b"), ".", "c")));
		print(parse_bytecode($("wat-string", "string")));
		//*/
		/* solo se parse_bytecode -> $
		print(vm.$("a"));
		print(vm.$("#ignore"));
		print(vm.$());
		print(vm.$("a"));
		print(vm.$("a", "b"));
		print(vm.$("a", "b", ".", vm.$()));
		print(vm.$("a", "b", ".", "c"));
		print(vm.$("a", vm.$("b"), ".", "c"));
		print(vm.$("wat-string", "string"));
		//*/
		/*
		print(list());
		print(list("a"));
		print(list("a", "b"));
		print(array_to_list(true));
		print(array_to_list(true, "a"));
		print(array_to_list(true, "a", "b"));
		print(list_star());
		print(list_star("a"));
		print(list_star("a", "b"));
		print(array_to_list(false));
		print(array_to_list(false, "a"));
		print(array_to_list(false, "a", "b"));
		//*/
		/*
		print(parse_bytecode(parse("(vm-cons 1 2)"))); // -> ((vm-cons 1 2))
		//*/	
		/*
		print(list_to_list_star(1)); // -> 1 
		print(list_to_list_star(NIL)); // -> ()
		print(list_to_list_star(list())); // -> ()
		print(list_to_list_star(list(1, 2))); // -> (1 . 2)
		print(list_to_list_star(list(1, 2, 3))); // -> (1 2 . 3)
		//print(list_to_list_star(list_star(1, 2))); -> not a proper list
		//print(list_to_list_star(list_star(1, 2, 3)));  -> not a proper list
		//print(list_to_list_star(list_star(1, 2, 3, 4)));  -> not a proper list
		//*/

		/*
		assertEq("1", 1);
		assertEq("(vm-begin 1 2 3)", 3);
		assertEq("(vm-cons 1 2)", $(1,".",2));
		assertEq("(vm-list 1 2 3)", $(1,2,3));
		assertEq("(vm-list* 1 2 3 4)",$(1,2,3,".",4));
		assertEq("(vm-list-to-array (vm-list 1 2 3 4))", new Object[] {1,2,3,4});
		assertEq("(vm-array-to-list #t (vm-list-to-array (vm-list 1 2 3 4)))", $(1,2,3,4));
		assertEq("(vm-array-to-list #f (vm-list-to-array (vm-list 1 2 3 4)))", $(1,2,3,".",4));
		assertEq("(vm-reverse-list (vm-list 1 2 3 4))", $(4,3,2,1));
		
		assertEq("(+ 1 2)", 3);
		assertEq("(* 3 2)", 6);
		assertEq("(* (* 3 2) 2)", 12);
		
		assertEq("((vm-vau a #ignore a) 1 2)", $(1,2));
		assertEq("((vm-vau (a b) #ignore b) 1 2)", 2);
		assertEq("((vm-vau (a . b) #ignore b) 1 2 3)", $(2,3));
		assertEq("(vm-begin (vm-def x (vm-list 1)) x)", $(1));
		assertEq("(vm-def x 1)((vm-wrap(vm-vau (a) #ignore a)) x)", 1);
		
		assertEq("(vm-def () 1) a", Throw);
		assertEq("(vm-def a (vm-list 1)) a", $(1));
		assertEq("(vm-def (a) (vm-list 1)) a", 1);
		assertEq("(vm-def a (vm-list 1 2 3)) a", $(1,2,3));
		assertEq("(vm-def (a) 1 2 3) a", Throw);
		assertEq("(vm-def 2 1)", Throw);
		assertEq("(vm-def (a) (vm-list 1)) a", 1);
		assertEq("(vm-def (a b) (vm-list 1 2)) a b", 2);
		assertEq("(vm-def (a . b) (vm-list 1 2 3)) a b", $(2,3)); 
				
		assertEq("(vm-vau (a . 12) #ignore 0)", Throw);
		assertEq("(vm-def (a b 12) 1)", Throw);
		
		assertEq("(vm-qot a b c)", Throw);

		assertEq("(vm-def 2 1)", Throw);
		assertEq("(vm-def a 1) a", 1);
		assertEq("(vm-def (a . b) (vm-list 1 2 3)) a b", $(2,3));
		assertEq("(vm-def define vm-def) (define a 1) a", 1);
		assertEq("(vm-def $define! vm-def) ($define! a 1) a", 1);
		assertEq("$define!", the_environment.get("vm-def"));
		assertEq("a", 1);
		assertEq("b", $(2,3));
		assertEq("(vm-if #t 1 2)", 1);
		assertEq("(vm-if #f 1 2)", 2);
		assertEq("(vm-if #null 1 2)", 2);
		assertEq("(($vau x #ignore 1 2 3 4 5 x) 6)", 6);
		//*/
		
		eval("""
			(assert (vm-def a 1) #ignore)
			(assert (vm-def a)          ) ;throw
			
			(vm-def $define! vm-def)
			
			;; Rename bindings that will be used as provided by VM
			($define! array->list vm-array-to-list)
			($define! begin vm-begin)
			($define! cons vm-cons)
			($define! cons? vm-cons?)
			($define! dnew vm-dnew)
			($define! dref vm-dref)
			($define! error vm-error)
			($define! eval vm-eval)
			($define! if vm-if)
			;($define! js-getter vm-js-getter)
			;($define! js-global vm-js-global)
			;($define! js-invoker vm-js-invoker)
			($define! list* vm-list*)
			($define! list->array vm-list-to-array)
			($define! make-environment vm-make-environment)
			;($define! new vm-js-new)
			($define! nil? vm-nil?)
			($define! reverse-list vm-reverse-list)
			;($define! setter vm-setter)
			($define! string->symbol vm-string-to-symbol)
			($define! symbol-name vm-symbol-name)
			($define! symbol? vm-symbol?)
			($define! throw vm-throw)
			($define! unwrap vm-unwrap)
			($define! wrap vm-wrap)
			
			;; Important utilities
			($define! $vau vm-vau)
			($define! quote ($vau (x) #ignore x))
			($define! list (wrap ($vau elts #ignore elts)))
			($define! list vm-list)
			($define! the-environment ($vau () e e))
			($define! get-current-environment (wrap ($vau () e e)))
			
			;;;; Macro and vau
			
			;(@log &console "da qui ---------------------")
			
			; derivazione Shutt!
			($define! $vau
			  ((wrap
			     ($vau ($vau) #ignore
			       ($vau (formals eformal . body) env
			         (eval (list $vau formals eformal (list* begin body)) env) )))
			  $vau ))
			(($vau (x) #ignore 1 2 3 4 x) 1)
			  
		""");		
		repl();
	}
	
	Throwable Throw = new Throwable() {
		private static final long serialVersionUID = 1L;
		public String toString() { return "a throw"; };		
	};
}
