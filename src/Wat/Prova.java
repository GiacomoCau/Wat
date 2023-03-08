package Wat;

import static java.lang.System.out;
import static java.nio.charset.Charset.forName;
import static java.nio.file.Files.readString;
import static java.nio.file.Paths.get;
import static java.util.Map.of;
import static java.util.regex.Pattern.compile;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.PrintWriter;
import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.net.URI;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.stream.Collectors;

import javax.tools.DiagnosticCollector;
import javax.tools.JavaCompiler;
import javax.tools.JavaFileObject;
import javax.tools.SimpleJavaFileObject;
import javax.tools.ToolProvider;

import List.Parser;
import Wat.Vm.Keyword;
import Wat.Vm.Symbol;

public class Prova {
	
	//class $ {}

	//record Key(Object ... a) {
	static class Key { Object[] a; Key(Object ... a) { this.a = a; }
		@Override public boolean equals(Object obj) { return obj instanceof Object[] b && Arrays.deepEquals(a, b); }
		@Override public int hashCode() { return Arrays.deepHashCode(a); }
	}
	
	public static void main(String[] args) throws Exception {
		extend2("Wat.Vm$Box2",null);
	}

	public static void key() {
		var map = new HashMap<Key,Object>();
		out.println(new Key(1, " ", true).equals(new Key(1, " ", true)));
		map.put(new Key(1, " ", true), 2);
		out.println(map.containsKey(new Key(1, " ", true)));
	}

	public static void box() throws NoSuchMethodException {
		//extend2("Ooo", null);
		Box.class.getConstructor((Class[]) null);
		Box.class.getConstructor((Class[]) null);
		out.println(Box.class); // -> Wat.Prova$Box
		out.println(Box.class.getSuperclass()); // -> Object
		out.println(Box.class.getClass()); // -> Class
		out.println(Box.class.getClass().getClass()); // -> Class
		out.println(Box.class.getClass().getSuperclass()); // -> Object
		out.println(Box.class.getClass().getSuperclass().getClass()); // -> Class
		out.println(Box.class.getClass().getSuperclass().getClass().getClass()); // -> Class
		new Box(1).get();
	}

	/* funziona ma ha bisogno di una istanza di Vm
	public void extend2(String className, Class superClass) throws Exception{
		class JavaStringFile extends SimpleJavaFileObject {
		    final String code;
		    JavaStringFile(String name, String code) {
		        super(URI.create("string:///" + name.replace('.','/') + Kind.SOURCE.extension), Kind.SOURCE); this.code = code;
		    }
		    @Override public CharSequence getCharContent(boolean ignoreEncodingErrors) { return code; }
		}
	    var diagnostics = new DiagnosticCollector<JavaFileObject>();
	    var task = ToolProvider.getSystemJavaCompiler().getTask(
	    	null, null, diagnostics,
	    	java.util.List.of("-d", "bin", "--enable-preview", "-source", "19", "-Xlint:unchecked" ),
	    	null,
	    	java.util.List.of(
		    	new JavaStringFile("Ext.StdObj2", """
					package Ext;
					import Wat.Vm;
					public class StdObj2 extends Vm.StdObj {
						public StdObj2(Vm vm, Vm.List l) { vm.super(l); }
					}
					"""
				)
	    	)
	    );
	    if (task.call()) {
	    	var cls = Class.forName("Ext.StdObj2");
	    	out.println(cls.getCanonicalName());
	    	cls.getConstructor(Vm.class, List.class).newInstance(Vm.this, null);
	    }
	    else {
	    	System.out.println(diagnostics.getDiagnostics());
	    }
	}
    */
	public static void extend2(String className, Class superClass){
		class JavaStringFile extends SimpleJavaFileObject {
		    final String code;
		    JavaStringFile(String name, String code) {
		        super(URI.create("string:///" + name.replace('.','/') + Kind.SOURCE.extension), Kind.SOURCE); this.code = code;
		    }
		    @Override public CharSequence getCharContent(boolean ignoreEncodingErrors) { return code; }
		}
	    var diagnostics = new DiagnosticCollector<JavaFileObject>();
	    var task =	ToolProvider.getSystemJavaCompiler().getTask(
	    	null, null, diagnostics,
	    	java.util.List.of("-d", "bin", "--enable-preview", "-source", "19", "-Xlint:unchecked" ),
	    	null,
	    	java.util.List.of(
	    		/*new JavaStringFile("Wat.Vm$", """
					package Ext;
					import Wat.Vm;
					/*public* / class %1$s extends %2$s {
						private static final long serialVersionUID = 1L;
						public %1$s(Vm.List l) { super(l); }
						@Override public String toString() { return "{%1$s" + Vm.reverseMap(this) + "}"; }
					}
					""".formatted(className, superClass == null ? "Vm.StdObj" : superClass.getCanonicalName())
				)*/
		    	new JavaStringFile("Wat.Vm$StdObj2", """
					package Wat;
					import Wat.Vm;
					class StdObj2 extends Vm.StdObj {
						private static final long serialVersionUID = 1L;
						public StdObj2(Vm.List l) { super(Vm.this, l); }
					}
					"""
				)
	    	)
	    );
	    if (!task.call()) System.out.println(diagnostics.getDiagnostics());
	}

	static void stackLength() {
		out.println(new Throwable().getStackTrace().length);
	}

	static void nullKey() {
		var m = new LinkedHashMap();
		m.put(null, 3);
		out.println(m.get(null));
	}
	
	static void watObject() throws IllegalAccessException, NoSuchFieldException {
		//dynamicCompilation3("Test2", "Vm.WatClass");
		//dynamicCompilation3("Test3", "Test2");
		
		//out.println(Wat1.class instanceof WatObj0);
		//out.println(Wat1.class instanceof WatObj0.class);
		//out.println(Wat1.class.isInstance(WatObj0.class));
		out.println(WatObj0.class.isAssignableFrom(Wat1.class));
		Wat0.methods.put("m", 1);
		Wat0 w0 = new Wat0();
		w0.slot.put("a", 1);
		Wat1.methods.put("m", 2);
		Wat1 w1 = new Wat1();
		w1.slot.put("b", 2);
		out.println(Wat1.class.getSuperclass());
		var wat1 = Wat1.class;
		var wat1Methods = (Map) wat1.getDeclaredField("methods").get(wat1);
		var wat0 = wat1.getSuperclass();
		var wat0Methods = (Map) wat0.getDeclaredField("methods").get(wat0);
		out.println(wat0Methods.get("m"));
		wat0Methods.put("m", 3);
		out.println(wat0Methods.get("m"));
		
		putMethod(WatObj0.class, "x", 0);
		out.println(getMethod(Wat2.class, "x"));
		out.println(getMethod(Wat2.class, "o"));
	}
	
	static class WatObj0 {
		static Map<String, Object> methods = new LinkedHashMap();
		Map<String,Object> slot = new LinkedHashMap();
	}
	static class Wat0 extends WatObj0 {
		static Map<String, Object> methods = new LinkedHashMap();
	}
	static class Wat1 extends Wat0 {
		static Map<String, Object> methods = new LinkedHashMap();
	}
	static class Wat2 extends Wat1 {
		static Map<String, Object> methods = new LinkedHashMap();
	}
	
	static Map<String,Object> getMethods(Class<? extends WatObj0> cls) {
		try {
			return (Map<String,Object>) cls.getDeclaredField("methods").get(cls);
		}
		catch (Exception e) {
			return null;
		}
	}
	static Object getMethod0(Class<? extends WatObj0> cls, String name) {
		try {
			return getMethods(cls).get(name);
		}
		catch (Exception e) {
			return null;
		}
	}
	static Object getMethod(Class<? extends WatObj0> cls, String name) {
		Class c = cls; do {
			var m = getMethod0(c, name); if (m != null) return m;
		} while (WatObj0.class.isAssignableFrom(c = c.getSuperclass()));
		return null;
	}
	static Object putMethod(Class<? extends WatObj0> cls, String name, Object method) {
		return getMethods(cls).put(name, method);
	}
	
	static Class extend(String className, Class<? extends WatObj0> superClass) throws Exception {
		String source = """
			package Wat;
			class %1$s extends %2$s { 
				static Map<String, Object> methods = new LinkedHashMap();
				@Override public String toString() { return "{%1$s" + vm.reverseMap(slot) + "}"; }
			}
			""".formatted(className, superClass == null ? "WatObj0" : superClass.getSimpleName())
		;
		var classPath = "src/Wat/%s.java".formatted(className);
		Files.write(new File(classPath).toPath(), source.getBytes("cp1252"));
		ToolProvider.getSystemJavaCompiler().run(null, null, null, classPath, "-d", "bin", "--enable-preview", "-source", "19", "-Xlint:unchecked" );
		return Class.forName("Wat."+className);
	}
	
	static class WatObj {
		@SuppressWarnings("unused")
		private static Map<Symbol, Object> methods = new LinkedHashMap();
		<T extends WatObj> Object  getMethod(Symbol name) {
			 for(Class cls = this.getClass();;cls = cls.getSuperclass()) {
				try {
					Object res = ((Map<Symbol, Object>) cls.getDeclaredField("methods").get(cls)).get(name);
					if (res != null) return res;
				}
				catch (IllegalArgumentException | IllegalAccessException | NoSuchFieldException | SecurityException e) {
					return null;
				}	
			}
		}
		private Map<Keyword,Object> slot = new LinkedHashMap();
		WatObj(Vm.List list) {
	        for (var l=list; l!=null; l=l.cdr()) {
	        	var car = l.car; if (!(car instanceof Keyword key)) throw new RuntimeException("not a keyword: " + car + " in: " + list); 
	        	l = l.cdr(); if (l == null) throw new RuntimeException("a value expected in: " + list);
	        	slot.put(key, l.car);
	        }
		}
		@Override public String toString() { return "{WatObject" /*+ reverseMap(this)*/ + "}"; }
	}

	/*
	static <T extends Vm.WatClass> void newClass(String className, Class<T> watSuperclass) {
		/*
		String source = watSuperclass == null
			? """
			package Wat;
			class %1$s extends Vm.WatClass {
				Vm vm;
				public %1$s(Vm vm, Vm.WatClass watClass) { vm.super(watClass); this.vm = vm; }
				@Override public String toString() { return "{%1$s" + vm.reverseMap(map) + "}"; }
			}
			""".formatted(className)
			: """
			class %1$s extends %2$s {
				Vm vm;
				public %1$s(Vm vm, %2$s watClass) { super(vm, watClass); this.vm = vm; }
				@Override public String toString() { return "{%1$s" + vm.reverseMap(map) + "}"; }
			}
			""".formatted(className, watSuperclass.getSimpleName())
		;
		* /
		String source = """
			package Wat;
			class %1$s extends %2$s { 
				Vm vm; public %1$s(Vm vm, %2$s watClass) { %3$s; this.vm = vm; }
				@Override public String toString() { return "{%1$s" + vm.reverseMap(map) + "}"; }
			}
			""".formatted(className,
				watSuperclass == null ? "Vm.WatClass" : watSuperclass.getSimpleName(),
				watSuperclass == null ? "vm.super(watClass)" : "super(vm, watClass)"
			)
		;
	}
	*/
	
	/*
	//package Wat;
	//class Test extends Vm.Root { Test(Vm vm) { vm.super(); }}
	//class Test extends Vm.WatClass { Test(Vm vm) { vm.super(null); }}
	//class Test extends Vm.WatClass { Test(Vm vm, Vm.WatClass watClass ) { vm.super(watClass); }}
	//class Test<T extends Vm.WatClass> extends Vm.WatClass { Test(Vm vm, T watClass ) { vm.super(watClass); }}
	
	static void dynamicCompilation3(String name, String nome2) throws Exception {
		//out.println(name);
		String source = """
			package Wat;
			class %s extends %s {
				static {
					System.out.println("hello");
				}
				Vm vm;
				public %1$s(Vm vm, %2$s watClass) {
					vm.super(watClass);
					this.vm = vm;
					System.out.println("world");
				}
				@Override public String toString() { return "{%1$s" + vm.reverseMap(map) + "}"; }
			}
			""".formatted(name, nome2);
		out.println(source);
		var cname = "src/Wat/%s.java".formatted(name);
		//out.println(name);
		// Save source in .java file.
		Files.write(new File(cname).toPath(), source.getBytes("cp1252"));

		// Compile source file.
		ToolProvider.getSystemJavaCompiler().run(null, null, null, cname, "-d", "bin", "--enable-preview", "-source", "19", "-Xlint:unchecked" );

		// Load and instantiate compiled class.
		Class<?> cls = Class.forName("Wat."+name); // Should print "hello".
		//out.println(cls.getConstructors().length);
		//for (var c: cls.getConstructors()) out.println(c);
		Vm vm = new Vm();
		Vm.WatClass wc = vm.new WatClass(null);
		Object instance = cls.getConstructor(Vm.class, Vm.WatClass.class).newInstance(vm, wc); // Should print "world".
		System.out.println(instance); // Should print "Wat.Test@hashcode".
	}
	*/
	
	/*
	static void dynamicCompilation2() throws Exception {
		String source = """
			package Wat;
			class Test<T extends Vm.WatClass> extends Vm.WatClass<T> {
				static {
					System.out.println("hello");
				}
				public Test(Vm vm, T watClass) {
					vm.super(watClass);
					System.out.println("world");
				}
			}
			""";
		// Save source in .java file.
		var name = "src/Wat/Test.java";
		Files.write(new File(name).toPath(), source.getBytes("cp1252"));

		// Compile source file.
		JavaCompiler compiler = ToolProvider.getSystemJavaCompiler();
		compiler.run(null, null, null, name, "-d", "bin", "--enable-preview", "-source", "19", "-Xlint:unchecked" );

		// Load and instantiate compiled class.
		Class<?> cls = Class.forName("Wat.Test"); // Should print "hello".
		out.println(cls.getConstructors().length);
		for (var c: cls.getConstructors()) out.println(c);
		Vm vm = new Vm();
		Vm.WatClass wc = vm.new WatClass(null);
		Object instance = cls.getConstructor(Vm.class, Vm.WatClass.class).newInstance(vm, wc); // Should print "world".
		System.out.println(instance); // Should print "Wat.Test@hashcode".
	}
	*/

	static void dynamicCompilation() throws Exception {
		String source = """
			package Wat;
			class Test {			
				static {
					System.out.println("hello");
				}
				public Test() {
					super();
					System.out.println("world");
				}
			}
			""";
		// Save source in .java file.
		var name = "src/Wat/Test.java";
		Files.write(new File(name).toPath(), source.getBytes("cp1252"));

		// Compile source file.
		JavaCompiler compiler = ToolProvider.getSystemJavaCompiler();
		compiler.run(null, null, null, name, "-d", "bin", "--enable-preview", "-source", "19" );

		// Load and instantiate compiled class.
		Class<?> cls = Class.forName("Wat.Test"); // Should print "hello".
		//Object instance = cls.newInstance(); // Should print "world".
		Object instance = cls.getConstructor().newInstance(); // Should print "world".
		System.out.println(instance); // Should print "Wat.Test@hashcode".
	}

	static void parse() throws Exception {
		var s = """
		A
		|#
		  "#|"
		  ; #|
		#|
		  C
		|#
		  "D"
		#|
		E 
		""";
		//var is = new ByteArrayInputStream(s.getBytes());
		//while (is.available() > 0) out.println(Vm.read(is));
		out.println(Parser.toString(Parser.parse(s)));
		out.println("fatto");
	}
	
	static void executable() {
		out.println(Utility.getExecutable(Box.class, "new", Integer.class));
		//MethodUtils.getMatchingMethod(Box.class, null, null)
		//out.println(ConstructorUtils.getMatchingAccessibleConstructor(Box.class, null));
		for (var c: Box.class.getConstructors()) out.println(c);
		for (var c: Vm.DVar.class.getConstructors()) out.println(c);
		out.println();
	}
	
	static void function() {
		Function<List, Cons> f = l-> (Cons) l;
		out.println(f.getClass().getMethods());
		for (Method m: f.getClass().getMethods()) out.println(m);
	}

	interface List {}
	static class Nil implements List {}
	static Nil nil = new Nil();
	static class Lons extends Cons implements List {
		Lons (Object car, List cdr) { super(car, cdr); }
	}
	static class Cons {
		Object car, cdr;
		Cons(Object car, Object cdr) { this.car=car; this.cdr=cdr;}
	}
	static <T> T cons(Object car, Object cdr) {
		return (T)(cdr instanceof Nil nil ? new Lons(car, nil) : cdr instanceof Lons list ? new Lons(car, list) : new Cons(car, cdr));
	}
	@SuppressWarnings("preview")
	Cons cons2(Object car, Object cdr) {
		return switch (cdr) {
			default-> new Cons(car, cdr); 
			case Nil nil-> new Lons(car, nil);
			case Lons lons-> new Lons(car, lons);
		};
	}
	
	static void list() {
		Cons x = cons(1, cons( 2, nil));
		System.out.println(x instanceof Lons);
		System.out.println(x instanceof List);
		System.out.println(x instanceof Cons);
		List y = cons(1, cons(2, 3));
		System.out.println(y instanceof Lons);
		System.out.println(y instanceof List);
		System.out.println(y instanceof Cons);
		out.println("finito");
	}
	
	static void camelize() throws IOException, FileNotFoundException {
		for (var s: $("testVm", "boot", "test", "testJni")) {
			try (var pw = new PrintWriter(s + ".lsp")) {
				pw.print(replace(readString(get(s + ".wat"), forName("cp1252"))));
			}
		}
	}
	static String replace(String s) {
		return compile("-\\w").matcher(s).replaceAll(mr-> mr.group().substring(1).toUpperCase());
	}
	
	enum Binop {
		Pls((a, b)-> a+b,   (a, b)-> a+b,   (a, b)-> a+b),
		Mns((a, b)-> a-b,   (a, b)-> a-b,   (a, b)-> a-b),
		Pwr((a, b)-> a*b,   (a, b)-> a*b,   (a, b)-> a*b),
		Dvd((a, b)-> a/b,   (a, b)-> a/b,   (a, b)-> a/b),
		Rst((a, b)-> a%b,   (a, b)-> a%b,   (a, b)-> a%b),
		 Ls((a, b)-> a<b,   (a, b)-> a<b,   (a, b)-> a<b),
		 Gt((a, b)-> a>b,   (a, b)-> a>b,   (a, b)-> a>b),
		 Le((a, b)-> a<=b,  (a, b)-> a<=b,  (a, b)-> a<=b),
		 Ge((a, b)-> a>=b,  (a, b)-> a>=b,  (a, b)-> a>=b),
		 Sl((a, b)-> a<<b,  (a, b)-> a<<b,  null),
		 Sr((a, b)-> a>>b,  (a, b)-> a>>b,  null),
		Sr0((a, b)-> a>>>b, (a, b)-> a>>>b, null);
		
		BiFunction<Integer, Integer, Object> i;
		BiFunction<Double, Double, Object> d;
		BiFunction<Long, Long, Object> l;
		Binop(BiFunction<Integer, Integer, Object> i, BiFunction<Long, Long, Object> l, BiFunction<Double, Double, Object> d) {
			this.i = i; this.l = l; this.d = d;		
		};
	}
	static BiFunction<Number, Number, Object> getNumOp(Binop op) {
		return (a, b)-> a instanceof Integer i1 && b instanceof Integer i2 ? op.i.apply(i1, i2) : op.d.apply(a.doubleValue(), b.doubleValue());
	}	
	static void binop() {
		out.println(1 * 2.1);
		var plus = getNumOp(Binop.Pwr);
		out.println(plus.apply(2.1, 3));
		var lesser = getNumOp(Binop.Le);
		out.println(lesser.apply(1.0, 2));
	}
	
	
	static void varArgs() {
		args(Integer.class, int.class); // 2
		args((Object) new Class [] {Integer.class, int.class}); // 1
		args((Object[]) new Class [] {Integer.class, int.class}); // 2
		args((Object) $(Integer.class, int.class)); // 1
		args((Object[]) $(Integer.class, int.class)); // 2
		args($(Integer.class, int.class, (Object) $(Integer.class, int.class), ((Function) x->x))); // 3
	}
	
	static <T> T[] $(T... objects) {
		return objects;
	}	
	static void args(Object ... args) {
		out.println(args.length);
		
		for (Object o: args) {
			var cl = o.getClass();
			out.println(
				cl.getName() + "|" + cl.getSimpleName() + "|" + cl.getCanonicalName() + "|" + cl.getPackageName() + "|" + cl.getTypeName());  
		}
	}
	
	static void mapReversed() {
		Map<String,Object> map = new LinkedHashMap();
		map.put("z", 1);
		map.put("y", 2);
		map.put("a", 3);
		map.put("k", 3);
		out.println(map);
		out.println(reverse(map));
		out.println(reverse2(map));
	}
	static Map reverse(Map<String,Object> map) {
		Map<String,Object> nMap = new LinkedHashMap();
		var list = map.entrySet().stream().collect(Collectors.toList());
		Collections.reverse(list);
		for (Entry<String,Object> e: list) nMap.put(e.getKey(),e.getValue());
		return nMap;
	}
	static Map reverse2(Map<String,Object> map) {
		Map<String,Object> nMap = new LinkedHashMap();
		var list = new ArrayList<String>(map.keySet());
		Collections.reverse(list);
		for (String k: list) nMap.put(k ,map.get(k));
		return nMap;
	}

	static void escape() {
		var s = "\"\n\t\r\b\f"; 
		var m = of("\"", "\\\\\"", "\n", "\\\\n", "\t", "\\\\t", "\r", "\\\\r", "\b", "\\\\b", "\f", "\\\\f");
		for (Entry<String,String> e: m.entrySet()) s = s.replaceAll(e.getKey(), e.getValue());
		out.println("\"" + s + "\"");
	}

	static void array() {
		var I = new Integer[] {1,2,3};
		out.println(I.length);
		out.println(I.getClass());
		Arrays.stream(I).mapToInt(ii->ii).toArray();
		
		var i = new int[] {1,2,3};
		Object o = i;
		out.println(i.getClass());
		out.println(((int[])o).length);
	}

	static void constructor() throws Exception {
		//out.println(Utility.classForName("int"));
		//out.println(Class.class.getClass());
		//out.println();
		
		out.println(Prova.Box.class.getConstructor());
		out.println(Prova.Box.class.getConstructor(int.class));
		
		// facendolo diretto è semplice
		out.println(Integer.class.getConstructor(String.class));
		out.println();

		// ed usarlo pure
		out.println(Integer.class.getConstructor(String.class).newInstance("1"));
		out.println();

		// se provo a prendere invece tutti i costruttori della classe Integer via Class ancora ci si riesce
		var classGetConstructors = Class.class.getMethod("getConstructors");
		var integerConstructors = (Constructor[]) classGetConstructors.invoke(Integer.class);
		for (var c: integerConstructors) out.println(c);
		out.println();
		
		// posso anche andare avanti e scegliendo quello desisderato a mano
		var stringIntegerConstructor = integerConstructors[0];
		out.println(stringIntegerConstructor/*.getClass().getSimpleName()*/);
		out.println();
		
		// e creare la nuova istanza 
		var obj = stringIntegerConstructor.newInstance("1");
		out.println(obj);
		out.println();
		
		// fino a qui tutto bene ma va fatto avendo a disposizione
		// la classe Integer, la String "getConstructor" e la class String

		// se provo a recuperarlo da Integer becco un NoSuchMethodException java.lang.Integer.getConstructor([Ljava.lang.Class;)
		try { Integer.class.getMethod("getConstructor", Class[].class); } catch (Exception e) { e.printStackTrace(out); }
		out.println();
		
		// invece se provo a recuperare il metodo su Class funziona
		var classGetConstructor = Class.class.getMethod("getConstructor", Class[].class);
		out.println(classGetConstructor/*.getClass().getSimpleName()*/);
		out.println();

		// a questo punto sapendo che è un method devo invocarlo passandogli i parametri giusti ma qui sta il problema
		// devo recuperare il metodo getConstructor di Integer che prende come argomenti un Class... ovvero un Class[]
		
		// però se provo quanto immagino becco un IllegalArgumentException argument type mismatch
		try { classGetConstructor.invoke(Integer.class, Class[].class); } catch (Exception e) { e.printStackTrace(out); }
		out.println();
		
		// ma è la stessa cosa se passo direttamente la classe String
		try { classGetConstructor.invoke(Integer.class, String.class); } catch (Exception e) { e.printStackTrace(out); }
		out.println();

		// se lo chiamo non passando parametri o un Class[] vuoto prendo un IllegalArgumentException wrong number of arguments: 0 expected: 1
		try { classGetConstructor.invoke(Integer.class); } catch (Exception e) { e.printStackTrace(out); }
		out.println();
		
		try { classGetConstructor.invoke(Integer.class, new Object[] {}); } catch (Exception e) { e.printStackTrace(out); }
		out.println();

		// sembrerebbe un piccolo pertugio dove infilarsi ma se provo ad aggiungere altro becco invariabilmente un IllegalArgumentException argument type mismatch
		try { classGetConstructor.invoke(Integer.class, new Object[] { Class.class }); } catch (Exception e) { e.printStackTrace(out); }
		out.println();
		
		try { classGetConstructor.invoke(Integer.class, new Object[] { Class[].class }); } catch (Exception e) { e.printStackTrace(out); }
		out.println();

		try { classGetConstructor.invoke(Integer.class, new Object[] { String.class} ); } catch (Exception e) { e.printStackTrace(out); }
		out.println();

		// a questo punto non so più cosa pensare e soprattutto come riuscire!

		//var integerGetConstructor = classGetConstructor.invoke(new Object[] {Integer.class, String.class} ); // object is not an instance of declaring class
		//out.println(integerGetConstructor.getClass().getSimpleName());
		//out.println();

		//getConstructor.invoke(Integer.class, java.lang.String.class);
	}

	static void method() throws Exception {
		
		out.println(java.lang.Integer.class.getMethod("toOctalString", int.class));
		out.println();
		
		var classGetMethods = java.lang.Class.class.getMethod("getMethods");
		for (var c: (Object[]) classGetMethods.invoke(java.lang.Integer.class)) out.println(c);
		out.println();
		
		//
		
		var classGetMethod = java.lang.Class.class.getMethod("getMethod", java.lang.String.class, java.lang.Class[].class);
		out.println(classGetMethod);
		out.println();

		var toOctalString = (Method) classGetMethod.invoke(java.lang.Integer.class, "toOctalString", new java.lang.Class[] { int.class } );
		//  longValue = getMethod.invoke(java.lang.Integer.class, "longValue", null); va anche bene
		out.println(toOctalString);
		out.println();
		
		var value = toOctalString.invoke(null, 10);
		out.println(value/*.getClass().getSimpleName()*/);
		out.println();
	}

	static void dynamicClass() throws Exception {
		String source = """
			package test;
			public class Test {
				static {
					System.out.println("hello");
				}
				public Test() {
					System.out.println("world");
				}
			}
			""";

		// Save source in .java file.
		File root = new File("/java"); // On Windows running on C:\, this is C:\java.
		File sourceFile = new File(root, "test/Test.java");
		sourceFile.getParentFile().mkdirs();
		Files.write(sourceFile.toPath(), source.getBytes("UTF-8"));

		// Compile source file.
		JavaCompiler compiler = ToolProvider.getSystemJavaCompiler();
		compiler.run(null, null, null, sourceFile.getPath());

		// Load and instantiate compiled class.
		URLClassLoader classLoader = URLClassLoader.newInstance(new URL[] { root.toURI().toURL() });
		Class<?> cls = Class.forName("test.Test", true, classLoader); // Should print "hello".
		//Object instance = cls.newInstance(); // Should print "world".
		Object instance = cls.getConstructor().newInstance(); // Should print "world".
		System.out.println(instance); // Should print "test.Test@hashcode".
	}

	static Object $_(Object... objects) {
		return objects;
	}	
	static void prova$() {
		var list = new ArrayList();
		list.add(1);
		list.add(2);
		list.add(3);
		System.out.println(list);
		list.toArray();
		String id = " ";
		Object o = id.length() == 1 ? id : switch (id.charAt(0)) {
			case '.' -> $_("jsGetSet", $_("wat-string", id.substring(1)));
			case '@' -> $_("jsInvoker", $_("wat-string", id.substring(1)));
			default -> id;
		};
		System.out.println(o);
	}
	
	public static class Box {
		public Object i;
		public void set(int i) {
			this.i = i;
		}
		public void setn(int[] i) {
			this.i = i[i.length-1];
		}
		public int get() {
			return (Integer) i;
		}
		public Box() {
		}
		public Box(int i) {
			this.i = i;
		}
		public Box(Object o) {
			this.i = o;
		}
		@Override
		public String toString() {
			return "{Box " + i + "}";
		}
	}
}
