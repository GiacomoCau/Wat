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
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.stream.Collectors;

import javax.tools.JavaCompiler;
import javax.tools.ToolProvider;

import List.Parser;

public class Prova {
	
	//class $ {}

	public static void main(String[] args) throws Exception {
		dynamicCompilation();
	}

	static void dynamicCompilation() throws Exception {
		String source = """
			package Wat;
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
		var name = "src/Wat/Test.java";
		Files.write(new File(name).toPath(), source.getBytes("cp1252"));

		// Compile source file.
		JavaCompiler compiler = ToolProvider.getSystemJavaCompiler();
		compiler.run(null, null, null, name, "-d", "bin");

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
