package Wat;

import static Wat.Utility.member;
import static java.lang.Character.isISOControl;
import static java.lang.Integer.parseInt;
import static java.lang.Integer.toHexString;
import static java.lang.Runtime.getRuntime;
import static java.lang.System.getProperty;
import static java.lang.System.in;
import static java.lang.System.out;
import static java.nio.charset.Charset.forName;
import static java.nio.file.Files.readString;
import static java.nio.file.Paths.get;
import static java.util.Map.of;
import static java.util.regex.Pattern.compile;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.net.URI;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.charset.Charset;
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
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import javax.tools.DiagnosticCollector;
import javax.tools.JavaCompiler;
import javax.tools.JavaFileObject;
import javax.tools.SimpleJavaFileObject;
import javax.tools.ToolProvider;

import List.Parser;
import Wat.Vm.Keyword;
import Wat.Vm.Symbol;

//import jdk.internal.vm.Continuation;
//import jdk.internal.vm.ContinuationScope;

public class Prova {
	
	public static void main(String[] args) throws Exception {
		out.println(1 * 1.2);
		out.println(1.2 * 1);
	}
	
	public static void functions() {
		System.out.println(Arrays.stream(new Object[] {1, 2, 3}).map((n)->"_"+n).toList());
		// (@toList (@map (@stream &java.util.Arrays (array 1 2 3)) (%tof (\ (n) ($ "_" n)))))
		System.out.println(Arrays.stream(new Object[] {1, 2, 3}).map(toFunction("_")).toList());
		System.out.println(Arrays.stream(new Object[] {1, 2, 3}).map(new ToF().toFunction("_")).toList());
		System.out.println(Arrays.stream(new Object[] {1, 2, 3}).map(new Prova().new ToF2().toFunction("_")).toList());
		
		Map.of("a",1,"b",2).forEach((k, v)->{ out.println(k + ":" + v); });
	}
	
	public static final <T> T toFunction(String s) {
		return (T) new Function() {
			@Override public final Object apply(Object t) {
				return s + t;
			}
		};
	}
	
	public static final class ToF {
		public final <T> T toFunction(String s) {
			return (T) new Function() {
				@Override public final Object apply(Object t) {
					return s + t;
				}
			};
		}
	}
	
	public final class ToF2 {
		public final <T> T toFunction(String s) {
			return (T) new Function() {
				@Override public final Object apply(Object t) {
					return s + t;
				}
			};
		}
	}
	
	public static void stdCtrl() throws IOException {
		out.println(Utility.toSource("a\nb"));
		out.println(Utility.toString("a\\nb"));
		out.println(Utility.toSource(Utility.toString("a\\nb")));
		out.println(Utility.toString(Utility.toSource("a\nb")));
		out.println(Charset.defaultCharset());
		out.println(System.getProperty("stdout.encoding"));
		var isr = new InputStreamReader(in, forName("UTF-8"));
		for(;;) { var v = Utility.read(0, isr); if (v.equals("")) break; out.print(v); }
		
		var str = "aa\\n\\\\rx\\rc\\c\\td\\\"f";
		out.println(str);
		
		// toString
		var pat1 = compile("\\\\[\"nrtbf\\\\]");
		var map1 = of("\\\"", "\"", "\\n","\n", "\\r","\r", "\\t","\t", "\\b","\b", "\\f","\f", "\\\\", "\\\\\\\\");
		var res = pat1.matcher(str).replaceAll(mr-> map1.get(mr.group()));
		out.println(res);
		
		//toSource
		var pat2 = compile("[\"\n\r\t\b\f\\\\]");
		var map2 = of("\"","\"", "\n","n", "\r","r", "\t","t", "\b","b", "\f","f", "\\","");
		var org = pat2.matcher(res).replaceAll(mr-> "\\\\" + map2.get(mr.group()));
		out.println(org);
	}

	public static void altMcomment() throws InterruptedException, IOException {
		//*
		process1();
		/*/
		process1();
		//*/
	}
	public static void process1() throws InterruptedException, IOException {
		//out.println(getRuntime().exec("cmd.exe /e:on /c dir").waitFor());
		out.println(getRuntime().exec(new String[] {"cmd.exe", "/e:on", "/c", "dir"}).waitFor());
	}
	
    public static void main2(String[] args) throws Exception {
        ProcessBuilder builder = new ProcessBuilder("cmd.exe", "/e:on", "/c", "dir");
        builder.redirectErrorStream(true);
        Process p = builder.start();
        BufferedReader r = new BufferedReader(new InputStreamReader(p.getInputStream()));
        for (String line=null; (line = r.readLine()) != null; ) System.out.println(line);
        out.println(p.exitValue());
    }

	public static void charcters() {
		{
			// to source
			@SuppressWarnings("unused")
			var names = false;
			var p = Pattern.compile("[\\x00-\\x1f\\x7f-\\x9f]");
			var sb = new StringBuffer();
			var m = p.matcher("\u0015\u007f\u009f");
			while (m.find()) {
				var c = m.group().charAt(0);
				m.appendReplacement(sb, !isISOControl(c) ? "" + c : "\\\\x" + toHexString(c) + ";");  
				//m.appendReplacement(sb, !isISOControl(c) ? "" + c : "\\\\x" + apply(n-> (n != null && names ? n : toHexString(c)), getName(c)) + ";" );  
			}
			m.appendTail(sb);
			out.println(sb);
		}
		{
			// to String
			var p = Pattern.compile("\\\\x([0-9a-bA-b]{1,4});");
			var sb = new StringBuffer();
			var m = p.matcher("a\\x3bb;b\\x9;c");
			while (m.find()) m.appendReplacement(sb, "" + (char) parseInt(m.group(1), 16));
			m.appendTail(sb);
			out.println(sb);
		}
	}

	public static void stringhe() {
		out.println("\u0028");
		out.println((char) Integer.parseInt("#\\x0040".substring(3),16));
		out.println(Arrays.stream(";._".split("")).toList());
		out.println(Arrays.stream("cc.1".splitWithDelimiters(":|!|,|'|\\.|&", 0)).toList());
	}

	public static void utf8() throws IOException {
		out.println(Charset.defaultCharset());
		out.println(getProperty("stdout.encoding"));
		var isr = new InputStreamReader(in, Charset.forName("UTF-8"));
		loop: for (;;) {
			if (!isr.ready()) out.print("> ");
			for (int c; (c = isr.read()) != '\n';) {
				if (c == -1) break loop;
				out.print((char) c);
			}
		}
		out.print("\nfinito");
	}

	public static void utf82() throws IOException {
		// non funziona
		out.println(Charset.defaultCharset());
		out.println(getProperty("stdout.encoding"));
		var isr = new InputStreamReader(in, Charset.forName("UTF-8"));
		var osw = new OutputStreamWriter(out, Charset.forName("UTF-8"));
		loop: for (;;) {
			if (!isr.ready()) osw.write("> ");
			for (int c; (c = isr.read()) != '\n';) {
				if (c == -1) break loop;
				osw.write((char) c);
			}
		}
		osw.write("\nfinito");
	}

	public static void readPrint() throws IOException, FileNotFoundException {
		try (var x = new BufferedReader(new FileReader("testFile.txt")); ) {
			for (String l=null; (l = x.readLine()) != null; ) {
				out.println(l);
			}
		}
	}
	
	//private BiFunction<String,Integer, Double> xx =  null;
	
	public static void replaceAll() {
		//out.println(Pattern.compile("\\|").matcher("\\|").quoteReplacement("|"));
		//out.println("\\|".replaceAll("\\|", Matcher.quoteReplacement("__")));
		//out.println("\\|".replaceAll("\\\\|", "!"));
		out.println("\\|".replaceAll("\\\\\\|", "|"));
		out.println("\\\\".replaceAll("\\\\\\\\", "\\\\"));
		out.println("\\(".replaceAll("\\\\(.)", "$1"));
		out.println("\\|".replaceAll("\\\\(.)", "$1"));
	}

	public static void check() {
		//Object[][] a = { $(1, 2, Integer.class, String.class),  $(1, 2, Integer.class, String.class) };
		Object[] b = $(1, 2, Integer.class, String.class);
		Arrays.copyOfRange(b, 1, b.length);
	}

	static void trueFalse() {
		out.println(Boolean.TRUE == true);
		out.println(Boolean.FALSE == true);
		//out.println(new Object() == true);
		//Object obj = null; out.println(obj == true);
		out.println(member(0, null, false, 0));
	} 

	//class $ {}

	//record Key(Object ... a) {
	static class Key { Object[] a; Key(Object ... a) { this.a = a; }
		@Override public boolean equals(Object obj) { return obj instanceof Object[] b && Arrays.deepEquals(a, b); }
		@Override public int hashCode() { return Arrays.deepHashCode(a); }
	}
	
	public static void classi() {
		classi(true, (Object) Cls3.class);
		classi(false, new Cls3());
		classi(false, Cls3 .class);
	}
	
	static void classi(boolean constructors, Object obj) {
		Class <?> classe = constructors ? (Class) obj : obj.getClass();
		do {
			out.print(classe.getSimpleName() + " ");
		}
		while (!constructors && (classe = classe.getSuperclass()) != null);
		if (!constructors && obj instanceof Class) {
			out.print("| ");
			classe = (Class) obj;
			do {
				out.print(classe.getSimpleName() + " ");
			} while ((classe = classe.getSuperclass()) != Object.class );
		};
		out.println();
	}
	

	static void version() {
		System.out.println(Runtime.version());
		System.out.println(Runtime.version().feature());
		System.out.println(Runtime.version().interim());
		System.out.println(Runtime.version().update());
		System.out.println(Runtime.version().patch());
		System.out.println(Runtime.version().build());
		System.out.println(Runtime.version().optional());
	}

	static class A {
		int val;
		A(int val) {this.val = val; }
	}
	static class B extends A {
		B(int val) { super(val); }
	}
	
	public static void anotherError() {
		Object o = new B(2);
		switch (o) {
			case null /*, default*/-> out.println(0); // darebbe 0
			case A a when a.val == 1-> out.println(1); 
			case B b when b.val == 2-> out.println(2);
			case A a-> out.println(3);
			default-> out.println(4); // con default qui da 3 ma mai 2
		};
	}

	public static void key() {
		var map = new HashMap<Key,Object>();
		out.println(new Key(1, " ", true).equals(new Key(1, " ", true)));
		map.put(new Key(1, " ", true), 2);
		out.println(map.containsKey(new Key(1, " ", true)));
	}

	public static void box() throws NoSuchMethodException {
		//extend2("Ooo", null);
		Cls.class.getConstructor((Class[]) null);
		Cls.class.getConstructor((Class[]) null);
		out.println(Cls.class); // -> Wat.Prova$Cls
		out.println(Cls.class.getSuperclass()); // -> Object
		out.println(Cls.class.getClass()); // -> Class
		out.println(Cls.class.getClass().getClass()); // -> Class
		out.println(Cls.class.getClass().getSuperclass()); // -> Object
		out.println(Cls.class.getClass().getSuperclass().getClass()); // -> Class
		out.println(Cls.class.getClass().getSuperclass().getClass().getClass()); // -> Class
		new Cls(1).get();
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
		    	new JavaStringFile("Ext.Obj2", """
					package Ext;
					import Wat.Vm;
					public class Obj2 extends Vm.Obj {
						public Obj2(Vm vm, Vm.List l) { vm.super(l); }
					}
					"""
				)
	    	)
	    );
	    if (task.call()) {
	    	var cls = Class.forName("Ext.Obj2");
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
					""".formatted(className, superClass == null ? "Vm.Obj" : superClass.getCanonicalName())
				)*/
		    	new JavaStringFile("Wat.Vm$Obj2", """
					package Wat;
					import Wat.Vm;
					class Obj2 extends Vm.Obj {
						private static final long serialVersionUID = 1L;
						public Obj2(Vm.List l) { super(Vm.this, l); }
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
		@SuppressWarnings("unused")
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
		out.println(Parser.toString(Parser.toByteCode(s)));
		out.println("fatto");
	}
	
	static void executable() {
		out.println(Utility.getExecutable(Cls.class, "new", Integer.class));
		//MethodUtils.getMatchingMethod(Cls.class, null, null)
		//out.println(ConstructorUtils.getMatchingAccessibleConstructor(Cls.class, null));
		for (var c: Cls.class.getConstructors()) out.println(c);
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
	Cons cons2(Object car, Object cdr) {
		return switch (cdr) {
			case Nil nil-> new Lons(car, nil);
			case Lons lons-> new Lons(car, lons);
			default-> new Cons(car, cdr); 
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
		
		out.println(Prova.Cls.class.getConstructor());
		out.println(Prova.Cls.class.getConstructor(int.class));
		
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
	
	public static class Cls {
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
		public Cls() {
		}
		public Cls(int i) {
			this.i = i;
		}
		public Cls(Object o) {
			this.i = o;
		}
		@Override
		public String toString() {
			return "{&Wat.Prova$Cls " + i + "}";
		}
	}
	public static class Cls2 extends Cls {}
	public static class Cls3 extends Cls2 {}
}
