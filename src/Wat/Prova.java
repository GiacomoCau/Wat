package Wat;

import static java.lang.System.out;

import java.io.File;
import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.Arrays;

import javax.tools.JavaCompiler;
import javax.tools.ToolProvider;

public class Prova {
	
	static Object $_(Object... objects) {
		return objects;
	}
	
	class $ {}
	
	public static void main(String[] args) throws Exception {
		var i = new Integer[] {1,2,3};
		Arrays.stream(i).mapToInt(ii->ii).toArray();
	}

	private static void constructor() throws Exception {
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

	private static void method() throws Exception {
		
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

	private static void dinamicClass() throws Exception {
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

	private static void prova$() {
		var list = new ArrayList();
		list.add(1);
		list.add(2);
		list.add(3);
		System.out.println(list);
		list.toArray();
		String id = " ";
		Object o = id.length() == 1 ? id : switch (id.charAt(0)) {
			case '.' -> $_("js-getter", $_("wat-string", id.substring(1)));
			case '@' -> $_("js-invoker", $_("wat-string", id.substring(1)));
			default -> id;
		};
	}
	
	public static class Box {
		public int i;
		public void set(int i) {
			this.i = i;
		}
		public int get() {
			return i;
		}
		public Box() {
		}
		public Box(int i) {
			this.i = i;
		}
	}
}
