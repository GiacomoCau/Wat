package Wat;

import java.io.File;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.lang.reflect.InvocationTargetException;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.file.Files;
import java.util.ArrayList;

import javax.tools.JavaCompiler;
import javax.tools.ToolProvider;

public class Prova {
	
	static Object $_(Object... objects) {
		return objects;
	}
	
	class $ {}
	
	public static void main(String[] args) throws Exception {
	}

	private static void dinamicClass() throws IOException, UnsupportedEncodingException, MalformedURLException, ClassNotFoundException,
	InstantiationException, IllegalAccessException, InvocationTargetException, NoSuchMethodException {
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
	
	
	// Prepare source somehow.
	
}
