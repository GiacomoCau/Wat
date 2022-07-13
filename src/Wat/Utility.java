package Wat;

import static java.lang.System.out;

import java.lang.reflect.Array;
import java.lang.reflect.Executable;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.List;

public class Utility {
	
	public static Object[] $(Object... objects) {
		return objects;
	}
	
	public static List toList (Object... objects) {
		return Arrays.stream(objects).toList();
	}
	
	public static boolean isInstance(Object o, Class ... cs) {
		for (Class c: cs) if (c.isInstance(o)) return true;
		return false;
	}
	
	private enum Primitive {
			byte$("B", byte.class, Byte.class),
			short$("S", short.class, Short.class),
			int$("I", int.class, Integer.class),
			long$("J", long.class, Long.class),
			float$("F", float.class, Float.class),
			double$("D", double.class, Double.class),
			boolean$("Z", boolean.class, Boolean.class),
			void$("V", void.class, Void.class)
		;
		public final String type;
		public final Class classe;
		@SuppressWarnings("unused")
		public final Class wrapper;
		public final String name;
		Primitive(String type, Class classe, Class wrapper) {
			this.type = type;
			this.classe = classe;
			this.wrapper = wrapper;
			this.name = classe.getSimpleName();
		}
	}

	public static Class <?> classForName(String name) {
		if (name == null) return null;
		// if (name.equals("")) return null; // va aggiunto?
		// if (name.equals("null")) return null; // va aggiunto?
		boolean L = true;
		for (Primitive p: Primitive.values()) {
			if (!name.startsWith(p.name)) continue;
			if (name.length() == p.name.length()) return p.classe;
			name = p.type + name.substring(p.name.length());
			L = false;
			break;
		}
		L &= name.endsWith("[]");
		if (L) name = "L" + name;
		while (name.endsWith("[]"))	name = "[" + name.substring(0, name.length()-2);
		if (L) name += ";";
		try {
			return Class.forName(name);
		}
		catch (ClassNotFoundException e) {
			return null;
		}
	}
	
	public static Object[] reorg(Class[] classi, Object[] args) throws Exception {
		int length = classi.length;
		/* TODO verificare l'ottimizzazione, eliminare altrimenti
		if (length == 1 && !classi[0].isPrimitive()) return args;
		*/
		Object[] newArgs = Arrays.copyOf(args, length); // eventualmente null in più!
		if (args.length < length) return newArgs;
		Class componentType = classi[length-=1].getComponentType();
		if (!componentType.isPrimitive()) {
			//newArgs[length] = Arrays.copyOfRange(args, length, args.length, (Class) Array.newInstance(componentType, 0).getClass()); // ok
			newArgs[length] = Arrays.copyOfRange(args, length, args.length, getClassArray(componentType)); // ok
		}
		else {
			Object last = Array.newInstance(componentType, args.length-length);
			for (int i=length; i<args.length; i+=1) Array.set(last, i-length, args[i]);
			newArgs[length] = last;
		}
		return newArgs;
	}
	
	public static Class getClassArray(Class c) {
	    return Array.newInstance(c, 0).getClass();
	    /* TODO in alternativa al precedente, eliminare altrimenti
		return getClassArray(c, 1);
		*/
	}
	
	public static Class getClassArray(Class c, int n) {
	    return Array.newInstance(c, new int[n]).getClass();
	}

	static boolean publicMember = false;
		
	public static Field getField(Class <?> classe, String name) {
		return getField(classe, publicMember, name);
	}
	public static Field getField(Class <?> classe, boolean onlyPublicMember, String name) {
		do {
			try {
				Field f = onlyPublicMember
					? classe.getField(name)
					: classe.getDeclaredField(name)
				;
				return f;
			}
			catch (Exception e) {
				if (onlyPublicMember) return null;
			}
		} while ((classe = classe.getSuperclass()) != null);
		return null;
	}
	
	public static void main(String[] args) throws Exception {
		//Class c = Integer.class;
		//out.println(getField(Integer.class, "MAX_VALUE"));
		//out.println();
		
		//out.println(getExecutable((Object) Integer.class, "equals", new Object[] { Object.class }));
		//out.println();
		//out.println(getExecutable((Object) Prova.Box.class, "getMethod", new Object[] { "get" }));
		//out.println();
		//out.println(getExecutable((Object) Prova.Box.class, "getMethod", new Object[] { "set", 1 }));
		//out.println();
		
		out.println(getExecutable(Prova.Box.class, "getMethod", new Class[] { String.class, Class[].class } ));
		out.println();
		out.println(getExecutable(Prova.Box.class, "getConstructor", new Class[] { Prova.Box.class, Class[].class } ));
		out.println();
		
		Method m = (Method) getExecutable(Prova.Box.class, "getConstructor", Class[].class );
		out.println(m);
		out.println();
		
		out.println(m.invoke((Object) Prova.Box.class, int.class));
		//Constructor c2 = (Constructor) m.invoke((Object) Class.class , int.class);
		
		//out.println(getExecutable((Object) Class.class, "getMethod", new Object[] { String.class, Class[].class } ));
		//out.println(getExecutable((Object) Integer.class, "invoke", new Object[] { String.class }));
		//out.println(getExecutable((Object) Integer.class, "new", new Object[] { String.class }));
		//out.println(getExecutable((Object) Integer.class, "newInstance", new Object[] { String.class }));
	}
	
	public static Executable getExecutable(Object obj, String name, Object ... args) {
		return getExecutable(obj.getClass(), name, getClasses(args));
	}
	
	public static Class[] getClasses(Object ... objects) {
		return Arrays.stream(objects).map(o-> o == null ? null : o.getClass()).toArray(Class[]::new);
	}

	public static <T extends Executable> T getExecutable(Class <?> classe, String name) {
		return getExecutable(classe, name, (Class[]) null);
	}
	
	public static <T extends Executable> T getExecutable(Class <?> classe, String name, Class <?> ... classes) {
		return getExecutable(classe, publicMember, name, classes);
	}
	
	private static boolean trace = false; 
	
	public static <T extends Executable> T getExecutable(Class <?> classe, boolean publicMember, String name, Class <?> ... argumentsClass) {
		if (trace) out.println("%s %s(%s)".formatted(classe, name, Arrays.stream(argumentsClass).toList()));
		boolean constructor = name.equals("new");
		Executable r = null;
		do {
			if (trace) out.println("\tc: " + classe);
			try {
				var t = (T) getExecutor(classe, constructor, publicMember, name, argumentsClass);
				if (trace) out.println("\t\t1: " + t);
				return t;
			}
			catch (NoSuchMethodException nsme) {
				for (Executable executor: getExecutors(classe, constructor, publicMember)) {
					if (!constructor && !executor.getName().equals(name)) continue;
					if (trace) out.println("\t\tn: " + executor);
					if (argumentsClass == null) return (T) executor; // si va per nome, non si possono controllare gli argomenti
					boolean isVarArgs = executor.isVarArgs();
					Class [] parametersClass = executor.getParameterTypes();
					if (!isVarArgs && argumentsClass.length != parametersClass.length || argumentsClass.length < parametersClass.length - 1) continue;
					if (!isInvokeConvertible(isVarArgs, parametersClass, argumentsClass)) continue;
					/* TODO sostituito dal seguente, eliminare appena verificato
					return (T) e; // torna il primo buono 
					*/
					if (r != null && executor.isVarArgs() && executor.getParameterCount() >= r.getParameterCount()) continue;
					r = executor; // torna il primo buono con meno varargs 
				}
				if (publicMember) break;
			}
		} while ((classe = classe.getSuperclass()) != null);
		// funziona ma sono valutare le implicazioni
		//} while (classe != Class.class && (classe = classe.getSuperclass() == null ? Class.class: classe.getSuperclass()) != null);
		return (T) r;
	}

	private static Executable getExecutor(Class<?> classe, boolean constructor, boolean publicMember, String name, Class<?>... classes)
	throws NoSuchMethodException {
		return constructor
			? (publicMember ? classe.getConstructor(classes) : classe.getDeclaredConstructor(classes))
			: (publicMember ? classe.getMethod(name, classes) : classe.getDeclaredMethod(name, classes));
	}
	
	private static Executable[] getExecutors(Class<?> classe, boolean constructor, boolean publicMember) {
		return constructor
			? (publicMember ? classe.getConstructors() : classe.getDeclaredConstructors())
			: (publicMember ? classe.getMethods() : classe.getDeclaredMethods());
	}

	private static boolean isInvokeConvertible(boolean varArg, Class[] parameters, Class[] arguments) {
		if (!varArg) return isInvokeConvertible(parameters, arguments);
		int i=0;
		for (int e=parameters.length-1; i<e; i+=1) {
			if (!isInvokeConvertible(parameters[i], arguments[i])) return false;
		}
		if (parameters.length == arguments.length && isAssignableFrom(parameters[i], arguments[i])) return true;
		Class componentType = parameters[i].getComponentType();
		for (int e=arguments.length; i<e; i+=1) {
			if (!isInvokeConvertible(componentType, arguments[i])) return false;
		}
		return true;
	}
	private static boolean isInvokeConvertible(Class[] ps, Class[] as) {
		for (int i=0; i<as.length; i+=1) {
			if (!isInvokeConvertible(ps[i], as[i])) return false;
		}
		return true;
	}
	
	private static boolean isInvokeConvertible(Class p, Class a) {
		return isAssignableFrom(p, a) || isWPConvertible(p, a);
	}
	
	// Identity and Wideninig Reference Conversion
	private static boolean isAssignableFrom(Class t, Class f) {
		/* TODO potrebbe essere utile, eliminare altrimenti
		if (t == null) return false;
		*/
		if (f == null) return !t.isPrimitive();
		/* TODO non necessario, eliminare
		if (f.isPrimitive()) f = wrapper(f);
		*/
		if (t == Class.class && t ==  f.getClass()) return true;
		return t.isAssignableFrom( f );
	}
	
	/*
	private static boolean isAssignableFrom(Class t, Class ... fs) {
		for (Class f: fs) if (isAssignableFrom(t, f)) return true;
		/* TODO in alternativa al precedente; eliminare altrimenti
		for (Class f: fs) if (f == null ? !t.isPrimitive() : t.isAssignableFrom(f)) return true;
		* /
		return false;
	}
	
	private static Class getAssignableFrom(Class t, Class ... fs) {
		for (Class f: fs) if (isAssignableFrom(t, f)) return f;
		return null;
	}
	*/
	
	// Identity and Widening Primitive Conversion
	private static boolean isWPConvertible(Class t, Class f) {
		/* TODO verificare l'ottimizzazione, eliminare altrimenti
		if (!t.isPrimitive() || f == null || f.isPrimitive()) return false;
		//*/
		switch (f == null ? "" : f.getSimpleName()) {
			case "Boolean":   return t == boolean.class;
			case "Byte":      if (t == byte.class  ) return true;
			case "Short":     if (t == short.class ) return true;
							  if (t == char.class  ) return false; // Byte e Short non vanno a char
			case "Character": if (t == char.class  ) return true;
			case "Integer":   if (t == int.class   ) return true;
			case "Long":      if (t == long.class  ) return true;
			case "Float":     if (t == float.class ) return true;
			case "Double":    if (t == double.class) return true;
			default:          return false;
		}
	}	
}
