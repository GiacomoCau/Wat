package Wat;

import java.lang.reflect.Array;
import java.lang.reflect.Executable;
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

	public static Executable getExecutable(Object obj, String name, Object ... args) {
		return getExecutable(/*obj instanceof Class ? (Class) obj :*/  obj.getClass(), name, getClasses(args));
	}
	
	public static Class[] getClasses(Object ... objects) {
		return Arrays.stream(objects).map(o-> o == null ? null : o.getClass()).toArray(Class[]::new);
	}

	public static <T extends Executable> T getExecutable(Class <?> classe, String name) {
		return getExecutable(classe, name, (Class[]) null);
	}
	
	public static <T extends Executable> T getExecutable(Class <?> classe, String name, Class <?> ... classes) {
		return getExecutable(classe, true, name, classes);
	}
	
	public static <T extends Executable> T getExecutable(Class <?> classe, boolean publicMember, String name, Class <?> ... argumentsClass) {
		boolean constructor = name.equals("new");
		Executable r = null;
		do {
			try {
				return (T) getExecutor(classe, constructor, publicMember, name, argumentsClass);
			}
			catch (NoSuchMethodException nsme) {
				for (Executable e: getExecutors(classe, constructor, publicMember)) {
					if (!constructor && !e.getName().equals(name)) continue;
					if (argumentsClass == null) return (T) e; // si va per nome, non si possono controllare gli argomenti
					boolean isVarArgs = e.isVarArgs();
					Class [] parametersClass = e.getParameterTypes();
					if (argumentsClass.length != parametersClass.length || isVarArgs && argumentsClass.length < parametersClass.length - 1) continue;
					if (!isInvokeConvertible(isVarArgs, parametersClass, argumentsClass)) continue;
					/* TODO sostituito dal seguente, eliminare appena verificato
					return (T) e; // torna il primo buono 
					*/
					if (r != null && e.isVarArgs() && e.getParameterCount() >= r.getParameterCount()) continue;
					r = e;
				}
				/* TODO sostituito dal seguente, eliminare appena verificato
				if (publicMember) return null;
				*/
				if (publicMember) break;
			}
		} while ((classe = classe.getSuperclass()) != null);
		/* TODO sostituito dal seguente, eliminare appena verificato
		return null;
		*/
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
		return t.isAssignableFrom( f );
	}
	
	private static boolean isAssignableFrom(Class t, Class ... fs) {
		for (Class f: fs) if (isAssignableFrom(t, f)) return true;
		/* TODO in alternativa al precedente; eliminare altrimenti
		for (Class f: fs) if (f == null ? !t.isPrimitive() : t.isAssignableFrom(f)) return true;
		*/
		return false;
	}
	
	private static Class getAssignableFrom(Class t, Class ... fs) {
		for (Class f: fs) if (isAssignableFrom(t, f)) return f;
		return null;
	}
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
