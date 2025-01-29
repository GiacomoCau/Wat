package textmate;

import java.nio.file.Path;

import org.eclipse.tm4e.core.grammar.IGrammar;
import org.eclipse.tm4e.core.grammar.IToken;
import org.eclipse.tm4e.core.grammar.ITokenizeLineResult;
import org.eclipse.tm4e.core.registry.IGrammarSource;
import org.eclipse.tm4e.core.registry.Registry;

public class textMate {
	public static void main(String[] args) {
		Registry registry = new Registry();
		var grammarSource = IGrammarSource.fromFile(Path.of("src/textmate/tmLanguage.json"));
		IGrammar grammar = registry.addGrammar(grammarSource);
		var str = """
			ab 1 (2)
			\"str\\\"ing\"
			\";|!|,|'|…|_[1-9a-z*]?\"
			#\\x28 #\\x #\\ 
			#|
				aba
			|#
			&sss
			1 1.2 1.2e4 1.2e4bd 1d 1_2.3_4e5_6
			'abc[]
			'(1 2 (3))
			""";
		ITokenizeLineResult lineTokens = grammar.tokenizeLine(str);
		IToken[] tokens = (IToken[]) lineTokens.getTokens();
		for (int i = 0; i < tokens.length; i++) {
			IToken token = tokens[i];
			System.out.printf( "from %s to %s with scopes %s %s\n",
				token.getStartIndex(),
				token.getEndIndex(),
				token.getScopes(),
				str.substring(token.getStartIndex(), token.getEndIndex()).replaceAll("\n", "\\\\n").replaceAll("\t", "\\\\t")
			);
		}
	}
}
