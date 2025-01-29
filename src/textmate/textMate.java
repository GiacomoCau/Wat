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
		var str = "(def ab 1 (2) \"string\" #\\x28 #\\x #\\  '(1 2) 'abc #|\naba\n|#)";
		ITokenizeLineResult lineTokens = grammar.tokenizeLine(str);
		IToken[] tokens = (IToken[]) lineTokens.getTokens();
		for (int i = 0; i < tokens.length; i++) {
			IToken token = tokens[i];
			token.getScopes().add(str.substring(token.getStartIndex(), token.getEndIndex()));
			System.out.println(
				"Token from " + token.getStartIndex()
				+ " to " + token.getEndIndex()
				+ " with scopes " + token.getScopes()
			);
		}
	}
}
