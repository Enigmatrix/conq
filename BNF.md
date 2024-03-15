```bnf
<unit> ::= "()"
<int> ::= [0-9]+
<bool> ::= "true" | "false"
<string> ::= "\"" [^"\""]*  "\"" // there’s probably a better way :)
<literal> ::= <int> | <bool> | <string>

<unary_op> ::= "!" | "-"
<arith_bin_op> ::= "+" | "-" | "/" | "*" | "%"
<log_bin_op> ::= "&&" | "||" | ">" | ">=" | "<" | "<=" | "==" | "!="
<bin_op> ::= <arith_bin_op> | <log_bin_op>

<cond> ::= "if" <expr> "{" <block> "}" ("else" "if " <expr> "{" <block> "}")* ("else" "{" <block> "}")?
<while> ::= "while" <expr> "{" <block> "}"

<ident> ::= [a-z]+ ; there’s probably a better way :)
<assign> ::= <ident> "=" <expr>
<decl> ::= "let" <ident> "=" <expr>

<fn_decl> ::= "fn" <ident> "(" <ident>* ")" "{" <block> "}"
<apply> ::= <ident> "(" <expr>* ")"

<expr> ::= <literal>
           | <unary_op> <expr>
           | <expr> <bin_op> <expr>
           | <cond>
           | <assign>
           | <apply>
           | <block>
           | "(" <expr> ")"
<block> ::= <stmt>* <expr>?
<stmt> ::= <expr> ";"
		 | <decl>
            | <fn_decl>
		 | "break" ";"
            | "continue" ";"
            | <while>
            | "return" <expr> ";"

<program> ::= <expr>
```

## Example

Drawing a cirle:

```
canvas_init();

let rad_delta = 0.1;
let rad_angle = 0;
let radius = 20;

while rad_angle <= 2 * math_pi() {
    let x = radius * math_cos(rad_angle);
    let y = radius * math_sin(rad_angle);
    canvas_draw_point(x, y);
    rad_angle += rad_delta;
}
```