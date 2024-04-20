export const makeTokensProvider = () => {
    return {
        keywords: [
            "break",
            "continue",
            "else",
            "fn",
            "for",
            "fun",
            "if",
            "let",
            "return",
            "true",
            "while",
        ],

        operators: [
            "-",
            "+",
            "/",
            "*",
            "%",
            "!",
            "!=",
            "=",
            "==",
            ">",
            ">=",
            "<",
            "<=",
            "&&",
            "||",
            ",",
        ],

        symbols: /[=><!:+\-*\/,]+/,

        tokenizer: {
            root: [
                // identifiers and keywords
                [
                    /[a-z_$][\w$]*/,
                    { cases: { "@keywords": "keyword", "@default": "identifier" } },
                ],
                // [/[A-Z][\w\$]*/, "type.identifier"], // to show class names nicely

                // whitespace
                { include: "@whitespace" },

                // delimiters and operators
                [/[{}()]/, "@brackets"],
                [/@symbols/, { cases: { "@operators": "operators", "@default": "" } }],

                // numbers
                // [/\d*\.\d+/, "number.float"],
                [/\d+/, "number"],

                // delimiter: after number because of .\d floats
                [/[;,.]/, "delimiter"],

                // strings
                [/"([^"\\]|\\.)*$/, "string.invalid"], // non-teminated string
                [/"/, { token: "string.quote", bracket: "@open", next: "@string" }],
            ],

            string: [
                [/[^\\"]+/, "string"],
                [/"/, { token: "string.quote", bracket: "@close", next: "@pop" }],
            ],

            whitespace: [
                [/[ \t\r\n]+/, "white"],
                [/\/\*/, "comment", "@comment"],
                [/\/\/.*$/, "comment"],
            ],

            comment: [
                [/[^\/*]+/, "comment"],
                [/\/\*/, "comment", "@push"], // nested comment
                ["\\*/", "comment", "@pop"],
                [/[\/*]/, "comment"],
            ],
        },
    };
};