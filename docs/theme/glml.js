(function () {
  function glml(hljs) {
    const KEYWORDS = {
      keyword: "let rec in fun match with if then else type of and as when",
      literal: "true false",
      type:
        "float int bool unit " +
        "vec2 vec3 vec4 " +
        "mat2 mat2x2 mat2x3 mat2x4 mat3 mat3x2 mat3x3 mat3x4 mat4 mat4x2 mat4x3 mat4x4",
    };

    const NUMBER = {
      className: "number",
      variants: [
        { begin: /\b0[xX][0-9a-fA-F]+\b/ },
        { begin: /(?:\b\d+\.\d*|\b\d+\.|\.\d+|\b\d+)(?:[eE][+-]?\d+)?/ },
      ],
      relevance: 0,
    };

    return {
      name: "GLML",
      aliases: ["glml"],
      case_insensitive: false,
      keywords: KEYWORDS,
      contains: [
        hljs.COMMENT("//", "$"),
        hljs.COMMENT(/\(\*/, /\*\)/),
        // Preprocessor-like directives (`#extern ...`) - only at start of line
        {
          className: "meta",
          begin: /^\s*#extern\b/,
          end: /$/,
          keywords: KEYWORDS,
          contains: [NUMBER],
        },
        // Builtins like #sin, #length
        {
          className: "built_in",
          begin: /#[a-zA-Z][a-zA-Z0-9_]*/,
        },
        NUMBER,
        // Type variables: 'a, 'b
        {
          className: "type",
          begin: /'[a-zA-Z][a-zA-Z0-9_']*/,
        },
        // Constructors: capitalized identifiers
        {
          className: "title.class",
          begin: /\b[A-Z][a-zA-Z0-9_']*/,
        },
        // Function definitions: `let name ...` or `let rec name ...`
        {
          begin: /\blet\s+(rec\s+)?/,
          end: /(?=[\s(=:])/,
          keywords: "let rec",
          contains: [
            {
              className: "title.function",
              begin: /[a-z_][a-zA-Z0-9_']*/,
            },
          ],
        },
      ],
    };
  }

  hljs.registerLanguage("glml", glml);

  document.querySelectorAll("code.language-glml").forEach(function (block) {
    block.removeAttribute("data-highlighted");
    block.classList.remove("hljs");
    block.innerHTML = block.textContent;
    hljs.highlightBlock(block);
  });
})();
