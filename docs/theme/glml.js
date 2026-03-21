(function () {
  function glml(hljs) {
    return {
      name: "GLML",
      aliases: ["glml"],
      case_insensitive: false,
      keywords: {
        keyword: "let rec in fun match with if then else type of",
        literal: "true false",
        type: "float int bool vec2 vec3 vec4 mat2 mat2x2 mat2x3 mat2x4 mat3 mat3x2 mat3x3 mat3x4 mat4 mat4x2 mat4x3 mat4x4",
      },
      contains: [
        hljs.COMMENT("//", "$"),
        // Builtins
        {
          className: "built_in",
          begin: /#[a-zA-Z][a-zA-Z0-9_]*/,
        },
        // Float literals
        {
          className: "number",
          begin: /[+-]?\b\d+\.\d*/,
          relevance: 0,
        },
        // Integer literals
        {
          className: "number",
          begin: /\b\d+\b/,
          relevance: 0,
        },
        // Type variables
        {
          className: "type",
          begin: /'[a-zA-Z][a-zA-Z0-9_']*/,
        },
        // Constructors
        {
          className: "title.class",
          begin: /\b[A-Z][a-zA-Z0-9_']*/,
        },
      ],
    };
  }

  hljs.registerLanguage("glml", glml);

  // rehighlight existing languages
  document.querySelectorAll("code.language-glml").forEach(function (block) {
    hljs.highlightBlock(block);
  });
})();
